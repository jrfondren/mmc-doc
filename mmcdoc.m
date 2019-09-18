:- module mmcdoc.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module string, list, char, bool, maybe.
:- import_module getopt_io, popen, solutions.
:- use_module libinfo, config, dir.

:- func version = string.
version = "v0.1.0".

:- type option
    --->    help
    ;       version
    ;       local_docs
    ;       no_cache
    ;       list_modules
    ;       refresh_cache
    ;       index.

:- pred short_option(char::in, option::out) is semidet.
short_option('h', help).
short_option('v', version).
short_option('l', local_docs).
short_option('n', no_cache).
short_option('i', index).

:- pred long_option(string::in, option::out) is semidet.
long_option("help", help).
long_option("version", version).
long_option("local", local_docs).
long_option("no-cache", no_cache).
long_option("list-modules", list_modules).
long_option("refresh-cache", refresh_cache).

:- pred option_defaults(option, option_data).
:- mode option_defaults(out, out) is multi.
option_defaults(help, bool(no)).
option_defaults(version, bool(no)).
option_defaults(local_docs, bool(no)).
option_defaults(no_cache, bool(no)).
option_defaults(list_modules, bool(no)).
option_defaults(refresh_cache, bool(no)).
option_defaults(index, bool(no)).

:- pred usage(io::di, io::uo) is erroneous.
usage(!IO) :-
    io.progname_base("mmc-doc", Program, !IO),
    Usage = string.format(
        "usage: %s [OPTION] <module>\n\n" ++
        "  -h, --help         show this text\n" ++
        "  -v, --version      show version\n" ++
        "  -l, --local        use local documentation (implied by -i)\n" ++
        "  -n, --no-cache     loading remote docs without caching\n\n" ++
        "  -i [lib|ref|user|prolog|faq]\n" ++
        "                     load doc index, stdlib index, etc.\n\n" ++
        "  --list-modules     list standard library modules\n" ++
        "  --refresh-cache    download all stdlib library files if old\n\n" ++
        "Absent flags, pull up Mercury stdlib docs by module name.\n\n",
        [s(Program)]),
    die(Usage, !IO).

:- pred index(string, string).
:- mode index(in, out) is semidet.
index("lib", "library").
index("ref", "ref").
index("user", "user_guide").
index("prolog", "trans_guide").
index("faq", "faq").

main(!IO) :-
    io.command_line_arguments(RawArgs, !IO),
    Config = option_ops_multi(short_option, long_option, option_defaults),
    getopt_io.process_options(Config, RawArgs, Args, Res, !IO),
    (
        Res = ok(Options),
        ( if getopt_io.lookup_bool_option(Options, help, yes) then
            usage(!IO)
        else if getopt_io.lookup_bool_option(Options, version, yes) then
            io.write_string(io.stderr_stream,
                "mmc-doc version " ++ version ++ "\n", !IO)
        else if getopt_io.lookup_bool_option(Options, list_modules, yes) then
            P = (pred(M::out) is nondet :- libinfo.stdlib(M)),
            solutions(P, Modules),
            foldl(io.print_line, Modules, !IO)
        else if getopt_io.lookup_bool_option(Options, refresh_cache, yes) then
            P = (pred(M::out) is nondet :- libinfo.stdlib(M)),
            solutions(P, Modules),
            foldl(refresh_cache, Modules, !IO)
        else if
            getopt_io.lookup_bool_option(Options, index, yes),
            Args = [Index],
            index(Index, FileBase)
        then
            local_index("mercury_" ++ FileBase ++ ".html", !IO)
        else if getopt_io.lookup_bool_option(Options, index, yes) then
            local_index("mercury.html", !IO)
        else if
            Args = [Module],
            libinfo.stdlib(Module)
        then
            getopt_io.lookup_bool_option(Options, local_docs, LocalDocs),
            (
                LocalDocs = yes,
                local_stdlib(Module, !IO)
            ;
                LocalDocs = no,
                getopt_io.lookup_bool_option(Options, no_cache, NoCache),
                (
                    NoCache = no,
                    ensure_cache(Module, !IO),
                    cache_stdlib(Module, !IO)
                ;
                    NoCache = yes,
                    remote_stdlib(Module, !IO)
                )
            )
        else
            usage(!IO)
        )
    ;
        Res = error(Reason),
        io.progname_base("mmc-doc", Program, !IO),
        io.format(io.stderr_stream, "%s: %s\n", [s(Program), s(Reason)], !IO),
        usage(!IO)
    ).

:- pred local_index(string::in, io::di, io::uo) is det.
local_index(Index, !IO) :-
    det_htmldir(Dir, !IO),
    Cmd = string.format("w3m '%s/%s'", [s(Dir), s(Index)]),
    call_system(Cmd, !IO).

:- pred local_stdlib(string::in, io::di, io::uo) is det.
local_stdlib(Module, !IO) :-
    det_htmldir(Dir, !IO),
    Cmd = string.format("w3m '%s/mercury_library.html#%s'",
        [s(Dir), s(libinfo.anchor(Module))]),
    call_system(Cmd, !IO).

:- pred det_htmldir(string::out, io::di, io::uo) is det.
det_htmldir(Dir, !IO) :-
    config.htmldir(Res, !IO),
    (
        Res = yes(Dir)
    ;
        Res = no,
        io.progname_base("mmc-doc", Program, !IO),
        Error = string.format(
            "%s: Error: Can't find installed doc directory.\n",
            [s(Program)]),
        die(Error, !IO)
    ).

:- pred ensure_cache(string::in, io::di, io::uo) is det.
ensure_cache(Module, !IO) :-
    config.cachefile(Module, Res, !IO),
    (
        Res = yes(Path),
        io.check_file_accessibility(Path, [read], Res2, !IO),
        (
            Res2 = ok
        ;
            Res2 = error(_),
            refresh_cache(Module, !IO)
        )
    ;
        Res = no,
        die("Unable to determine local cachefile. Try --local\n", !IO)
    ).

:- pred refresh_cache(string::in, io::di, io::uo) is det.
refresh_cache(Module, !IO) :-
    config.cachefile(Module, Res, !IO),
    (
        Res = yes(Path),
        config.older(Path, 14, Old, !IO),
        (
            Old = yes,
            Cmd = string.format("wget -O '%s' '%s'",
                [s(Path), s(config.url(Module))]),
            call_system(Cmd, !IO),
            call_system("touch " ++ Path, !IO)
        ;
            Old = no
        )
    ;
        Res = no,
        die("Unable to determine local cachefile. Try --local\n", !IO)
    ).

:- pred cache_stdlib(string::in, io::di, io::uo) is det.
cache_stdlib(Module, !IO) :-
    config.cachefile(Module, Res, !IO),
    (
        Res = yes(Path),
        Cmd = string.format("w3m '%s'", [s(Path)]),
        call_system(Cmd, !IO)
    ;
        Res = no,
        die("Unable to determine local cachefile. Try --local\n", !IO)
    ).

:- pred remote_stdlib(string::in, io::di, io::uo) is det.
remote_stdlib(Module, !IO) :-
    Cmd = string.format("w3m '%s'", [s(config.url(Module))]),
    call_system(Cmd, !IO).

:- pred call_system(string::in, io::di, io::uo) is det.
call_system(Cmd, !IO) :-
    io.call_system(Cmd, CallRes, !IO),
    ( if CallRes = ok(0) then
        true
    else if CallRes = error(E) then
        die(string(E), !IO)
    else
        die(string.format("Failed to spawn command: %s\n",
            [s(Cmd)]), !IO)
    ).

:- pred die(string::in, io::di, io::uo) is erroneous.
die(Error, !IO) :-
    io.write_string(io.stderr_stream, Error, !IO),
    die(!IO).

:- pred die(io::di, io::uo) is erroneous.
:- pragma foreign_proc("C",
    die(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    exit(1);
").
