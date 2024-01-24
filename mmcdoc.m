:- module mmcdoc.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module string, list, char, bool, maybe.
:- import_module getopt_io, popen, solutions.
:- use_module libinfo, config, dir, html.

:- func version = string.
version = "v0.2.0".

:- type option
    --->    help
    ;       version
    ;       local_docs
    ;       no_cache
    ;       list_modules
    ;       refresh_cache
    ;       index
    ;       backup
    ;       grep
    ;       comment
    ;       restrict.

:- pred short_option(char::in, option::out) is semidet.
short_option('h', help).
short_option('v', version).
short_option('l', local_docs).
short_option('n', no_cache).
short_option('i', index).
short_option('b', backup).
short_option('g', grep).
short_option('c', comment).
short_option('t', restrict).

:- pred long_option(string::in, option::out) is semidet.
long_option("help", help).
long_option("version", version).
long_option("local", local_docs).
long_option("no-cache", no_cache).
long_option("list-modules", list_modules).
long_option("refresh-cache", refresh_cache).
long_option("backup", backup).
long_option("grep", grep).
long_option("comment", comment).

:- pred option_defaults(option, option_data).
:- mode option_defaults(out, out) is multi.
option_defaults(help, bool(no)).
option_defaults(version, bool(no)).
option_defaults(local_docs, bool(no)).
option_defaults(no_cache, bool(no)).
option_defaults(list_modules, bool(no)).
option_defaults(refresh_cache, bool(no)).
option_defaults(index, bool(no)).
option_defaults(backup, bool(no)).
option_defaults(grep, bool(no)).
option_defaults(comment, bool(no)).
option_defaults(restrict, maybe_string(no)).

:- pred usage(io::di, io::uo) is erroneous.
usage(!IO) :-
    io.progname_base("mmc-doc", Program, !IO),
    Usage = string.format(
        "usage: %s [OPTION] <module>\n\n" ++
        "  -h, --help         show this text\n" ++
        "  -v, --version      show version\n" ++
        "  -l, --local        use local documentation (implied by -i)\n" ++
        "  -n, --no-cache     load remote docs without caching\n" ++
        "  -b, --backup       use backup (mercury-in.space) URL for remote docs\n\n" ++
        "  <module> <token>   search module declarations for token\n" ++
        "  -g, --grep         search for substrings instead of tokens\n" ++
        "  -c, --comment      search for comment substrings\n" ++
        "  -t <declaration>   restrict search to 'type', 'pred', etc.\n\n" ++
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
    getopt_io.process_options_io(Config, RawArgs, Args, ResOpts, !IO),
    (
        ResOpts = ok(Options),
        getopt_io.lookup_bool_option(Options, backup, Backup),
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
            foldl(refresh_cache(Backup), Modules, !IO)
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
                    ensure_cache(Backup, Module, !IO),
                    cache_stdlib(Module, !IO)
                ;
                    NoCache = yes,
                    (
                        Backup = yes,
                        backup_stdlib(Module, !IO)
                    ;
                        Backup = no,
                        remote_stdlib(Module, !IO)
                    )
                )
            )
        else if
            Args = [Module, Atom],
            libinfo.stdlib(Module)
        then
            getopt_io.lookup_bool_option(Options, local_docs, LocalDocs),
            getopt_io.lookup_maybe_string_option(Options, restrict, Restrict),
            (
                LocalDocs = yes,
                open_local_stdlib(Module, Res, !IO)
            ;
                LocalDocs = no,
                ensure_cache(Backup, Module, !IO),
                open_cache_stdlib(Module, Res, !IO)
            ),
            ( if getopt_io.lookup_bool_option(Options, grep, yes) then
                Target = substring(Atom)
            else if getopt_io.lookup_bool_option(Options, comment, yes) then
                Target = comment(Atom)
            else
                Target = token(Atom)
            ),
            (
                Res = ok(File),
                grep_atom(Restrict, Module, Target, File, !IO)
            ;
                Res = error(Reason),
                die(string(Reason), !IO)
            )
        else
            usage(!IO)
        )
    ;
        ResOpts = error(Reason),
        io.progname_base("mmc-doc", Program, !IO),
        io.format(io.stderr_stream, "%s: %s\n",
            [s(Program), s(option_error_to_string(Reason))], !IO),
        usage(!IO)
    ).

:- pred local_index(string::in, io::di, io::uo) is det.
local_index(Index, !IO) :-
    det_htmldir(Dir, !IO),
    browse_to(Dir ++ "/" ++ Index, !IO).

:- pred local_stdlib(string::in, io::di, io::uo) is det.
local_stdlib(Module, !IO) :-
    det_htmldir(Dir, !IO),
    browse_to(Dir ++ "/mercury_library.html#" ++ libinfo.anchor(Module), !IO).

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

:- pred ensure_cache(bool::in, string::in, io::di, io::uo) is det.
ensure_cache(Backup, Module, !IO) :-
    config.cachefile(Module, Res, !IO),
    (
        Res = yes(Path),
        io.check_file_accessibility(Path, [read], Res2, !IO),
        (
            Res2 = ok
        ;
            Res2 = error(_),
            refresh_cache(Backup, Module, !IO)
        )
    ;
        Res = no,
        die("Unable to determine local cachefile. Try --local\n", !IO)
    ).

:- pred refresh_cache(bool::in, string::in, io::di, io::uo) is det.
refresh_cache(Backup, Module, !IO) :-
    config.cachefile(Module, Res, !IO),
    (
        Res = yes(Path),
        config.older(Path, 14, Old, !IO),
        (
            Old = yes,
            (
                Backup = yes,
                Url = config.backup_url(Module)
            ;
                Backup = no,
                Url = config.url(Module)
            ),
            Cmd = string.format("wget -O '%s' '%s'", [s(Path), s(Url)]),
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
        browse_to(Path, !IO)
    ;
        Res = no,
        die("Unable to determine local cachefile. Try --local\n", !IO)
    ).

:- pred remote_stdlib(string::in, io::di, io::uo) is det.
remote_stdlib(Module, !IO) :-
    browse_to(config.url(Module), !IO).

:- pred backup_stdlib(string::in, io::di, io::uo) is det.
backup_stdlib(Module, !IO) :-
    browse_to(config.backup_url(Module), !IO).

:- type grep_target
    --->    comment(string)
    ;       substring(string)
    ;       token(string).

:- type grep_state
    --->    other_module
    ;       in_module(list(string))
    ;       in_term(list(string))
    ;       ending_term.

:- pred grep_atom(maybe(string), string, grep_target, io.input_stream, io, io).
:- mode grep_atom(in, in, in, in, di, uo) is det.
grep_atom(Restrict, Module, Target, File, !IO) :-
    grep_atom(other_module, Restrict, Module, Target, File, !IO).

:- pred grep_atom(grep_state, maybe(string), string, grep_target, io.input_stream, io, io).
:- mode grep_atom(in, in, in, in, in, di, uo) is det.
grep_atom(other_module, Restrict, Module, Target, File, !IO) :-
    io.read_line_as_string(File, Res, !IO),
    (
        Res = ok(Line),
        ( if Line = ":- module " ++ Module ++ ".\n" then
            S = in_module([])
        else
            S = other_module
        ),
        grep_atom(S, Restrict, Module, Target, File, !IO)
    ;
        Res = eof
    ;
        Res = error(E),
        die(string.format("error reading cache file: %s", [s(string(E))]), !IO)
    ).

grep_atom(InSomething, Restrict, Module, Target, File, !IO) :-
    ( InSomething = in_module(L) ; InSomething = in_term(L) ),
    io.read_line_as_string(File, Res, !IO),
    (
        Res = ok(Line),
        CodeLine = no_comments(Line),
        ( if Line = ":- module " ++ Module ++ ".\n" then
            S = in_module([])
        else if prefix(Line, ":- module ") then
            S = other_module
        else if prefix(Line, "    %") then
            ( if Target = comment(Text), sub_string_search(Line, Text, _) then
                dump(Line, L, !IO),
                S = ending_term
            else
                S = in_module([Line | L])
            )
        else if
            Target = substring(Text),
            (
                InSomething = in_module(_),
                prefix(Line, ":-"),
                (
                    Restrict = yes(P),
                    prefix(Line, ":- " ++ P)
                ;
                    Restrict = no
                )
            ;
                InSomething = in_term(_)
            ),
            sub_string_search(CodeLine, Text, _)
        then
            dump(Line, L, !IO),
            S = ending_term
        else if
            Target = token(Token),
            (
                InSomething = in_module(_),
                prefix(Line, ":-"),
                (
                    Restrict = yes(P),
                    prefix(Line, ":- " ++ P)
                ;
                    Restrict = no
                ),
                tokenize(Token, CodeLine,
                    [_ | Tokens]) % skip leading 'pred', 'func', etc.
            ;
                InSomething = in_term(_),
                tokenize(Token, CodeLine, Tokens)
            ),
            list.member(Token, Tokens)
        then
            dump(Line, L, !IO),
            S = ending_term
        else if
            ( Target = substring(_) ; Target = token(_) ),
            prefix(Line, ":-"),
            (
                Restrict = yes(P),
                prefix(Line, ":- " ++ P)
            ;
                Restrict = no
            ),
            not suffix(string.rstrip(CodeLine), ".")
        then
            S = in_term([Line | L])
        else if
            InSomething = in_term(_),
            ( prefix(Line, " ") ; prefix(Line, "%") ),
            not all_match(is_whitespace, Line)
        then
            S = in_term([Line | L])
        else
            S = in_module([])
        ),
        grep_atom(S, Restrict, Module, Target, File, !IO)
    ;
        Res = eof
    ;
        Res = error(E),
        die(string.format("error reading cache file: %s", [s(string(E))]), !IO)
    ).

grep_atom(ending_term, Restrict, Module, Target, File, !IO) :-
    io.read_line_as_string(File, Res, !IO),
    (
        Res = ok(Line),
        dump(Line, [], !IO),
        ( if all_match(is_whitespace, Line) then
            S = in_module([])
        else
            S = ending_term
        ),
        grep_atom(S, Restrict, Module, Target, File, !IO)
    ;
        Res = eof
    ;
        Res = error(E),
        die(string.format("error reading cache file: %s", [s(string(E))]), !IO)
    ).

:- pred dump(string, list(string), io, io).
:- mode dump(in, in, di, uo) is det.
dump(Line, L, !IO) :-
    foldl(io.write_string, map(html.decode_html_entities, reverse(L)), !IO),
    io.write_string(html.decode_html_entities(Line), !IO).

:- func no_comments(string) = string.
no_comments(Line) = CodeLine :-
    ( if [CodeLine0 | _] = split_at_char('%', Line) then
        CodeLine = CodeLine0
    else
        CodeLine = Line
    ).

:- pred tokenize(string, string, list(string)).
:- mode tokenize(in, in, out) is det.
tokenize(Token, Line, Tokens) :-
    ( if string.contains_char(Token, '.') then
        P = (pred(C::in) is semidet :-
            not C = ('.'),
            not char.is_alnum_or_underscore(C)),
        Tokens0 = words_separator(P, Line),
        Tokens = list.map(string.remove_suffix_if_present("."), Tokens0)
    else
        P = (pred(C::in) is semidet :-
            not char.is_alnum_or_underscore(C)),
        Tokens = words_separator(P, Line)
    ).

:- pred open_local_stdlib(string, io.res(io.input_stream), io, io).
:- mode open_local_stdlib(in, out, di, uo) is det.
open_local_stdlib(_, Res, !IO) :-
    det_htmldir(Dir, !IO),
    io.open_input(Dir ++ "/mercury_library.html", Res, !IO).

:- pred open_cache_stdlib(string, io.res(io.input_stream), io, io).
:- mode open_cache_stdlib(in, out, di, uo) is det.
open_cache_stdlib(Module, Res, !IO) :-
    config.cachefile(Module, CacheRes, !IO),
    (
        CacheRes = yes(Path),
        io.open_input(Path, Res, !IO)
    ;
        CacheRes = no,
        die("Unable to determine local cachefile. Try --local\n", !IO)
    ).

:- pred browse_to(string::in, io::di, io::uo) is det.
browse_to(URL, !IO) :-
    config.browser(MaybeBrowser, !IO),
    (
        MaybeBrowser = yes(Browser),
        call_system(string.format("%s '%s'", [s(Browser), s(URL)]), !IO)
    ;
        MaybeBrowser = no,
        die("couldn't find a browser to use (set $WWWPAGER? get w3m? get xdg-open?)", !IO)
    ).

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
