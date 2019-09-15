:- module config.
:- interface.
:- import_module io, maybe, bool.

:- pred configdir(maybe(string)::out, io::di, io::uo) is det.

:- pred htmldir(maybe(string)::out, io::di, io::uo) is det.

:- pred cachefile(string::in, maybe(string)::out, io::di, io::uo) is det.

:- func url(string) = string.

:- pred exists(string::in, bool::out, io::di, io::uo) is det.

    % older(Path, Days, Bool, !IO)
    % Bool = no if Path exists and was modified within Days days
    % Bool = yes otherwise (so if old or absent)
    %
:- pred older(string::in, int::in, bool::out, io::di, io::uo) is det.

:- implementation.
:- import_module popen, string, list.
:- use_module dir, libinfo.

:- mutable(configdir, maybe(maybe(string)), no, ground, [untrailed, attach_to_io_state]).
:- mutable(htmldir, maybe(maybe(string)), no, ground, [untrailed, attach_to_io_state]).

:- type maybeio == (pred(maybe(string), io, io)).
:- inst maybeio == (pred(out, di, uo) is det).
:- func (maybeio::in(maybeio)) // (maybeio::in(maybeio))
    = (maybeio::out(maybeio)).
A // B = C :-
    C = (pred(Res::out, !.IO::di, !:IO::uo) is det :-
        A(Res1, !IO),
        (
            Res1 = Res @ yes(_)
        ;
            Res1 = no,
            B(Res, !IO)
        )).

cachefile(Module, Path, !IO) :-
    configdir(Res, !IO),
    (
        Res = yes(Dir),
        Path = yes(dir.(Dir / libinfo.anchor(Module)) ++ ".html")
    ;
        Res = no,
        Path = no
    ).

url(Module) = Base ++ libinfo.anchor(Module) ++ ".html" :-
    Base = "https://mercurylang.org/information/doc-latest/mercury_library/".

configdir(Dir, !IO) :-
    get_configdir(Decided, !IO),
    (
        Decided = yes(Dir)
    ;
        Decided = no,
        GetConfig = get_environment_var("XDG_CONFIG_HOME")
            // get_environment_var("APPDATA")
            // (pred(Res::out, !.IO::di, !:IO::uo) is det :-
                get_environment_var("HOME", Res1, !IO),
                (
                    Res1 = Res @ no
                ;
                    Res1 = yes(Homedir),
                    Res = yes(dir.(Homedir / ".config"))
                )),
        GetConfig(ConfigRes, !IO),
        (
            ConfigRes = yes(Config),
            Path = dir.(Config / "mmc-doc"),
            Dir = yes(Path),
            dir.make_single_directory(Path, _, !IO),
            set_configdir(yes(Dir), !IO)
        ;
            ConfigRes = no,
            Dir = no
        )
    ).

htmldir(Dir, !IO) :-
    get_htmldir(Decided, !IO),
    (
        Decided = yes(Dir)
    ;
        Decided = no,
        GetDir = get_environment_var("MMCDOC_HTMLDIR")
            //  (pred(Res::out, !.IO::di, !:IO::uo) is det :-
                    configdir(ConfigRes, !IO),
                    ( if ConfigRes = yes(Config) then
                        exists(dir.(Config / "mercury_library.html"), HasHTML, !IO),
                        (
                            HasHTML = yes,
                            Res = ConfigRes
                        ;
                            HasHTML = no,
                            Res = no
                        )
                    else
                        Res = no
                    ))
            //  (pred(Res::out, !.IO::di, !:IO::uo) is det :-
                    popen("which mmc", Res1, !IO),
                    ( if
                        Res1 = ok(BinDir, _),
                        string.remove_suffix(BinDir, "/bin/mmc\n", MmcDir),
                        list.last(string.split_at_char('/', MmcDir), Version)
                    then
                        Res = yes(MmcDir ++ "/share/doc/" ++ Version ++ "/")
                    else
                        Res = no
                    )),
        GetDir(Dir, !IO),
        set_htmldir(yes(Dir), !IO)
    ).

:- pragma foreign_decl("C", "
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
").

:- pragma foreign_proc("C",
    exists(Path::in, Exists::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    int res = open(Path, O_RDONLY);
    if (res == -1) {
        Exists = MR_NO;
    } else {
        Exists = MR_YES;
        close(res);
    }
").

:- pragma foreign_proc("C",
    older(Path::in, Days::in, Old::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    struct stat statbuf;
    time_t now = time(NULL);

    if (0 == stat(Path, &statbuf)) {
        Old = ((now - statbuf.st_mtime)/(24*3600)) > Days ? MR_YES : MR_NO;
    } else {
        Old = MR_YES;
    }
").
