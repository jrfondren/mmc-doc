:- module popen.
:- interface.
:- import_module io.

:- type popen_failure
    --->    popen
    ;       read
    ;       pclose.

:- type popen_result
    --->    ok(string, int)
    ;       error(popen_failure).

:- pred popen(string::in, popen_result::out, io::di, io::uo) is det.

:- implementation.

popen(Command, Res, !IO) :-
    popen_2(Command, Output, Error, !IO),
    ( if Error = -1 then
        Res = error(popen)
    else if Error = -2 then
        Res = error(read)
    else if Error = -3 then
        Res = error(pclose)
    else
        Res = ok(Output, Error)
    ).

:- pred popen_2(string::in, string::out, int::out, io::di, io::uo) is det.
:- pragma foreign_decl("C", "#include <stdio.h>").
:- pragma foreign_proc("C",
    popen_2(Command::in, Output::out, Error::out, _IO0::di, _IO::uo),
    [promise_pure],
"
    FILE *pipe = popen(Command, ""r"");
    if (pipe == NULL) {
        Error = -1;
    } else {
        char *contents = NULL;
        size_t len = 0;
        if (-1 == getdelim(&contents, &len, 0, pipe)) {
            Error = -2;
        } else {
            int res = pclose(pipe);
            if (res == -1) {
                Error = -3;
            } else {
                MR_allocate_aligned_string_msg(Output, len + 1, MR_ALLOC_ID);
                strcpy(Output, contents);
                Error = res;
            }
        }
        free(contents);
    }
").
