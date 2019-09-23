:- module html.
:- interface.

    % replace only the HTML entities that Mercury docs use
:- func decode_html_entities(string) = string.

:- implementation.
:- import_module string.

decode_html_entities(!.S) = !:S :-
    replace_all(!.S, "&lt;", "<", !:S),
    replace_all(!.S, "&quot;", """", !:S),
    replace_all(!.S, "&gt;", ">", !:S),
    replace_all(!.S, "&nbsp;", " ", !:S),
    replace_all(!.S, "&amp;", "&", !:S).
