:- module libinfo.
:- interface.

    % stdlib(S)
    % Succeeds if S is the name of a stdlib module
    %
:- pred stdlib(string).
:- mode stdlib(in) is semidet.
:- mode stdlib(out) is multi.

    % anchor(S0) = S
    % replace . and _ chars in stdlib module names to match
    % the generated HTML anchors
    %
:- func anchor(string) = string.

:- implementation.
:- import_module string.

anchor(S0) = S :-
    string.replace_all(S0, "_", "_005f", S1),
    string.replace_all(S1, ".", "_002e", S).

stdlib("array").
stdlib("array2d").
stdlib("assoc_list").
stdlib("bag").
stdlib("benchmarking").
stdlib("bimap").
stdlib("bit_buffer").
stdlib("bit_buffer.read").
stdlib("bit_buffer.write").
stdlib("bitmap").
stdlib("bool").
stdlib("bt_array").
stdlib("builtin").
stdlib("calendar").
stdlib("char").
stdlib("construct").
stdlib("cord").
stdlib("counter").
stdlib("deconstruct").
stdlib("diet").
stdlib("digraph").
stdlib("dir").
stdlib("edit_seq").
stdlib("enum").
stdlib("eqvclass").
stdlib("exception").
stdlib("fat_sparse_bitset").
stdlib("float").
stdlib("gc").
stdlib("getopt").
stdlib("getopt_io").
stdlib("hash_table").
stdlib("injection").
stdlib("int").
stdlib("int8").
stdlib("int16").
stdlib("int32").
stdlib("int64").
stdlib("integer").
stdlib("io").
stdlib("lazy").
stdlib("library").
stdlib("list").
stdlib("map").
stdlib("math").
stdlib("maybe").
stdlib("mercury_term_lexer").
stdlib("mercury_term_parser").
stdlib("multi_map").
stdlib("ops").
stdlib("pair").
stdlib("parsing_utils").
stdlib("pprint").
stdlib("pqueue").
stdlib("pretty_printer").
stdlib("prolog").
stdlib("psqueue").
stdlib("queue").
stdlib("random").
stdlib("ranges").
stdlib("rational").
stdlib("rbtree").
stdlib("require").
stdlib("rtree").
stdlib("set").
stdlib("set_bbbtree").
stdlib("set_ctree234").
stdlib("set_ordlist").
stdlib("set_tree234").
stdlib("set_unordlist").
stdlib("solutions").
stdlib("sparse_bitset").
stdlib("stack").
stdlib("std_util").
stdlib("store").
stdlib("stream").
stdlib("stream.string_writer").
stdlib("string.builder").
stdlib("string").
stdlib("table_statistics").
stdlib("term").
stdlib("term_conversion").
stdlib("term_io").
stdlib("term_to_xml").
stdlib("thread.barrier").
stdlib("thread.channel").
stdlib("thread.future").
stdlib("thread").
stdlib("thread.mvar").
stdlib("thread.semaphore").
stdlib("time").
stdlib("tree234").
stdlib("tree_bitset").
stdlib("type_desc").
stdlib("uint").
stdlib("uint8").
stdlib("uint16").
stdlib("uint32").
stdlib("uint64").
stdlib("unit").
stdlib("univ").
stdlib("varset").
stdlib("version_array").
stdlib("version_array2d").
stdlib("version_bitmap").
stdlib("version_hash_table").
stdlib("version_store").
