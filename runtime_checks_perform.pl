
% this package strips out pp_mnf,mnf,pp_call
% to use it type: ":- include(runtime_checks_perform)."



:- op(500,yfx,pre).
:- op(500,yfx,post).
:- op(500,yfx,unit_test).

:- use_module('tools/error_manager.pl', [add_error/3]).
:- load_compilation_module('runtime_checks_perform_expand').
%:- load_compilation_module('runtime_checks_ignore_expand').

:- add_sentence_trans(ignore_mnf/3).

