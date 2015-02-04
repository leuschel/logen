
% this package strips out pp_mnf,mnf,pp_call
% to use it type: ":- include(runtime_checks_ignore)."
:- op(500,yfx,pre).
:- op(500,yfx,post).
:- op(500,yfx,unit_test).

:- load_compilation_module('runtime_checks_ignore_expand').

:- add_sentence_trans(ignore_mnf/3).

