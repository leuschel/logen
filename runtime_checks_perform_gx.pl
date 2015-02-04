
% this package strips out pp_mnf,mnf,pp_call
% to use it type: ":- include(runtime_checks_perform)."
% this file is imported into the GX Files


%:- load_compilation_module('runtime_checks_ignore_expand').

%:- use_module('tools/error_manager.pl', [add_error/3]).
:- load_compilation_module('runtime_checks_perform_expand').

:- add_sentence_trans(ignore_mnf/3).

