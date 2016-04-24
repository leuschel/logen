%% these imports are added to all gx files
:- if(current_prolog_flag(dialect, ciao)).
:- use_module(library(terms_vars)).
:- else.
varset(A,B) :- term_variables(A,B).
:- endif.
