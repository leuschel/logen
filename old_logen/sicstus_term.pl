:- multifile term_expansion/2.

%:- print('Loaded Sicstus Term Expansion\n').
%% this file defines the term expansions for Sicstus

%term_expansion(FOO, _) :- portray_clause(te(FOO)), fail.
%term_expansion((:- use_package(PACK)) , ( :- print('Ignoring CIAO Package::'), print(PACK), nl)).
term_expansion((:- use_package(_PACK)) , ( :- true)).
term_expansion((sicstus(CALL) :- BODY), (CALL :- BODY)).
term_expansion((sicstus(FACT)), FACT).
term_expansion((sicstus((:-Decl))), (:-Decl)).
term_expansion((ciao(_) :- _), (:- true)).
term_expansion((ciao(_)), (:- true)).
term_expansion((:-comment(_,_)), (:- true)).
%term_expansion((:-ensure_loaded('sicstus_term.pl')), (:- true)).



