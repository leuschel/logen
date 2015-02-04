
:- module(sicsclp_expand,[ expand_clp/3 ],[ ]).

:- use_package(clpq).


expand_clp({SICS_CLP}, CIAO_CLP,_M) :- convertCLP(SICS_CLP, CIAO_CLP).
expand_clp(Term, Term, _M).

expand_clp(end_of_file, end_of_file,_M).


convertCLP((A,B), (CA,CB)) :- convertCLP(A,CA),convertCLP(B,CB).
convertCLP(SICS, CIAO) :-
	SICS =.. [Sym, LHS,RHS], 
	convertSym(Sym, CSym),
	CIAO =.. [CSym, LHS,RHS].

convertSym(=, .=.).
convertSym(>, .>.).
convertSym(<, .<.).
convertSym(>=, .>=.).
convertSym(=<, .=<.).


