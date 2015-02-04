
:- module(builtin_db,
	    [equallike_imperative/1,
	     varlike_imperative/1,
	     groundlike_imperative/1,
 	     hide_nf_literal/2,
	     is_built_in_literal/1]).

%% safety for built ins using online annotation
%% not current used but it will be...
is_safe_built_in(is(_A,B), ground(B)).
is_safe_built_in(\==(A,B), ground((A,B))).
is_safe_built_in(\=(A,B), diff(A,B)).
is_safe_built_in(atomic(A), nonvar(A)).






	     
equallike_imperative(=(_X,_Y)).
equallike_imperative(is(_X,Y)) :- ground(Y).
varlike_imperative(var(_X)).
varlike_imperative(copy_term(_X,_Y)).
varlike_imperative((_X\==_Y)).
 /* ground like: if it succeeds then it succeeds for all instances */
groundlike_imperative(ground(_X)).
groundlike_imperative(nonvar(_X)).
groundlike_imperative(_X==_Y).
groundlike_imperative(atom(_X)).
groundlike_imperative(integer(_X)).



%%% used by bta (Steve)
%% --- database of hide literal
hide_nf_literal(true,logen(rescall,true)) :- !.
hide_nf_literal(logen(call,true),logen(call,true)) :- !.
hide_nf_literal(logen(call,X=Y),logen(rescall,X=Y)) :- !.
hide_nf_literal(logen(memo,X),logen(memo,X)) :- !.
hide_nf_literal(logen(rescall,X),logen(rescall,X)) :- !.
hide_nf_literal(hide_nf(X),hide_nf(X)) :- !.
hide_nf_literal(resnot(X),resnot(X)) :- !.
hide_nf_literal(resdisj(X,Y),resdisj(X,Y)) :- !.
hide_nf_literal(resif(X,Y,Z),resif(X,Y,Z)) :- !.
hide_nf_literal(reslogif(X,Y,Z),reslogif(X,Y,Z)) :- !.
hide_nf_literal(resfindall(X,Y,Z),resfindall(X,Y,Z)) :- !.
hide_nf_literal(pp_cll(X),pp_cll(HX)) :- !, hide_nf_literal(X,HX).
hide_nf_literal(pp_mnf(X),pp_mnf(HX)) :- !, hide_nf_literal(X,HX).
hide_nf_literal(mnf(X),mnf(HX)) :- !, hide_nf_literal(X,HX).
hide_nf_literal((A,B),(A,HB)) :- hide_nf_literal(A,HA),A=HA,!, hide_nf_literal(B,HB).
hide_nf_literal((logen(call,X=Y),B),(logen(rescall,X=Y),HB)) :- !, hide_nf_literal(B,HB).
hide_nf_literal(Module:Ann,Module:HAnn) :- !,hide_nf_literal(Ann,HAnn).
hide_nf_literal(X,hide_nf(X)).

%% --- database of builtin literals
is_built_in_literal('='(_X,_Y)).
is_built_in_literal('{}'(_)).
is_built_in_literal('C'(_X,_Y,_Z)).
is_built_in_literal(true).
is_built_in_literal(fail).
is_built_in_literal('=..'(_X,_Y)).
is_built_in_literal(functor(_X,_Y,_Z)).
is_built_in_literal(arg(_X,_Y,_Z)).
is_built_in_literal(name(_X,_Y)).
is_built_in_literal('is'(_X,_Y)).
is_built_in_literal('<'(_X,_Y)).
is_built_in_literal('=<'(_X,_Y)).
is_built_in_literal('>'(_X,_Y)).
is_built_in_literal('>='(_X,_Y)).
is_built_in_literal(call(X)) :- var(X).
is_built_in_literal(call(X)) :- nonvar(X), is_built_in_literal(X).
is_built_in_literal(nonvar(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(ground(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(number(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(real(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(integer(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(atomic(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(atom(_X)).   /* declarative if delays until nonvar */
is_built_in_literal('\\=='(_X,_Y)). /* declarative if delays until 
sufficiently ground */
is_built_in_literal('=='(_X,_Y)). /* declarative if delays until sufficiently ground */
is_built_in_literal('\\='(_X,_Y)). 
is_built_in_literal(clause(_X,_Y)). 
is_built_in_literal(print(_X)).   	/* not declarative */
is_built_in_literal(write(_X)).   	/* not declarative */
is_built_in_literal(nl). 		/* not declarative */
is_built_in_literal(assert(_X)). 	/* not declarative */
is_built_in_literal(asserta(_X)). 	/* not declarative */
is_built_in_literal(assertz(_X)). 	/* not declarative */
is_built_in_literal(retract(_X)). 	/* not declarative */
is_built_in_literal(var(_X)). 		/* not declarative */
is_built_in_literal(copy(_X,_Y)). 	/* not declarative */
is_built_in_literal(term_variables(_,_)).
is_built_in_literal(copy_term(_,_)).
is_built_in_literal(load_files(_,_)).
is_built_in_literal(current_predicate(_,_)).
is_built_in_literal(predicate_property(_,_)).
is_built_in_literal(prolog_flag(_,_)).
is_built_in_literal(portray_clause(_)).

/*
is_built_in_literal(X) :-
	nonvar(X),
	is_open_literal(X).
*/



