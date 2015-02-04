:- module(bin_loader, [load_rules/1, rule/2,rebuild_ann/3]).


:- dynamic filter/2.
:- dynamic gensym__r/1.
:- dynamic program_point_for/2.
:- dynamic filtered/1.
:- dynamic unsafe_pp/1.
%% BTA Stuff
:- op(750,fx, filter).
:- op(1150, fx, type).

/*
assert_ann(OS,(logen(ID,H):-B)) :- !,
	   convert_body(B,LB),
	   portray_clause(OS,':-'(logen(ID,H),LB)).

assert_ann(OS,X) :-
	portray_clause(OS,X).

convert_body((A,B),(NA,NB)) :-
	convert_body(A,NA),
	convert_body(B,NB).

convert_body(logen(Ann,Call), logen(NAnn,Call)) :-
	gensym(PP),
	(unsafe_pp(PP) ->
	    weaken_ann(Ann,NAnn)
	;
	    Ann = NAnn
	).

read_ann(S,OS) :-
	read_term(S,Term, []),
	
	(Term = end_of_file ->
	    true
	;
	    assert_ann(OS,Term),
	    read_ann(S,OS)
	).
*/


%% New version doesnt use gensym but instead passes it around
assert_ann(OS,(logen(ID,H):-B),Sym,NewSym) :- !,
	   convert_body(B,LB,Sym,NewSym),
	   portray_clause(OS,':-'(logen(ID,H),LB)).

assert_ann(OS,X,Sym,Sym) :-
	portray_clause(OS,X).

convert_body((A,B),(NA,NB),Sym,NewSym) :-
	convert_body(A,NA,Sym,Sym1),
	convert_body(B,NB,Sym1,NewSym).

convert_body(logen(Ann,Call), logen(NAnn,Call),PP,NewSym) :-
	%gensym(PP),
	NewSym is PP +1,
	(unsafe_pp(PP) ->
	    weaken_ann(Ann,NAnn)
	;
	    Ann = NAnn
	).
	  
read_ann(S,OS,Sym) :-
	read_term(S,Term, []),
	
	(Term = end_of_file ->
	    true
	;
	    assert_ann(OS,Term,Sym,NewSym),
	    read_ann(S,OS,NewSym)
	).	   

weaken_ann(unfold,memo) :- !.
%weaken_ann(call,rescall) :- !.
weaken_ann(Ann,Ann) :- format('!!! Unable to weaken ~a\n',[Ann]).


assert_unsafe([]).
assert_unsafe([U|T]) :-
	assert(unsafe_pp(U)),
	assert_unsafe(T).

rebuild_ann(OutS,Filename,PP) :-	
	reset,
	assert_unsafe(PP),	
	open(Filename, read,FS),
	%read_ann(FS,OutS),
	read_ann(FS,OutS,0),
	close(FS).
	

%% Bin solve stuff


load_rules(Filename) :-
	reset,
	%seeing(Old),
	%see(Filename),
	open(Filename, read,Stream),
	read_assert(Stream),
	close(Stream).

	%seen,
	%see(Old).	



%% Shared



reset :-
	%op(750,fx, filter),
	clear.

clear :-
	retractall(unsafe_pp(_)),
	retractall(rule(_,_)),
	retractall(filter(_,_)),
	retractall(gensym__r(_)),	
	retractall(program_point_for(_,_)),
	retractall(filtered(_)).

read_assert(Stream) :-
	read_term(Stream,Term, []),
	
	(Term = end_of_file ->
	    true
	;
	    assert_term(Term),
	    
	    read_assert(Stream)
	).


assert_term((logen(_ID,H):-B)) :- !,
       convert_body2list(B,LB),
       assert(rule(H,LB)),
       add_filtered(H).
assert_term((:-filter(Call))):-
                  !,
		  freshCall(Call,NCall),
		  assert(filter(NCall,Call)).
assert_term((:-_)):- !.
assert_term((logen(_ID,H))) :-
       assert(rule(H,[])),
       add_filtered(H).       

add_filtered(Head) :-
	freshCall(Head,NHead),
	filtered(NHead),
	!.

add_filtered(Head) :-
	freshCall(Head,NHead),
	assert(filtered(NHead)).

convert_body2list((Call,B), [ACall| Tail]) :-
	convert_call(Call,ACall),
	convert_body2list(B,Tail).
convert_body2list(Call,[ACall]) :-
	convert_call(Call,ACall).
convert_call(logen(Ann,Call), ACall) :-
	ACall =.. [Ann, PP, Call],
	gensym(PP),freshCall(Call,NCall),
	assert(program_point_for(NCall,PP)).
/*
freshCall1(Call, FCall) :-
	
	Call=..[Func|Args],
	length(Args,L),
	length(NArgs,L),
	FCall =.. [Func|NArgs].
*/

freshCall(Call, FCall) :-
	functor(Call,F,A),
	functor(FCall,F,A).
	%Call=..[Func|Args],
	%length(Args,L),
	%length(NArgs,L),
	%FCall =.. [Func|NArgs].


gensym(X) :-
	gensym__r(X),
	!,
	retract(gensym__r(X)),
	X1 is X +1,
	assert(gensym__r(X1)).

gensym(0) :- assert(gensym__r(1)).







