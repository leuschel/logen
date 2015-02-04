
 
%% Bin solve but loads from file.  
%% test('../examples/match.pl.ann',R,W)
:- dynamic rule/2.
:- dynamic gensym__r/1.
:- dynamic filtered/1.
:- dynamic program_point_for/1.


solve([]).
solve([unfold(PP,H)|T]) :-
        solve_atom(H),
        solve(T).
solve([call(PP,H)|T]) :-
        solve(T).
solve([memo(PP,H)|T]) :-
	solve_atom(H),
        solve(T).
solve([rescall(PP,H)|T]) :-
        solve(T).


solve_atom(H) :-
	    rule(H,Bdy),
	    solve(Bdy).


generalise(H,static,H,H).
generalise(_,dynamic,_,dyn).
generalise(Call,Struct,GCall,GCall2) :-
    Struct=..[F,A1|RestArgs],
    Call=.. [F|CA],
    l_gen(CA,[A1|RestArgs],GA,GA2),
    GCall =.. [F|GA],
    GCall2 =.. [F|GA2].
    
l_gen([],[],[],[]).
l_gen([H|T],[FH|FT],[GH|GT],[GH2|GT2]) :-
    generalise(H,FH,GH,GH2),
    l_gen(T,FT,GT,GT2).



%% We have found a memo loop to some same program point
%% so generalise and save call info
bin_solve(Func/Arity,[memo(_,H)|_T],H2) :-
	functor(H,Func,Arity),  %call
	filter(H,F),		%call
	generalise(H,F,_H1,H2). %unfold



%% We are searching for loops to PP, we have found another 
%% memo point, we should filter, generalise and descend..
bin_solve(PP,[memo(_,H)|_T],ResCall) :-
	filter(H,F),			%call
	generalise(H,F,H1,_),		%unfold
	memo_solve_atom(PP,H1,ResCall).	%memo

bin_solve(PP,[unfold(_,H)|_],R) :-
	%filter(H,F),			%call
	%generalise(H,F,H1,H2),		%unfold
	bin_solve_atom1(PP,H,R).	%memo

%% Unfold point, so use solve to get substitutions and continue
bin_solve(PP,[unfold(_,H)|T],RecCall) :-
        solve_atom(H),  /*memo solve it and then find recursive calls for T */
        bin_solve(PP,T,RecCall). %unfold

%% Skip over a memo point? following specialisation evaluation...
bin_solve(PP,[memo(_,_)|T],RecCall) :-
        bin_solve(PP,T,RecCall). %unfold

%% add call to residual program and continue
bin_solve(PP,[call(_,Call)|T],RecCall) :-
        call(Call), %rescall
        bin_solve(PP,T,RecCall). % unfold

%% rescall just ignore
bin_solve(PP,[rescall(_,_)|T],RecCall) :-
        bin_solve(PP,T,RecCall). % unfold
        

bin_solve_atom1(PP,H,Rec) :-
	    %filter(H,F), %call
	    %generalise(H,F,H1,H), %unfold
	    rule(H,Bdy), %call
	    bin_solve(PP,Bdy,Rec). %unfold

memo_solve_atom(PP,H2,Rec) :-
	    filter(H,F), %call
	    generalise(H,F,H1,H2), %unfold
	    bin_solve_atom1(PP,H1,Rec). %memo






solve_file(Filename) :-
	use_module(library(system)),
	(file_exists(Filename) -> (
	load_rules(Filename),	
	!,
	%solve_atom(-1,Goal),
	%!,
	filtered(H),
	solve_atom(H),
	functor(H,Func,Arity),
	functor(Rec,Func,Arity),
	pp(Func/Arity),
	PP = Func/Arity,
	%program_point_for(H,PP),
 	%program_point_for(Rec,PP),
	memo_solve_atom(PP,H,Rec)
				  )
	;
	    file_not_found(Filename)).
	


load_rules(Filename) :-
	op(750,fx, filter),
	clear,
	seeing(Old),
	see(Filename),
	read_assert,
	seen,
	see(Old).




clear :-
	retractall(rule(_,_)),
	retractall(filter(_,_)),
	retractall(gensym__r(_)),	
	retractall(program_point_for(_,_)),
	retractall(filtered(_)).



read_assert :-
	read_term(Term, []),
	
	(Term = end_of_file ->
	    true
	;
	    assert_term(Term),
	    read_assert
	).




assert_term((logen(ID,H):-B)) :- !,
       convert_body2list(B,LB),
       assert(rule(H,LB)),
       add_filtered(H).
assert_term((:-filter(Call))):-
                  !,
		  freshCall(Call,NCall),
		  assert(filter(NCall,Call)).
assert_term((:-_)):- !.
assert_term((logen(ID,H))) :-
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

freshCall(Call, FCall) :-
	Call=..[Func|Args],
	length(Args,L),
	length(NArgs,L),
	FCall =.. [Func|NArgs].


gensym(X) :-
	gensym__r(X),
	!,
	retract(gensym__r(X)),
	X1 is X +1,
	assert(gensym__r(X1)).

gensym(0) :- assert(gensym__r(1)).












