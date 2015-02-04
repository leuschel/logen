
 
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
        call(H),
%	call(PP,H),
        solve(T).
solve([memo(PP,H)|T]) :-
	solve_atom(H),
        solve(T).
solve([rescall(PP,H)|T]) :-
%	call(PP,H),
        solve(T).


solve_atom(H) :-
	    rule(H,Bdy),
	    solve(Bdy).


generalise(H,static,H).
generalise(_,dynamic,_).
generalise(Call,Struct,GCall) :-
    Struct=..[F,A1|RestArgs],
    Call=.. [F|CA],
    l_gen(CA,[A1|RestArgs],GA),
    GCall =.. [F|GA].
    
l_gen([],[],[]).
l_gen([H|T],[FH|FT],[GH|GT]) :-
    generalise(H,FH,GH),
    l_gen(T,FT,GT).

bin_solve(PP,[unfold(PP,H)|_T],H) :-
	via(PP).

bin_solve(PP,[memo(PP,H)|_T],memo(H1)) :-
	filter(H,F),
	generalise(H,F,H1).
bin_solve(PP,[unfold(PP1,H)|_T],RecCall) :-
	via(PP1),
        bin_solve_atom(PP,H,RecCall).
bin_solve(PP,[unfold(_,H)|T],RecCall) :-
        solve_atom(H),  /* solve it and then find recursive calls for T */
        bin_solve(PP,T,RecCall).
bin_solve(PP,[memo(_,_)|T],RecCall) :-
        bin_solve(PP,T,RecCall).
bin_solve(PP,[call(_,Call)|T],RecCall) :-
        call(Call),
        bin_solve(PP,T,RecCall).
bin_solve(PP,[rescall(_,_)|T],RecCall) :-
        bin_solve(PP,T,RecCall).
        
via(X).        

bin_solve_atom(PP,H,Rec) :-
	    rule(H,Bdy),
		bin_solve(PP,Bdy,Rec).


test(H,Rec) :-
	        filtered(H),
	        program_point_for(H,PP),
                program_point_for(Rec,PP),
		solve_atom(H),
		bin_solve_atom(PP,H,Rec).

test(Filename,H,Rec) :- load_rules(Filename),
	!,test(H,Rec).

solve_file(Filename) :-
%	use_module(library(system)),
%	(file_exists(Filename) -> (
	load_rules(Filename),	
	!,
	%solve_atom(-1,Goal),
	%!,
	filtered(H),
	solve_atom(H),
	program_point_for(H,PP),
        program_point_for(Rec,PP),
	bin_solve_atom(PP,H,Rec).

%				  )
%	;
%	    file_not_found(Filename)).
	


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













