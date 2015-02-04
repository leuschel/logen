:- module(input,[load_file/1, my_clause/2, remember_clause/1]).

:- dynamic my_clause/2.
print_myclause :-
	my_clause(A,B),
	portray_clause(	my_clause(A,B)),
	
	fail.
print_myclause.


reset_input :-
	retractall(my_clause(_,_)).

load_file(F) :-
    retractall(my_clause(_,_)),
	see(F),
	remember_all,
	seen.

remember_all :-
	read(C),
	(
	    C == end_of_file -> true
	;
	    remember_clause(C),
	    remember_all
	).

remember_clause((A :- Body)) :-
        print_residue((A :- Body)),
	!,
        normalize(A,ANorm,As1-As2),
	tuple2list(Body,As2-[]),
	assert(my_clause(ANorm,As1)).

remember_clause(A) :-
	print_residue(A),
        normalize(A,ANorm,Unifs-[]),
	assert(my_clause(ANorm,Unifs)).

print_residue(R) :-
	true.
%	call_residue(portray_clause(A),R),
%	portray_clause(rem(R)).



normalize(Atom,ANorm,DUs) :-
    Atom =.. [Pred|Args],
    normalize_args(Args,VArgs,DUs),
    (Pred == (=) -> NewPred = unify ; NewPred=Pred),	
    ANorm =.. [NewPred|VArgs].

normalize_args([],[],X-X).
normalize_args([T|Ts], [V|Vs], [unify(V,T)|Us]-Us1) :-
    normalize_args(Ts,Vs,Us-Us1).


tuple2list((B,Bs),Bs1-Bs3) :-
	!,
        normalize(B,BNorm,Bs1-[BNorm|Bs2]),
	tuple2list(Bs,Bs2-Bs3).

tuple2list(B,Bs1-Bs2) :-
    normalize(B,BNorm,Bs1-[BNorm|Bs2]).




