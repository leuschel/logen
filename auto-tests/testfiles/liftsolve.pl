/* file: liftsolve.pl */


test(X,Y,Z) :-
  solve([
	 term(clause,[term(app,[term(null,[]),var(l),var(l)]) ]),
	 term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),
					term(cons,[var(h),var(z)])]),
		term(app,[var(x),var(y),var(z)]) ])
		],
	[term(app,[X,Y,Z])]).

/* --------------------- */
/* solve(GrRules,NgGoal) */
/* --------------------- */


solve(_GrRules,[]).
solve(GrRules,[NgH|NgT]) :-
	non_ground_member(term(clause,[NgH|NgBody]),GrRules),
	   /* print(matching_clause(NgH,NgBody)),nl, */
	solve(GrRules,NgBody),
	solve(GrRules,NgT).

/* -------------------------------------- */
/* non_ground_member(NgExpr,GrListOfExpr) */
/* -------------------------------------- */

non_ground_member(NgX,[GrH|_GrT]) :-
	make_non_ground(GrH,NgX).
non_ground_member(NgX,[_GrH|GrT]) :-
	non_ground_member(NgX,GrT).


/* --------------------------------------------------------- */
/* make_non_ground(GroundRepOfExpr,NonGroundRepOfExpr) */
/* --------------------------------------------------------- */

/* ex. ?-make_non_ground(pos(term(f,[var(1),var(2),var(1)])),X). */

make_non_ground(G,NG) :-
	mkng(G,NG,[],_Sub).


mkng(var(N),X,[],[sub(N,X)]).
mkng(var(N),X,[sub(N,X)|T],[sub(N,X)|T]).
mkng(var(N),X,[sub(M,Y)|T],[sub(M,Y)|T1]) :-
	N \== M,
	mkng(var(N),X,T,T1).
mkng(term(F,Args),term(F,IArgs),InSub,OutSub) :-
	l_mkng(Args,IArgs,InSub,OutSub).
l_mkng([],[],Sub,Sub).
l_mkng([H|T],[IH|IT],InSub,OutSub) :-
	mkng(H,IH,InSub,IntSub),
	l_mkng(T,IT,IntSub,OutSub).

