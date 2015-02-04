/* file: liftsolve.pl */

test :-
  build_data(20000,X), build_data(10,Y),
  statistics(runtime,_),test__0(X,Y,Z), statistics(runtime,[_,T2]), nl,
  %print(Z),nl,
  print(T2), print(ms),nl.
  
build_data(0,term(null,[])) :- !.
build_data(X,term(cons,[  term(a,[]), TAIL])) :-
  X1 is X-1, build_data(X1,TAIL).
  
/* test(A,B,C) :- test__0(A,B,C). */
test__0(term(null,[]),A,A) :-
        solve__1.
test__0(term(cons,[B,C]),A,term(cons,[B,D])) :-
        solve__2(D,A,C).

/* solve([term(clause,[term(app,[term(null,[]),var(l),var(l)])]),term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),term(cons,[var(h),var(z)])]),term(app,[var(x),var(y),var(z)])])],[]) :- solve__1. */
solve__1.

/* solve([term(clause,[term(app,[term(null,[]),var(l),var(l)])]),term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),term(cons,[var(h),var(z)])]),term(app,[var(x),var(y),var(z)])])],[term(app,[A,B,C])]) :- solve__2(C,B,A). */
solve__2(A,A,term(null,[])) :-
        solve__1.
solve__2(term(cons,[B,C]),A,term(cons,[B,D])) :-
        solve__2(C,A,D).