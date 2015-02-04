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
test__0(term(null,[]),A,A).
test__0(term(cons,[B,term(null,[])]),A,term(cons,[B,A])).
test__0(term(cons,[B,term(cons,[C,D])]),A,term(cons,[B,term(cons,[C,E])])) :-
        solve__1(A,D,E).

/* solve([term(clause,[term(app,[term(null,[]),var(l),var(l)])]),term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),term(cons,[var(h),var(z)])]),term(app,[var(x),var(y),var(z)])])],[term(app,[A,B,C])]) :- solve__1(B,A,C). */
solve__1(A,term(null,[]),A).
solve__1(A,term(cons,[B,C]),term(cons,[B,D])) :-
        solve__1(A,C,D).