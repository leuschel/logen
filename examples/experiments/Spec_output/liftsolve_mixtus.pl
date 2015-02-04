/* file: liftsolve.pl */

test :-
  build_data(20000,X), build_data(10,Y),
  statistics(runtime,_),test(X,Y,Z), statistics(runtime,[_,T2]), nl,
  %print(Z),nl,
  print(T2), print(ms),nl.
  
build_data(0,term(null,[])) :- !.
build_data(X,term(cons,[  term(a,[]), TAIL])) :-
  X1 is X-1, build_data(X1,TAIL).
  
test(A, B, C) :-
        test1(A, B, C).

% test1(A,B,C):-test(A,B,C)
test1(A, B, C) :-
        'solve.termclause1'(A, B, C).

% 'solve.termclause1'(A,B,C):-solve([term(clause,[term(app,[term(null,[]),var(l),var(l)])]),term(clause,[term(app,[term(cons,[var(h),var(x)]),var(y),term(cons,[var(h),var(z)])]),term(app,[var(x),var(y),var(z)])])],[term(app,[A,B,C])])
'solve.termclause1'(term(null,[]), A, A).
'solve.termclause1'(term(cons,[A,B]), C, term(cons,[A,D])) :-
        'solve.termclause1'(B, C, D).