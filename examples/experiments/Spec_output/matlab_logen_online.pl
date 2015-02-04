test :-
  statistics(runtime,_), run1__0(const(7000)), statistics(runtime,[_,T2]), nl,print(T2),nl.
  


/* run1([A]) :- run1__0(A). */
run1__0(const(A)) :-
        A>0,
        B is 1*A,
        C is A-1,
        C>0,
        D is B*C,
        E is C-1,
        eval_while__1(_,F,D,E),
        print_value__2(F).
run1__0(const(A)) :-
        A>0,
        B is 1*A,
        _ is A-1,
        print_value__2(const(B)).
run1__0(_) :-
        print_value__2(const(1)).

/* eval_while(bin_op(>,var(x),const(0)),[assign(var(y),bin_op(*,var(y),var(x)),true),assign(var(x),bin_op(-,var(x),const(1)),true)],env([x/const(A),y/const(B)],[function(fact,[y],[x],[x,y],[assign(var(y),const(1),true),while(bin_op(>,var(x),const(0)),[assign(var(y),bin_op(*,var(y),var(x)),true),assign(var(x),bin_op(-,var(x),const(1)),true)])])]),env([x/C,y/D],[function(fact,[y],[x],[x,y],[assign(var(y),const(1),true),while(bin_op(>,var(x),const(0)),[assign(var(y),bin_op(*,var(y),var(x)),true),assign(var(x),bin_op(-,var(x),const(1)),true)])])])) :- eval_while__1(C,D,B,A). */
eval_while__1(C,D,B,A) :-
        A>0,
        E is B*A,
        F is A-1,
        eval_while__1(C,D,E,F).
eval_while__1(const(A),const(B),B,A).

/* print_value(A) :- print_value__2(A). */
print_value__2(const(A)) :-
        write('    '),
        write(A),
        nl,
        nl.
print_value__2(undef) :-
        write('    undefined!'),
        nl,
        nl.
print_value__2(matrix([[]])) :-
        write('    []'),
        nl.
print_value__2(matrix(A)) :-
        print_rows__3(A),
        nl.

/* print_rows(A) :- print_rows__3(A). */
print_rows__3([]).
print_rows__3([A|B]) :-
        print_row__4(A),
        nl,
        print_rows__3(B).

/* print_row(A) :- print_row__4(A). */
print_row__4([]).
print_row__4([A|B]) :-
        print_element__5(A),
        print_row__4(B).

/* print_element(A) :- print_element__5(A). */
print_element__5(const(A)) :-
        write('    '),
        write(A).
/* Specialisation time 11.90999999888835 ms (runtime) */