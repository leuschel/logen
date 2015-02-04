test :-
  statistics(runtime,_), run1([const(7000)]), statistics(runtime,[_,T2]), nl,print(T2),nl.
  


run1([A]) :-
        'run1.1'(A).

% 'run1.1'(A):-run1([A])
'run1.1'(const(A)) :-
        0<A,
        B is A,
        C is A-1,
        'eval_whilebin_op>1'(C, B, D),
        (   D=const(E),
            write('    '),
            write(E),
            nl,
            nl
        ;   D=undef,
            write('    undefined!'),
            nl,
            nl
        ;   D=matrix([[]]),
            write('    []'),
            nl
        ;   D=matrix(F),
            print_rows1(F),
            nl
        ).
'run1.1'(_) :-
        write('    '),
        write(1),
        nl,
        nl.

% 'eval_whilebin_op>1'(A,B,C):-eval_while(bin_op(>,var(x),const(0)),[assign(var(y),bin_op(*,var(y),var(x)),true),assign(var(x),bin_op(-,var(x),const(1)),true)],env([x/const(A),y/const(B)],[function(fact,[y],[x],[x,y],[assign(var(y),const(1),true),while(bin_op(>,var(x),const(0)),[assign(var(y),bin_op(*,var(y),var(x)),true),assign(var(x),bin_op(-,var(x),const(1)),true)])])]),env([x/D,y/C],[function(fact,[y],[x],[x,y],[assign(var(y),const(1),true),while(bin_op(>,var(x),const(0)),[assign(var(y),bin_op(*,var(y),var(x)),true),assign(var(x),bin_op(-,var(x),const(1)),true)])])]))
'eval_whilebin_op>1'(A, B, C) :-
        0<A,
        D is B*A,
        E is A-1,
        'eval_whilebin_op>1'(E, D, C).
'eval_whilebin_op>1'(_, A, const(A)).

% print_rows1(A):-print_rows(A)
print_rows1([]).
print_rows1([A|B]) :-
        print_row1(A),
        nl,
        print_rows1(B).

% print_row1(A):-print_row(A)
print_row1([]).
print_row1([const(A)|B]) :-
        write('    '),
        write(A),
        print_row1(B).
        
        