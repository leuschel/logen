

%p1([   z,  :=,  1,  ;,
%       while,  x, >, 0, do,
%          z,  :=,  z,  *,  y,  ;,
%          x,  :=,  x,  -, 1, od
%   ]).


%semantics

% p.e. main2:
main2(Vala, Valb, Output) :-  wt(P),
        prog_eval(P, Vala, Valb, Output).

main(Vala, Valb, Output) :-  p1(Prog),
        program(P, Prog, []), prog_eval(P, Vala, Valb, Output).

prog_eval(p(Comm), Vala, Valb, Output) :-
        initialize_store(Store),
        update(x, Vala, Store, Mmidstore),
        update(y, Valb, Mmidstore, Midstore),
        command(Comm, Midstore, Newstore),
        access(z, Newstore, Output).

command(comb(C1, C2), Store, Outstore) :-
        command(C1, Store, Newstore),
        command(C2, Newstore, Outstore).

command(while(B, C), Store, Outstore) :-
        (boolean(B, Store) ->
                command(C, Store, Newstore),
                command(while(B, C), Newstore, Outstore);
                Outstore=Store).

command(ce(B, C1, C2), Store, Outstore) :-
        (boolean(B, Store) ->
                command(C1, Store, Outstore);
                command(C2, Store, Outstore)).

command(assign(I, E), Store, Outstore) :-
        expression(E, Store, Val),
        update(I, Val, Store, Outstore).

expression(add(E1, E2), Store, Result) :-
        expression(E1, Store, Val_E1),
        expression(E2, Store, Val_E2),
        Result is Val_E1+Val_E2.
expression(sub(E1, E2), Store, Result) :-
        expression(E1, Store, Val_E1),
        expression(E2, Store, Val_E2),
        Result is Val_E1-Val_E2.
expression(multi(E1, E2), Store, Result) :-
        expression(E1, Store, Val_E1),
        expression(E2, Store, Val_E2),
        Result is Val_E1*Val_E2.

expression(id(X), Store, Result) :-
        access(X, Store, Result).
expression(num(X), _, X).
 
boolean(greater(E1, E2), Store) :-
        expression(E1, Store, Eval1),
        expression(E2, Store, Eval2),
        Eval1 > Eval2.
boolean(less(E1, E2), Store) :-
        expression(E1, Store, Eval1),
        expression(E2, Store, Eval2),
        Eval1 < Eval2.
boolean(equal(E1, E2), Store) :-
        expression(E1, Store, Eval),!,
        expression(E2, Store, Eval).
 
%initialize_store([(x,0),(z,0),(y,0)]).
%access(Id, [(Id,Val)|_], Val).
%access(Id, [_|R], Val) :- access(Id,R,Val).
%update(Id,NewV,[(Id,_)|R],[(Id,NewV)|R]).
%update(Id,NewV,[P|R],[P|R1]) :- update(Id,NewV,R,R1).
 
 
p1([    z,  :=,  1,  ;,
        while,  x, >, 0, do,
           z,  :=,  z,  *,  y,  ;,
           x,  :=,  x,  -, 1, od
   ]).
 
wt(p(comb(assign(z,num(1)),while(greater(id(x),num(0)),
        comb(assign(z,multi(id(z),id(y))),assign(x,sub(id(x),num(1)))))))).
 
t2(p(comb(assign(z,num(1)),comb(assign(w,id(x)),while(greater(id(w),num(0)),
        comb(assign(z,multi(id(z),id(y))),assign(w,sub(id(w),num(1))))))))).

