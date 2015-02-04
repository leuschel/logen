int(true,_).
int(implies(X,Y),Env) :- int(or(not(X),Y),Env).
int(and(X,Y),Env) :- int(X,Env), int(Y,Env).
int(or(X,_Y),Env) :- int(X,Env).
int(or(_X,Y),Env) :- int(Y,Env).
int(not(X),Env) :- not_int(X,Env).
int(var(X),Env) :- member(X,Env).

not_int(false,_).
not_int(implies(X,Y),Env) :- not_int(or(not(X),Y),Env).
not_int(or(X,Y),Env) :- not_int(X,Env), not_int(Y,Env).
not_int(and(X,_Y),Env) :- not_int(X,Env).
not_int(and(_X,Y),Env) :- int(Y,Env).
not_int(not(X),Env) :- int(X,Env).
not_int(var(X),Env) :- not_member(X,Env).

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).
not_member(_X,[]).
not_member(X,[Y|T]) :- X \= Y,not_member(X,T).

test(X) :- int(implies(var(a),var(b)),[a,X]).