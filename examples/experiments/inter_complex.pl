      



store([],Key,Value,[Key/Value]).
store([Key/_Value2|T],Key,Value,[Key/Value|T]).
store([Key2/Value2|T],Key,Value,[Key2/Value2|BT]) :-
   Key \== Key2,
   store(T,Key,Value,BT).

%:- mode lookup(i,i,o).

lookup(Key,[],_Value) :- format(user,"Could not find ~w.~n",[Key]),fail.
lookup(Key,[Key2/Value2|T],Value) :-
   (Key = Key2
    ->  Value=Value2
    ;   lookup(Key,T,Value)
   ).
   
   

eval([],Env,Env).
eval(skip,Env,Env).
eval(set(X,Val),Env,NewEnv) :- store(Env,X,Val,NewEnv).
eval(print(X),Env,Env) :- lookup(X,Env,ValX), 
   print(ValX),nl.
eval(dec(X),Env,NewEnv) :- lookup(X,Env,ValX), 
   NewVal is ValX-1, store(Env,X,NewVal,NewEnv).
eval(inc(X),Env,NewEnv) :- lookup(X,Env,ValX), 
   NewVal is ValX+1, store(Env,X,NewVal,NewEnv).
eval(if_gt(X,Y,Then,Else),Env,NewEnv) :-
   lookup(X,Env,ValX),
   lookup(Y,Env,ValY),
   (ValX>ValY -> eval(Then,Env,NewEnv) ; eval(Else,Env,NewEnv)).
eval(while_gt(X,Y,Body),Env,NewEnv) :-
   eval_while(X,Y,Body,Env,NewEnv).
eval([B1|B2],Env,NewEnv) :-
  eval(B1,Env,E1),
  eval(B2,E1,NewEnv).
  

eval_while(X,Y,Body,Env,NewEnv) :- 
   lookup(X,Env,ValX),
   lookup(Y,Env,ValY),
  ValX>ValY, !,
  eval([Body,while_gt(X,Y,Body)],Env,NewEnv).
eval_while(_ValX,_ValY,_Body,Env,NewEnv) :- 
  eval(skip,Env,NewEnv).
  
test(X) :-
   Prog = [set(zero,0),set(x,X),set(z,1),while_gt(x,zero,[dec(x),inc(z)]),print(z)],
   eval(Prog,[],Env),
   print(Env),nl.
   





:- test(3).

