:- module(alpha3, [ runtest/2, eval/6]).

%--------------------------------------------------------------------
%-- helper predicates -----------------------------------------------

update_store([], Object, FieldName, NewValue, [] ) :-
  format(user,"*** Runtime Error, Object ~w not found.~n*** Could not update field ~w to ~w.~n",
         [Object,FieldName,NewValue]).
update_store([dyn_obj(ObjectID, ClassName, Fields)|R], Object, FieldName, NewValue,
             [dyn_obj(ObjectID, ClassName, NewFields)|NewRest] ) :-
    (ObjectID = Object
      -> (update_field(Fields, FieldName, NewValue, NewFields), NewRest=R)
      ; (NewFields=Fields,
         update_store(R,Object, FieldName, NewValue, NewRest))
    ).
	
update_field([], FieldName, Value,[]) :-
  format(user,"*** Runtime Error, Field ~w not found.~n*** Could not update field to ~w.~n",
         [FieldName,Value]).
update_field([dyn_field(DynFieldName,OldVal)|R], FieldName, Value,
             [dyn_field(DynFieldName,NewVal)|NewRest]) :-
        (DynFieldName=FieldName
          -> (NewVal=Value, NewRest=R)
          ;  (NewVal=OldVal,update_field(R, FieldName, Value, NewRest))
        ).

class_of(Ast, Store, Object, Class) :-
	member(class(Class,_,_,_,_), Ast),
	member(dyn_obj(Object, Class, _), Store).

method_body(Ast, ClassName, MethodName, Body) :-
	member(class(ClassName, _, _, Methods, _), Ast),
	member(meth(_,MethodName,_,Body), Methods).

init_val(Type, Value ) :-
	(   Type = bool -> Value = false; Value = null ).

create_field_list([],[]).
create_field_list([field(Type,Name)|RestFields], FieldList) :-
	create_field_list(RestFields, RestList),
	init_val(Type, InitValue),
	FieldList = [ dyn_field(Name, InitValue) | RestList ].

super_class(Ast, ClassName, SuperClass) :- 
	member( class(ClassName, SuperClass, _, _, _), Ast).

init_instance_fields(Ast, ClassName, FieldList) :-
	member(class(ClassName, _, Fields, _,_), Ast),
	create_field_list(Fields,FieldList).

init_fields(_Ast, 'object', [] ).
init_fields(Ast, ClassName, FieldList ) :-
	init_instance_fields(Ast,ClassName,Fields),
	super_class(Ast,ClassName,SuperClass),
	init_fields(Ast,SuperClass, SuperFields),
	append(Fields,SuperFields,FieldList).
	

%--------------------------------------------------------------------
%-- aspect handling -------------------------------------------------

aspects(Ast,Aspects) :-
	member(class(main,_,_,_,Aspects), Ast).


execute_advice( _When, _Ast, _Trace, [], _Env,InState, StateR ) :- StateR = InState.
execute_advice( When, Ast, Trace, [aspect(ID)|Aspects], Env,InState, StateR ) :-
    (aspect(ID,When,Ast,Trace,Advice)
     ->  eval(Ast,Env,InState,Advice,StateI,_Value)
     ;   (StateI = InState)
    ),
    execute_advice(When,Ast,Trace,Aspects,Env,StateI,StateR).
     



before( Ast, Env, state(Store,Trace), StateR ) :-
	on_event(before, Ast, Env, state(Store,Trace), StateR).
after( Ast, Env, state(Store,Trace), StateR ) :-
	on_event(after, Ast, Env, state(Store,Trace), StateR).

on_event( When, Ast, Env, state(Store, Trace), StateR ) :-
    print(on_event(When,Env,Trace)),nl,
	aspects( Ast, Aspects ),
	(execute_advice(When,Ast,Trace,Aspects,Env,state(Store,Trace),StateR) -> true
	  ; (print('### FAIL: '),println_quoted(execute_advice(When,Ast,Trace,Aspects,Env,state(Store,Trace),StateR)),nl,
	     StateR = state(Store,Trace))
	).
	
	
aspect(1,before, _, TraceV, ('',print('advice'))) :- TraceV=[calls(_,_,foo,_)|_].
aspect(2,before, _, TraceV, ('',print('before-new'))) :- TraceV=[new(_,_)].
aspect(3,after,  _, TraceV, ('',print('after-new'))) :- TraceV=[new(_,_)].
		   
aspect(6,before, _, TraceV, ('',print(X))) :- TraceV=[new(X,_)|_].
		   
%--------------------------------------------------------------------
% -- Interpreter ----------------------------------------------------

% eval: Ast x State x (Token x Expr) -> State x Value
eval( Ast, Env, State, (Token, Expr), StateR, Value ) :-
    print(eval(Expr)),nl,
	(eval(Token, Ast, Env, State, Expr, StateR, Value)
	  -> (print(eval_res(Expr,Value)),nl)
	  ;  (print('### FAIL: '), println_quoted(eval( Ast, Env, State, (Token, Expr), StateR, Value )),nl, fail)
	).

% eval: Token x Ast x State x Expr -> State x Value
eval( _Token, _Ast, _Env, State, true, State, true ).
eval( _Token, _Ast, _Env, State, false, State, false ).
eval( _Token, _Ast, _Env, State, null, State, null ).

eval( _Token, Ast, Env, State, seq(E1,E2), StateR, Value ):-
	eval(Ast, Env, State, E1, State1, _),
	eval(Ast, Env, State1, E2, StateR, Value).


eval( _Token, _Ast, env(x(Value),_), State, x, State, Value ).
eval( _Token, _Ast, env(_,this(Value)), State, this, State, Value ).

eval( Token, Ast, Env, state(Store, Trace), call(RecExpr, MethodName,ArgExpr), StateR, Value ) :-
	% evaluate receiver and argument
	eval( Ast, Env, state(Store, Trace), RecExpr, State1, RecVal ),
	eval( Ast, Env, State1, ArgExpr, state(Store2,Trace2), ArgVal ),
	% create new environment
	Env1 = env(x(ArgVal), this(RecVal)),
	% lookup body
	class_of(Ast, Store2, RecVal, Class),
	method_body(Ast, Class, MethodName, Body),
	% create & handle event
	Trace3 = [calls(Token,RecVal,MethodName,ArgVal)|Trace2],
	on_event( _, Ast, Env, state(Store2, Trace3), State5 ),
	% execute method
	eval( Ast, Env1, State5, Body, state(Store6,Trace6), Value ),
	% create & handle (end-) event
	Trace7 = [endCall(Token,RecVal,MethodName,ArgVal)|Trace6],
	on_event( _, Ast, Env, state(Store6,Trace7), StateR ).


eval( _Token, Ast, Env, State, if(Cond, Then, Else), State2, Value ) :-
	eval( Ast, Env, State, Cond, State1, CondValue ),
	(   (   CondValue == true , Expr=Then ) ; (CondValue==false, Expr=Else) ),
	eval( Ast, Env, State1, Expr, State2, Value ).

eval( _Token, Ast, _Env, state(Store, Trace), new(Class), StateR, Object) :-
	gensym('iota',Object),
	init_fields(Ast, Class, Fields),
	Trace1 = [new(Class, Object)|Trace],
	before(Ast, Env, state(Store,Trace1), state(Store1,Trace2) ),
	Store2 = [ dyn_obj(Object, Class, Fields) | Store1 ],
	after(Ast, Env, state(Store2,Trace2), StateR).

eval( _Token, _Ast, _Env, State, print(S), State, null ) :-
	write(S), nl.

eval( Token, Ast, Env, state(Store,Trace), get(RecExpr, FieldName), StateR, Value ) :-
	eval( Ast, Env, state(Store, Trace), RecExpr, state(Store1, Trace1), RecVal ),
	member( dyn_obj(RecVal,_, Fields), Store1 ),
	member( dyn_field(FieldName,Value), Fields ),
	Trace2 = [get(Token,RecVal,FieldName,Value)|Trace1],
	on_event( _, Ast, Env, state(Store1, Trace2), StateR ).

eval( Token, Ast, Env, state(Store, Trace), set(RecExpr, FieldName, ValueExpr), StateR, Value ) :-
	eval( Ast, Env, state(Store, Trace), RecExpr, state(Store1, Trace1), RecVal ),
	eval( Ast, Env, state(Store1, Trace1), ValueExpr, state(Store2, Trace2), Value ),
	Trace3 = [set(Token,RecVal, FieldName, Value) | Trace2],
	before(Ast,Env,state(Store2,Trace3), state(Store4,Trace4) ),
	update_store(Store4, RecVal, FieldName, Value, Store5 ),
	after(Ast,Env,state(Store5,Trace4), StateR).

%--------------------------------------------------------------------
%-- Tests -----------------------------------------------------------

runtest(1,Result) :-
	Ast = [class(main, 'object', [field(bool,i)],[],[])],
	Expr = ('',new(main)),
	eval(Ast, [], state([],[]), Expr, ResultState, ResultValue),
	Result = (ResultState, ResultValue).

runtest(2,Result) :-
	FooMethod = meth(main, foo, bool, ('',this)),
	Ast = [class(main, 'object', [field(bool,i)],[FooMethod],[])],
	Env = [],
	Store = [],
	Trace = [],
	Expr = ('',call(('',new(main)),foo,('',true))),
	eval(Ast, Env, state(Store,Trace), Expr, ResultState, ResultValue),
	Result = (ResultState, ResultValue).
	
runtest(3,Result) :-
	FooMethod = meth(main, foo, bool, ('', set(('',this) ,i,('',true) )) ),
	Ast = [class(main, 'object', [field(bool,i)],[FooMethod],[])],
	Env = [],
	Store = [],
	Trace = [],
	Expr = ('',call(('',new(main)),foo,('',true))),
	eval(Ast, Env, state(Store,Trace), Expr, ResultState, ResultValue),
	Result = (ResultState, ResultValue).

runtest(4,Result) :-
	FooMethod = meth(main, foo, bool, ('',this)),
	Aspects = [
		   aspect(1),  aspect(2),  aspect(3) 
		  ],
	Ast = [class(main, 'object', [field(bool,i)],[FooMethod],Aspects)],
	Env = [],
	Store = [],
	Trace = [],
	Expr = ('',call(('',new(main)),foo,('',true))),
	eval(Ast, Env, state(Store, Trace), Expr, ResultState, ResultValue),
	Result = (ResultState, ResultValue).

runtest(5,Store) :-
	Ast = [class(point, 'figure', [field(bool,xx)],[],[]), 
	       class(figure, object, [field(bool,draw)],[],[]),
	       class(main, object, [],[],[])],
	eval( Ast, [], state([],[]), ('',new(point)), state(Store,_), _).

runtest(6,_Value) :- 
	FooMethod = meth(main, foo, bool, ('',this)),
	Aspect = [
		   aspect(6)
		  ],
	Ast = [class(main, 'object', [field(bool,i)],[FooMethod],Aspect)],
	Env = [],
	Store = [],
	Trace = [],
	Expr = ('',new(main)),
	eval(Ast, Env, state(Store, Trace), Expr, _, _Value).



%runtest(Nr, Result) :-
%	concat_atom(['test', Nr], F),
%	Term =.. [F, Result],
%	call(Term).






% ----------------------------------------------------------------------------------


:- use_module(library(lists)).


oldvalue(Prefix,N) :- flag(gensym(Prefix),N),!.
oldvalue(_Prefix,0).

set_flag(Name,X) :-
	nonvar(Name),
	retract(flag(Name,_Val)),!,
	asserta(flag(Name,X)).
set_flag(Name,X) :-
	nonvar(Name),
	asserta(flag(Name,X)).

:- dynamic flag/2.
flag(foo,foo) :- fail.

gennum(Prefix,N1) :- 
	var(N1),
	atom(Prefix),
	oldvalue(Prefix,N),
	N1 is N + 1,
	set_flag(gensym(Prefix),N1).

gensym(Prefix,V) :-
	gennum(Prefix,N1),
	name(PE_Sep,"__"),
	string_concatenate(Prefix,PE_Sep,PreSep),
	string_concatenate(PreSep,N1,V).
	
	
%:- mode string_concatenate(i,i,o).
string_concatenate(X,Y,XY) :-
   name(X,Xs),name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).


println_quoted(X) :- write_term(X,
                            [quoted(true),max_depth(0),
                             numbervars(true),portrayed(true)]),nl.




:- Nr=2, runtest(Nr,Result), print(runtest(Nr,Result)),nl.



