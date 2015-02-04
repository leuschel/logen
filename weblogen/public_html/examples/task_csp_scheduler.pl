:- module(newtask,[main/1,main2/1],[]).

/* to specialise do:
logen task_csp_scheduler..pl "main(X)" > logen_out.txt
logen task_csp_scheduler..pl "main2(X)" > logen_out2.txt
*/


:- use_module(library(write)).
%% :- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(sort)).
:- use_package(assertions).
%% :- use_package(nativeprops).

%:- use_module(streamio).

%:- use_module(streamio).

doOutput(Stream,V) :-
    write(Stream), write(' <= '), write(V),nl.

%:- entry main(L) : list(num).

main(In):-
%	ifalt(Program),
	simpleif(Program),
	execute(Program,In).
	
main2(In):-
	ifalt(Program),
%	simpleif(Program),
	execute(Program,In).

simplech([channel(a,1:10),channel(b,1:20)]).
simple([process([input(a,x),output(b,x)])]).
simpleif([process([input(a,x),if(x,output(b,x),output(b,x+10))])]).
simple2([process([input(a,x),write(c,x)]),process([read(c,y),output(b,y)])]).
simple3([process([input(a,x),write(c,x)]),process([read(c,y),write(d,y)]),process([read(d,z),output(b,z)])]).
simple4([process([input(a,x),write(c,x*x)]),process([read(c,y),output(b,y)])]).

alttest([process([input(a,x),write(c,x)]),
         process([input(b,y),write(d,y)]),
         process([alt(c,d,z),output(o,z)])]).

feedback([process([input(a,x),write(c,x),read(f,k)]),
          process([alt(e,c,y),output(o,y),write(d,y),alt(e,c,y),write(f,y)]),
          process([read(d,z),write(e,z)])]).

straight([process([input(a,x),write(c,x),input(a,x),write(d,x)]),
          process([alt(c,d,y),output(o,y)])]).

ifalt([process([input(a,x),if(x,write(c,x),write(d,x))]),
       process([read(c,y),write(e,255)]),
       process([read(d,z),write(f,0)]), 
       process([alt(e,f,r),output(o,r)])]).


%:- check calls update(A,B,C,D) : (ground(A), ground(C), ground(D)).

update(StIn,StOut, Var,Val) :-
    stVars(StIn,VarsI,StIn),
    updateList(VarsI,Var,Val,VarsO),
    stVars(StIn,VarsO,StOut).

%:- check calls updateList(A,B,C,D) : (ground(A), ground(B), ground(C)).

%updateList([],Var,Val,(Var,Val)):-fail.
updateList([(Var2,_)|Vs],Var,Val,Result):-
	Var2 == Var,
	Result = [(Var,Val)|Vs].
updateList([(Var2,Val)|Vs],Var,V,[(Var2,Val)|Vs2]) :-
	Var2 \== Var,
	updateList(Vs,Var,V,Vs2).

%:- check calls stTasks(A,B,C) : (ground(A), (ground(B) ; ground(C))).

stTasks(st(_,Chans,Vars),Tasks,st(Tasks,Chans,Vars)).

%:- check calls stChans(A,B,C) : (ground(A), (ground(B) ; ground(C))).

stChans(st(Tasks,_,Vars),Chans,st(Tasks,Chans,Vars)).

%:- check calls stVars(A,B,C) : (ground(A), (ground(B) ; ground(C))).

stVars(st(Tasks,Chans,_),Vars,st(Tasks,Chans,Vars)).

%% :- trust comp eval(A,B,C) + sideff(free).
%% :- trust comp eval(A,B,C) : (ground(A), ground(C)) + eval.

%:- check calls eval(A,B,C) : (ground(A), ground(C)).

eval(Expr,Expr,_) :-
    num(Expr).

eval(Expr,Value,St) :-
    atom(Expr),
    stVars(St,Vals,St),
    member((Expr,Value),Vals).

eval(T, Value, St) :-
    T =.. [Op,E0,E1],
    eval(E0,V0,St),
    eval(E1,V1,St),
    T2 =.. [Op,V0,V1],
    Value is T2.


%:- trust comp getChannel( StIn,StOut, C, St) + (eval,sideff(free)).

getChannel( StIn,StOut, C, St) :-
    stChans(StIn,Ci,StIn),
    updateCList(Ci,C,blocking,Co,St),
    stChans(StIn,Co,StOut).

%:- trust comp setChannel( StIn,StOut, C, St ) + (eval,sideff(free)).

setChannel( StIn,StOut, C, St ) :-
    stChans(StIn,Ci,StIn),
    updateCList(Ci,C,St,Co,blocking),
    stChans(StIn,Co,StOut).

updateCList([channel(Chan,St)|Vs],Chan,Val,[channel(Chan,Val)|Vs],St).
updateCList([channel(Chan2,Val)|Vs],Chan,V,[channel(Chan2,Val)|Vs2],St):-
    Chan2 \== Chan,
    updateCList(Vs,Chan,V,Vs2,St).

%:- trust comp getReady( StIn,StOut, Task) + (eval,sideff(free)).

%:- check calls getReady(StIn,StOut,Task) : (ground(StIn), ground(Task)).

getReady( StIn,StOut, Task) :-
    stTasks(StIn,[Task|Ts],StIn),
    stTasks(StIn,Ts,StOut).
%% getReady( StIn, StOut, terminated) :-
%%     stTasks(StIn,[],StOut).

%:- trust comp putReady( StIn,StOut, T1)  + (eval,sideff(free)).

%:- check calls putReady(StIn,StOut,T1) : (ground(StIn), ground(T1)).

putReady( StIn,StOut, task(T1,T2)) :-
    stTasks(StIn,Tasks,StIn),
    stTasks(StIn,[task(T1,T2)|Tasks],StOut).
putReady( StIn,StIn, blocked).
%putReady( StIn,StIn, terminated).

%%%%
%:- push_prolog_flag(discontiguous_warnings,off).

int(task([],X),task(X,X),I,I,S,S).

int(task([if(E,T,F)|Is],Loop),task([H|Is],Loop), I,I, Si,Si) :-
    %eval(E,Res,Si),                                        TEST CHANGE
    stVars(Si,Vars,Si),
%%     format('Current streams ~w~n',I),
%%     format('Doing an iffy thang on ~w~n',Vars),
    member((E,Res),Vars),
    select(Res,T,F,H).

int(task([poll(X)|Is],Loop),task(Is,Loop),I,I,Si,So) :-
    update(Si,So,X,1). 

int(task([assign(T,E)|Is],Loop),task(Is,Loop),I,I,Si,So) :-
    eval(E,Val,Si),
    update(Si,So,T,Val).

int(task([output(Stream,E)|Is],Loop),task(Is,Loop), I,I, Si,Si) :-
    eval(E,V,Si),
    doOutput(Stream,V).
   % putOutput(Si,So,Stream,V).
   % format('Output: ~k = ~k',[E,V]),nl.

int(task([read(C,V)|Is],Loop),task(Is,Loop), I,I, Si,So) :-
    getChannel(Si,S0,C,writing(Task,Val)),
    putReady(S0,S1,Task),
    update(S1,So,V,Val).
    %format('read(~w) -> continue\n',C).

int(task([read(C,V)|Is],Loop),blocked, I,I, Si,So) :-
    getChannel(Si,_,C,blocking),
    setChannel(Si,So,C,reading(task(Is,Loop),V)).
    %format('read(~w) -> blocked\n',C).

int(task([write(C,E)|Is],Loop),task(Is,Loop), I,I, Si,So) :-
    getChannel(Si,S0,C,reading(Task,Var)),
    putReady(S0,S1,Task),
    eval(E,V,S1),
    update(S1,So,Var,V).
    %format('write(~w) -> continue\n',C).

int(task([write(C,E)|Is],Loop),blocked, I,I, Si,So) :-
    getChannel(Si,Si,C,blocking),       
    eval(E,V,Si),
    setChannel(Si,So,C,writing(task(Is,Loop),V)).
    %format('task(~w,~w)\n',[[write(C,E)|Is],Loop]),
    %format('write(~w) -> blocking\n',C).

int(task([write(C,E)|Is],Loop),task(Is,Loop), I,I, Si,So) :-
    getChannel(Si,S1,C,alting(Task,Other,Var)),
    putReady(S1,S2,Task),
    eval(E,Val,S2),
    update(S2,S3,Var,Val),
    getChannel(S3,So,Other,alting(Task,C,Var)).
    %setChannel(S3,So,Other,blocked),
    %format('write(~w) -> alting\n',C).

int(task([alt(C,D,V)|Is],Loop), OutTask, I,I, Si,So ) :-
    %write('making alt...'),nl,
    getChannel(Si,S2,C,CState),
    getChannel(S2,S3,D,DState),
    int_alt(CState,DState,C,D,V,Is,Loop,OutTask,S3,So).

int(task([input(Stream,V)|Is],Loop), OutTask, In,In2, Si, So) :-
    advanceInput(In, In2, Val),
    doOutput(Stream,Val),
    update(Si,So,V,Val),    % Might be empty - shouldn't matter        TEST CHANGE
    %update(Si,So,V,0),    % Might be empty - shouldn't matter
%    mapInputOnTask(Val, task(Is,Loop), OutTask).
    OutTask = task(Is,Loop).

%% %mapInputOnTask(empty, _, terminated).
%% mapInputOnTask(Val, Task, Task) :-
%%     num(Val).

advanceInput([stream(a,[X|Xs])], [stream(a,Xs)], X).

select(Val,_,F,F) :- Val =\= 0.
select(Val,T,_,T) :- Val =:= 0.


int_alt(blocking,blocking,C,D,V,Is,Loop,OutTask,S3,So):-
	setChannel(S3,S4,C,alting(task(Is,Loop),D,V)),
	setChannel(S4,So,D,alting(task(Is,Loop),C,V)),
	OutTask = blocked.
        %format('alt(~w,~w) -> both blocked\n',[C,D])
int_alt(blocking,writing(OutTask,Val),_C,_D,V,Is,Loop,OutTask,S3,So):-
	update(S3,S4,V,Val),
        %setChannel(S1,S2,D,blocking),
        putReady(S4,So,task(Is,Loop)).
        %setChannel(S5,So,C,blocking).
        %format('alt ~w=blocked, ~w=continue\n',[C,D])
int_alt(writing(OutTask,Val),DState,_C,D,V,Is,Loop,OutTask,S3,So):-
        update(S3,S4,V,Val),
        %setChannel(S1,S2,C,blocking),
        putReady(S4,S5,task(Is,Loop)),
        setChannel(S5,So,D,DState).
        %format('alt ~w=continue ~w=unknown\n',[C,D])
        
        
%:- pop_prolog_flag(discontiguous_warnings).

%%

intloop(Si,In) :-
    getReady(Si,S1,Task),
    int(Task,Task2,In,In2,S1,S2),
    putReady(S2,S3,Task2),
%%     intLoopRepeat(Task2,In2,S3).
%% 
%% intLoopRepeat(terminated,_In,_Si).
%% intLoopRepeat(Task,In,Si) :-
%%     Task \== terminated,
    intloop(S3,In2).


%%%% Initialisation %%%%%%%%%%%%%%%

exprVars(X,[]) :-
	num(X).
exprVars(X,[(X,-1)]) :-
	atom(X).
exprVars(T,Vs) :-
	T =.. [_,E1,E2],
	exprVars(E1,V1),
	exprVars(E2,V2),
	append(V1,V2,Vs).

taskVars([],[]).
taskVars([input(_,X)|Prog],[(X,-1)|Prog2]) :-
	taskVars(Prog,Prog2).
taskVars([write(_,E)|Prog],Vars) :-
	exprVars(E,Vars1),
	taskVars(Prog,Vars2),
	append(Vars1,Vars2,Vars).
taskVars([read(_,X)|Prog], [(X,-1)|Prog2]) :-
	taskVars(Prog,Prog2).
taskVars([assign(V,E)|Prog],Vars) :-
	exprVars(E,Vars1),
	taskVars(Prog,Vars2),
	append([V|Vars1],Vars2,Vars).
taskVars([output(_,E)|Prog],Vars) :-
	exprVars(E,Vars1),
	taskVars(Prog,Vars2),
	append(Vars1,Vars2,Vars).
taskVars([alt(_,_,V)|Prog],[(V,-1)|Vars]) :-
        taskVars(Prog,Vars).
taskVars([if(E,T,F)|Prog],Vars) :-
        exprVars(E,Vars1),
        taskVars([T],Vars2),
        taskVars([F],Vars3),
        taskVars(Prog,Vars4),
        append(Vars1,Vars2,Vars5),
        append(Vars5,Vars3,Vars6),
        append(Vars6,Vars4,Vars).
        

vars([],[]).
vars([process(X)|Ts],Set) :-
	taskVars(X,Xs),
	vars(Ts,Ys),
	append(Xs,Ys,List),
	sort(List,Set).

taskChannels([],[]).
taskChannels([input(_,_)|P],P2) :- taskChannels(P,P2).
taskChannels([write(C,_)|P],[channel(C,blocking)|P2]) :- taskChannels(P,P2).
taskChannels([read(C,_)|P],[channel(C,blocking)|P2]) :- taskChannels(P,P2).
taskChannels([assign(_,_)|P],P2) :- taskChannels(P,P2).
taskChannels([output(_,_)|P],P2) :- taskChannels(P,P2). 
taskChannels([alt(C,D,_)|P],[channel(C,blocking),channel(D,blocking)|P2]) :- taskChannels(P,P2).
taskChannels([if(_C,T,F)|P],Cs) :-
    taskChannels([T],Cs1),
    taskChannels([F],Cs2),
    taskChannels(P,Cs3),
    append(Cs1,Cs2,Cs4),
    append(Cs4,Cs3,Cs).

channels([],[]).
channels([process(X)|Ts],Set) :-
	taskChannels(X,Xs),
	channels(Ts,Ys),
	append(Xs,Ys,List),
	sort(List,Set).

tasks([],[]).
tasks([process(X)|Ps],[task([],X)|Ts]) :-
	tasks(Ps,Ts).


initState(X,st(T,C,V)) :-
	tasks(X,T),
	vars(X,V),
	channels(X,C).

execute(X,I) :-
	initState(X,St),
        wrapStreams(I,In2),
        intloop(St,In2).

wrapStreams(In, Wrap) :-
    Wrap = [stream(a,In)].

%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

