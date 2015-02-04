:- module(filters, [genFilters/2, 
		writeFilters/2, 
		userFilters/3,
		allTypes/3]).

% JPG - November 2003 - January 2004

:- use_module(library(lists)).
:- use_module(builtins).
:- use_module(tp).
:- use_module('logen/filter').
:- use_module(cartprod).
:- use_module(canonical).
:- use_module(queryGen,[flat_name/2]).
:- use_module(canonical).

genFilters([[(H :- S)]|Ms],[F|Fs]) :-
	queryPred(H),
	!,
	reduceArgs(S,[],S1), 
	pruneDynamicArgs(S1,S2),
	renameBody(S2,F),
	genFilters(Ms,Fs).
genFilters([_|Ms],Fs) :-
	genFilters(Ms,Fs).
genFilters([],[]).

pruneDynamicArgs([S1|Ss],[S2|Ts]) :-
	pruneDynamic(S1,S2),
	pruneDynamicArgs(Ss,Ts).
pruneDynamicArgs([],[]).
	

pruneDynamic(S1,['$VAR'(0)]) :-
	member('$VAR'(_),S1),
	!.
pruneDynamic(S1,S1).

userFilters(Cls,Ts,Fs) :-
	userDecls(Cls,Ds),
	makeUserFilters(Ds,Ts,Fs,[]).
	
makeUserFilters([filter(F)|Decls],Ts,Fs0,Fs2) :-
	!,
	transformFilter(F,Ts,Fs0,Fs1),
	makeUserFilters(Decls,Ts,Fs1,Fs2).
makeUserFilters([_|Decls],Ts,Fs0,Fs1) :-
	makeUserFilters(Decls,Ts,Fs0,Fs1).
makeUserFilters([],_,Fs,Fs).

transformFilter(F,Ts,Fs0,Fs1) :-
	flat_name(query(F),F1),
	F1 =.. [P|Xs],
	makeDisjTypes(Xs,Ts,Ys),
	canonical(Ys),
	cartprod(Ys,Zs),
	filterList(Zs,P,Fs0,Fs1).

%% this code changes Logen types to Johns types

makeDisjTypes([D|Xs],Ts,[[_]|Ys]) :-
	userDynamicType(D),
	!,
	makeDisjTypes(Xs,Ts,Ys).

%%% WARNING THIS IS A STEVE HACK, and as I dont really know whats going on in the code here....
%% perhaps you should think twice before not deleting this....
makeDisjTypes([type(Type)|Xs],Ts,[NewType|Ys]) :-
	makeDisjTypes([Type|Xs],Ts,[NewType|Ys]).
	
/*
makeDisjTypes([type(list(dynamic))|Xs],Ts,[[[dynamic,list,nonvar,static],[dynamic,list,nonvar]]|Ys]) :-
	!,
	makeDisjTypes(Xs,Ts,Ys).
makeDisjTypes([type(list(static))|Xs],Ts,[[[dynamic,list,nonvar,static]]|Ys]) :-
	!,
	makeDisjTypes(Xs,Ts,Ys).
*/
makeDisjTypes([X|Xs],Ts,[Y|Ys]) :-
	allIntersects(Ts,X,Y),
	makeDisjTypes(Xs,Ts,Ys).
makeDisjTypes([],_,[]).

allIntersects([],_,[]).
allIntersects([T|Ts],X,[T|Ys]) :-
	member(X,T),
	!,
	allIntersects(Ts,X,Ys).
allIntersects([_|Ts],X,Ys) :-
	allIntersects(Ts,X,Ys).
	
filterList([As|Zs],P,[clause((F :- true),[])|Fs0],Fs1) :-
	F =.. [P|As],
	filterList(Zs,P,Fs0,Fs1).
filterList([],_,Fs,Fs).

userDynamicType(dynamic).
	
queryPred(H) :-
	functor(H,Q,_),
	name(Q,QN),
	name('_query',X),
	append(_,X,QN).
	
writeFilters(_,[]).
writeFilters(S,[F|Fs]) :-
	%write(S, ':- filter '),
	write(S, F),
	write(S, '.'),
	nl(S),
	writeFilters(S, Fs).
	
renameBody([[B]|Bs],BN) :-
	name('_query', Q),
	name(B,QPN),
	append(PN,Q,QPN),
	name(P,PN),
	BN =.. [P|Bs].

userDecls([predicates(_)|Cls],Ds) :-
	!,
	userDecls(Cls,Ds).
userDecls([clause((filter(F) :- true),_)|Cls],[filter(F)|Ds]) :-
	!,
	userDecls(Cls,Ds).
userDecls([clause((residual(F) :- true),_)|Cls],[residual(F)|Ds]) :-
	!,
	userDecls(Cls,Ds).
userDecls([_|Cls],Ds) :-
	userDecls(Cls,Ds).
userDecls([],[]).
	

allTypes([predicates(_)|Ms],Ts0,Ts1) :-
	allTypes(Ms,Ts0,Ts1).
allTypes([clause((D :- true),_)|Ms],Ts0,Ts1) :- % ignore denotes_symbol (from logenbta)
	functor(D,denotes_symbol,1),
	!,
	allTypes(Ms,Ts0,Ts1).
% assumes that D is of form denotes_f(...T)
allTypes([clause((D :- true),_)|Ms],Ts0,Ts2) :- 
	functor(D,_,N),
	arg(N,D,T),
	addType(T,Ts0,Ts1),
	allTypes(Ms,Ts1,Ts2).
allTypes([],Ts,Ts).

addType(X,Ys,Ys) :-
	member(X,Ys),
	!.
addType(X,Ys,[X|Ys]).

