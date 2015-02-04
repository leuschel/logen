:- module(tp, [tp/2, tpf/3,
	tpr/3, 
	tpr1/3,
	tp_fix/4, 
	display_fix/1, 
	displayClauses/2,
	reduceArgs/3,
	tsolve/3,
	setunion/3,
	test/0,
	benchtest/0
	]).

% JPG - April 95 (revised from earlier versions back to 1992)
% Compute lfp(T_P)
% For a description of the algorithm,
% see Gallagher :  "A Bottom-UP Analysis Toolkit"  WAILL, Eilat June 1995.
% http://www.cs.bris.ac.uk/Tools/Reports/Ps/1995-gallagher.ps.gz

% Modified, optimised and ported to Ciao June 2003

:- use_module(library(lists)).
:- use_module(builtins).
:- use_module(scc).
:- use_module(balanced_tree).
:- use_module(readprog).
:- use_module(canonical).
:- use_module(cartprod).
/*%%%CIAO
:- use_module(timer_ciao).
*/%%%CIAO
%%%SICS/*
:- use_module(timer_sics).
%%%SICS*/
:- use_module(myterms).

benchtest :-
      write(user_output,'Tests/gpeep.pl'), nl(user_output),
	start_time,
	tpf('Tests/gpeep.pl',[],_),
	end_time('Analysis time: ',user_output),
	write(user_output,'Tests/gchat.pl'), nl(user_output),
	start_time,
	tpf('Tests/gchat.pl',[],_),
	end_time('Analysis time: ',user_output),
	write(user_output,'Tests/maqu.pl'), nl(user_output),
	start_time,
	tpf('Tests/maqu.pl',[],_),
	end_time('Analysis time: ',user_output).

test :-
	write(user_output,'Tests/pqr.pl'), nl(user_output),
	start_time,
	tp('Tests/pqr.pl',[]),
	end_time('Analysis time: ',user_output),
	write(user_output,'Tests/graph1.pl'), nl(user_output),
	start_time,
	tp('Tests/graph1.pl',[]),
	end_time('Analysis time: ',user_output),
	write(user_output,'Tests/groundrev.pl'), nl(user_output),
	start_time,
	tp('Tests/groundrev.pl',[]),
	end_time('Analysis time: ',user_output),
	write(user_output,'Tests/listrev.pl'), nl(user_output),
	start_time,
	tp('Tests/listrev.pl',[]),
	end_time('Analysis time: ',user_output),
	write(user_output,'Tests/press1.pl'), nl(user_output),
	start_time,
	tp('Tests/press1.pl',[]),
	end_time('Analysis time: ',user_output).

tpf(F,Import,FixList) :-
	%start_time,
	readprog(F,Cls),
	sortClauses(Cls,Ps,Prog),
	updateDefs(Import,root,D1),
	equalsDefn(E),
	updateDefs([E],D1,D2),
	tp_fix(Ps,Prog,D2,Fix),
	traverseVal_tree(Fix,FixList).
	
equalsDefn([((X=Y) :- [(U=U)])]) :-
	canonical((X=Y)),
	canonical(U).

tp(F,Import) :-
	tpf(F,Import,FixList),	
	display_fix(FixList).

tpr(Cls,Import,FixList) :-
	sortClauses(Cls,Ps,Prog),
	updateDefs(Import,root,D1),
	equalsDefn(E),
	updateDefs([E],D1,D2),
	tp_fix(Ps,Prog,D2,Fix),
	traverseVal_tree(Fix,FixList).
	
	
tpr1(Cls,Import,Fix) :-
	sortClauses(Cls,Ps,Prog),
	updateDefs(Import,root,D1),
	equalsDefn(E),
	updateDefs([E],D1,D2),
	tp_fix(Ps,Prog,D2,Fix).

tp_fix(Ps,Prog,Import,F) :-
	scc(Ps,Prog,G),% compute the dependency ordering
	%write(G),nl,
	tp_fix_groups(G,Prog,Import,F).

% tp_fix_groups(X,Prog,Y,Z): X is a list of SCCs.  Y is the current approx.
% Z is the final result.

tp_fix_groups([],_,F,F) :-
	!.
% case where the SCC is non-recursive

tp_fix_groups([(R,[G])|Gs],Prog,F,F2) :-
	nonrec(R),
	%write(user_output,G),nl(user_output),
	!,
	init([G],R0),
	user_clauses(Prog,G,Cls),
	tp_alpha([Cls],iterate(0,[]),F,R0,R2),  % no need to iterate if non-rec.
	pruneeachset(R2,R3),
	updateDefs(R3,F,F1),
	tp_fix_groups(Gs,Prog,F1,F2).

% case where the SCC is recursive

tp_fix_groups([(_,G)|Gs],Prog,F,F2) :-
	%write(user_output,G),nl(user_output),
	user_procs(G,Prog,Procs),
	init(G,R0),
	tp_iterate(iterate(0,[]),Procs,G,F,R0,R2),
	updateDefs(R2,F,F1),
	tp_fix_groups(Gs,Prog,F1,F2).

nonrec(non_recursive).

user_procs([],_,[]).
user_procs([P|Ps],Prog,[Cls|Procs]) :-
	user_clauses(Prog,P,Cls),
	user_procs(Ps,Prog,Procs).
	
init([_|Ps],[[]|R]) :-
	init(Ps,R).
init([],[]).

% tp_iterate(A,C,D,E,F,G):  finds the solution for a single SCC
%	A: list of predicates that changed on previous iteration.
%	C: solutions for the temp goals
%	D: list of predicates in the SCC
%	E: accumulated solution of all lower SCCs
%	F: current solution for this SCC
%	G: final solution for this SCC

tp_iterate(iterate(N,[]),_,_,_,R,R) :-
	N > 0,
	!.
tp_iterate(iterate(N,Nofix),Procs,Ps,Acc,R1,R) :-
	%write(R1),nl,
	updateDefs(R1,Acc,D),
	tp_alpha(Procs,iterate(N,Nofix),D,R1,R3),
	pruneeachset(R3,R2),
	check_fix(Ps,R1,R2,Fix),
	N1 is N+1,
	!,
	tp_iterate(iterate(N1,Fix),Procs,Ps,Acc,R2,R).

updateDefs([],D,D).
updateDefs([[(H :- S)]|R1],D0,D2) :-
	functor(H,P,N),
	search_replace_tree(D0,P/N,_,D1,[(H :- S)]),
	!,
	updateDefs(R1,D1,D2).
updateDefs([[(H :- S)]|R1],D0,D2) :-
	!,
	functor(H,P,N),
	insert_tree(D0,P/N,[(H :- S)],D1),
	updateDefs(R1,D1,D2).
updateDefs([[]|R1],D0,D1) :-
	updateDefs(R1,D0,D1).

setintersect([],_,[]).
setintersect([X|Xs],Vs,[X|Ws]) :-
	memb3(X,Vs),
	!,
	setintersect(Xs,Vs,Ws).
setintersect([_|Xs],Vs,Ws) :-
	setintersect(Xs,Vs,Ws).
	
setunion([],X,X).
setunion([X|Xs],Vs,Ws) :-
	memb3(X,Vs),
	!,
	setunion(Xs,Vs,Ws).
setunion([X|Xs],Vs,[X|Ws]) :-
	setunion(Xs,Vs,Ws).

pruneeachset([],[]).
pruneeachset([P|Ps],[R|Rs]) :-
	pruneset(P,R),
	pruneeachset(Ps,Rs).
	
pruneset([],[]).
pruneset([(S :- U1)],[(S :- U)]) :-
	removeDupls(U1,U2),
	pruneAtoms(U2,U),
	canonical_each(U).
	

	
% tp_alpha(X,Z,U,V,W): does one iteration of T_P function applied to one SCC
%	X: list of procedures for one SCC (output of pre-compute)
%	Z: list of predicates that changed on previous iteration 
%	   (for semi-naive)
%	U: current approx. for all predicates
%	V: current approxs for this SCC
%	W: output of this iteration.

tp_alpha([],_,_,[],[]).
tp_alpha([Cls|Ps], Nofix,R,[PR|PRs],[UP1|R1]) :-
	infer_proc(Cls,Nofix,R,As),
	ub_proc(As,PR,UP1),
	tp_alpha(Ps,Nofix,R,PRs,R1).


	
infer_proc([(_:-B)|Cls],Nofix,R,As) :-% if no predicate in the body
	nochange(B,Nofix),			% changed, skip clause
	!,
	infer_proc(Cls,Nofix,R,As).
infer_proc([Cl|Cls],Nofix,R,[S|As]) :-
	melt(Cl,(H:-B)),
	tsolve(B,R,T),
	elimTrue(T,TM),
	localVars(H,B,Zs),
	indexBody(TM,T1,1,K),
	bodyMap(Zs,T1,BMap),
	headtype(H,T1,S,BMap,K), 	
	!,
	infer_proc(Cls,Nofix,R,As).
infer_proc([_|Cls],Nofix,R,As) :-
	infer_proc(Cls,Nofix,R,As).
infer_proc([],_,_,[]).

elimTrue((true,Bs),Bs1) :-
	!,
	elimTrue(Bs,Bs1).
elimTrue((B,Bs),(B,Bs1)) :-
	!,
	elimTrue(Bs,Bs1).
elimTrue(B,B).

localVars(H,B,Zs) :-
	vars(H,Us),
	vars(B,Vs),
	setdiff(Vs,Us,Zs).
	
indexBody((member(B,Ss),Bs),[atom(N,L,member(B,Ss))|Bs1],N,M) :-
	!,
	N1 is N+1,
	length(Ss,L),
	indexBody(Bs,Bs1,N1,M).
indexBody(true,[],N,N) :-
	!.
indexBody(member(B,Ss),[atom(N,L,member(B,Ss))],N,N) :-
	length(Ss,L).
	
bodyMap([],_,[]).
bodyMap([V|Vs],B,[varmap(V,As,Ws)|BMap]) :-
	findV(B,V,As,Ws,1),
	bodyMap(Vs,B,BMap).
	
findV([atom(N,_,member(B,_))|Bs],V,[N|Ns],Ws,N) :-
	vars(B,Xs),
	memb3(V,Xs),
	!,
	N1 is N+1,
	findV(Bs,V,Ns,Ws1,N1),
	setunion(Xs,Ws1,Ws).
findV([_|Bs],V,Ns,Ws,N) :-
	!,
	N1 is N+1,
	findV(Bs,V,Ns,Ws,N1).
findV([],_,[],[],_).
	


	
setdiff([],_,[]).
setdiff([X|Xs],Ys,Zs) :-
	memb3(X,Ys),
	!,
	setdiff(Xs,Ys,Zs).
setdiff([X|Xs],Ys,[X|Zs]) :-
	setdiff(Xs,Ys,Zs).


nochange(B,iterate(N,Nofix)) :-
	N > 0,		% always evaluate the first (N=0) iteration
	preds(B,Ps),
	\+ commonpred(Ps,Nofix).

preds(true,[]) :-
	!.
preds((B,Bs), [P/N|Ps]) :-
	!,
	functor(B,P,N),
        preds(Bs,Ps).
preds(B,[P/N]) :-
	functor(B,P,N).

commonpred(X,Y) :-
	member(Z,X),
	member(Z,Y).
	
% tsolve(A,B,C):
%	A: clause body (only predicates in current SCC)
%	B: current approximation for all predicates
%	C: "solved" clause body.
%	
% example:  let A ; (p(X),q(X))
%	let B = [[p(a),p(b)], [q(d),q(b),q(e)]]
%	output C = (member(p(X),[p(a),p(b)]), member(q(X), [q(d),q(b),q(e)]))

tsolve(true,_,true) :-
	!.
tsolve((B,Bs),R,(T1,T2)) :-
	!,
	tsolve(B,R,T1),
	tsolve(Bs,R,T2).
tsolve(B,R,member(B,T)) :-
	functor(B,S,N),
	headclause(S/N,R,(_ :- T1)),
	!,
	melteach(T1,T).
tsolve(B,_,true) :-	% builtins assumed to succeed with no binding if no defn.
       sp_builtin(B).

% headtype(A,B,C):  projects solution from body to head of clause
%	A: clause head
%	B: solution for body (output from tsolve)
%	C: representation of set of instances of H

headtype(H, B1, (X :- Sols),BMap,K) :- 
	functor(H,F,N),
	functor(X,F,N),
	solveBody(H,B1,Sols,BMap,K),
	canonical(X),
	canonical_each(Sols).

solveBody(H,Bs,Sols,BMap,K) :-
	eliminateLocals(BMap,Bs,B,K),
	selectGoals(B,_,B1,[]), 	% all local vars eliminated from B1
	findall(H,B1,Sols).
	
eliminateLocals([],B,B,_) :-
	!.
eliminateLocals(BMap,Bs,B,K) :-
	findLeastProduct(BMap,varmap(_,As,Ws)),
	selectAll(BMap,As,BMap1,Ys),
	selectGoals(Bs,As,ABs,Bs1),
	%write(ABs),nl,
	setdiff(Ws,Ys,Us),
	G =.. [temp|Us],
	findall(G,ABs,Sols1), 	% eliminate locals Ys, return solutions for Us
	K1 is K+1,
	canonical_each(Sols1),
	removeDupls(Sols1,Sols2),
	pruneAtoms(Sols2,Sols3),
	length(Sols3,L),
	adjustMap(BMap1,As,Us,Ys,K1,BMap2),
	eliminateLocals(BMap2,[atom(K1,L,member(G,Sols3))|Bs1],B,K1).
	
selectAll([],_,[],[]).
selectAll([varmap(V,As,_)|BMap],As,BMap1,[V|Vs]) :-
	!,
	selectAll(BMap,As,BMap1,Vs).
selectAll([varmap(V,As1,Ws)|BMap],As,[varmap(V,As1,Ws)|BMap1],Vs) :-
	selectAll(BMap,As,BMap1,Vs).
	
	
selectGoals([],_,true,[]).
selectGoals([atom(N,_,B)|Bs],As,(B,Bs1),Bs2) :-
	member(N,As),
	!,
	selectGoals(Bs,As,Bs1,Bs2).
selectGoals([atom(N,L,B)|Bs],As,Bs1,[atom(N,L,B)|Bs2]) :-
	selectGoals(Bs,As,Bs1,Bs2).
	
findLeastProduct([First|Map],MinV) :-
	findLeast(Map,First,MinV).
	
findLeast([],V,V) :-
	!.
findLeast([varmap(V1,As1,Ws1)|Map],varmap(_,_,Ws),MinV) :-
	shorter(Ws1,Ws), 	% pick the set of atoms with the least no. of variables
	!,
	findLeast(Map,varmap(V1,As1,Ws1),MinV).
findLeast([_|Map],CurrentMin,MinV) :-
	findLeast(Map,CurrentMin,MinV).
	
shorter([],[_|_]) :-
	!.
shorter([_|X],[_|Y]) :-
	shorter(X,Y).
	
adjustMap([varmap(V,As,Ws)|Map],As1,Us,Ys,K1,[varmap(V,[K1|As2],Ws2)|Map1]) :-
	member(X,As),
	member(X,As1), 	% As and As1 intersect 
	!,
	setdiff(As,As1,As2),
	setdiff(Ws,Ys,Ws1),
	setunion(Us,Ws1,Ws2),
	adjustMap(Map,As1,Us,Ys,K1,Map1).
adjustMap([V|Map],As1,Us,Ys,K1,[V|Map1]) :-
	adjustMap(Map,As1,Us,Ys,K1,Map1).
adjustMap([],_,_,_,_,[]).


check_fix([],[],[],[]).
check_fix([_|Ps],[RP|R],[RP1|S],Qs) :-
	fixpoint(RP,RP1),
	!,
	check_fix(Ps,R,S,Qs).
check_fix([P|Ps],[_|R],[_|S],[P|Qs]) :-
	check_fix(Ps,R,S,Qs).

fixpoint([],[]).
fixpoint([(_ :- X)],[(_ :- Y)]) :-
	\+ surplus_element(Y,X).

surplus_element(Y,X) :-
	member(Z,Y), \+ memb1(Z,X).
	
ub_proc([],As,As).
ub_proc([(S :- B)|As],Bs,[(S :- B1)]) :-
	append(Bs,[(S :- B)|As],Cs),
	get_all_sols(Cs,B1).

get_all_sols([],[]).
get_all_sols([(_ :- Sols)|Cls],U) :-
	get_all_sols(Cls,U1),
	append(Sols,U1,U).
	%myunion(Sols,U1,U).
	
myunion([],Ys,Ys) :-
        !.
myunion([X|Xs],Ys,Zs) :-
	insert_ord(X,Ys,Ys1),
	myunion(Xs,Ys1,Zs).

insert_ord(X,L,L) :-
        memb1(X,L),
        !.
insert_ord(X,L,[X|L]).

	

	
headclause(P/N,R,Cl) :-
	search_tree(R,P/N,[Cl]).
	
display_fix(Cls) :-
	%nl(user_output),write('Filename (enter tty. for user_output): '),
	%read(user_input,F),
	display_clauses(tty,Cls).

display_clauses(tty,Cls) :-
	!,
	nl(user_output),write('Model:'),
	nl(user_output),nl(user_output),
	displayProcs(user_output,Cls),
	nl(user_output),nl(user_output).
display_clauses(F,Cls) :-
	open(F, write, S),
	displayProcs(S,Cls),
	close(S).
	
displayClauses(_,[]).
displayClauses(S,[C|Cls]) :-
	writeClause(S,C),
	nl(S),
	displayClauses(S,Cls).
	
writeClause(S,(H :- B)) :-
	%B is non-reduced and non-renamed body
	%RB is reduced and renamed body
	reduceBody(B,RB),	
	write(S,H),
	write(S,' :- '),nl(S),
%	writeBody(S,B),
	writeBody(S,RB),

	write(S,'.').
writeClause(S,[(H :- B)]) :-
	%B is non-reduced and non-renamed body
	%RB is reduced and renamed body
	reduceBody(B,RB),	
	write(S,H),
	write(S,' :- '),nl(S),
	
%	writeBody(S,B),
	writeBody(S,RB),
	write(S,'.'),
	nl(S).
writeClause(_,[]).
	
writeBody(S,(callpattern(P),Bs)) :-
	!,
	write(S,callpattern(P)),
	write(S,','),
	nl(S),
	writeBody(S,Bs).

writeBody(S,(B,Bs)) :-
	!,
	write(S,'     '),
	write(S,B),
	write(S,','),
	writeBody(S,Bs).

writeBody(S,callpattern(P)) :-
	!,
	write(S,callpattern(P)).

writeBody(S,B) :-
	write(S,'     '),
	write(S,B).

/* Begin reduce callpatterns */
	
%reduceBody((callpattern(P),Bs),[callpattern(PN)|RB]) :-
reduceBody((callpattern(P),Bs),(callpattern(PN),RB)) :-
	!,
	reduceArgs(P,[],PR),
	renameBody(PR,PN),
	reduceBody(Bs,RB).	

%reduceBody((B,Bs),[B|RB]) :-
reduceBody((B,Bs),(B,RB)) :-
	!,
	reduceBody(Bs,RB).

%reduceBody(callpattern(P),[callpattern(PN)]) :-
reduceBody(callpattern(P),callpattern(PN)) :-
	!,
	reduceArgs(P,[],PR),
	renameBody(PR,PN).
		
%reduceBody(B,[B]) :-
reduceBody(B,B) :-
	!.

	
%Form sets of unique occurences of arguments 	
reduceArgs([],X,X).	
reduceArgs([A|As],T,R) :-	
	A =.. B,
	reduceArg(B,T,C),
	reduceArgs(As,C,R).

reduceArg([],[],[]).
reduceArg([A|As],[],[[A]|Rs]) :-
	reduceArg(As,[],Rs).

reduceArg([A|As],[T|Ts],[T|Rs]) :-
	member(A,T),
	!,
	reduceArg(As,Ts,Rs).
reduceArg([A|As],[T|Ts],[[A|T]|Rs]) :-
	reduceArg(As,Ts,Rs).

%Strip _query from predicate name and turn list of predicatename+arguments into predicate.
renameBody([[B]|Bs],BN) :-
	name('_query', Q),
	name(B,QPN),
	append(PN,Q,QPN),
	name(P,PN),
	BN =.. [P|Bs].

/* End */
	
tty(F) :-
	name(F,[]).

/* write the program to current output file  */

displayProcs(S,[[]|Ps]) :-
	!,
	displayProcs(S,Ps).
displayProcs(S,[P|Ps]) :-
	displayClauses(S,P),
	displayProcs(S,Ps).
displayProcs(_,[]).

memb1(X,[X|_]) :-
	!.
memb1(X,[_|Xs]) :-
	memb1(X,Xs).

memb3(X,[X1|_]) :-
	X == X1,
	!.
memb3(X,[_|Xs]) :-
	memb3(X,Xs).

vars(T,Vs) :-
        vars3(T,[],Vs).

vars3(X,Vs,Vs1) :-
        var(X),
        !,
        insertvar(X,Vs,Vs1).
vars3(X,Vs,Vs) :-
	atomic(X),
	!.
vars3(X,Vs,Vs1) :-
        nonvar(X),
        X =.. [_|Args],
        argvars(Args,Vs,Vs1).
 
argvars([],Q,Q).
argvars([X|Xs],Vs,Vs2) :-
        vars3(X,Vs,Vs1),
        argvars(Xs,Vs1,Vs2).
 
insertvar(X,[],[X]).
insertvar(X,[Y|Vs],[Y|Vs]) :-
        X == Y,
        !.
insertvar(X,[Y|Vs],[Y|Vs1]) :-
        insertvar(X,Vs,Vs1).


