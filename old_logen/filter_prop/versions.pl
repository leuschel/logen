:- module(versions, [versions/3,nversions/3]).

:- use_module(readprog).
:- use_module(domainProg).
:- use_module(tp).
:- use_module(myterms).
:- use_module(balanced_tree).
:- use_module(library(lists)).
/*%%%CIAO
:- use_module(timer_ciao).
*/%%%CIAO
%%%SICS/*
:- use_module(timer_sics).
%%%SICS*/
:- use_module(canonical).
:- use_module(tbta).

nversions(Prog,Int,Q) :-
	write(user_output,Prog),nl(user_output),
	start_time,
	readprog(Prog,Cls),
	readprog(Int,ICls),
	domainProg(Cls,MCls),
	tpr(ICls,[],M1),
	tpr1(MCls,M1,M2),
	write('Model computed'),
	nl(user_output),
	sortClauses(MCls,_,Procs),
	insert_tree(root,Q,true,V0),
	genVersions([Q],M2,Procs,V0,Vs),
	traversekey_tree(Vs,Versions),
	end_time('Time to computer versions: ', user_output),
	write(user_output,Versions),
	nl(user_output).
	
versions(Prog,Int,Q) :-
	write(user_output,Prog),nl(user_output),
	makeDFTA(Int,Prog,ICls),
	start_time,
	readprog(Prog,Cls),
	domainProg(Cls,MCls),
	tpr(ICls,[],M1),
	tpr1(MCls,M1,M2),
	sortClauses(MCls,_,Procs),
	insert_tree(root,Q,true,V0),
	genVersions([Q],M2,Procs,V0,Vs),
	traversekey_tree(Vs,Versions),
	end_time('Time to computer versions: ',user_output),
	write(user_output,Versions),
	nl(user_output).
	
genVersions([],_,_,V,V).
genVersions([Q|Qs],M,Prog,V0,V2) :-
	versionPredicate(Q,P/N),
	user_clauses(Prog,P/N,Cls),
	specialise(Cls,Q,M,SCls),
	insertVersions(SCls,V0,V1,VNew,[]),
	append(VNew,Qs,Qs1),
	genVersions(Qs1,M,Prog,V1,V2).
	
makeDFTA(Int,Prog,ICls) :-
	readtype_sdv(Prog,Int,tempdtype,d),
	readprog(tempdtype,ICls).
	
versionPredicate([A|_],P/N) :-
	functor(A,P,N).
	
insertVersions([],V,V,New,New).
insertVersions([[]|Ws],V0,V1,New0,New1) :-
	!,
	insertVersions(Ws,V0,V1,New0,New1).
insertVersions([W|Ws],V0,V2,New0,New2) :-
	insertVersions1(W,V0,V1,New0,New1),
	insertVersions(Ws,V1,V2,New1,New2). 
	
insertVersions1([],V,V,New,New).
insertVersions1([W|Ws],V0,V1,New0,New1) :-
	search_tree(V0,W,true),
	!,
	insertVersions1(Ws,V0,V1,New0,New1).
insertVersions1([W|Ws],V0,V2,New0,New1) :-
	insert_tree(V0,W,true,V1),
	!,
	insertVersions1(Ws,V1,V2,New0,[W|New1]).

specialise([],_,_,[]).
specialise([Cl|Cls],Q,M,[BVs|SCls]) :-
	melt(Cl,(H:-B)),
	separateDenotes(B,BList),
	tsolve(B,M,T),
	findall(BList,(member(H,Q),call(T)),Sols),
	transpose(Sols,B1),
	collectVersions(B1,BVs),
	!,
	specialise(Cls,Q,M,SCls).
	
separateDenotes(true,[]) :-
	!.
separateDenotes((B,Bs),Bs1) :-
	denotesPred(B),
	!,
	separateDenotes(Bs,Bs1).
separateDenotes((B,Bs),[B|Bs1]) :-
	!,
	separateDenotes(Bs,Bs1).
separateDenotes(B,[]) :-
	denotesPred(B),
	!.
separateDenotes(B,[B]).

transpose(Xs,[]) :-
	nullrows(Xs).
transpose(Xs,[Y|Ys]) :-
	makerow(Xs,Y,Zs),
	transpose(Zs,Ys).

makerow([],[],[]).
makerow([[X|Xs]|Ys],[X|Xs1],[Xs|Zs]):-
	makerow(Ys,Xs1,Zs).


nullrows([]).
nullrows([[]|Ns]) :-
	nullrows(Ns).
	
collectVersions([],[]).
collectVersions([B|Bs],[B2|Bs1]) :-
	pruneSolns(B,B1),
	canonical_each(B1),
	sort(B1,B2),
	collectVersions(Bs,Bs1).
	
	
denotesPred(D) :-
	D=..[Den|_],
	name(Den,Dn),
	name('denotes_',Prefix),
	append(Prefix,_,Dn).
	
	
	
