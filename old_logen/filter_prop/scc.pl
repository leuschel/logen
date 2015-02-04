% Strongly connected components based
% on depth-first search of a graph.
% Algorithm by M. Sharir, adapted from
% Baase and Van Gelder, Chapter 7.5
% JPG 20/8/01

:- module(scc, [scc/3]).

:- use_module(readprog, 
	[user_clauses/3]).
:- use_module(balanced_tree).
:- use_module(library(lists)).
:- use_module(builtins).


% scc(Ps,Prog,Cs):   
% scc(+,-):
%	Ps:  a list of predicates.
%	Cs:  a list of components, each labelled as recursive or non-recursive
%

	
% scc(Ps,Cs):  Ps is a list of predicates


scc(Ps,Prog,Cs) :-
	%write('Start make callgraph'),nl,
	make_callgraph(Ps,Prog,G),
	%write('End make callgraph'),nl,
	scc_sharir(G,Cs).


% scc_sharir: the SCC procedure.

scc_sharir(root,[]) :-
	!.
scc_sharir(Graph,SCCs) :-
	phase1(Graph,Stack),
	phase2(Stack,Graph, SCCs),
	!,
	recursive_classify(SCCs,Graph).
	
phase1(Graph,Stack) :-
	traversekey_tree(Graph,Nodes),
	dfsSweep(Nodes,Graph,root,_,[],Stack).

dfsSweep([], _, MarkList, MarkList, Stack, Stack).
dfsSweep([N|Ns], Graph, MarkListIn, MarkListOut, StackIn, StackOut) :-
	search_tree(MarkListIn,N,black),   % N already visited
	!,
	dfsSweep(Ns, Graph, MarkListIn, MarkListOut, StackIn, StackOut).
dfsSweep([N|Ns], Graph, MarkListIn, MarkListOut, StackIn, StackOut) :-
	dfsNode(Graph, N, MarkListIn, MarkListMid, StackIn, StackMid),
	dfsSweep(Ns, Graph, MarkListMid, MarkListOut, StackMid, StackOut).

dfsNode(Graph,N,M0,M2,S0,S1) :-
	insert_tree(M0,N,black,M1),   % mark node as visited
	find_succs(Graph,N,SuccList),
	dfs_each(SuccList,Graph,N,M1,M2,S0,S1).

find_succs(Graph,N,SuccList) :-
	search_tree(Graph,N,links(SuccList,_)),
	!.
find_succs(_,_,[]).

dfs_each([],_,Par,M,M,S,[Par|S]).
dfs_each([N|Ns],G,Par,M0,M1,S0,S1) :-
	search_tree(M0,N,black),
	!,
	dfs_each(Ns,G,Par,M0,M1,S0,S1).
dfs_each([N|Ns],G,Par,M0,M2,S0,S2) :-
	dfsNode(G,N,M0,M1,S0,S1),
	dfs_each(Ns,G,Par,M1,M2,S1,S2).

% phase 2:  use the depth-first ordering from phase 1
% to traverse the transposed graph.

phase2(Nodes,Graph,SCCs) :-
	dfsSweep2(Nodes,Graph,root,_,[],SCCs).

dfsSweep2([], _, MarkList, MarkList, S,S).
dfsSweep2([N|Ns], Graph, MarkListIn, MarkListOut, S0,S1) :-
	search_tree(MarkListIn,N,black),  % N already visited
	!,
	dfsSweep2(Ns, Graph, MarkListIn, MarkListOut, S0,S1).
dfsSweep2([N|Ns], Graph, MarkListIn, MarkListOut, S0,S2) :-
	dfsNode2(Graph, N, N,MarkListIn, MarkListMid,[],S1),
	dfsSweep2(Ns, Graph, MarkListMid, MarkListOut, [(_,S1)|S0],S2).

dfsNode2(Graph,N,L,M0,M2,S0,S1) :-
	insert_tree(M0,N,black,M1),  % mark node as visited
	search_tree(Graph,N,links(_,PrecList)),
	dfs_each2(PrecList,Graph,N,L,M1,M2,[N|S0],S1).

dfs_each2([],_,_,_,M,M,S,S).
dfs_each2([N|Ns],G,L,Par,M0,M1,S0,S1) :-
	search_tree(M0,N,black),
	!,
	dfs_each2(Ns,G,Par,L,M0,M1,S0,S1).
dfs_each2([N|Ns],G,Par,L,M0,M2,S0,S2) :-
	dfsNode2(G,N,L,M0,M1,S0,S1),
	dfs_each2(Ns,G,L,Par,M1,M2,S1,S2).

recursive_classify([],_).
recursive_classify([(recursive,[_,_|_])|Cs],G) :-
	!,
	recursive_classify(Cs,G).
recursive_classify([(recursive,[P])|Cs],G) :-
	direct_recursive(P,G),
	!,
	recursive_classify(Cs,G).
recursive_classify([(non_recursive,_)|Cs],G) :-
	recursive_classify(Cs,G).

direct_recursive(P,G) :-
	search_tree(G,P,links(Ss,_)),
	member(P,Ss).

% starting from a list of predicates, 
% make an adjacency list representation of the call graph 
% and the transposed call graph (reversing links).

make_callgraph([],_,root).
	
make_callgraph([P|Ps],Prog,G) :-
	make_callgraph(Ps,Prog,G1),
	!,
	immed_depends(Prog,P,[],Es),	% could be optimised by using tree instead of list
	%write('Start forward links '), write(P),nl,
	insert_forward_links(G1,P,Es,G2),
	%write('Start backward links'),nl,
	insert_back_links(Es,P,G2,G).

insert_forward_links(G1,P,Es,G2) :-
	search_replace_tree(G1,P,links(_,Ss),G2,links(Es,Ss)),
	!.
insert_forward_links(G1,P,Es,G2) :-
	insert_tree(G1,P,links(Es,[]),G2).

insert_back_links([],_,G,G).
insert_back_links([Q|Qs],P,G0,G2) :-
	search_replace_tree(G0,Q,links(Ps,Ss),G1,links(Ps,Ss1)),
	!,
	setunion([P],Ss,Ss1),
	insert_back_links(Qs,P,G1,G2).
insert_back_links([Q|Qs],P,G0,G2) :-
	insert_tree(G0,Q,links([],[P]),G1),
	insert_back_links(Qs,P,G1,G2).

immed_depends(Prog,P/N,Rs,Rs1) :-
	user_clauses(Prog,P/N,Cls),
	body_preds(Cls,Rs,Rs1).

body_preds([(_ :- B)|Cs],S,S2) :-	
	bodylits(B,S,S1),
	body_preds(Cs,S1,S2).
body_preds([],S,S).


bodylits((B,Bs),S,S2) :-
	\+ sp_builtin(B),
	!,
	functor(B,T,N),
	insertp(T/N,S,S1),
	bodylits(Bs,S1,S2).
bodylits((_,Bs),S,S1) :-
	!,
	bodylits(Bs,S,S1).
bodylits(B,S,S1) :-
	\+ sp_builtin(B),
	!,
	functor(B,T,N),
	insertp(T/N,S,S1).
bodylits(_,S,S).

insertp(X,L,L) :-
	memb1(X,L),
	!.
insertp(X,L,[X|L]).


memb1(X,[X|_]) :-
	!.
memb1(X,[_|Xs]) :-
	memb1(X,Xs).

setunion([],As,As).
setunion([X|Xs],Ys,Zs) :-
	member(X,Ys),
	!,
	setunion(Xs,Ys,Zs).
setunion([X|Xs],Ys,[X|Zs]) :-
	setunion(Xs,Ys,Zs).