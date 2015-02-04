:- module(balanced_tree, [insert_tree/4,
		   search_tree/3,
		   maketree/2,
		   search_replace_tree/5,
		   traverse_tree/2,
		   traverseVal_tree/2,
		   traversekey_tree/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Insertion and search in 234-trees.  %
% 234 trees are B-trees of order 4.   %
% Insertion maintains height-balance. %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% JPG - June 2001 - August 2001

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Structure of 234-trees
%
% Tree234 ::= root |  
%             node(Records, Succs) |
%	      leaf(Records)
%
% NonRootTree234 ::= 
%             node(Records, Succs) |
%	      leaf(Records)
%
% -- max length(Records) = 3  
% -- length(Succs) = 1+length(Records)
%
% Records ::= [Record] | [Record|Records] 
% Succs   ::= [NonRootTree234,NonRootTree234] | 
%             [NonRootTree234|Succs]
%
% -- Key is compared using  @<, =
%
% Record  ::= rec(Key, Vlist)
% Key     ::= GroundTerm 
% Vlist   ::= Any  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% insert_tree(T,K,V,T1)
% Assume K is not in T.
% T is a tree
% K is key to be inserted
% V is the new record for K
% T1 is the new tree


insert_tree(root,K,V,leaf([rec(K,V)])) :-
	!.
insert_tree(T,K,V,T2) :-
	insert_base_tree(T,K,V,T1),
	splitroot(T1,T2).

insert_base_tree(leaf(Recs),K,V,leaf(Recs1)) :-
	insertleaf(K,Recs,V,Recs1).
insert_base_tree(node(Recs,Succs),K,V,NewTree) :-
	findinterval(K,Recs,N,Succs,Succ),
	insert_base_tree(Succ,K,V,Tree1),
	newtree(Tree1,N,Recs,Succs,NewTree).

insertleaf(K,[rec(K1,V1)|Rs],V,[rec(K,V),rec(K1,V1)|Rs]) :-
	K @< K1,
	!.
insertleaf(K,[R|Rs],V,[R|Rs1]) :-
	insertleaf(K,Rs,V,Rs1).
insertleaf(K,[],V,[rec(K,V)]).

findinterval(K,[rec(K1,_)|_],1,[S|_],S) :-
	K @< K1,
	!.
findinterval(K,[_|Rs],N,[_|Ss],S) :-
	findinterval(K,Rs,N1,Ss,S),
	N is N1+1.
findinterval(_,[],1,[S],S).


newtree(leaf([K1,K2,K3,K4]),N,Recs,Succs,NewTree) :-
	!,
	promote(N,leaf([K1]),leaf([K3,K4]),K2,Recs,Succs,NewTree).
newtree(node([K1,K2,K3,K4],[S1,S2|Ss]),N,Recs,Succs,NewTree) :-
	!,
	promote(N,node([K1],[S1,S2]),node([K3,K4],Ss),K2,Recs,Succs,NewTree).
newtree(T,N,Recs,Succs,NewTree) :-
	replace_subtree(N,Succs,T,Recs,NewTree).

replace_subtree(1,[_|Ss],T,Recs,node(Recs,[T|Ss])).
replace_subtree(2,[S1,_|Ss],T,Recs,node(Recs,[S1,T|Ss])).
replace_subtree(3,[S1,S2,_|Ss],T,Recs,node(Recs,[S1,S2,T|Ss])).
replace_subtree(4,[S1,S2,S3,_],T,Recs,node(Recs,[S1,S2,S3,T])).

promote(1,S11,S12,K2,Recs,[_|Succs],node([K2|Recs],[S11,S12|Succs])).
promote(2,S11,S12,K2,[J1|Recs],[R1,_|Ss],node([J1,K2|Recs],[R1,S11,S12|Ss])).
promote(3,S11,S12,K2,[J1,J2|Recs],[R1,R2,_|Ss],
	node([J1,J2,K2|Recs],[R1,R2,S11,S12|Ss])).
promote(4,S11,S12,K2,[J1,J2,J3|Recs],[R1,R2,R3,_],
	node([J1,J2,J3,K2|Recs],[R1,R2,R3,S11,S12])).

splitroot(leaf([K1,K2,K3,K4]),node([K2],[leaf([K1]),leaf([K3,K4])])) :-
	!.
splitroot(node([K1,K2,K3,K4],[S1,S2|Ss]),
	       node([K2],[node([K1],[S1,S2]),node([K3,K4],Ss)])) :-
	!.
splitroot(T,T).


% search for key K in 234 tree and return value.

search_tree(leaf([rec(K1,V1)|Rs]),K,Vs) :-
	returnleaf(K,K1,V1,Rs,Vs).
search_tree(node(Rs,Ss),K,Vs) :-
	searchinterval(K,Rs,Ss,S),
	!,
	searchsubtree(S,K,Vs).

returnleaf(K,K,Vs,_,Vs) :-
	!.
returnleaf(K,_,_,[rec(K2,V1)|Rs],Vs) :-
	returnleaf(K,K2,V1,Rs,Vs).

searchinterval(K,[rec(K1,_)|Rs],[_|Ss],S) :-
	K @> K1,
	!,
	searchinterval(K,Rs,Ss,S).
searchinterval(K,[rec(K1,Vs)|_],[S|_],R) :-
	check_interval_found(K,K1,Vs,S,R).
searchinterval(_,[],[S],S).

check_interval_found(K,K,Vs,_,result(Vs)) :-
	!.
check_interval_found(_,_,_,S,S).



searchsubtree(result(Vs),_,Vs) :-
	!.
searchsubtree(S,K,Vs) :-
	search_tree(S,K,Vs).
	
maketree([],root).
maketree([X|Xs],T) :-
	maketree(Xs,T1),
	insert_tree(T1,X,x,T).

% search for key K in 234 tree and replace value.

search_replace_tree(leaf([rec(K1,V1)|Rs]),K,Vs,leaf(Rs1),Vs1) :-
	replaceleaf(K,K1,V1,Rs,Vs,Rs1,Vs1).
search_replace_tree(node(Rs,Ss),K,Vs,node(Rs1,Ss1),Vs1) :-
	replace_interval(K,Rs,Ss,S,Rs1,Ss1,S1),
	!,
	search_replace_subtree(S,K,Vs,S1,Vs1).

replaceleaf(K,K,Vs,R2,Vs,[rec(K,Vs1)|R2],Vs1) :-
	!.
replaceleaf(K,K1,V1,[rec(K2,V2)|Rs],Vs,[rec(K1,V1)|Rs1],Vs1) :-
	replaceleaf(K,K2,V2,Rs,Vs,Rs1,Vs1).


replace_interval(K,[rec(K1,Vs)|Rs],[S1|Ss],S,[rec(K1,Vs)|Rs1],[S1|Ss1],S2) :-
	K @> K1,
	!,
	replace_interval(K,Rs,Ss,S,Rs1,Ss1,S2).
replace_interval(K,[rec(K1,Vs)|Rs],[S|Ss],V1,[rec(K2,Vs1)|Rs],[S1|Ss],V2) :-
	check_replace_interval(K,K1,K2,Vs,V1,Vs1,V2,S,S1).
replace_interval(_,[],[S],S,[],[S1],S1).

check_replace_interval(K,K,K,Vs,val(Vs),Vs1,val(Vs1),S,S) :-
	!.
check_replace_interval(_,K1,K1,Vs,S,Vs,S1,S,S1).

search_replace_subtree(val(Vs),_,Vs,val(Vs1),Vs1) :-
	!.
search_replace_subtree(S,K,Vs,S1,Vs1) :-
	search_replace_tree(S,K,Vs,S1,Vs1).


% traverse the tree in order, returning keys 

traversekey_tree(T,L) :-
	travkey_tree(T,L,[]).

travkey_tree(root,L,L).
travkey_tree(leaf(Rs),L0,L1) :-
	traverse_leafkeys(Rs,L0,L1).
travkey_tree(node(Rs,Ss),L0,L1) :-
	traversekey_each(Rs,Ss,L0,L1).

traversekey_each([],[S],L0,L1) :-
	travkey_tree(S,L0,L1).
traversekey_each([rec(K,_)|Rs],[S|Ss],L0,L2) :-
	travkey_tree(S,L0,[K|L1]),
	traversekey_each(Rs,Ss,L1,L2).

traverse_leafkeys([],L0,L0).
traverse_leafkeys([rec(K,_)|Rs],[K|L0],L2) :-
	traverse_leafkeys(Rs,L0,L2).


% traverse the tree in order, returning keys and records

traverse_tree(T,L) :-
	trav_tree(T,L,[]).

trav_tree(root,L,L).
trav_tree(leaf(Rs),L0,L1) :-
	traverse_leaf(Rs,L0,L1).
trav_tree(node(Rs,Ss),L0,L1) :-
	traverse_each(Rs,Ss,L0,L1).

traverse_each([],[S],L0,L1) :-
	trav_tree(S,L0,L1).
traverse_each([R|Rs],[S|Ss],L0,L2) :-
	trav_tree(S,L0,[R|L1]),
	traverse_each(Rs,Ss,L1,L2).

traverse_leaf([],L0,L0).
traverse_leaf([R|Rs],[R|L0],L2) :-
	traverse_leaf(Rs,L0,L2).

% traverse the tree in order, returning values

traverseVal_tree(T,L) :-
	travVal_tree(T,L,[]).

travVal_tree(root,L,L).
travVal_tree(leaf(Rs),L0,L1) :-
	traverse_leafVals(Rs,L0,L1).
travVal_tree(node(Rs,Ss),L0,L1) :-
	traverseVal_each(Rs,Ss,L0,L1).

traverseVal_each([],[S],L0,L1) :-
	travVal_tree(S,L0,L1).
traverseVal_each([rec(_,V)|Rs],[S|Ss],L0,L2) :-
	travVal_tree(S,L0,[V|L1]),
	traverseVal_each(Rs,Ss,L1,L2).

traverse_leafVals([],L0,L0).
traverse_leafVals([rec(_,V)|Rs],[V|L0],L2) :-
	traverse_leafVals(Rs,L0,L2).
