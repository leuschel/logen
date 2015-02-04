:- multifile term_expansion/2.
:- use_package(sicsignore).
:- write('Ignoring Annotations'), nl.

:- use_module('ann_db.pl').

%% from ann db
%%logen_annotation_structure(Pattern, InArgs, OutArgs, Result)

term_expansion((:-use_package(_package)) , ( :- true)).
term_expansion((:-set_logen_flag(_flag)) , ( :- true)).


term_expansion((logen(_,Head) :- Body), (Head :- NewBody)) :- 
		      remove_annotation_body(Body,NewBody).
term_expansion(logen(_KeyWord, Head), Head).



%%logen declarations
:- op(1150, fx, residual).
:- op(1150, fx, filter).
:- op(1150, fx, type).
:- op(1150, fx, table).
:- op(1150, fx , residual_pred).
:- op(500,yfx,--->).
term_expansion((:-residual(_pred)),(:- true)).
term_expansion((:-filter(_pred)),(:- true)).
term_expansion((:-table(_pred)),(:- true)).
term_expansion((:-type(_pred)),(:- true)).
term_expansion((:-residual_pred(_pred)),(:- true)).

term_expansion((:-(_:residual(_pred))),(:- true)).
term_expansion((:-(_:filter(_pred))),(:- true)).
term_expansion((:-(_:table(_pred))),(:- true)).
term_expansion((:-(_:type(_pred))),(:- true)).
term_expansion((:-(_:residual_pred(_pred))),(:- true)).





%%% OBSOLETE
%%% remove_annotation((A,B),(NA,NB)) :- remove_annotation(A,NA), remove_annotation(B,NB).
%%% remove_annotation(logen(_Keyword, Body), Body).
%%% remove_annotation(resif(A, B, C), (NA -> NB ; NC) ) :-
%%%     remove_annotation(A,NA), remove_annotation(B,NB), remove_annotation(C,NC).
%%% remove_annotation(if(A, B, C), (NA -> NB ; NC) ) :-
%%%     remove_annotation(A,NA), remove_annotation(B,NB), remove_annotation(C,NC).
%%% remove_annotation(semif(A, B, C), (NA -> NB ; NC) ) :-
%%%     remove_annotation(A,NA), remove_annotation(B,NB), remove_annotation(C,NC).
%%% remove_annotation(resdisj(A, B), (NA ; NB) ) :-
%%%     remove_annotation(A,NA), remove_annotation(B,NB).
%%% remove_annotation(';'(A, B), (NA ; NB) ) :-
%%%     remove_annotation(A,NA), remove_annotation(B,NB).
%%% remove_annotation(hide_nf(A), NA ) :-
%%%     remove_annotation(A,NA).
%%% remove_annotation(hide(A), NA ) :-
%%%     remove_annotation(A,NA).

%%% remove_annotation(resnot(A), \+(NA) ) :-
%%%     remove_annotation(A,NA).
%%% remove_annotation(not(A), \+(NA) ) :-
%%%     remove_annotation(A,NA).
%%% remove_annotation(findall(A, B, C), findall(A,NB,C) ) :-
%%%     remove_annotation(B,NB).
%%% remove_annotation(resfindall(A, B, C), findall(A,NB,C) ) :-
%%%     remove_annotation(B,NB).
%remove_annotation(X,X).





