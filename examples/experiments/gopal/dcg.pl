:- module(dcg,[program/4]).

program(comm_list(CommList),FileName) --> 
        {write(parsing(FileName))},
        comm(CommList),[.],
        {open(FileName,write,Str),write(Str,'comm_list('),write(Str,CommList),
          write(Str,').'),close(Str),
          write(done_parsing(FileName)),nl}.

%if the command has no label, then give none
%deal with situation that there is only one command
%comm([X,exit]) --> comm1(X).
comm([(none,X)]) --> comm1(X).
comm([(X,Y)]) -->id(X),[:],comm1(Y).

comm([CH|CT]) --> comm_unit(CH),[;],comm(CT).

%have label
comm_unit((X,Y)) -->id(X),[:],comm1(Y).
%no label
comm_unit((none,Y)) -->comm1(Y).

comm1(assign(I,E)) --> id(I),[=],expn(E).

%deal with condition command
%the first arg is condition
%second and third arg are used to recond code of true branch and false branch
comm1(ce(X,TrueBranch,FalseBranch)) --> 
        [if],bool(X),[then], comm(TrueBranch),        
        [else], comm(FalseBranch), [endif].

comm1(ce(X,TrueBranch)) --> 
        [if],bool(X),[then], comm(TrueBranch), [endif].

comm1(while(B,LoopBody) ) --> 
        [loop, while], bool(B), 
        comm(LoopBody), [endloop, while].

comm1(abort)-->[abort].

comm1(jmp(X)) -->[jmp],id(X).

expn1(X) -->  id(X).

expn1(num(X)) -->  n(X).

expn1(e(X)) --> ['('],expn(X),[')'].

expn(X) -->  expn1(X).

expn(add(X,Y)) --> expn1(X),[+],expn(Y).

expn(sub(X,Y)) --> expn1(X),[-],expn(Y).

expn(multi(X,Y)) --> expn1(X),[*],expn(Y).

bool(equal(X,Y)) -->  expn(X),[=],expn(Y).

bool(great(X,Y)) --> expn(X),[>],expn(Y).

bool(less(X,Y)) -->  expn(X),[<],expn(Y).

id(id(X)) --> [X], {atomic(X),name(X,[C|_]), alpha(C)}. /* mal: added atomic test */
alpha(X) :- X >= 65, X =< 90.       % Upper case alpha
alpha(X) :- X >= 97, X =< 122.      % Lower case alpha

n(0) --> [0].  
n(1) --> [1].  
n(2) --> [2].  
n(3) --> [3].  
n(4) --> [4].  
n(5) --> [5].  
n(6) --> [6].  
n(7) --> [7].  
n(8) --> [8].  
n(9) --> [9].
