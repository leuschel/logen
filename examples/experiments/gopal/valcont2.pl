%This program is used to demo how to deal with continuation semantics in prolog
%previous version: 04/23/2003                last modification date: 02/02/2004
%author: Qian Wang
% adapted by Gopal Gupta to get rid of ignore Flag
% adatped by Gopal Gupta & Michael Leuschel for LOGEN


:- use_module(memory).
:- use_module(dcg).
:- use_module(lex,[lex/2]).


/* --------------------------------------------------------------- */

example1(ValX, ValY, ValZ) :- 
    program(comm_list(CommList), 
     'example1.tree',[z,=,1,;, w,=,x,;,loop, while,w,>,0,z,=,z,*,y,;, w,=,w,-,1,endloop, while,'.'], []),
    write(CommList),nl,
    prog_eval(CommList,ValX,ValY,ValZ).

example2(ValX, ValY, ValZ) :- 
    program(comm_list(CommList),
     'example2.tree', [z,=,1,;, w,=,x,;,if,w,>,0,then,z,=,z, *, y, ;, w,=,w,-,1, endif,'.'], []),
    write(CommList),nl,
    prog_eval(CommList,ValX,ValY,ValZ).
    
example3(ValX, ValY, ValZ) :- 
    program(comm_list(CommList),
     'example3.tree', [z,=,1,;, w,=,x,;,jmp,label,;,loop,while,w,>,0,z,=,z, *, y, ;,label,:,w,=,w,-,1,endloop,while,;,z,=,7,'.'], []),
    write(CommList),nl,
    prog_eval(CommList,ValX,ValY,ValZ).
    
 example4(ValX, ValY, ValZ) :- 
    program(comm_list(CommList),
    'example4.tree',[z,=,1,;, w,=,x,;,loop,while,w,>,0,z,=,z, *, y, ;, w,=,w,-,1,;,jmp,label, endloop,while,;,label,:,z,=,8,;,z,=,7,'.'], []),
    write(CommList),nl,
    prog_eval(CommList,ValX,ValY,ValZ).
    
example5(ValX, ValY, ValZ) :- 
    program(comm_list(CommList),
     'example5.tree',[z,=,1,;, w,=,y,;,loop, while,x,>,1,y,=,w,;,loop, while,y,>,1,z,=,z,*,w,;, y,=,y,-,1, endloop, while,;,x,=,x,-,1,endloop, while,;,z,=,z,+,1,'.'], []),
    write(CommList),nl,
    prog_eval(CommList,ValX,ValY,ValZ).
    

% to generate a compiler:
%  alias compile 'logen valcont2.pl "run_file(argv(1),X,Y,Z)"'
% in my case: alias cpascal '$logen_bin_directory/logen $logen_examples_directory/gopal/demo/valcont2.pl "run_file(argv(1),X,Y,Z)"'

run_file(Filename,ValX,ValY,ValZ) :-
 (lex(Filename,Tokens)
  -> ( atom_concat(Filename,'.tree',TF),
      ( program(comm_list(CommList),
               TF,Tokens, [])
        -> (%write(CommList),nl,
            prog_eval(CommList,ValX,ValY,ValZ)
           )
        ; (print(parse_failed(Filename)),nl)
      )
     )
  ; (print(lex_failed(Filename)),nl)
  ).
    
ex1(X,Y,Z) :-
   run_file('pascal/example1.p',X,Y,Z).
   
/* --------------------------------------------------------------- */

prog_eval([], _, _, 0).
prog_eval(CommList, Val_x, Val_y, Output) :- 
        initialize_store(Store),
        update(x, Val_x, Store, Mst), 
        update(y, Val_y, Mst, Nst), 
        comm_list_eval(CommList,CommList,Nst, Pst),
        access(z, Pst, Output).

comm_list_eval([],_Program,Store,NS) :- NS=Store.
comm_list_eval(CurrList,Program,Store,NS):-
    CurrList = [_|_],
	comm_eval(CurrList,Rest,Program,Store,NS1),
	comm_list_eval(Rest,Program,NS1,NS).
	
%deal with abort command
%each command has two parts. The first part is label and the second part is command parse tree

%comm_eval(E,_,_,_,_) :- print(comm_eval(E)),nl,fail.

comm_eval([(_,abort)|_],[],_,Store, NS) :- NS=Store. /* rescall this to ensure that Store
 is not instantiated; see bool_eval comment */

%handle while command
comm_eval([(_,while(B,LoopBody))|T],[],Program,Store,NS) :- 
        append(LoopBody,[(www,while(B,LoopBody)) | T],Cont),
	bool_while_eval(B, Cont, T,  Program, Store, NS).

%handle with if else command         
comm_eval([(_,ce(B,C1,C2))|T],[],Program,Store,NS) :-
	append(C1,T,C1n), append(C2,T,C2n),
        bool_eval(B, C1n,C2n,Program,Store,NS).

%deal with jmp
%set the rest of comm to empty, stop the normal execute 
%and put the Jmp command to Jmp_list
comm_eval([(_,jmp(ID))|_],JumpList,Program,Store,NS):-
        find_label(ID,Program,JumpList),!,
        NS=Store.

%deal with assign
comm_eval([(_,assign(id(I), E))|T],T,_, Store, Outstore) :- 
        expr(E, Store, Val), 
        update(I, Val, Store, Outstore). 

expr(add(E1, E2), Store, Result) :- expr(E1, Store, Val_E1), 
        expr(E2, Store, Val_E2), Result is Val_E1+Val_E2.

expr(sub(E1, E2), Store, Result) :- expr(E1, Store, Val_E1), 
        expr(E2, Store, Val_E2), Result is Val_E1-Val_E2.

expr(multi(E1, E2), Store, Result) :- expr(E1, Store, Val_E1), 
        expr(E2, Store, Val_E2), Result is Val_E1*Val_E2.

expr(id(X), Store, Result) :-  access(X, Store, Result).

expr(num(X), _, X).


bool_while_eval(Cond,C1,C2,Program,Store,NS) :- 
	bool_eval(Cond,C1,C2,Program,Store,NS).

bool_eval(great(E1, E2),C1,C2,Program,Store,NS) :- 
	expr(E1, Store, Eval1), 
	expr(E2, Store, Eval2), 
	/* comm_list_eval can be unfoldied if Store & NS are not instantiated ! */
	(Eval1 > Eval2 -> comm_list_eval(C1,Program,Store,NS)
	               ;  comm_list_eval(C2,Program,Store,NS)).

bool_eval(less(E1, E2), C1,C2,Program,Store,NS) :- 
	expr(E1, Store, Eval1), 
	/* comm_list_eval can be unfoldied if Store & NS are not instantiated ! */
	expr(E2, Store, Eval2), 
	(Eval1 < Eval2 -> comm_list_eval(C1,Program,Store,NS)
	               ;  comm_list_eval(C2,Program,Store,NS)).

bool_eval(equal(E1, E2),C1,C2,Program,Store,NS) :- 
	expr(E1, Store, Eval1), 
        expr(E2, Store, Eval2),
	/* comm_list_eval can be unfoldied if Store & NS are not instantiated ! */
        (Eval1 == Eval2 -> comm_list_eval(C1,Program,Store,NS)
	               ;  comm_list_eval(C2,Program,Store,NS)).
        
%a predicate is used to find a command position which it has a label.
find_label(Label,[(Label,Command)|T],[(Label,Command)|T]).
find_label(Label,[(_,ce(_,CBT,CBF))|T],Result):-
	(find_label(Label,CBT,R1);find_label(Label,CBF,R1)),
        append(R1,T,Result).
find_label(Label,[(_,ce(_,CB))|T],Result):-
	find_label(Label,CB,R1), append(R1,T,Result).
find_label(Label,[(WL,while(B,LoopBody))|T],Result):-
	find_label(Label,LoopBody,R1),
        append(R1,[(WL,while(B,LoopBody))|T],Result).
find_label(Label,[_|T],Result):-find_label(Label,T,Result).

append([],X,X).
append([H|T],X,[H|Y]):-append(T,X,Y).


