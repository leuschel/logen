%%% March 2005
%%% Loads Annotation Files
%%%  Takes Stream and Populates ann_clause and ann_decl
%%%!! THAT IS ALL IS SHOULD DO!
%%% See: annmeta.pl to define annotations and declarations
%%% See: op_decl to define operators

:- module(annloader,[load_annfile/1,
		     annloader_unittest/0,
		     ann_clause/3,
		     ann_decl/2,
		     predicate_defined_by_filter/2]).

:- use_module('tools/error_manager.pl', [add_error/3,add_message/4]).
:- use_module(annmeta, [annmeta/5, declmeta/3]).


%:- use_module(runtime_checks).
%:- include(runtime_checks_perform).


predicate_defined_by_filter(F,N) :-
   ann_decl(filter, Call), functor(Call,F,N).

%% annfile data stores
:- dynamic ann_clause/3.
%% ann_decl should store *everything* else
%% ann_decl(id,value) e.g. ann_decl(filter, match(static, dynamic))
:- dynamic ann_decl/2.


clear_anndata :-
	retractall(ann_clause(_,_,_)),
	retractall(ann_decl(_,_)).

load_annfile(Filename) :-
	open(Filename, read, Stream),
	load_annstream(Stream),
	close(Stream).
	
load_annstream(Stream) :-
	clear_anndata,
	load_clauses(Stream).


%% Put all ops in here...
:- include(op_decl).

load_clauses(Stream) :-
	read_term(Stream, Term,[]),
	(Term == end_of_file ->
	    true
	;
	    load_ann(Term),
	    load_clauses(Stream)
	).

load_ann(logen_module(_M,Decl)) :- !,
     load_ann(Decl).
load_ann((:-Decl)) :-
         !,
	 load_decl(Decl, Assert),
	 (Assert == true ->
	     true
	 ;
	     assert(Assert)
	 ).

load_ann((logen(ID, Head):-Body)) :-
         !,
	 (load_body(Body,AnnBody)
	   -> assert(ann_clause(ID,Head, AnnBody))
	   ;  add_error(load_ann,"Could not parse body of annotated clause: ~w.~n",[Body])
	 ).

load_ann(logen(ID,Head)) :-
	!,
	assert(ann_clause(ID, Head,true)).
load_ann(AnnotatedClause) :-
	add_error(load_ann,"Unrecognised annotated clause ~w", [AnnotatedClause]).


%%% Loads things that are not clauses
load_decl(true,true) :- !.
load_decl(Declaration, ann_decl(ID,NA)) :-
	declmeta(Declaration, ID, NA),
	!,
	add_message(load_decl,3,"Stored Declaration: '~w'.~n",[Declaration]),
	(execute_decl(Declaration) 
	  -> add_message(load_decl,2,"Executed Declaration: '~w'.~n",[Declaration])
	  ; true
	).
load_decl(Decl, true) :-
	add_error(load_decl,"Unknown declaration ~w", [Decl]).

	
execute_decl(op(Prio,Assoc,Op)) :- op(Prio,Assoc,Op),!.
	/* we need to execute the operator declaration as the the rest of
	   the ann file may not be parsable otherwise */

load_body(Body, AnnBody) :-
	annmeta(load, Body,AnnBody,ToDo,ToDoMap),
	load_list(ToDo,ToDoMap).

load_list([],[]).
load_list([A|As],[NA|NAs]) :-
	load_body(A,NA),
	load_list(As,NAs).




%%%
% Unit Testing 
%%%
annloader_unittest :-
	use_module(annloader,[load_annfile/1]),
	annloader:load_annfile('tests/test.pl.ann'),		
	findall(Call,
		(
		  (Call=ann_clause(_,_,_), Call)
		;		
		  (Call=ann_decl(_,_), Call)
		)    ,
		All),
	%portray_clause(All),
	test_answer(All).

test_answer([ann_clause(id(1),my_head(A,B),(logen(call,foo(A)),logen(rescall,bar(B)))),ann_clause(id(2),my_head(C,D),(logen(call,before(C)),resif(logen(rescall,cond(C)),logen(rescall,if(C)),logen(rescall,else(D))),logen(call,next(C)))), ann_decl(module,test),ann_decl(filter, my_head(static, dynamic))]).
