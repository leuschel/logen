/* ------------------------------------------------------------------------- */
/* TCLTK interface */
/* 28.06.2001      */
/* ------------------------------------------------------------------------- */

:- use_module(library(lists)).
:- use_module(cgs).
:- use_module(tix).
:- use_module(mix).
:- use_module(pp).
:- use_module(memo).
:- use_module('cogen-ml').
:- use_module('cgs-tools').
:- use_module('cgs-io').
:- use_module('cgs-bta').

/* --------------------------------------------*/
/* writing file without directory specified    */
/* Atom: (in) atom to be specialised           */
/* Time: (in) time spent for specialisation    */
/* FilteredAtom: (in) atom filtered            */
/* SF2: (out) file result of specialisation    */
/* --------------------------------------------*/
tcltk_write_specialised_program_to_file(Atom,Time,FilteredAtom,SF2) :- 
   tcltk_write_specialised_program_to_file('',Atom,Time,FilteredAtom,SF2).
 
/* -----------------------------------------------------*/
/* same as write_specialised_program_to_file,           */
/* but does not use path relative to examples-directory */
/* DirPath: (in) directory path                         */
/* Atom: (in) atom to be specialised                    */
/* Time: (in) time spent for specialisation             */
/* FilteredAtom: (in) atom filtered                     */
/* SF2: (out) file result of specialisation             */
/* -----------------------------------------------------*/
tcltk_write_specialised_program_to_file(DirPath,Atom,Time,FilteredAtom,SF2) :- 
  /* SF2 is  output */
  print('writing the specialised program to: '),
  program_to_be_specialised(F),
  string_concatenate(F,'.pe.',SF_basic),
  string_concatenate(DirPath,SF_basic,SF),
  Atom =.. [Pred|Args],
  cgs:add_ground_args(SF,[Pred|Args],SF2),
  print(SF2),nl,
  cgs_set_output(SF2),
  treatement_to_write_specialised_program(Atom,Time,FilteredAtom,SF2).

/*--------------------------------------------------*
  | initialise tcltk connection                     |
 *--------------------------------------------------*/
tcltk_initialise :-
  cgs:change_flush_to_screen(no).

/*--------------------------------------------------*
 | set new program to specialise
 *--------------------------------------------------*/
tcltk_set_new_program(F) :- print(new_program(F)),
   /* F should be the root file name, i.e. without .pl,.ann extensions */
   change_program_to_be_specialised(F),nl.

/*--------------------------------------------------*
 | running cogen
 | tcltk_run_cogen/0: running cogen only
 | tcltk_run_cogen/1: running cogen, and get the file generated name
 | tcltk_run_cogen/2: running cogen, get the file generated, situated in given path
 *--------------------------------------------------*/
tcltk_run_cogen :- tcltk_run_cogen(_).
tcltk_run_cogen(GF) :- tcltk_run_cogen('',GF).
tcltk_run_cogen(DirPath,GF) :-
  print(running_cogen(DirPath)),nl,
  reset_cogen,
  cogen,
  print('writing the generating extension to: '),
  program_to_be_specialised(F),
  string_concatenate(DirPath,F,GF_basic),
  string_concatenate(GF_basic,'.gx',GF),
  print(GF),nl,
  cgs_set_output(GF),
  print('/'),print('* file: '),print(GF), print(' *'),print('/'),nl,
  flush_cogen,
  cgs_close_output,
  screen_flush_pp,
  tcltk_load_genex(GF_basic). /* loading generated file */


/*--------------------------------------------------*
 | running generated cogen extension (genex)
 | please run tcltk_run_cogen before running tcltk_run_genex
 | tcltk_run_genex/2: running genex for given atom and get the file generated
 *--------------------------------------------------*/
tcltk_run_genex(Atom, SF2):- tcltk_run_genex('',Atom, SF2).
tcltk_run_genex(DirPath,Atom, SF2):-
  cogen:add_extra_argument("_m",Atom,V,GenexCall),
  reset_pp,
  print('delete table'),nl,
  delete_table,
  print('reset gensym'),nl,
  reset_gensym,
  print('run genex:'), print(GenexCall),nl,
  cgs_run_genex(GenexCall,Time),
  print('run genex ends:'), print(Time),nl,
  tcltk_write_specialised_program_to_file(DirPath,Atom,Time,V,SF2),
  set_last_specialisation_info(SF2,Atom,V).

/*--------------------------------------------------*
 *--------------------------------------------------*/
tcltk_run_tix(Atom):- tcltk_run_tix('',Atom).
tcltk_run_tix(DirPath,Atom):-
     tcltk_run_tix(DirPath,Atom,_).
tcltk_run_tix(DirPath,Atom, SF2):-
  print(run_tix(DirPath,Atom,SF2)),nl,
  reset_pp,delete_table,reset_gensym,
  time(tix:trace_memo_call(Atom,V),Time),
  print(time(Time)),nl,
  tcltk_write_specialised_program_to_file(DirPath,Atom,Time,V,SF2),
  print(wrote(SF2)),nl,
  set_last_specialisation_info(SF2,Atom,V).

/*--------------------------------------------------*
 *--------------------------------------------------*/
tcltk_run_mix(Atom, SF2):- tcltk_run_mix('',Atom, SF2).
tcltk_run_mix(DirPath,Atom, SF2):-
  /*enter_atom_to_specialise(Atom),*/
  reset_pp,delete_table,reset_gensym,
  time(mix:memo_call(Atom,V),Time),
  tcltk_write_specialised_program_to_file(DirPath,Atom,Time,V,SF2),
  set_last_specialisation_info(SF2,Atom,V).
	
	
tcltk_get_default_goal(DefaultGoal) :-
   residual(X),
   X =.. [DefaultGoal|_Args].
   
/*--------------------------------------------------*
 *--------------------------------------------------*/
tcltk_load_genex(GF) :-
  cgs_load_genex(GF).


/*--------------------------------------------------*
 *--------------------------------------------------*/
tcltk_load_annotated_file(F) :-
  load_annotated_ann_file(F).
	
/*--------------------------------------------------*
 *--------------------------------------------------*/

	
/*--------------------------------------------------*
 | get the status (static/dynamic) from clause given
  | CallNr:(in) the argument numbers
  | BodyList:(in) the clause name
  | Result: (out) the status
 *--------------------------------------------------*/
tcltk_get_body_call_status(CallNr,BodyList,Result) :-
     nth(CallNr,BodyList,AnnCall),
     tcltk_get_call_status(AnnCall,Result).

/*--------------------------------------------------*
 *--------------------------------------------------*/
tcltk_create_annotated_program_to_file(PF,AF) :-
  /* lax, 01.09.2000: put tell() instead of see(AF),*/
  cgs_set_input(PF),
  cgs_set_output(AF), 
  cgs:annotate(1,AF),
  cgs_close_output,
  cgs_close_input,
  print('done !'),nl.  
/*--------------------------------------------------*/
tcltk_create_annotated_program_to_file(PF,AF,Norm,Query) :-
  /* lax, 01.09.2000: put tell() instead of see(AF),*/
   print(perform_bta(PF,AF,Norm,Query)),nl,
  cgsbta:perform_bta(PF,Norm,Query),
   print('perform bta: done'),nl,
  cgs_set_input(PF),
  cgs_set_output(AF), 
  cgs:annotate(1,AF),
  cgs_close_output,
  cgs_close_input,
  print('done !'),nl.  

/*--------------------------------------------------*
 *--------------------------------------------------*/
tcltk_write_annotated_program_to_file(PF,AF) :-
 /* lax, 01.09.2000: put tell() instead of see(AF),*/
  print(writing_annotate_file(AF)),nl,
  cgs_set_output(AF), 
  print('/'),print('* file: '),print(AF),print(' *'),print('/'),nl,nl,
  print(':'), print('- multifile residual/1, filter/2, ann_clause/3, table/1.'),nl, 
  print(':'), print('- dynamic residual/1, filter/2, ann_clause/3, table/1.'),nl,
  print(':'), print('- dynamic static_consult/1.'),nl,  
  print(static_consult([])),print('.'),nl,nl,

  print_predicate(ann_clause(_,_,_)),
  print_predicate(residual(_)),
  print_predicate(filter(_,_)), 
  cgs_close_output.

/*--------------------------------------------------*
 *--------------------------------------------------*/
tcltk_benchmark_specialised_program(DirFile) :-
 bench:benchmark_specialised_program(DirFile).


/* auxilarry functions to retrieve residual predicates
 */
lresidual(F,A) :- tools:residual(X), functor(X,F,A).
fresidual(F) :- findall(X, lresidual(X,_Y), F).
aresidual(A) :- findall(Y, lresidual(_X,Y), A).
faresidual(F,A) :- fresidual(F), aresidual(A).

tcltk_filter(X,Y) :- tools:filter(X,Z),make_readable(Z,Y).

allfilter(X,Y) :- findall(B,(tools:filter(A,_),functor(A,B,_)),X),findall(C,(tools:filter(A,_),functor(A,_,C)),Y).


/*---------------------------------------------------------*/
/* change the status of an annotate clause */
/*---------------------------------------------------------*/

tcltk_change_ann_clause(Numbody,Pos,U) :-
  ann_clause(Numbody,H,Bdy),
  modify_clause(Bdy,Pos,U,Res,_),
  retract(ann_clause(Numbody,_,_)),
  assert(ann_clause(Numbody,H,Res)),
  print(Res),nl.

modify_clause((Clause1,Clause2),0,NewStatus,((Result,Clause2)),0) :-
  !,modify_clause(Clause1,0,NewStatus,Result,0).

modify_clause(Clause,0,NewStatus,NewClause,0) :-
  Clause=..[Head|ArgList],
  embedded_status(Head,_),!,
  NewClause=..[NewStatus|ArgList].

modify_clause(Clause,0,NewStatus,NewClause,0) :-
  !,
  Clause=..[_|ArgList],
  NewClause=..[NewStatus|ArgList].

modify_clause((Clause1,Clause2),Position,NewStatus,Result,LastPos) :-
  Position>0,!,
  (Clause1=(_,_)->
   (modify_clause(Clause1,Position,NewStatus,NewClause,Last1),
    ((Last1==0)->
     LastPos is 0,
     Result=((NewClause,Clause2));
     NewPosition is Last1-1,
     modify_clause(Clause2,NewPosition,NewStatus,NewClause,LastPos),
     ((LastPos==0)->
	Result=((Clause1,NewClause));
	true
     )
    )
   );
   (NewPosition is Position-1,
    modify_clause(Clause2,NewPosition,NewStatus,NewClause,LastPos),
    ((LastPos==0)->
     Result=((Clause1,NewClause));
     true
    )
   )).

modify_clause(Clause,Position,NewStatus,Result,Position) :-
  Position>0,
  Clause=..[Head|ArgList],
  (embedded_status(Head,_)->
   ((ArgList=[If,Then]->
     WithElse is 0,
     ClauseToVerify=(If,Then);
     WithElse is 1,
     ArgList=[If,Then,Else],
     ClauseToVerify=(If,Then,Else)
    ),
    NewPosition is Position-1,
    modify_clause(ClauseToVerify,NewPosition,NewStatus,NewClause,_),
    ((WithElse==0)->
     (NewClause=(NewIf,NewThen),
     Result=..[Head,NewIf,NewThen]);
     (NewClause=(NewIf,NewThen,NewElse),
	Result=..[Head,NewIf,NewThen,NewElse]
     )
    )
   );
   true
  ).

embedded_status(if,if(_)).
embedded_status(resif,resif(_)).

/*---------------------------------------------------------*/
/* to check the number of filter predicate */
/*---------------------------------------------------------*/
check_filter(Predicate,Stat) :-
  print(Stat),
  findall(A,tools:filter(Predicate,A),B),functor(B,_,NbFilter),
  print(NbFilter),nl.
 
/*-------------------------------------------------------------- */
/*modify the argument of a predicate (e.g from static to dynamic)*/
/*-------------------------------------------------------------- */
tcltk_modify_arg_pred(Predicate,NumArg,NewValue) :-
  tools:filter(Predicate,Status),
  print(Status),nl,
  modify_arg_pred(Status,NumArg,NewValue,NewStatus),
  print('nb of '),print(filter(Predicate,_)),
  check_filter(Predicate,' before retractall:'),
  retractall(tools:filter(Predicate,_)),
  check_filter(Predicate,' and after retractall:'),
  assert(tools:filter(Predicate,NewStatus)),
  check_filter(Predicate,' and then after assert:'),
  print(NewStatus),nl.

/* rules to modify from dynamic to static and from type(list(dynamic)) to
  type(list(static)) */

modify_arg_pred([_|Tail],0,NewValue,[NewValue|Tail]) :- !.
modify_arg_pred([Head|Tail],NumArg,NewValue,[Head|NewTail]) :- 
  NumArg>0, NewNumArg is NumArg-1, modify_arg_pred(Tail,NewNumArg,NewValue,NewTail).

/* get the number of arguments of a clause*/
tcltk_get_nb_arg_clause(NumBody,NbArg) :-
     ann_clause(NumBody,Clause,_),
     functor(Clause,_,NbArg).
	
/* ==============================================PRIVATE AREA ===*/  

/*--------------------------------------------------*
 *--------------------------------------------------*/

tcltk_get_group_status([rescall,call,semicall]).
tcltk_get_group_status([unfold,memo]).
tcltk_get_group_status([if,resif,semif]).
tcltk_get_group_status([logif,reslogif]).
tcltk_get_group_status([not,resnot]).
tcltk_get_group_status([';',resdisj]).
tcltk_get_group_status([ucall,mcall]).
tcltk_get_group_status([hide_nf,hide]).

tcltk_get_inverse(A,C) :-
  tcltk_get_group_status(C),
  member(A,C),!.

tcltk_get_all_call_status(X) :-
  findall([C,B],(tcltk_get_call_status(A,B),functor(A,C,_)),X).

tcltk_get_call_status(rescall(_),'dynamic').
tcltk_get_call_status(call(_),'static').
tcltk_get_call_status(semicall(_),'semi').
tcltk_get_call_status(unfold(_),'static').
tcltk_get_call_status(memo(_),'dynamic').
/* to be extended for compound calls: if/disj/not... */
tcltk_get_call_status(not(_),'static').
tcltk_get_call_status(resnot(_),'dynamic').
tcltk_get_call_status(if(_),'static').
tcltk_get_call_status(resif(_),'dynamic').
tcltk_get_call_status(semif(_),'semi').
tcltk_get_call_status((_G1;_G2),'static').
tcltk_get_call_status(resdisj(_),'dynamic').
/* a few special annotations */
tcltk_get_call_status(ucall(_),'ucall').
tcltk_get_call_status(mcall(_),'mcall').
tcltk_get_call_status(hide_nf(_),'hide_nf').
tcltk_get_call_status(hide(_),'hide').
tcltk_get_call_status(test(_),'test').

/* ------------------------------------
  convert from unreadable form to readable form
   ------------------------------------ */
make_readable(Atom,Atom) :-
  atom(Atom),!.
make_readable([],[]) :- !.
make_readable([Head|Tail],Result) :-
  make_readable(Tail,R2),
  (compound(Head)->
   (format_to_chars('~w',Head,CharCode),
    atom_codes(R1,CharCode),
    append([R1],R2,Result)
   );
   (
    append([Head],R2,Result)
   )
  ).


/*--------------------------------------------------*
 *--------------------------------------------------*/
tcltk_get_ann_clause_status(Nbr,[]) :-
  ann_clause(Nbr,_,true),!.

tcltk_get_ann_clause_status(Nbr,Stat) :-
  ann_clause(Nbr,_,StrucStat),
  get_status(StrucStat,Stat).

get_status((Arg1,Arg2),Stat) :-
  Arg1=..[Stat1|_],!,
  get_status(Arg2,Stat2),
  ((atom(Stat2))->
   (append([Stat1],[Stat2],Stat));
   (append([Stat1],Stat2,Stat))
  ).

get_status(A,ListChild) :-
  A=..[Parent|Children],
  get_embedded_status(Parent,Children,ListChild).

get_embedded_status('if',EmbeddedStatus,['if',List]) :-
  !, get_status_children(EmbeddedStatus,List).

get_embedded_status('resif',EmbeddedStatus,['resif',List]) :-
  !, get_status_children(EmbeddedStatus,List).

get_embedded_status(Parent,_,Parent).
  
get_status_children([Child1|Child2],[ChildName,List2]) :-
  Child2\==[],!,
  get_status_children(Child2,List2),
  get_status(Child1,ChildName).

get_status_children([Child],List) :-
  !,Child=..[Head|Tail],
  get_embedded_status(Head,Tail,List).

/* -------------- */
/* TCL/TK SECTION */
/* -------------- */
/* for direct use from Tcl/Tk: */


print_predicate(P) :-
  clause(P,Body),
  portray_clause(':-'(P,Body)),
  fail.
print_predicate(_) :- nl.

/* ---------------------------------------------------*/
/* list of argument type                              */
/* this list should be synchronized with cogen module */
/* ---------------------------------------------------*/

tcltk_list_arg_types([static,dynamic,free,'nonvar(dynamic)',semi]).
