/* ----------------------- */
/* INTERACTIVE COGEN SHELL */
/* ----------------------- */
:- module(cgs, [
		    add_ground_args/3,
		    ann_clause/2,
		    annotate/2,
		    cgs_load_genex/1,
		    cgs_run_genex/2,
  		    change_flush_to_screen/1,
		    change_program_to_be_specialised/1,
		    disable_write_interface_clause/0,
		    flush_to_screen/1,
		    last_specialisation_info/3,
		    load_annotated_ann_file/1,
		    load_annotated_ex_file/1,
		    multi_file_annotate/2,
		    measure_no/1,
		    measure_no/2,
		    number_of_times/1,
		    program_to_be_specialised/1,
		    please/2,
		    reset_already_encountered/0,
		    screen_flush_pp/0,
		    set_last_specialisation_info/3,
		    treatement_to_write_specialised_program/4
		 	]).
  

/*----- MODULE imported -----*/
:- use_module('cgs-tools').
:- use_module('memo').
:- use_module('tix').
:- use_module('pp').
:- use_module('cgs-bta').
:- use_module(cogen). /* to use generalise,... from within generating extensions */


%Steve
:- use_module('annfile').

/* -------------- */
/*  FILES SECTION */
/* -------------- */

/* --------------------------------------------------- *
   Program to specialise
 * --------------------------------------------------- */ 

:- dynamic program_to_be_specialised/1.
program_to_be_specialised('parser').

change_program_to_be_specialised(NewFile) :-
	set_new_program_to_be_specialised(NewFile).
	
set_new_program_to_be_specialised(_New) :-
	retract(program_to_be_specialised(_Old)),fail.
set_new_program_to_be_specialised(New) :-
	(atomic(New) ->
	   assert(program_to_be_specialised(New))
	 ; assert(program_to_be_specialised('parser'))
	).

/* --------------------------------------------------- *
	load an annotate file:
	F:(in) full basic filename without extension 
 * --------------------------------------------------- */ 
load_annotated_ex_file(F) :-
  atomic(F),
  string_concatenate(F,'.ann',AF),
  tools:logen_examples_directory(Dir),
  string_concatenate(Dir,AF,CF),!,
  load_annotated_ann_file(CF).

load_annotated_ex_file(F) :- print('Unable to load annotated file:'),print(F),nl.

load_annotated_ann_file(AF) :-
  print(AF),nl,
  clear_ann_clauses,
  on_exception(existence_error(_Goal,_ArgNo,_ObjectType,_Culprit,_Reserved),
	reconsult_without_redefine_warning(AF),
	(print('*'),nl,fail)).

load_annotated_ann_file(F) :- print('Unable to load annotated file:'),print(F),nl.

/* ----------------------- */
/* Generate Annotated File */
/* ----------------------- */

ann_clause(Head,Body) :- tools:ann_clause(_ClauseNr,Head,Body).
  
/* -------------------------------- */
/* add_ground_args(FileName,Arglist,NewFileName) */
/* -------------------------------- */
add_ground_args(F,[],F).
add_ground_args(F,[H|T],NF) :-
	(atomic(H)
	-> (string_concatenate(F,H,FI))
	;  (string_concatenate(F,'_',FI))
	), add_ground_args(FI,T,NF).

/* --------------------------------------------------- *
 multi_file_annotate: annotate multiple files
 Nr: (in) clause number,
 AF:(in) full filename with extension 
 * --------------------------------------------------- */ 
multi_file_annotate(Nr,AF) :-  /* use for files w/o static_consult */
  print('/'),print('* file: '),print(AF),print(' *'),print('/'),nl,nl,
  print(':'), print('- multifile residual/1, clp_residual/1, filter/2, ann_clause/3, table/1.'),nl, 
  print(':'), print('- dynamic residual/1, clp_residual/1, filter/2, ann_clause/3, table/1.'),nl,
  nl,
  annotate(Nr).
  
/* --------------------------------------------------- *
 annotate/2: annotate into a file
 Nr: (in) clause number,
 AF:(in) full filename with extension 
 * --------------------------------------------------- */ 
annotate(Nr,AF) :- /* use for files with static_consult */
  print('/'),print('* file: '),print(AF),print(' *'),print('/'),nl,nl,
  print(':'), print('- multifile residual/1, clp_residual/1, filter/2, ann_clause/3, table/1.'),nl, 
  print(':'), print('- dynamic residual/1, clp_residual/1, filter/2, ann_clause/3, table/1.'),nl,
  print(':'), print('- dynamic static_consult/1.'),nl,  
  print(static_consult([])),print('.'),nl,nl,
  annotate(Nr).

/* --------------------------------------------------- *
 annotate/1: annotate a clause
 Nr: (in) clause number,
 * --------------------------------------------------- */ 
annotate(ClauseNr) :-
	((read(Term),not(Term = end_of_file))
	-> (Term =.. [Functor|Args],
	    ((Functor=(':-'))
	    -> (/* we have a clause */
		((Args = [Head,BodyCommaList])
		 -> (annotate_clause(ClauseNr,Head,BodyCommaList))
		 ;  (annotate_query(ClauseNr,Args))
		)
	       )
	    ;  (/* we have a fact */
		annotate_clause(ClauseNr,Term,true)
	       )
	    ),
	    CN1 is ClauseNr + 1,
	    annotate(CN1)
	   )
	;  (true)
	).

	
%% --- to annotate a query 
annotate_query(_ClauseNr,[op(Priority,Infix,Op)]) :-
	!,op(Priority,Infix,Op).
annotate_query(ClauseNr,Args) :-
	/* print('/'),print('*'),nl,
        print('### encountered query'),nl,
	print('### '), print(Args), print('*'),
        print('/'),nl, */
	annotate_clause(ClauseNr,q__u__e__r__y,Args).

%% --- to annotate a query (query body or a body of a clause or even a fact)
annotate_clause(ClauseNr,Head,Body) :- 
	add_head_predicate(Head),
%	get_status_clause(ClauseNr,StatusClause),
	annotate_literal(Body,ABody),
	numbervars(g(Head,ABody),0,_),
	pp_term(ann_clause(ClauseNr,Head,ABody)),print('.'),nl.

%% --- annotate every literal
annotate_literal(true,true).
annotate_literal((A,B),(AA,AB)) :-
	annotate_literal(A,AA),
	annotate_literal(B,AB).
annotate_literal((If -> Then ; Else),
		 resif(HAIf,HAThen,HAElse)) :-
	!,annotate_literal(If,AIf),
	hide_nf_literal(AIf,HAIf),
	annotate_literal(Then,AThen),
	hide_nf_literal(AThen,HAThen),
	annotate_literal(Else,AElse),
	hide_nf_literal(AElse,HAElse).
annotate_literal((A;B),(HAA;HAB)) :-
	annotate_literal(A,AA),
	hide_nf_literal(AA,HAA),
	annotate_literal(B,AB),
	hide_nf_literal(AB,HAB).
annotate_literal(findall(A,B,C),resfindall(A,HAB,C)) :-
	annotate_literal(B,AB),
	hide_nf_literal(AB,HAB).

annotate_literal('='(X,Y),call('='(X,Y))) :- !.

annotate_literal(not(X),resnot(X)) :- !.
annotate_literal(\+(X),resnot(X)) :- !.

annotate_literal(Call,rescall(Call)) :- 
	is_built_in_literal(Call),!.

annotate_literal(X,memo(X)).

%% --- database of hide literal
hide_nf_literal(true,rescall(true)) :- !.
hide_nf_literal(memo(X),memo(X)) :- !.
hide_nf_literal(rescall(X),rescall(X)) :- !.
hide_nf_literal(hide_nf(X),hide_nf(X)) :- !.
hide_nf_literal(X,hide_nf(X)).

%% --- database of builtin literals
is_built_in_literal('='(_X,_Y)).
is_built_in_literal('C'(_X,_Y,_Z)).
is_built_in_literal(true).
is_built_in_literal(fail).
is_built_in_literal('=..'(_X,_Y)).
is_built_in_literal(functor(_X,_Y,_Z)).
is_built_in_literal(arg(_X,_Y,_Z)).
is_built_in_literal('is'(_X,_Y)).
is_built_in_literal('<'(_X,_Y)).
is_built_in_literal('=<'(_X,_Y)).
is_built_in_literal('>'(_X,_Y)).
is_built_in_literal('>='(_X,_Y)).
is_built_in_literal(call(X)) :- var(X).
is_built_in_literal(call(X)) :- nonvar(X), is_built_in_literal(X).
is_built_in_literal(nonvar(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(ground(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(number(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(real(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(integer(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(atomic(_X)).   /* declarative if delays until nonvar */
is_built_in_literal(atom(_X)).   /* declarative if delays until nonvar */
is_built_in_literal('\\=='(_X,_Y)). /* declarative if delays until 
sufficiently ground */
is_built_in_literal('=='(_X,_Y)). /* declarative if delays until sufficiently 
ground */
is_built_in_literal('\\='(_X,_Y)). 
is_built_in_literal(clause(_X,_Y)). 
is_built_in_literal(print(_X)).   	/* not declarative */
is_built_in_literal(nl). 		/* not declarative */
is_built_in_literal(assert(_X)). 	/* not declarative */
is_built_in_literal(asserta(_X)). 	/* not declarative */
is_built_in_literal(assertz(_X)). 	/* not declarative */
is_built_in_literal(retract(_X)). 	/* not declarative */
is_built_in_literal(var(_X)). 		/* not declarative */
is_built_in_literal(copy(_X,_Y)). 	/* not declarative */
/*
is_built_in_literal(X) :-
	nonvar(X),
	is_open_literal(X).
*/

%% --- an important clause: starting to annotate by given the head
%% --- of the clause
add_head_predicate(Head) :- 
	Head =.. [Pred|Args],
	gen_new_args(Args,NewArgs),
	NewHead =.. [Pred|NewArgs],
	(already_encountered(NewHead)
	 -> (true)
	 ;  (assert(already_encountered(NewHead)),nl,
	     numbervars(NewHead,0,_),
	     print(residual(NewHead)),print('.'),nl,
	     gen_dynamic_args(NewArgs,Status),
	   /*  get_status_constructor(NewHead,Status), for bta use */
	     print(filter(NewHead,Status)),print('.'),nl
	    )
	).

%% --- generate a new fresh argument
gen_new_args([],[]).
gen_new_args([_H|T],[_NA|NT]) :-
	gen_new_args(T,NT).
	
%% --- generate dynamic arguments (all are dynamic)
gen_dynamic_args([],[]).
gen_dynamic_args([_H|T],[dynamic|NT]) :-
	gen_dynamic_args(T,NT).

%% ----
:- dynamic already_encountered/1.

reset_already_encountered :-
	retract(already_encountered(_X)),fail.
reset_already_encountered.

		
/*------------------------------------------------------*/
/*LA 28.06.2001: to avoid redunduncy with tcltk version,*/ 
/*                    extrait the same common treatment */
/* Atom: (in) atom to be specialised                    */
/* Time: (in) time spent for specialisation             */
/* FilteredAtom: (in) atom filtered                     */
/* SF2: (out) file result of specialisation             */
/*------------------------------------------------------*/
treatement_to_write_specialised_program(Atom,Time,FilteredAtom,SF2) :- 
  print('/'),print('* file: '),print(SF2), print(' *'),print('/'),nl,
  print('/'),print('* benchmark info: '),
  write(Time), write(' ms'), print(' *'),print('/'),nl,nl,
  write_interface_clause(Atom,FilteredAtom),nl,
  flush_pp,
  told,nl,
  screen_flush_pp.
  
:- dynamic write_interface_clause/0.
write_interface_clause.

disable_write_interface_clause :-
   retractall(write_interface_clause).
enable_write_interface_clause :-
   (write_interface_clause -> true ; assert(write_interface_clause)).

write_interface_clause(OAtom,OFilteredAtom) :-
    (write_interface_clause
    -> (copy_term(cl(OAtom,OFilteredAtom),CL),
       numbervars(CL,0,_),
       CL=cl(Atom,FilteredAtom),
       print('/'),print('* atom specialised: '),
       print(Atom), print(' *'),print('/'),nl,nl,
       print(Atom), print(' :- '), print(FilteredAtom),
       print('.'),nl);
    true).
    
/* -------------------------------- */

:- dynamic flush_to_screen/1.

screen_flush_pp :-
  flush_to_screen(yes), !,
  flush_pp.

screen_flush_pp.

flush_to_screen(yes).


/* -------------------------------- */

:- dynamic last_specialisation_info/3.
nothing.

last_specialisation_info('nothing_really',nothing,nothing).

set_last_specialisation_info(_File,_Call,_FilteredCall) :- 
	retract(last_specialisation_info(_,_,_)),fail.
set_last_specialisation_info(File,Call,FilteredCall) :- 
	assert(last_specialisation_info(File,Call,FilteredCall)).
	
     
/* ------- */
/* measure */
/* ------- */

:- dynamic number_of_times/1.

number_of_times(0).

measure_no(G) :-
	measure_no(G,_).

measure_no(G,Time) :- 
        number_of_times(No),
	((No > 0)
	-> (please(tw,on),
	    time(cgs:measure(G,No),Time1),
            Time is Time1 / No, 
            print(' '),write(Time), write(' ms'), nl,
            reset_pp, please(tw,off)
	   )
	; (Time = 0) ),
	reset_pp,delete_table,reset_gensym,
        tools:tools_call(G),print(G),nl,
        nl .

measure(_G,0).
measure(G,N) :- 
  copy(G,CG),
  reset_pp,delete_table,reset_gensym,
  N1 is N - 1,
  call(CG),!,
  measure(G,N1).

/* ------------------------------------------- *
	SCREEN
 * ------------------------------------------- */
change_flush_to_screen(_NewVal) :- retract(flush_to_screen(_X)),fail.
change_flush_to_screen(NewVal) :- assert(flush_to_screen(NewVal)).


/* ------------------------------------------- *
	GENEX
 * ------------------------------------------- */
cgs_load_genex(F) :-
  string_concatenate(F,'.gx',GF),
  print(GF),nl,
  on_exception(existence_error(_Goal,_ArgNo,_ObjectType,_Culprit,_Reserved),
	reconsult_without_redefine_warning(GF),
	(print('* unable to load generating extension: '),print(GF),print(' *'),nl)).

cgs_run_genex(GenexCall,Time) :-
  print(GenexCall),nl,
  time(GenexCall,Time),
  print('end genex call'),nl.


/* -------------------------------------- */
/* remove from cgs-ml-sics.pl		  */
/* -------------------------------------- */

please(_X,_Y). /* to be improved tw,off tw,on */

