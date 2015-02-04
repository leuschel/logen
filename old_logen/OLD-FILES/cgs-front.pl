/* ----------------------- */
/* INTERACTIVE COGEN SHELL */
/* ----------------------- */

:- use_module(library(lists)).
:- use_module('cgs-tools').
:- use_module(cgs).
:- use_module(tix).
:- use_module(mix).
:- use_module(memo).
:- use_module('cgs-bench').
:- use_module('cogen-ml').
:- use_module('pp').
:- use_module('cgs-io').
:- use_module('cgs-bta').

cgs :- please(tw,off),
	cgs_close_input,
	cgs_set_input(user),
	cgs_close_output,
	cgs_set_output(user),
	print('-----------------------'),nl,
	print('INTERACTIVE COGEN SHELL'),nl,
	print('-----------------------'),nl,
	nl,
	print('A handwritten compiler generator'),nl,
	print('by Jesper Jorgensen and Michael Leuschel'),nl,
	nl,
	load_annotated_file,nl,
	print('type h or ? for help'),nl,
	front_end,
	cgs_close_input,
	cgs_close_output.

front_end :-
	prompt(_OldPrompt,''),
	print('=> '),
	get(AsciiChar),
	(action(AsciiChar)
	-> (true)
	;  (print('Unknown command, type h or ? for help'),nl,front_end)
	).

/* --------------------------------------- */
/* keystroke event binding.                */
/* --------------------------------------- */
action(46). /* . for quit */
action(97) :-  /* a for annotate */
	 annotate_program,!,front_end.
action(98) :- /* b: benchmark */
	benchmark_specialised_program,!,front_end.
action(99) :- /* c: change specialised program */
	change_program_to_be_specialised,!,
	load_annotated_file,!,front_end.
action(102):- /* f */
	nl,print('Specialised (Filtered) Predicates:'),nl,
	print_memo_table,!,
	front_end.
action(103):- /* g */
	run_genex,!,
	front_end.
action(108):- /* l */
	load_genex,!,
	front_end.
action(113). /* . for quit */
action(114) :- /* r */
	rcg,!,
	load_genex,!,front_end.
action(116) :- /* t */
	toggle_flush_to_screen,!,front_end.
action(109) :- /* m */
        run_mix,!,front_end.
action(100) :- /* d */
        run_tix,!,front_end.
action(110) :- /* n */
        change_number_of_times,!,front_end.
action(120) :- stop. /* x for exit */

action(63) :- action(104). /* ? for help  */
action(104) :- /* h for help */
	print(' Cogen Interactive Shell'),nl,
	print(' By Jesper Jorgensen and Michael Leuschel'),nl,
	print(' Command Summary:'),nl,
	print('  r: 1.Run cogen and load generating extension'),nl,
	print('  g: 2.run the Generating extension'),nl,
	print('  b: 3.Benchmark specialised program'),nl,
	nl,
	print('  m: run Mix'),nl,
	print('  d: run Debugging/tracing Mix'),nl,
	nl,
	print('  f: print (Filtered) specialised versions'),nl,
	nl,
	print('  c: Change program to be specialised'),nl,
	print('  n: set Number of executions for benchmarking'),nl,
	nl,
	print('  a: Annotate a program'),nl,
	print('  l: re-Load the generating extension'),nl,
	print('  t: Toggle screen flushing on/off'),nl,
	print('  h: Help (also ?)'),nl,
	print('  x: eXit (also  q or . for quit)'),nl,
	nl,
	program_to_be_specialised(File),
	print(' Current program to be specialised: '),print(File),nl,
	front_end.

/* -------------- */
/*  FILES SECTION */
/* -------------- */

change_program_to_be_specialised :-
	print('Old program to be specialised: '),
	program_to_be_specialised(OldFile),
	print(OldFile),nl,
	print('New program (l for last) =>'),
	read(NewFile),
	((NewFile = 'l') -> change_program_to_be_specialised(OldFile)
     ;	change_program_to_be_specialised(NewFile)).

load_annotated_file :-
  print('loading annotated file: '),
  program_to_be_specialised(F),
  load_annotated_ex_file(F).

/* --------------------------------------------------------------*/
/* LA 28.06.2001: extrait common section into seperate predicate */	
/* --------------------------------------------------------------*/
write_specialised_program_to_file(Atom,Time,FilteredAtom,SF2) :- /* SF2 is output */
  print('writing the specialised program to: '),
  program_to_be_specialised(F),
  string_concatenate(F,'.pe.',SF),
  Atom =.. [Pred|Args],
  cgs:add_ground_args(SF,[Pred|Args],SF2),
  print(SF2),nl,
  examples_tell(SF2),
  treatement_to_write_specialised_program(Atom,Time,FilteredAtom,SF2).

/* ------------- */
/* Running cogen */
/* ------------- */
rcg :-
  reset_cogen,
  print('running cogen'),
  print_number_of_times,
  nl,
  measure_no(cogen:cogen,Time),
  print('writing the generating extension to: '),
  program_to_be_specialised(F),
  string_concatenate(F,'.gx',GF),
  print(GF),nl,
  examples_tell(GF),
  print('/'),print('* file: '),print(GF), print(' *'),print('/'),nl,
  flush_cogen,
  told,
  screen_flush_pp,
  print('/'),print('* benchmark info: '),
  write(Time), write(' ms'), print(' *'),print('/'),nl,nl.

/* -------------------------------- */
/* loading the generating extension */
/* -------------------------------- */
load_genex :-
  print('loading generating extension: '),
  program_to_be_specialised(F),
  tools:logen_examples_directory(Dir),
  string_concatenate(Dir,F,CF),!,
  cgs_load_genex(CF).
	
change_number_of_times :-
	print('Old number of additional times: '),
	number_of_times(Old_No),
	print(Old_No),nl,
	print('New number of additional times =>'),
	read(New_No),
	(New_No >= 0 
	->
	    (retract(number_of_times(Old_No)),
		  assert(number_of_times(New_No))) 
	; (print('ERROR: number not >= 0.'),nl)
	).

print_number_of_times :-
  number_of_times(No),
  (No=1 -> true ; (print(' ('),print(No),print(' additional times)'))).


/* -------------------------------- */
/* Running the generating extension */
/* -------------------------------- */
run_genex :-
	enter_atom_to_specialise(Atom),
	add_extra_argument("_m",Atom,V,GenexCall),
	reset_pp,
	print('running the generating extension:'),  print(GenexCall),
	print_number_of_times,
	nl,
	measure_no(GenexCall,Time),
	print('specialisation time: '),print(Time), print(' ms'),nl,
	print('filtered atom: '),print(V),nl,nl,
	write_specialised_program_to_file(Atom,Time,V,SF2),
	set_last_specialisation_info(SF2,Atom,V).

/* -------------------------------- */

run_tix :-
        enter_atom_to_specialise(Atom),
        reset_pp,delete_table,reset_gensym,
        print('running tix'), nl,
	  tix:trace_memo_call(Atom,V),
	  print('filtered atom: '),print(V),nl,nl,
	  write_specialised_program_to_file(Atom,0,V,SF2),
	  set_last_specialisation_info(SF2,Atom,V).

/* -------------------------------- */

run_mix :-
  enter_atom_to_specialise(Atom),
  reset_pp,print('running mix'),  
  print_number_of_times,
  nl,
  measure_no(mix:memo_call(Atom,V),Time),
  print('specialisation time: '),print(Time), print(' ms'),nl,
  print('filtered atom: '),print(V),nl,nl,
  write_specialised_program_to_file(Atom,Time,V,SF2),
  set_last_specialisation_info(SF2,Atom,V).
/* -------------------------------- */

enter_atom_to_specialise(_) :-
  print('List of specialisable predicates:'),nl,
  residual(Call),print('  '),print(Call),nl,fail.

enter_atom_to_specialise(Atom) :-
   print('enter atom to specialise'),
	print('(also enter dynamic data if you want to do benchmarking)'),
	nl,print(' =>'),
	read(Atom),
	(residual(Atom) -> true
	 ; 
	 (print('### Not a predicate to be specialised !'),nl,fail)
	).
  
/* -------------------------------- */

annotate_program :-
  reset_already_encountered,
  print('reading from: '),
  program_to_be_specialised(F),
  string_concatenate(F,'.pl',PF),
  string_concatenate(F,'.ann',AF),
  logen_examples_directory(Dir),
  string_concatenate(Dir,PF,CF),
  print(CF),nl,
  /* 
  format("Please enter the query:",[]),
  read(Query),
  format("Please enter the norm (termsize or listlength):",[]),
  read(Norm),
  perform_bta(CF,Norm,Query),
  */
  examples_see(PF),
  print('======= Begin of Annotation ======='),nl,
  cgs:annotate(1,AF),
  print('======= End of Annotation ======='),nl,
  cgs_close_input.

/* -------------------------------- */

toggle_flush_to_screen :-
	 flush_to_screen(yes),!,
	 print('screen flushing off'),nl,
	cgs:change_flush_to_screen(no).
toggle_flush_to_screen :-
	 print('screen flushing on'),nl,
	 cgs:change_flush_to_screen(yes).

benchmark_specialised_program :-
  tools:logen_examples_directory(Dir),
  bench:benchmark_specialised_program(Dir).	
     
/* ------- */
/* measure */
/* ------- */

change_number_of_times :-
	print('Old number of additional times: '),
	number_of_times(Old_No),
	print(Old_No),nl,
	print('New number of additional times =>'),
	read(New_No),
	(New_No >= 0 
	->
	    (retract(number_of_times(Old_No)),
		  assert(number_of_times(New_No))) 
	; (print('ERROR: number not >= 0.'),nl)
	).

print_number_of_times :-
  number_of_times(No),
  (No=1 -> true ; (print(' ('),print(No),print(' additional times)'))).


/* Main */
:- cgs. 
