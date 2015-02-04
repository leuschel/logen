/* ----------------------- */
/* INTERACTIVE COGEN SHELL */
/* ----------------------- */

/* SICSTUS STUFF */

:- use_module(library(lists)).
:- use_module(library(system)).


%% depricated by steve from now on use clear_ann_clauses in annfile module
%clear_ann_clauses :-
%	write('WARNING WARNING WARNING'), nl,
%	write('WARNING WARNING WARNING'), nl,
%	write('WARNING WARNING WARNING'), nl,
%	write('WARNING WARNING WARNING'), nl,
%	write('Depricated clear_ann_clauses'), nl,
%	write('WARNING WARNING WARNING'), nl,
%	write('WARNING WARNING WARNING'), nl,
%	write('WARNING WARNING WARNING'), nl,
%	write('use annfile instead'), nl,
%	retractall(filter(_,_)),
%	retractall(static_consult(_)),
%	retractall(residual(_)),
%	retractall(table(_)),
%XS	retractall(ann_clause(_,_,_)).

/* ----------------------------- */

:- mode string_concatenate(i,i,o).

string_concatenate(X,Y,XY) :-
   name(X,Xs),name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).

consult_without_redefine_warning(File) :-
    prolog_flag(redefine_warnings, Old, off),
    prolog_flag(single_var_warnings, Old2, off),
    (ensure_loaded(File)
      -> OK=true ; OK=false),
    prolog_flag(redefine_warnings, _, Old),
    prolog_flag(single_var_warnings, _, Old2),
    OK=true.
    
reconsult_without_redefine_warning(File) :-
    prolog_flag(redefine_warnings, Old, off),
    prolog_flag(single_var_warnings, Old2, off),
    (load_files(File,[if(true)])
      -> OK=true ; OK=false),
    prolog_flag(redefine_warnings, _, Old),
    prolog_flag(single_var_warnings, _, Old2),
    OK=true.

ensure_consulted(File) :- 
	logen_source_directory(Dir),
	string_concatenate(Dir,File,CF),!,
	ensure_loaded(CF).

logen_reconsult(File) :-
	logen_source_directory(Dir),
	string_concatenate(Dir,File,CF),!,
	ensure_loaded(CF).


not(Goal) :- \+(Goal).

copy(C,CC) :- copy_term(C,CC).


namevars(Term,Z,V,_Name) :- numbervars(Term,Z,V).



time(Goal,Time) :- 
	statistics(runtime,[Global1,_]),
	call(Goal),
	statistics(runtime,[Global2,_TimeSinceLastStat]),
	Time is Global2 - Global1.

time(Goal) :-
	time(Goal,Time),
	print('Time for goal: '),print(Goal),
	print(' is: '),print(Time), print(' ms'),nl.

cputime(T) :- 
	statistics(runtime,[T,_TimeSinceLastStat]).
stack_size(H,L,T) :- 
	statistics(local_stack,[L,_]),
	statistics(trail,[T,_]),
	statistics(global_stack,[H,_]).
clear_stacks.


:- use_module(library(terms)).

is_inf(X) :- cyclic_term(X).
varlist(T,VList) :- term_variables(T,VList).

stop :- halt.

/* END SICSTUS STUFF */

  /* LA 04.06.2001: get cogen directory */

get_env_var(Key,Value) :-
  L=..[Key,Value], predicate_property(L,_),!,call(L).

get_env_var(Key,Value) :-
  environ(Key,Value),!,
  L=..[Key,Value],
  assert(L).

get_env_var(Key,_) :-
  write('Error: Cannot find environment variable'),
  write(Key),nl,
  write('Please set this variable and restart sicstus prolog.'),
  fail.


:-  get_env_var(logen_source_directory,_),
    get_env_var(logen_examples_directory,_).
