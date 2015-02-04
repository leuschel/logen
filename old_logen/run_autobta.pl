%%% the source for auto_bta
:- include('auto_bta.pl').


%%% Used to reset the file to full unfold
:- use_module(bta, [annotateFile/3]).


%%% System includes
:- use_module(library(system),[file_exists/1]).

runtime_entry(start) :- go.

:- use_module(library(charsio),[read_from_chars/2]).
:- use_module(library(system),[system/1]).

go :-
   prolog_flag(argv,ArgV),
    get_options(ArgV,Opts,AX),
    (AX = [F,Out] 
     -> (
	  reset_file(F,Ann),
	  bta(F,Ann,Out,Opts)),
	show_file(Out)
     ;  fail
    ),!.

go :- print('Usage: autobta  File.pl Output.pl.ann'),nl,
      print('Switches: -list: list norm'),nl,
      print('          -term: term norm'),nl.
      


get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,Opt) -> (Options = [Opt|OT], Args = AT)
                          ; (Options = OT,     Args = [X|AT])
   ),
   get_options(T,OT,AT).



recognised_option('-list',list) :- set_covnex_norm(list).
recognised_option('-term',term) :- set_convex_norm(term).



get_norm(Filename,Norm) :-
	atom_concat(Filename, '.filters', FilterFile),
	(file_exists(FilterFile) ->
	    open(FilterFile, read, Filters),	    
	    get_norm1(FilterFile,Norm),
	    close(Filters)
	;
	    portray_clause(user_error, 'No Filter file found to set norm, using term'),
	    Norm = term	
	).

get_norm1(F,Norm) :-
	read_term(F,Filter,[]),
	(Filter == end_of_file ->
	    %%% No norm found use default term
	    Norm = term
	;
	    %% If its the norm then just set it and stop
	    (Filter = norm(Norm) ->
		true
	    ;
		get_norm1(F,Norm)
	    )	    
	).



%% This will reset the file to full unfold/rescall
%% and load any filters found in Filename.filters
reset_file(Filename, AnnFile) :-
	atom_concat(Filename, '.filters', FilterFile),
	atom_concat(Filename, '.ann', AnnFile),	
	annotateFile(Filename, AnnFile, auto_bta),
	add_filters(FilterFile, AnnFile).

add_filters(FilterFile, AnnFile) :-	
	open(AnnFile, append, Anns),
	(file_exists(FilterFile) ->
	    open(FilterFile, read, Filters),
	    write_filters(Filters,Anns),
	    close(Filters),
	    close(Anns)
	;
	    portray_clause(user_error, 'No Filter file found to reset file')
	).


write_filters(F,A) :-
	read_term(F,Filter,[]),
	(Filter == end_of_file ->
	    true
	;
	    %% If its the norm then just set it and continue
	    (Filter = norm(Norm) ->
		set_convex_norm(Norm)
	    ;
		(
		 %portray_clause(A,Filter)
         write_term(A,Filter,[quoted(true),ignore_ops(true)]),
         write_term(A,'.',[]), nl(A)
        )
	    ),
	    write_filters(F,A)
	).

	
	

	


%recognised_option('-s',silent).
%recognised_option('-u',unfold).



bta(F,Ann,Out,_Opts) :-
	run_bta_loop_int(F,Ann,Out, no).



save :- save_program('run_autobta.sav').





show_file(File) :-
      open(File, read, Stream),
      (show_stream(Stream) -> true ; true),
      close(Stream).
show_stream(Stream) :- get_char(Stream,Char),
    ((Char = end_of_file)
      -> true
      ; (put_char(Char),
         show_stream(Stream)
        )
    ).




