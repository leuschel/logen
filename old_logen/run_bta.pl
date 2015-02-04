
:- ensure_loaded(sicstus).


:- use_module(bta).
:- use_module(library(lists)).
:- use_module(library(system)).

runtime_entry(start) :- on_exception(E,go, exception_thrown(E)).

%
%exception_thrown(E) :- 
exception_thrown(permission_error(_Call, input, stream, S, _)) :-
		
		stream_property(S,file_name(Info)),
		 portray_clause(stream(Info)),
		 fail.
		
exception_thrown(E) :- portray_clause(E), halt.




:- use_module(library(charsio),[read_from_chars/2]).
%:- use_module(library(system),[system/1]).



:- ensure_loaded('auto_bta.pl').

:- use_module(run_binsolve, [run_binsolve/3]).

:- use_module(convex_analyser,[showfacts/1]).

go :-
   prolog_flag(argv,ArgV),
    get_options(ArgV,Opts,AX),
    (member(show_help,Opts) ->
	fail
    ;
	true
    ),
    
    (AX = [F] ->	
	(bta(F,Opts))
    ;
	fail
    ),!.

go :- print('Usage: bta [-u][-s][-a] File.pl [-i Input.ann] [-o Output.ann]'),nl,
      print('Switches: -u: unfold everything'),nl,
      print('          -s: silent'),nl,
      print('          -a: run the auto bta'),nl,
      print('          -b: Run Program through Binsolve interpreter only'),nl,
      print('          -c: Run Program through Binsolve and Convex Hull (-b is required)'),nl,      
      print('          -i Input.ann: Input Base Ann file'),nl,
      print('          -o Output.ann: Output ann file'),nl.

      

      

   

recognised_option('-filter',[],filter_prop).
recognised_option('--help',[],show_help).
recognised_option('-s',[],silent).
recognised_option('-u',[],unfold).
recognised_option('-a',[],run_auto_bta).

recognised_option('-b',[],run_binsolve).
recognised_option('-c',[],run_convex).

recognised_option('-gui',[],gui_mode).
recognised_option('-norm',[N],norm(N)).
recognised_option('-r',[],reset_auto).

recognised_option('-o', [X], output(X)).
recognised_option('-output', [X], output(X)).
recognised_option('-i', [File], input_ann(File)).
recognised_option('-input', [File], input_ann(File)).








get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,As,Opt) ->
       (
	 append(As,T1,T),
	 %portray_clause(Opt),
	 Options = [Opt|OT],
	 Args = AT
       )
   ;
      (
	T1 = T,
	Options = OT,
	Args = [X|AT]
      )
   ),
   get_options(T1,OT,AT).




bta(F,Opts) :-
	%% if we are either unfolding or running auto bta then set the mode...	
	( (member(unfold,Opts);member(run_auto_bta,Opts)) -> Mode = auto_bta ; Mode = safe),

   
   (member(output(File),Opts) ->
       AnnFile = File          
   ;
       %%% if no output file is specified use default .ann file
       (
	 name(F,AF),
	 append(AF,".ann",AAF),
	 name(AnnFile,AAF)
       )
   ),

   %% if in autobta mode, and not gui we want to ask user for filters
   %% if they dont exist...   
   (((Mode=auto_bta;member(filter_prop,Opts)), \+member(gui_mode,Opts), \+member(input_ann(_),Opts)) ->
       createFiltersFile(F)
   ;
       true),
   
   %%% we should annotate file using simple if we dont provide a base annfile
   (member(input_ann(Input),Opts) ->
       true
   ;
       annotateFile(F,AnnFile,Mode),
       Input = AnnFile
   ),   
   %%% if auto bta flag is set we should run it...

   ((member(filter_prop,Opts),\+member(run_auto_bta,Opts)) ->
       
       run_filter_prop_int(F,Input,AnnFile)
   ;
       true
   ),
   (member(run_binsolve, Opts) ->
       
       (run_binsolve(Input,Memo,Spec),       
       (member(output(File),Opts) ->
	   open(File,write, OutStream),
	   show_file(OutStream,Memo),
	   show_file(OutStream,Spec),
	   close(OutStream)
       ;
	   true
       ),
       (member(silent,Opts) ->	       
	   true
       ;
	   show_file(Memo),
	   show_file(Spec)
       ),
       (member(run_convex, Opts) ->
	   (member(norm(N),Opts) -> set_convex_norm(N) ; true),
	   run_convex(Input,'tmp.out'),
	   
	   %% set norm
	   
	   
	   (member(output(File), Opts) ->	   
	       open(File,write, OutStream),
	       show_file(OutStream,Memo),
	       format(OutStream, "~n~n%% Raw Convex Analysis:~n",[]),	       
	       showfacts(OutStream),
	       format(OutStream, "~n~n%% Pretty Printing Convex Analysis:~n",[]),
	       'bta_driver':pretty_print_convex(OutStream),
	       
	       (bta_driver:unsafe_pp(_) ->
		   findall(PP,unsafe_pp(PP),UnsafeCalls),
		   portray_clause(OutStream, termination_with_unsafe(UnsafeCalls))
	       ;
		   true
	       ),
		   
	       close(OutStream)
	   ;
	       true	   	   
	   ),
	   (member(silent,Opts) ->	       
	       true
	   ;
	       showfacts(user)
	   )	    	   
       ;
	   true
       )	   
       )

   
   ;
       true
   ),

   ((member(run_auto_bta,Opts), \+member(run_binsolve,Opts),\+member(run_convex,Opts)) ->
       (
	 %% set norm
	 (member(norm(N),Opts) -> set_convex_norm(N) ; true),
	 %% run auto bta
        (member(silent, Opts)
          -> run_bta_loop_int(F,Input,AnnFile, yes)
          ;  run_bta_loop_int(F,Input,AnnFile, no))
       )
   ;
       true
   ),                   
   ((member(silent,Opts); member(run_binsolve,Opts))
     -> true
     ; (print('/*    ANNOTATED FILE      */'),nl,
        print('/*  file: '), print(AnnFile), print('  */'),nl,nl,
        show_file(AnnFile))
    ).


	


createFiltersFile(F) :-
   atom_concat(F,'.filters',FilterFile),
   (file_exists(FilterFile)
     -> true
     ;  (print(user_output,'No .filters file.'), nl(user_output),
         print(user_output,'Please provide entry point (e.g., p(static,dynamic)):'),nl(user_output),
         read(FilterQuery),
           /* TO DO: IMPROVE USER GUIDANCE: list of predicates ?? + check properly typed ! */
         open(FilterFile,write,Str),
         write_term(Str,'/* Filters */'),nl(Str),
         %portray_clause(Str,( :- filter(FilterQuery))),
         write_term(Str,( :- filter(FilterQuery)),[quoted(true),ignore_ops(true)]),
         write_term(Str,'.',[]), nl(Str),
         close(Str)
        )
   ).
  
force_rename_file(File1,File2) :-
   (file_exists(File1) -> true ; (print(file_does_not_exists(File1)),fail)),
   (file_exists(File2) -> delete_file(File2) ; true),
   rename_file(File1,File2).

show_file(File) :-
	show_file(user,File).

show_file(OutStream,File) :-
      open(File, read, Stream),
      (show_stream(OutStream,Stream) -> true ; true),
      close(Stream).
show_stream(OutStream,Stream) :- get_char(Stream,Char),
    ((Char = end_of_file)
      -> true
      ; (put_char(OutStream,Char),
         show_stream(OutStream,Stream)
        )
    ).
    
    
save :- save_program('run_bta.sav').

% alias bta 'sicstus -r $logen_source_directory/run_bta.sav --goal "go,halt." -a'
% alias btarb 'cd $logen_source_directory/; sicstus -l run_bta.pl --goal "save,halt."'
