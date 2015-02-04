/* A simple entry point file; just to allow to run .gx files */


:- ensure_loaded(sicstus).
:- use_module(run_gx).
:- use_module(run_cogen).


:- use_module(moduledriver,[view_spec_file/1]).
                         

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module('prob/logen_preferences.pl').
:- use_module(logen_messages).

spec(F,Goal,Opts) :-
   print_message('Running Logen'),
   print_debug_message(options(Opts)),
   (member(debug_mode,Opts) -> set_preference(gx_debug_mode,2) ; true),
   (member(debug_mode1,Opts) -> set_preference(gx_debug_mode,1) ; true),
   (member(debug_mode3,Opts) -> set_preference(gx_debug_mode,3) ; true),
   (member(xsb_mode,Opts) -> set_preference(prolog_mode_flag,xsb) ; true),
   (member(ciao_mode,Opts) -> set_preference(prolog_mode_flag,ciao) ; true),
   (member(modular,Opts) -> set_preference(modular_spec_mode,true) ; true),
   (member(error_log(ErrLog),Opts) ->
       (file_exists(ErrLog) -> delete_file(ErrLog) ; true)
   ; true),
   (member(timeout(TOut), Opts) ->  name(TOut, TOutS), name(Timeout,TOutS); Timeout = -1),
   (member(no_interface_clauses,Opts) -> set_preference(print_interface_clauses,false) ; true),
   (member(rebuild_everything,Opts)
      -> (Rebuild=rebuild_everything,
          run_cogen_on_plfile(F,_,GxFile)
         )
       ; (run_cogen_if_necessary(F,_,GxFile),
          (member(keep_memo,Opts) -> Rebuild=none ; Rebuild=rebuild_memo))
   ),
   (member(cogen_only,Opts)
     -> (member(silent,Opts) -> true ; show_file(GxFile))
      ; (print_debug_message(starting_specialization(Goal)),
         %specialise_plfile(F,Goal,Rebuild,R),
	 specialise_plfile(F,Goal,Rebuild,R,_SpecTime, Timeout, Result),
	 (Result == time_out ->
	     portray_clause(user,time_out_occurred(Timeout)),
	     (ground(ErrLog) ->
		 (
		   open(ErrLog, write, ErrStream),
		   portray_clause(ErrStream,time_out_occurred(Timeout)),
		   close(ErrStream)
		 )
	     ;
		 true
	     )	     	 	     	     	     
	 ;
	     print_debug_message(residual_call(R)),!,
	     (member(silent,Opts) -> true ; view_spec_file(F))
	 )	    	             
         )
   ).

spec(F,Goal,Opts) :-
   print_message(specialization_failed(F,Goal,Opts)).
   
%:- spec('/Users/mal/cvs_root/cogen2/examples/interpreters/lloyd_topor_op.pl',solve1(app([],[],R))).


save :- save_program('run_gx.sav').

% usage: sicstus -r run_gx.sav --goal "spec(File,Query)"
% e.g.: sicstus -r run_gx.sav --goal "spec('/Users/mal/cvs_root/cogen2/examples/interpreters/lloyd_topor_op.pl',solve1(app([],[],R)))."
% or better:
% alias rungxrb 'sicstus -l $logen_source_directory/logen_cli.pl --goal "save,halt."'
% alias rungx 'sicstus -r $logen_source_directory/run_gx.sav --goal "go,halt." -a'
% to make binary:    
% spld --static --output logen --resources=run_gx.sav=/run_gx.sav
% 

runtime_entry(start) :- go.

:- use_module(library(charsio),[read_from_chars/2]).
:- use_module(library(system),[system/1]).

go :-
   init_preferences,
    prolog_flag(argv,ArgV),
    get_options(ArgV,Opts,AX),
    
    (AX = [F,Query|RestArgv] 
     -> (name(Query,AsciiL),add_dot(AsciiL,AL2),read_from_chars(AL2,Q), adapt_query(Q,AQ,RestArgv),
         spec(F,AQ,Opts))
     ;  (AX=[F]
          -> (member(cogen_only,Opts)
               -> spec(F,none,Opts)
               ;  (print('Query to specialise =>'), read(Query),spec(F,Query,Opts))
              )
          ;  fail
        )
    ).
go :- print('Usage: logen [-s -k -r ...] File.pl ["Atom."]'),nl,
      print('Switches: -s: silent (do not print specialized program)'),nl,
      print('          -k: keep memo table (do not respecialize if version already exists)'),nl,
      print('          -r: rebuild everything (rebuild .gx, .memo, .spec files)'),nl,
      print('          -d: debug mode (-d1 -d2 -d3 also possible; d=d2)'),nl,
      print('          -m: modular specialization'),nl,
      print('          -c: cogen only (no specialisation)'),nl,
      print('          -i: no interface clauses for specialized predicates)'),nl,
      print('          -ciao generate code for Ciao-Prolog'),nl,
      print('          -xsb: generate code for XSB-Prolog (incompatible with -ciao)'),nl,
      print('          -t value: timeout after value ms' ),nl,           
      print('Note:     the .pl and the . after Atom are optional'),nl.
    


% add a dot at the end; in case user forgets
add_dot([],".").
add_dot(".",".") :- !.
add_dot([A|T],[A|R]) :- add_dot(T,R).

get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,Opt,Values) ->       
       ( append(Values, Rest, T),
	 RT = Rest,  
	 Options = [Opt|OT], Args = AT
       )   
   ;
       (
	 Options = OT,     Args = [X|AT],
	 RT = T
       )
   ),
   get_options(RT,OT,AT).

recognised_option('-c',cogen_only,[]).
recognised_option('-s',silent,[]).
recognised_option('-k',keep_memo,[]).
recognised_option('-r',rebuild_everything,[]).
recognised_option('-d',debug_mode,[]).
recognised_option('-i',no_interface_clauses,[]).
recognised_option('-d1',debug_mode1,[]).
recognised_option('-d2',debug_mode,[]).
recognised_option('-d3',debug_mode3,[]).
recognised_option('-xsb',xsb_mode,[]).
recognised_option('-ciao',ciao_mode,[]).
recognised_option('-m',modular,[]).

recognised_option('-t', timeout(T), [T]).
recognised_option('-errorlog', error_log(File), [File]).



/* replace argv(Nr) by argument from command line */
adapt_query(X,V,_Argv) :- var(X),!,X=V.
adapt_query(argv(Nr),Res,Argv) :- (nth(Nr,Argv,Res) -> true
                                                    ; (print_debug_message('Unspecified argument'),Res=argv(Nr))),!.
adapt_query(X,Res,Argv) :- nonvar(X), X=..[Func|Args],!,l_adapt_query(Args,AArgs,Argv), Res =..[Func|AArgs].
adapt_query(X,X,_).

l_adapt_query([],[],_).
l_adapt_query([A|T],[AA|AT],Argv) :- adapt_query(A,AA,Argv), l_adapt_query(T,AT,Argv).



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