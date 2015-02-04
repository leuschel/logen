/* A simple entry point file; just to allow to run the cogen files */


:- ensure_loaded(sicstus).
                         

:- use_module(library(lists)).
:- use_module('prob/logen_preferences.pl').
:- use_module(logen_messages).

:- use_module(run_cogen).


/* -------------------------------------- */
/*           Command Line Stuff           */
/* -------------------------------------- */

cogen(F,Opts) :-
   print_message('Running Cogen: '), print_message(F),
   print_debug_message(options(Opts)),
   (member(debug_mode,Opts) -> set_preference(gx_debug_mode,2) ; true),
   (member(debug_mode1,Opts) -> set_preference(gx_debug_mode,1) ; true),
   (member(debug_mode3,Opts) -> set_preference(gx_debug_mode,3) ; true),
   (member(xsb_mode,Opts) -> set_preference(prolog_mode_flag,xsb) ; true),
   (member(ciao_mode,Opts) -> set_preference(prolog_mode_flag,ciao) ; true),
   (member(modular,Opts) -> set_preference(modular_spec_mode,true) ; true),
   print_debug_message(starting),
   run_cogen_on_plfile(F,_,GxF),
   print_debug_message(generated(GxF)),!,
  (member(silent,Opts) -> true ; show_file(GxF)).
cogen(F,Opts) :-
   print_message(cogen_failed(F,Opts)).
   

save :- save_program('run_cogen.sav').


% alias cogenrb 'cd $logen_source_directory/; sicstus -l $logen_source_directory/run_cogen_cli.pl --goal "save,halt."'
% alias cogen 'sicstus -r $logen_source_directory/run_cogen.sav --goal "go,halt." -a'
% to make binary:    
% spld --static --output cogen --resources=run_cogen.sav=/run_cogen.sav


runtime_entry(start) :- go.

:- use_module(library(charsio),[read_from_chars/2]).
:- use_module(library(system),[system/1]).

go :-
   init_preferences,
    prolog_flag(argv,ArgV),
    get_options(ArgV,Opts,AX),
    AX = [F],
    cogen(F,Opts).
go :- print('Usage: cogen [-s -k -r -d] File.pl ["Atom."]'),nl,
      print('Switches: -s: silent (do not print gx program)'),nl,
      print('          -d: debug mode (-d1 -d2 -d3 also possible; d=d2)'),nl,
      print('          -ciao generate code for Ciao-Prolog'),nl,
      print('          -xsb: generate code for XSB-Prolog (incompatible with -ciao)'),nl,
      print('Note:     the .pl and the . after Atom are optional'),nl.
    

get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,Opt) -> (Options = [Opt|OT], Args = AT)
                          ; (Options = OT,     Args = [X|AT])
   ),
   get_options(T,OT,AT).

recognised_option('-s',silent).
recognised_option('-d',debug_mode).
recognised_option('-d1',debug_mode1).
recognised_option('-d2',debug_mode).
recognised_option('-d3',debug_mode3).
recognised_option('-xsb',xsb_mode).
recognised_option('-ciao',ciao_mode).

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