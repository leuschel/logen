:- module(sics_tools, [environ/2, 
                       read_from_chars/2, write_to_chars/2, format_to_chars/3,
                       compile_gx_with_ciao/4]).




:- use_module(library(system), [environ/2]).


:- use_module(library(codesio),[format_to_codes/3]).
format_to_chars(FString, FArgs, String) :- format_to_codes(FString,FArgs,String).

:- use_module(library(codesio),[read_from_codes/2]).
read_from_chars(A,B) :- read_from_codes(A,B).

:- use_module(library(codesio),         [write_term_to_codes/3]).
write_to_chars(Term, String) :-
   %write_term_to_codes(Term,String,[quoted(true)]). %,numbervars(true)
   write_term_to_codes(Term,String,[legacy_numbervars(true)]).


:- use_module(library(process)).
:- use_module(error_manager,[add_error/3]).

system_call(Command,Options,ExitCode) :-
    on_exception(E,
          process:process_create(Command, Options,[process(Process)]),
		  (add_error(system_call,"Could not execute command ~w due to exception ~w~n",[Command,E]),fail)),
	process_wait(Process,ExitCode).

:- use_module(error_manager,[add_message/4]).
compile_gx_with_ciao(Ciaoc,GxCpFile,GXFile,ExitCode) :-
	   format_to_chars("~w -o ~w ~w", [Ciaoc,GxCpFile,GXFile], CompileCmdS),
	   add_message(ciao_entry,3, "cmdLine: ~s",[CompileCmdS]),
	   system_call(Ciaoc,['-o',GxCpFile,GXFile],EX),
	   (EX=exit(ExitCode) -> true ; ExitCode=EX).
	   
	   
% --- possibly useful

system_call(Command,Options,ErrorTextAsCodeList,ExitCode) :-
    on_exception(E,
          process:process_create(Command, Options,
		       [process(Process),stderr(pipe(JStderr))]),
		  (add_error(system_call,"Could not execute command ~w due to exception ~w~n",[Command,E]),fail)),

	read_all(JStderr,ErrorTextAsCodeList),
	process_wait(Process,ExitCode).

% read all characters from a stream
read_all(S,Text) :-
	read_all2(S,Lines),
	append(Lines,Text).
read_all2(S,Text) :-
	read_line(S,Line),
	( Line==end_of_file ->
	    Text=[""],close(S)
	; otherwise ->
	    Text = [Line, "\n" | Rest],
	    read_all2(S,Rest)).
