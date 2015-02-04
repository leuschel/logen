:- module(logen_codesize,[get_code_size/2]).


:- use_module(library(system)).
%:- use_module(library(charsio)).
%get_code_size(File, Size) :-
%	file_property(File, size(Size)),!.

%% only works for .pl and .spec file as sicstus is annoying...
get_code_size(File,Size) :-
	(atom_concat(Base,'.pl', File) ->
	    true
	;
	    atom_concat(Base,'.spec', File)
	),
	atom_concat(Base,'.ql', QlFile),
	(file_exists(QlFile) ->
	    %file_property(QlFile, SizeOld),
	    delete_file(QlFile)
	;
	    true
	),
			
	fcompile(File),
	file_property(QlFile, size(Size)).


	
	

	

:- use_module(library(charsio)).

test_codesize(0,_).
test_codesize(Iter,OldSize) :-
	Iter >0,
	%open('testcodesize.pl', write, S),
	length(L, Iter),
	format_to_chars("sicstus --goal \"portray_clause((p(~w) :- p(~w))),halt.\" > testcodesize.pl",[L,L],CmdS),
	name(Cmd, CmdS),
	system(Cmd),
	get_code_size('testcodesize.pl', Size),
	portray_clause(size(Iter,Size)),
	(OldSize = Size ->
	    portray_clause(failed), halt
	;
	    true
	),
	X1 is Iter -1,
	test_codesize(X1,Size).
	