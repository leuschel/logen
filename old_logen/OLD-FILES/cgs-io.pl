:- module(cgsio,[cgs_set_input/1,
		 cgs_close_input/0,
		 cgs_set_output/1,
		 cgs_close_output/0]).

:- dynamic cgs_input/1, cgs_output/1.

cgs_set_input(Filename) :-
	retractall(cgs_input(_)),
	( (Filename==user) -> 
	   (FileStream = user) ;
	   (open(Filename,read,FileStream))
	),!,
	set_input(FileStream),
	assert(cgs_input(FileStream)).

cgs_set_input(Filename) :-
	print(Filename),
	print(': file is not found !'),
	fail.

cgs_close_input :-
	cgs_input(FileStream),!,
	retractall(cgs_input(_)),
	close(FileStream).
cgs_close_input.
% -----------
cgs_set_output(Filename) :-
	retractall(cgs_output(_)),
	( (Filename==user) -> 
	   (FileStream = user) ;
	   (open(Filename,write,FileStream))
	),!,
	set_output(FileStream),
	assert(cgs_output(FileStream)).

cgs_set_output(Filename) :-
	print(Filename),
	print(': file is not found !'),
	fail.

cgs_close_output :-
	cgs_output(FileStream),!,
	retractall(cgs_output(_)),
	close(FileStream).
cgs_close_output.
