:- module(sicstus_charsio,
	[
	    format_to_chars/3,
	    format_to_chars/4,
	    write_to_chars/2,
	    write_to_chars/3,
	    write_term_to_chars/3,
	    write_term_to_chars/4,
	    atom_to_chars/2,
	    atom_to_chars/3,
	    number_to_chars/2,
	    number_to_chars/3,
	    read_from_chars/2,
	    read_term_from_chars/3,
	    open_chars_stream/2,
	    with_output_to_chars/2,
	    with_output_to_chars/3,
	    with_output_to_chars/4,
	    at_end_of_stream/1,
	    get0/2
	],[]).

format_to_chars(_,_,_).
format_to_chars(_,_,_,_).
write_to_chars(_,_).
write_to_chars(_,_,_).
write_term_to_chars(_,_,_).
write_term_to_chars(_,_,_,_).
atom_to_chars(_,_).
atom_to_chars(_,_,_).
number_to_chars(_,_).
number_to_chars(_,_,_).
read_from_chars(_,_).
read_term_from_chars(_,_,_).
open_chars_stream(_,_).
with_output_to_chars(_,_).
with_output_to_chars(_,_,_).
with_output_to_chars(_,_,_,_).
at_end_of_stream(_).

get0(A,B):-
	get_code(A,B).


