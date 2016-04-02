:- module(sics_tools, [environ/2, 
 read_from_chars/2, write_to_chars/2, format_to_chars/3]).




:- use_module(library(system), [environ/2]).


:- use_module(library(codesio),[format_to_codes/3]).
format_to_chars(FString, FArgs, String) :- format_to_codes(FString,FArgs,String).
:- use_module(library(codesio),[read_from_codes/2]).
read_from_chars(A,B) :- read_from_codes(A,B).
:- use_module(library(codesio),         [write_term_to_codes/3]).
write_to_chars(Term, String) :-
   write_term_to_codes(Term,String,[quoted(true)]). %,numbervars(true)

