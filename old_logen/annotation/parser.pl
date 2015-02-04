:- module('parser', [parse_file_syntax/2, parse_string_syntax/2]).


:- use_module(read).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module('../prob/error_manager.pl').

/*test(Term,TermPos,Comments) :-
	open_chars_stream("% this is the text\np(A,B,C) :- {X=C+1},term([A,B,C],B,C),\nterm1(A,B,C).\n\n", S),
	set_input(S),
	syntax_parse(Term,TermPos,Comments).


	
parse_string_syntax(AtomString, Syntax) :-
	name(AtomString, String),
	open_chars_stream(String, Stream),
	set_input(Stream),
	parse_stream(Syntax),
	close(Stream).*/




	
parse_file_syntax(Filename,Syntax) :-
	seeing(Old),
	see(Filename),
	parse_stream(Syntax),
	seen,
	see(Old).

parse_stream(Syntax) :-
	(syntax_parse(_,_,Syntax) ->
	    (
	      true
	      
	      );
	    (
	      error_msg_syntax(AtomList,_,Err),
	      get_error_position(AtomList, Pos),
	      add_error(parse,'Parse Error:',Err),
	      
	      Syntax = [error, Pos]
	      )
	).

get_error_position([atom(_,Pos)|_], Pos) :- !.
get_error_position(_, 0).
		
	
syntax_parse(Term, TermPos,Syntax) :-	
	portable_read_position(Term,TermPos,Comments),
	(Term = end_of_file ->
	    (
	      Syntax = []
	    )
	;
	    (
	      create_syntax_from_tokens(Comments, TokSyntax),
	      create_syntax_info(Term,TermPos,Syn),
	      append(Syn, TokSyntax, LSyntax), !,
	      syntax_parse(_,_, RecSyntax),
	      append(LSyntax, RecSyntax, Syntax)
	    )).
	
create_syntax_from_tokens([], []).
create_syntax_from_tokens([list(A,B)|Rest], [ A, B,list| SRest]) :-
	create_syntax_from_tokens(Rest,SRest).
create_syntax_from_tokens([comment(A,B)|Rest], [A, B,comment| SRest]) :-
	create_syntax_from_tokens(Rest,SRest).

	


create_syntax_info((Head:-Body),(:-(_Pos, HPos,BPos)), Syntax) :-
        create_syntax_head(Head,HPos, SHead),
	create_syntax_body(Body,BPos, SBody),
	append(SHead, SBody, Syntax),!.
create_syntax_info(Fact,HPos, Syntax) :-	
	create_syntax_head(Fact,HPos, Syntax).


create_syntax_info(_,_,[]).

create_syntax_head(H, HPos, [Start,End,head]) :-
	create_syntax_call(H, HPos, [Start,End,_]).

create_syntax_body((A,B), (APos,BPos), Syntax) :- !,
	create_syntax_call(A,APos, ASyntax),
	create_syntax_body(B,BPos, BSyntax),
	append(ASyntax, BSyntax, Syntax).

create_syntax_body(A, APos, Syntax) :-
	create_syntax_call(A,APos, Syntax).

create_syntax_call(_, HPos, [Start,End,term]) :-
	arg(1,HPos, Start),
	functor(HPos,Func, _),
	get_end_pos(Start,Func, End).
	

get_end_pos(Start, Atom, End) :-
	name(Atom,AtomS),
	length(AtomS,L),
	End is Start + L.

