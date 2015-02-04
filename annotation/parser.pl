%   File   : read.pl
%   Authors: David H. D. Warren + Richard A. O'Keefe
%   Updated: 29 Aug 1989
%   Purpose: Read Prolog terms in Dec-10 syntax.

/*  This code was originally written at the University of Edinburgh.
    David H. D. Warren wrote the first version of the parser.
    Richard A. O'Keefe ripped it out of the Dec-10 Prolog system
    and made it use only user-visible operations.  He also added
    the feature whereby P(X,Y,Z) is read as call(P,X,Y,Z).
    Alan Mycroft reorganised the code to regularise the functor modes.
    This is both easier to understand (there are no more '?'s),
    and also fixes bugs concerning the curious interaction of cut with
    the state of parameter instantiation.
    Richard A. O'Keefe then took it over again and made a number of
    other changes.  There are three intentional differences between
    this and the Dec-10 Prolog parser:

	"predicate variables" as syntactic saccharine for call/N

	when there is a syntax error, DEC-10 Prolog will backtrack
	internally and read the next term.  This fails.  If you
	call portable_read/1 with an uninstantiated argument, 
	failure means a syntax error.  You can rely on it.

	", .." is not accepted in place of "|".  This was always a
	parser feature, not a tokeniser feature:  any amount of
	layout and commentary was allowed between the "," and the
	"..".  It wouldn't be hard to allow again.

    The tokeniser returns a list of the following tokens:

	var(_,Name)		a variable with name Name
	atom(AnAtom)		an atom
	number(Num)		an integer or float
	string(Chars)		a list of character codes

	'{' '[' '(' ' ('	punctuation marks
	'}' ']' ')'		' (' is the usual "(", '(' is "("
	    '|' ','		coming directly after an atom.
*/

:- module(parser, [
	portable_gread/1,
	portable_gread/2,
	portable_read/1,
	portable_read/2,
	remove_pos/2,
	portable_read_position/3,		 
	bind_named_variables/2,
	error_msg_syntax/3,
	test_parse/3
		
   ]).


:- ensure_loaded('../op_decl.pl').
:- op(200,yfx,'--->').


test_parse(T,PT,Comms) :- see('test.pl'), portable_read_position(T,PT,Comms), seen.

% :- use_module(library(charsio)).

:- dynamic error_msg_syntax/3.
:- dynamic get_user_error/1.

%get_user_error(user_error).
get_user_error(user_output).
clear_errors :- retractall(error_msg_syntax(_,_,_)).
%steves remove position information code

/* this predicate will remove position information from parse tree */
remove_pos(X,X) :- var(X), !.

remove_pos('$VAR'(_Pos,_Name,X),X) :- var(X),!.
remove_pos([],[]) :- !.
remove_pos([H|T], [NH|NT]) :- !,
	remove_pos(H,NH),
	remove_pos(T,NT).
remove_pos((A,B), (NA,NB)) :- !,
	remove_pos(A,NA),
	remove_pos(B,NB).
remove_pos(Atom, Atom) :-
	functor(Atom, _F, 0).
remove_pos(Term, NewTerm) :-
	nonvar(Term),
	Term =.. [Func, _Pos|Args],
	remove_pos(Args, NArgs),
	NewTerm =.. [Func|NArgs].


:- use_module(tokens, [
	read_tokens/1,
	read_tokens/2,
        get_current_position/1		     
   ]).


sccs_id('"@(#)89/08/29 read.pl    33.1"').


portable_read_position(Term, PosTerm, Syntax) :-
	portable_read(PosTerm,Syntax),
	%portray_clause(posterm(PosTerm)),
	remove_pos(PosTerm, Term).

%   portable_read(?Answer)
%   reads a term from the current input stream and unifies it with Answer.
%   Dec-10 syntax is used.  If a syntax error is found, the parser FAILS.
portable_read(Answer, Syntax) :-
	%portray_clause(user_error,start_portable_read_tokens),
	read_tokens(Tokens),
	%portray_clause(user_error,portable_read_tokens(Tokens)),
	remove_comments(Tokens, ParseTokens, Syntax),
	parse(ParseTokens, Answer).

%   portable_read(?Answer, ?Variables)
%   reads a term from the current input stream and unifies it with
%   Answer.  Variables is bound to a list of [Atom=Variable] pairs.
%   The variables are listed in the order in which they appeared, but
%   anonymous variables `_' do not appear at all.

portable_read(Answer, Variables, Syntax) :-
	read_tokens(Tokens, Variables),
	remove_comments(Tokens, ParseTokens, Syntax),
	parse(ParseTokens, Answer).


remove_comments([], [] ,[]).
remove_comments([comment(S,E)|Xs], Ys, [comment(S,E)|Zs]) :-
	!,
	remove_comments(Xs,Ys,Zs).
remove_comments(['['/Pos|Xs], ['['|Ys], [list(Pos, Pos2)|Zs]) :-
	!,
    Pos2 is Pos + 1,
	remove_comments(Xs,Ys,Zs).
remove_comments([']'/Pos|Xs], [']'|Ys], [list(Pos, Pos2)|Zs]) :-
	!,
    Pos2 is Pos + 1,
	remove_comments(Xs,Ys,Zs).

remove_comments([Token|Xs], [Token|Ys], Zs) :-
	remove_comments(Xs,Ys,Zs).


%   portable_gread(?Answer)
%   reads a term from the current input stream, treating variable names
%   as constants.  In particular, the variable name "_" is treated as
%   the atom '_'; every occurrence of it is mapped to that atom.  The
%   right way to hack this is to bend the tokeniser; but this is for
%   compatibility with LPA Prolog, which wants a version that gives you
%   the names (which is why LPA Prolog gets muddled about variables and
%   constants with the same spelling).

portable_gread(Answer) :-
	read_tokens_1(RawTokens, _),
	replace_variables_by_atoms(RawTokens, Tokens, yes),
	parse(Tokens, Answer).


%   portable_gread(?Answer, ?VarNames)
%   reads a term from the current input stream, treating variable names
%   as constants (even as function symbols) and returning all the names
%   of things that looked like variables.  Note that this means that a
%   term like "Fred(a)" will be accepted, and Fred will be returned as
%   one of the variable names, but the atom 'Fred' will NOT be considered
%   as occurring anywhere in the result; the functor Fred/1 is NOT an
%   occurrence of the atom Fred.

portable_gread(Answer, VarNames) :-
	read_tokens_1(RawTokens, Variables, _),
	replace_variables_by_atoms(RawTokens, Tokens, HadAnon),
	parse(Tokens, Answer),
	collect_variable_names(HadAnon, VarNames, Variables).


replace_variables_by_atoms([], [], _).
%replace_variables_by_atoms([var(_,X)|Xs], [atom(X)|Ys], HadAnon) :- !,
replace_variables_by_atoms([var(_,X,_)|Xs], [atom(X,Pos)|Ys], HadAnon) :- !,
	%portray_clause(user_output,postion_info(Pos,X)),
	( X == '_' -> HadAnon = yes ; true ),
	replace_variables_by_atoms(Xs, Ys, HadAnon).
replace_variables_by_atoms([X|Xs], [X|Ys], HadAnon) :-
	replace_variables_by_atoms(Xs, Ys, HadAnon).


collect_variable_names(no, Names, Vars) :- !,
	collect_variable_names(Vars, Names).
collect_variable_names(yes, Names, Vars) :-
	collect_variable_names(Vars, Names).

collect_variable_names([], []).
collect_variable_names([Name=_|Pairs], [Name|Names]) :-
	collect_variable_names(Pairs, Names).



%   bind_named_variables(DesiredBindings, Dictionary)
%   takes a variable Dictionary as returned by read_tokens/2 or
%   portable_read/2 (that is, a list of 'Name'=Variable pairs)
%   and a list of DesiredBindings (again, a list of 'Name'=Value
%   pairs, except that the Values are any terms you like).
%   For each 'Name'=Value pair in DesiredBindings, the
%   corresponding Variable in Dictionary is unified with Value.
%   What is this for?  It lets you read a term and then supply
%   values for variables by name, e.g.
%	| ?- portable_read(Term, Dictionary),
%	|    bind_named_variables(['X'=fred], Dictionary).
%	|: /* input */ f(X,Y,X,Y).
%	Dictionary = ['X'=fred,'Y'=_53],
%	Term = f(fred,_53,fred,_53)

bind_named_variables([], _).
bind_named_variables([Name=Value|Bindings], Dictionary) :-
	bind_named_variable(Dictionary, Name, Value),
	bind_named_variables(Bindings, Dictionary).

bind_named_variable([], _, _).
bind_named_variable([Name=Variable|_], Name, Value) :- !,
	Variable = Value.
bind_named_variable([_|Dictionary], Name, Value) :-
	bind_named_variable(Dictionary, Name, Value).


%   expect(Token, TokensIn, TokensOut)
%   reads the next token, checking that it is the one expected, and
%   giving an error message if it is not.  It is used to look for
%   right brackets of various sorts, as they're all we can be sure of.

expect(Token, [Token|Rest], Rest) :- !.
expect(Token, S0, _) :-
	syntax_error([Token,or,operator,expected], S0).


parse(Tokens, Answer) :-
	clear_errors,
	(   parse(Tokens, 1200, Term, LeftOver),
	    all_read(LeftOver),
	    !,
	    Answer = Term
	;   syntax_error(Tokens)
	).

%   all_read(+Tokens)
%   checks that there are no unparsed tokens left over.  Note that
%   parse/4 is not determinate, so the failure of syntax_error/4
%   may force parse/4 to look for another solution.

all_read([]).
all_read([Token|S]) :-
	syntax_error([operator,expected,after,expression], [Token|S]).




%   parse(+TokenList, +Precedence, -Term, -LeftOver)
%   parses a Token List in a context of given Precedence,
%   returning a Term and the unread Left Over tokens.

parse([], _, _, _) :-
	syntax_error([expression,expected], []).
parse([Token|RestTokens], Precedence, Term, LeftOver) :-
	parse(Token, RestTokens, Precedence, Term, LeftOver).


/*  There are several occurrences of parse/N in the context
	parse(S1, Prio, Term, S2),
	expect_one_of(PunctuationMarks, S2)
	!
    The punctuation marks in question are
	, | ]			[lists]
	}			{goals}
	, )			compound(terms)
	)			(parenthesised,terms)
	-|			complete term; no actual token, alas.
    Now the point here is that if we do not find the right kind of token,
    we'll store an error message and then try to provoke parse/4 into
    finding another parse which does end at the right spot.  We do NOT
    like having cuts all over the place.  So to try to make things clearer,
    we have a predicate full_parse/4 which goes as far as it possibly can.
*/

%   parse(+Token, +RestTokens, +Precedence, -Term, -LeftOver)

parse('}', S0, _, _, _) :- cannot_start('}', S0).
parse(']', S0, _, _, _) :- cannot_start(']', S0).
parse(')', S0, _, _, _) :- cannot_start(')', S0).
parse(',', S0, _, _, _) :- cannot_start(',', S0).
parse('|', S0, _, _, _) :- cannot_start('|', S0).

parse(string(Chars), S0, Precedence, Answer, S) :-
	exprtl0(S0, Chars, Precedence, Answer, S).

parse(number(Number), S0, Precedence, Answer, S) :-
	exprtl0(S0, Number, Precedence, Answer, S).

parse('[', [']'|S1], Precedence, Answer, S) :- !,
	read_atom([]/0, S1, Precedence, Answer, S).
parse('[', S1, Precedence, Answer, S) :-
	parse(S1, 999, Arg1, S2),		/* look for ",", "|", or "]" */
	read_list(S2, RestArgs, S3),
	!,
	exprtl0(S3, [Arg1|RestArgs], Precedence, Answer, S).

parse('(', S1, Precedence, Answer, S) :-
	parse(S1, 1200, Term, S2),		/* look for ")" */
	expect(')', S2, S3),
	!,
	exprtl0(S3, Term, Precedence, Answer, S).

parse(' (', S1, Precedence, Answer, S) :-
	parse(S1, 1200, Term, S2),		/* look for ")" */
	expect(')', S2, S3),
	!,
	exprtl0(S3, Term, Precedence, Answer, S).

parse('{'/_Pos, ['}'|S1], Precedence, Answer, S) :- !,
	read_atom('{}', S1, Precedence, Answer, S).

parse('{'/Pos, S1, Precedence, Answer, S) :-
	parse(S1, 1200, Term, S2),		/* look for "}" */
	expect('}', S2, S3),
	!,	
	exprtl0(S3, '{}'(Pos,Term), Precedence, Answer, S).

parse(var(Variable,Name,Pos), ['('|S1], Precedence, Answer, S) :- !,
	parse(S1, 999, Arg1, S2),		% look for "," or ")"
	read_args(S2, RestArgs, S3),
	!,
	Term =.. [call,'$VAR'(Pos,Name,Variable),Arg1|RestArgs],
	exprtl0(S3, Term, Precedence, Answer, S).

parse(var(Variable,Name,Pos), S0, Precedence, Answer, S) :-
	exprtl0(S0, '$VAR'(Pos,Name,Variable), Precedence, Answer, S).

%parse(atom(Atom), S0, Precedence, Answer, S) :-
parse(atom(Atom,P), S0, Precedence, Answer, S) :-
%	read_atom(Atom, S0, Precedence, Answer, S).
	read_atom(Atom/P, S0, Precedence, Answer, S).

%read_atom(-, [number(Number)|S1], Precedence, Answer, S) :- !,
read_atom((-)/_Pos, [number(Number)|S1], Precedence, Answer, S) :- !,
	Negative is -Number,
	exprtl0(S1, Negative, Precedence, Answer, S).

read_atom(Functor/Pos, ['('|S1], Precedence, Answer, S) :- !,
	parse(S1, 999, Arg1, S2),		% look for "," or ")"
	read_args(S2, RestArgs, S3),
	!,
	Term =.. [Functor,Pos,Arg1|RestArgs],
	%length(RestArgs, L),
	%L1 is L +1, 
	%Term = Functor/L1,	
	exprtl0(S3, Term, Precedence, Answer, S).
read_atom(Op/Pos, S0, Precedence, Answer, S) :-
	prefixop(Op, Oprec, Aprec),
	!,
    /*	At this point we have consumed Op, which is an atom which might
	be a prefix operator of precedence Prec, or it might just be an
	atom.  We consider two things:  what does the next token tell
	us, and what does the local precedence tell us?  The next token
	tells us that
	it may not be an operator	}, ], ), |, ,, end of list
	it must be an operator		var, number, string, {, [, ' ('
	it might be an operator		atom, '('
    */
	possible_right_operand(S0, Flag),
	(   Flag < 0 ->		% can't be a prefix op
	    Term =.. [Op, Pos],
	    %exprtl0(S0, Op/Pos, Precedence, Answer, S)
	    exprtl0(S0, Term, Precedence, Answer, S)
	;   Oprec > Precedence ->
	    syntax_error([prefix,operator,Op,
			  in,context,with,precedence,Precedence], S0)
	;   Flag > 0 ->		% must be a prefix op
	    parse(S0, Aprec, Arg, S1),
	    !,
	    Term =.. [Op,Pos,Arg],
	    exprtl(S1, Oprec, Term, Precedence, Answer, S)
	;/* Flag = 0; it MIGHT be an atom */
	    peepop(S0, S1),
	    prefix_is_atom(S1, Oprec), % can't cut but would like to
	    exprtl(S1, Oprec, Op/Pos, Precedence, Answer, S)
	;/* Flag = 0; it CAN'T be an atom (we tried) */
	    parse(S0, Aprec, Arg, S1),
	    !,
	    Term =.. [Op,Pos,Arg],
	    exprtl(S1, Oprec, Term, Precedence, Answer, S)
	).
read_atom(Atom/Pos, S0, Precedence, Answer, S) :-
	Term =.. [Atom,Pos],
	exprtl0(S0, Term, Precedence, Answer, S).
%	exprtl0(S0, Atom, Precedence, Answer, S).


cannot_start(Token, S0) :-
	syntax_error([Token,cannot,start,an,expression], S0).


%   read_args(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ')' and returns a list of terms.

read_args([','|S1], [Term|Rest], S) :- !,
	parse(S1, 999, Term, S2),
	!,
	read_args(S2, Rest, S).
read_args([')'|S], [], S) :- !.
read_args(S, _, _) :-
	syntax_error([', or )',expected,in,arguments], S).


%   read_list(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ['|' expr(999)] ']' and returns a list of terms.

read_list([], _, _) :-
	syntax_error([', | or ]',expected,in,list], []).
read_list([Token|S1], Rest, S) :-
	read_list(Token, S1, Rest, S).

read_list(',', S1, [Term|Rest], S) :- !,
	parse(S1, 999, Term, S2),
	!,
	read_list(S2, Rest, S).
read_list('|', S1, Rest, S) :- !,
	parse(S1, 999, Rest, S2),
	!,
	expect(']', S2, S).
read_list(']', S1, [], S1) :- !.
read_list(Token, S1, _, _) :-
	syntax_error([', | or ]',expected,in,list], [Token|S1]).


%   When you had :- op(120, fx, !), op(110, xf, !),
%   the input [! a !] was incorrectly rejected.  The reason for this
%   was that the "]" following the "!" should have told us that the
%   second "!" *couldn't* be a prefix operator, but the postfix
%   interpretation was never tried.

possible_right_operand([],	-1).
possible_right_operand([H|T],	Flag) :-
	possible_right_operand(H, Flag, T).

possible_right_operand(var(_,_,_),	 1, _).
possible_right_operand(number(_),	 1, _).
possible_right_operand(string(_),	 1, _).
possible_right_operand(' (',		 1, _).
possible_right_operand('(',		 0, _).
possible_right_operand(')',		-1, _).
possible_right_operand('[',		 0, [']'|_]) :- !.
possible_right_operand('[',		 1, _).
possible_right_operand(']',		-1, _).
possible_right_operand('{',		 0, ['}'|_]) :- !.
possible_right_operand('{',		 1, _).
possible_right_operand('}',		-1, _).
possible_right_operand(',',		-1, _).
possible_right_operand('|',		-1, _).
%possible_right_operand(atom(_),		 0, _).
possible_right_operand(atom(_,_),		 0, _).

%   The next clause fixes a bug concerning "mop dop(1,2)" where
%   mop is monadic and dop dyadic with higher Prolog priority.

%peepop([atom(F),'('|S1], [atom(F),'('|S1]) :- !.
%peepop([atom(F)|S1], [infixop(F,L,P,R)|S1]) :- infixop(F, L, P, R).
%peepop([atom(F)|S1], [postfixop(F,L,P)|S1]) :- postfixop(F, L, P).
peepop([atom(F,Pos),'('|S1], [atom(F,Pos),'('|S1]) :- !.
peepop([atom(F,Pos)|S1], [infixop(F/Pos,L,P,R)|S1]) :- infixop(F, L, P, R).
peepop([atom(F,Pos)|S1], [postfixop(F/Pos,L,P)|S1]) :- postfixop(F, L, P).
peepop(S0, S0).


%   prefix_is_atom(+TokenList, +Precedence)
%   is true when the right context TokenList of a prefix operator
%   of result precedence Precedence forces it to be treated as an
%   atom, e.g. (- = X), p(-), [+], and so on.

prefix_is_atom([Token|_], Precedence) :-
	prefix_is_atom(Token, Precedence).

prefix_is_atom(infixop(_,L,_,_), P) :- L >= P.
prefix_is_atom(postfixop(_,L,_), P) :- L >= P.
prefix_is_atom(')', _).
prefix_is_atom(']', _).
prefix_is_atom('}', _).
prefix_is_atom('|', P) :- 1100 >= P.
prefix_is_atom(',', P) :- 1000 >= P.
prefix_is_atom([],  _).


%   exprtl0(+Tokens, +Term, +Prec, -Answer, -LeftOver)
%   is called by parse/4 after it has read a primary (the Term) which
%   has not caused the next token to be peeked at.  That is, the
%   Term was a constant, a variable, a plain compound term, or was
%   bracketed with () or [] or {}.  An atom which might have been an
%   operator or a term built up out of operators will result in a call
%   to exprtl/6 instead.
%   It checks for following postfix or infix operators.

exprtl0([], Term, _, Term, []).
exprtl0([Token|S1], Term, Precedence, Answer, S) :-
	exprtl0(Token, Term, Precedence, Answer, S, S1).

exprtl0('}', Term, _, Term, ['}'|S1], S1).
exprtl0(']', Term, _, Term, [']'|S1], S1).
exprtl0(')', Term, _, Term, [')'|S1], S1).
exprtl0(',', Term, Precedence, Answer, S, S1) :-
	(   Precedence >= 1000 ->
	    parse(S1, 1000, Next, S2),
	    !,
	    exprtl(S2, 1000, (Term,Next), Precedence, Answer, S)
	;   Answer = Term, S = [','|S1]
	).
exprtl0('|', Term, Precedence, Answer, S, S1) :-
	(   Precedence >= 1100 ->
	    parse(S1, 1100, Next, S2),
	    !,
	    exprtl(S2, 1100, (Term ; Next), Precedence, Answer, S)
	;   Answer = Term, S = ['|'|S1]
	).
exprtl0(string(S), _, _, _, _, S1) :-
	cannot_follow(chars, string(S), S1).
exprtl0(number(N), _, _, _, _, S1) :-
	cannot_follow(number, number(N), S1).
exprtl0('{', Term, Precedence, Answer, S, ['}'|S1]) :- !,
	exprtl0_atom('{}', Term, Precedence, Answer, S, S1).
exprtl0('{', _, _, _, _, S1) :-
	cannot_follow(brace, '{', S1).
exprtl0('[', Term, Precedence, Answer, S, [']'|S1]) :- !,
	exprtl0_atom('[]', Term, Precedence, Answer, S, S1).
exprtl0('[', _, _, _, _, S1) :-
	cannot_follow(bracket, '[', S1).
exprtl0('(', _, _, _, _, S1) :-
	cannot_follow(parenthesis, '(', S1).
exprtl0(' (', _, _, _, _, S1) :-
	cannot_follow(parenthesis, '(', S1).
exprtl0(var(A,B,P), _, _, _, _, S1) :-
	cannot_follow(variable, var(A,B,P), S1).
%exprtl0(atom(F), Term, Precedence, Answer, S, S1) :-
exprtl0(atom(F,P), Term, Precedence, Answer, S, S1) :-
	exprtl0_atom(F/P, Term, Precedence, Answer, S, S1).

exprtl0_atom(F/Pos, Term, Precedence, Answer, S, S1) :-
	ambigop(F, Precedence, L1, O1, R1, L2, O2),
	!,
	(   prefix_is_atom(S1, Precedence),
	    !,
	    exprtl([postfixop(F/Pos,L2,O2) |S1], 0, Term, Precedence, Answer, S)
	;   exprtl([infixop(F/Pos,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S)
	;   exprtl([postfixop(F/Pos,L2,O2) |S1], 0, Term, Precedence, Answer, S)
	).
exprtl0_atom(F/Pos, Term, Precedence, Answer, S, S1) :-
	infixop(F, L1, O1, R1),
	!,
	exprtl([infixop(F/Pos,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S).
exprtl0_atom(F/Pos, Term, Precedence, Answer, S, S1) :-
	postfixop(F, L2, O2),
	!,
	exprtl([postfixop(F/Pos,L2,O2) |S1], 0, Term, Precedence, Answer, S).
exprtl0_atom(X, _, _, _, _, S1) :- !,
%	syntax_error([non-operator,X,follows,expression], [atom(X)|S1]).
	syntax_error([non-operator,X,follows,expression], [atom(X,_)|S1]).

cannot_follow(Type, Token, Tokens) :-
	syntax_error([Type,follows,expression], [Token|Tokens]).





%   exprtl(+S0, +C, +Term, +Precedence, -Answer, -S)
%   is called by after_prefix_op or exprtl0 when the first token of
%   S0 has already been peeked at.  Basically, it parses
%	{ <infixop> <operand> | <postfixop> }*

exprtl([], _, Term, _, Term, []).
exprtl([Token|Tokens], C, Term, Precedence, Answer, S) :-
	exprtl(Token, C, Term, Precedence, Answer, S, Tokens).

exprtl(infixop(F/Pos,L,O,R), C, Term, Precedence, Answer, S, S1) :-
	Precedence >= O, C =< L,
	!,
	parse(S1, R, Other, S2),
	%Expr =.. [F,Term,Other], /*!,*/
	%steve
	Expr =.. [F,Pos,Term,Other], /*!,*/
	exprtl(S2, O, Expr, Precedence, Answer, S).

exprtl(postfixop(F/Pos,L,O), C, Term, Precedence, Answer, S, S1) :-
	Precedence >= O, C =< L,
	!,
	Expr =.. [F,Pos,Term],
	peepop(S1, S2),
	exprtl(S2, O, Expr, Precedence, Answer, S).

exprtl(',', C, Term, Precedence, Answer, S, S1) :-
	Precedence >= 1000, C < 1000,
	!,
	parse(S1, 1000, Next, S2), /*!,*/
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).

exprtl('|', C, Term, Precedence, Answer, S, S1) :-
	Precedence >= 1100, C < 1100,
	!,
	parse(S1, 1100, Next, S2), /*!,*/
	exprtl(S2, 1100, (Term ; Next), Precedence, Answer, S).

exprtl(Token, _, Term, _, Term, [Token|Tokens], Tokens).


%   This business of syntax errors is tricky.  When an error is detected,
%   we have to write out a message.  We also have to note how far it was
%   to the end of the input, and for this we are obliged to use the data-
%   base.  Then we fail all the way back to parse(), and that prints the
%   input list with a marker where the error was noticed.  If subgoal_of
%   were available in compiled code we could use that to find the input
%   list without hacking the data base.  The really hairy thing is that
%   the original code noted a possible error and backtracked on, so that
%   what looked at first sight like an error sometimes turned out to be
%   a wrong decision by the parser.  This version of the parser makes
%   fewer wrong decisions, and my goal was to get it to do no backtracking
%   at all.  This goal has not yet been met, and it will still occasionally
%   report an error message and then decide that it is happy with the input
%   after all.  Sorry about that.


syntax_error(Message, List) :-
	convert_msg_list_to_string(Message, MsgString),
	name(Msg, MsgString),
	throw(error(syntax_error(0,0, Msg, List),read_term_pos/2)).
/*
	current_output(CurrentOutput),
	set_output(user_error),
	format('~N**', []),
	display_list(Message),
	set_output(CurrentOutput),
	length(List, Length),
	recorda(syntax_error, length(Length), _),
	!, fail.
*/

convert_msg_list_to_string([], []).
convert_msg_list_to_string([Item|Rest], Message) :-
	safer_name(Item,ItemS),
	convert_msg_list_to_string(Rest, Message1),
	append(ItemS, [32|Message1], Message).

	
safer_name(A,As) :-
	atom(A), !,name(A,As).

safer_name(L-R, As) :-
	safer_name(L,Ls),
	safer_name(R,Rs),!,
	append(Ls, [45|Rs],As). 


safer_name(A/Ar, As) :-
	safer_name(A,As).

safer_name(A," error_in_safer_name ").



display_list([]) :-
	nl.
display_list([Head|Tail]) :- !,
	put_code(32),
	display_token(Head),
	display_list(Tail).

syntax_error(List) :-

	recorded(syntax_error, length(AfterError), Ref),
	erase(Ref),
	length(List, Length),
	BeforeError is Length-AfterError,
	format(user_error, "~@", [display_list(List, BeforeError)]),
	assert(error_msg_syntax(List,BeforeError, Err_out)),
	!, fail.

display_list(X, 0) :- !,
	write('<<here>> '),
	display_list(X, 99999).
display_list([Head|Tail], BeforeError) :- !,
	display_token(Head),
	put_code(32),
	Left is BeforeError-1,
	display_list(Tail, Left).
display_list([], _) :-
	nl.

%display_token(atom(X))	 :- !,	writeq(X).
display_token(atom(X,_))	 :- !,	writeq(X).
display_token(var(_,X,_))	 :- !,	write(X).
display_token(number(X)) :- !,	write(X).
display_token(string(X)) :- !,	Q is """", put_code(Q), display_string(X, Q).
display_token(X)	 :-	write(X).

display_string([], Quote) :-
	put_code(Quote).
display_string([Quote|Chars], Quote) :- !,
	put_code(Quote), put_code(Quote),		% later, put(0'\), put(Quote)
	display_string(Chars, Quote).
display_string([Char|Chars], Quote) :-
	put_code(Char),
	display_string(Chars, Quote).


%.  The original public-domain code was written to go with a similarly
%   public-domain version of op/3 and current_op/3 where the following
%   three tables were the primary reality.  Whether they are or aren't,
%   only current_op/3 is (currently) directly available to customers.

prefixop(F, O, Q) :-
	(   current_op(O, fx, F) -> Q is O-1
	;   current_op(O, fy, F) -> Q is O
	).

postfixop(F, P, O) :-
	(   current_op(O, xf, F) -> P is O-1
	;   current_op(O, yf, F) -> P is O
	).

infixop(F, P, O, Q) :-
	(   current_op(O, xfy, F) -> P is O-1, Q is O
	;   current_op(O, xfx, F) -> P is O-1, Q is P
	;   current_op(O, yfx, F) -> Q is O-1, P is O
	).

%   ambigop(Symbol, Precedence, L1, O1, R1, L2, O2)
%   is true when Symbol has an infix (L1,O1,R1) and a postfix (L2,O2)
%   definition both of which are compatible with Precedence.
%   I assume here that postfixop and infixop have been coded so that
%   they are determinate when called with a constant first argument.

ambigop(F, Precedence, L1, O1, R1, L2, O2) :-
	postfixop(F, L2, O2),
	O2 =< Precedence,
	infixop(F, L1, O1, R1),
	O1 =< Precedence.
