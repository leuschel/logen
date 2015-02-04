

%   Package: tokens
%   Author : Richard A. O'Keefe
%   Updated: 29 Aug 1989
%   Defines: a public-domain tokeniser in reasonably standard Prolog.

:- module(tokens, [
	read_tokens/1,
	read_tokens/2,
        get_current_position/1
   ]).

get(C) :- get_code(C).
get0(C) :- get(C).

atom_number_codes(A,B) :- number(A), !, number_codes(A, B).
atom_number_codes(A,B) :- atom_codes(A, B).

/*  This tokeniser is meant to complement the library READ routine.
    It recognises Dec-10 Prolog with the following exceptions:

	%( is not accepted as an alternative to {

	%) is not accepted as an alternative to }

	NOLC convention is not supported (read_name could be made to do it)

	,.. is not accepted as an alternative to | (hooray!)

	large integers are not read in as xwd(Top18Bits,Bottom18Bits)

	After a comma, "(" is read as ' (' rather than '('.  This does the
	parser no harm at all, and the Dec-10 tokeniser's behaviour here
	doesn't actually buy you anything.  This tokeniser guarantees never
	to return '(' except immediately after an atom, yielding ' (' every
	other where.

    Some (KL-10, DEC-10 Prolog v3.53) times might be of interest.
    Applied to an earlier version of this file:
	this code took			1.66 seconds
	the Dec-10 tokeniser took	1.28 seconds
	A Pascal version took		0.96 seconds

    Some more (Sun-3/50M, Quintus Prolog 2.4.2) times.
	Test file		tokens.pl	read.pl		long.pl
	This code		5.25 sec	5.57 sec	15.02 sec
	Quintus tokeniser	3.07 sec	2.97 sec	 7.90 sec
	The "ppl" utility	0.40 sec	0.50 sec	 1.50 sec
	ppl + hsearch for ids	0.60 sec	0.70 sec	 2.20 sec
    The main factor explaining the greater speed of the Quintus tokeniser
    is that it reads most of the characters in C, thus making fewer trips
    across the C interface.  It turns out that even in the built-in
    tokeniser, the overhead from the existing I/O system is
	Get0 time		2.57 sec	2.43 sec	 6.60 sec
	Old I/O overhead	0.68 sec	0.68 sec	 1.85 sec
	C interface overhead	0.80 sec	1.25 sec	 3.25 sec
	New I/O total saving:	1.48 sec	1.93 sec	 5.10 sec
	This code MAY be	3.78 sec	3.64 sec	 9.92 sec
	Quintus code MAY be	2.39 sec	2.29 sec	 6.05 sec
	Quintus/ppl+hsearch	3.98 times	3.27 times	 2.74 times
    The savings for this code may be even greater, as a Prolog call and
    the corresponding stack frame creation will be avoided.  It is worth
    noting that this doesn't look as though it will be a good idea to
    replace the existing Quintus tokeniser.  The appropriate C comparison
    is ppl + hsearch (a hash table lookup) applied to atoms and variable
    names.  Given that these numbers are significant to 1 figure, if that,
    a factor of 3..4 times slower than C code doesn't sound too bad.
	
    These figures predict that the Quintus tokeniser with the future 
    new I/O code will be about 30% faster than it is now, and about
    1.6 times as fast as this tokeniser will be then.
    By a curious coincidence, read/1 is now about 1.6 times as fast as
    portable_read/1 is now.

    The Dec-10 tokeniser was called via the old RDTOK interface, with
    which this file is compatible.  One reason for the difference in
    speed is the way variables are looked up: this code uses a linear
    list, while the Dec-10 tokeniser uses some sort of tree.  The Pascal
    version is the program WLIST which lists "words" and their frequencies.
    It uses a hash table.  Another difference is the way characters are
    classified: the Dec-10 tokeniser and WLIST have a table which maps
    ASCII codes to character classes, and don't do all this comparison
    and and memberchking.  We could do that without leaving standard Prolog,
    but what do you want from one evening's work?

    Changes:
	integers can have underscores in them.
	characters in 0'x, "x", and 'x' can be escaped as in C or PopLog.
	So in 'foo\t%t\n' the \t and \n are now single characters.
	Radix notation is no longer exactly like DEC-10 Prolog v3.53.
	The radix may be any number of digits, e.g. 0016'deed.

    BEWARE: this file does _not_ recognise floating-point numbers.
    In order to make this file independent of whether character-escapes
    are enabled or not, the magic numbers
	9	(TAB)	and
	92	( \ )
    are used.
*/

sccs_id('"@(#)89/08/29 tokens.pl    33.1"').


%   read_tokens(-TokenList)
%   returns a list of tokens.  The difference between it and
%   read_tokens/2 is that it doesn't bother to return the dictionary.
%   Now that read_tokens/2 sorts the dictionary, this can be a useful
%   saving.  Note that var(_,_) tokens still contain the variable name,
%   so it is possible to reconstruct the dictionary if you really need it.




read_tokens(TokenList) :-
	get(C1),
	read_tokens(C1, _, ListOfTokens),
	!,
	TokenList = ListOfTokens.
read_tokens([atom(end_of_file,0)]).

%   read_tokens(TokenList, Dictionary)
%   returns a list of tokens, and a dictionary of VarName=Variable pairs
%   in standard order, where the VarNames are atoms and the variables
%   are all the named variables among the tokens.
%   This predicate "primes" read_tokens/3 with the initial blank, and
%   checks for end of file.
%   The way end of file is handled is that everything else FAILS when it
%   hits character -1, sometimes printing a warning.  It might have been
%   an idea to return the atom 'end_of_file' instead of the same token list
%   that you'd have got from reading "end_of_file. ", but (1) this file is
%   for compatibility, and (b) there are good practical reasons for wanting
%   this behaviour.

read_tokens(TokenList, Dictionary) :-
	get(C1),
	read_tokens(C1, Dict, ListOfTokens),
	terminate_list(Dict),		%  fill in the "hole" at the end.
	!,				%  we have to unify explicitly so
	sort(Dict, Dictionary),		%  that we'll read and then check
	TokenList = ListOfTokens.	%  even with filled in arguments.
read_tokens([atom(end_of_file,0)], []).	%  End Of File is only problem.

terminate_list([]).
terminate_list([_|Tail]) :-
	terminate_list(Tail).


read_tokens(C1, Dict, Tokens) :-
    (	C1 =< 0'  ->			% layout: CR, LF, TAB, space, &c
	C1 >= 0,			% FAIL at end of file
	get(C2),
 	read_tokens(C2, Dict, Tokens)
    ;	C1 >= 0'a, C1 =< 0'z ->		% plain identifier
	read_identifier(C1, Dict, Tokens)
    ;	C1 >= 0'A, C1 =< 0'Z ->		% variable name
	read_variable(C1, Dict, Tokens)
    ;	C1 >= 0'0, C1 =< 0'9 ->
	read_number(C1, Dict, Tokens)
    ;   C1 < 127 ->			% special character
	read_special(C1, Dict, Tokens)
    ;   C1 =< 160 ->			% DEL or unassigned control
	get(C2),
	read_tokens(C2, Dict, Tokens)
    ;   C1 >= 223, C1 =\= 247 ->	% ISO lower case letter
	read_identifier(C1, Dict, Tokens)
    ;	C1 >= 192, C1 =\= 215 ->	% ISO upper case letter
	read_variable(C1, Dict, Tokens)
    ;	C1 =\= 170, C1 =\= 186 ->	% ISO symbol char
	read_symbol(C1, Dict, Tokens)
    ;   				% _a_ or _o_ ordinal characters
	read_identifier(C1, Dict, Tokens)
    ).

read_special(0'_, Dict, Tokens) :-	% underscore; starts variables
	read_variable(0'_, Dict, Tokens).
read_special(247, Dict, Tokens) :-	% -:- (division sign)
	read_symbol(247, Dict, Tokens).
read_special(215, Dict, Tokens) :-	% x (multiplication sign)
	read_symbol(215, Dict, Tokens).

	
	
read_special(0'%, Dict, [comment(StartPos, EndPos)|Tokens]) :-		%  %comment
%read_special(0'%, Dict, Tokens) :-		%  %comment
	get_current_position(StartPos),
	
	repeat,					%  skip characters to any
	    get0(Ch),				%  line terminator
	    Ch < 0' , Ch =\= 9 /*TAB*/,		%  control char, not tab
	!,					%  stop when we find one
	Ch \== -1,				%  fail on EOF
%	comment(Comm),
	get_current_position(EndPos),
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0'/, Dict, T) :-		%  /*comment?
	get0(C2),
	(   C2 =:= 0'* ->			% is /*comment*/
	    T = [comment(StartPos, EndPos)|Tokens],
	    get_current_position(StartPos1),
	    StartPos is StartPos1 -1,	   
	    read_solidus(0' , NextCh),
	    get_current_position(EndPos),
	    
	    
	    read_tokens(NextCh, Dict, Tokens)
	;/* C2 =\= 0'* */			% begins symbol
	    T = Tokens,	    
	    rest_symbol(C2, Chars, NextCh),
	    read_after_atom(NextCh, Dict, Tokens, [0'/|Chars])
	).
read_special(0'!, Dict, [atom(!,Pos)|Tokens]) :-	%  This is a special case so
	get_current_position(Pos),
	get0(NextCh),				%  that "!." is two tokens
	read_after_atom(NextCh, Dict, Tokens).	%  It could be cleverer.
read_special(0'(, Dict, [' ('|Tokens]) :-	%  NB!!!  "(" turns into
	get0(NextCh),				%  the token ' ('.
	read_tokens(NextCh, Dict, Tokens).
read_special(0'), Dict, [')'|Tokens]) :-
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0',, Dict, [','|Tokens]) :-
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0';, Dict, [atom(;,Pos)|Tokens]) :-	%   ; is not a punctuation
	get_current_position(Pos),
	get0(NextCh),				%   mark but an atom (e.g.
	read_after_atom(NextCh, Dict, Tokens).	%   you can :-op declare it).
read_special(0'[, Dict, ['['/Pos|Tokens]) :-
	get_current_position(Pos),
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0'], Dict, [']'/Pos|Tokens]) :-
	get_current_position(Pos),

	get0(NextCh),
	read_after_atom(NextCh, Dict, Tokens).
read_special(0'{, Dict, ['{'/Pos|Tokens]) :-
	get_current_position(Pos),
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0'|, Dict, ['|'|Tokens]) :-
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0'}, Dict, ['}'|Tokens]) :-
	get0(NextCh),
	read_after_atom(NextCh, Dict, Tokens).
read_special(0'., Dict, Tokens) :-		%  full stop
	get0(NextCh),				%  or possibly .=. &c
	read_fullstop(NextCh, Dict, Tokens).
read_special(0'", Dict, [string(Chars)|Tokens]) :-	%  "string"
	read_string(Chars, 0'", NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_special(0'', Dict, Tokens) :-		%  'atom'
	read_string(Chars, 0'', NextCh),
	read_after_atom(NextCh, Dict, Tokens, Chars).
read_special(0'#, Dict, Tokens) :-
	read_symbol(0'#, Dict, Tokens).
read_special(0'$, Dict, Tokens) :-
	read_symbol(0'$, Dict, Tokens).
read_special(0'&, Dict, Tokens) :-
	read_symbol(0'&, Dict, Tokens).
read_special(0'*, Dict, Tokens) :-
	read_symbol(0'*, Dict, Tokens).
read_special(0'+, Dict, Tokens) :-
	read_symbol(0'+, Dict, Tokens).
read_special(0'-, Dict, Tokens) :-
	read_symbol(0'-, Dict, Tokens).
read_special(0':, Dict, Tokens) :-
	read_symbol(0':, Dict, Tokens).
read_special(0'<, Dict, Tokens) :-
	read_symbol(0'<, Dict, Tokens).
read_special(0'=, Dict, Tokens) :-
	read_symbol(0'=, Dict, Tokens).
read_special(0'>, Dict, Tokens) :-
	read_symbol(0'>, Dict, Tokens).
read_special(0'?, Dict, Tokens) :-
	read_symbol(0'?, Dict, Tokens).
read_special(0'@, Dict, Tokens) :-
	read_symbol(0'@, Dict, Tokens).
read_special( 92, Dict, Tokens) :-		% 92 is "\\"
	read_symbol( 92, Dict, Tokens).
read_special(0'^, Dict, Tokens) :-
	read_symbol(0'^, Dict, Tokens).
read_special(0'`, Dict, Tokens) :-
	read_symbol(0'`, Dict, Tokens).
read_special(0'~, Dict, Tokens) :-
	read_symbol(0'~, Dict, Tokens).


%   read_symbol(+C1, +Dict, -Tokens)
%   C1 is the first character of an atom made up of the following characters:
%	#$&*+-./:<=>?\^'~ (which are ASCII codes) and the ISO 8859/1 codes
%	215 (x) 247 (-:-) 161-169, 171-185, 187-191

read_symbol(C1, Dict, Tokens) :-
	get0(C2),
	rest_symbol(C2, Chars, NextCh),	% might read 0 chars
	read_after_atom(NextCh, Dict, Tokens, [C1|Chars]).


%   rest_symbol(+C2, -String, -NextCh)
%   reads the second and subsequence characters of an atom made up of
%   "symbol" characters.  It returns those characters as the list
%   String, and the following character as NextCh.  Note that it need
%   not read any characters at all, e.g. Ch might be " ".

rest_symbol(C2, [C2|Chars], LastCh) :-
	(   C2 > 160 -> C2 < 192, C2 =\= 186, C2 =\= 170
	;   symbol_char(C2)
	),
	!,
	get0(NextCh),
	rest_symbol(NextCh, Chars, LastCh).
rest_symbol(C2, [], C2).

symbol_char(0'#).
symbol_char(0'$).
symbol_char(0'&).
symbol_char(0'*).
symbol_char(0'+).
symbol_char(0'-).
symbol_char(0'.).	% yes, +./* is a legal atom
symbol_char(0'/).
symbol_char(0':).
symbol_char(0'<).
symbol_char(0'=).
symbol_char(0'>).
symbol_char(0'?).
symbol_char(0'@).
symbol_char(92 /* \ */).
symbol_char(0'^).
symbol_char(0'`).	% CHAT-80 uses `` as an atom.
symbol_char(0'~).


get_current_position(Pos) :-
	current_input(S), character_count(S,Pos).

read_after_atom(Ch, Dict, [atom(Atom,Pos)|Tokens], Chars) :-
	current_input(S), character_count(S,X), length(Chars,L),Pos is X - L,
	flush_output,
        atom_codes(Atom, Chars),
	read_after_atom(Ch, Dict, Tokens).

%   The only difference between read_after_atom(Ch, Dict, Tokens) and
%   read_tokens/3 is what they do when Ch is "(".  read_after_atom
%   finds the token to be '(', while read_tokens finds the token to be
%   ' ('.  This is how the parser can tell whether <atom> <paren> must
%   be an operator application or an ordinary function symbol application.
%   See the public-domain library file READ.PL for details.

read_after_atom(0'(, Dict, ['('|Tokens]) :- !,
	get0(NextCh),
	read_tokens(NextCh, Dict, Tokens).
read_after_atom(Ch, Dict, Tokens) :-
	read_tokens(Ch, Dict, Tokens).




%   read_string(Chars, Quote, NextCh)
%   reads the body of a string delimited by Quote characters.
%   The result is a list of ASCII codes.  There are two complications.
%   If we hit the end of the file inside the string this predicate FAILS.
%   It does not return any special structure.  That is the only reason
%   it can ever fail.  The other complication is that when we find a Quote
%   we have to look ahead one character in case it is doubled.  Note that
%   if we find an end-of-file after the quote we *don't* fail, we return
%   a normal string and the end of file character is returned as NextCh.
%   If we were going to accept C-like escape characters, as I think we
%   should, this would need changing (as would the code for 0'x).  But
%   the purpose of this module is not to present my ideal syntax but to
%   present something which will read present-day Prolog programs.

read_string(Chars, Quote, NextCh) :-
	get0(Ch),
	read_char(Ch, Quote, Char, Next),
	rest_string(Char, Next, Chars, Quote, NextCh).


rest_string(-1, NextCh, [], _, NextCh) :- !.		% string ended
rest_string(Char, Next, [Char|Chars], Quote, NextCh) :-
	read_char(Next, Quote, Char2, Next2),
	rest_string(Char2, Next2, Chars, Quote, NextCh).


%   read_char(C1, Quote, Char, C2)
%   reads a single `character' from a string, quoted atom, or character
%   constant.  C1 is the first character it is to look at, and has been
%   read already.  Quote is the surrounding quotation mark, which is "
%   for strings, ' for quoted atoms, and the radix character (also ')
%   for character constants.  A Dec-10 Prolog incompatibility is that
%   it does not allow newline characters in strings unless they are
%   preceded by an escape character.  As reading an extended character
%   would sometimes read one character too many, it is made to do so
%   always, and to return the first character which does not belong in
%   the character as C2.  When we have hit the end of the string, we
%   return Char = -1 (which does not necessarily mean that we have hit
%   the end of the source file, look at C2 for that).

read_char(Char, Quote, Result, Next) :-
    (	Char =:= 92 /* \ */ ->
	get0(C1),
	(   C1 < 0 ->
		format(user_error, '~N** end of file in ~cquoted~c~n',
		       [Quote,Quote]),
		Result = -1, Next = C1
	;   C1 =< 0'  ->
		/* \<layout> is skipped */
		get0(C2),
		read_char(C2, Quote, Result, Next)
	;   C1\/32 =:= 0'c ->
		/* \c<layout>* is skipped; to get a blank after this */
		/* do e.g. "...\c      \ <space>" where the "\ " ends */
		/* the skipping and the NEXT blank is taken.  */
		get(C2),
		read_char(C2, Quote, Result, Next)
	;   C1 =< 0'7, C1 >= 0'0 ->
		/* \<1-3 octal digits> */
		/* hairy bit: \1234 is S4 */
		get0(C2),
		(   C2 =< 0'7, C2 >= 0'0 ->
		    get0(C3),
		    (   C3 =< 0'7, C3 >= 0'0 ->
			get0(Next),
			Result is (C1*8+C2)*8+C3 - 73*0'0
		    ;   Next = C3,
			Result is (C1*8+C2) - 9*0'0
		    )
		;   Next = C2,
		    Result is C1-0'0
		)
	;   C1 =:= 0'^ ->
		get0(C2),
		(   C2 < 0 ->
		    format(user_error, '~N** end of file in ~c..~c^..~c~n',
		   	   [Quote,92 /* \ */,Quote]),
		    Result = -1, Next = C2
		;   C2 =:= 0'? ->
		    Result = 127,	% \^? = DEL
		    get0(Next)
		;   Result is C2/\31,	% \^X -> control-X
		    get0(Next)
		)
	;   escape_char(C1, Result) ->
		get0(Next)
	;   /* otherwise */
		Result = C1,		% probably "'", '"',  or \ itself
		get0(Next)
	)

    ;	Char =:= Quote ->
	get0(Ch),
	(   Ch =:= Quote ->
	    Result = Quote,
	    get0(Next)
	;   Result = -1, Next = Ch
	)

    ;	Char < 0' , Char =\= 9 /*TAB*/, Char =\= 10 /* new line */ ->
	Result = -1, Next = Char,
	format(user_error,
	    '~N** Strange character ~d ends ~ctoken~c~n',
	    [Char, Quote, Quote])

    ;
	Result = Char,
	get0(Next)
    ).

/*  For LPA Prolog we would use
    (   Char =:= 126  / * ~ * /  ->
	get0(C1),
	(   C1 =< 0'_, C1 >= 0'@ -> Result is C1  / \ 31
	;   C1 =:= 0'? -> Result is 127
	;   C1 > 0'_, C1 =\= Char -> Result is C1  / \ 31
	;   Result is C1
	)
    ;   Char =:= Quote -> Result is -1
    ;	Result is Char
    ),
    get0(Next).
*/


%  This table is for ASCII.  On Xerox Lisp systems, \n maps to
%  13 (CR).  The whole table needs replacing in EBCDIC systems,
%  in which the assumption that A..Z and a..z are contiguous
%  blocks also needs correcting.

escape_char(0'n, 10).		% \n = NewLine
escape_char(0'N, 10).		% \N = NewLine
escape_char(0't,  9).		% \t = Tab
escape_char(0'T,  9).		% \T = Tab
escape_char(0'r, 13).		% \r = Return
escape_char(0'R, 13).		% \R = Return
escape_char(0'v, 11).		% \v = Vertical tab
escape_char(0'V, 11).		% \V = Vertical tab
escape_char(0'b,  8).		% \b = Backspace
escape_char(0'B,  8).		% \B = Backspace
escape_char(0'f, 12).		% \f = FormFeed
escape_char(0'F, 12).		% \F = FormFeed
escape_char(0'e, 27).		% \e = Escape
escape_char(0'E, 27).		% \E = Escape
escape_char(0'd,127).		% \d = Delete
escape_char(0'D,127).		% \D = Delete
escape_char(0's, 32).		% \s = visible Space
escape_char(0'S, 32).		% \S = visible Space
escape_char(0'z, -1).		% \z = end of file
escape_char(0'Z, -1).		% \Z = end of file



%   read_variable(+C1, +Dict, -Tokens)
%   C1 is the first character of a variable name.  If the whole
%   variable name is "_", this is an anonymous variable, not identical
%   to any other variable.  Otherwise, the variable and its name are
%   looked up in (or added to) the dictionary, which is an improper list.
%   This is the only place that read_lookup/2 is called.

%%steve -- added pos info to var
read_variable(C1, Dict, [var(Var,Name,StartPos)|Tokens]) :-
	get_current_position(StartPos),
	read_name(C1, Chars, NextCh),

	atom_codes(Name, Chars),
	(   Name == '_' -> true
	;   read_lookup(Dict, Name, Var)
	),
	read_after_atom(NextCh, Dict, Tokens).

read_lookup([N=V|L], Name, Var) :-
	(   N = Name -> V = Var
	;   read_lookup(L, Name, Var)
	).


%   read_solidus(+Ch, -LastCh)
%   is called when we have read the "/" and "*" that open a PL/I-style
%   comment.  It skips the rest of the comment.  We have to take great
%   care to handle end of file inside a comment; if the end-of-file is
%   is reported, we return -1 as LastCh,  while a space is returned if
%   the "*" and "/" that terminate the comment are found, and the next
%   character is left unread.  That might be changed.

read_solidus(Ch, LastCh) :-
    (	Ch =:= 0'* ->		% maybe end of comment
	get0(NextCh),
	(   NextCh =:= 0'/ ->	% end of comment*/ found
	    get(LastCh)		% skip over any layout following
	;   read_solidus(NextCh, LastCh)
	)
    ;	Ch =\= -1 ->		% ordinary comment character
	get0(NextCh),
	read_solidus(NextCh, LastCh)
    ;				% end of file
	LastCh = Ch,
	format(user_error, '~N** end of file in /*comment~n', [])
    ).


%   read_identifier(+C1, +Dict, -Tokens)
%   reads an atom which begins with a lower case letter C1 and
%   continues with letters, digits, and underscores.

read_identifier(C1, Dict, Tokens) :-
	read_name(C1, Chars, NextCh),
	read_after_atom(NextCh, Dict, Tokens, Chars).


%   read_name(+C1, -Chars, -LastCh)
%   reads a sequence of letters, digits, and underscores, where the
%   last character read was C1 and it is known that C1 is to be
%   included in the result.  The desired characters are returned as
%   the list Chars, and the next character as LastCh.
%   This version has been tuned, oy, has it been tuned!
%   A table-driven version is nearly as fast in Prolog.

read_name(C1, [C1|Chars], LastCh) :-
    get0(C2),
    (   C2 >= 0'a ->
	(	C2 =< 0'z ->			% ASCII lower case letter
	    read_name(C2, Chars, LastCh)
	;   C2 < 192, C2 \/ 16 =\= 186 ->	% {|}~, ISO 8859/1 symbols
	    Chars = [], LastCh = C2
	;   C2 \/ 32 =:= 247 ->		% times or divide-by chars
	    Chars = [], LastCh = C2
	;					% ISO 8859/1 top letters
	    read_name(C2, Chars, LastCh)
	)
    ;   C2 >= 0'A ->
	(	C2 > 0'Z, C2 =\= 0'_ ->		% [\]^`
	    Chars = [], LastCh = C2
	;					% ASCII upper case or "_"
	    read_name(C2, Chars, LastCh)
      )
    ;   (	C2 >= 0'0, C2 =< 0'9 ->		% ASCII digits
	    read_name(C2, Chars, LastCh)
	;					% other characters
	    Chars = [], LastCh = C2
	)
    ).


%   read_fullstop(Char, Dict, Tokens)
%   looks at the next character after a full stop.  There are
%   three cases:
%	(a) the next character is an end of file.  We treat this
%	    as an unexpected end of file.  The reason for this is
%	    that we HAVE to handle end of file characters in this
%	    module or they are gone forever; if we failed to check
%	    for end of file here and just accepted .<EOF> like .<NL>
%	    the caller would have no way of detecting an end of file
%	    and the next call would abort.
%	(b) the next character is a layout character.  This is a
%	    clause terminator.
%	(c) the next character is anything else.  This is just an
%	    ordinary symbol and we call read_symbol to process it.

read_fullstop(Ch, Dict, Tokens) :-
	(   Ch =< 0'9, Ch >= 0'0 ->
	    Tokens = [number(Number)|Tokens1],
	    read_float(Number, Dict, Tokens1, 0'0, Ch)
	;   Ch > 0'  ->		% ordinary token starting with "."
	    rest_symbol(Ch, Chars, NextCh),
	    read_after_atom(NextCh, Dict, Tokens, [0'.|Chars])
	;   Ch >= 0 ->		% END OF CLAUSE
	    Tokens = []
	;			% END OF FILE
	    format(user_error, '~N** end of file just after full stop~n', []),
	    fail
	).



%   read_float(N, C, Dict, Tokens)
%   is called when we have parsed <digit>* "." <digit>; N is the integer
%   value of the characters preceding the decimal point, and C is the
%   first digit after the decimal point.

read_float(Number, Dict, Tokens, Digits, Digit) :-
	prepend(Digits, Chars, Rest),
	read_float(Digit, Rest, NextCh, Chars),
	number_chars(Number, Chars),
	read_tokens(NextCh, Dict, Tokens).

prepend([]) --> ".".
prepend([C|Cs]) --> [C], prepend(Cs).

read_float(C1, [C1|Chars], NextCh, Total) :-
	get0(C2),
	(   C2 >= 0'0, C2 =< 0'9 ->
	    read_float(C2, Chars, NextCh, Total)
	;   C2\/32 =:= 0'e ->
	    get0(C3),
	    (   C3 =:= 0'- -> get0(C4), Chars = [C2,0'-|More]
	    ;   C3 =:= 0'+ -> get0(C4), Chars = [C2|More]
	    ;		      C4 = C3,  Chars = [C2|More]
	    ),
	    (   C4 >= 0'0, C4 =< 0'9 ->
		read_exponent(C4, More, NextCh)
	    ;   More = "",
		format(user_error, '~N** Missing exponent in ~s~n', [Total]),
		fail
	    ;   More = 0'0, NextCh = C4
	    )
	;   Chars = [], NextCh = C2
	).

read_exponent(C1, [C1|Chars], NextCh) :-
	get0(C2),
	(   C2 >= 0'0, C2 =< 0'9 ->
	    read_exponent(C2, Chars, NextCh)
	;   Chars = [], NextCh = C2
	).


%   read_number(+C1, +Dict, -Tokens)
%   C1 is the digit which begins the number.

read_number(C1, Dict, [number(Number)|Tokens]) :-
	read_number(C1, C2, 0, N),
	(   C2 =:= 0'' ->
	    (   N >= 2, N =< 36 ->
		read_based(N, 0, Number, C)
	    ;   N =:= 0 ->
		get0(C3),
		read_char(C3, -1, Number, C)
	    ;   format(user_error, '~N** ~d'' read as ~d ''~n', [N,N]),
		Number = N, C = C2
	    ),
	    read_tokens(C, Dict, Tokens)
	;   C2 =:= 0'. ->
	    get0(C3),
	    (   C3 >= 0'0, C3 =< 0'9 ->
		number_chars(N, Digits),
		read_float(Number, Dict, Tokens, Digits, C3)
	    ;   Number = N,
		read_fullstop(C3, Dict, Tokens)
	    )
	;   Number = N,
	    read_tokens(C2, Dict, Tokens)
	).


%   read_number(+C0, -C, +N0, -N)
%   read a decimal integer.

read_number(C0, C, N0, N) :-
    (	C0 >= 0'0, C0 =< 0'9 ->
	N1 is N0*10 - 0'0 + C0,
	get0(C1),
	read_number(C1, C, N1, N)
    ;   C0 =:= 0'_ ->
	get0(C1),
	read_number(C1, C, N0, N)
    ;   C = C0, N = N0
    ).


%   read_based(+Base, +N0, -N, -LastCh)
%   read an integer in base Base.

read_based(Base, N0, N, C) :-
	get0(C1),
	(   C1 >= 0'0, C1 =< 0'9 -> Digit is C1-0'0
	;   C1 >= 0'A, C1 =< 0'Z -> Digit is C1-(0'A-10)
	;   C1 >= 0'a, C1 =< 0'z -> Digit is C1-(0'a-10)
	;   Digit is 99
	),
	(   Digit < Base ->
	    N1 is N0*Base + Digit,
	    read_based(Base, N1, N, C)
	;   C1 =:= 0'_ ->
	    read_based(Base, N0, N, C)
	;   N = N0, C = C1
	).
