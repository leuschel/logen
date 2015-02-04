

read_tokens(TokenList, Dictionary) :-
        read_tokens(32, Dict, TokenList),
        append(Dict, [], Dict), 
        Dictionary = Dict.
read_tokens([atom(end_of_file)], []).        


my_get0(1).
my_get0(2).

read_tokens(-1, _, _) :-                
        2 = 3.                                        
read_tokens(Ch, Dict, Tokens) :-
        Ch =< 32,                                
        my_get0(NextCh),                                
        read_tokens(NextCh, Dict, Tokens).
read_tokens(37, Dict, Tokens) :-                 
        my_get0(Ch),                                
        Ch \== 26,                                
        my_get0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(47, Dict, Tokens) :-                 
        my_get0(NextCh),
        read_solidus(NextCh, Dict, Tokens).
read_tokens(33, Dict, [atom('!')|Tokens]) :-         
        my_get0(NextCh),                                
        read_after_atom(NextCh, Dict, Tokens).        
read_tokens(40, Dict, [' ('|Tokens]) :-         
        my_get0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(41, Dict, [')'|Tokens]) :- 
        my_get0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(44, Dict, [','|Tokens]) :- 
        my_get0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(59, Dict, [atom(';')|Tokens]) :-         
        my_get0(NextCh),                                
        read_tokens(NextCh, Dict, Tokens).        
read_tokens(91, Dict, ['['|Tokens]) :- 
        my_get0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(93, Dict, [']'|Tokens]) :- 
        my_get0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(123, Dict, ['{'|Tokens]) :- 
        my_get0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(124, Dict, ['|'|Tokens]) :- 
        my_get0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(125, Dict, ['}'|Tokens]) :- 
        my_get0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(46, Dict, Tokens) :-                 
        my_get0(NextCh),                                
        read_fullstop(NextCh, Dict, Tokens).
read_tokens(34, Dict, [string(S)|Tokens]) :-         
        read_string(S, 34, NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(39, Dict, [atom(A)|Tokens]) :-         
        read_string(S, 39, NextCh),
        getname(A),                                
        read_after_atom(NextCh, Dict, Tokens).
read_tokens(Ch, Dict, [var(Var,Name)|Tokens]) :-
        Ch >= 65, Ch =< 90,
        read_name(Ch, S, NextCh),
        getname(Name),
        read_lookup(Dict, Name=Var),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(Ch, Dict, [integer(I)|Tokens]) :-
        Ch >= 48, Ch =< 57,        
        read_integer(Ch, I, NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_tokens(Ch, Dict, [atom(A)|Tokens]) :-
        Ch >= 97, Ch =< 122,                        
        read_name(Ch, S, NextCh),
        getname(A),
        read_after_atom(NextCh, Dict, Tokens).
read_tokens(Ch, Dict, [atom(A)|Tokens]) :-        
        my_get0(AnotherCh),
        read_symbol(AnotherCh, Chars, NextCh),        
        getname(A),
        read_after_atom(NextCh, Dict, Tokens).

getname('toto').
getname('titi').

read_after_atom(40, Dict, ['('|Tokens]) :- 
        my_get0(NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_after_atom(Ch, Dict, Tokens) :-
        read_tokens(Ch, Dict, Tokens).

read_string(Chars, Quote, NextCh) :-
        my_get0(Ch),
        read_string(Ch, Chars, Quote, NextCh).
read_string(26, _, Quote, 26) :-
        my_display('! end of file in '), my_ttyput(Quote),
        my_display(token), my_ttyput(Quote), my_ttynl,
         my_fail.
read_string(Quote, Chars, Quote, NextCh) :- 
        my_get0(Ch),                                
        more_string(Ch, Quote, Chars, NextCh).
read_string(Char, [Char|Chars], Quote, NextCh) :-
        read_string(Chars, Quote, NextCh).        

more_string(Quote, Quote, [Quote|Chars], NextCh) :- 
        read_string(Chars, Quote, NextCh).        
more_string(NextCh, _, [], NextCh).                

my_display(X).
my_ttyput(X).
my_ttynl.
my_fail :-
  3 = 4.

read_solidus(42, Dict, Tokens) :- 
        my_get0(Ch),
        read_solidus(Ch, NextCh),
        read_tokens(NextCh, Dict, Tokens).
read_solidus(Ch, Dict, [atom(A)|Tokens]) :-
        read_symbol(Ch, Chars, NextCh),                
        getname(A),
        read_tokens(NextCh, Dict, Tokens).

read_solidus(26, 26) :- 
        my_display('! end of file in /*comment'), my_ttynl.
read_solidus(42, LastCh) :-
        my_get0(NextCh),
        NextCh \== 47,         
        read_solidus(NextCh, LastCh).
read_solidus(42, 32).
read_solidus(_, LastCh) :-
        my_get0(NextCh),
        read_solidus(NextCh, LastCh).

read_name(Char, [Char|Chars], LastCh) :-
        Char >= 97, Char =< 122,        
        my_get0(NextCh),
        read_name(NextCh, Chars, LastCh).
read_name(LastCh, [], LastCh).


read_symbol(Char, [Char|Chars], LastCh) :-
        check_special(Char), 
        my_get0(NextCh),
        read_symbol(NextCh, Chars, LastCh).
read_symbol(LastCh, [], LastCh).

check_special('#').
check_special('$').
check_special('&').
check_special('*').
check_special('+').
check_special('-').
check_special('.').
check_special('/').
check_special(':').
check_special('<').
check_special('=').
check_special('>').
check_special('?').
check_special('@').
%check_special('\').
check_special('^').
check_special('`').
check_special('~').

read_fullstop(26, _, _) :- 
        my_display('! end of file just after full stop'), my_ttynl,
        my_fail.
read_fullstop(Ch, _, []) :-
        Ch =< 32.
read_fullstop(Ch, Dict, [atom(A)|Tokens]) :-
        read_symbol(Ch, S, NextCh),
        getname(A),
        read_tokens(NextCh, Dict, Tokens).

read_integer(BaseChar, IntVal, NextCh) :-
        Base is BaseChar - 48,
        my_get0(Ch),
        Ch \== 26,
         Ch \== 39, read_digits(Ch, Base, 10, IntVal, NextCh).

read_digits(SoFar, Base, Value, NextCh) :-
        my_get0(Ch),
        Ch \== 26,
        read_digits(Ch, SoFar, Base, Value, NextCh).

read_digits(Digit, SoFar, Base, Value, NextCh) :-
        Digit >= 48, Digit =< 57,
        Next is SoFar*Base-48+Digit,
        read_digits(Next, Base, Value, NextCh).
read_digits(LastCh, Value, _, Value, LastCh).

read_lookup([X|_], X).
read_lookup([_|T], X) :-
        read_lookup(T, X). 

append([],L,L).
append([H|L1],L2,[H|L3]) :- append(L1,L2,L3).


read2(Answer, Variables) :-
    read_tokens([Tokens|Ts],Variables),
    read([Tokens|Ts], 1200, Term, LeftOver), 
    all_read(LeftOver),
    Answer = Term.

all_read([]).
all_read([X|Y]) :-
   syntax_error([operator,expected,after,expression], [X|Y]).

expect(Token, [Token|Rest], Rest).
expect(Token, S0, _) :-
   syntax_error([Token,or,operator,expected], S0).

prefixop(Op, Prec, Prec) :-
   my_current_op(Prec, fy, Op).
prefixop(Op, Prec, Less) :-
   my_current_op(Prec, fx, Op),
   Less is Prec-1.


postfixop(Op, Prec, Prec) :-
   my_current_op(Prec, yf, Op).
postfixop(Op, Less, Prec) :-
   my_current_op(Prec, xf, Op), Less is Prec-1.

infixop(Op, Less, Prec, Less) :-
   my_current_op(Prec, xfx, Op), Less is Prec-1.
infixop(Op, Less, Prec, Prec) :-
   my_current_op(Prec, xfy, Op), Less is Prec-1.
infixop(Op, Prec, Prec, Less) :-
   my_current_op(Prec, yfx, Op), Less is Prec-1.

my_current_op(1,xfx,toto).
my_current_op(2,xfx,titi).
my_current_op(1,xfy,toto).
my_current_op(2,xfy,titi).
my_current_op(1,yfx,toto).
my_current_op(2,yfx,titi).
my_current_op(1,fx,toto).
my_current_op(2,fx,titi).
my_current_op(1,yf,toto).
my_current_op(2,yf,titi).
my_current_op(1,xf,toto).
my_current_op(2,xf,titi).
my_current_op(1,fy,toto).
my_current_op(2,fy,titi).

ambigop(F, L1, O1, R1, L2, O2) :-
   postfixop(F, L2, O2),
   infixop(F, L1, O1, R1).

read([Token|RestTokens], Precedence, Term, LeftOver) :-
   read(Token, RestTokens, Precedence, Term, LeftOver).
read([], _, _, _) :-
   syntax_error([expression,expected], []).

read(var(Variable,_), ['('|S1], Precedence, Answer, S) :- 
   read(S1, 999, Arg1, S2),
   read_args(S2, RestArgs, S3),
   exprtl0(S3, apply(Variable,[Arg1|RestArgs]), Precedence, Answer, S).

read(var(Variable,_), S0, Precedence, Answer, S) :- 
   exprtl0(S0, Variable, Precedence, Answer, S).

read(atom('-'), [integer(Integer)|S1], Precedence, Answer, S) :-
   Negative is -Integer,
   exprtl0(S1, Negative, Precedence, Answer, S).

read(atom(Functor), S0, Precedence, Answer, S) :-
   prefixop(Functor, Prec, Right),
   after_prefix_op(Functor, Prec, Right, S0, Precedence, Answer, S).

read(atom(Atom), S0, Precedence, Answer, S) :-
   exprtl0(S0, Atom, Precedence, Answer, S).

read(integer(Integer), S0, Precedence, Answer, S) :- 
   exprtl0(S0, Integer, Precedence, Answer, S).

read('[', [']'|S1], Precedence, Answer, S) :- 
   exprtl0(S1, [], Precedence, Answer, S).

read('[', S1, Precedence, Answer, S) :- 
   read(S1, 999, Arg1, S2),
   read_list(S2, RestArgs, S3),
   exprtl0(S3, [Arg1|RestArgs], Precedence, Answer, S).

read('(', S1, Precedence, Answer, S) :- 
   read(S1, 1200, Term, S2),
   expect(')', S2, S3),
   exprtl0(S3, Term, Precedence, Answer, S).

read(' (', S1, Precedence, Answer, S) :- 
   read(S1, 1200, Term, S2),
   expect(')', S2, S3),
   exprtl0(S3, Term, Precedence, Answer, S).

read('{', ['}'|S1], Precedence, Answer, S) :- 
   exprtl0(S1, '{}', Precedence, Answer, S).

read('{', S1, Precedence, Answer, S) :- 
   read(S1, 1200, Term, S2),
   expect('}', S2, S3),
   exprtl0(S3, set(Term), Precedence, Answer, S).

read(string(List), S0, Precedence, Answer, S) :- 
   exprtl0(S0, List, Precedence, Answer, S).

read(Token, S0, _, _, _) :-
   syntax_error([Token,cannot,start,an,expression], S0).

read_args([','|S1], [Term|Rest], S) :- 
   read(S1, 999, Term, S2),
   read_args(S2, Rest, S).
read_args([')'|S], [], S).
read_args([X|Y], _, _) :-
   syntax_error([', or )',expected,in,arguments], S).


read_list([','|S1], [Term|Rest], S) :- 
   read(S1, 999, Term, S2),
   read_list(S2, Rest, S).
read_list(['|'|S1], Rest, S) :- 
   read(S1, 999, Rest, S2),
   expect(']', S2, S).
read_list([']'|S], [], S).
read_list(S, _, _) :-
   syntax_error([', | or ]',expected,in,list], S).

after_prefix_op(Op, Oprec, Aprec, S0, Precedence, _, _) :-
   Precedence < Oprec,
   syntax_error([prefix,operator,Op,in,context,
      with,precedence,Precedence], S0).

after_prefix_op(Op, Oprec, Aprec, S0, Precedence, Answer, S) :-
   peepop(S0, S1),
   prefix_is_atom(S1, Oprec), 
   exprtl(S1, Oprec, Op, Precedence, Answer, S).

peepop([atom(F),'('|S1], [atom(F),'('|S1]) .
peepop([atom(F)|S1], [infixop(F,L,P,R)|S1]) :- infixop(F, L, P, R).
peepop([atom(F)|S1], [postfixop(F,L,P)|S1]) :- postfixop(F, L, P).
peepop(S0, S0).

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


exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
   ambigop(F, L1, O1, R1, L2, O2),
   exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S).

exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
   ambigop(F, L1, O1, R1, L2, O2),
   exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S).

exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
   infixop(F, L1, O1, R1),
   exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
   postfixop(F, L2, O2),
   exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S).

exprtl0([','|S1], Term, Precedence, Answer, S) :-
   Precedence >= 1000,
   read(S1, 1000, Next, S2),
   exprtl(S2, 1000, c(Term,Next), Precedence, Answer, S).

exprtl0(['|'|S1], Term, Precedence, Answer, S) :-
   Precedence >= 1100,
   read(S1, 1100, Next, S2),
   exprtl(S2, 1100, sc(Term,Next), Precedence, Answer, S).

exprtl0([Thing|S1], _, _, _, _) :-
   cant_follow_expr(Thing, Culprit),
   syntax_error([Culprit,follows,expression], [Thing|S1]).

exprtl0(S, Term, _, Term, S).


cant_follow_expr(atom(_),   atom).
cant_follow_expr(var(_,_),   variable).
cant_follow_expr(integer(_),   integer).
cant_follow_expr(string(_),   string).
cant_follow_expr(' (',      bracket).
cant_follow_expr('(',      bracket).
cant_follow_expr('[',      bracket).
cant_follow_expr('{',      bracket).



exprtl([','|S1], C, Term, Precedence, Answer, S) :-
   Precedence >= 1000, C < 1000,
   read(S1, 1000, Next, S2),
   exprtl(S2, 1000, c(Term,Next), Precedence, Answer, S).

exprtl(['|'|S1], C, Term, Precedence, Answer, S) :-
   Precedence >= 1100, C < 1100,
   read(S1, 1100, Next, S2),
   exprtl(S2, 1100, sc(Term,Next), Precedence, Answer, S).

exprtl(S, _, Term, _, Term, S).

syntax_error(Message, List) :-
  my_fail.


