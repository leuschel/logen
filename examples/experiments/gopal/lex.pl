:- module(lex,[lex/2]).

%Tokenizer; Author: Qian Wang
%ALPS Lab, University of Texas at Dallas
%

:- use_module(library(lists)).

lex(FileName,Tokenlist):-
    print_message(informational,lexing(FileName)),
    seeing(Oldin), 
    see(FileName),
    (get_tokens(Tokenlist);
     write('meet error when tokenize file')),!,
    seen,
    see(Oldin),
    print_message(informational,done_lexing(FileName)).
    
%read byte from input stream and return a list of token
get_tokens(Tokenlist) :- get0(Firstchar),
                         tokenize(Firstchar,Tokenlist).
                         
%deal with first char is char of word
tokenize(Char,[Token|Tokens]) :-
                         word_char(Char),
                         read_word(Char,Token,Nextchar),
                         tokenize(Nextchar,Tokens).
                         
%deal with first char is char of special char
tokenize(Char,[Token|Tokens]) :-
                         special_char(Char),
                         name(Token,[Char]),
                         get0(Nextchar),
                         tokenize(Nextchar,Tokens).
%ignore all fill char 
tokenize(Char,Tokenlist) :-
                         fill_char(Char),
                         get0(Nextchar),
                         tokenize(Nextchar,Tokenlist).
                         
%meet END of file and stop tokenize
tokenize(Char,[]) :- eof_char(Char).

%read byte from input stream and return a word and next read byte
%name predicate function convert a list of byte into a word
read_word(Char,Word,Nextchar) :-
                         word_chars(Char,Chars,Nextchar),
                         name(Word,Chars).

word_chars(Char,[],Char) :- \+ word_char(Char).

word_chars(Char,[Char|Chars],Finalchar) :-
                         word_char(Char), !,
                         get0(Nextchar),
                         word_chars(Nextchar,Chars,Finalchar).



word_char(X) :- X >= 65, X =< 90.       % Upper case alpha
word_char(X) :- X >= 97, X =< 122.      % Lower case alpha
word_char(X) :- X >= 48, X =< 57.       % Digits 0 to 9
word_char(X) :- X =:= 95.               % Underscore

special_char(X) :- member(X, [33,34,38,39,40,41,42,43,44,45,46,47,
                   58,59,60,61,62,64,91,93,95,123,124,125]).

fill_char(X) :- X =:= 32.               % Space
fill_char(X) :- X =:= 13.               % Newline
fill_char(X) :- X =:= 10.               % Newline
fill_char(X) :- X =:= 9.

eof_char(X) :- X =:= -1.                % EOF value

%parse for command line
get_tokens([],[]).
get_tokens(Tokenlist,CommandLine) :- get_char(Firstchar,CommandLine,Left),
                         tokenize(Firstchar,Tokenlist,Left,[]).
                         
%deal with first char is char of word
tokenize(Char,[Token|Tokens],InputList,LeftList) :-
                         word_char(Char),
                         read_word(Char,Token,Nextchar,InputList,Left1),
                         tokenize(Nextchar,Tokens,Left1,LeftList).

%ignore all fill char 
tokenize(Char,Tokenlist,InputList,LeftList) :-
                         fill_char(Char),!,
                         get_char(Nextchar,InputList,Left1),
                         tokenize(Nextchar,Tokenlist,Left1,LeftList).
                         
%deal with first char is char of special char
tokenize(Char,[Token|Tokens],InputList,LeftList) :-
                         special_char(Char),!,
                         name(Token,[Char]),
                         get_char(Nextchar,InputList,Left1),
                         tokenize(Nextchar,Tokens,Left1,LeftList).
                         
%meet END of file and stop tokenize
tokenize(Char,[Char],[],[]):- \+ end_command_char(Char).
tokenize(Char,[],[],[]):- end_command_char(Char).
tokenize(Char,[],_,[]) :- end_command_char(Char).

%read byte from input stream and return a word and next read byte
%name predicate function convert a list of byte into a word
read_word(Char,Word,Nextchar,InputList,LeftList) :-
                         word_chars(Char,Chars,Nextchar,InputList,LeftList),
                         name(Word,Chars).

word_chars(Char,[],Char,InputList,InputList) :- \+ word_char(Char).
%word_chars(Char,[Char],Char,[],[]).
word_chars(Char,[Char|Chars],Finalchar,InputList,LeftList) :-
                         word_char(Char), !,
                         get_char(Nextchar,InputList,Left1),
                         word_chars(Nextchar,Chars,Finalchar,Left1,LeftList).

get_char(H,[H|T],T).
skip_char(X) :- X =:= 32.               % Space
%skip_char(X) :- X =:= 44.               % ,
skip_char(X) :- X =:=  9.               % \t

end_command_char(X):-X=:=10.             % end of command
end_command_char(X):-X=:=13.             % end of command
%end_command_char(X):-X=:=46.             % end of command '.'
