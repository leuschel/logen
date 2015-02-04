:- module('matlab_parser', [parse/2,
							print_mfile/1,
							print_function/1,
							first_parse/2,
							disambiguate/2]).

map(_,[],[]).
map(P,[H|T],[PH|PT]) :- Call =.. [P,H,PH], call(Call), map(P,T,PT).

append([], L, L).
append([H|T], L, [H|T1]) :- append(T, L, T1).

parse(Text, NewTree) :- first_parse(Text, Tree),
						map(disambiguate, Tree, NewTree), !.

first_parse(Text, Tree) :- tokenise(Text, _, Tokens), !,
                        mfile(Tree, Tokens, Empty), !,
						success(Empty).

success([]).
success(NotEmpty) :- write('Parse failure\nRemaining in buffer: '), write(NotEmpty), fail.

token(43, +).
token(45, -).
token(42, *).
token(47, /).
token(92, '\\').
token(94, ^).
token(39, '\'').
token(62, >).
token(60, <).
token(61, =).
token(126, ~).
token(38, &).
token(124, '\|').
token(64, @).
token(58, :).
token(59, ;).
token(91, '[').
token(93, ']').
token(40, '(').
token(41, ')').
token(10, '\n').
token(44, ',').

comment(37, '\%').
long_token(46, 42, '.*').
long_token(46, 47, './').
long_token(46, 92, '.\\').
long_token(46, 94, '.^').
long_token(46, 39, '.\'').
long_token(62, 61, '>=').
long_token(60, 61, '<=').
long_token(61, 61, '==').
long_token(126, 61, '~=').

point(46).

code('for', for).
code('while', while).
code('if', if).
code('else', else).
code('elseif', elseif).
code('end', end).
code('switch', switch).
code('case', case).
code('otherwise', otherwise).
code('break', break).
code('continue', continue).
code('return', return).
code('function', function).
code(X, var(X)).

tokenise([H|T], List, L) :- digit(H), !, get_rest_float(T, [H], Num, Rem),
					append(List, [const(Num)], NewList), !, 
					tokenise(Rem, NewList, L).
tokenise([H|T], List, L) :- letter(H), !, get_rest_id(T, [H], Id, Rem),
					code(Id, NI), append(List, [NI], NewList), !,
					tokenise(Rem, NewList, L).
%tokenise([H|T], List, L) :- comment(H), !, consume(T, NT),
%					!, tokenise(NT, List, L).
tokenise([H1|[H2|T]], List, L) :- long_token(H1, H2, Token),
					append(List, [Token], NewList), !, tokenise(T, NewList, L).
tokenise([H|T], List, L) :- token(H, Token), !, append(List, [Token], NewList),
					!, tokenise(T, NewList, L).
tokenise([10|T], List, L) :- append(List, ['\n'], NewList),
					!, tokenise(T, NewList, L).
tokenise([32|T], List, L) :- !, tokenise(T, List, L).
tokenise([], List, List).


get_rest_float([H|T], List, Num, X) :- digit(H), !, append(List, [H], Nlist),
					get_rest_float(T, Nlist, Num, X).
get_rest_float([32|T], List, Num, T) :- name(Num, List), !.
get_rest_float([H|T], List, Num, [H|T]) :- token(H,_), name(Num, List), !.
get_rest_float([H1|[H2|T]], List, Num, [H1|[H2|T]]) :- long_token(H1,H2,_), name(Num, List), !.
get_rest_float([H|T], List, Num, X) :- point(H), !, append(List, [H], Nlist),
					get_rest_num(T, Nlist, Num, X).
get_rest_float([], List, Num, []) :- !, name(Num, List).

get_rest_num([H|T], List, Num, X) :- digit(H), !, append(List, [H], Nlist),
					get_rest_num(T, Nlist, Num, X).
get_rest_num([32|T], List, Num, T) :- name(Num, List), !.
get_rest_num([H|T], List, Num, [H|T]) :- token(H,_), name(Num, List), !.
get_rest_num([H1|[H2|T]], List, Num, [H1|[H2|T]]) :- long_token(H1,H2,_), name(Num, List), !.
get_rest_num([], List, Num, []) :- !, name(Num, List).

get_rest_id([H|T], List, Id, X) :- identifier(H), !, append(List, [H], Nlist),
					get_rest_id(T, Nlist, Id, X).
get_rest_id([32|T], List, Id, T) :- name(Id, List), !.
get_rest_id([H|T], List, Id, [H|T]) :- token(H,_), name(Id, List), !.
get_rest_id([H1|[H2|T]], List, Id, [H1|[H2|T]]) :- long_token(H1,H2,_), name(Id, List), !.
get_rest_id([], List, Id, []) :- !, name(Id, List).

letter(C) :- C >= "A", C =< "Z".
letter(C) :- C >= "a", C =< "z".

digit(C) :- C >= "0", C =< "9".

identifier(C) :- letter(C).
identifier(C) :- digit(C).
identifier(95). % _

consume_delim --> [';'], consume_delim.
consume_delim --> ['\n'], consume_delim.
consume_delim --> [].

silence(exp(E, _), exp(E, true), true).
silence(exp(E, _), exp(E, false), false).
silence(assign(V, E, _), assign(V, E, false), false).
silence(assign(V, E, _), assign(V, E, true), true).
silence(A, A, _).

delim(C, NC, CL) --> [';'], consume_delim,
				{ silence(C, NC, true) }, commands(CL).
delim(C, NC, CL) --> ['\n'], consume_delim,
				{ silence(C, NC, false) }, commands(CL).
delim(C, NC, []) --> [], {silence(C, NC, false)}.

mfile([F|M]) --> function(F), mfile(M).
mfile([]) --> [].

function(F) --> [function], func(F).

func(function(Name, RV, P,C)) --> rv(RV), [var(Name)], parameters(P), consume_delim, commands(C).

rv(RV) -->  ['['], rvs(RV) ,[']'], [=].
rv([RV]) --> [var(RV)], [=].
rv([]) --> [].

rvs(RS) --> [var(R)], rest_rvs([R], RS).
rvs([]) --> [].

rest_rvs(R, RS) --> [','], [var(R1)], {append(R, [R1], R2)}, rest_rvs(R2, RS).
rest_rvs(R, RS) --> [var(R1)], {append(R, [R1], R2)}, rest_rvs(R2, RS).
rest_rvs(R, R) --> [].

parameters(P) --> ['('], pars(P), [')'].
parameters([]) --> [].
pars([H|T]) --> [var(H)], [','], pars(T).
pars([P]) --> [var(P)].

commands([NC|CL]) --> command(C), delim(C,NC, CL).
commands([]) --> [].

command(C) --> forloop(C).
command(C) --> whileloop(C).
command(C) --> ifstatement(C).
command(D) --> switchstatement(D).
command(D) --> expassign(D).
command(break) --> [break].
command(continue) --> [continue].
command(return) --> [return].

forloop(for(V, E, C)) --> [for], [V], [=], expr(E), consume_delim, commands(C), [end].
expassign(AE) --> expr(L), restexpassign(L, AE).
whileloop(while(E, C)) --> [while], expr(E), consume_delim, commands(C), [end].
switchstatement(switch(E, C, O)) --> [switch], expr(E), consume_delim, 
									cases(C,O).
ifstatement(if(E, C, C2)) --> [if], expr(E), consume_delim, commands(C), restif(C2).
restif([]) --> [end].
restif(C) --> [else], consume_delim, commands(C), [end].
restif([if(E, C, C2)]) --> [elseif], expr(E), consume_delim, commands(C), restif(C2).
cases([case(E, Comms)|Cs], O) --> [case], expr(E), consume_delim,
						  commands(Comms), cases(Cs, O).
cases([], O) --> [otherwise], consume_delim, commands(O), [end].
cases([], []) --> [end].

isvar(var(_)).

restexpassign(L, assign(L, R, _)) --> [=], !, { isvar(L) }, expr(R).
restexpassign(L, exp(L, _)) --> [].

semicolon(true) --> [;].
semicolon(false) --> ['\n'].
semicolon(false) --> [].

assign(assign(D, E, false)) --> [D], [=], expr(E).

expr(E) --> exp2(L), restexp(L, E).
restexp(L, bin_op('\|', L,R)) --> ['\|'], !, expr(R).
restexp(L, L) --> [].

exp2(E) --> exp3(L), restexp2(L, E).
restexp2(L, bin_op(&,L,R)) --> [&], !, exp2(R).
restexp2(L, L) --> [].

exp3(E) --> exp4(L), restexp3(L, E).
restexp3(L, bin_op(==,L,R)) --> ['=='], !, exp3(R).
restexp3(L, bin_op(~=,L,R)) --> ['~='], !, exp3(R).
restexp3(L, bin_op(<,L,R)) --> ['<'], !, exp3(R).
restexp3(L, bin_op(<=,L,R)) --> ['<='], !, exp3(R).
restexp3(L, bin_op(>=,L,R)) --> ['>='], !, exp3(R).
restexp3(L, bin_op(>,L,R)) --> ['>'], !, exp3(R).
restexp3(L, L) --> [].

exp4(E) --> exp5(L), restexp4(L, E).
%restexp4(colon(S, I), colon(S,I,E)) --> [:], !, exp4(E).
restexp4(L, C) --> [:], !, exp5(R), !, restcolon(L, R, C).
restexp4(L, L) --> [].

restcolon(S, I, colon(S, I, E)) --> [:], !, exp5(E).
restcolon(S, E, colon(S, E)) --> [].

exp5(E) --> exp6(L), restexp5(L, E).
restexp5(L, bin_op(+,L,R)) --> [+], !, exp5(R).
restexp5(L, bin_op(-,L,R)) --> [-], !, exp5(R).
restexp5(L, L) --> [].

exp6(T) --> exp7(L), restexp6(L,T).
restexp6(L, bin_op(*,L, R)) --> [*], !, exp6(R).
restexp6(L, bin_op(.*,L, R)) --> ['.*'], !, exp6(R).
restexp6(L, bin_op(./,L, R)) --> ['./'], !, exp6(R).
restexp6(L, bin_op(/,L, R)) --> [/], !, exp6(R).
restexp6(L, bin_op(\\,L, R)) --> [\\], !, exp6(R).
restexp6(L, bin_op(.\\,L, R)) --> ['.\\'], !, exp6(R).
restexp6(L, L) --> [].

exp7(N) --> [+], exp7(N).
exp7(un_op(-,N)) --> [-], exp7(N).
exp7(un_op('~',N)) --> [~], exp7(N).
exp7(N) --> exp8(N).

exp8(T) --> number(L), restexp8(L, T).
restexp8(L, bin_op(^,L, R)) --> [^], exp8(R).
restexp8(L, bin_op(.^,L, R)) --> [.^], exp8(R).
restexp8(L, un_op('\'',L)) --> ['\''].
restexp8(L, un_op('.\'',L)) --> ['.\''].
restexp8(L, L) --> [].

number(M) --> ['['], rest_matrix(M).
number(R) --> [var(C)] -> rest_var(var(C), R).
number(const(C)) --> [const(C)].

rest_matrix(matrix([])) --> [']'].
rest_matrix(matrix(R)) --> rows(R), [']'].

rows([H|T]) --> row(H), rest_rows(T).
rows([]) --> [].

rest_rows(T) --> [';'], rows(T).
rest_rows([]) --> [].

row(R) --> expr(E), comma_sep([E], R).

rest_var(V, subscript(V, R)) --> ['('], expr(E), comma_sep([E], R), [')'].
rest_var(V, V) --> [].

comma_sep(L, R) --> [','], expr(E), {append(L, [E], R1)}, comma_sep(R1, R).
comma_sep(L, L) --> [].

print_commands([], _).
print_commands([C|Cs], Indent) :- print_comm(C, Indent),
								  print_commands(Cs, Indent).
%print_commands([C|_], _) :- write('error printing :'), write(C), nl, !, fail.

indent(0) :- !.
indent(I) :- write(' '), I1 is I - 1, indent(I1).

print_mfile([]).
print_mfile([F|T]) :- print_function(F), nl, print_mfile(T).

print_function(function(Name, R, P, _, C)) :- write('function '), print_rv(R),
						write(Name), print_pars(P), nl, print_commands(C, 0).

print_rv([]).
print_rv([R]) :- write(R), write(' = ').
print_rv(L) :- print_list(L), write(' = ').

print_pars([]).
print_pars(L) :- write('('), print_list(L), write(')').

print_list([A]) :- write(A).
print_list([H|T]) :- write(H), write(', '), print_list(T).

print_comm(exp(E, false), Indent) :- indent(Indent), print_exp(E), nl.
print_comm(exp(E, true), Indent) :- indent(Indent), print_exp(E), write(';'), nl.
print_comm(assign(V, E, false), Indent) :- indent(Indent), print_exp(V), write(' = '), print_exp(E), nl.
print_comm(assign(V, E, true), Indent) :- indent(Indent), print_exp(V), write(' = '), print_exp(E), write(';'), nl.
print_comm(for(V, E, Commands), Indent) :-
	indent(Indent), write('for '), print_exp(V), write(' = '), print_exp(E), nl,
	I1 is Indent + 2, print_commands(Commands, I1),
	indent(Indent), write('end'), nl.
print_comm(while(E, Commands), Indent) :-
	indent(Indent), write('while '), print_exp(E), nl,
	I1 is Indent + 2, print_commands(Commands, I1),
	indent(Indent), write('end'), nl.
print_comm(if(E, C1, C2), Indent) :-
	indent(Indent), write('if '), print_exp(E), nl,
	I1 is Indent + 2, print_commands(C1, I1),
	indent(Indent), print_restif(C2, Indent).
print_comm(switch(E, C, O), Indent) :-
	indent(Indent), write('switch '), print_exp(E), nl,
	I1 is Indent + 2, print_cases(C, I1),
	print_otherwise(O, I1),
	indent(Indent), write('end'), nl.
print_comm(break, Indent) :- indent(Indent), write('break'), nl.
print_comm(continue, Indent) :- indent(Indent), write('continue'), nl.
print_comm(return, Indent) :- indent(Indent), write('return'), nl.
	
print_otherwise([], _).
print_otherwise(Commands, Indent) :-
	indent(Indent), write('otherwise'), nl,
	I1 is Indent + 2, print_commands(Commands, I1).

print_cases([], _).
print_cases([case(E,C)|Cs], Indent) :-
	indent(Indent), write('case '), print_exp(E), nl,
	I1 is Indent + 2, print_commands(C, I1),
	print_cases(Cs, Indent).

print_restif([], _) :- write('end'), nl.
print_restif([if(E, C1, C2)], Indent) :- write('elseif '), print_exp(E), nl,
									  I1 is Indent + 2, print_commands(C1, I1),
									  indent(Indent), print_restif(C2, Indent).
print_restif(C, Indent) :- write('else'), nl,
						   I1 is Indent + 2, print_commands(C, I1),
						   indent(Indent), write('end'), nl.

left_un_op('-').
left_un_op('+').
left_un_op('~').

print_exp(bin_op(Op, L, R)) :- print_exp(L), write(' '), write(Op), write(' '), print_exp(R).
print_exp(un_op(Op, E)) :- left_un_op(Op), write(Op), print_exp(E).
print_exp(un_op(Op, E)) :- print_exp(E), write(Op).
print_exp(colon(S, E)) :- print_exp(S), write(':'), print_exp(E).
print_exp(colon(S, I, E)) :- print_exp(S), write(':'), print_exp(I), write(':'), print_exp(E).
print_exp(var(V)) :- write(V).
print_exp(const(C)) :- write(C).
print_exp(func_call(N, [])) :- write(N).
print_exp(func_call(N, P)) :- write(N), write('('), print_call_pars(P), write(')').

print_call_pars([P]) :- print_exp(P).
print_call_pars([P|Ps]) :- print_exp(P), write(', '), print_call_pars(Ps).

disambiguate(function(Name, R, P, C), function(Name, R, P, V, NC)) :-
	disambig_commands(C, NC, P, V).

disambig_commands([], [], V, V).
disambig_commands([C|Cs], [NC|NCs], V, NV) :- disambig_comm(C, NC, V, V1),
										disambig_commands(Cs, NCs, V1, NV).

disambig_comm(assign(var(Var), E, S), assign(var(Var), E1, S), V, NV) :-
	disambig_exp(E, E1, V), set_or(V, NV, Var).
disambig_comm(exp(E, S), exp(E1, S), V, V) :- disambig_exp(E, E1, V).
disambig_comm(for(var(Var), E, C), for(var(Var), E1, C1), V, NV) :-
	disambig_exp(E, E1, V), set_or(V, V1, Var),
	disambig_commands(C, C1, V1, NV).
disambig_comm(while(E, C), while(E1, C1), V, NV) :-
	disambig_exp(E, E1, V), disambig_commands(C, C1, V, NV).
disambig_comm(if(E, C, C1), if(E1, C2, C21), V, NV) :-
	disambig_exp(E, E1, V), disambig_commands(C, C2, V, V1),
	disambig_commands(C1, C21, V1, NV).
disambig_comm(switch(E, Cs, O), switch(E1, Cs1, O1), V, NV) :-
	disambig_exp(E, E1, V), disambig_cases(Cs, Cs1, V, V1),
	disambig_commands(O, O1, V1, NV).
disambig_comm(break, break, V, V).
disambig_comm(continue, continue, V, V).
disambig_comm(return, return, V, V).

disambig_cases([], [], V, V).
disambig_cases([case(E,C)|Cs], [case(E1,C1)|Cs1], V, NV) :-
	disambig_exp(E, E1, V), disambig_commands(C, C1, V, V1),
	disambig_cases(Cs, Cs1, V1, NV).

disambig_exps([], [], _).
disambig_exps([H|T], [H1|T1], Vars) :- disambig_exp(H, H1, Vars),
	disambig_exps(T, T1, Vars).
disambig_exp(var(V), var(V), Vars) :- exists(Vars, V).
disambig_exp(var(V), func_call(V, []), _).
disambig_exp(subscript(var(V), I), subscript(var(V), NI), Vars) :-
	exists(Vars, V), disambig_exps(I, NI, Vars).
disambig_exp(subscript(var(V), I), func_call(V, NI), Vars) :-
	disambig_exps(I, NI, Vars).
disambig_exp(bin_op(Op, E1, E2), bin_op(Op, N1, N2), V) :-
	disambig_exp(E1, N1, V), disambig_exp(E2, N2, V).
disambig_exp(un_op(Op, E), un_op(Op, N), V) :- disambig_exp(E, N, V).
disambig_exp(const(N), const(N), _).
disambig_exp(colon(E1, E2), colon(N1, N2), V) :-
	disambig_exp(E1, N1, V), disambig_exp(E2, N2, V).
disambig_exp(colon(E1, E2, E3), colon(N1, N2, N3), V) :-
	disambig_exp(E1, N1, V), disambig_exp(E2, N2, V), disambig_exp(E3, N3, V).
disambig_exp(matrix([[R]]), R1, V) :- disambig_exp(R, R1, V).
disambig_exp(matrix(Rows), matrix(R1), V) :- disambig_rows(Rows, R1, V).

disambig_rows([], [], _).
disambig_rows([H|T], [H1|T1], Vars) :- disambig_exps(H, H1, Vars),
	disambig_rows(T, T1, Vars).

set_or([], [Var], Var).
set_or([Var|V], [Var|V], Var).
set_or([Var1|V1], [Var1|V2], Var) :- set_or(V1, V2, Var).

exists([V|_], V).
exists([V1|Vars], V) :- V1 \== V, exists(Vars, V).
