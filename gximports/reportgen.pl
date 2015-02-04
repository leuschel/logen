reportgen(S, Tag) :-
	rgen(S, Tag, 0).

indent(_, 0) :- !.
indent(S, X) :- X2 is X - 1, write(S, ' '), indent(S, X2).

rgen(S, Tag, Indent) :-
	indent(S, Indent),
	Tag =.. [H|Args],
	write(S, H),
	rgen_args(S, Args, Indent).

rgen_args(S, [A], Indent) :-
	is_list(A), !, write(S, ':'), nl(S), Indent2 is Indent + 2,
	rgen_list(S, A, Indent2).

rgen_args(S, [A], _) :-
	format(S, ': ~w~n', [A]).

rgen_args(S, [Attrs, A], Indent) :-
	is_list(A), !, write(S, ' ('), rgen_attrs(S, Attrs), write(S, '):'), nl(S),
	Indent2 is Indent + 2, rgen_list(S, A, Indent2).

rgen_args(S, [Attrs, A], _) :-
	write(S, ' ('), rgen_attrs(S, Attrs), 
	format(S, '): ~w~n', [A]).

rgen_list(_, [], _).
rgen_list(S, [A|As], Indent) :-
	rgen(S, A, Indent), rgen_list(S, As, Indent).

rgen_attrs(_, []).
rgen_attrs(S, [A]) :- A =.. [L,R], format(S, '~w ~w', [L, R]).
rgen_attrs(S, [A|As]) :- A =.. [L,R], format(S, '~w ~w ', [L, R]),
	rgen_attrs(S, As).

is_list(X) :- var(X),!,fail.
is_list([]).
is_list([_|_]).