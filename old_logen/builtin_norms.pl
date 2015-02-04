

:- module(builtin_norms, [builtin_convex/2]).



:- use_module(convex_norm, [convex_norm/1]).



builtin_convex(A=B, [A=B]).
builtin_convex(A==B, [A=B]). % if succeeds then norm must be the same?

builtin_convex(A\=B, []). % no relation between norms...
builtin_convex(A\==B, []). % no relation between norms...


builtin_convex(length(X,Y), [Y = rat(0,1)]) :-	convex_norm(term).
builtin_convex(length(X,Y), []) :- convex_norm(list).

builtin_convex(A is B, []) :- convex_norm(list), !.
builtin_convex(A is B, [A = rat(0,1)]) :- convex_norm(term), !.


/* added by mal: 30-09-04 */
builtin_convex(A=<B, [A=B]). /* both must be numbers, hence same norm !? */
builtin_convex(A<B, [A=B]).
builtin_convex(A>B, [A=B]).
builtin_convex(A>=B, [A=B]).
