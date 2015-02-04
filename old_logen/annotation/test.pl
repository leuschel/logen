/* this is an example program for the new split annotations */
app([A],[B],C) :- memo_me. % comment of this type

/* hopefully the predicates will be correctly coloured in tcl/tk */
error(a, a).
app([X|Xs], Y,[X|Zs]) :- (cond->then;else). %more comment





