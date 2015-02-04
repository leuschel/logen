table(_).

cputime(R) :- statistics(runtime,[R,_]).

abolish_all_tables.

