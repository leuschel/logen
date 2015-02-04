
:- module(sicsignore_expand,[ ignore_sics/3 ],[ ]).

ignore_sics(term_expansion(_,_), [], _M).

ignore_sics((:-ensure_loaded(sicstus)), [], _M).

ignore_sics((ciao(Head) :- Body), (Head :- Body), _M).
ignore_sics((ciao(Head)), Head, _M).


ignore_sics((sicstus(_Head) :- _Body),[], _M).
ignore_sics((sicstus(_)), [], _M).
%ignore_sics(end_of_file, end_of_file,_M).




