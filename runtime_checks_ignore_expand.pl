
:- module(runtime_checks_ignore_expand,[ ignore_mnf/3 ],[ ]).
:- op(500,yfx,pre).
:- op(500,yfx,post).
:- op(500,yfx,unit_test).

ignore_mnf(term_expansion(_,_), [], _M).

ignore_mnf(( :- mnf(_Pred/_Arity) ), [], _).
ignore_mnf(( :- pp_mnf(_Pred/_Arity) ), [], _).
ignore_mnf(( :- pp_call(_Pred/_Arity) ), [], _).
ignore_mnf(( :- pre _Pred:_PreCond ), [], _).
ignore_mnf(( :- post _Pred:_PostCond ), [], _).
ignore_mnf((Head :- Body), (Head :- IBody), _M) :-
        ignore_mnf_body(Body,IBody),!.


ignore_mnf_body(X,X) :- var(X),!.
ignore_mnf_body(pp_mnf(X),X).
ignore_mnf_body(mnf(X),X).
ignore_mnf_body(pp_call(X),X).
ignore_mnf_body((A,B),(IA,IB)) :- ignore_mnf_body(A,IA), ignore_mnf_body(B,IB).
ignore_mnf_body((A;B),(IA;IB)) :- ignore_mnf_body(A,IA), ignore_mnf_body(B,IB).
ignore_mnf_body((A->B),(IA->IB)) :- ignore_mnf_body(A,IA), ignore_mnf_body(B,IB).
ignore_mnf_body(\+(A),\+(IA)) :- ignore_mnf_body(A,IA).
ignore_mnf_body(findall(V,A,R),findall(V,IA,R)) :- ignore_mnf_body(A,IA).
ignore_mnf_body(setof(V,A,R),setof(V,IA,R)) :- ignore_mnf_body(A,IA).
ignore_mnf_body(X,X).

%ignore_mnf(end_of_file, end_of_file,_M).




