
% a version with nested lists
cps_int_nest([]).
cps_int_nest([H|T]) :- cps2(H,T).

cps2([],T) :- cps_int_nest(T).
cps2([H|T1],T2) :- cps_int_nest([H,T1|T2]).
cps2(A,T2) :- cl(A,B), cps_int_nest([B|T2]).



cl(app([],L,L),[]).
cl(app([H|X],Y,[H|Z]),[app(X,Y,Z)]).
cl(dapp(A,B,C,R),[app(B,C,BC),app(A,BC,R)]).


% a version with append
cps_int([]).
cps_int([H|T]) :- cl(H,B), app(B,T,BT), cps_int(BT).

% a version with DCGs

cps_dcg(C) :- cps_dcg(C,[]).

cps_dcg --> [].
cps_dcg --> [H], cl3(H), cps_dcg.


cl3(app([],L,L)) --> [].
cl3(app([H|X],Y,[H|Z]),In,Out) :- Out=[app(X,Y,Z)|In].
cl3(dapp(A,B,C,R),In,Out) :- Out = [app(B,C,BC),app(A,BC,R)|In].


app([],L,L).
app([H|X],Y,[H|Z]) :- app(X,Y,Z).


test_dcg([A,B,C,R]) :- cps_dcg([dapp(A,B,C,R)]).

/*
The BTA does not yet infer that cps_dcg's first argument is a list nonvar:

bta cps_int.pl --entry "test_dcg(d)." -o cps_int.pl.ann
BTA Call/Answer Patterns
cps_dcg/2 [d,s] -> [nv,s]
cps_dcg/1 [list_nv] -> [list_nv]
test_dcg/1 [d] -> [list_nv]
cl3/3 [d,d,d] -> [nv,d,d]

Thus, currently I am using a hand annotated file
logen cps_int.pl "test_dcg(X)" -ap --spec_file cps_int_spec.pl -v -w

*/