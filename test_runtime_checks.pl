
:- module(test_runtime_checks,[app/3, rev/2, rev/3]).

% A simple test file to check whether the pp_mnf expansion package works

% use_module(test_runtime_checks),rev([a,b],X).

:- include(runtime_checks_perform).


:- pp_mnf(app/3).

% Specif which predicats to unit test  for unit_test.pl
:- meta_predicate unit_test_predicate(goal).
unit_test_predicate(app(_,_,_)).
unit_test_predicate(rev(_,_)).


:- meta_predicate unit_test_pred(goal,?,goal).

%:- unit_test app([zz],[],X) : X=[zz].

unit_test_pred(app([],[],X),succ,X=[]).
unit_test_pred(app([],[],X),succ,X=[a]).
unit_test_pred(app([],[],X),fail,X=[]).
unit_test_pred(app([a,b,c],[d],X),succ,X=[a,b,c,d]).
unit_test_pred(app([a,b,c],[d],X),det,X=[a,b,c,d]).
unit_test_pred(app(_X,_,[a,b]),det,true).
unit_test_pred(app([a,b,c],[d],[_,_,_]),fail,fail).
unit_test_pred(rev([a,b,c],X),succ,X=[c,b,a]).

app_succ([],[],[]).
app_succ([a],[b],[a,b]).
app_succ([a],[c],X) :- X=[a,c].
app_succ(X,[],X) :- (X=[] ; X=[a] ; X=[1,2,3,4,5]).
app_fail([],[a],[]).
app_fail([a],[b],[_,_,_|_]).



:- pre app(X,_,Z) : (nonvar(X);nonvar(Z)).
%app_pre(X,_,Z) :- nonvar(X);nonvar(Z).
:- post app(X,_,Z) : (nonvar(X),nonvar(Z)).
%app_post(X,_,Z) :- nonvar(X),nonvar(Z).

app([],L,L).
app([H|X],Y,[H|Z]) :- app(X,Y,Z).

:- mnf(rev/2).

rev_succ([],[]).
rev_succ([a,b,c],[c,b,a]).
rev_fail([a],[_,_|_]).

rev(X,R) :- rev(X,[],R).

:- pp_call(rev/3).


rev_pre(X,H,_) :- nonvar(X),nonvar(H).
rev_post(_,_,R) :- nonvar(R).

rev([],A,A).
rev([H|X],A,R) :- rev(X,[H|A],R).


test(X) :- app([a],[b],X).
test(X) :- app(X,[b],[a]).
test([a]) :- app([a],[b],[a]).
test(X) :- rev([a,b,c],X).
test(_X) :- rev([a,b,c],[]).

