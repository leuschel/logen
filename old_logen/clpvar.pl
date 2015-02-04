
:- module(clpvar, 
	    [
	     copy/3,
	     copy/4,
	     is_clp_var/1
	     
	    ]).

%% Ciao-specific libs
:- use_package(sicsignore).
	 
%% Ciao
:- use_package(clpr). % for CIAO
%:- use_module(library(clpr)). % for SICStus

sicstus((:- use_module(library(clpq)))).
:- use_package(library(clpq)).

:- use_module(library(terms)).

% Is the variable flagged for the clp(r) domain?
is_clp_var(X) :-
	var(X),
%	clpr:get_atts(X,L),
	clpq:get_atts(X,L),
	L \= [].

% Given a list of variables, remove all the non clp vars
get_clp_vars([],[]).
get_clp_vars([Var|Tail], NEW) :- (is_clp_var(Var) -> NEW = [Var|NTail]; NEW =NTail), get_clp_vars(Tail, NTail).


%% Copy term and remove clp bindings on new vars,
copy(F,N,CopyConstr) :- copy(F,N,CopyConstr, _).
copy(F, N, CopyConstr, Env) :-
	term_variables(F, Vars),
	get_clp_vars(Vars, CLPVars),
	dump(CLPVars,CLPVars, Constr),
	copy_vars(Vars, _, _,Env),!,
	copy_vars_term(F, N, Env),
	copy_vars_term(Constr, CopyConstr, Env).
	
%are the constraints implied by exisitng constraints?
%%checkCLPEntailment(Vars, Constraints)

copy_vars_term(F, N, Env) :- var(F), getVar(F,N,Env,_).
copy_vars_term(F,N,_) :- ground(F),!, N = F.
copy_vars_term(F, N, Env) :-
	nonvar(F),
	F =.. [Func| Args],
	list_copy_vars(Args, NewArgs, Env),
	N =.. [Func|NewArgs],!.


list_copy_vars(F,N, _) :- F ==[], N = [].
list_copy_vars([Head|Tail], [NHead|NTail], Env) :-
	copy_vars_term(Head, NHead, Env),
	list_copy_vars(Tail, NTail, Env).


copy_vars([],[],Env, Env).
copy_vars([Var | Tail], [New| NTail],Bindings ,Env   ) :-
	getVar(Var, New, Bindings, NewBindings),
	(NewBindings == [] -> Binds = Bindings ; Binds = [NewBindings | Bindings]),
	copy_vars(Tail, NTail, Binds,Env).

		 
getVar(X,Y , [], b(X,Y)).
getVar(X, Y, [b(A,Y) | _], []) :- X == A.
getVar(X,Y, [b(A,_)| Tail], Bindings) :- A \==X, getVar(X,Y,Tail , Bindings).






