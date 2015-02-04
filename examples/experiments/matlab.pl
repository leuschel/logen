% specialisation goals : run1([X]), run12([X]), run2([X]), run22([X])
% run1 is factorial using a while loop
% run2 is factorial using recursion
% run12 and run22 use the parser but should produce exactly the same results

% to run try run12([const(5)])
% should eventually print 120

eval_mfile(Text, Params, Nargout) :- load, parse(Text, MFile), !,
			eval_matlab(MFile, Params, Nargout).

% NB: The following line should point to the absolute path of matlab_parser.pl
load :- use_module('matlab_parser.pl').

eval_matlab([F|Funcs], Pars, 1) :-
	eval_function(F, Pars, 1, [R], [F|Funcs]),
	print_value(R).

%print_answers([]).
%print_answers([A|As]) :- write('Result is '), print_value(A), nl,
%	print_answers(As).
	
eval_function(function(_, Rets, Pars, Vars, Comms), Params, Nargout, Values, Funcs) :-
	bind_undef(env([], Funcs), Vars, NEnv3),
	bind_params(NEnv3, NEnv, Pars, Params),
	eval_commands(Comms, NEnv, NEnv2),
	return_values(Nargout, Rets, Values, NEnv2).

bind_undef(Env, [], Env).
bind_undef(Env, [Var|Vars], NEnv) :-
	store_var(Env, Var, undef, Env2),
	bind_undef(Env2, Vars, NEnv).

bind_params(Vars, Vars, _, []).
bind_params(Vars, NVars, [Var|Pars], [Value|Values]) :-
	store_var(Vars, Var, Value, Vars2),
	bind_params(Vars2, NVars, Pars, Values).

return_values(0, _, [], _).
return_values(N, [Var|Rets], [V|Values], Env) :-
	M is N - 1,
	return_values(M, Rets, Values, Env),
	lookup_var(Var, Env, V).

eval_commands([], Env, Env).
eval_commands([C|Program], Env, NEnv) :-
	eval_comm(C, Env, N), eval_commands(Program, N, NEnv).

eval_comm(exp(E,false), Env, NEnv) :-
	eval_exp(E, NE, Env, NEnv), print_exp('ans', NE).
eval_comm(exp(E,true), Env, NEnv) :- eval_exp(E, _NE, Env, NEnv).

eval_comm(assign(var(V), E, false), Env, NEnv) :-
	eval_exp(E, NE, Env, N1), store_var(N1, V, NE, NEnv), print_exp(V, NE).
eval_comm(assign(var(V), E, true), Env, NEnv) :-
	eval_exp(E, NE, Env, N1), store_var(N1, V, NE, NEnv).
eval_comm(while(Exp, Commands), Env, NEnv) :-
	env_copy(Env, NEnv),
	eval_while(Exp, Commands, Env, NEnv).

eval_comm(for(Var, Range, Commands), Env, NEnv) :-
	eval_exp(Range, R, Env, N),
	eval_for(Var, R, Commands, N, NEnv).

eval_comm(if(Exp, Commands, _), Env, NEnv) :-
	eval_exp(Exp, N, Env, Env2), non_zero(N), !,
	eval_commands(Commands, Env2, NEnv).
eval_comm(if(_, _, Commands), Env, NEnv) :-
	eval_commands(Commands, Env, NEnv).

eval_while(Exp, Commands, Env, NEnv) :-
	eval_exp(Exp, N, Env, Env2), non_zero(N),
	eval_commands(Commands, Env2, Env3),
	env_copy(Env3, NEnv),
	eval_while(Exp, Commands, Env3, NEnv).

eval_while(_, _, Env, Env).

eval_for(var(Var), matrix([[]]), _, Env, NEnv) :- store(Env, Var, [], NEnv).
eval_for(var(Var), R, Commands, Env, NEnv) :- get_elements(R, R1), eval_for2(Var, R1, Commands, Env, NEnv).

get_elements(const(X), [const(X)]).
get_elements(matrix([X]), X).

eval_for2(_, [], _, Env, Env).
eval_for2(Var, [R|Range], Commands, Env, NEnv) :-
	store_var(Env, Var, R, N),
	eval_commands(Commands, N, N2),
	eval_for2(Var, Range, Commands, N2, NEnv).

env_copy(env(V, F), env(NV, F)) :- var_copy(V, NV).

var_copy([], []).
var_copy([X/_|T], [X/_|T1]) :- var_copy(T,T1).

eval_exp(const(C), const(C), Env, Env).
eval_exp(var(Var), Value, Env, Env) :- lookup_var(Var, Env, Value).
eval_exp(bin_op(Op, Exp1, Exp2), Res, Env, NEnv) :-
	eval_exp(Exp1, R1, Env, E1),
	eval_exp(Exp2, R2, E1, NEnv),
	apply_op(Op, R1, R2, Res).

eval_exp(func_call(Func, Params), Value, Env, NEnv) :-
	eval_exps(Params, NParams, Env, NEnv),
	lookup_func(Func, Env, Function),
	get_funcs(Env, Funcs),
	eval_function(Function, NParams, 1, [Value], Funcs).

eval_exp(matrix(Rows), Result, Env, Env) :-
	eval_rows(Rows, NewRows, Env), convert(NewRows, Result).

eval_exp(colon(S, E), Result, Env, Env) :- eval_exp(S, const(S1), Env, Env),
	eval_exp(E, const(E1), Env, Env),
	expand_colon(S1, 1, E1, Result), !.
eval_exp(colon(S, I, E), Result, Env, Env) :-
	eval_exp(S, const(S1), Env, Env),
	eval_exp(I, const(I1), Env, Env),
	eval_exp(E, const(E1), Env, Env),
	expand_colon(S1, I1, E1, Result), !.

expand_colon(_, 0, _, matrix([[]])).
expand_colon(S, I, E, matrix([P])) :- I > 0, arith_prog_incr(S, I, E, P).
expand_colon(S, I, E, matrix([P])) :- I < 0, arith_prog_decr(S, I, E, P).

arith_prog_incr(S, _, E, []) :- S > E.
arith_prog_incr(S, I, E, [const(S)|T]) :- S1 is S + I, arith_prog_incr(S1, I, E, T).

arith_prog_decr(S, _, E, []) :- S < E.
% I is negative so addition will result in decreasing sequence
arith_prog_decr(S, I, E, [const(S)|T]) :- S1 is S + I, arith_prog_decr(S1, I, E, T).

convert([[const(R)]], const(R)).
convert(Rows, matrix(Rows)).

eval_rows([], [], _).
eval_rows([R|Rows], [N|NRows], Env) :-
	eval_exps(R, N, Env, _), eval_rows(Rows, NRows, Env).

get_funcs(env(_, Funcs), Funcs).


eval_exps([], [], E, E).
eval_exps([P|Pars], [NP|NPars], Env, NEnv) :-
	eval_exp(P, NP, Env, Env2),
	eval_exps(Pars, NPars, Env2, NEnv).
	
add(const(C1), const(C2), const(C3)) :- C3 is C1 + C2.
minus(const(C1), const(C2), const(C3)) :- C3 is C1 - C2.
mldivide(const(C1), const(C2), const(C3)) :- C3 is C2 / C1.
mrdivide(const(C1), const(C2), const(C3)) :- C3 is C1 / C2.
mtimes(const(C1), const(C2), const(C3)) :- C3 is C1 * C2.
gt(const(E1), const(E2), const(1)) :- E1 > E2.
gt(const(E1), const(E2), const(0)) :- E1 =< E2.
lt(const(E1), const(E2), const(1)) :- E1 < E2.
lt(const(E1), const(E2), const(0)) :- E1 >= E2.
ge(const(E1), const(E2), const(1)) :- E1 >= E2.
ge(const(E1), const(E2), const(0)) :- E1 < E2.
le(const(E1), const(E2), const(1)) :- E1 =< E2.
le(const(E1), const(E2), const(0)) :- E1 > E2.
eq(const(E1), const(E2), const(1)) :- E1 == E2.
eq(const(E1), const(E2), const(0)) :- E1 \== E2.
ne(const(E1), const(E2), const(1)) :- E1 \== E2.
ne(const(E1), const(E2), const(0)) :- E1 == E2.

is_zero(const(0)).
non_zero(const(N)) :- N \== 0.

apply_op('+', E1, E2, R) :- add(E1, E2, R).
apply_op('-', E1, E2, R) :- minus(E1, E2, R).
apply_op('/', E1, E2, R) :- mrdivide(E1, E2, R).
apply_op('\\', E1, E2, R) :- mldivide(E1, E2, R).
apply_op('*', E1, E2, R) :- mtimes(E1, E2, R).
apply_op('>', E1, E2, R) :- gt(E1, E2, R).
apply_op('<', E1, E2, R) :- lt(E1, E2, R).
apply_op('>=', E1, E2, R) :- ge(E1, E2, R).
apply_op('<=', E1, E2, R) :- le(E1, E2, R).
apply_op('==', E1, E2, R) :- eq(E1, E2, R).
apply_op('~=', E1, E2, R) :- ne(E1, E2, R).

print_exp(V, NE) :- write(V), write(' ='), nl, print_value(NE).

print_element(const(NE)) :- write('    '), write(NE).
print_value(const(NE)) :- write('    '), write(NE), nl,nl.
print_value(undef) :- write('    undefined!'), nl,nl.
print_value(matrix([[]])) :- write('    []'), nl.
print_value(matrix(NE)) :- print_rows(NE), nl.
print_rows([]).
print_rows([R|Rows]) :- print_row(R), nl, print_rows(Rows).
print_row([]).
print_row([C|Cols]) :- print_element(C), print_row(Cols).

store_var(env(Vars, Funcs), Key, Value, env(NVars, Funcs)) :-
	store(Vars, Key, Value, NVars).

lookup_var(Key, env(Vars, _Funcs), Value) :- lookup(Key, Vars, Value).
lookup_func(Key, env(_, Funcs), Func) :- lookupf(Key, Funcs, Func).

func_matches(Key, function(Key, _, _, _, _)).
lookupf(Key, [F|_x], F) :- func_matches(Key, F).
lookupf(Key, [_|T], Value) :- lookupf(Key, T, Value).

store([], Key, Value, [Key/Value]).
store([Key/_Value2|T], Key, Value, [Key/Value|T]).
store([Key2/Value2|T], Key, Value, [Key2/Value2|BT]) :-
	Key\==Key2, store(T, Key, Value, BT).

lookup(Key, [Key/Value|_], Value).
lookup(Key, [Key2/_|T], Value) :- Key \== Key2, lookup(Key, T, Value).

run1(Pars) :- eval_matlab([
	function('fact', ['y'], ['x'], ['x', 'y'],
		     [assign(var('y'), const(1), true),
			  while(bin_op('>', var('x'), const(0)),
			    [assign(var('y'), bin_op('*', var('y'), var('x')), true),
				 assign(var('x'), bin_op('-', var('x'), const(1)), true)])])],
	Pars, 1).

run12(Pars) :- eval_mfile("function y = fact(x)
y = 1;
while x > 0
  y = y * x;
  x = x - 1;
end", Pars, 1).

run2(Pars) :- eval_matlab([
	function('fact', ['y'], ['x'], ['x', 'y'],
		     [if(bin_op('>', var('x'), const(1)),
			 [assign(var('y'), bin_op('*', var('x'), func_call('fact', [bin_op('-', var('x'), const(1))])), true)],
			 [assign(var('y'), const(1), true)])])],
	Pars, 1).

run22(Pars) :- eval_mfile("function y = fact(x)
if x > 1
  y = x * fact(x - 1);
else
  y = 1;
end", Pars, 1).

run3(Pars) :- eval_mfile("function y = fact(x)
y = 1;
for n = 1:x
  y = y * n;
end", Pars, 1).

/*add_row_const([], _, []).
add_row_const([E|Row], C, [NE|NRow]) :-
	add(E, const(C), NE), add_row_const(Row, C, NRow).

add_matrix_const([], _, []).
add_matrix_const([R|Rows], C, [NR|NRows]) :-
	add_row_const(R, C, NR), add_matrix_const(Rows, C, NRows).

add_row_row([], [], []).
add_row_row([E1|Row1], [E2|Row2], [NE|NRow]) :-
	add(E1, E2, NE), add_row_row(Row1, Row2, NRow).

add_matrix_matrix([], [], []).
add_matrix_matrix([R1|Rows1], [R2|Rows2], [NR|NRows]) :-
	add_row_row(R1, R2, NR), add_matrix_matrix(Rows1, Rows2, NRows).*/

/*add(matrix(M1), const(C2), matrix(M3)) :- add_matrix_const(M1, C2, M3).
add(const(C1), matrix(M2), matrix(M3)) :- add_matrix_const(M2, C1, M3).
add(matrix(M1), matrix(M2), matrix(M3)) :- add_matrix_matrix(M1, M2, M3).*/
/*print_value(matrix([])) :- write('   []'), nl, nl.
*/

