:- module(dmc, [dm/2]).

:- use_module(readprog).
:- use_module(domainProg).
:- use_module(library(lists)).
:- use_module(tp).
/*%%%CIAO
:- use_module(timer_ciao).
*/%%%CIAO
%%%SICS/*
:- use_module(timer_sics).
%%%SICS*/
:- use_module(library(ciaopp)).
:- use_module(program(p_unit),
        [ program/2]).




dm(Prog,Int) :-
	start_time,
	ciaopp_readprog(Prog,Cls),
	domainProg(Cls,MCls),
	readprog(Int,ICls),
	tpr(ICls,[],M1),
	tpr(MCls,M1,M2),
	displayClauses(user_output,M2),
	end_time('Abstract model computation time: ',user_output),
	nl(user_output).
	
ciaopp_readprog(F,Cls) :-
	module(F),
	program(P1,D1),
	jpg_program_format(P1,D1,Cls).
