:- module(dm, [dm/2,dmt/3, dmbench/2, dmbenchall/0]).

:- use_module(readprog).
:- use_module(domainProg).
:- use_module(library(lists)).
:- use_module(tp).
%:- use_module(tpset).
/*%%%CIAO
:- use_module(timer_ciao).
*/%%%CIAO
%%%SICS/*
:- use_module(timer_sics).
%%%SICS*/
%:- use_module(tbta).
:- use_module(dfta).



dm(Prog,Int) :-
	start_time,
	readprog(Prog,Cls),
	domainProg(Cls,MCls),
	readprog(Int,ICls),
	tpr(ICls,[],M1),
	tpr(MCls,M1,M2),
	displayClauses(user_output,M2),
	end_time('Abstract model computation time: ',user_output),
	nl(user_output).
	
dmt(Prog,Int,SDV) :-
	write(user_output,Prog),nl(user_output),
	makeDFTA(Int,Prog,ICls,SDV),
	start_time,
	readprog(Prog,Cls),
	domainProg(Cls,MCls),
	tpr(ICls,[],M1),
	tpr(MCls,M1,M2),
	open(dmtout,write,S),
	displayClauses(S,M2),
	close(S),
	end_time('Abstract model computation time: ',user_output),
	nl(user_output),
	!.

% version to call tbta.pl
	
%makeDFTA(Int,Prog,ICls,SDV) :-
%	readtype_sdv(Prog,Int,tempdtype,SDV),
%	readprog(tempdtype,ICls).
	
% version to call dfta.pl

makeDFTA(Int,Prog,ICls,SDV) :-
	fdfta(Int,Prog,tempdtype,SDV),
	readprog(tempdtype,ICls).
	
dmbench(Int,SDV) :-
	write('===================='),nl,
	write(Int),write(' '), write(SDV),nl,
	write('===================='),nl,
	dmt('Tests/trans.pl',Int,SDV), !,
	dmt('Tests/GAIA/peep.p',Int,SDV), !,
	dmt('Tests/GAIA/plan.p',Int,SDV), !,
	dmt('Tests/GAIA/cs_r.p',Int,SDV), !,
	dmt('Tests/GAIA/gabriel.p',Int,SDV), !,
	dmt('Tests/GAIA/kalah_r.p',Int,SDV), !,
	dmt('Tests/GAIA/press1.p',Int,SDV), !,
	dmt('Tests/GAIA/qsort.p',Int,SDV), !,
	dmt('Tests/GAIA/read_r.p',Int,SDV), !,
	dmt('Tests/GAIA/queens.p',Int,SDV), !,
	dmt('Tests/GAIA/pg.p',Int,SDV), !,
	%dmt('Tests/aquarius_compiler.pl',Int,SDV), !,
	dmt('Tests/chat_parser.pl',Int,SDV).
	
dmbenchall :-
	dmbench('empty.pl',sd),
	dmbench('empty.pl',dv),
	dmbench('listtype.pl',d),
	dmbench('Tests/matrixtype.pl',d),
	dmbench('empty.pl',sdv).
