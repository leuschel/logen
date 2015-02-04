:- module(trial_client, [start/1]).

:- use_module(lindaserver).
:- use_module(library('linda/client')).
:- use_module(trial).
:- use_module(library(system)).




:- dynamic current_data/1.
:- dynamic client_name/1.

start(Host) :-
	connect_to_linda(Host),
	tmpnam(Name),
	assert(client_name(Name)),
	out(client_name(Name)),
	loop.

loop :-
	rd(command(starting)),
	format(user, "Getting Settings...",[]),	      
	get_settings,
	format(user, "OK~n",[]),
	client_name(Name),
	out(client_ready(Name)),
	process_conf.



get_settings :-
	retractall(current_data(X)),
	bagof_rd_noblock(current_data(X), current_data(X), Data),
	assert_list(Data).

assert_list([]).
assert_list([A|As]) :- assert(A), assert_list(As).


process_conf :-
	in(conf(BtaAnn,BtaFils)),
	%portray_clause(in(conf(BtaAnn,BtaFils))),
	(BtaAnn \== stop ->
	    (
	      %%% Do a task and loop.  
	      (BtaAnn = bta(ann(Ann,Fil)) ->
		  client_bta(Ann, Fil)
	      ;
		  client_conf(conf(BtaAnn,BtaFils))
	      ),		  
	      process_conf
	    )
	;
	    format(user, "Quitting~n",[]),	      
	    loop
	).

client_bta(Ann,Fil) :-
	current_data(filename(Filename)),
	current_data(norm(Norm)),
	current_data(state(State)),
	current_data(tmpfile(Output)),
	current_data(filter_prop_only(FilterProp)),
	%format(user, "Safety...",[]),
	trial:safety_check_configuration(ann(Ann,Fil),ann(BtaAnn,BtaFil),State,Filename,Norm,Output,FilterProp),
	%%format(user, "OK~nSending...",[]),	      
	out(bta_result(ann(Ann,Fil), ann(BtaAnn,BtaFil))),
	%format(user, "OK~n",[]),
	true.
	
client_conf(conf(BtaAnn,BtaFils)) :-
	current_data(annfile(AnnFile)),
	current_data(filename(Filename)),
	current_data(iter(BenchTimes)),
	current_data(query(Query)),
	current_data(state(BtaState)),
	current_data(timeout(Timeout)),	
	%portray_clause(testing(BtaAnn,BtaFils)),
	%format(user, "Testing...",[]),
	trial:test_configuration(AnnFile, BtaAnn,BtaFils,BtaState,Filename,Query,BenchTimes,Timeout,Result),
	%%%format(user, "OK~nSending...",[]),	      
	out(conf_result(BtaAnn,BtaFils, Result)),
	%format(user, "OK~n",[]),
	true.

	
	
	
	


	

