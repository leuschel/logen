:- module(trial, [run/9]).


%%% see trial_tests for tests


:- use_module(lindaserver).
:- use_module(library('linda/client')).





:- use_module('trial_interface.pl').
:- use_module('ann_manager.pl').
:- use_module('mutator.pl').
:- use_module(library(system)).
:- use_module(library(lists)).

:- dynamic current_data/1.
:- dynamic waiting/1.

linda_clear_data :-
	(in_noblock(current_data(__)) ->
	    linda_clear_data
	;
	    true
	).

stop_clients :-
	bagof_rd_noblock(Name, client_name(Name), Clients),
	stop_clients(Clients).

stop_clients([]).
stop_clients([_Name|Ns]):-
	out(conf(stop,stop)),
	stop_clients(Ns).





wait_for_clients :-
	bagof_rd_noblock(Name, client_name(Name), Clients),
	wait_for_clients(Clients).

wait_for_clients([]).
wait_for_clients([Name|Ns]) :-
	in(client_ready(Name)),
	portray_clause(ready(Name)),
	wait_for_clients(Ns).
	



run(File, AnnFile, Query, Times, Norm, Timeout, Answer, Score,Count) :-
	%%linda
	connect_to_linda(localhost),
	
	%% Get Annotations
	load_annotations(AnnFile, Annotations, AnnSigs, Filters, FilSigs, State),
	

	%% Save state
	retractall(cache(_,_)),
	retractall(bta_cache(_,_)),
	retractall(waiting(_)),
	retractall(waiting_bta(_)),
	
	
	retractall(current_data(_)),
	assert(current_data(filename(File))),
	assert(current_data(annfile(AnnFile))),
	assert(current_data(state(State))),
	assert(current_data(query(Query))),
	assert(current_data(iter(Times))),
	assert(current_data(norm(Norm))),
	assert(current_data(timeout(Timeout))),
	assert(current_data(score(Score))),
	assert(current_data(filter_prop_only(true))),
	assert(current_data(tmpfile('tmp.ann'))),
	

	%% linda data
	linda_clear_data,
	
	out(current_data(filename(File))),
	out(current_data(annfile(AnnFile))),
	out(current_data(state(State))),
	out(current_data(query(Query))),
	out(current_data(iter(Times))),
	out(current_data(norm(Norm))),
	out(current_data(timeout(Timeout))),
	out(current_data(score(Score))),
	out(current_data(filter_prop_only(true))),
	out(current_data(tmpfile('tmp.ann'))),
	
	out(command(starting)),	
	wait_for_clients,
	in(command(starting)),
	
	%% Check original File
	bench_basecase(ann(Annotations,Filters),File,AnnFile,State,Query,Times,Timeout,Orig),
	
	assert(current_data(base_result(Orig))),
	%bench_original(File, Query, Times, Orig),
	portray_clause(user, query(Query, iterations(Times))),
	portray_clause(original-Orig),
	
	portray_clause(starting(Annotations,Filters)),
	%get_all_mutations(Annotations, Filters, Mutations),
	%test_l(Mutations),
	run_iterations([ann(Annotations, Filters)], Answer, signatures(AnnSigs,FilSigs),[]),

	current_data(spec_and_bench(Count)),
	
	%% linda, everything should be empty by now!
	%linda_clear,
	stop_clients,
	%out(conf(stop,stop)), 	%% should be one per client!
	
	
	
	close_client,
	
	%%% Reset the file to how we found it...	
	save_annotations(AnnFile, Annotations, Filters, State).

bench_basecase(ann(Ann,Fil), Filename,_AnnFile,State,Query,BenchTimes,Timeout,Result) :-
	get_base_case(ann(Ann,Fil), ann(BAnn,BFil)),
	test_configuration(_AnnFile, BAnn,BFil, State, Filename, Query, BenchTimes,Timeout, Result).

	%save_annotations(AnnFile, BAnn,BFil, State),	
	%spec_and_bench(Filename, Query, BenchTimes,Timeout, Result).

run_iterations(AnnFils, Answer, Signatures,OldScores) :-
	%% input is a set of Annotations
	%% get all mutations from this set
	current_data(query(Query)),
	functor(Query, Func, Arity),
	get_all_mutations_l(AnnFils, Mutations, Signatures, Func/Arity),
	
	
	%length(Mutations, Number),
	%portray_clause(user,number_of_mutations(Number)),

	%% test all these mutations, answers will be in cache/2
	length(Mutations,X), portray_clause(user,mutations(X)),


	safe_set_l(Mutations),
	get_safe_set_l(SafeSet),
	test_l(SafeSet),	   
%	test_l(Mutations),
	
	get_linda_results,
	choose_best(Best, Scores),
	

	
	%(Best \== AnnFils ->
	(Scores \== OldScores ->
	    run_iterations(Best, Answer, Signatures,Scores)
	;
	    get_best_answers(Best, Answer)
	).

get_best_answers([], []).
get_best_answers([AnnFil|As], [AnnFil-Results|Rs]) :-
	cache(AnnFil, Results),
	get_best_answers(As, Rs).



%calc_score(Method,Result, Score) :-	
%	current_data(score_cache(Method,Result, Score)),
%	!.


calc_score(size,bench(Time,Iter,Size), Score) :-
	number(Size), !,
	current_data(base_result(bench(_BTime,_,BSize))),
	Score is BSize/Size,
	assert(current_data(score_cache(size_time, bench(Time, Iter, Size), Score))).
	
	
calc_score(time,bench(Time,Iter,Size), Score) :-
	number(Time), !,
	current_data(base_result(bench(BTime,_,_BSize))),
	Score is BTime/Time,
	assert(current_data(score_cache(size_time, bench(Time, Iter, Size), Score))).

calc_score(size_time,bench(Time,Iter,Size), Score) :-
	number(Time),number(Size),
	!,
	current_data(base_result(bench(BTime,_,BSize))),	
	NTime is BTime/Time,
	NSize is BSize/Size,
	Score is NTime * NSize,
	assert(current_data(score_cache(size_time, bench(Time, Iter, Size), Score))).

calc_score(_,_,-1.0).

	

%calc_score(bench(Time, Iter, Size),Time).
	

get_first_n(0, _,[],[]).
get_first_n(_,[],[],[]).
get_first_n(X, [S-A|As], [A|Bs],[S|Ss]) :-
	X >0,
	X1 is X -1,
	get_first_n(X1, As,Bs,Ss).


	    
choose_best(Best, Scores) :-
	current_data(score(Method)),
	findall(Score-Ann, (cache(Ann,Result), calc_score(Method,Result, Score)), ScoreResults),
	keysort(ScoreResults, RSorted),
	reverse(RSorted, Sorted),
	get_first_n(3, Sorted, Best,Scores).

			
print_cache :-
	cache(A,B),
	portray_clause(cache(A,B)),
	fail.
print_cache.
	
	



:- dynamic cache/2.
:- dynamic bta_cache/2.
:- dynamic waiting_bta/1.
% cache(ann([unfold,...], [static, ...]), bench(Time, Iter, Size)).


safe_set_l([]).
safe_set_l([AnnFil|As]) :-
	get_btad_ann(AnnFil),
	safe_set_l(As).


get_safe_set_l([BtaAnnFils| Bs]) :-
	waiting_bta(_),
	!,	
	in(bta_result(AnnFils, BtaAnnFils)),
	retract(waiting_bta(AnnFils)),
	assert(bta_cache(AnnFils, BtaAnnFils)),
	get_safe_set_l(Bs).

get_safe_set_l([]).

%%% Ensure the annotations are safe
%%% this requies running the bta/filter_prop or using cached answer
get_btad_ann(AnnFils) :- 
	bta_cache(AnnFils,_BTAAnnFils),
	!.

get_btad_ann(AnnFils) :-
	waiting_bta(AnnFils),
	!.

get_btad_ann(AnnFils):-
	assert(waiting_bta(AnnFils)),
	out(conf(bta(AnnFils),0)).

	%in(bta_result(AnnFils, BtaAnnFils)),	
	%assert(bta_cache(AnnFils, BtaAnnFils)).


	%% save to ann file
	%save_annotations(AnnFile, Ann,Fils, State),

	%(current_data(filter_prop_only(true)) ->
	%    run_filterprop(Filename, Output, AnnFile)
	%;	    
	%    run_bta(Filename, Norm, Output, AnnFile)
	%),
	
	% retrieve new annotations
	%load_annotations(Output, BtaAnn,_AnnSigs, BtaFil,_FilSigs, BtaState),	
	%% save in cache and return	
	%assert(bta_cache(ann(Ann,Fils), ann(BtaAnn,BtaFil))).
	
safety_check_configuration(ann(Ann,Fil),ann(BtaAnn,BtaFil),State,Filename,Norm,Output,FilterProp) :-	
	%current_data(state(State)),
	%current_data(norm(Norm)),
	%current_data(tmpfile(Output)),
	%current_data(filename(Filename)),	
	save_configuration_to_tmpfile(ann(Ann,Fil), Filename, State, PlFile, AnnFile),
	((FilterProp == true) ->
	    run_filterprop(PlFile, Output, AnnFile)
	;	    
	    run_bta(PlFile, Norm, Output, AnnFile)
	),
	load_annotations(Output, BtaAnn,_AnnSigs, BtaFil,_FilSigs, _BtaState).

	
	

	
	

	
	

%%% What do we do for each mutation?
%     Check CACHE if its been seen before then use old answer
%            What to cache?  Ann, Filter, Result:(Time, Size, Iter, SpecTime)
%     specialise *produced* program
%     benchmark program
%     save answer information
%     update cache
run_one(AnnFils) :-
	cache(AnnFils,_),
	!.

run_one(AnnFils) :-
	waiting(AnnFils),!.

% not in cache so we must re test
run_one(ann(BtaAnn,BtaFils)) :-
	
	inc_hit_count(spec_and_bench,V),
	
	portray_clause(user, hit(V)),
	%% linda testing
	assert(waiting(ann(BtaAnn,BtaFils))),
	out(conf(BtaAnn,BtaFils)).
	
	%in(conf_result(Ann,Fil,Result)),
	%assert(cache(ann(Ann,Fil), Result)).

        %old version before linda
        %current_data(annfile(AnnFile)),
	%current_data(filename(Filename)),
	%current_data(iter(BenchTimes)),
	%current_data(query(Query)),
        %current_data(timeout(Timeout)),

	%test_configuration(AnnFile, BtaAnn,BtaFils,BtaState,Filename,Query,BenchTimes,Timeout,Result),	
	%assert(cache(ann(BtaAnn,BtaFils), Result)).					

save_configuration_to_tmpfile(ann(Ann,Fil), Filename, State,PlFile, AnnFile) :-
	tmpnam(TMP),
	atom_concat(TMP,'.pl', PlFile),
	format_to_chars("cp ../~w ~w",[Filename, PlFile],Chars),
	name(CopyCmd, Chars), system(CopyCmd),	
	atom_concat(TMP,'.pl.ann', AnnFile),	
	save_annotations(AnnFile, Ann, Fil, State,"").
	
			      
test_configuration(_AnnFile, Ann,Fil, State, Filename, Query, BenchTimes,Timeout, Result) :-
	save_configuration_to_tmpfile(ann(Ann,Fil), Filename, State, TmpPL, _),		
	spec_and_bench(TmpPL, Query, BenchTimes, Timeout, Result).

	
	%save_annotations(AnnFile, Ann,Fil, State),
	%spec_and_bench(Filename, Query, BenchTimes,Timeout, Result).



inc_hit_count(Counter,X1) :-
	Call =.. [Counter, X],
	(current_data(Call) ->
	    retract(current_data(Call))
	;
	    X =0
	),
	X1 is X +1,
	Call1 =..[Counter, X1],	
	assert(current_data(Call1)).


%% Test A list of mutations
test_l([]).
test_l([AnnFils|As]) :-	
	%sleep(0.5),  %% I think this is still causing errors?
	%% get safe version first
	%get_btad_ann(AnnFils, BtaAnnFils),	
	%run_one(BtaAnnFils,_Result),
	run_one(AnnFils),

	test_l(As).



get_linda_results :-
	waiting(_),
	%retract(current_data(waiting)),
	in(conf_result(Ann,Fil,Result)),
	retract(waiting(ann(Ann,Fil))),

	%portray_clause(user, result(Result)),
	assert(cache(ann(Ann,Fil), Result)),
	get_linda_results.
get_linda_results.


	

	

	
	

/*
run_trial(Filename, AnnFile, Query, BenchTimes) :-
	bench_original(Filename, Query, BenchTimes, Orig),
	spec_and_bench(Filename, Query, BenchTimes, Spec),
	portray_clause(Orig),
	portray_clause(Spec).
*/

:- use_module(library(charsio)).
:- dynamic counter/1.
get_spec_counter(X) :-
	counter(X),
	!,
	X1 is X +1,
	retractall(counter(_)),
	assert(counter(X1)).
get_spec_counter(1).


%bench_original(Filename, Query, BenchTimes, Result) :-
%	run_bench(Filename, Query, BenchTimes, Result).


spec_and_bench(Filename, Query, BenchTimes, Timeout,Result) :-
	%current_data(timeout(Timeout)),
	(run_spec(Filename, Query, SpecFile, Timeout) ->
	    %get_spec_counter(Counter),
	    %format_to_chars("cp ../~w specfiles/~w.pl", [SpecFile, Counter],Chars),
	    %name(Cmd, Chars),
	    %system(Cmd),
	    run_bench(SpecFile, Query, BenchTimes, Result)
	;	    
	    Result = bench(error_specialising, fail,fail)
	).

	


	
	