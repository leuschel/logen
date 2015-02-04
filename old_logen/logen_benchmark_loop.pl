:- module('logen_benchmark_loop', [run_benchmark_loop/4]).
%%%run_benchmark_spec_only(File,Call,Times,TT).

:- use_module('logen_codesize.pl',[get_code_size/2]).



run_benchmark_loop(File,Call,Times,TT) :-
	(split_components_private(Dir, Base, '.spec', File) ->
	    atom_concat(Base, '.spec', Module),
	    
	    atom_concat(Base, '.memo', Memo),
	    atom_concat(Dir, Memo, MemoFile),
	    ensure_loaded(MemoFile),
	    table(Call, SpecCall, [crossmodule]),
	    
	    %Goal = Module:Call,
	    Goal = Module:SpecCall,
	    portray_clause(user_error, calling(Goal)),
	    portray_clause(user_error, using_spec_file)
	;
	    Goal = Call,
	    portray_clause(user_error, calling(Goal)),
	    portray_clause(user_error, using_pl_file)
	),

	(Times > 0 -> (
			TmpFile = 'tmp_benchmark.pl', 
			open(TmpFile,write, Out),
			write_benchmark(Out,File, Goal, Times),
				%write_benchmark(Out,File, findall(_,Goal,_), Times),
			close(Out),
			ensure_loaded(TmpFile),			
			catch((bench_call(TT), OK=true),benchmarking_failed, bench_failed(OK))
		      )
	;
	    (
	      OK = true, TT = 0
	    )
	),
	(OK==true ->
	    (
	      get_code_size(File,CodeSize),	
	      format("~NBenchmark completed in ~wms for ~w runs~n", [TT, Times]),	
	      format(user, "Compiled Code Size: ~d bytes~n", [CodeSize]),
	      bench_output(bench(TT, Times, CodeSize))
	     )
	;
	    true
	),
	!.





run_benchmark_loop(File, Call ,Times, TT) :-
	bench_output(bench(invalid_query,fail, fail)).

bench_failed(fail) :-
	bench_output(bench(query_failed, fail,fail)).

bench_output(Output) :-
	open('bench_output.tmp', write, OUTS),
	portray_clause(OUTS, Output),
	close(OUTS).
	    




	

write_benchmark(Stream, File, Call,Times) :-
	portray_clause(Stream, (:-module('tmp_benchmark', [bench_call/1]))),
	portray_clause(Stream, repeat_bench(0)),
	copy_term(Call,NCall1),
	Repeat is Times,
	portray_clause(Stream, (call_once :- NCall1,!)),
	
	portray_clause(Stream, (call_once :- throw(benchmarking_failed))),	
	portray_clause(Stream, (repeat(_N))),
	portray_clause(Stream, (repeat(N) :- (
					       	N > 1,
					       N1 is N - 1,
					       repeat(N1)
					     ))),
	portray_clause(Stream, (dobench(Count) :- (
						    repeat(Count),
						    %NCall1,
						    call_once,
						    fail))),
	portray_clause(Stream, (dobench(_Count))),
	portray_clause(Stream, (bench_call(Time):- (
						ensure_loaded(File),
						statistics(runtime, [T,_]),
						%repeat_bench(Repeat),
						 dobench(Repeat),    
						statistics(runtime, [T1,_]),
						Time is T1- T
					      ))).

split_components_private(Dir, Base, Ext, FullPath) :-
	use_module(library(lists)),
        name(FullPath, SFullPath),
        append(SDir, SBaseExt, SFullPath),
        \+ memberchk(47, SBaseExt),    % 47 = '/'
        \+ memberchk(92, SBaseExt),    % 92 = '\'  for windows
        !,
        (memberchk(46, SBaseExt) ->    % 46 = '.'
            append(SBase, SExt, SBaseExt),
            SExt = [46 | SExtTail],
            \+ memberchk(46, SExtTail),    % 46 = '.'
            !
        ;
            SBase = SBaseExt,
            SExt = []
        ),
        name(Dir, SDir),
        name(Base, SBase),
        name(Ext, SExt).

