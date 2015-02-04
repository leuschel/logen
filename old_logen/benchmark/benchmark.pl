/* Created by Pylogen */

stack_call(X,[]) :- call(X).

stack_call(X,[0|_]).
stack_call(X,[N|Ns]) :-
	N > 0,
	stack_call(X,Ns),
	N1 is N -1,
	stack_call(X,[N1|Ns]).


calc_total([N], N).
calc_total([N|Ns], N1) :-
	calc_total(Ns,NTotal),
	N1 is N * NTotal.


run_benchmark(File, Call, Times, TT) :-
	ensure_loaded(File),
	statistics(runtime,[_,T1]),
	stack_call(Call,Times),
	statistics(runtime,[_,T2]),
	calc_total(Times, TotalRuns),
	TT is T2 - T1,
	format("~NBenchmark completed in ~wms for ~w runs~n", [TT, TotalRuns]).

benchmark_spec_only(File,Call,Times,T1):-
    split_components(Dir,Base,Ext,File),
	atom_concat(BaseName, '.pl', File),
	atom_concat(BaseName, '.spec', SpecFile),
	atom_concat(Base,'.spec', SpecModule),
	run_benchmark(SpecFile, SpecModule:Call, Times,T1).


	
benchmark_specialised_file(File, Call, Times) :-
	%split_components(Dir,Base,Ext,File),
	split_components(Dir,Base,Ext,File),
	atom_concat(BaseName, '.pl', File),
	atom_concat(BaseName, '.spec', SpecFile),
	atom_concat(Base,'.spec', SpecModule),
	run_benchmark(SpecFile, SpecModule:Call, Times,T1),
	run_benchmark(File, Call, Times,T2),
	calc_total(Times,TotalRuns),
	format("Query ~w ~w times:~n", [Call, TotalRuns]),
	Diff is T2 - T1,
	format("Original Program: ~wms ~n", [T2]),
	format("Specialised Program: ~wms ~n", [T1]),
	format("    Difference is ~wms~n", [Diff]).
	
split_components(Dir, Base, Ext, FullPath) :-
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
	