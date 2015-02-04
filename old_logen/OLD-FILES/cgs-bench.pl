/* ------------------------------------------- *
	BENCHMARK
 * ------------------------------------------- */

:- module(bench,[benchmark_specialised_program/1]).

:- use_module(cgs).
:- use_module('cgs-tools').
/*:- consult('cgs-tools').*/

call_10(G) :- tools:G,fail.
call_10(G) :- tools:G,fail.
call_10(G) :- tools:G,fail.
call_10(G) :- tools:G,fail.
call_10(G) :- tools:G,fail.
call_10(G) :- tools:G,fail.
call_10(G) :- tools:G,fail.
call_10(G) :- tools:G,fail.
call_10(G) :- tools:G,fail.
call_10(G) :- tools:G,fail.

call_loop(G,_N) :- call_10(G),fail.
call_loop(G,_N) :- call_10(G),fail.
call_loop(G,_N) :- call_10(G),fail.
call_loop(G,_N) :- call_10(G),fail.
call_loop(G,_N) :- call_10(G),fail.
call_loop(G,_N) :- call_10(G),fail.
call_loop(G,_N) :- call_10(G),fail.
call_loop(G,_N) :- call_10(G),fail.
call_loop(G,_N) :- call_10(G),fail.
call_loop(G,_N) :- call_10(G),fail.
call_loop(G,N) :-
	((N>1) -> (N1 is N - 1, call_loop(G,N1)) ; (true)).

benchmark_no_loop(G) :- 
        number_of_times(No1),
	((No1 > 0)
	 -> (No=No1) ; (No=1)
	),
	please(tw,on),
        time(bench:call_loop(G,No),Time),!,
        time(bench:call_loop(true,No),Time1),
        print('           '),write(Time), write(' ms'), nl,
        print('Overhead:  '),write(Time1), write(' ms'), nl,
	Timed is Time - Time1,
        print('Difference:'),write(Timed), write(' ms'), nl,
        please(tw,off),
        nl .

benchmark_specialised_program(DirFile) :-
	cgs:last_specialisation_info(SpecFile,Call,FilteredCall),
	program_to_be_specialised(F),
      string_concatenate(DirFile,F,Filename),
      string_concatenate(DirFile,SpecFile,FullSpecFile),
  	string_concatenate(Filename,'.pl',OrigFile),
	print('consulting: '),print(FullSpecFile),nl,
	on_exception(existence_error(_,_,_,_,_),
	 reconsult_without_redefine_warning(FullSpecFile),
	 (print('* unable to load specialised program *'),nl)),
	print('consulting: '),print(OrigFile),nl,
	on_exception(existence_error(_,_,_,_,_),
	 reconsult_without_redefine_warning(OrigFile),
	 (print('* unable to load original program *'),nl)),
	print('benchmarking:'),print(Call),nl,
	benchmark_no_loop(Call),
	print('benchmarking:'),print(FilteredCall),nl,
	benchmark_no_loop(FilteredCall).

