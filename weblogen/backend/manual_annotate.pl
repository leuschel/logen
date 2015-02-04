:- use_module('../../tools/error_manager.pl', [add_error/3, add_message/4,
             count_errors_occured/1,set_verbosity_level/2, add_exception/4]).
:- use_module('../../annotation/annsaver.pl').
:- use_module('../../annotation/annmodifier.pl').
:- use_module('../../annmeta.pl', [higher_order_gui_info/3]).

main(Args) :-
    catch(main1(Args),E,
		  (add_exception(ciao_entry, "Uncaught exception: ",[],E), halt(1))).

check_annotations(Anns) :-
	findall(A, (higher_order_gui_info(_, A, _), nonvar(A)), Unknowns),
	check_unknowns(Unknowns, Anns).
check_annotations(Anns) :-
	write(user_error, 'Annotations list contains unknowns!'), nl(user_error),
	write(user_error, Anns),
	halt(1).

check_unknowns(_, []).
check_unknowns(Unknowns, [A|Anns]) :-
	\+member(A, Unknowns), !,
	check_unknowns(Unknowns, Anns).

main1(Args) :-
	get_options(Args, Opts, [Plfile,NewAnnfile|_AX]), !,
	add_message(ciao_entry, 2, "Options: ~w", Opts),
	(member(help, Opts) -> fail; true),

	read(Annotations),
	check_annotations(Annotations),
	read_filters(Filters),
	portray_clause(user_error, Annotations),
	portray_clause(user_error, plfile(Plfile)),
	portray_clause(user_error, newannfile(NewAnnfile)),
	open(NewAnnfile, write, Out),
	load_file_with_annlist(Plfile, Annotations, Out), !,
	write_filters(Out, Filters),
	close(Out).

main1(_) :- print_usage.

read_filters([F|Filters]) :-
	read(F),
	F \== end_of_file, !,
	read_filters(Filters).
read_filters([]).

get_options([],[],[]).
get_options([X|T],Options,OtherArgs) :-
	(recognised_option(X,Opt,Values,_) ->
		( append(Values, Rest, T), RT = Rest,  /* extract args and insert into Opt by shared var*/
		  Options = [Opt|OT], OtherArgs = AT )
	;
	( Options = OT,     OtherArgs = [X|AT],
	  RT = T )
	),
	get_options(RT,OT,AT).

usage('Usage: manual_annotate [Options] File.pl File.pl.ann').
recognised_option('--help',help,[],'Prints this message').

print_usage :-
	usage(Msg),
	format(user_error, "~w~nPossible Options are :~n", [Msg]),
	print_options.

print_options :-
	recognised_option(Opt,_,Args,Msg),
	format(user_error, "      ~w", [Opt]),
	print_option_args(Args,1),
	format(user_error, ": ~w~n", [Msg]),
	fail.

print_options.

print_option_args([],_).
print_option_args([_|T],N) :- format(user_error, " ARG~w~n", [N]),
							  N1 is N+1, print_option_args(T,N1).

