:- use_module('../../tools/error_manager.pl',
			[add_error/3, add_message/4,add_exception/4,
             count_errors_occured/1,set_verbosity_level/2]).

:- use_module(pillow/pillow).
:- use_module(prolog_to_xml_swi).

test :- main1(['--correctable_filter_replace', append, 3, 'append(dynamic, dynamic, dynamic)', 'append.pl', 'append.pl.ann']).

main(Args) :-
	catch(main1(Args),E,
	     (add_exception(ciao_entry, "Uncaught exception: ",[],E),
	      halt(1))).
	
process_opts2([], []).
process_opts2([O|Opts], [O|Nopts]) :-
	is_list(O), process_opts2(Opts, Nopts).
process_opts2([O|Opts], [N|Nopts]) :-
	term_to_atom(N,O), process_opts2(Opts, Nopts).

process_opts([], []).
process_opts([O|Opts], [N|NOpts]) :-
	O =.. [A|As],
	process_opts2(As, As2),
	N =.. [A|As2],
	process_opts(Opts, NOpts).

main1(Args) :-
	get_options(Args, Opts, [Plfile, Annfile|_AX]), !,
	process_opts(Opts, NOpts),
	add_message(ciao_entry, 2, "Options: ~w", NOpts),
	(member(help, NOpts) -> fail; true),

	output_xml_decl('prologtohtml.xsl'),
	output_html([begin(article), nl]),
	output_file_as_html(Plfile, Annfile, NOpts),
	output_html([end(article), nl]),
	halt.

main1(_) :- print_usage.

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

usage('Usage: show_annotated [Options] File.pl File.pl.ann').
recognised_option('--help',help,[],'Prints this message').
recognised_option('--extra_info',extra(Ann, Pred, Arity, Clause, Path),[Ann, Pred, Arity, Clause, Path],'Add an annotation').
recognised_option('--correctable',fixable(Ann, Pred, Arity, Clause, Path, Fix),[Ann, Pred, Arity, Clause, Path, Fix],'Add a fixable annotation').
recognised_option('--correct_hide_nf',fix_hide_nf(Pred, Arity, Clause, Path),[Pred, Arity, Clause, Path],'Mark a path as being fixable with hide_nf').
recognised_option('--correctable_filter',fixable_filter(Name, Arity, Arg),[Name, Arity, Arg],'Add a fixable filter').
recognised_option('--correctable_filter_replace',replace_filter(Name, Arity, Replace),[Name, Arity, Replace],'Add a fixable filter (by replacement)').
recognised_option('--mark_filter',filter(Name, Arity),[Name,Arity],'Mark a filter as problematic').

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
print_option_args([_|T],N) :- format(user_error, " ARG~w", [N]),
							  N1 is N+1, print_option_args(T,N1).
