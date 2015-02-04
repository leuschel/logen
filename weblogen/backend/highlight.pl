:- use_module('../../tools/error_manager.pl',
		[add_error/3, add_message/4, add_exception/4, count_errors_occured/1,
		 set_verbosity_level/2]).

:- use_module(pillow/pillow).
:- use_module('prolog_to_xml_swi.pl').

main(Args) :-
	catch(main1(Args),E,
	     (add_exception(ciao_entry, "Uncaught exception:~n",[],E),
	      halt(1))).

main1(Args) :-
	get_options(Args, Opts, [Plfile|_AX]), !,
	add_message(ciao_entry, 2, "Options: ~w", Opts),
	(member(help, Opts) -> fail; true),

	output_xml_decl('prologtohtml.xsl'),
	output_html([begin(article), nl]),
	output_file_as_html_no_ann(Plfile),
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

usage('Usage: highlight [Options] File.pl').
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

