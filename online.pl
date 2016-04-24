% (c) 1996-2016 Michael Leuschel
% see https://github.com/leuschel/logen for more details

:- if(current_prolog_flag(dialect, ciao)).
:- use_module(library(strings),[get_line/2]).
:- endif.

%:- use_module(runtime_checks).

:- dynamic ignore_homeo_warnings/0.

confirm_user(Problem, [Call,_History],c,PP) :-
    ignore_homeo_warnings,!,
    write(user_error,Problem),nl(user_error),
    (Problem=online_unsafe_unfold(_) -> UNSAFE=unsafe_unfold ; UNSAFE=unsafe_memo),
    print_program_point_error('<| HOMEMORPHIC WARNING |> ',warning,UNSAFE,[call(Call)],PP).
confirm_user(online_unsafe_unfold(Once), [Call,History],Return,PP) :-
    copy_term([Call,History],FormatArgList),
    numbervars(FormatArgList,0,_),
	format(user_error, "~n<| HOMEOMORPHIC WARNING |> : UNFOLDING ~w,~nHistory: ~w~n",FormatArgList),
	FormatArgList = [RC,RH], convert_history(RH,ConvH),
    print_program_point_error('A predicate is possibly being unfolded infinitely often. You may want to annotate the call as memo.',warning,unsafe_unfold,
                                [call(RC),history(ConvH)],correctann([annotation(memo)],PP)),
	print_diagnostic(Call,History,_Res),
	(Once==yes
	  ->  abort_specialization
	  ;   (get_user_key(Return),
			(Return=="c" -> true
				 ; (Return=="F"
					 -> (format(user_error,"*** WARNING: User forced a branch to fail! ***~n*** Generated code is probably incorrect! ***~n~n",[]),
						 add_gx_error(unfolding_stopped),
						 fail)
					 ; (Return=="C"
						 -> assert(ignore_homeo_warnings)
						 ; abort_specialization)))
	      )
	).

confirm_user(online_unsafe_memo(Once), [Call,History],Return,PP) :-
    copy_term([Call,History],FormatArgList),
    numbervars(FormatArgList,0,_),
	format(user_error, "~n<| HOMEOMORPHIC WARNING |> : MEMO Atom ~w,~nHistory: ~w~n",FormatArgList),
	FormatArgList = [RC,RH],convert_history(RH,ConvH),
	print_diagnostic(Call,History,Res),
	functor(RC,RCPred,RCArity),
    print_program_point_error('A predicate is possibly being memoised infinitely often. You may want to make the filter declaration for that predicate more dynamic.',
                              warning,unsafe_memo,[call(RC),history(ConvH)],
                              correctfiltpp(Res,filter(RCPred,RCArity),PP)),
	(Once==yes
	  ->  abort_specialization
	  ;   (get_user_key(Return),
			(Return=="c" -> true
				 ; (Return=="F"
					 -> (format(user_error,"*** WARNING: User prevented a memo entry being added! ***~n*** Generated code is probably incorrect! ***~n~n",[]),
						 add_gx_error(memo_entry_not_added),
						 fail)
					 ; (Return=="C"
						 -> assert(ignore_homeo_warnings)
						 ;  abort_specialization)))
	      )
	).
	
get_user_key(Return) :-
   format(user_error,"~nType 'c' to continue unfold, 'F' to fail branch, 'C' to continue without further intervention, anything else to abort:~n",[]),
    get_line(user,Return).


   
convert_history([],[]).
convert_history([H|T],[call(H)|CT]) :- convert_history(T,CT).

generalise_online_mixed(Filters, Call, MixedGenCall, ParentID) :-
	Call =.. [F|Args],
	gen_filters_list([Filters],Args, GArgs, _,OnlineArgs,OnlineMap),
	OnlineCall =.. [F|OnlineArgs],
	HisMapFrom =.. [F|GArgs], HisMapTo =.. [F|OnlineMap],
	get_history_mixed(ParentID, History, HisMapFrom, HisMapTo),
	generalise_online_history(OnlineCall, GenCall, History),
	%print(generalise_online_history(OnlineCall, GenCall, History)),nl,
	GenCall =.. [F|GenOnlineArgs],
	GenOnlineArgs = OnlineMap,
	MixedGenCall =.. [F| GArgs],
	add_message(generalise_online_mixed,2,"Mixed Generalisation: '~w'.~n",[generalise_online_mixed(Filters, Call, MixedGenCall, ParentID)]).
	
	
/*	
generalise_online_mixed(Filters, Call, GenCall, ParentID) :-
	generalise_online(Call, GenCall, ParentID),
	portray_clause(generalise_online_mixed(Filters, Call, GenCall, ParentID)).
*/

			 
get_history_mixed(entry, [],_,_) :- !.
get_history_mixed(ID, HistoryList,MapFrom,MapTo) :-
	memo_table(_,History, ID, MEMODATA),
	copy_term((MapFrom,MapTo), (CMapFrom,CMapTo)),
	(History = CMapFrom -> 
	              HistoryList = [CMapTo|Hs]
	;
	              HistoryList = Hs
	),
	get_memodata_id(MEMODATA, ParentID),
	get_history_mixed(ParentID, Hs,MapFrom,MapTo).


%%% homemorphic,[homeomorphic_embedded/2,homeomorphic_embedded_call/2
%:- use_module('../homeomorphic.pl').  %% <---- IMPROVE !!!!
%:- use_module('../msg.pl').  %% <---- IMPROVE !!!!


generalise_online(Call, GenCall, ParentID) :-
	%portray_clause(generalise_online(Call, ParentID)),
	get_history(ParentID, History),
	%portray_clause(history(History)),
	mnf(generalise_online_history(Call, GenCall, History)).
	%generalise_call([[dynamic,dynamic,dynamic]], Call,GenCall).


generalise_online_history(Call, Call, []).
generalise_online_history(Call, GCall, [H|Hs]) :-
	(homeomorphic_embedded_call(H,Call) ->
	    msg(H, Call, MGU),
	    generalise_online_history(MGU, GCall, Hs)
	;
	    generalise_online_history(Call, GCall, Hs)
	).

%% checks the history for whistle in memo table
%is_safe_to_add_memo_entry(+Call, +ParentID,-History) :-
is_not_safe_to_add_memo_entry(Call, ParentID,History) :-
	get_history(ParentID,History),
	!,
	\+ (is_safe_to_add_l(Call, History)).

is_safe_to_add_l(_Call, []).
is_safe_to_add_l(Call, [H|Hs]) :-
	(homeomorphic_embedded_call(H,Call)
	 -> (add_message(generalise_online_mixed,2,"Homeomorphic Embedding: ~w |> ~w!~n",[Call,H]),
	     fail)
	  ;	is_safe_to_add_l(Call,Hs)
	).


/* print information about a growing call*/
print_diagnostic(Call, History,Res) :-
  (get_growing_arguments(Call,History,GrowingArgNrs,GCall,GAnc)
    -> ( 
	     format(user_error,"~n<| Growing Argument Positions |> : ~w~n",[GrowingArgNrs]),
	     format(user_error,"   Call     = ~w~n   Ancestor = ~w~n",[GCall,GAnc])
       )
   ;  (add_error(print_diagnostic,"~n~w failed.~n",[get_growing_arguments(Call, History)]),
       GrowingArgNrs=[])
  ),
  get_filter_correction(GrowingArgNrs,Res).
  
/* refine later: take msg and make more detailed recommendation about filter */
get_filter_correction([],[]).
get_filter_correction([N|T],[make_dynamic(N)|CT]) :- get_filter_correction(T,CT).
  

get_growing_arguments(Call,[H|Hs],Res,GCall,GAnc) :-
	(homeomorphic_embedded_call(H,Call)
	 -> (Call=..[F|CArgs], H=..[F|HArgs],
	     get_growing_args(CArgs,HArgs,1,Res,CGA,HGA),
	     GCall=..[F|CGA], GAnc=..[F|HGA]
	    )
	  ;	get_growing_arguments(Call,Hs,Res,GCall,GAnc)
	).
   

get_growing_args([],[],_,[],[],[]).
get_growing_args([Call|TC],[H|HB],Nr,Res,ResC,ResH) :-
    (\+ variant_of(H,Call)
      -> (Res = [Nr|ResT], ResC=[Call|ResCT], ResH=[H|ResHT])
      ;  (Res=ResT, ResC=['*'|ResCT], ResH=['*'|ResHT])
     ),
     N1 is Nr+1,
     get_growing_args(TC,HB,N1,ResT,ResCT,ResHT).
	
	
			 
get_history(entry, []) :- !.
get_history(ID, [History|Hs]) :-
	memo_table(_,History, ID, MEMODATA),
	get_memodata_id(MEMODATA, ParentID),
	get_history(ParentID, Hs).


%........
	
:- if(current_prolog_flag(dialect, ciao)).
:- use_module(library(terms_vars),[varset/2]). /* term_variables in SICStus */
:- else.
varset(A,B) :- term_variables(A,B).
:- endif.

filter_online(_Call, GenCall, ResCall) :-
	varset(GenCall, Variables),
	GenCall =.. [F|_Args],
	gensym(F,NewF),
	ResCall =.. [NewF|Variables].


is_safecall(_Call, []).
is_safecall(Call, [H|Hs]) :-
	(homeomorphic_embedded_call(H,Call) ->
	    fail
	;
	    is_safecall(Call, Hs)
	).

