/* ----------- */
/*  C O G E N  */
/* ----------- */

:- module(cogen, 
	    [

	     cogen/0,
	     filter_cons/4,
	     flush_cogen/0,
	     flush_cogen/1,
%	     memo_clause/1,
	     make_disjunction/3,
	     reset_cogen/0,
%	     typedef/2,
%	     set_generate_debugging_gx/1,
%	     semiwhen/1,
	     inspect_residual/2,
	     remove_ext/3,
	     module_name_from_filename/2
	    
	     
	     ]).

:- ensure_loaded('sicstus_term.pl').

%:- dynamic body/3.


%%% Module Imports

%% System Modules
:- use_module(library(lists)).

%% Logen Modules
:- use_module('pp.pl').
:- use_module('builtin_db.pl').
:- use_module('annfile.pl').
:- use_module('cogen-tools.pl').
:- use_module('flags.pl').
:- use_module('ann_db.pl',[see_through_primitive/4, remove_annotation_body/2]).
:- use_module('logen_messages.pl').


:- use_module('logen_filter.pl').


:- use_module(library(terms)).
:- use_module('prob/logen_preferences.pl').

ciao((:- set_prolog_flag(multi_arity_warnings, off))).


reset_cogen :- retractall(is_clp(_)), reset_pp.

cogen :-
  (check_clp ->
      assert(is_clp(on))
  ;
      assert(is_clp(off))
  ),
  %print_debug_message('Generating _spec Clauses'),
  %findall(C,spec_clause(C),Clauses1),
  print_debug_message('Generating _unfold Clauses'),
  findall(C,unfold_clause(C),Clauses2),
  print_debug_message('Generating _memo Clauses'),
  findall(C,request_clause(C),Clauses3),
  print_debug_message('Generating header code'),
  findall(C,query_header_clauses(C),Clauses4),

  
  print_debug_message('Printing Dynamic Predicates'),
  findall(clause((:- dynamic(PredSpec)),true),dynamic_pred(PredSpec),Clauses5),
  print_debug_message('Printing Dynamic Predicates Specs'),
  findall(clause(dynamic_gx(PredSpec),true),dynamic_pred(PredSpec),ClausesDynGx),
  

  print_debug_message('Printing Ops'),
  findall(clause((:- op(A,B,C)),true),op_def(A,B,C),ClausesOps),
  
  print_debug_message('Printing User Types'),
  findall(clause(user_type(A,B),true),user_type(A,B),ClausesTypes),
  print_debug_message('Printing Op Defs'),
  findall(clause(op_def_gx(A,B,C),true),op_def(A,B,C),ClausesOpDefs),

  print_debug_message('Printing use libraries'),
  findall(decl(use_module(Lib)),gx_use_module(Lib),ClausesLibs1),
  findall(decl(ensure_loaded(Lib)),gx_ensure_loaded(Lib),ClausesLibs2),
  append(ClausesLibs1,ClausesLibs2,ClausesLibs),
  findall(decl(use_module(Lib,List)),gx_use_module(Lib,List),ClausesLibsList),

  print_debug_message('Printing table declarations'),
  findall(clause(table_gx(Call),true),table(Call),ClausesTable),  
  
  pp(ClausesOps),
  

  ((ClausesTable=[]) -> pp([clause(table_gx(_),fail)]) ; pp(ClausesTable)),
  ((ClausesTypes=[]) -> pp([clause(user_type(_,_),fail)]) ; pp(ClausesTypes)),
  ((ClausesOpDefs=[]) -> pp([clause(op_def_gx(_,_,_),fail)]) ; pp(ClausesOpDefs)),
  ((ClausesDynGx=[]) -> pp([clause(dynamic_gx(_),fail)]) ; pp(ClausesDynGx)),
  pp(ClausesLibs),
  pp(ClausesLibsList),


  pp(Clauses4),
  pp(Clauses5),
  pp(Clauses3),
  %pp(Clauses1),
  pp(Clauses2).



%% Print the gx file to the screen (default t modulename gx)
flush_cogen :- flush_cogen('gx').

%%% flush cogen from printer and flag we are in gx mode
flush_cogen(Module) :-
  print_header(Module),
  set_logen_flag(pppp(gx)),
  flush_pp,
  unset_logen_flag(pppp(gx)).

%% flag to say whether clp is on
:- dynamic is_clp/1.
check_clp :-
	findall(Types, filter(_,Types), AllTypes),
	is_clp_nest_list(AllTypes).

is_clp_nest_list([List|Tail]) :-
	(is_clp_list(List) ->
	    true
	;
	    is_clp_nest_list(Tail)
	).	
is_clp_list([clp|_   ]) :- !.
is_clp_list([clp(_,_)|_   ]) :- !.
is_clp_list([_|Tail]) :- is_clp_list(Tail).

generate_debugging_gx(N) :- get_preference(gx_debug_mode,C), C>=N.
   %  get_preference is defined in prob/logen_preferences

% ____________________________________________________________
%
% generate extra code that will be put in the header of the gx file.
%
query_header_clauses(clause(logen_residual_clauses((:-module(ResidualModule, []))), true)) :-
        % the header of the .spec module
        this_module_name(ThisModule),
        string_concatenate(ThisModule, '.spec', ResidualModule).
query_header_clauses(clause(logen_residual_clauses((:-ensure_loaded(LoadedFile))), true)) :-
        % this logen_residual_clauses entry will cause a :-ensure_loaded
        % to be written to the .spec file.
        gx_ensure_loaded(LoadedFile).
query_header_clauses(clause(logen_residual_clauses((:-use_module(ResidualModule))), true)) :-
        % this logen_residual_clauses entry will cause a :-use_module
        % to be written to the .spec file.

/*        gx_use_module(UsingModule), */
        annfile:residual_pred((:-use_module(UsingModule))),
        ((UsingModule \= library(_),get_preference(modular_spec_mode,true))
         -> (name(UsingModule, SUsingModule),
              (append(SBaseName, ".pl", SUsingModule) -> true
                       ; SBaseName = SUsingModule ),
               name(BaseName, SBaseName),string_concatenate(BaseName, '.spec', ResidualModule)
            )
         ;  ResidualModule=UsingModule
        ).
query_header_clauses(clause(logen_residual_clauses((:-use_module(ResidualModule,List))), true)) :-
        % this logen_residual_clauses entry will cause a :-use_module
        % to be written to the .spec file.
        gx_use_module(UsingModule,List),
        ((UsingModule \= library(_),get_preference(modular_spec_mode,true))
         -> (name(UsingModule, SUsingModule),
              (append(SBaseName, ".pl", SUsingModule) -> true
                       ; SBaseName = SUsingModule ),
               name(BaseName, SBaseName),string_concatenate(BaseName, '.spec', ResidualModule)
            )
         ;  ResidualModule=UsingModule
        ).

query_header_clauses(clause(logen_residual_clauses((Head:-NewBody)),true)) :-
        % this logen_residual_clauses entry will cause some user-defined
        % residual clause to be written to the .spec file.
	residual_pred(Head),	
	print_debug_message('Generating Residual for'),
	print_debug_message(Head),
	ann_clause(_ID,Head,Body),
	ann_db:remove_annotation_body(Body,NewBody).
query_header_clauses(clause(logen_residual_clauses((:-op(Prio,Type,Symb))),true)) :-
        % this logen_residual_clauses entry will cause some user-defined
        % residual clause to be written to the .spec file.
	op_def(Prio,Type,Symb),	
	print_debug_message('Generating Residual Operator Definition for'),
	print_debug_message(Symb).
	
query_header_clauses(clause(':_request'(Module, Call, _Requestor, ResCall),
                            moduledriver:request_external_pattern(Module, Call,
                                           ResCall, module(ThisModule)))) :-
        % this one cannot easily be explained, just see in the .gx
        % file how a ':_request' predicate might be useful for cross-module
        % requests :-)
        logen_preferences:get_preference(modular_spec_mode,true),
        this_module_name(ThisModule).


%%% remove_annotation_body(Pattern, Result) :-
%%% 	logen_annotation_structure(Pattern, In, Out, Result),
%%% 	!,
%%% 	logen_map_remove(In,Out)
%%% 	.
%%% remove_annotation_body(Pattern, error) :-
%%% 	print(unknown_pattern(Pattern)).

%%% logen_map_remove([],[]).
%%% logen_map_remove([A|T],[B|T1]) :- remove_annotation_body(A,B),logen_map_remove(T,T1).	

	


request_clause(clause(Head, Body)) :-
	residual(Call),
	add_two_arguments("_request", Call, Requestor, ResCall,Head),
	findall(Fs,filter(Call, Fs), AllFilterTypes),
	this_module_name(This),
	Body = (	 
            (memoizer:find_pattern(This, Call, ResCall,Requestor) ->
		true
	    ;
		GeneraliseStep,
		memoizer:insert_pattern_with_filter_types(This, GCall, AllFilterTypes, Requestor, ResCall),
		GCall = Call,
		DebugCode
	    )
	),
	(generate_debugging_gx(1) 
	    ->  (DebugCode = 'logen_messages':print_message(request(Call,ResCall)))
	                            ;   (DebugCode=true)),
	(cogen_can_generalise(Call, GCall) ->
	    GeneraliseStep = true
	
	;
	    GeneraliseStep = logen_filter:generalise_with_filter_types(Call,AllFilterTypes,GCall)),
	    
	Requestor = '$VAR'('Requestor'),
	ResCall = '$VAR'('ResidualCall')
	.





unfold_clause(clause(ResCall,FlatResBody)) :-
  ann_predicate(Pred),
  print_debug_message(generating_unfolder(Pred)),
  get_predicate_info(Call,Pred),
  add_extra_argument("_u",Call,FlatVars,ResCall),
  ((generate_debugging_gx(2),
    FlatResBody = ('logen_messages':print_indentation,
                   'logen_messages':print_short_msg(unfold(ResCall)),
                   'logen_messages':print_short_msg('\n'),
                   'logen_messages':increase_indentation,fail)
   )
  ;(ann_clause(_,Call,Body),
    print_debug_message(generating_clause(Call)),
    body(Body,ResBody,Vars),
    flatten(ResBody,FlatResBody),
    flatten(Vars,FlatVars)
   )
  ;(generate_debugging_gx(2),
    FlatResBody = ('logen_messages':decrease_indentation,
                   'logen_messages':print_indentation,
                   'logen_messages':print_short_msg(end_unfold(ResCall)),
                   'logen_messages':print_short_msg('\n'),fail)
   )
  ).

ann_predicate(Pred) :-
   setof(P,ann_predicate2(P),L),
   member(Pred,L).
ann_predicate2(Pred) :-
   ann_clause(_,Call,_),
   get_predicate_info(Call,Pred).

%body(Body,GX,Spec) :- print_message(body(Body,GX,Spec)),nl,fail.

body((G,GS),GRes,VRes) :-
  body(G,G1,V),
  filter_cons(G1,GS1,GRes,true),
  filter_cons(V,VS,VRes,true),
  body(GS,GS1,VS).

body(reswhen(ground(X),G2), (RG2,(ground(X) -> (Code=VS2) ; (Code = when(ground(X),VS2)))),
              Code) :- !, /* residual when - also handle nonvar,...*/
  body(G2,RG2,VS2).	
  
body(reswhen(C,G2), RG2, when(C,VS2)) :- /* residual when */
  body(G2,RG2,VS2).	
  
  
body(semiwhen(Cond,Call), GX,Res) :-
	%GX  = portray_clause(user_error, gx_semiwhen(Cond,Call)),
	body(Call,BGX,BRes),
	GX  = when(Cond, (Res=BRes,BGX)).

	%Res = portray_clause(user_error, res_semiwhen(Cond,Call)).
	

/*
	(generate_debugging_gx(1) ->
	    Debug = print_message(when_executing(when(Cond,BGX),res(Res=BRes)))
	;
	    Debug = true),
	body(Call,BGX,BRes),
%	GX = when(Cond, (Res = BRes,Debug, BGX)).
	GX = when(Cond, (Res = BRes,Debug, BGX)).
*/


body(time_out(C,T,Res), RG2, time_out(VS2,T,Res)) :- !, /* residual time_out */
  body(C,RG2,VS2).	
  
body(Module:logen(Ann,Call),R,V) :-
  body(logen(Ann,Module:Call),R,V).
  

body(logen(unfold,Call),ResidualCall,V) :-
	add_extra_argument("_u",Call,V,ResCall),
	(generate_debugging_gx(3) -> ResidualCall = (print_message(unfold(Call,V)),ResCall)
	                          ;  ResidualCall = ResCall).
	
body(logen(memo,Call),AVCall,VFilteredCall) :-
        add_two_arguments("_request",Call,internal,VFilteredCall,AVCall).

body(logen(clpmemo,Call),AVCall,VFilteredCall) :-
        add_extra_argument("_clp_needs_fixing",Call,VFilteredCall,AVCall).  %%% no longer using _cm


/*  Commented this out, insert_pattern/2 is no longer supported,
    if it is needed please ammend code.
body(dynamic_memo(Call),(find_pattern(Call,VFilteredCall) ->
				 true ;
				 (generalise(Call,GCall),
				     insert_pattern(GCall,_Hd),
				     find_pattern(Call,VFilteredCall))
				),VFilteredCall).
*/
/* dynamic_memo is for :- dynamic predicates that will be asserted dynamically */

body(true,true,true).



	
body(logen(online,Call),GX,R) :-
	findall((Call,Guard),is_safe(Call,Guard), Guards),
	(Guards \= [] ->
	%(is_safe(Call,Guard) ->
	    (filter(Call, _) ->		   
		Safe = unfold,
		Unsafe = memo
	    ;
		Safe = call,
		Unsafe = rescall
	    ),	    
	    body(logen(Safe,Call),UGx,UR),
	    body(logen(Unsafe,Call),MGx,MR),
	    make_guard_disj(Call,Guards,GuardDisj),
	    GX = when(GuardDisj, (_O=online(MGx,R=MR),R = UR,UGx))  	    		     
	;
	    GX = true,
	    R = no_safe_guard_specified
	).

	
	    
	

body(logen(call,Call),Call,true).
body(logen(rescall,Call),true,Call).
body(logen(unknown,Call),true,true) :- print_message(ignoring_unknown(Call)),fail.
body(logen(semicall,Call),GenexCall,ResCall) :-
	specialise_imperative(Call,GenexCall,ResCall).



/* Ecce/ProB PrePost directives */
body(pp_mnf(G),R,V) :- body(G,R,V).
body(pp_cll(G),R,V) :- body(G,R,V).
body(mnf(G),R,V) :- body(G,R,V).

body(constraint(Call),constraintTest(Code,Call),Code).

%% Steve changed...
%% I need if condition to return residual aswell,
%% is this not the desired behaviour?
%% Would imagine most times VS1 = true anyway
% mal: but in case G1 fails nothing will ever instantiate the code VS1 !!
% corrected problem:

%body('->'(G1,G2),    /* if without then; RG1 should be determinate but can fail */
%     (RG1,RG2),
%     ((V1)->(V2))) :-
%	body(G1,RG1,VS1),
%	body(G2,RG2,VS2).
body(if(G1,G2,G3),    /* Static if: */
     ((RG1) -> (RG2,(V=(VS1,VS2))) ; (RG3,(V=VS3))),
     V) :-
	body(G1,RG1,VS1),
	body(G2,RG2,VS2),
	body(G3,RG3,VS3).
%body(resif(G1,G2,G3), /* Dynamic if: which tries to propagate bindings from G1 into G2: does not work */
%     findall((VS1,VS2,VS3), ((RG1,RG2);RG3), [ (V1,V2,_), (_,_,V3)]),
%     /* RG1,RG2,RG3 shouldnt fail but RG1 and RG3 can instantiate ?? */
%     ((VS1=V1,V1) -> (VS2=V2,V2) ; (VS3=V3,V3))) :-
%  body(G1,RG1,VS1),
%  body(G2,RG2,VS2),
%  body(G3,RG3,VS3).
body(resif(G1,G2,G3), /* Dynamic if: */
     safe_if_conjunction(RG1,RG2,RG3,VS1,VS2,VS3), /* RG1,RG2,RG3 shouldnt fail and not instantiate */
     ((VS1) -> (VS2) ; (VS3))) :-
  body(G1,RG1,VS1),
  body(G2,RG2,VS2),
  body(G3,RG3,VS3).
body(semif(G1,G2,G3), /* Semi-online if: */
	 (safe_call(RG1,VS1)
	  -> ('cogen-tools':flatten(VS1,FlatVS1),
		  ((FlatVS1 == true)
		   -> (RG2,SpecCode = VS2)
		   ;  ((FlatVS1 == fail)
			 -> (RG3,SpecCode = VS3)
			 ;  (safe_or(RG2,RG3,VS2,VS3), (SpecCode = ((FlatVS1) -> (VS2) ; (VS3))))
			)
		  ))
	   ; (RG3,SpecCode = VS3)
	 ),
	 /* RG1,RG2,RG3 shouldnt fail and be determinate */
      SpecCode) :-
	body(G1,RG1,VS1),
	body(G2,RG2,VS2),
	body(G3,RG3,VS3).
body(logif(G1,G2,G3), /* logical if without cut : */
     if(RG1,(RG2,V=(VS1,VS2)),(RG3,V=VS3)),
     V) :-
  body(G1,RG1,VS1),
  body(G2,RG2,VS2),
  body(G3,RG3,VS3).
body(reslogif(G1,G2,G3), /* residual logical if without cut : */
     safe_if_conjunction(RG1,RG2,RG3,VS1,VS2,VS3), /* RG1,RG2,RG3 shouldnt fail and not instantiate */
     if(VS1,VS2,VS3)) :-
  body(G1,RG1,VS1),
  body(G2,RG2,VS2),
  body(G3,RG3,VS3).
  
body(try(G1,G2),
        (RG1 -> (RG2,SpecCode = ((VS1) -> true ; (VS2)))
	      ; (RG2,SpecCode = VS2)),
	  SpecCode) :- 
	body(G1,RG1,VS1),
	body(G2,RG2,VS2).

	
body(resdisj(G1,G2),safe_or(RG1,RG2,VS1,VS2),(VS1 ; VS2)) :- /* residual disjunction */
  body(G1,RG1,VS1),
  body(G2,RG2,VS2).
body( (G1;G2), ((RG1,V=VS1) ; (RG2,V=VS2)), V) :- /* static disjunction */
  body(G1,RG1,VS1),
  body(G2,RG2,VS2).
  
body(not(G1),    /* Static declarative not: */
     \+(RG1),true) :-
  body(G1,RG1,_VS1).

body(resnot(G1), /* Dynamic declarative not: */
     safe_call(RG1,VS1),\+(VS1)) :-
  body(G1,RG1,VS1).
body(restnot(G1), /* Dynamic declarative not: */
     safe_call(RG1,VS1),tnot(VS1)) :-
  body(G1,RG1,VS1).

body(hide_nf(G1),GXCode,ResCode) :- 
  (body(G1,RG1,VS1)->
   (flatten(RG1,FlatRG1),
    flatten(VS1,FlatVS1),
    GXCode = (terms:term_variables(G1,VarsG1), 
		  findall((FlatVS1,VarsG1),FlatRG1,ForAll1),
		  cogen:make_disjunction(ForAll1,VarsG1,ResCode)));
   (GXCode = true,
    ResCode=fail)). 
body(hide(G1),GXCode,ResCode) :- 
  (body(G1,RG1,VS1)->
   (flatten(RG1,FlatRG1),
    flatten(VS1,FlatVS1),
    GXCode = (terms:term_variables(G1,VarsG1), 
		  findall((FlatVS1,VarsG1),FlatRG1,ForAll1),
		  ForAll1 = [_|_], /* detect failure */
		  make_disjunction(ForAll1,VarsG1,ResCode)));
   (GXCode = true,
    ResCode=fail)). 
  

  /* some special annotations: */
body(logen(ucall,Call),
     ('cogen-tools':add_extra_argument("_u",Call,V,ResCall),
	call(ResCall)),
     V).
  
body(logen(mcall,Call),
     ('cogen-tools':add_two_arguments("_request",Call,internal,V,ResCall),
	call(ResCall)),
     V).


body(clpmcall(Call),
     ('cogen-tools':add_extra_argument("_cm",Call,V,ResCall),
	call(ResCall)),
     V).

body(det(Call),
     (copy_term(Call,CCall),
	((CCall)->(C=(Call=CCall));(C=fail))),C).

body(findall(Vars,G2,Sols), /* Static findall: */
     findall(Vars,RG2,Sols),
     true) :-
  body(G2,RG2,_VS2).
body(resfindall(Vars,G2,Sols), /* Dynamic findall: */
     safe_call(RG2,VS2), /* RG2 shouldnt fail and be determinate */
     findall(Vars,VS2,Sols)) :-
  body(G2,RG2,VS2).
body(Call,RG, ResCall) :- 
   see_through_primitive(Call,G,ResCall,VS),
   body(G,RG,VS).

body(Call,true,Call) :- \+(proper_annotation(Call)),
   print_error('Warning: not a proper annotation, doing rescall on:'),
   print_error(Call). 
   
%% Makes the guards for online annotation in disjunction form
convert_guard(diff(A,B), ?=((A,_),(B,_))):-!. %%only pass guard if def diff
convert_guard(nonvar(G),nonvar(G)).
convert_guard(ground(G),ground(G)).

convert_guard1(A,A1) :- convert_guard(A,A1).
convert_guard1((A,B),(A1,B1)):-
	     convert_guard(A,A1),
	      convert_guard1(B,B1).
	      
make_guard_disj(Call, [(Call,Guard)], CGuard) :-
	convert_guard(Guard,CGuard).
make_guard_disj(Call, [(Call,Guard)|T], (CGuard;DT)) :-
	convert_guard(Guard,CGuard),	
	make_guard_disj(Call,T,DT).


proper_annotation(X) :- see_through_primitive(X,_,_,_).
proper_annotation(X) :-
	member(X,[(_1126,_1127),reswhen(_1121,_1122),semiwhen(_1121,_1122),logen(unfold,_1117),
	logen(memo,_1112),logen(clpmemo,_1107),dynamic_memo(_1102),true,logen(call,_1096),
	logen(rescall,_1091),logen(semicall,_1086),constraint(_1081),if(_1075,_1076,_1077),
	resif(_1069,_1070,_1071),semif(_1063,_1064,_1065),
	reslogif(_1069,_1070,_1071),logif(_1063,_1064,_1065),
	try(_1058,_1059),
	resdisj(_1053,_1054),(_1048;_1049),not(_1044),resnot(_1040),restnot(_1036),
	hide_nf(_1032),hide(_1028),logen(ucall,_1024),logen(mcall,_1019),clpmcall(_1014),
	det(_1010),findall(_1004,_1005,_1006),resfindall(_998,_999,_1000),
	_Module:logen(_,_),pp_mnf(_), pp_cll(_), mnf(_), time_out(_,_,_), logen(online,_)]).


% to regenerate above list for proper_annotation, do the following:
%:- dynamic body/3.
% ?- findall(X,clause('cogen':body(X,Y,Z),B),L), print(L).
%% updated for new annotation style (24 sept 03), BTW dynamic pred must be above body delcaration

%% convert cogen code back into normal code (for semiwhen etc)
convert_residual((B1;B2), (NB1;NB2)) :-
	!,
	convert_residual(B1,NB1),
	convert_residual(B2,NB2).

convert_residual((B1,B2), (NB1,NB2)) :-
	!,
	convert_residual(B1,NB1),
	convert_residual(B2,NB2).
convert_residual(Unfolded, Original) :-
	remove_cogen_args(Unfolded, "_u",1,Original),!.
convert_residual(Memo, Original) :- 
	remove_cogen_args(Memo,"_m", 2, Original),!.
convert_residual(X,X).	
remove_cogen_args(Residual, Suffix,ExtraArgCount, Original) :-
	Residual =.. [Call|Args],	
	name(Call, CList),
	append(NCallList, Suffix, CList),
	name(OrigCall,NCallList),
	length(ExtraArg,ExtraArgCount),
	append(OrigArgs, ExtraArg, Args),
	Original =.. [OrigCall|OrigArgs].	


make_disj([],fail).
make_disj([H],H) :- !.
make_disj([H|T],(H ; DT)) :- 
	make_disj(T,DT).
	
	
make_disjunction([],_,fail).
make_disjunction([(H,CRG)],RG,FlatCode) :- 
	!,pp:simplify_equality(RG,CRG,EqCode),
	flatten((EqCode,H),FlatCode).
make_disjunction([(H,CRG)|T],RG,(FlatCode ; DisT)) :-
	pp:simplify_equality(RG,CRG,EqCode),
	make_disjunction(T,RG,DisT),
	flatten((EqCode,H),FlatCode).
	
specialise_imperative(dif(X,Y),
        (\+(X=Y) -> (Code=true)
           ; ((X==Y) -> fail ; (Code=dif(X,Y)))), Code) :- !.
specialise_imperative((X \= Y),
        (\+(X=Y) -> (Code=true)
           ; ((X==Y) -> fail ; (Code=(X \= Y)))), Code) :- !.
specialise_imperative('=='(X,Y),
        ('=='(X,Y) -> (Code=true)
           ; (\+(X=Y) -> fail ; (Code='=='(X,Y)))), Code) :- !.
specialise_imperative(is(X,Y),
        (ground(Y) -> (X is Y, Code = true) ; (Code = is(X,Y))), Code) :- !.
specialise_imperative(Call,Call,true)
	:- equallike_imperative(Call),!.
specialise_imperative(Call,Call,Call)
	:- varlike_imperative(Call),!.
specialise_imperative(Call,
	(Call -> (Code=true)
	   ; (ground(Call) -> fail ; (Code=Call))), Code) :-
	 groundlike_imperative(Call),!. 
	 /* ground like: if it succeeds at spectime then it succeeds for all instances at runtime */
specialise_imperative(X,true,X).




 

    
cogen_can_generalise(Call,GenCall) :-
    findall(AArgTypes,filter(Call,AArgTypes),[ArgTypes]), /* only a single type present */
    Call =.. [F|Args],
    static_types(ArgTypes,Args,GenArgs), /* check whether we can filter at cogen time  and do it*/
    GenCall =.. [F|GenArgs]. 

/* types which allow generalisation/filtering at cogen time */
static_types([],[],[]).
static_types([static|T],[X|AT],[X|GT]) :- static_types(T,AT,GT).
static_types([free|T],[X|AT],[X|GT]) :- static_types(T,AT,GT).
static_types([static_or_free|T],[X|AT],[X|GT]) :- static_types(T,AT,GT).
static_types([dynamic|T],[_X|AT],[_|GT]) :- static_types(T,AT,GT).

%%added by steve





filter_cons(H,T,HT,FVal) :-
	((nonvar(H),H = FVal) -> (HT = T) ; (HT = (H,T))).

print_header(Module) :-
  print('/'),print('*  --------------------  *'),print('/'),nl,
  print('/'),print('*  GENERATING EXTENSION  *'),print('/'),nl,
  print('/'),print('*  --------------------  *'),print('/'),nl,
  nl,
  remove_directory(Module,BaseModuleName),
  print(':- module('),pp_term(BaseModuleName),
    print(', [table_gx/1,user_type/2,op_def_gx/3,dynamic_gx/1]).'),nl,
  findall(M, ann_directive(use_module(M)), DependencyList),
  print_quoted(logen_module_dependencies(DependencyList)),print('.'),nl,

        % XXX I have removed a bunch of headers that I don't really
        % XXX understand, and that currently conflict with the modular
        % XXX specialization.  -- Armin

%  print(':- use_package(sicsignore).'),nl,
 print('/* commented out, otherwise standalone .sav and .exe do not work !'),nl, print(' '),
  /* because the logen_attributes will be wiped clear by the use_module when loading the gx! */
  print_use_module('memoizer.pl', 
       [find_pattern/4,insert_pattern_with_filter_types/5]),
 (logen_preferences:get_preference(modular_spec_mode,true)
  -> print_use_module('moduledriver.pl', 
       [request_external_pattern/4])
  ;  true
 ),
%  print_use_module('pp.pl',[simplify_equality/3]),
 print_use_module('logen_filter',[generalise_with_filter_types/3]),
 print_use_module('cogen-tools',[add_two_arguments/5,add_extra_argument/4]),
  (generate_debugging_gx(1)
     -> print_use_module('logen_messages',[print_message/1, print_short_msg/1]) 
     ;  true),
  %print_use_module('cogen.pl'), /* for generalise with filter types maybe move somewhere else*/
  % print_use_module('logen_filter.pl'),
  %print_use_module('gx_pp.pl'),
  print(' ---- */'),nl,
  %print(':- dynamic op_ref_gx/2.\n'),
  (is_clp(on) ->   print(':'),print('- use_module(library(clpq)).'),nl ; true),   (is_clp(on) ->   print_use_module('clprmemo.pl') ; true), 
%  (static_consult(List) -> pp_consults(List) ; true),
  nl,
  auxiliary_predicates(Text),
  print(Text),nl.


print_use_module(File) :-
  'cogen-tools':logen_source_directory(Dir),
  print(':'), print('- use_module('''),
  print(Dir), print(File), print(''').'),nl.

print_use_module(File,ImportList) :-
  'cogen-tools':logen_source_directory(Dir),
  print(':'), print('- use_module('''),
  print(Dir), print(File), print(''','),
  print(ImportList),print(').'),nl.
  
  

print_gx_module_declaration(_) :-
   get_preference(prolog_mode_flag,xsb),!,nl.
print_gx_module_declaration(FileName) :-
   module_name_from_filename(FileName, Module),	
   print(' :- module(\''), print(Module), print('\', '),
   findall(PredInfo, (annfile:residual(ResidualCall),
                      get_memo_pred_info(ResidualCall,PredInfo)), ExportedPreds),
   findall(Request, (annfile:residual(ResidualCall),
                      get_request_pred_info(ResidualCall, Request)), ExportedReqPreds),
   append(ExportedPreds, ExportedReqPreds, Export1),
   append(Export1, [residual_predicate/2], Export1A),
   append(Export1A, [op_def_gx/3, dynamic_gx/1], Export1B),
   append(Export1B, [use_module_in_specfile/1, use_module_in_specfile/2, table_gx/1], Export),
   pp_term(Export),      /* use pp_term to print funny predicate names properly */
   print(').'),nl.
   
   
get_memo_pred_info(ResidualCall,ResPred/ResArity) :-
    get_predicate_info(ResidualCall,Pred/Arity),
    res_name("_spec",Pred,ResPred),ResArity is Arity+2.


get_request_pred_info(ResidualCall,ReqPred/ReqArity) :-
    get_predicate_info(ResidualCall,Pred/Arity),
    res_name("_request", Pred,ReqPred), ReqArity is Arity +2.




module_name_from_filename(File, Module) :-
	no_path_file_name(File,Filename),
	remove_ext(Filename, Module, _Ext).


%%% this is pinched from the ciao 1.9 source, thank you guys :-)
no_path_file_name(P,F) :-
	atom(P),
	atom_codes(P,PasStr),
	!,
	no_path_file_name_str(PasStr,FasStr),
	atom_codes(F,FasStr).

no_path_file_name(P,F) :-
	no_path_file_name_str(P,F).

no_path_file_name_str(P, F) :-
        append(_, [0'/|R], P), !,
        no_path_file_name_str(R, F).

no_path_file_name_str(F, F).

remove_ext(File, Root,Ext) :-
	name(File,SFile),
	remove_ext1(SFile, SRoot,SExt),
	name(Root,SRoot),
	name(Ext,SExt).

remove_ext1([],[],[]).
remove_ext1([46|T],[], T).
remove_ext1([H|T], [H|T1], T2) :-
	remove_ext1(T,T1,T2).




auxiliary_predicates('

:- use_module(library(terms), [term_variables/2]).

  /* Logen auxiliary predicates */

safe_if_conjunction(If,Th,El,Code1,Code2,Code3) :-
  terms:term_variables((El,Code3),V3),
  safe_call(If,Code1),
  (check_var(V3) -> true
    ; logen_messages:print_error(\'WARNING: If Condition has instantiated Else Branch\'),
      (logen_messages:print_error(condition(If)), logen_messages:print_error(vars(V3)))
  ),
  terms:term_variables((If,Code1),V1), terms:term_variables((El,Code3),V3_2), 
  safe_call(Th,Code2),
  (check_var(V1) -> true
    ; logen_messages:print_error(\'WARNING: Then Branch has instantiated If Condition\'),
      (logen_messages:print_error(then_branch(Th)), logen_messages:print_error(vars(V1)))
  ),
  (check_var(V3_2) -> true
    ; logen_messages:print_error(\'WARNING: If Condition has instantiated Else Branch\'),
      (logen_messages:print_error(then_branch(Th)), logen_messages:print_error(vars(V3_2)))
  ),
  terms:term_variables((If,Code1),V1_2), terms:term_variables((Th,Code2),V2), 
  safe_call(El,Code3),
  (check_var(V1_2) -> true
    ; logen_messages:print_error(\'WARNING: Else Branch has instantiated If Condition\'),
      (logen_messages:print_error(else_branch(El)), logen_messages:print_error(vars(V1_2)))
  ),
  (check_var(V2) -> true
    ; logen_messages:print_error(\'WARNING: Else Branch has instantiated Then Branch\'),
      (logen_messages:print_error(else_branch(El)), logen_messages:print_error(vars(V2)))
  ).
  
safe_call(Call,Code) :-
   if(Call,true,(Code=fail,logen_messages:print_error(\'WARNING: call has failed\'),
                 logen_messages:print_error(call(Call)))).
   

safe_or(Call1,Call2,Code1,Code2) :-
   terms:term_variables((Call2,Code2),V2),
   safe_call(Call1,Code1),
  (check_var(V2) -> true
    ; logen_messages:print_error(\'WARNING: Or Branch 1 has instantiated Branch 2\'),
      (logen_messages:print_error(branch1(Call1)), logen_messages:print_error(vars(V2)))
  ),
   terms:term_variables((Call1,Code1),V1),
   safe_call(Call2,Code2),
  (check_var(V1) -> true
    ; logen_messages:print_error(\'WARNING: Or Branch 2 has instantiated Branch 1\'),
      (logen_messages:print_error(branch1(Call2)), logen_messages:print_error(vars(V1)))
  ).
  
check_var([]).
check_var([H|T]) :- var(H),check_var(T).

').

