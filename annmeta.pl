:- module(annmeta,[annmeta/5, 
				   declmeta/3,
				   cogenmeta/8,
				   annotation_args/3,
				   is_simple_ann/1,
				   annotation_matcher_data/4,
				   is_infix/1,
				   get_simple_anns/1,
				   display_in_filters/1,
				   ann_to_position/7,
				   term_to_position/6,
				   higher_order_gui_info/3]).

:- use_module(cogen_data, [cogen_data/2]).

:- use_module('gximports/safe_call.pl').

:- use_module(annloader,[ predicate_defined_by_filter/2]).

%%% All information relating to annotations should be stored in meta data

%% What do we have to do with annotations?
%%   Load from file
%%      -must understand higher order e.g. must descend into meta calls
%%   Create GX files (cogen) from them
%%      -cogen time, gx time, spec time, run time.


%is_simple_ann(A) :- \+(higher_order_ann(A,_,_,_,_)).

is_simple_ann(unfold).

is_simple_ann(memo).
is_simple_ann(call).
is_simple_ann(rescall).
is_simple_ann(semicall).
is_simple_ann(online).
is_simple_ann(watched).
is_simple_ann(mcall).

is_infix(if(_,_,_)).
is_infix(resif(_,_,_)).
is_infix(resdisj(_,_)).
is_infix(disj(_,_)).
is_infix(';'(_,_)).

higher_order_gui_info(1, _, _).
higher_order_gui_info(2, unknown_disj, [disj, resdisj]).
higher_order_gui_info(3, unknown_if, [if, resif]).
% NB no menu for 4 : hide_nf.
higher_order_gui_info(5, unknown_not, [not, resnot]).
higher_order_gui_info(6, unknown_when, [reswhen]).
higher_order_gui_info(7, unknown_findall, [findall, resfindall]).

%% Ann structure, internal structure, original code, Higher Order map, Higher Order map, CONSUMES ANN
higher_order_ann((A,B),(NA,NB),(NA,NB),[A,B],[NA,NB], 1).
higher_order_ann(if(A,B,C),if(NA,NB,NC),(NA->NB;NC),[A,B,C],[NA,NB,NC], 3).
higher_order_ann(resif(A,B,C),resif(NA,NB,NC),(NA->NB;NC),[A,B,C],[NA,NB,NC], 3).
higher_order_ann(resdisj(A,B),resdisj(NA,NB),(NA;NB),[A,B],[NA,NB], 2).
higher_order_ann(';'(A,B),';'(NA,NB),(NA;NB),[A,B],[NA,NB], 2).
higher_order_ann('disj'(A,B),';'(NA,NB),(NA;NB),[A,B],[NA,NB], 2).
higher_order_ann(hide_nf(A),hide_nf(NA),(NA),[A],[NA], 4).
higher_order_ann(resnot(A),resnot(NA),\+(NA),[A],[NA], 5).
higher_order_ann(not(A),not(NA),\+(NA),[A],[NA], 5).
higher_order_ann(resnot(A),resnot(NA),not(NA),[A],[NA], 5).
higher_order_ann(not(A),not(NA),not(NA),[A],[NA], 5).
higher_order_ann(reswhen(D,A),reswhen(D,NA),when(D,NA),[A],[NA], 6).
higher_order_ann(findall(V,A,R),findall(V,NA,R),findall(V,NA,R),[A],[NA], 7).
higher_order_ann(resfindall(V,A,R),resfindall(V,NA,R),findall(V,NA,R),[A],[NA], 7).

position_info(1, term_position(_,_,_,_,[P1,P2]), [P1,P2], []).
position_info(2, term_position(_,_,S,E,[P1,P2]), [P1,P2], [S,E]).
position_info(3, term_position(_,_,_,_,[term_position(_,_,S,E,[P1,P2]), P3]),
				 [P1,P2,P3], [S, E]).
position_info(4, term_position(S,E,CS,CE,P),
				 [term_position(S,E,CS,CE,P)], [S, E]).
position_info(4, S-E, [S-E], [S,E]).
position_info(5, term_position(_,_,S,E,[P]), [P], [S,E]).
position_info(6, term_position(_,_,S,E,[_,P]), [P], [S,E]).
position_info(7, term_position(_,_,S,E,[_,P,_]), [P], [S,E]).

% this makes annotations of a:b(...) highlight better
ann_to_position(logen(Ann, _:_),
				term_position(_,_,_,_,[S-_,term_position(_,_,_,E, Args)]), [], [], Ann, [S,E], Arity) :-
	!, length(Args, Arity).
ann_to_position(logen(Ann, _:_),
				term_position(_,_,_,_,[S-_,_-E]), [], [], Ann, [S,E], 0) :- !.
ann_to_position(logen(Ann, _),
				term_position(_,_,S,E,Args), [], [], Ann, [S,E], Arity) :-
	length(Args, Arity).
ann_to_position(logen(Ann, _), S-E, [], [], Ann, [S,E], 0).
ann_to_position(A, P, SubA, Ps, Ann, Pos, 0) :-
	higher_order_ann(A, _, _, SubA, _, Index),
	functor(A, Ann, _),
	position_info(Index, P, Ps, Pos).

% 6th parameter is the unknown type. For simple anns this is just 'unknown'
% for higher order anns it is something else.
term_to_position(T, P, Pos, TMap, PMap, Unknown/0) :-
	higher_order_ann(A, _, T, _, TMap, Index),
	%hide_nf can match anything so exclude it completely
	A \= hide_nf(_), !,
	higher_order_gui_info(Index, Unknown, _),
	position_info(Index, P, PMap, Pos).
% this makes annotations of a:b(...) highlight better
term_to_position(_:_, term_position(_,_,_,_, [S-_, term_position(_,_,_,E,Args)]), [S,E], [], [], unknown/Arity) :- !, length(Args, Arity).
term_to_position(_:_, term_position(_,_,_,_, [S-_, _-E]), [S,E], [], [], unknown/0) :- !.
term_to_position(_, term_position(_, _, S, E, Args), [S,E], [], [], unknown/Arity) :- length(Args, Arity).
term_to_position(_, S-E, [S,E], [], [], unknown/0).
	
%% used by matcher.pl 
annotation_matcher_data(A,B, MapFrom,MapTo) :-
	higher_order_ann(A,_,B, MapFrom,MapTo,_).
annotation_matcher_data(logen(_,A), A, [], []).



%higher_order_ann(A,_,B, MapFrom,MapTo),
%strip_ann_l([],[]).
%strip_ann_l([A|As],[O|Os]) :-
%	strip_ann(A,O),
%	strip_ann_l(As,Os).%

%strip_ann(logen(_,A),A) :- !.
%strip_ann(A, B) :-
%	higher_order_ann(A,_,B, MapFrom,MapTo),
%	strip_ann_l(MapFrom,MapTo).



%% higher order annotations
annmeta(load,A,NA,Map1,Map2) :-
	higher_order_ann(A,NA,_,Map1,Map2,_).

%%% the simple annotations
annmeta(load, logen(Ann,Call), logen(Ann,Call),[],[]) :-
	is_simple_ann(Ann).



%%% declaration loading,
%% input, id, output
declmeta(filter(Call), filter, Call).
declmeta(type(TypeDef), type, TypeDef).
declmeta(module(Module,_), module, Module).
declmeta(module(Module), module, Module).
declmeta(op(Prio,Assoc,Op),op,op(Prio,Assoc,Op)).
declmeta(dynamic(Pred),dynamic,dynamic(Pred)).
declmeta(use_module(library(X)),use_library,use_module(library(X))).
declmeta(use_module(library(X),Preds),use_library,use_module(library(X),Preds)).
declmeta(use_module(X),use_module,use_module(X)) :- \+(X=library(_)).
declmeta(use_module(X,Preds),use_module,use_module(X,Preds)) :- \+(X=library(_)).
declmeta(use_package(X),use_library,use_package(X)).
declmeta(ensure_loaded(X),ensure_loaded,ensure_loaded(X)).
declmeta(table(Pred),table, table(Pred)). /* XSB Prolog */

display_in_filters(type).
display_in_filters(filter).

%% Code generation
%% cogenmeta(Annotation, Cogen, Gx, Spec, Map,GxMap,SpecMap)
%%   Cogen= calls to execute at cogen time
%%   Gx = calls to execute in the generating extension
%%   Spec = calls to execute at specialisation time
%%   Map,GxMap,SpecMap: lists of same length for higher-order traversal of cogenmeta

%% watched is like unfold but watched by online control
cogenmeta(logen(unfold, Call), %%WATCHED UNFOLD note guard!!!!
	  (
	    %% Cogen time
	  import_online_files,
	    %%import_file_into_gx('online.pl',all),
	    logendata_varname(LOGENDATA),
	    build_unfold_call(Call, SpecCode, LOGENDATAPRIME, UnfoldCall)      /* had to remove the $VAR around LOGENDATAPRIME: as two unfolders can be in same clause !!*/
	    %build_request_call(Call, internal, SpecCode,LOGENDATA, MemoCall)
	  ),
	  ( %% GX time
	      get_logendata_history(LOGENDATA,HISTORY),
	      set_logendata_history([Call|HISTORY], LOGENDATA, LOGENDATAPRIME),
	      (is_safecall(Call,HISTORY) ->
		  true  %% Online control agrees it is safe
	      ;
		  %% Online control whistles!!! may not be safe
		  confirm_user(online_unsafe_unfold(Once), [Call,HISTORY],_RetVal,unfold(Call,PP))
	      ),
	      catch(UnfoldCall,_Err,
	              (format(user_error, "~n<| UNFOLD ERROR |> : CALL Atom ~w~n",[Call]),
	               print_program_point_error('A call marked as unfold has no defining clauses. Mark the call as rescall or call.',
	                   error,no_unfolder,[call(Call)],unfold(Call,PP)),
	                      abort_specialization
	              ))
	  ),
	  SpecCode, [],[],[],PP)  :- cogen_data:cogen_data(watch,unfold),!, 
	                          (cogen_data(watch_once,_) -> Once=yes ; Once=no).


	  

cogenmeta(logen(unfold, Call),
	  %add_extra_argument('_u', Call, Extra, UnfoldCall),
	  %add_two_extra_arguments('_u', Call, Extra,'$VAR'('LOGENDATA'), UnfoldCall),
	  (
	    logendata_varname(LOGENDATA),
	    build_unfold_call(Call, Extra, LOGENDATA, UnfoldCall)
	  ),
	  UnfoldCall, Extra,[],[],[],_PP).	

cogenmeta(logen(memo, Call),
	 (
	   logendata_varname(LOGENDATA),
	   build_request_call(Call, internal, FCall,LOGENDATA2, MemoCall)
	   %add_two_extra_arguments('_request', Call, internal,FCall, MemoCall1),
	   %add_extra_argument(MemoCall1, '$VAR'('LOGENDATA'), MemoCall)
	 ),	   
	  ResCode, FCall,[],[],[],PP) :-
	  (functor(Call,Pred,Arity),
	   predicate_defined_by_filter(Pred,Arity) -> true
	    ; (write(user_error,'Predicated marked as memo has no filter declaration !'), nl(user_error),
	       write(user_error(memo(Call))), nl(user_error)
	  )),
	  (cogen_data:cogen_data(watch,_)
	    -> ResCode = (set_logendata_pp(PP,LOGENDATA,LOGENDATA2),
	                  catch(MemoCall,_Err,
	                     (format(user_error, "~n<| FILTER ERROR |> : CALL Atom ~w~n",[Call]),
	                      print_program_point_error('A memoised call has no filter declaration!',
	                   error,no_filter,[call(Call)],memo(Call,PP)),
	                      abort_specialization))
	                 )
	      /* store PP info */
	    ;  (LOGENDATA2=LOGENDATA,ResCode = MemoCall)).


%cogenmeta(logen(mcall, call(Call)),
cogenmeta(logen(mcall, Call),
	logendata_varname(LOGENDATA),
	 (
	   Call = call(Call1),
	   build_request_call(Call1, internal, FCall,LOGENDATA, MemoCall),
	   MemoCall
	   
	 ),	   
	  FCall,[],[],[],_PP).


cogenmeta(true, true, true,true,[],[],[],_PP).
cogenmeta(logen(call, '!'), true, '!',true,[],[],[],_PP) :- !.
cogenmeta(logen(call, Call),    true, GXCall, true,[],[],[],PP) :- 
   ((\+cogen_data:cogen_data(sandbox_mode,_) ; white_list(Call))
     -> TheCall = Call
     ;  TheCall = safe_call(Call)
   ),
   (cogen_data:cogen_data(watch,builtins)
          -> generate_watchdog_call(TheCall,call(Call,PP),GXCall)
           ; GXCall = TheCall
   ).
%cogenmeta(logen(call, Call),    true, Call, true,[],[],[],_PP).
cogenmeta(logen(rescall, Call), true, WatchGXCall, Call,[],[],[],PP) :- 
    cogen_data:cogen_data(watch,backpropagations),!,
    generate_watch_backprop_call(Call,PP,WatchGXCall).
cogenmeta(logen(rescall, Call), true, true, Call,[],[],[],_PP).
cogenmeta(logen(semicall, Call), true, GXCode, SpecCall, [],[],[],_PP) :-
   (semi_call_primitive(Call,GXCode, SpecCall)
     -> true
     ; (GXCode=true, SpecCall = Call)
   ).
cogenmeta(resif(A,B,C),         true, 
   watch_gx_if_conjunction(GxA,GxB,GxC,SpA,SpB,SpC,PP,Once,A,B,C),/* GxA,GxB,GxC shouldnt fail and not instantiate */
             (SpA->SpB;SpC),[A,B,C],[GxA,GxB,GxC],[SpA,SpB,SpC],PP) :-
    cogen_data:cogen_data(watch,connectives),!,
    (cogen_data(watch_once,_) -> Once=true ; Once=false).
cogenmeta(resif(A,B,C),         true,
          (if(GxA,(Spec=(SpA->SpecB;SpecC),
                    if(GxB,SpecB=SpB,SpecB=fail)),
                  Spec=SpecC),
           if(GxC,SpecC=SpC,SpecC=fail)),
           Spec,[A,B,C],[GxA,GxB,GxC],[SpA,SpB,SpC],_PP).
cogenmeta(if(A,B,C),            true,   
  (GxA->(GxB,SpecCode=(SpA,SpB));(GxC,SpecCode=SpC)),SpecCode,[A,B,C],
  [GxA,GxB,GxC],[SpA,SpB,SpC],_PP).
 


cogenmeta(hide_nf(A),   
        %%% Cogen time
	 import_file_into_gx('cogen.pl', [make_disj/3,single_disj/3,make_unification/3]),
	 
	(     %%% Spec time
	    %portray_clause(cog(A,GA)),
	    copy_term((A,GA,SA),(CA,CGA,CSA)),
	    varset(CA, Vars1),
	    varset(A, Vars2),
	    findall((Vars1-CSA),CGA,Ans),
	    %portray_clause(make_disj(Ans,Vars2, Disj)),
	    make_disj(Ans,Vars2, Disj)
	    %,portray_clause(make_disj(Ans,Vars2, Disj))
	),
	(  %%%% Residual Code
	    Disj
	),
	[A],[GA],[SA],_PP).
 

cogenmeta(logen(online, Call),
	  (
	    %% Cogen time
	  import_online_files,
	    logendata_varname(LOGENDATA),
	    build_unfold_call(Call, SpecCode, LOGENDATAPRIME, UnfoldCall), /* had to remove the $VAR around LOGENDATAPRIME: as two unfolders can be in same clause !!*/
	    build_request_call(Call, internal, SpecCode,LOGENDATA, MemoCall)
	  ),
	  ( %% GX time
	      
	      get_logendata_history(LOGENDATA,HISTORY),
	    
	      (is_safecall(Call,HISTORY) ->
		   (SafeDebug,
		    set_logendata_history([Call|HISTORY], LOGENDATA, LOGENDATAPRIME),
		    UnfoldCall)
	      ;
		  (UnsafeDebug,
		   MemoCall)
	      )
	  ),
	  SpecCode, [],[],[],_PP) :-
	  (cogen_data(debug_mode_full,true) 
	    -> (SafeDebug=format(user,"Safe online unfold: ~w~n",[Call]), 
	        UnsafeDebug=format(user,"UNSAFE online unfold: ~w~n",[Call]) )
	    ;  (SafeDebug=true, UnsafeDebug=true)
	  ).


		  
	  
	  
	  
cogenmeta((A,B), true,(GxA,GxB),(SpecA,SpecB), [A,B], [GxA,GxB],[SpecA,SpecB],_PP).
cogenmeta((A;B), true,((GxA,Spec=SpecA) ; (GxB,Spec=SpecB)),Spec, [A,B], [GxA,GxB],[SpecA,SpecB],_PP). /* static or */
cogenmeta(disj(A,B), true,((GxA,Spec=SpecA) ; (GxB,Spec=SpecB)),Spec, [A,B], [GxA,GxB],[SpecA,SpecB],_PP). /* alias for or */
cogenmeta(resdisj(A,B), true,watch_gx_or_conjunction(GxA,GxB,SpecA,SpecB,PP,Once),(SpecA;SpecB), [A,B], [GxA,GxB],[SpecA,SpecB],PP) :-
    cogen_data:cogen_data(watch,connectives),!,
    (cogen_data(watch_once,_) -> Once=true ; Once=false).
cogenmeta(resdisj(A,B), true,
         (if(GxA,if(GxB,Spec=(SpecA;SpecB),Spec=SpecA),
                 if(GxB,Spec=SpecB,Spec=fail)),GxB),Spec, [A,B], 
         [GxA,GxB],[SpecA,SpecB],_PP).
cogenmeta(resnot(A), true,if(GxA,(Spec='\\+'(SpecA)),Spec=true),Spec, [A], [GxA],[SpecA],_PP).
cogenmeta(not(A), true,\+(GxA),true, [A], [GxA],[_SpecA],_PP).  /*Spec = true as GxA must fail and hence cannot instantiate SpecA */
cogenmeta(reswhen(D,A), true,if(GxA,Spec=when(D,SpecA),Spec=fail),Spec,
  [A], [GxA],[SpecA],_PP).
cogenmeta(resfindall(V,A,R), true,if(GxA,Spec=SpecA,Spec=fail),
                     findall(V,Spec,R), [A], [GxA],[SpecA],_PP).
cogenmeta(findall(V,A,R), true,findall(V,GxA,R),SpecA, [A], [GxA],[SpecA],_PP).


generate_watchdog_call(X,PP,WatchdogCall) :-
   (get_callable_builtin_conditions(X,Cond)
    -> WatchdogCall = 
          (Cond -> catch(X,Exc,wcall_exc(X,PP,Exc)) ; wcall_error(X,PP)) 
       
    ;  WatchdogCall = catch(X,Exc,wcall_exc(X,PP,Exc)) 
   ).
   
generate_watch_backprop_call(X,PP,WatchCall) :-
  (cogen_data(watch_once,_) -> Once=yes ; Once=no),
  (get_backpropagation_sensitive_conditions(X,SCond)
         -> WatchCall = (SCond -> wcatch_backprops(X,PP,Once) ; true)
         ;  WatchCall = true
  ).
  
/* annotation_args(Annotation, AnnotatedArgs, UnannotatedArgs) */
annotation_args(logen(_,A),[],[A]).
annotation_args((A,B),[A,B],[]).
annotation_args((A;B),[A,B],[]).
annotation_args(disj(A,B),[A,B],[]).
annotation_args(resdisj(A,B),[A,B],[]).
annotation_args(resif(A,B,C),[A,B,C],[]).
annotation_args(if(A,B,C),[A,B,C],[]).
annotation_args(hide_nf(A),[A],[]).
annotation_args(not(A),[A],[]).
annotation_args(resnot(A),[A],[]).
annotation_args(reswhen(D,A),[A],[D]).
annotation_args(when(D,A),[A],[D]).
annotation_args(resfindall(V,A,R),[A],[V,R]).
annotation_args(findall(V,A,R),[A],[V,R]).
annotation_args(true,[],[]).


:- use_module('tools/ciao_tools.pl',[is_list_skel/1]).

semi_call_primitive(Call, 
  (Cond -> (Call,SpecCode=true) ; SpecCode=Call),
  SpecCode) :- callable(Call,Cond).
semi_call_primitive(ground(X), 
  (ground(X) -> SpecCode=true ; SpecCode=ground(X)),
  SpecCode).
semi_call_primitive('\\=='(A,B), 
  ('\\=='(A,B), (A \= B -> SpecCode=true ; SpecCode='\\=='(A,B))),
  SpecCode).
semi_call_primitive('\\='(A,B), 
  ('\\=='(A,B), (A \= B -> SpecCode=true ; SpecCode='\\='(A,B))),
  SpecCode).
semi_call_primitive(nonvar(X), 
  (nonvar(X) -> SpecCode=true ; SpecCode=nonvar(X)),
  SpecCode).
%semi_call_primitive(is(X,Y), 
%  (ground(Y) -> (X is Y,SpecCode=true) ; SpecCode=is(X,Y)),
%  SpecCode).
%semi_call_primitive(COMP, 
%  (ground(COMP) -> (COMP,SpecCode=true) ; SpecCode=COMP),
%  SpecCode) :- (COMP = '>'(X,Y) ; COMP = '<'(X,Y) ; COMP = '=<'(X,Y) ; COMP = '=<'(X,Y)).
%semi_call_primitive(length(X,L), 
%  ((ground(L);is_list_skel(X)) -> (length(X,L),SpecCode=true) ; SpecCode=length(X,L)),
%  SpecCode).
%semi_call_primitive(reverse(L,R), 
%  (is_list_skel(L) -> (reverse(L,R),SpecCode=true) ; SpecCode=reverse(L,R)),
%  SpecCode).
%semi_call_primitive(append(L,M,R), 
%  (is_list_skel(L) -> (append(L,M,R),SpecCode=true) ; SpecCode=append(L,M,R)),
%  SpecCode).
  
	
callable(integer(X),nonvar(X)).
callable(float(X),nonvar(X)).
callable(number(X),nonvar(X)).
callable(atomic(X),ground(X)).
callable(atom(X),ground(X)).
callable(compound(X),nonvar(X)).
callable(is(_X,Y),ground(Y)).
callable(length(X,L), (ground(L);is_list_skel(X)) ).
callable(reverse(L,_R), is_list_skel(L)).
callable(append(L,_M,_R), is_list_skel(L)).
callable(COMP, ground(COMP)) :- COMP = '>'(X,Y) ; COMP = '<'(X,Y) ; COMP = '=<'(X,Y) ; COMP = '=<'(X,Y).

%genfiltermeta(static, [A], [A], [], true, ground(A)).
%genfiltermeta(dynamic, [A], [_], [_], true, true).
%genfiltermeta(nonvar, [A], GArgs, FArgs, true, (nonvar(A), A=..[F

get_simple_anns(List) :- findall(A, is_simple_ann(A), List).

