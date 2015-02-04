/* file: memo.pl */



%============================================================
%
%               THIS FILE IS NO LONGER IN USE
%                FOR MODULAR SPECIALIZATION
%                     See memoizer.pl.
%
%        Please try to no longer update memo.pl and work
%                with memoizer.pl from now on.
%         I didn't update this file because it was some
%             kind of a major rewrite (with chunks
%              of code copied over, though). -- Armin
%
%============================================================



:- module(memo, 
	    [
	     delete_table/0,
	     find_pattern/4,
	     lookup_pattern_and_mark/3,
	     insert_pattern_with_filter_types/4, 	     
	     filter_atom/3,    %%exported for clprmemo
	     filter_atom_with_filter_types/4, %% added sjc 28/4/03
	     memo_table_entry/5,
%Shouldnt be exporting internals, but used by printing, will alter later
	     memo_table/3,
	     set_memo_table_printing/1,
% print_spec_file_header/1,
	     print_spec_file_header/2, print_use_libraries_and_ops/0,
	     xsb_mode_is_on/0,
	     load_memo_table/2,
	     get_property_value/3,
	     service_all_requests/1
	     ]).


%%% All modules should load this for sicstus support
%% but this doesnt seem to work on sicstus!
%%:- ensure_loaded('sicstus_term.pl').

%% System

:- use_module(library(lists)).


% Logen
%%%%%%  :- use_module('cogen.pl').
%%%%%%  :- use_module('annfile.pl').
:- use_module('cogen-tools.pl').
:- use_module('pp.pl').
:- use_module('logen_messages.pl').
:- use_module('prob/logen_preferences.pl').
:- use_module('gensym.pl').
:- use_module('logen_filter.pl').
:- use_module('memo_pp.pl',[assert_memo_clause/1,reset_memopp/0]).

ciao((:-use_module(library(dec10_io)))).
ciao((:- set_prolog_flag(multi_arity_warnings, off))).



make_gx_visible_to_memo(GXModule) :-
	use_module(GXModule).
%	ensure_loaded(GXModule).

service_all_requests(GX) :-
	make_gx_visible_to_memo(GX),
	service_all_requests1(GX).

service_all_requests1(_GX) :-
	(memo_table_entry(_ID, CallPattern, Residual, request(User),_Properties) ->
	    ( add_two_arguments("_spec", CallPattern, User,Residual, Call),
	      call(Call),
	      fail
	    )
	;
	    !,true
	).

service_all_requests1(GX) :- service_all_requests1(GX).



load_memo_table(FileName, Discard) :-
	seeing(OldFile),
	see(FileName),
	repeat,
	   read(T),
	   process_memo_entry(T, Discard),
	   T == end_of_file,
	   !,
	   seen,
	   see(OldFile).

process_memo_entry(end_of_file, _).
process_memo_entry(gensym(Sym, ID), keep) :- set_gensym_counter(Sym,ID).
process_memo_entry(memo_table(ID, CallPattern, ResCall, Status,Prop),discard):-
	update_status(Status, NewStatus),
	old_gennum_value(SYM),
	(ID >= SYM -> NewSym is ID +1, set_gennum_counter(NewSym)),
	assert(memo_table_entry(ID, CallPattern, ResCall, NewStatus,Prop)).

process_memo_entry(memo_table(ID,CallPattern,ResCall, Status,Prop), keep) :-
	assert(memo_table_entry(ID,CallPattern, ResCall, Status,Prop)),
	assert_code_generated(ResCall).

process_memo_entry(_IGNORE, _):- !.

update_status(done(internal), _) :- !, fail.
update_status(done(external), request(external)).
update_status(done(user), request(user)).




	      




xsb_mode_is_on :- get_preference(prolog_mode_flag,xsb).



%% either returns property value if exists or fails
%% [(Key,Value), (Key1,Value1)......]
get_property_value(Key, [(Key,Value)|_], Value).
get_property_value(Key, [_|T], Value) :- get_property_value(Key,T,Value).

:- dynamic memo_table_entry/5.
memo_table_entry(1,s(1,X,Y), s(X,Y), pending, [(module,test)]).





memo_table(Nr,OrigCall,FiltCall) :- memo_table_entry(Nr,OrigCall,FiltCall,_PEStatus, _Prop).

delete_table :-
	retract(memo_table_entry(_ID,_Patt,_Res,_Status,_Prop)),fail.
delete_table.

%% possible bug here what if p(X) is numbered var to p('A'), and matches p('A')??
find_pattern(ID,CallPattern, ResidualCall, Status) :-
	copy(CallPattern,Copy),
	numbervars(Copy,40,_),   %Start somewhere highish....
	memo_table_entry(ID,Copy, _, Status, _Prop),   
	memo_table_entry(ID,CallPattern,ResidualCall, Status,_Prop).



   
   

lookup_pattern_and_mark(CallPattern,ResidualCall, Status) :-
  /* just like lookup_pattern but marks call as partially evaluated */
   (find_pattern(ID,CallPattern,ResidualCall,_)
    -> (retract(memo_table_entry(ID,CP,RC,_,Prop)),
        assert(memo_table_entry(ID,CP,RC,Status,Prop))
       )
     ; (print('*** lookup_pattern error; fails: '),nl,
        write_canonical(find_pattern(CallPattern,ResidualCall)),nl,
        /*print_memo_table,nl,*/
        ResidualCall = CallPattern)
   ).




insert_pattern_with_filter_types(Call, FilterTypes, ResidualCall, Status) :-
        filter_atom_with_filter_types(Call,FilterTypes,ResidualCall,ID),	
	assert(memo_table_entry(ID,Call, ResidualCall, Status,[])).




	
update_entry(Entry, NewEntry) :-
	retract(Entry),
	assert(NewEntry).



/* Printing */
print_spec_file_header(Module,SpecFile) :-
	reset_memopp,

	
        print_module_declaration(Module),
        print_memo_table,
        print_dynamic_preds,
	print_tabling_directives,
	print_use_libraries_and_ops,
	print_residual_predicates,
	print_include_statement(SpecFile).




print_residual_predicates :-
	residual_predicate(Head,Body),
	assert_memo_clause(Head,Body),
	portray_clause((Head:-Body)),
	fail.
print_residual_predicates.


print_include_statement(SpecFile) :-
	assert_memo_clause(':-'(include(SpecFile))).
	%print(':- include(\''), print(SpecFile),print('\').'), nl.


print_use_libraries_and_ops :- 
   use_module_in_specfile(Lib),
   assert_memo_clause(':-'(use_module(Lib))),
   %%%% print(' :- use_module(library('), print(Lib), print(')).'),nl,
   fail.

print_use_libraries_and_ops :-
   op_def_gx(Prio,Type,Symb),
 /* print op declarations, e.g., in case user input is read from keyboard or file in .gx or .spec*/
   assert_memo_clause(':-'(op(Prio,Type,Symb))),
   %%%% print(' :- '),print(op(Prio,Type,Symb)), print('.'),nl,
   fail.
print_use_libraries_and_ops.   
 
/* print the top module declaration for the .spec files */
print_module_declaration(_) :-
   get_preference(prolog_mode_flag,xsb),!,nl.
print_module_declaration(Module) :-
   findall(PredInfo, (memo_table(_Id,_Call,ResidualCall),
                      get_predicate_info(ResidualCall,PredInfo)), ExportedPreds),
   assert_memo_clause(':-'(module(Module, ExportedPreds))).
   %%%print(':- module(\''),print(Module), print('\','), print(ExportedPreds),print(').'),nl.
   


print_memo_table :- get_preference(memo_table_printing,true),!,
    (get_preference(prolog_mode_flag,ciao) -> (print(' :- use_package(assertions).'),nl)
    ;
	(gennum(X),assert_memo_clause(gennum(num_num,X))
	    %%%%,print(gensym(num__num, X)), print('.'), nl
	)
    ),		   
    print_memo_table3(_,_,_).
print_memo_table.

print_memo_table3(Id,Call,ResidualCall) :-
    memo_table_entry(Id,Call,ResidualCall,Status,Prop),
    %numbervars((Call,ResidualCall,Prop),0,_),
    print_memo_table_entry(Id,Call,ResidualCall,Status,Prop),
    fail.
print_memo_table3(_,_,_).

print_memo_table_entry(_Id,Call,ResidualCall,_Status,_Prop) :-
   get_preference(prolog_mode_flag,ciao),!,
   print(' :- true pred '), print(Call),
   print(': true + '),
   print(equiv(ResidualCall)),
   print('.').
print_memo_table_entry(Id,Call,ResidualCall,Status,Prop) :-
     assert_memo_clause(memo_table(Id,Call,ResidualCall,Status,Prop)).
     %%%%print(memo_table(Id,Call,ResidualCall,Status,Prop)), print('.').


print_tabling_directives :- 
    memo_table(_Id,Call,ResidualCall),
    table_gx(Call), /* check whether call has to be tabled */
    numbervars((Call,ResidualCall),0,_),
    (memo:xsb_mode_is_on -> true ; (print('  /'), print('*'))),
    functor(ResidualCall,RFunc,RArity),
    print(' :- table '), print(RFunc/RArity),print('.'),
    (memo:xsb_mode_is_on -> true ; (print('*'), print('/'))),
    nl,
    fail.
print_tabling_directives.


print_dynamic_preds :-
   dynamic_gx(Func/Arity),
   functor(Call, Func,Arity),
   memo_table(_Id,Call,ResidualCall),
   functor(ResidualCall,RFunc,RArity),
   assert_memo_clause(':-'(dynamic(RFunc/RArity))),
   %%%% print(' :- dynamic '), print(RFunc/RArity), print('.'),nl,
   fail.
print_dynamic_preds :-
   dynamic_gx(Func/Arity),
   functor(Call, Func,Arity),   
   \+(memo_table(_,Call,_)),
   assert_memo_clause(':-'(dynamic(Func/Arity))),   
   %%%%print(' :- dynamic '), print(Func/Arity),print('.'),nl,
   fail.
print_dynamic_preds.




/* ---------------------------------------------------------- */
/* filter_atom(Atom,ResultingFilteredAtom) */
/* ---------------------------------------------------------- */


filter_atom(Atom,FilteredAtom,NrOfPredToGenerate) :-
	gennum(NrOfPredToGenerate),
	findall(AArgTypes,filter(Atom,AArgTypes),ListOfArgTypes),
	((ListOfArgTypes\=[])
	 -> ((member(ArgTypes,ListOfArgTypes),
	      filter_atom_with_filter_types(Atom,ArgTypes,FilteredAtom,NrOfPredToGenerate))
	      -> true
	      ; (print_error('*** WARNING: no matching filter declarations for: '), print_error(Atom),
	         FilteredAtom = unable_to_filter(Atom))
	    )
	 ;  (print_error('*** WARNING: no filter declarations for: '), print_error(Atom),
	     FilteredAtom = no_filter(Atom))
	).

filter_atom_with_filter_types(Atom,AllArgTypes,FilteredAtom,NrOfPredToGenerate) :-
%	gennum(NrOfPredToGenerate),
	((Atom =..[Predicate|Arguments],
%	  name(PE_Sep,"__"),
%	  string_concatenate(Predicate,PE_Sep,IntPred),
%	  string_concatenate(IntPred,NrOfPredToGenerate,NewPred),
	  make_new_predicate_name(Predicate, NrOfPredToGenerate, NewPred, FilteredAtom),
	  
	  member(ArgTypes,AllArgTypes),
	  l_type_filter(ArgTypes,Arguments,[],RDynamicArguments))
	 -> (reverse(RDynamicArguments,DynamicArguments),
	     FilteredAtom =.. [NewPred|DynamicArguments])
	 ;  (print_error('*** WARNING: error in filter: '), print_error(Atom),
	     print_error(AllArgTypes),
	     FilteredAtom = error_in_filter(Atom))
	).

make_new_predicate_name(Predicate, NrOfPredToGenerate, NewPred, FilteredAtom) :-
	(nonvar(FilteredAtom) ->
	    NrOfPredToGenerate = FilteredAtom   %% if already named, use same name
	;
	    (gennum(NrOfPredToGenerate),
             name(PE_Sep,"__"),
             string_concatenate(Predicate,PE_Sep,IntPred),
	     string_concatenate(IntPred,NrOfPredToGenerate,NewPred))
	).


:- use_module(intersectiontypes).

%%added by steve
type_filter(clp,Argument,ArgList,[Argument|ArgList]). /* treat as dynamic */
type_filter(clp(_,_),Argument,ArgList,[Argument|ArgList]). /* treat as dynamic */


type_filter(static,_Argument,ArgList,ArgList) :- ground(_Argument).
    /* mal: added groundness check so that we can use static in disjunctive types */
type_filter(dynamic,Argument,ArgList,[Argument|ArgList]).
type_filter(semi,Argument,ArgList,[Argument|ArgList]). /* treat as dynamic */
type_filter(free,_Argument,ArgList,ArgList) :- var(_Argument).
         /* mal: added var check so that we can use free in disjunctive types */
type_filter(static_or_free,_Argument,ArgList,ArgList).
        /* this corresponds to the original static of the '96 paper, in case somebody needs it */
type_filter(nonvar(Type),Argument,InArgList,OutArgList) :-
    nonvar(Argument),
    Argument =.. [_F|FArgs],
    l_type_filter2(Type,FArgs,InArgList,OutArgList).
type_filter(nonvar,Argument,InArgList,OutArgList) :-
    nonvar(Argument),
    Argument =.. [_F|FArgs],
    rev_acc(FArgs,InArgList,OutArgList).
type_filter(nonvar_nf,Argument,ArgList,[Argument|ArgList]). /* nonvar_no filter */
type_filter(nonnonvar,Argument,InArgList,OutArgList) :-
    nonvar(Argument),
    Argument =.. [_F|FArgs],
    make_copies(FArgs,nonvar,TList),
    l_type_filter(TList,FArgs,InArgList,OutArgList).
type_filter((Type1 ; _Type2),Argument,InArgList,OutArgList) :-
    type_filter(Type1,Argument,InArgList,OutArgList).
type_filter((_Type1 ; Type2),Argument,InArgList,OutArgList) :-
    type_filter(Type2,Argument,InArgList,OutArgList).
type_filter(type(F),Argument,InArgList,OutArgList) :-
    typedef(F,TypeExpr),
    type_filter(TypeExpr,Argument,InArgList,OutArgList).
type_filter(list(F),Argument,InArgList,OutArgList) :-
    typedef(list(F),TypeExpr),
    type_filter(TypeExpr,Argument,InArgList,OutArgList).
type_filter(struct(F,TArgs),Argument,InArgList,OutArgList) :-
    nonvar(Argument),
    Argument =.. [F|FArgs],
    l_type_filter(TArgs,FArgs,InArgList,OutArgList).
type_filter([IT1|RT],Argument,InArgList,OutArgList) :-
     filter_intersection_type([IT1|RT],Argument,InArgList,OutArgList).

l_type_filter([],[],Args,Args).
l_type_filter([Type1|TT],[A1|AT],InArgList,OutArgList) :-
    type_filter(Type1,A1,InArgList,IntArgList),
    l_type_filter(TT,AT,IntArgList,OutArgList).
    
l_type_filter2(_,[],Args,Args).
l_type_filter2(Type1,[A1|AT],InArgList,OutArgList) :-
    type_filter(Type1,A1,InArgList,IntArgList),
    l_type_filter2(Type1,AT,IntArgList,OutArgList).


rev_acc([],Args,Args).
rev_acc([A1|AT],InArgList,OutArgList) :-
    rev_acc(AT,[A1|InArgList],OutArgList).




filter_atom_wo(Atom,VarsToIgnore,FilteredAtom,NrOfPredToGenerate) :-
	gennum(NrOfPredToGenerate),
	varlist(Atom,CompleteVarlist),
	flatten(VarsToIgnore,FVarsToIgnore),
	remove_vars(FVarsToIgnore,CompleteVarlist,Varlist),
	Atom =..[Predicate|_Arguments],
	name(PE_Sep,"__"),
	string_concatenate(Predicate,PE_Sep,IntPred),
	string_concatenate(IntPred,NrOfPredToGenerate,NewPred),
	FilteredAtom =.. [NewPred|Varlist].

remove_vars([],Vars,Vars).
remove_vars([V|Rest],InVars,OutVars) :-
	remove_var(V,InVars,IntVars),
	remove_vars(Rest,IntVars,OutVars).

remove_var(_V,[],[]).
remove_var(V,[H|T],OutVars) :-
	((V==H)
	  -> (OutVars = T) /* supposing V occurs at most once */
	  ;  (OutVars = [H|OutT], remove_var(V,T,OutT))
	).



difference_varlist(Term1,Term2,Vars12) :- 
	varlist(Term1,Vars1),
	varlist(Term2,Vars2),
	memo:difference(Vars1,Vars2,Vars12).

difference([],_,[]).
difference([V|T],Vs,R) :-
  vmember(V,Vs),!,
  memo:difference(T,Vs,R).
difference([V|T],Vs,[V|R]) :-
  memo:difference(T,Vs,R).


vmember(H,[X|T]) :- (X==H -> true ; vmember(H,T)).





%%code graveyard below


%lookup_pattern_and_mark(CallPattern,ResidualCall) :-
%  /* just like lookup_pattern but marks call as partially evaluated */
%   (find_pattern(ID,CallPattern,ResidualCall)
%    -> (retract(memo_table_entry(ID,CP,RC,pending(Flag),Prop)),
%        assert(memo_table_entry(ID,CP,RC,done(Flag),Prop))
%       )
%     ; (print('*** lookup_pattern error; fails: '),nl,
%        write_canonical(find_pattern(CallPattern,ResidualCall)),nl,
%        /*print_memo_table,nl,*/
%        ResidualCall = CallPattern)
%   ).


/* REMOVED steve 29/09/03 Use /4
insert_pattern(Call,ResidualCall) :-
	filter_atom(Call,ResidualCall,ID),
	assert(memo_table_entry(ID,Call,ResidualCall,pending(internal),[])).
	*/
/* REMOVED Steve 29/09/03, use /4 this shouldnt be called	
insert_pattern_with_filter_types(Call,FilterTypes,ResidualCall)  :-
	(find_pattern(ID, Call, ResidualCall, request(Status)) ->
	    %% if pattern is already in table then update status
	    update_entry(memo_table_entry(ID, Call, ResidualCall, request(Status),Prop),
			 memo_table_entry(ID, Call, ResidualCall, pending(Status),Prop))
	;	    	    	
	    filter_atom_with_filter_types(Call,FilterTypes,ResidualCall,ID),
	    assert(memo_table_entry(ID,Call,ResidualCall,pending(internal),[]))
	).
*/


%lookup_pattern(CallPattern,ResidualCall) :- /* just like find_pattern but does not fail */
%   (find_pattern(CallPattern,ResidualCall) -> true
%     ; (print('*** lookup_pattern error; fails: '),nl,
%        write_canonical(find_pattern(CallPattern,ResidualCall)),nl,
%        /*print_memo_table,nl,*/
%        ResidualCall = CallPattern)
%   ).


%%% this should only be true if status flag in memo table is true
%%% TODO check status flag
%find_pattern(CallPattern,ResidualCall) :-
%    find_pattern(_,CallPattern,ResidualCall).

%% find pattern but ignore status
%find_pattern(ID,CallPattern,ResidualCall) :-
%	find_pattern(ID, CallPattern, ResidualCall, _).

%% find pattern and return status as well
