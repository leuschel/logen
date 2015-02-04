/* ----------------------------- */
/* An interpreter for B machines */
/* working from an XML encoding  */
/* by Michael Leuschel, 2001     */
/* ----------------------------- */

:- ['bsets_clp.pl'].
:- ['bsets_clp_1.pl'].
:- ['BTypes.pl'].

:- use_module(itracer).
:- use_module(xml_b_parser).
:- use_module(tools).
:- use_module(library(lists)).
:- use_module(store).
:- use_module(self_check).


/* --------------------------------------------------------- */

/*

Current assumptions:

 All global variables of the machine must be properly initialised
  in the Initialisation section; the statements in the section must
  be executable in sequence they appear.
 If a global variable is not initialised in the Initialisation section
  then no space will be allocated for it.
  
 All global sets are mapped to finite domain variables and can thus
 currently not be mixed (union, intersection,...) with ordinary sets
 represented as Prolog lists. Also, global sets can currently
 not be unioned, intersected, ... in between themselves, although that
 could be achieved.
  
*/
	
/* --------------------------------------------------------- */

/* definitions for use with the itracer */

operation(initialise_machine,[],IState,AbortVal) :-
     b_check_machine,
     b_initialise_machine(IState),
     (b_state_is_abort(IState) -> (AbortVal = init_aborts) ; (AbortVal = false)).
     
operation(Operation,InState,NewState,AbortVal) :- \+ (InState = []),
     b_execute_operation(Operation,InState,NewState,AbortVal).

/* --------------------------------------------------------- */


	
/* --- Basic Machine Manipulations --- */                                


/* get the name of the current B machine */
b_machine_name(Name) :-
  machine(M),
  get_arguments(['Header'],M,HA),
  get_name(HA,Name).
  
b_global_set(Set) :-
  machine(M),
  get_arguments(['Sets','Declaration'],M,DA), 
  get_identifier(DA,Set).
  
b_check_machine :-
  b_machine_name(N), 
  print('Machine: '),print(N),nl,
  b_check_operations,
  b_check_global_set_type_definitions,
  b_check_var_type_definitions.
  
b_check_global_set_type_definitions :-
  print(' checking existence of type definitions: '),
  b_global_set(Set),
  (b_fd_type(Set,LowBnd,UpBnd) 
    -> (print(Set),print('=='),print(LowBnd),print('..'),print(UpBnd))
    ;  (nl,print('### no type/2 definition for global set: '),
        print(Set),nl,
        print('### add the following to the DEFINTIONS section: '),
        print('scope_'),print(Set),print(' == 1..3'),nl)
   ),print(' '),
   fail.
b_check_global_set_type_definitions :- nl.
   
b_check_operations :-
  print(' checking operations: '),
  b_operation(O,P,R,_B), 
  Op =.. [O|P],
  ((R=[]) -> true ; (print(R), print(':='))),
  print(Op),print(' '),
  fail.
b_check_operations :- nl.
  
/* compute the inital state of the current B machine */
b_initialise_machine(InitialState) :-
  machine(M),
  get_arguments(['Initialisation'],M,InitArgs),
  b_set_up_valid_state(FirstState),
  b_execute_statements_in_parallel(InitArgs,[],FirstState,InitialUpdates),
  store_updates(InitialUpdates,FirstState,InitialState),
  b_check_valid_state(InitialState).
  
b_state_is_abort(State) :-
 lookup_value(abort__flag__info,State,AbortVal),
 AbortVal \= false.
  
/* extract operation names, parameters, and body form current B machine */
b_operation(Name,Parameters,Results,Body) :-
  machine(M),
  get_arguments(['Operations','Operation'],M,OA),
  get_arguments(['Header'],OA,HA),
  get_name(HA,Name),
  (member('Parameters'(_,_),HA)
    -> (get_arguments(['Parameters','ListIdent'],HA,PA),
        l_get_identifier(PA,Parameters))
    ;  (Parameters = [])
  ),
  (member('Results'(_,_),HA)
    -> (get_arguments(['Results','ListIdent'],HA,RA),
        l_get_identifier(RA,Results))
    ;  (Results = [])
  ),
  (OA = [_Header,Body] -> true
      ;  (print('### illegal operation argument list:'),
          print(OA),nl,Body = fail)
  ).
  
  
/* --------------------------------------------------------- */
/* Extracting Finite Domain type information from the B machine */
/* --------------------------------------------------------- */

/* below treats annotations in the form:
DEFINITIONS
    scope_Name == 1..3;
    scope_Code == 4..8
 which inform us about which finidte domain ranges we should
  give to global sets defined in SETS
*/

extract_fd_range_from_machine(GlobalSetName,LowBound,UpBound) :-
  machine(M),
  member('Definitions'(_,_),M), /* check whether Definitions section exists */
  get_arguments(['Definitions','Definition'],M,DA),
  get_arguments(['Header','Name'],DA,DefA),
  get_identifier(DefA,Scope_Name),
  name(Scope_Name,[115,99,111,112,101,95|Arg]),
  name(GlobalSetName,Arg),
   /* the special keyword for fd annotations in the source */
  get_arguments(['NatRange'],DA,FDRange),
  debug_print('********************** FDRange = '), debug_print(FDRange), debug_nl,
  FDRange = ['Integer'(LowBound,_),'Integer'(UpBound,_)].
  
 
:- assert_pre(user:b_fd_type(_G,_L,_U),true).
:- assert_post(user:b_fd_type(G,L,U),(atomic(G),(integer(L),integer(U)))).

b_fd_type(GlobalSetName,LowBound,UpBound) :-
  b_global_set(GlobalSetName),
  (extract_fd_range_from_machine(GlobalSetName,LowBound,UpBound)
    -> true
    ;  (LowBound=1,UpBound=3)
   ).
 

global_type(fd(X,GlobalSet),GlobalSet) :- b_global_set(GlobalSet),
   b_fd_type(GlobalSet,Low,Up),
   X in Low..Up.

not_global_type(X,GlobalSet) :- b_global_set(GlobalSet), 
   ( when(nonvar(X),\+(X=fd(_,_)))
    ;
     (X = fd(_,G2), when(nonvar(G2), dif(G2,GlobalSet)))
   ).
not_global_type(fd(X,GlobalSet),GlobalSet) :- b_global_set(GlobalSet),
     b_fd_type(GlobalSet,Low,Up),
     (X #>Up #\/ X#<Low).
  
/* -----------------------------*/
/*      b_execute_operation     */
/* -----------------------------*/
/* execute a single operation */
b_execute_operation(FullOperation,InState,NewState,AbortVal) :-
    b_execute_operation(_,FullOperation,InState,NewState,AbortVal).
    
b_execute_operation(Name,FullOperation,InState,NewState,AbortVal) :-
    b_operation(Name,Parameters,Results,Body),
    same_length(Parameters,FreshVars),
    set_up_localstate(Parameters,FreshVars,[],LocalState),
    /* same_length(Results,RFreshVars),
       set_up_localstate(Results,RFreshVars,LocalState0,LocalState), */
    Operation =.. [Name|FreshVars],
    debug_nl,
    debug_print('Attempting Operation ------------> '),
    debug_print(Operation),debug_nl,
    debug_print(call(b_execute_top_level_statement(Body,LocalState,InState,OutState))),nl,
    b_execute_top_level_statement(Body,LocalState,InState,OutState),
    lookup_value_for_existing_id(abort__flag__info,OutState,InState,AbortVal), 
       /* first look in OutState then in InState */
    debug_print('Operation Successful ------> '),
    get_results(Results,OutState,ResultValues,NewOutState),
    debug_print(results(ResultValues)),debug_nl,
    ((ResultValues = [])
      -> /* we have an ordinary operation */
         (FullOperation = Operation)
      ;  (FullOperation =.. ['-->',Operation,ResultValues])
      ),
    debug_print(NewOutState),debug_nl,
    store_updates(NewOutState,InState,NewState) /* actually execute the updates
     to the global store computed by b_execute_statement */.
    
get_results([],OutState,[],OutState).
get_results([R|Results],OutState,[RV|ResultValues],NewOutState) :-
    lookup_and_delete_value(R,RV,OutState,OutState2),
    get_results(Results,OutState2,ResultValues,NewOutState).
  
/* -----------------------------------------*/
/*     b_execute_statements_in_parallel     */ 
/* -----------------------------------------*/
/* execute a parallel composition of statements */
b_execute_statements_in_parallel([],_,_,[]).
b_execute_statements_in_parallel([Stmt|TS],LocalState,InState,OutState) :-
  b_execute_statement(Stmt,LocalState,InState,OutState1),
  debug_print(par1(OutState1)),debug_nl,
  b_execute_statements_in_parallel(TS,LocalState,InState,OutState2),
  debug_print(app(OutState1,OutState2)),debug_nl,
  append(OutState1,OutState2,OutState). /* merge updates */


%My Code
b_compute_expressions([], _, _, []).
b_compute_expressions([EXPRsHd|EXPRsTl],LocalState,InState,[ValHd|ValTl]) :-
  b_compute_expression(EXPRsHd,LocalState,InState,ValHd),
  b_compute_expressions(EXPRsTl,LocalState,InState,ValTl).   

%My Code
store_values([], [], []).
store_values([IDsHd|IDsTl], [VALsHd|VALsTl], ResState) :-
  ((VALsHd = abort(AbortMsg))
   -> (store_value(abort__flag__info,AbortMsg,[],[OutAbortHd]),
       store_value(IDsHd,VALsHd,[],[OutStateHd]),
       print(abort_multi_assign(AbortMsg)),nl,
       ResState = [OutAbortHd,OutStateHd|OutStateTl])
   ;  (store_value(IDsHd,VALsHd,[],[OutStateHd]),
       ResState = [OutStateHd|OutStateTl])
   ),
  store_values(IDsTl, VALsTl, OutStateTl).


/* ---------------------------------------*/
/*     b_execute_top_level_statement      */
/* ---------------------------------------*/

/* this is just like b_execute_statement with the
 only difference that a PRE-condition is treated like
 a select, i.e., it does not generate an abort if used
 outside of its condition */

b_execute_top_level_statement('SubstitutionPrecondition'(_,[PreCond,Body]),
   LocalState,InState,OutState) :- !,
  debug_print(top_level_pre),debug_nl,
  b_test_boolean_expression(PreCond,LocalState,InState), %check PRE
  debug_print(top_level_precondition_ok),debug_nl,
  b_execute_statement(Body,LocalState,InState,OutState),
  debug_print(newstate(OutState)),debug_nl.
b_execute_top_level_statement(XML,LocalState,InState,OutState) :- 
  b_execute_statement(XML,LocalState,InState,OutState).

/* -----------------------------*/
/*     b_execute_statement      */
/* -----------------------------*/
/* b_execute_statement(XMLStatements:input, StateofLocalVariables:input,
                      StateOfGlobalVariables:input,
                      ListOfUpdateBindingsForGlobalVariablesAndResultVariables:output) */

%My Code
b_execute_statement('SubstitutionBecomeEqualVariables'(_,[LHS,'ListExpression'(_, EXPRs)]),  
/*  Var1, ..., VarN := Expr1, ..., ExprN */
   LocalState,InState,OutState) :- !,
  debug_print('assign: '),
  get_identifiers(LHS, IDs), %IDs - a list of variables from LHS
  b_compute_expressions(EXPRs,LocalState,InState,VALs),% VALs - a list of computed values 
  store_values(IDs, VALs, OutState).

b_execute_statement('SubstitutionBecomeEqualVariables'(_,[LHS,RHS]),  
  /*  LHS := RHS */
   LocalState,InState,OutState) :- !,
  debug_print('assign: '),  
  get_identifier_from_single_arg(LHS,Id), debug_print(id), debug_print(' := '),
  b_compute_expression(RHS,LocalState,InState,Val), debug_print(val(Val)),debug_nl,
  ((Val = abort(AbortMsg),
   store_value(abort__flag__info,AbortMsg,[],S1),print(abort_assign(AbortMsg)),nl,
   store_value(Id,Val,S1,OutState))
  ;
  (not_top_level_functor(Val,abort),
   store_value(Id,Val,[],OutState)
  )).

b_execute_statement('SubstitutionBecomeEqualFunction'(_,
                     ['CallFunction'(_,[Fun,FunArg]), RHS]),     
		     /*  Fun(X)  :=  RHS */
   LocalState,InState,OutState) :- !, debug_print(assignfun),debug_nl,
  get_identifier_from_single_arg(Fun,Fid), debug_print(Fid),
  b_compute_expression(FunArg,LocalState,InState,ArgVal),
  debug_print(ArgVal),  debug_print(' := '),
  b_compute_expression(RHS,LocalState,InState,RHSVal), 
  debug_print(rhsval(RHSVal)),debug_nl,
  lookup_value_for_existing_id(Fid,InState,DB),
   /* we do not examine local state: these vars cannot be modified by operations !? */
  override(DB,ArgVal,RHSVal,DB2), /* <---- */
  store_value(Fid,DB2,[],OutState).



b_execute_statement('SubstitutionPrecondition'(_,[PreCond,Body]),
   LocalState,InState,OutState) :- !,
  print('*** Non-outermost PRE construct'),nl,
  (  (b_test_boolean_expression(PreCond,LocalState,InState),
         /* PRECONDITION SATISFIED */
      print('PRE ok'), nl,
      b_execute_statement(Body,LocalState,InState,OutState),
      debug_print(newstate(OutState)),
      debug_nl
     )
   ;
     (b_not_test_boolean_expression(PreCond,LocalState,InState),
          /* PRE-CONDITION VIOLATED */
      print('PRE false  => ABORT'),nl,
      store_value(abort,pre_condition_violated(PreCond),InState,OutState)
     )
  	
  ).

%My Code
b_execute_statement('SubstitutionSelect'(_, [ PreCond, 'Then'(_,[Body]) | SubstSelTl ]),
  LocalState, InState, OutState) :- !,
  (
	(  
		b_test_boolean_expression(PreCond,LocalState,InState), %check PRE
		print('In PreCond'), nl,
		b_execute_statement(Body,LocalState,InState,OutState)
  	);
  	(	
		member('When'(_, [PreCond1, Body1]), SubstSelTl),
		b_test_boolean_expression(PreCond1,LocalState,InState), %check PRE
		print('In When'), nl,
		b_execute_statement(Body1,LocalState,InState,OutState)
  	); 
	(	print('In Else'), nl,
	 /* MISSING: test that all other conditions are false */
		member('Else'(_, [Body2]), SubstSelTl), 
		b_execute_statement(Body2,LocalState,InState,OutState)
  	)
  ).

/*
b_execute_statement(SelectInternals, LocalState, InState, OutState) :- !,
	member('Else'(_, [Body2]), SelectInternals), 
	b_execute_statement(Body2,LocalState,InState,OutState).
*/



b_execute_statement('SubstitutionAny'(_,[Variables,PreCond,Body]),   /* ANY _ WHERE */
   LocalState,InState,OutState) :- !, debug_print(any),
   Variables = 'ListIdent'(_,LA),
   l_get_identifier(LA,Parameters), debug_print(par(Parameters)),
    same_length(Parameters,FreshVars),
    set_up_localstate(Parameters,FreshVars,LocalState,NewLocalState),
  debug_print(where),debug_nl,
  b_test_boolean_expression(PreCond,NewLocalState,InState), /* check WHERE */
  debug_print(where_precondition_ok),debug_nl,
  b_execute_statement(Body,NewLocalState,InState,OutState),
  debug_print(newstate(OutState)),debug_nl. /* Missing: CHECK that Variables are actually bound */
b_execute_statement('Then'(_,[Body]),    /* THEN */
   LocalState,InState,OutState) :- !,debug_print(then),
  b_execute_statement(Body,LocalState,InState,OutState).

b_execute_statement('SubstitutionSkip'(_,[]),    /* SKIP */
   _LocalState,_InState,OutState) :- !,debug_print(skip),
   OutState = [].
b_execute_statement('Parallel'(_,Statements),  /*   LHS  || RHS  */ %@@@
   LocalState,InState,OutState) :-  !, debug_print(par),
   b_execute_statements_in_parallel(Statements,LocalState,InState,OutState),
   debug_print(end_par),debug_nl.
b_execute_statement('SubstitutionIf'(_,[Test,'Then'(_,[ThenBody]),'Else'(_,[_ElseBody])]),
   LocalState,InState,OutState) :- !,
  debug_print(if),debug_nl,
  b_test_boolean_expression(Test,LocalState,InState), /* check IF test */
  debug_print(if_test_ok),debug_nl,
  b_execute_statement(ThenBody,LocalState,InState,OutState),
  debug_print(newstate(OutState)),debug_nl.
b_execute_statement('SubstitutionIf'(_,[Test,'Then'(_,[_ThenBody]),'Else'(_,[ElseBody])]),
   LocalState,InState,OutState) :- !,
  debug_print(if),debug_nl,
  b_not_test_boolean_expression(Test,LocalState,InState), /* check negation of IF test */
  debug_print(if_test_ok),debug_nl,
  b_execute_statement(ElseBody,LocalState,InState,OutState),
  debug_print(newstate(OutState)),debug_nl.
b_execute_statement(E,_,_,_) :-
   print(uncovered_statement(E)),nl,fail.
  
  
  
/* -----------------------------*/
/*     b_compute_expression     */
/* -----------------------------*/
b_compute_expression('EmptySet'(_,_),_LState,_State,Res) :- !, empty_set(Res).
b_compute_expression('EmptySequence'(_,_),_LState,_State,Res) :- !, empty_sequence(Res).
b_compute_expression('Identifier'(Id,_),LocalState,State,Value) :- !,
        debug_print(call_lookup_value(Id,LocalState,State,Value)),debug_nl,
        lookup_value_in_store_and_global_sets(Id,LocalState,State,Value),
        debug_print(b_comp_expr_id(Id,Value)),debug_nl.
b_compute_expression('IdentifierComposed'(_,[IEx]),LocalState,State,Value) :- !,
        /* just ignore IdentifierComposed; does it serve any purpose ? */
        /* USEFUL if we refer to other machines */
        b_compute_expression(IEx,LocalState,State,Value).

b_compute_expression('ListExpression'(_,[Ex]),LocalState,State,Value) :- !,
        /* just ignore ListExpression; does it serve any purpose ?
        --- marks a list of expressions separated with comma @@@ */
        b_compute_expression(Ex,LocalState,State,Value).

b_compute_expression('ExtensionSet'(_,[Ex]),LocalState,State,[Value]) :- !,
        /* convert element into singleton set ? */
        b_compute_expression(Ex,LocalState,State,Value).
b_compute_expression('ExtensionSequence'(_,[Ex]),LocalState,State,cons(Value,nil_seq)) :- !,
        /* convert element into singleton sequence ? */
        b_compute_expression(Ex,LocalState,State,Value).
b_compute_expression('OrderedPair'(_,[El1,El2]),LocalState,State,(Val1,Val2)) :- !, debug_print(pair),
        b_compute_expression(El1,LocalState,State,Val1),
        b_compute_expression(El2,LocalState,State,Val2), debug_print(pair(Val1,Val2)).

b_compute_expression(Expression,LocalState,State,Value) :- 
        nonvar(Expression),
        Expression =.. [Op,_,[Arg1]], unary_function(Op,KernelFunction),
       !, debug_print(unary_fun(Op)),
        b_compute_expression(Arg1,LocalState,State,SV1), debug_print(arg1(Op,SV1)),
        expand_global_sets(SV1,ESV1),
        KernelCall =.. [KernelFunction,ESV1,Value],
        call(KernelCall), debug_print(kernel_res(KernelCall)),debug_nl.
b_compute_expression(Expression,LocalState,State,Value) :- 
        Expression =.. [Op,_,[Arg1,Arg2]], binary_function(Op,KernelFunction),
       !, debug_print(binary_fun(Op)),
        b_compute_expression(Arg1,LocalState,State,SV1), debug_print(arg1(Op,SV1)),
        b_compute_expression(Arg2,LocalState,State,SV2), debug_print(arg2(Op,SV2)),
        expand_global_sets(SV1,ESV1),
        expand_global_sets(SV2,ESV2),
        KernelCall =.. [KernelFunction,ESV1,ESV2,Value],
        call(KernelCall), debug_print(kernel_res(KernelCall)),debug_nl.

%My Code
/*
b_compute_expression('Overriding'(_,[Rel,ExtSet]),LocalState,State,Value) :- !, debug_print(union),
        b_compute_expression(Rel,LocalState,State,SV1), debug_print(union_set1(SV1)),
        b_compute_expression(ExtSet,LocalState,State,SV2), debug_print(union_set2(SV2)),
        union(SV1,SV2,Value), debug_print(union_res(Value)),debug_nl.
*/

b_compute_expression(E,_,_,_) :-
   print(uncovered_expression(E)),nl,fail.

expand_global_sets(G,Res) :- /* translate global SETS into explicit sets */
   ((G=global_set(GS)) -> 
       (all_elements_of_type(GS,Res),print('^'),print(GS),print('^ '))
                       ; (Res=G)).
/* IMPROVE to avoid expanding variables */

   
unary_function('First',first_sequence).
unary_function('Front',front_sequence).
unary_function('Tail',tail_sequence).
unary_function('Rev',rev_sequence).
unary_function('Last',last_sequence).
unary_function('Domain',domain).
unary_function('Range',range).

binary_function('ConcatSequence',concat_sequence).
binary_function('Union',union).
binary_function('Intersection',intersection).
binary_function('SetMinus',difference_set).
binary_function('CallFunction',apply_to).
   
/* ----------------------------------*/
/*     b_test_boolean_expression     */
/* ----------------------------------*/
b_test_boolean_expression([],_,_).
b_test_boolean_expression('PredicateParenthesis'(_,[BExpr]), LocalState,State) :- !,
        b_test_boolean_expression(BExpr,LocalState,State).
b_test_boolean_expression('Not'(_,[BExpr]), LocalState,State) :- !,
        b_not_test_boolean_expression(BExpr,LocalState,State).
b_test_boolean_expression('And'(_,[LHS,RHS]), LocalState,State) :- !,
        b_test_boolean_expression(LHS,LocalState,State),
        b_test_boolean_expression(RHS,LocalState,State).
b_test_boolean_expression('Or'(_,[LHS,RHS]), LocalState,State) :- !,
        (b_test_boolean_expression(LHS,LocalState,State)  ;
         b_test_boolean_expression(RHS,LocalState,State)).
b_test_boolean_expression('In'(_,[Rel,'PartialFunction'(_,
                                   ['Identifier'(Dom,[]),'Identifier'(Ran,[])])]),
                          LocalState,State) :-
        b_global_set(Dom),b_global_set(Ran),!,
        print(pf(Dom,Ran)),nl,
        b_compute_expression(Rel,LocalState,State,RV),
        partial_function(RV). /* don't care about Dom,Ran as this should be done by B Type Checking */
b_test_boolean_expression('In'(_,[El,Type]), LocalState,State) :- !,
        b_compute_expression(El,LocalState,State,EV),
        b_compute_expression(Type,LocalState,State,TV),debug_nl,
        ((TV = global_set(GTV))
           -> (debug_print(in_type(EV,GTV)),debug_nl,
               global_type(EV,GTV))
           ;  (debug_print(check_element_of(EV,TV)),debug_nl,
               check_element_of(EV,TV))
         ).
b_test_boolean_expression(Expression,LocalState,State) :-
        Expression =.. [BOP,_,[Arg1,Arg2]],
        binary_boolean_operator(BOP,Kernel_predicate),
        debug_print(binary_bool(BOP)),debug_nl,!,
        /* Sometimes NotEqualSet seems to be called even if the args are not sets !! */
        b_compute_expression(Arg1,LocalState,State,SV1),debug_nl,
        b_compute_expression(Arg2,LocalState,State,SV2),debug_nl,
        KernelCall =.. [Kernel_predicate,SV1,SV2],
        debug_print(kernel_call(KernelCall)),debug_nl,
        call(KernelCall).
b_test_boolean_expression(E,_,_) :-
    print('### uncovered boolean expression'),
    print(E),nl,fail.

binary_boolean_operator('NotSetMemberShip',not_element_of).
binary_boolean_operator('NotEqualSet',not_equal_object).
    /* Sometimes NotEqualSet/EqualSet seems to be called even if the args are not sets !! */
binary_boolean_operator('EqualSet',equal_object).
    
/* --------------------------------------*/
/*     b_not_test_boolean_expression     */
/* --------------------------------------*/
b_not_test_boolean_expression([],_,_).
b_not_test_boolean_expression('PredicateParenthesis'(_,[BExpr]), LocalState,State) :- !,
        b_not_test_boolean_expression(BExpr,LocalState,State).
b_not_test_boolean_expression('Not'(_,[BExpr]), LocalState,State) :- !,
        b_test_boolean_expression(BExpr,LocalState,State).
b_not_test_boolean_expression('And'(_,[LHS,RHS]), LocalState,State) :- !,
        (b_not_test_boolean_expression(LHS,LocalState,State)  ;
         b_not_test_boolean_expression(RHS,LocalState,State)).
b_not_test_boolean_expression('Or'(_,[LHS,RHS]), LocalState,State) :- !,
        b_not_test_boolean_expression(LHS,LocalState,State),
        b_not_test_boolean_expression(RHS,LocalState,State).
b_not_test_boolean_expression('In'(_,[Rel,'PartialFunction'(_,
                                   ['Identifier'(Dom,[]),'Identifier'(Ran,[])])]),
                          LocalState,State) :-
        b_global_set(Dom),b_global_set(Ran),!,
        print(not_pf(Dom,Ran)),nl,
        b_compute_expression(Rel,LocalState,State,RV),
        not_partial_function(RV). /* don't care about Dom,Ran as this should be done by B type checking */
b_not_test_boolean_expression('In'(_,[El,Type]), LocalState,State) :- !,
        b_test_boolean_expression('NotSetMemberShip'(_,[El,Type]), LocalState,State).
b_not_test_boolean_expression('NotSetMemberShip'(_,[El,Set]),LocalState,State) :- !,
        b_test_boolean_expression('In'(_,[El,Set]), LocalState,State).
b_not_test_boolean_expression('EqualSet'(_,[Set1,Set2]),LocalState,State) :-
        debug_print(not(eq_sets)),debug_nl,!,
        b_test_boolean_expression('NotEqualSet'(_,[Set1,Set2]),LocalState,State).
b_not_test_boolean_expression('NotEqualSet'(_,[Set1,Set2]),LocalState,State) :-
        debug_print(not(not_eq_sets)),debug_nl,!,
        b_test_boolean_expression('EqualSet'(_,[Set1,Set2]),LocalState,State).
b_not_test_boolean_expression(E,_,_) :-
    print('### uncovered boolean expression (not)'),
    print(E),nl,fail.
        

  
/* --------------------------------------------------------- */


/* XML Structure manipulations */
  
get_arguments([],XMLArgs,XMLArgs).
get_arguments([Tag|T],XMLArgs,Res) :-
  Struct =.. [Tag,_,NewArgs],
  member(Struct,XMLArgs),
  get_arguments(T,NewArgs,Res).
get_arguments([Tag|_],XMLArgs,_) :-
  Struct =.. [Tag,_,_],
  \+(member(Struct,XMLArgs)),
  print('### illegal XMLArgs in get_arguments'),nl,
  print('### expected tag: '),print(Tag),nl,
  print('### XMLArgs: '),print(XMLArgs),nl,
  fail.
  
get_name(Args,Name) :-
  member('Name'(_,NArgs),Args), /* print(NArgs),nl,*/
  get_identifier(NArgs,Name).  
  
  
l_get_identifier(L,LA) :- 
   map(get_identifier_from_single_arg,L,LA).
   
get_identifier_from_single_arg(Arg,Id) :- get_identifier([Arg],Id).



%My Code
get_identifiers('Identifiers'(_, Args), IDs) :-
    get_identifiers1(Args, IDs).

get_identifiers1([], []).
get_identifiers1(['IdentifierComposed'(_, ['Identifier'(IDsHd, _)])|ArgsTl], [IDsHd|IDsTl]) :-
    get_identifiers1(ArgsTl, IDsTl).




get_identifier(Args,Id) :-  
    member('Identifier'(Id,_),Args).
get_identifier(Args,Id) :-  
    member('IdentifierComposed'(_,Args2),Args),
    get_identifier(Args2,Id).
get_identifier(Args,Id) :-  
    member('Identifiers'(_,Args2),Args),
    get_identifier(Args2,Id).


  
/* --------------------------------- */

lookup_value_in_store_and_global_sets(Id,State,Val) :- 
 (lookup_value(Id,State,Val) -> true 
    ; (b_global_set(Id) -> (Val=global_set(Id))  /* global set evaluates to itself */
        ; (print(lookup_failed(Id,State)),nl,Val=fail))
  ).
lookup_value_in_store_and_global_sets(Id,LocalState,State,Val) :- 
 (lookup_value(Id,LocalState,State,Val) -> true 
    ; (b_global_set(Id) -> (Val=global_set(Id))  /* global set evaluates to itself */
        ; (print(lookup_failed(Id,State)),nl,Val=fail))
  ).  
        
/* --------------------------------- */
    
%My Code
check_and_execute(PreCond, Body, LocalState, InState, OutState) :-
  debug_print(pre),debug_nl,
  b_test_boolean_expression(PreCond,LocalState,InState), %check PRE
  debug_print(precondition_ok),debug_nl,
  b_execute_statement(Body,LocalState,InState,OutState),
  debug_print(newstate(OutState)),debug_nl. 

%My Code
make_abort_list([], []).
make_abort_list([_|IdsTl],['Abort'|ATl]) :-
  make_abort_list(IdsTl, ATl).
  
/* --------------------------------- */



map(_P,[],[]).
map(P,[H|T],[PH|PT]) :-
	Call =.. [P,H,PH],
	call(Call),
	map(P,T,PT).


reduce(_Func,Base,[],Base).
reduce(Func,Base,[H|T],Res) :-
	reduce(Func,Base,T,TRes),
	Call =.. [Func,H,TRes,Res],
	call(Call).
	