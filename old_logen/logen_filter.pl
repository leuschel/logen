
:- module('logen_filter', [typedef/2,
                           filter_atom_with_filter_types/4,
                           generalise_with_filter_types/3]).



:- use_module(annfile, [user_type/2]).
:- use_module(logen_messages).
:- use_module(library(lists)).
:- use_module(logen_attributes).
:- use_module(library(terms),[term_variables/2]).

typedef(list(T),(struct([],[]) ; struct('.',[T,type(list(T))]))).
typedef(static_or_dynamic,(static ; dynamic)).
/* typedef(model_elim_literal,(struct(pos,[nonvar]) ; struct(neg,[nonvar]))).
typedef(channel, (struct(io,[type(list(dynamic)),nonvar]))).
typedef(rename_unit, (struct('<-',[type(channel),type(channel)]))).
typedef(csp,(struct(stop,[]) ;
             struct(skip,[]) ;
             struct(omega,[]);
             struct(agent_call,[nonnonvar]) ;
             struct(rename,[type(csp),type(list(type(rename_unit)))]) ;
             struct(hide,[type(csp),type(list(type(channel)))]) ;
             struct(prefix,[type(list(nonvar)),static,type(csp)]) ;
             struct(prefix,[type(list(nonvar)),static,nonvar,type(csp)]) ;
             struct(interleave,[type(csp),type(csp)]) ;
             struct(par,[type(csp),type(list(type(channel))),type(csp)]) ;
             struct(choice,[type(csp),type(csp)]) ;
             struct(int_choice,[type(csp),type(csp)]) ;
             struct(let,[dynamic,dynamic,type(csp)]) ;
             struct(if,[nonvar,type(csp),type(csp)]) ;
             struct(if,[nonvar,type(csp)])
	     )).
*/
typedef(Type,Def) :- user_type(Type,Def).  /* from annfile  */



/* ---------------------------------------------------------- */
/* filter_atom(Atom,ResultingFilteredAtom) */
/* ---------------------------------------------------------- */


filter_atom_with_filter_types(Module,Atom,AllArgTypes,FilteredAtom) :-
        Atom =..[Predicate|Arguments],
	((member(ArgTypes,AllArgTypes),
	  l_type_filter(ArgTypes,Arguments,[],RDynamicArguments))
	 -> (reverse(RDynamicArguments,DynamicArguments),
	     make_new_predicate_name(Module, Predicate, NewPred, FilteredAtom),
	     FilteredAtom =.. [NewPred|DynamicArguments])
	 ;  (print_error('*** WARNING: unable to filter: '), print_error(Atom),
	     print_error(AllArgTypes),
	     FilteredAtom = error_in_filter(Atom),
	     detect_error(AllArgTypes,Arguments))
	).
	
	
detect_error([ArgTypes],Arguments) :- !,
  /* if a single filter declaration: find source of error */
   l_detect_error(ArgTypes,Arguments).
detect_error(_,_).

l_detect_error([],[]).
l_detect_error([Type1|TT],[A1|AT]) :-
    (type_filter(Type1,A1,[],_)
     ->  l_detect_error(TT,AT)
     ;   (print_error(filter_argument_not_of_type(A1,Type1)),
          (type_check(Type1,A1)->true;true))
    ).

make_new_predicate_name(Module, Predicate, NewPred, FilteredAtom) :-
	(nonvar(FilteredAtom) ->
            FilteredAtom =.. [NewPred|_]   % already named
	;
            (getattr(Module, gensym, N) -> true ; N=0),
            N1 is N+1,
            setattr(Module, gensym, N1),
            name(PE_Sep,"__"),
            string_concatenate(Predicate,PE_Sep,IntPred),
	        string_concatenate(IntPred,N,NewPred)
	).


string_concatenate(X,Y,XY) :-
   name(X,Xs),name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).

% ____________________________________________________________

:- use_module(intersectiontypes).

%%added by steve
type_filter(clp,Argument,ArgList,[Argument|ArgList]). /* treat as dynamic */
type_filter(clp(_,_),Argument,ArgList,[Argument|ArgList]). /* treat as dynamic */


type_filter(static,_Argument,ArgList,ArgList) :- ground(_Argument).
type_filter(static_nf,Argument,ArgList,[Argument|ArgList]) :- ground(Argument).
    /* mal: added groundness check so that we can use static in disjunctive types */
type_filter(dynamic,Argument,ArgList,[Argument|ArgList]).
type_filter(semi,Argument,ArgList,Res) :-  % [Argument|ArgList]) :-
  term_variables(Argument,Vars),
  reverse(Vars,RVars),
  append(RVars,ArgList,Res). /* treat as dynamic */
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
    
    
    
%type_check(T,A) :- print_error(type_check(T,A)),nl,fail.    
%New type checker, for better diagnostic
type_check(clp,_). /* treat as dynamic */
type_check(clp(_,_),_). /* treat as dynamic */
type_check(static,A) :- 
  (ground(A) -> true ; (print_error(not_static(A)),fail)).
type_check(static_nf,A) :- 
  (ground(A) -> true ; (print_error(not_static(A)),fail)).
type_check(dynamic,_).
type_check(semi,_). /* treat as dynamic */
type_check(free,A) :-
  (var(A) -> true ; (print_error(not_free(A)),fail)).
type_check(static_or_free,_).
type_check(nonvar(Type),A) :-
    (nonvar(A) -> (A =.. [_F|FArgs], l_type_check2(Type,FArgs))
               ;  (print_error(not_nonvar(A)),fail)).
type_check(nonvar,A) :-
    (nonvar(A) -> true  ; (print_error(not_nonvar(A)),fail)).
type_check(nonvar_nf,A) :-
    (nonvar(A) -> true  ; (print_error(not_nonvar_nf(A)),fail)).
type_check(nonnonvar,A) :-
    (nonvar(A) -> (A =.. [_F|FArgs],
                   make_copies(FArgs,nonvar,TList),
                   l_type_check(TList,FArgs))
               ; (print_error(not_nonvar(A)))
    ).
type_check((Type1 ; Type2),A) :-
    (type_check(Type1,A)
      -> true
      ;  (type_check(Type2,A) -> true ; print_error(not_or(Type1,Type2,A)))
     ).
type_check(type(F),A) :-
    typedef(F,TypeExpr),
    print_message(informational,typedef(F,TypeExpr,A)),
    type_check(TypeExpr,A).
type_check(list(F),A) :-
    typedef(list(F),TypeExpr),
    type_check(TypeExpr,A).
type_check(struct(F,TArgs),A) :-
    ((nonvar(A),A =.. [F|FArgs])
      -> l_type_check(TArgs,FArgs)
      ;  (print_error(not_struct(F,A)),fail)).
type_check([IT1|RT],A) :-
     (filter_intersection_type([IT1|RT],A,[],_)
      -> true ; (print(not_intersection_type([IT1|RT],A)),fail)).

l_type_check([],[]).
l_type_check([Type1|TT],[A1|AT]) :-
    type_check(Type1,A1),l_type_check(TT,AT).
    
l_type_check2(_,[]).
l_type_check2(Type1,[A1|AT]) :-
    type_check(Type1,A1),l_type_check2(Type1,AT).

    
    
    
    
    
    
/* ---------------------------------------------------------- */
/* generalise_with_filter_types(Atom,FilterTypes,ResultingGenAtom) */
/* ---------------------------------------------------------- */


generalise_with_filter_types(Call,[],GCall) :-  !,
    print_error('*** no filter; unable to generalise: '),print_error(Call),GCall=Call.
generalise_with_filter_types(Call,AllArgTypes,GCall) :-
    ((member(ArgTypes,AllArgTypes),
      Call =.. [F|FArgs],
      l_generalise(ArgTypes,FArgs,GArgs),
      (GCall =..[F|GArgs]) )
    ->
       true
    ;
     (print_error('*** WARNING: unable to generalise: '), print_error(Call),
      GCall = Call,
      Call =.. [_|FArgs],
      detect_error(AllArgTypes,FArgs))
    ).
    
    
    
:- use_module(intersectiontypes).
generalise_using_type(static_nf,Argument,Argument) :- ground(Argument). 
generalise_using_type(static,Argument,Argument) :- ground(Argument). 
           /* mal: added groundness check so that we can use static in disjunctive types,
                  one can avoid the overhead of the check for non-disjunctive types by
                  using static_or_free introduced below */
generalise_using_type(dynamic,_Argument,_FreshVariable).
generalise_using_type(free,_Argument,_FreshVariable) :- var(_Argument).
         /* mal: added var check so that we can use free in disjunctive types */
generalise_using_type(static_or_free,Argument,Argument).
        /* this corresponds to the original static of the '96 paper, in case somebody needs it */
generalise_using_type(nonvar(Type),Argument,GenArgument) :-
    (nonvar(Argument) ->
     (Argument =.. [F|FArgs],
      make_fresh_variables(FArgs,FVArgs),
      l_generalise2(Type,FVArgs,GArgs),
      GenArgument =..[F|GArgs] )
     ; (fail)
     ).
generalise_using_type(nonvar_nf,Argument,GenArgument) :-  generalise_using_type(nonvar,Argument,GenArgument).
generalise_using_type(nonvar,Argument,GenArgument) :-
    (nonvar(Argument) ->
     (Argument =.. [F|FArgs],
      make_fresh_variables(FArgs,GArgs),
      GenArgument =..[F|GArgs] )
     ; (fail)
     ).
generalise_using_type(nonnonvar,Argument,GenArgument) :-
    (nonvar(Argument) ->
     (Argument =.. [F|FArgs],
      make_copies(FArgs,nonvar,TList),
      l_generalise(TList,FArgs,GArgs),
      GenArgument =..[F|GArgs] )
     ; (fail)
     ).
generalise_using_type((Type1 ; _Type2),Argument,GenArgument) :-
    generalise_using_type(Type1,Argument,GenArgument).
generalise_using_type((_Type1 ; Type2),Argument,GenArgument) :-
    generalise_using_type(Type2,Argument,GenArgument).
generalise_using_type(list(X),Argument,GenArgument) :-
    /* special case for list: so that users do not have to use type(list(.)) */
    typedef(list(X),TypeExpr),
    generalise_using_type(TypeExpr,Argument,GenArgument).
generalise_using_type(type(F),Argument,GenArgument) :-
    typedef(F,TypeExpr),
    generalise_using_type(TypeExpr,Argument,GenArgument).
generalise_using_type(struct(F,TArgs),Argument,GenArgument) :-
    (nonvar(Argument) ->
     (Argument =.. [F|FArgs],
      l_generalise(TArgs,FArgs,GArgs),
      GenArgument =..[F|GArgs] )
     ; (fail)
     ).
generalise_using_type(semi,Argument,Argument). /* treat as static for generalisation */
generalise_using_type([IT1|RT],Argument,GenArgument) :-
    generalise_using_intersection_type([IT1|RT],Argument,GenArgument).

%%added by Steve
generalise_using_type(clp,Argument,Argument). /* treat as static for generalisation */
generalise_using_type(clp(_,_),Argument,Argument). /* treat as static for generalisation */


l_generalise([],[],[]).
l_generalise([Type1|TT],[A1|AT],[G1|GT]) :-
    generalise_using_type(Type1,A1,G1),
    l_generalise(TT,AT,GT).
    
l_generalise2(_,[],[]).
l_generalise2(Type1,[A1|AT],[G1|GT]) :-
    generalise_using_type(Type1,A1,G1),
    l_generalise2(Type1,AT,GT).
    
    
    
/* needs access to filter */
/* Not used anymore ??!!
generalise(Call,GCall) :-
	findall(AArgTypes,filter(Call,AArgTypes),ListOfArgTypes),
    (( ((ListOfArgTypes\=[]) -> true ; (print_error('*** no filter '),fail)),
      member(ArgTypes,ListOfArgTypes),
      Call =.. [F|FArgs],
      l_generalise(ArgTypes,FArgs,GArgs),
      (GCall =..[F|GArgs]) )
    ->
       true
    ;
     (print_error('*** WARNING: unable to generalise: '), print_error(Call),
      print_error('*** Filters: '), print_error(ListOfArgTypes),
      GCall = Call)
    ).
  ---- */
    


make_fresh_variables([],[]).
make_fresh_variables([_|T],[_|FT]) :-
    make_fresh_variables(T,FT).
    