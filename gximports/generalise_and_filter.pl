:- module(generalise_and_filter, [basic_binding_type/1,trunc_arglist/3]).

portray_message(informational, _).
:- use_module(library(lists)).
:- dynamic gx_data/2.


%%% ideally we want to be able to specialise generalise and filter on the fly
%%% producing all the code we require just for specialisation...

generalise_call(Filters, Call, GenCall) :-
	Call =.. [F|Args],
	gen_filters_list(Filters,Args, GArgs, _,_,_),
	safe_eqdotdot(GenCall,F,GArgs).

safe_eqdotdot(G,F,A) :- 
       (G=..[F|A] -> true
        ; (format(user_error,"Could not generate structure: ~w=..[~w|~w]~n",[G,F,A]),
           halt(1))
       ).
	
filter_call(Filters, Call, GCall,FCall) :-
	Call =.. [F|Args],
	safe_eqdotdot(GCall,F,GArgs),
	gen_filters_list(Filters, Args, GArgs, FArgs,_,_),
	gensym(F,NewF),
        generate_filter_call(NewF,FArgs,FCall).


generalise_and_filter(Filters, Call, GenCall, FCall) :-
	Call =.. [F|Args],
	gen_filters_list(Filters, Args, GenArgs,FilArgs,_,_),
	safe_eqdotdot(GenCall,F,GenArgs),
	gensym(F,NewF),
	generate_filter_call(NewF,FilArgs,FCall).

generate_filter_call(NewF,FilArgs,FCall) :-
    (FCall =.. [NewF|FilArgs]
     -> true
     ;  (/* too many arguments */
         %format(user_error,"err ~w=..[~w|~w]~n",[FCall,NewF,FilArgs]),
         trunc_arglist(FilArgs,253,NewArgs),
         safe_eqdotdot(FCall,NewF,NewArgs)
        )
     ).

trunc_arglist([],_,[]) :- !.
trunc_arglist([H|T],Rem,Res) :- 
    (Rem>0
     -> (Res = [H|RR], R1 is Rem-1,
         trunc_arglist(T,R1,RR))
     ;  Res = [rem([H|T])]
    ).

%% dont support multiple filters yet..
gen_filters_list(FilterList,Args,GArgs1,FArgs1,OnlineArgs1, OnlineMap1) :-
    member(Filter,FilterList), /* cater for multiple filters */
	l_gen_fil(Filter,Args,[],GArgs,[],FArgs,[],OnlineArgs,[],OnlineMap),
	reverse(GArgs, GArgs1),
	reverse(FArgs, FArgs1),
	reverse(OnlineArgs, OnlineArgs1),
        reverse(OnlineMap, OnlineMap1),
	!.


/* given a Filter list (consisting of a single filter for the moment)
 compute a correction that ensures that the call is covered */
get_uncovered_correction([Filter], Call, InvalidArgs) :-
 /* only works for a single filter for the moment */
	Call =.. [_F|Args],
	l_get_uncovered_correction(Filter,Args,1,InvalidArgs).
	
l_get_uncovered_correction([], [], _Nr, []).
l_get_uncovered_correction([Type|Types], [A|Args], Nr, InvalidArgs) :-
	(gen_fil(Type, A, _, _,_, _,_,_,_,_)
	 -> /* Ok, argument covered by filter */
	    InvalidArgs=IA
	  ; InvalidArgs=[make_dynamic(Nr)|IA]
	 ),
	 N1 is Nr+1,
	 l_get_uncovered_correction(Types,Args,N1,IA).
  
  


l_gen_fil([], [], G,G,F,F,OA,OA,OM,OM).
l_gen_fil([Type|Types], [A|Args], GArgsIn, GArgsOut, FArgsIn, FArgsOut,OIn,OOut,OMIn,OMOut) :-
	gen_fil(Type, A, GArgsIn, GArgsOut1,FArgsIn, FArgsOut1,OIn,OOut1,OMIn,OMOut1),	
	l_gen_fil(Types, Args, GArgsOut1, GArgsOut, FArgsOut1,FArgsOut,OOut1,OOut,OMOut1,OMOut).
l_gen_fil(X, _, _,_,_,_,_,_,_,_) :- \+(X=[]),\+(X=[_|_]),
  format(user,"*** Illegal filter in l_gen_fil; not a list: ~w.~n",[X]),fail.

	

gen_fil(static, A, G,[A|G], F,F,O,O,OM,OM) :- ground(A).
gen_fil(static_nf, A, G,[A|G], F,[A|F],O,O,OM,OM) :- ground(A).  /* static_nf = no filter */

gen_fil(online, A, G,[Var|G], F,F,O,[A|O],OM,[Var|OM]).


gen_fil(dynamic, _, G, [A|G], F,[A|F],O,O,OM,OM).
gen_fil(nonvar, A,GIn,[G|GIn],FIn,FOut,O,O,OM,OM) :- 
	functor(A, Func, Arity),
	functor(G, Func, Arity),	
	G =.. [Func|FArgs],
	reverse(FArgs,RFArgs),
	append(FIn,RFArgs,FOut). 

gen_fil(struct(Func,TArgs), A, GIn,[G|GIn], FIn,FOut,OIn,OOut,OMIn,OMOut) :-
    nonvar(A),
	A =.. [Func|Args],
	l_gen_fil(TArgs, Args,[],GArgs1, FIn,FOut1,OIn,OOut1,OMIn,OMOut1),
	reverse(FOut1,FOut),	
	reverse(GArgs1,GArgs),
	reverse(OOut1,OOut),
	reverse(OMOut1,OMOut),
	G =.. [Func|GArgs].

gen_fil(type(T), A, GI,GO,FI,FO,OIn,OOut,OMIn,OMOut) :-
	typedef(T,Def),
	gen_fil(Def,A,GI,GO,FI,FO,OIn,OOut,OMIn,OMOut).

gen_fil((T1;_T2), A, GIn,GOut, FIn,FOut,OIn,OOut,OMIn,OMOut) :-
	gen_fil(T1,A,GIn,GOut,FIn,FOut,OIn,OOut,OMIn,OMOut).
gen_fil((_T1;T2), A, GIn,GOut, FIn,FOut,OIn,OOut,OMIn,OMOut) :-
	gen_fil(T2,A,GIn,GOut,FIn,FOut,OIn,OOut,OMIn,OMOut).


gen_fil(list(T), A, GI,GO,FI,FO,OIn,OOut,OMIn,OMOut) :-
   format(user,"*** Type list(~w) deprecated.~nUse type(list(~w)) instead.~n",[T,T]),
   gen_fil(type(list(T)), A, GI,GO,FI,FO,OIn,OOut,OMIn,OMOut).

gen_fil(T,_, G, [A|G], F,[A|F],O,O,OM,OM) :-
  \+ (legal_binding_type_construct(T)),
  format(user,"*** Illegal binding type: ~w.~n*** Assuming dynamic.~n",[T]).

legal_binding_type_construct(X) :- basic_binding_type(X).
legal_binding_type_construct(struct(X,_List)) :- ground(X).
legal_binding_type_construct(type(T)) :- typedef(T,_).
legal_binding_type_construct((_T1;_T2)).
legal_binding_type_construct(list(_)).

basic_binding_type(static).
basic_binding_type(dynamic).
basic_binding_type(nonvar).
basic_binding_type(online).
basic_binding_type(static_nf).



:- dynamic usertypedef/2.

%% the built in type definitions
typedef(list(T),(struct([],[]) ; struct('.',[T,type(list(T))]))).
typedef(static_or_dynamic,(static ; dynamic)).
typedef(T, Def) :-
	usertypedef(T,Def).



gensym(H, NewHead) :-
	(gx_data(sym,Sym) ->
	    NewSym is Sym+1,
	    retract(gx_data(sym,Sym))
	;
	    NewSym = 0
	),
	assert(gx_data(sym, NewSym)),
	add_id(H,NewSym, NewHead).

add_id(H,Sym,NH) :-
	atom_concat(H,'__',H1),
	name(H1,H1S),
	%name(H,HS),
	%append(HS,"__",H1S),
	name(Sym,SymS),
	append(H1S,SymS,NHS),
	name(NH,NHS).

same_length([],[]).
same_length([_|T],[_|T2]) :- same_length(T,T2).
