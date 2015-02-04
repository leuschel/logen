
:- module(inline, [inline/0,test_inline/0]).

:- use_module('../logen_post').


:- use_module(library(lists)).

:- dynamic processing_call/1.

inline :-
	retractall(processing_call(_)),
	%clone_decl,
	inline_spec,	
	clone_memo.

is_external_call(Call) :-
	functor(Call, F, A),
	functor(Copy, F,A),
	memo_clause(table(_,Copy, Properties)),
	member(crossmodule,Properties).

inline_spec :-
	spec_clause(Spec),
	(Spec = (Head:-Body) ->
	    (inline_body(Body,OutBody) ->
		true
	    ;
		OutBody = Body
	    ),
	    OutSpec = (Head:-OutBody),
	    save_spec_clause(OutSpec),
	    true
	;	    
	    save_spec_clause(Spec)
	),fail;true.


inline_body((Call,Rest), (NCall,NRest)) :-
	!,
	inline_call(Call,NCall),
	inline_body(Rest,NRest).

inline_body((If->Then;Else), (NIf->NThen;NElse)) :-
	!,	
	inline_call(If,NIf),
	inline_call(Then,NThen),
	inline_call(Else,NElse).




inline_body(Call,NCall) :-
	inline_call(Call,NCall).

is_deterministic(Call) :-
	is_deterministic(Call,1).
is_deterministic(Call,X2) :-
	findall((Call), spec_clause((Call:-_)),BagC),
	findall((Call), spec_clause((Call)),BagF),
	length(BagC,X),
	length(BagF,X1),
	X2 is X + X1.
	

inline_call(Call,Call) :-
	processing_call(Call),
	!,fail.

inline_call(Call, Call) :-
	is_external_call(Call),
	!.
inline_call(Call,Call) :-
	\+(is_deterministic(Call)),
	!.

inline_call(Call,NCall) :-
	% first we check for facts?
	findall(Call, spec_clause(Call), Bag),
	length(Bag,1), % is it deterministic?
	!,
	Bag = [NCall].



inline_call(Call,NCall1) :-
	
	% now we check for clauses?
	findall((Call:-Body), spec_clause((Call:-Body)), Bag),
	length(Bag,1), % is it deterministic?
	%% Okay we have a single clause body....
	%% what could go wrong:  recursive inlining

	functor(Call,Func,Arity),
	functor(Copy,Func,Arity),
	Bag = [(Call:-NCall)],	
	assert(processing_call(Copy)),
	(inline_body(NCall,NCall1) ->
	    retract(processing_call(Copy)),
	    !
	;
	    %% We have a loop below us so do not inline
	    NCall1 = NCall
	).
	    
	
	


	

inline_call(C,C).

	





