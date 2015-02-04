
:- module(pp_iftransform, [iftransform_in/0,
			   iftransform_out/0
			   ]).

:- use_module(library(terms)).
:- use_module(library(lists)).


:- use_module(bta_pp, [in_logen_clausedb/1,save_logen_clause/1]).

:- dynamic if_gensym_id/1.
:- dynamic logen_if_table/3.

reset :-
	retractall(if_gensym_id(_)),
	retractall(logen_if_table(_,_,_)).


save_clause(logen_ignore) :- !.
save_clause(NC) :-
	save_logen_clause(NC).


iftransform_in :-
	reset,
	in_logen_clausedb(C),	
	logen_clause(C,NC),
	save_clause(NC),
	fail ; true.

iftransform_out :-
	reset,
	make_if_table,	
	in_logen_clausedb(C),
	logen_clause(C,NC),
	save_clause(NC),
	fail ; true.

make_if_table :-
	in_logen_clausedb((logen(ID,logen_iftable(IF,NIF)))),
	assert(logen_if_table(ID,IF,NIF)),
	fail ; true.



	


%% throw away if table info

logen_clause((:- filter(Filter)),logen_ignore) :-
            functor(Filter, Func,_),
	    (atom_concat(ID, '_then', Func) ->
		true
	    ;
		(atom_concat(ID, '_cond', Func) ->
		    true
		;
		    Func = ID)),
	    logen_if_table(ID,_,_),
	    !.


logen_clause((logen(ID,_):- _),logen_ignore) :-
	    logen_if_table(ID,_,_),
	     !.
	    

logen_clause((logen(_,logen_iftable(_,_))),logen_ignore) :- !.

logen_clause((:-Decl), (:-Decl)) :-
          !.

logen_clause((Head:-Body),(NHead:-NewBody)) :-
          !,
          logen_head(Head,NHead),
	  logen_body(Body,NewBody).
       
logen_clause(Fact, NFact) :-
	logen_head(Fact,NFact).


logen_head(Fact,Fact).


logen_body((A,B), (NA,NB)) :-
	!,
	logen_call(A,NA),
	logen_body(B,NB).

logen_body(A,NA) :-	
	logen_call(A,NA).

logen_call(IF,NIF) :-
	(IF = resif(_,_,_);IF = if(_,_,_); IF = semiif(_,_,_)),
	!,
	logen_if(IF, NIF).

logen_call(Disj,NDisj) :-
	Disj = (_;_),
	!,
	logen_disj(Disj,NDisj).

logen_call(Call,NCall) :-
	logen_plain_call(Call,NCall).


%% we just copy for now.
logen_plain_call(logen(_,NIF), NCall) :-
	logen_if_table(_,If,NIF),	
	!,
	%portray_clause(user_error, debug('Found if statement', NIF,If)),
	rebuild_if(If,NIF,NCall),
	%portray_clause(user_error, debug('Rebuilt IF', NCall)).
	true.
	


logen_plain_call(Call,Call).

logen_disj(Disj,Disj).




rebuild_if(If,NIF,Output) :-
	
	findall(NIF-IfBody,in_logen_clausedb((logen(_,NIF) :- IfBody)), [NIF-(logen(_CondAnn,CondPart),logen(_ThenAnn,ThenPart)), NIF-ElseAnn]),
	in_logen_clausedb((logen(_,CondPart) :-CondAnn)),
	in_logen_clausedb((logen(_,ThenPart) :-ThenAnn)),
	

	%Output = resif(CondAnn,ThenAnn, ElseAnn).
	static_or_dynamic_if(CondAnn,ThenAnn,ElseAnn, IFANN),
	%Output = resif(hide_nf(CondAnn),hide_nf(ThenAnn), hide_nf(ElseAnn)).
	Output =.. [IFANN,hide_nf(CondAnn),hide_nf(ThenAnn), hide_nf(ElseAnn)].

static_or_dynamic_if(logen(call,Cond), _,_,if) :-
	%%% Very cautious! only static if, if it is called and fully ground!
	%% this is not right though, we dont have ground data we have filters...
	%% we need to know that the call is fully static....	
	ground(Cond),
	!.


static_or_dynamic_if(_,_,_,resif).






if_gensym(X) :-
	(if_gensym_id(X) ->
	    retract(if_gensym_id(X))
	;
	    X = 0
	),
	X1 is X +1,
	assert(if_gensym_id(X1)).

%% Sicstus decides a number is not an atom so will not concat....
my_atom_concat(A,B,C) :-
	name(A,As),
	name(B,Bs),
	append(As,Bs,Cs),
	name(C,Cs).


	
	       
logen_if(IF,logen(unfold, NIF)) :-
	(IF =.. [_Type, hide_nf(IFpart),hide_nf(THENpart), hide_nf(ELSEpart)],! ;
	    
	IF =.. [_Type, IFpart,THENpart, ELSEpart]),
	term_variables(IF, Vars),
	if_gensym(ID),

	my_atom_concat(logen_if__, ID, IF_ID),
	my_atom_concat(IF_ID, '_cond', COND_ID),
	my_atom_concat(IF_ID,'_then', THEN_ID),
	
	NIF =.. [IF_ID|Vars],
	NCondPart =.. [COND_ID|Vars],
	NThenPart =.. [THEN_ID|Vars],		
	save_logen_clause((logen(IF_ID,NCondPart):- IFpart)),
	save_logen_clause((logen(IF_ID,NThenPart):- THENpart)),
	
		
	save_logen_clause((logen(IF_ID,NIF):- (logen(unfold,NCondPart),logen(unfold,NThenPart)))),
	save_logen_clause((logen(IF_ID,NIF):- (ELSEpart))),
	save_logen_clause((logen(IF_ID,logen_iftable(IF,NIF)))).


	








