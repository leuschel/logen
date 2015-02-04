
:- module('match_unknowns', [map_annotation_unknown/3, flatten_to_list/2,
			     get_end_pos/3,
			     store_unknown_clause/3,
			     store_unused_annotation/2,
			     reset_unknown/0,
			     get_unknown_annotations/1			    
			    ]).


:- use_module(library(lists)).

:- dynamic unknown_clause/2.
:- dynamic unused_ann/2.

:- use_module(match_ann).

/* These predicates maintain the unused annotations
   and the unknown clauses
   */
reset_unknown :-
	retractall(unknown_clause(_,_,_)),
	retractall(unused_ann(_,_)).

store_unknown_clause(H,B,PB) :-
	assert(unknown_clause(H,B,PB)).

store_unused_annotation(H,B) :-
	portray_clause(user_error, unused_annotation),
	portray_clause(user_error, ':-'(H,B)),	
	assert(unused_ann(H,B)).

	

/* So we have finished matching annotations
and we have a selection of unused annotations
and some unknown clauses can we match them? */
get_unknown_annotations(Anns) :-
	(unknown_clause(H,B,PB) ->
	    (
	      (smart_match(H,B,PB,Ann)->
		  true
	      ;
		  map_annotation_unknown(_,PB,Ann)
	      ),
	      retract(unknown_clause(H,B,PB)),
	      get_unknown_annotations(AnnR),
	      append(Ann,AnnR, Anns)
	    )
	;
	    Anns = []
	).


smart_match(H,B,PB,Ann) :-
	portray_clause(user_error, smart_match(H,B)),
	unused_ann(H,AnnBody),
	portray_clause(user_error, pos_match(AnnBody)),
	flatten_to_list(B, FlatBody),
	flatten_to_list(PB, FlatPBody),
	flatten_to_list(AnnBody, FlatAnn),
	smart_match_clause(FlatBody,FlatPBody,FlatAnn, Ann),
	retract(unused_ann(H,AnnBody)).

smart_match_clause(LB,LPB,LAnn,Ann) :-
	portray_clause(user_error, smart_match_clause(LB,LPB,LAnn,Ann)),
	find_all_matches(LB, LPB, LAnn,Ann).	


find_all_matches([],[],_LeftOverAnns,[]).
find_all_matches([B|Bs],[PB|PBs],LAnn, AnnList) :-
	find_match(B,LAnn, MatchedAnn, LeftAnns),
	portray_clause(user_error, matched(PB, MatchedAnn)),
	(MatchedAnn = unknown ->
	    map_annotation_unknown(_,PB, Ann2)
	;	      	  
	    map_annotation(MatchedAnn, PB, Ann2)
	),
	find_all_matches(Bs,PBs,LeftAnns, Ann1),
	append(Ann1, Ann2, AnnList).


find_match(B,[], unknown, []).
find_match(Call, [logen(Ann,Call)|As], logen(Ann,Call), As) :-
	!.
find_match(Call, [A|As], Matched, [A|Rest]) :-
	find_match(Call, As, Matched, Rest).




flatten_to_list((A,B), List) :-
	!,
	flatten_to_list(A,AList),
	flatten_to_list(B,BList),
	append(AList,BList, List).
flatten_to_list(A, [A]).

/*
 Given the starting position of an atom working out the position of the last char
*/
get_end_pos(Start, Atom, End) :-
	name(Atom,AtomS),
	length(AtomS,L),
	End is Start + L.	


map_annotation_unknown(Body, PosBody, AnnList) :-
	flatten_to_list(PosBody, PosList),
	map2_unknown_drive(Body,PosList, _Rest,AnnList),
	%portray_clause(	map_unknown(AnnList)).
	true.

map2_unknown_drive(_,[],[],[]).
map2_unknown_drive(_,PosList,PosRest,Ann) :-
	map2_unknown(_,PosList,Rest, Ann1),
	%portray_clause(map2_unknown(PosList,Rest,Ann1)),
	(Rest = [] ->
	    Ann = Ann1,
	    PosRest = Rest
	;
	    map2_unknown_drive(_,Rest,PosRest,Ann2),
	    append(Ann1,Ann2, Ann)
	).
		
		

	

%map2_unknown(A,B,C,D) :-
%	portray_clause(map2_unknown(A,B,C,D)), fail.


map2_unknown(_, [FAPos|Rest], Rest, [Start, End, FAann| AnnCall]) :-
	FAPos =.. ['findall',Start, _P,Call, _Bag],
	!,
	FAann = resfindall,
	End is Start +7,	
	flatten_to_list(Call, CallL),	
	map2_unknown_drive(_,CallL, [], AnnCall).
	
map2_unknown(_, [FAPos|Rest], Rest, [Start, End, FAann| AnnCall]) :-
	FAPos =.. ['time_out',Start, Call, _T, _Res],
	!,
	FAann = time_out,
	End is Start +8,	
	flatten_to_list(Call, CallL),	
	map2_unknown_drive(_,CallL, [], AnnCall).
	
map2_unknown(_, [FAPos|Rest], Rest, [Start, End, FAann| Anns]) :-
	FAPos =.. ['if',Start, Cond, Then, Else],
	!,
	FAann = reslogif,
	End is Start +2,	
	flatten_to_list(Cond, CondL),
	flatten_to_list(Then, ThenL),
	flatten_to_list(Else, ElseL),	
	map2_unknown_drive(_,CondL, [], AnnCond),
	map2_unknown_drive(_,ThenL, [], AnnThen),
	map2_unknown_drive(_,ElseL, [], AnnElse),
	append(AnnThen, AnnElse, AnnT),
	append(AnnCond, AnnT, Anns).

map2_unknown(_, [FAPos|Rest], Rest, [Start, End, FAann| AnnCall]) :-
	FAPos =.. [\+,Start,Call],
	!,
	FAann = resnot,
	End is Start +2,	
	flatten_to_list(Call, CallL),	
	map2_unknown_drive(_,CallL, [], AnnCall).

map2_unknown(_, [FAPos|Rest], Rest, [Start, End, FAann| AnnCall]) :-
	FAPos =.. [when,Start,_Cond,Call],
	!,
	FAann = reswhen,
	End is Start +4,	
	flatten_to_list(Call, CallL),	
	map2_unknown_drive(_,CallL, [], AnnCall).



map2_unknown(_, [FAPos|Rest], Rest, [Start, End, FAann| AnnCall]) :-
	FAPos =.. [pp_cll,Start,Call],
	!,
	FAann = pp_cll,
	End is Start +6,	
	flatten_to_list(Call, CallL),	
	map2_unknown_drive(_,CallL, [], AnnCall).

map2_unknown(_, [FAPos|Rest], Rest, [Start, End, FAann| AnnCall]) :-
	FAPos =.. [pp_mnf,Start,Call],
	!,
	FAann = pp_mnf,
	End is Start +6,	
	flatten_to_list(Call, CallL),	
	map2_unknown_drive(_,CallL, [], AnnCall).

map2_unknown(_, [FAPos|Rest], Rest, [Start, End, FAann| AnnCall]) :-
	FAPos =.. [mnf,Start,Call],
	!,
	FAann = mnf,
	End is Start +3,	
	flatten_to_list(Call, CallL),	
	map2_unknown_drive(_,CallL, [], AnnCall).




map2_unknown(_, [IFPos|Rest], Rest, [Start, End, Ifann| Anns]) :-
	IFPos =.. [';',_EPos, IF_part, Else],
	IF_part =.. ['->', Start, Cond, Then],
	!,
	Ifann = resif,
	End is Start +2,
	
	flatten_to_list(Cond, CondL),
	flatten_to_list(Then, ThenL),
	flatten_to_list(Else, ElseL),	
	map2_unknown_drive(_,CondL, [], AnnCond),
	map2_unknown_drive(_,ThenL, [], AnnThen),
	map2_unknown_drive(_,ElseL, [], AnnElse),
	append(AnnThen, AnnElse, AnnT),
	append(AnnCond, AnnT, Anns).

map2_unknown(_, [ORPos|Rest], Rest, [Start, End, ORann| Anns]) :-
	ORPos =.. [';',Start, Left, Right],
	!,
	ORann = resdisj,
	End is Start +1,
	
	flatten_to_list(Left, LeftL),
	flatten_to_list(Right, RightL),
	map2_unknown_drive(_,LeftL, [], AnnLeft),
	map2_unknown_drive(_,RightL, [], AnnRight),
	append(AnnLeft, AnnRight, Anns).


map2_unknown(_,[PosM|Rest], Rest, [Start, End, unknown]) :-
	PosM =.. [':', _P,_M,PosA],
	arg(1,PosA, Start),
	functor(PosA,Func,_),
        get_end_pos(Start, Func, End).


map2_unknown(_,[PosA|Rest], Rest, [Start, End, unknown]) :-
	arg(1,PosA, Start),
	functor(PosA,Func,_),
        get_end_pos(Start, Func, End).
