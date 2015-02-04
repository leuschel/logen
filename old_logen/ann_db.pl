:- module('ann_db', [
		     logen_annotation_structure/4,
		     logen_annotation/2,
		     see_through_primitive/4,
		     remove_annotation_body/2,
		     logen_map_remove/2]).

%%%this file should maintain the annotation database, in the nice simple structure...		    


:- use_module('logen_messages.pl').




remove_annotation_body(Pattern, Result) :-
	logen_annotation_structure(Pattern, In, Out, Result),
	!,
	logen_map_remove(In,Out),
	!.
remove_annotation_body(Pattern, error) :-
	print(unknown_pattern(Pattern)).


logen_map_remove([],[]).
logen_map_remove([A|T],[B|T1]) :- remove_annotation_body(A,B),logen_map_remove(T,T1).



%%%% complicated structures( PATTERN,   AnnotateArgs  Mapping      Plain Prolog Code )
%logen_annotation_structure((logen(ID,Head) :- Body), [Body], [NBody], (Head:-NBody)).
logen_annotation_structure((B1,B2)        , [B1,B2]   ,[NB1,NB2]    ,(NB1,NB2)).
logen_annotation_structure(if(C1,C2,C3)   , [C1,C2,C3],[NC1,NC2,NC3],(NC1->NC2;NC3)).
logen_annotation_structure(resif(C1,C2,C3), [C1,C2,C3],[NC1,NC2,NC3],(NC1->NC2;NC3)).
logen_annotation_structure(semif(C1,C2,C3), [C1,C2,C3],[NC1,NC2,NC3],(NC1->NC2;NC3)).
logen_annotation_structure(logif(C1,C2,C3), [C1,C2,C3],[NC1,NC2,NC3],if(NC1,NC2,NC3)).
logen_annotation_structure(reslogif(C1,C2,C3), [C1,C2,C3],[NC1,NC2,NC3],if(NC1,NC2,NC3)).
logen_annotation_structure(resdisj(C1,C2) , [C1,C2]   ,[NC1,NC2]    ,(NC1;NC2)).
logen_annotation_structure(';'(C1,C2)     , [C1,C2]   ,[NC1,NC2]    ,(NC1;NC2)).

logen_annotation_structure(hide_nf(C1)    , [C1]      ,[NC1]        ,NC1).
logen_annotation_structure(hide(C1)       , [C1]      ,[NC1]        ,NC1).
logen_annotation_structure(resnot(C1)     , [C1]      ,[NC1]        ,\+(NC1)).

logen_annotation_structure(when(A,C1)     , [C1]      ,[NC1]        ,when(A,NC1)).
logen_annotation_structure(reswhen(A,C1)  , [C1]      ,[NC1]        ,when(A,NC1)).
logen_annotation_structure(time_out(C1,T,R), [C1]      ,[NC1]       ,time_out(NC1,T,R)).
logen_annotation_structure(pp_cll(C1)     , [C1]      ,[NC1]        ,pp_cll(NC1)).
logen_annotation_structure(pp_mnf(C1)     , [C1]      ,[NC1]        ,pp_mnf(NC1)).
logen_annotation_structure(mnf(C1)     , [C1]      ,[NC1]           ,mnf(NC1)).
logen_annotation_structure(not(C1)        , [C1]      ,[NC1]        ,\+(NC1)).
logen_annotation_structure(findall(A,B,C) , [B]       ,[NB]         ,findall(A,NB,C)).
logen_annotation_structure(resfindall(A,B,C), [B]     ,[NB]         ,findall(A,NB,C)).

logen_annotation_structure(semiwhen(C1,C2), [C2]      ,[NC2]        ,when(C1,NC2)).
logen_annotation_structure(reswhen(C1,C2),  [C2]      ,[NC2]        ,when(C1,NC2)).

logen_annotation_structure(P,[A],[B],ResP) :-
   see_through_primitive(P,A,ResP,B).
logen_annotation_structure(M:logen(Ann,Call), []        ,[]           , M:Call) :-	
	logen_annotation_structure(logen(Ann,M:Call), []        ,[]           , M:Call).
%	portray_clause(here),
%	logen_annotation_structure(logen(Ann,M:Call),[],[],Call).

	
logen_annotation_structure(logen(Ann,Call), []        ,[]           , Call) :-
	(logen_annotation(Ann, Call) ->
	    true
	;
	    (  print_error('Logen does not understand annotation keyword'),
	      print_error(Ann),
	      print_error(Call),
	      print_error('If this is valid annotation please update annfile.pl')
	    )).
	
logen_annotation(call,_).
logen_annotation(online,_).
logen_annotation(semicall,_).
logen_annotation(rescall,_).
logen_annotation(unfold,_).
logen_annotation(memo,_).
logen_annotation(mcall,_).
logen_annotation(ucall,_).		     
logen_annotation(pp(_),_).

see_through_primitive(retractall(G),G,retractall(VS),VS).
see_through_primitive(retract(G),G,retract(VS),VS).
see_through_primitive(assert(G),G,assert(VS),VS).
see_through_primitive(assertz(G),G,assertz(VS),VS).
see_through_primitive(call_residue(G,Res),G,call_residue(VS,Res),VS).
