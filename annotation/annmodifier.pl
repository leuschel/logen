% This is a replacement for annmanager, which takes a source file as input
% instead of an annotation file. This means that the order of annotations
% in an annotation file can be changed (although this will always output in
% the same order as the source file). Additionally changes can be made to the
% source file without needing to recreate the annotations before modifying them.

:- module(annmodifier, [load_file_with_annlist/3, test/0,iftest/0]).


test :- load_file_with_annlist('../tests/match.pl',[memo, call, hide_nf, unfold, hide_nf_stop, memo],user).
iftest :- load_file_with_annlist('../tests/if.pl',[hide_nf, memo, memo, if,unfold, unfold,unfold, memo, hide_nf_stop],user).

:- use_module('../annmeta.pl', [annotation_matcher_data/4, declmeta/3,is_infix/1,is_simple_ann/1]).
:- use_module('../tools/error_manager.pl', [add_error/3,add_message/4]).

load_file_with_annlist(Filename,Anns, OutStream) :-
	open(Filename, read, Stream),
	load_clauses(Stream,Anns,OutStream),
	close(Stream).
	
load_clauses(Stream,AnnsIn,OutStream) :-
	read_term(Stream, Term,[]),
	(Term == end_of_file ->
	    (AnnsIn == [] ->
	        true  %%% all ok, all anns used
	    ;
		add_error(annmanager, "Unused annotations loading anns ~w", [AnnsIn]))
	   ;
	    load_clause(Term, NTerm, AnnsIn,AnnsOut),
	    (NTerm = true ->
	        true %%ignore
	    ;
		portray_clause(OutStream,NTerm)
	    ),
	    load_clauses(Stream,AnnsOut,OutStream)
	).

load_clause((H:-B), (logen(N/A,H) :- NB), Anns, NAnns) :-
	!, functor(H,N,A), load_body(B, NB, Anns, NAnns).
load_clause((:-_), true, Anns, Anns) :- !.
load_clause(H, logen(N/A,H), Anns, Anns) :- !, functor(H,N,A).

annmatch(AnnTerm, Term, MapTo, MapFrom) :-
	annotation_matcher_data(AnnTerm, Term, MapTo, MapFrom),
	AnnTerm \= hide_nf(_).

load_hide_nf_right((A, B), AnnT, Anns, NAnns, R) :-
	!, load_hide_nf_right(A, AnnT1, Anns, Anns2, R1),
	(Anns2 = [hide_nf_stop|_]
	->
		AnnT = AnnT1, NAnns = Anns2,
		(R1 = []
		->
			(R = B)
		;	(R = (R1, B)))
	;	(AnnT = (AnnT1, AnnT2),
		 load_hide_nf_right(B, AnnT2, Anns2, NAnns, R))).
		
load_hide_nf_right(T, A, Anns, NAnns, []) :-
	load_body(T, A, Anns, NAnns).

load_hide_nf_left((A, B), AnnT, Anns, NAnns) :-
	!, load_hide_nf_left(A, AnnT1, Anns, Anns2),
	(Anns2 = [hide_nf_stop|Anns3]
	->
		(load_body(B, AnnT2, Anns3, NAnns),
		 AnnT = (hide_nf(AnnT1), AnnT2))
	;	(load_hide_nf_right(B, AnnT2, Anns2, NAnns2, R),
		 (NAnns2 = [hide_nf_stop|NAnns3]
		 ->
		 	(R = [] 
			->
				(NAnns = NAnns3, AnnT = hide_nf((AnnT1, AnnT2)))
			;	(AnnT = (hide_nf((AnnT1, AnnT2)), AnnR),
				 load_body(R, AnnR, NAnns3, NAnns)))
		 ;	(AnnT = (AnnT1, AnnT2), NAnns = NAnns2)))).

load_hide_nf_left(B, AnnT, Anns, NAnns) :-
	is_infix(B), !,
	annmatch(AnnT2, B, [LTo|MapTo], [LFrom|MapFrom]),
	load_hide_nf_left(LFrom, LATerm, Anns, Anns2),
	(Anns2 = [hide_nf_stop|Anns3]
	->
		(insert_ann(AnnT2, AnnT, Anns3, Anns4),
		 load_list(MapFrom, MapTo, Anns4, NAnns),
		 LTo = hide_nf(LATerm))
	;	LTo = LATerm,
		(insert_ann(AnnT2, AnnT3, Anns2, Anns3),
		 load_list(MapFrom, MapTo, Anns3, Anns4),
		 (Anns4 = [hide_nf_stop|NAnns]
		 ->
		 	AnnT = hide_nf(AnnT3)
		 ; AnnT = AnnT3, NAnns = Anns4))).
	
load_hide_nf_left(B, NAnnTerm, Anns, NAnns) :-
	\+is_infix(B), !,
	annmatch(AnnTerm, B, MapTo, MapFrom),
	insert_ann(AnnTerm, NAnnTerm2, Anns, Anns2),
	load_list(MapFrom, MapTo, Anns2, Anns3),
	(Anns3 = [hide_nf_stop|Anns4]
	->
		NAnnTerm = hide_nf(NAnnTerm2), NAnns = Anns4
	;	NAnnTerm = NAnnTerm2, NAnns = Anns3).

load_body(B, NB, [hide_nf|Anns], NAnns) :-
	load_hide_nf_left(B, NB, Anns, NAnns).
		 
load_body(B, NB, Anns, NAnns) :-
	annmatch(NB2, B, MapTo, MapFrom),
	(is_infix(NB2) ->
	 (MapFrom = [LHS|R], MapTo = [LHSto|Rto],
	  load_body(LHS, LHSto, Anns, Anns2),
	  insert_ann(NB2, NB, Anns2, Anns3))
	;(insert_ann(NB2, NB, Anns, Anns3),
	  R = MapFrom, Rto = MapTo)),
	load_list(R, Rto, Anns3, NAnns).

load_list([],[],L,L).
load_list([A|RA], [NA|RNA], In, Out) :-
    load_body(A,NA,In,Out1),
    load_list(RA,RNA, Out1, Out).

insert_ann((A,B), (A,B), L, L) :- !.
insert_ann(logen(_,A),logen(Ann, A), [Ann|Anns], Anns) :- is_simple_ann(Ann), !.
insert_ann(A, NA, [Ann|Anns], Anns) :- 
	A =.. [_|Args],
	NA =.. [Ann|Args].
