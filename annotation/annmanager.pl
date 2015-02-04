% this loads an annotation file and replaces the annotations using a linear
% list. This is not especially useful as to use it with the web interface
% requires that the annotation file has exactly the same order as the source
% file and that there have been no changes to the source file.

% Hence annmanager is now unused. In future this could be deleted.

:- module(annmanager, [load_file_with_annlist/4]).

test :- load_file_with_annlist('../tests/match.pl.ann',[memo,call,unfold,memo],[ignore_filters],user).


:- use_module('../annmeta.pl', [annmeta/5, declmeta/3,is_infix/1]).
:- use_module('../tools/error_manager.pl', [add_error/3,add_message/4]).


load_file_with_annlist(AnnFilename,A,Opts, OutStream) :-
	open(AnnFilename, read, Stream),
	load_clauses(Stream,A,Opts,OutStream),
	close(Stream).
	
load_clauses(Stream,AnnsIn,Opts,OutStream) :-
	read_term(Stream, Term,[]),
	(Term == end_of_file ->
	    (AnnsIn == [] ->
	        true  %%% all ok, all anns used
	    ;
		add_error(annmanager, "Unused annotations loading annlist ~w", [AnnsIn]))
	   ;
	    load_clause(Term, NTerm, AnnsIn,AnnsOut,Opts),
	    (NTerm = true ->
	        true %%ignore
	    ;
		portray_clause(OutStream,NTerm)
	    ),
	    load_clauses(Stream,AnnsOut,Opts,OutStream)
	).

%% facts remain unchanged
load_clause(logen(ID,Head), logen(ID,Head), AnnList, AnnList,_) :- !.
load_clause((logen(ID,Head) :- Body), (logen(ID,Head) :- NBody), InAnns,OutAnns,_) :-
	!,
	load_body(Body, NBody, InAnns, OutAnns).

%% leave other things alone... modules etc
load_clause((:-filter(_)), true, L,L, Opts) :-
	member(ignore_filters, Opts).
load_clause((:-type(_)), true, L,L, Opts) :-
	member(ignore_filters, Opts).

load_clause(A,A, L,L,_).




load_body(Ann,NAnn1,In, Out) :-
	annmeta(load, Ann, NAnn, MapFrom, MapTo),
	(is_infix(Ann) ->
	 %% if infix then higerorder ann will appear *after* first item
	 MapFrom = [LHS|R], % get first item
	 MapTo = [LHSto|Rto],
	 load_body(LHS,LHSto, In, Out1),  % Annotate first item
	 insert_ann(NAnn, NAnn1, Out1, Out2) % Annotate Higher order

	;
	    insert_ann(NAnn, NAnn1, In, Out2), % Not infix so just annotate
	    MapFrom = R, MapTo = Rto
	),
	load_list(R, Rto, Out2,Out). % do rest of args	

%% conjunction is a special case because it doesnt consume annotations
insert_ann((A,B), (A,B), L,L) :- !.
insert_ann(logen(_,C), logen(Ann, C), [Ann|Out], Out) :- !.
insert_ann(HigherOrder, New, [Ann|Out], Out) :-
	HigherOrder =.. [_|Args],
	New =.. [Ann|Args].


	
load_list([],[],L,L).
load_list([A|RA], [NA|RNA], In, Out) :-
	load_body(A,NA,In,Out1),
	load_list(RA,RNA, Out1, Out).





