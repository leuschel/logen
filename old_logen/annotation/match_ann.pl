:- module('match_ann', [request_ann/6,
			   map_annotation/3,
			   get_annfile_name/2
			  ]).


:- use_module(library(lists)).
% :- use_module(library(charsio)).
:- use_module('../annfile.pl').
:- use_module('read.pl').
:- use_module('../ann_db.pl').


:- use_module('match_unknowns.pl').

/*
  Request the annotations for *FileName*
  Default behaviour is to look in *FileName*.ann and attempt
  to match up the annotations with the clauses

  FileName: The prolog main source file for program
  Ann: A list of the form [Start End Annotation ......]
  Syntax: A list of the form [Start End Syntax .... ] comments etc
  AllFilters: The filter declarations
*/
%request_ann(FileName, Ann, Syntax, AllFilters, FilterSyntax) :-
request_ann(FileName,Annfile, Ann, Syntax, AllFilters, FilterSyntax) :-
      statistics(runtime, [StartTime,_]),
	request_ann(FileName,Annfile, Ann, Syntax, AllFilters),!,
      statistics(runtime, [ReqTT,_]),
      ReqTime is ReqTT - StartTime,
      format(user_error, "Request Annotation Time ~w ms~n", [ReqTime]),
	%% get syntax info for AllFilters...
	current_input(OldStream),
	name(AllFilters, Chars),
	
	%%% Steve Hack
    %open('tmp', write, StS),
	
	%format(StS, "~s~n", [Chars]),
	%close(StS),
	
	%open('tmp', read, Stream),

	%%%		
 
	open_chars_stream(Chars,Stream),
	set_input(Stream),
	%print(user_error,'Finished request_ann'),nl(user_error),
	get_syntax_read(FilterSyntax),
	set_input(OldStream),
	true.
	%print(user_error,'Finished get_syntax_read'),nl(user_error).
%	nl,nl,portray_clause(request_ann(FileName, Ann, Syntax, filter_here, syntax_filter)).

get_syntax_read(FilterSyntax) :-
	portable_read_position(Term, PosTerm, CommTokens),
	%portray_clause(user_error,read(Term)),
	(Term == end_of_file ->
	    FilterSyntax = []
	;
	    (
	      get_syntax_read(Syntax),!,
	      get_head_tag(PosTerm, HeadTags),	    
	      make_comment_tags(CommTokens, Comments),
	  %    Comments =[],
	      append(Comments, HeadTags, CHSyntax),
	      append(CHSyntax, Syntax, FilterSyntax)
	    )
	).

request_ann(FileName,Annfile, Ann, Syntax, AllFilters) :-
	request_ann(FileName,Annfile, Ann, Syntax),
	collect_filters(AllFilters),
	%% for test print to screen
%	portray_clause(AllFilters).
	true.

request_ann(FileName,AnnFile, Ann, Syntax) :-	
	%get_annfile_name(FileName, AnnFile),
	reset_unknown,
	load_annfile(AnnFile),
	see(FileName),
	read_clauses(Ann1, Syntax),
	seen,

	findall(_, (ann_clause(_ID,H,B), store_unused_annotation(H,B)),_),
	%nl(user),print(user,'Unused Annotations:'), nl(user),
	%findall((Head:-Body),ann_clause(_ID,Head,Body),Bag),
	%print_unused(user,Bag),

	
	get_unknown_annotations(AnnUnknown),
	append(Ann1,AnnUnknown,Ann).
	


print_unused(_,[]).
print_unused(S,[P|Ps]) :-
	portray_clause(S,P),
	print_unused(S,Ps).

%% Should collect all the things that end up in the right hand side pane in gui...
collect_filters(Filters) :-
	findall(filter(H,F),filter(H,F), Bag),
	findall(user_type(T,EDef), user_type(T,EDef),Types),
	findall(is_safe(G,Call), is_safe(G,Call),Safe),
	
	with_output_to_chars(
			     (print('/* Filter Declarations */\n'),print_filter_list(Bag),print_types_list(Types),print_safety_list(Safe))
			     , Chars),
	name(Filters,Chars).

print_safety_list([]).
print_safety_list([is_safe(G,C)|Ss]) :-
	portray_clause(':-'(is_safe(G,C))),
	print_safety_list(Ss).

print_types_list([]).
print_types_list([user_type(T,EDef)|Ts]) :-
	portray_clause(':-'(type('--->'(T,EDef)))),
	print_types_list(Ts).

print_filter_list([]).
print_filter_list([filter(Head, Filter)|Ts]) :-
	Head =.. [Func|_Args],
	NewFilter =.. [Func|Filter],
	
	portray_clause(':-'(filter(NewFilter))),
	print_filter_list(Ts).

	


%convert_filter_to_tcl(filter(H,Filter), Atom, Format) :-	
%	func(H,Func,Arity),
%	write_to_chars(Func, String, EFunc).

/* for filters we 


/*
  Parse the list of syntax elements we wish to pass back for colouring etc
  At the moment comments and Lists
*/
make_comment_tags([], []).
make_comment_tags([comment(X,Y)|Xs], [X, Y, comment |Ys]) :-
	make_comment_tags(Xs,Ys).
make_comment_tags([list(X,_Y)|Xs], [X, NewY, list |Ys]) :-	
	NewY is X + 1,	
	make_comment_tags(Xs,Ys).

/*
  Read in each clause from the source file.
  We use a custom parser here because we need position information of all terms
  Ann: A list of the form [Start End Annotation ......]
  Syntax: A list of the form [Start End Syntax .... ] comments etc
*/
read_clauses(Ann, Syntax) :-
	%portable_read(PosTerm, CommTokens),
	portable_read_position(Term, PosTerm, CommTokens),
	%portray_clause(Term),
	%portray_clause(PosTerm),
	
	(PosTerm = end_of_file(_) ->
	    Ann =[], Syntax = []
	;
	    %% We have term, proceed and recurse
	    get_annclause(Term,PosTerm,AnnList),
	    %%get_annclause(PosTerm, AnnList),	    
	    read_clauses(ARest, CommRest),
	    append(AnnList, ARest, Ann),
	    get_head_tag(PosTerm, HeadTags),
	    make_comment_tags(CommTokens, Comm),
	    append(Comm, CommRest, Comments),
	    append(Comments, HeadTags, Syntax)
	).
	    

get_filter_tags([],[]).
get_filter_tags([static(Pos)|As],[Pos,End, static|Ts]) :-
	!,
	End is Pos + 6,
	get_filter_tags(As,Ts).
get_filter_tags([dynamic(Pos)|As],[Pos,End, dynamic|Ts]) :-
	!,
	End is Pos + 7,
	get_filter_tags(As,Ts).

get_filter_tags([_|As],Ts) :-
	!,
	get_filter_tags(As,Ts).



	



%%% This one should only match filters...
get_head_tag((:-(_Pos,filter(_Pos1,Head))), [HeadPos,HeadEnd, filter|HeadTags]) :-
        !,
	Head =.. [HeadFunc, HeadPos|Args],
	%arg(1,Head, HeadPos ),
	%functor(Head, HeadFunc, _),
	get_end_pos(HeadPos, HeadFunc, HeadEnd),
	get_filter_tags(Args, HeadTags).
		 




get_head_tag((:-(_Pos, Head)), HeadTags) :-
	!,
	get_head_tag(Head, HeadTags).

get_head_tag((:-(_Pos, Head, Body)), Tags) :-
	!,

	get_head_tag(Head, HeadTags),
	get_body_tag(Body, BodyTags),
	append(HeadTags, BodyTags, Tags).

get_head_tag(Head, [HeadPos, HeadEnd, head|TypeTags]) :-
 	arg(1,Head, HeadPos ),
	functor(Head, HeadFunc, Arity),
	get_end_pos(HeadPos, HeadFunc, HeadEnd),
	A1 is Arity -1,
	functor(CopyHead,HeadFunc, A1),
	(filter(CopyHead,Type) ->
	    Head =.. [_, _|Args],
	    tagHeadArguments(Args,Type, TypeTags) 
	    %TypeTags = []

	;
	    TypeTags = []
	),
	!.

%% Make sure this doesnt cause failure, if all else fails just dont
%% return any tags...
get_head_tag(_,[]).



get_call_tag(Call,Tags) :-
	nonvar(Call),
	functor(Call,HeadFunc,Arity),
	A1 is Arity -1,
	functor(CopyHead,HeadFunc,A1),
	(filter(CopyHead,Type) ->
	    Call =.. [_,_|Args],
	    tagHeadArguments(Args,Type,Tags)
	;
	    Tags=[]
	).

	




get_body_tag((A,B),Tags) :-
	nonvar(A),
	!,
	get_call_tag(A,CTag),
	get_body_tag(B,BTag),
	append(CTag,BTag,Tags).

get_body_tag(C,Tag) :-
	get_call_tag(C,Tag).

get_body_tag(B,[]) :-
	portray_clause(unable_to_parse_body(B)).



tagHeadArguments([],[],[]).
tagHeadArguments([X|As],[_Type| Ts],Tags) :-
	var(X),
	!,
	tagHeadArguments(As,Ts,Tags).

tagHeadArguments([X|As],[Type| Ts],Tags) :-
	getHeadArgumentTag(X,Type,Tag),
	%portray_clause(	getHeadArgumentTag(X,Type,Tag)),
	tagHeadArguments(As,Ts,TagR),
	append(Tag,TagR,Tags).




%getHeadArgumentTag(X,_Type,[]) :-
%	portray_clause(called_with(X)),
%	fail.
	
getHeadArgumentTag(X,_Type,[]) :-
	var(X),
	!.
%%% LISTS are a special case as they are
%%% not position marked the same as others
getHeadArgumentTag([](_),_Type,[]) :-
	!.
getHeadArgumentTag([],_Type,[]) :-
	!.
getHeadArgumentTag([H|T], Type,Tags) :-
	!,	
	getHeadArgumentTag(H,Type,Tag1),
	getHeadArgumentTag(T,Type,Tag2),
	%Tag2 = [],
	append(Tag1,Tag2,Tags).

getHeadArgumentTag(X,_,[]) :-
	atomic(X).
	
getHeadArgumentTag(X,Type,Tag) :-
	atomic(Type),
	!,
	(X = '$VAR'(Pos,Name,_) ->
	    get_end_pos(Pos,Name,End),
	    Tag = [Pos,End,Type]
	;
	    arg(1,X,Pos),
	    (number(Pos) ->
		functor(X,Name,_),
		get_end_pos(Pos,Name,End),
		X =.. [Name,Pos|Args],
		
		%portray_clause(call__getHeadArgumentTag(Args,Type,Tag1)),
		getHeadArgumentTag(Args,Type,Tag1),
		%portray_clause(exit__getHeadArgumentTag(Args,Type,Tag1)),		
		    
		%Tag1 = [],
		
		Tag = [Pos,End,Type|Tag1]
		
	    ;
		portray_clause(no_match_for_filter(X)),
		Tag = []
	    )
	).
	
getHeadArgumentTag(_X,_Type,[]) :-
	%portray_clause(ignoring(X,Type,[])),
	true.



getHeadArgumentTagL([],_Type,[]).
getHeadArgumentTagL([Arg|As],Type,Tags) :-
	%portray_clause(aaaa_getHeadArgumentTag(Arg,Type,Tag)),
	getHeadArgumentTag(Arg,Type,Tag),
	getHeadArgumentTagL(As,Type,TagR),
	append(Tag,TagR,Tags).

	

	
	
	

%% we have a clean prolog clause, find the matching annotation
%% need to handle exact and inexact matches differently

get_annclause(':-'(use_module(Module)),(_:-use_module(Pos,_)),[Pos,Pos1,ModuleAnn]) :-
	     Pos1 is Pos + 10,
	     %portray_clause(get_annclause(':-'(use_module(Module),PosTerm,Ann))),
	     (annfile:residual_pred((:-use_module(Module))) ->
		 ModuleAnn = gxspec
	     ;
		 ModuleAnn = gx
	     ).
	     %portray_clause(get_annclause(':-'(use_module(Module),PosTerm,[Pos,Pos1,ModuleAnn]))).  	     	     	     

%get_annclause(':-'(_),_,_). %% ignore declarations
get_annclause(':-'(_),_,[]). %% ignore declarations

get_annclause(Term,PosTerm,Ann) :-
	get_ann_id(Term, ID, exact),
	!,
	%print(exact_match(Term,ID)), nl,nl,
%	trace,
	ann_clause(ID,AH,AB),
	(AB == true ->
	    Ann = []	    %% we have a fact
	;
	    %% we have a head :- body
	    (arg(3,PosTerm, PosBody),
	     %portray_clause(map_annotation(AB,PosBody,Ann)),
	     map_annotation(AB,PosBody,Ann)
	    )
	),retract(ann_clause(ID,AH,AB)).

get_annclause(Term, PosTerm, Ann) :-
	print(user_error, Term),
	print(user_error, ':no_match_found'),nl(user_error),nl(user_error),
	( functor(PosTerm, ':-', 3) ->
	    (arg(3,PosTerm, PosBody),
		arg(1,Term,Head),
		arg(2,Term,Body),
		
		store_unknown_clause(Head,Body,PosBody),
		Ann = []
	    )
	    %map_annotation_unknown(_, PosBody, Ann))
	;
	    Ann = []).


flat_conj(Conj, FConj) :-  flat_conj(Conj, FConj1, true),!,FConj = FConj1. %keep 2nd arg free
flat_conj(X, X, Y) :-
	Y == true,
	(X \= (_,_);var(X)).
flat_conj(X, (X,Y),Y) :- var(X), !.
flat_conj(true, X, X).
flat_conj((A,B), X1, X3) :-
	flat_conj(A, X1, X2),
	flat_conj(B, X2, X3).
flat_conj(G, (G,X), X) :-
	\+G=true,
	\+G=(_,_).


/*
  Look up a clause in the ann_clause db for its ID
*/ 
get_ann_id((Head:-Body), ID,exact) :-
        %portray_clause(head(Head)),
        ann_clause(ID, Head, AnnBody),
	remove_annotation_body(AnnBody, Body1),
	flat_conj(Body1,Body).

	
get_ann_id(Head, ID,exact) :-
	ann_clause(ID,Head, true).


get_start_end_hidenf(Start,End,Rest) :-
	nl,
	write(Rest),
	nl,
	Rest = [Start,_|_],
	append(_,[_,End,_],Rest).


:- use_module(library(lists)).



/* map2 should be cleaned up and use the defs for the annotations from
   ann_db or some other place !! */
   

map_annotation(Body, PosBody, AnnList) :-
	flatten_to_list(PosBody, PosList),
	%portray_clause(map2(Body,PosList, _Rest,AnnList)),
	map2(Body,PosList, _Rest,AnnList),
	%portray_clause(	map2(AnnList)).
	true.

%map2(A,B,C,D) :-
%	portray_clause(map2(A,B,C,D)), fail.


map2((A,B),PosList, PosRest, Anns) :-
	map2(A,PosList, PosRest1, Ann),
        map2(B,PosRest1, PosRest, Ann2),
	append(Ann,Ann2, Anns).


map2(hide_nf(Body), PosList, Rest, [ Start, End, hide_nf|Anns]) :-
	map2(Body, PosList, Rest, Anns),
	Anns =  [Start|_],
	append(_,[_,End,_Ann], Anns).

%map2(;(Body,Body1), [OrPos|Rest], Rest, [ Start, End, or |Anns]) :-
map2(OR, [OrPos|Rest], Rest, [ Start, End, ORANN |Anns]) :-
	(OR = resdisj(Body,Body1) ->
	    ORANN = 'resdisj'
	;
	    OR = ';'(Body,Body1),
	    ORANN = ';'
	),
	OrPos =.. [';', Start,Left,Right],
	End is Start +1,
	flatten_to_list(Left,LeftL),
	flatten_to_list(Right,RightL),	
	map2(Body,LeftL,[],AnnLeft),
	map2(Body1,RightL,[],AnnRight),
	append(AnnLeft,AnnRight,Anns).




map2(IF, [IFPos|Rest], Rest, [Start, End, Ifann| Anns]) :-
	((IF = if(A,B,C), Ifann =if);(IF = resif(A,B,C), Ifann = resif);
	 (IF = semif(A,B,C), Ifann=semif) ),
	IFPos =.. [';',_EPos, IF_part, Else],
	IF_part =.. ['->', Start, Cond, Then],
	End is Start +2,
	flatten_to_list(Cond, CondL),
	flatten_to_list(Then, ThenL),
	flatten_to_list(Else, ElseL),	
	map2(A,CondL, [], AnnCond),
	map2(B,ThenL, [], AnnThen),
	map2(C,ElseL, [], AnnElse),
	append(AnnThen, AnnElse, AnnT),
	append(AnnCond, AnnT, Anns).


map2(LogicalIF, [FAPos|Rest], Rest, [Start, End, LogicalIFAnn| Anns]) :-
	((LogicalIF = logif(A,B,C), LogicalIFAnn =logif);
	 (LogicalIF = reslogif(A,B,C), LogicalIFAnn=reslogif)),
	%FAPos =.. [logif, Start, Cond, Then, Else],
	FAPos =.. [if, Start, Cond, Then, Else],
	End is Start+2, /* 2 = length of "if" */
	flatten_to_list(Cond, CondL),
	flatten_to_list(Then, ThenL),
	flatten_to_list(Else, ElseL),	
	map2(A,CondL, [], AnnCond),
	map2(B,ThenL, [], AnnThen),
	map2(C,ElseL, [], AnnElse),
	append(AnnThen, AnnElse, AnnT),
	append(AnnCond, AnnT, Anns).

map2(Findall, [FAPos|Rest], Rest, [Start, End, FindallAnn| Anns]) :-
	((Findall = findall(A,B,C), FindallAnn =findall);
	 (Findall = resfindall(A,B,C), FindallAnn=resfindall)),
	FAPos =.. [findall, Start, _Pattern, Call, _Bag],
	End is Start+7,
	flatten_to_list(Call,CallL),
	%portray_clause(map2(B,CallL, [], Anns)),
	map2(B,CallL, [], Anns).

map2(Timeout, [FAPos|Rest], Rest, [Start, End, TimeoutAnn| Anns]) :-
	(Timeout = time_out(A,B,C), TimeoutAnn =time_out),
	FAPos =.. [time_out, Start, Call, _Time, _Res],
	End is Start+8,
	flatten_to_list(Call,CallL),
	%portray_clause(map2(B,CallL, [], Anns)),
	map2(A,CallL, [], Anns).


map2(Resnot, [RNPos|Rest], Rest, [Start, End, ResnotAnn| Anns]) :-
	(
	  (Resnot = resnot(A), ResnotAnn = resnot)
	;
	  (Resnot = not(A), ResnotAnn = not)
	),	
	RNPos =.. [\+, Start, Call],
	End is Start+2,
	flatten_to_list(Call,CallL),
	%portray_clause(map2(B,CallL, [], Anns)),
	map2(A,CallL, [], Anns).


map2(Resnot, [RNPos|Rest], Rest, [Start, End, ResnotAnn| Anns]) :-
	(
	  (Resnot = when(Cond,Call), ResnotAnn = when)
	;
	  (
	    (Resnot = reswhen(Cond,Call), ResnotAnn = reswhen)
	  ;
	    (Resnot = semiwhen(Cond,Call), ResnotAnn = semiwhen)
	  )
	
	),	
	RNPos =.. [when, Start, _PosCond,PosCall],
	End is Start+4,
	flatten_to_list(PosCall,PosCallL),
	%portray_clause(map2(B,CallL, [], Anns)),
	map2(Call,PosCallL, [], Anns).




map2(PPMNF, [PPPos|Rest], Rest, [Start, End, PPAnn| Anns]) :-
	(
	  (PPMNF = pp_cll(A), PPAnn = pp_cll,
	      PPPos =.. [pp_cll,Start,Call],
	  	End is Start+6)
	;
	  (
	    (PPMNF = pp_mnf(A), PPAnn = pp_mnf,
		PPPos =.. [pp_mnf,Start,Call],
	    	End is Start+6)
	  ;
	    (PPMNF = mnf(A), PPAnn = mnf,
		PPPos =.. [mnf,Start,Call],
		End is Start+3)
	  )	
	),	
	flatten_to_list(Call,CallL),
	%portray_clause(map2(B,CallL, [], Anns)),
	map2(A,CallL, [], Anns).

	
	
	

	%IFPos =.. [';',_EPos, IF_part, Else],
	%IF_part =.. ['->', Start, Cond, Then],
	%End is Start +2,
	%flatten_to_list(Cond, CondL),
	%flatten_to_list(Then, ThenL),
	%flatten_to_list(Else, ElseL),	
	%map2(A,CondL, [], AnnCond),
	%map2(B,ThenL, [], AnnThen),
	%map2(C,ElseL, [], AnnElse),
	%append(AnnCond, AnnThen, AnnT),
	%append(AnnT, AnnElse, Anns).


map2(_M:logen(Ann,_Call),[PosM|Rest], Rest, [Start, End, Ann]) :-
	arg(3,PosM, PosA),	
	arg(1,PosA, Start),
	functor(PosA,Func,_),
	get_end_pos(Start, Func, End).

map2(logen(Ann,_Call),[PosA|Rest], Rest, [Start, End, Ann]) :-
	arg(1,PosA, Start),
	functor(PosA,Func,_),
         get_end_pos(Start, Func, End).



/*
  Append .ann onto the sourcefile...
*/
get_annfile_name(FileName, AnnFile) :-
	name(FileName, FS),
	append(FS, ".ann", AnnFileS),
	name(AnnFile, AnnFileS).






% test cases
test_or(R) :- map_annotation(';'(
				logen(unfold, f(_A)),
				logen(call,p(_X))
			       ),
			     ;(12,f(16,_B),p(20,_C)), R).

test_or(R) :- map_annotation('resdisj'(
				logen(unfold, f(_A)),
				logen(call,p(_X))
			       ),
			     ;(12,f(16,_B),p(20,_C)), R).

test_pp_call(R) :- map_annotation(pp_cll(logen(unfold, f(_A))),pp_cll(12,f(16,_B)),R).


test_when(R) :- map_annotation(when(ground(A),logen(unfold, f(A))),when(12,ground(13,A),f(16,A)),R).

									


test_logif(R) :- map_annotation(
				logif(
				      logen(unfold,c(A)),
				      logen(unfold,t(A)),
				      logen(unfold,e(A))),
				if(12,
				   c(16,B),
				   t(20,B),
				   e(24,B)
				  ),R).






test_resnot(R) :- map_annotation(resnot(logen(unfold, f(_A))),\+(12,f(16,_B)),R).
test_resnot(R) :- map_annotation(not(logen(unfold, f(_A))),\+(12,f(16,_B)),R).

test_resnot_unknown(R) :- map_annotation_unknown(_,\+(12,f(16,_B)),R).

test_or_unknown(R) :- map_annotation_unknown(_,';'(12,f(16,_B),p(18,_X)),R).


test_findall(R) :- map_annotation(resfindall(A,logen(unfold,f(A,d)),_), findall(16,B,f(27,B,d(31)),_), R).
test1(R) :- map_annotation(logen(unfold,f(_A,d)), f(27,_B,d(31)), R).

test2(R) :- map2(resfindall(_A,logen(unfold,f(_A,d)),_), [findall(16,_B,f(27,_B,d(31)),_)], _, R).











