
:- module(save_ann, [save_ann/3,test_save/1]).

:- use_module(library(charsio)).
:- use_module('match_ann.pl').
:- use_module('../cogen.pl').

%% Save annotations given a tcl string of annotations and a file name....


	

save_ann(Filename, Ann, Filters) :-
	
	statistics(runtime,[StartTime,_]),
	get_annfile_name(Filename, AnnFile),

	%write(file(Filename)),
	%nl,write(Ann),nl,nl,
	
	
	open(AnnFile,write,AnnStream),
	
	ann_file_header(AnnStream,Filename),

	statistics(runtime,[AnnHeadTime,_]),
	name(Ann, SAnn),
	parse(SAnn,[], List),
	statistics(runtime,[ParseTime,_]),
	
	annotate(AnnStream,Filename, List, Filters),
	statistics(runtime,[AnnotateTime,_]),

	
	close(AnnStream),
	print_stats(StartTime,AnnHeadTime,ParseTime,AnnotateTime).

print_stats(S,AH,P,AT) :-
	AH_T is AH -S,
	P_T is P - AH,
	AT_T is AT - P,
	format(user_error,"~n~nLoadtime AnnHead:~wms ParseTime:~wms Annotate:~wms~n",[AH_T,P_T,AT_T]).


ann_file_header(S,FileName) :-
	cogen:module_name_from_filename(FileName, Module),
	
	portray_clause(S,':-'(module(Module, []))).

% mal: commented out 20/6/04 as not being used and remove_dir not defined ???
%root_filename(Filename, Name) :-
%	name(Filename, SFile),
%	remove_dir(SFile, SNameExt),
%	remove_ext(SNameExt, SName),
%	name(Name, SName).



annotate(S,FileName, List, Filters) :-

	open(FileName,read,ReadStream),
	annotate_and_write(ReadStream,S,List),
	write_filters(S,Filters),
	close(ReadStream).



/* for now this is fine but if we change the format we need to parse back */
write_filters(S,Filters) :-
	name(Filters, String),
	%current_input(OldStream),
	open_chars_stream(String, FilterStream),
	%set_input(Stream),	
	read_filters(FilterStream,S),
	close(FilterStream).
	%set_input(OldStream).


read_filters(FilterStream,S) :-
	read_term(FilterStream,Term, []),
	(Term \= end_of_file ->
	    (
	      %Term = filter(Filter), 
	      %portray_clause((:-filter(Filter))),
	      %portray_clause((:-filter(Term))),
	      %% NEW STEVE: Just put out to ann file verbatim.
	      portray_clause(S,Term),
	      read_filters(FilterStream,S)
	      )
	;
	    true).	

%% this should have error checking making use of the line number information

annotate_and_write(Read,S,Annotations) :-
	read_term(Read,Term, []),
%	portray_clause(Term),
%	portray_clause(ann(Annotations)),
	(Term \= end_of_file ->
	    (
	      %steve rememvber to remove this...
	      %portray_clause(annotate_clause(Term,Annotations, Rest, AnnClause)),
	      %%%
	      
	      annotate_clause(Term,Annotations, Rest, AnnClause),
	      %% should put write canonical here...
	      portray_clause(S,AnnClause),!,
	      (annotate_and_write(Read,S,Rest)-> true; write(failed))
	      )
	;
	    true).

annotate_clause((:- dynamic(PredSpec)), Ann,Ann,(:-dynamic(PredSpec))) :- !.
annotate_clause((:- op(A,B,C)), Ann,Ann,(:-op(A,B,C))) :- !.

%
annotate_clause((:- use_module(A)), [ModAnn,_,_|Ann],Ann, logen_module(ModAnn,(:-use_module(A)))) :-
        (ModAnn = gx ; ModAnn =gxspec), !.
annotate_clause((:- use_module(A)), Ann,Ann,(:-use_module(A))) :- !. %for non annotated modules
annotate_clause((:- use_module(A,B)), Ann,Ann,(:-use_module(A,B))) :- !.


annotate_clause((:- ensure_loaded(A)), Ann,Ann,(:-ensure_loaded(A))) :- !.
annotate_clause((:-_Declaration),Ann,Ann,(:-true)).
annotate_clause((Head:-Body), Ann, Rest, (AnnHead:-AnnBody)) :-
        !,
	annotate_clause(Head, [],[], AnnHead),
%	trace,
        ar(Body, Ann,Rest,true, AnnBody).

annotate_clause(Fact, Ann,Ann, AnnFact) :-	
	functor(Fact,Func,_Arity),
	AnnFact = logen(Func,Fact).


annotate_clause(F,A,Ann,AnnFact) :-
	portray_clause(user_error,failed_on(annotate_clause(F,A,Ann,AnnFact))),
	fail.

/* ar stands for annotate recursive */

ar(InBody,AnnList , R,RB, AB) :-
	a(InBody, AnnList, AnnRest, LeftBody, AnnBody),
	(LeftBody = true ->
	    R = AnnRest, RB = true, AB = AnnBody
	;	    
	    ar(LeftBody, AnnRest, AnnRest1, LB1, AB1),
	    R = AnnRest1, RB = LB1, AB = (AnnBody,AB1)).

a_nf(Body, Rest, RA, RB, AB) :-
	a(Body, Rest, RestAnn, LeftBody, AnnBody),
	(RestAnn = [end_hide_nf,_,_|Post_nf_ann] ->
	    % finished hide_nf block
	    RA = Post_nf_ann,
	    RB = LeftBody,
	    AB = AnnBody	    	    
	;	    
	    % not finished so continue
	    a_nf(LeftBody, RestAnn, AnnRest1, LB1, AB1),
	    RA = AnnRest1, RB = LB1, AB = (AnnBody,AB1)).	

%debug
%a(A,B,C,D,E) :-
%	portray_clause(a(A,B,C,D,E)),
%	fail.

/* This should be made less hardwired and use ann_db !! :*/
/* a stands for annotate */

a(findall(P,Call,Bag),[ResAnn,_S,_E|Ann], Rest,true, AnnA) :-
	!,
	ar(Call, Ann, Rest, true, CallAnn),
	AnnA =.. [ResAnn, P, CallAnn, Bag].
	
a(time_out(Call,T,R),[ResAnn,_S,_E|Ann], Rest,true, AnnA) :-
	!,
	ar(Call, Ann, Rest, true, CallAnn),
	AnnA =.. [ResAnn, CallAnn, T, R].

a(\+(Call),[ResAnn,_S,_E|Ann], Rest,true, AnnA) :-
	!,
	ar(Call, Ann, Rest, true, CallAnn),
	AnnA =.. [ResAnn, CallAnn].

a(when(Cond,Call),[ResAnn,_S,_E|Ann], Rest,true, AnnA) :-
	!,
	ar(Call, Ann, Rest, true, CallAnn),
	AnnA =.. [ResAnn, Cond,CallAnn].


a(pp_cll(Call),[ResAnn,_S,_E|Ann], Rest,true, AnnA) :-
	!,
	ar(Call, Ann, Rest, true, CallAnn),
	AnnA =.. [ResAnn, CallAnn].

a(pp_mnf(Call),[ResAnn,_S,_E|Ann], Rest,true, AnnA) :-
	!,
	ar(Call, Ann, Rest, true, CallAnn),
	AnnA =.. [ResAnn, CallAnn].

a(mnf(Call),[ResAnn,_S,_E|Ann], Rest,true, AnnA) :-
	!,
	ar(Call, Ann, Rest, true, CallAnn),
	AnnA =.. [ResAnn, CallAnn].


a((If->Then;Else), Ann, Rest,true, AnnA) :-
	
	!,
	ar(If, Ann, [IfAnn, _Start, _End|Rest1],true, IfA),	
	ar(Then, Rest1, Rest2,true, ThenA),
	ar(Else, Rest2, Rest,true, ThenB),
	AnnA =.. [IfAnn, IfA,ThenA,ThenB].

a(if(If,Then,Else), [IfAnn,_Start,_End| Ann], Rest,true, AnnA) :-	
	!,
	
	ar(If, Ann, Rest1,true, IfA),	
	ar(Then, Rest1, Rest2,true, ThenA),
	ar(Else, Rest2, Rest,true, ThenB),
	AnnA =.. [IfAnn, IfA,ThenA,ThenB].





a((Left;Right), Ann, Rest,true, AnnA) :-	
	ar(Left, Ann, [ORAnn, _Start, _End|Rest1],true, LeftA),
	ar(Right, Rest1, Rest,true, RightA),
	!,
	AnnA =.. [ORAnn, LeftA,RightA].





a(Body, [start_hide_nf,_,_|Rest], RA, RB, hide_nf(AB)) :-
	a_nf(Body, Rest, RA, RB, AB).



a((true, A), Ann, Rest, Left, AnnA) :-
	a(A, Ann, Rest, Left, AnnA).
	
a((A, B), Ann, Rest, (RestA, B), AnnA) :-
	!,
	a(A,Ann,Rest, RestA, AnnA).

a(call(A), [mcall,_,_|Rest], Rest, true, logen(mcall,A)) :- !.
a(call(A), [ucall,_,_|Rest], Rest, true, logen(ucall,A)) :- !.

a(M:A, [Ann,_,_|Rest], Rest, true, M:logen(Ann,A)).
a(A, [Ann,_,_|Rest], Rest, true, logen(Ann,A)).




:- use_module(library(lists)).

parse(A,B) :- parse(A,[],B).
parse([],Atom,[A]) :- Atom \= [], !, name(A,Atom).
parse([],_Atom,[]).
parse([32|H], Atom, [A|Ts]) :- Atom \= [],!, name(A,Atom),!,parse(H,[],Ts).
parse([32|H], _Atom, Ts) :- parse(H,[],Ts).
parse([C|H], As, T) :- append(As, [C], New),parse(H,New, T).





test_save_or(Z) :- a(;((f(_A),d(X)),p(X)), [unfold,3.1,3.8,memo,3.9,3.10,';',3.12,3.13,memo,5.1,5.11,memo,6.1,6.11], _X, _Y, Z).
test_save_or(Z) :- a(;((f(_A),d(X)),p(X)), [unfold,3.1,3.8,memo,3.9,3.10,resdisj,3.12,3.13,memo,5.1,5.11,memo,6.1,6.11], _X, _Y, Z).



test_save_if(Z) :- a((p->r;q), [memo,3.1,3.8,resif,3.12,3.13,unfold,5.1,5.11,memo,6.1,6.11], _X, _Y, Z).


test_save_logif(Z) :- a(if(a,b,c),[logif,2.1,2.3, unfold,3.2,3.3,rescall,3.6,3.7, memo, 3.9,4.10], _X,_Y, Z).

test_save(Z) :- a(findall(_A,f(_A,d),_), [resfindall,3.1,3.8,unfold,3.12,3.13,memo,5.1,5.11,memo,6.1,6.11], _X, _Y, Z).

test_save(Z) :- a(\+(f(_A,d)), [resnot,3.1,3.8,unfold,3.12,3.13,memo,5.1,5.11,memo,6.1,6.11], _X, _Y, Z).

test_save(Z) :- a(\+(f(_A,d)), [not,3.1,3.8,unfold,3.12,3.13,memo,5.1,5.11,memo,6.1,6.11], _X, _Y, Z).

test_when_save(R) :-
	a(when(ground(A),f(A,d)), [reswhen,3.1,3.8,unfold,3.12,3.13,memo,5.1,5.11,memo,6.1,6.11], _X, _Y, R).

test_when_save(R) :-
	a(when(ground(A),f(A,d)), [when,3.1,3.8,unfold,3.12,3.13,memo,5.1,5.11,memo,6.1,6.11], _X, _Y, R).
	



%%% annotate_body((Call,Body), [Ann,_Start,_End|Anns], Rest, (ACall,ARest)) :-
%%% 	!,
%%% 	annotate_call(Call, Ann, ACall),
%%% 	annotate_body(Body,Anns, Rest,ARest).


%%% annotate_body(Call, [Ann, _Start,_End|Anns], Anns, ACall) :-
%%% 	annotate_call(Call, Ann, ACall).


%%% %% need to handle using mcall or ucall of call, the call bit shouldnt be kept
%%% annotate_call(call(Call), mcall, logen(mcall, Call)) :- !.
%%% annotate_call(call(Call), ucall, logen(mcall, Call)) :- !.
%%% annotate_call(Call, Ann, logen(Ann, Call)) :- !.



test_save_hidenf(Z) :-
	ar((!,(b_interpreter:b_test_boolean_expression(_1278,_1285,_1286),_1287=term(bool(1));b_not_test_boolean_expression(_1278,_1285,_1286),_1287=term(bool(0))),debug_print(pred_exp_res(_1287)),debug_nl),[call,3.1,3.2,start_hide_nf,-1,-1,memo,6.24|...],_1294,true,Z), portray_clause(Z).


test_save_hidenf2(Z) :-annotate_clause((b_compute_expression('PredicateExpression'(_,[A]),B,C,D):-!,(b_interpreter:b_test_boolean_expression(A,B,C),D=term(bool(1));b_not_test_boolean_expression(A,B,C),D=term(bool(0))),debug_print(pred_exp_res(D)),debug_nl), [call,3.1,3.2,start_hide_nf,-1,-1,memo,6.24,6.49,call,6.78,6.79,end_hide_nf,-1,-1,resdisj,7.8,7.9,memo,8.1,8.39,rescall,8.68,8.69,rescall,9.8,9.19,rescall,9.4,9.48], _, Z).
