:- module('bta', [
		annotateFile/3		 
	       ]).

:- use_package(.(sicsignore)).
:- use_module('builtin_db.pl').
:- use_module(library(lists)).
:- use_module(library(system)).

:- use_module('cogen-tools.pl').
:- use_module('logen_messages.pl').


:- use_module(pp,[print_quoted/1]).

:- op(1150, fx, residual).
:- op(1150, fx, filter).
:- op(1150, fx, table).
:- op(1150, fx, type).
:- op(500,yfx,--->).


ciao((:- use_module(library(dec10_io)))).
ciao((:- use_module(library(dcg_expansion)))).


%not(X) :- \+(X).

:- dynamic annotation_mode/1.
annotation_mode(safe).

assert_annotation_mode(M) :- 
   (M = safe ; M = auto_bta),!,
   retractall(annotation_mode(_)),
   assert(annotation_mode(M)).
assert_annotation_mode(M) :- print_error(unrecognised_annotation_mode(M)).

%%
% Read a prolog file and annotate iit
%%


annotateFile(PrologFile,AnnFile,Mode) :-
	reset_already_encountered,
	retractall(annotation_mode(_)),
	assert_annotation_mode(Mode),
	see(PrologFile),
	write_annotateHeader(PrologFile),
	annotate(1),
	seen,
	(file_exists(AnnFile) ->
	    (string_concatenate(AnnFile,'.ann_backup',Backup), rename(AnnFile, Backup))
	;
	    true
	),
	(Mode=auto_bta
	  -> copy_filters(PrologFile)
	  ; true
	),
	telling(Old),
	tell(AnnFile),
	do_delay,
	told,
	tell(Old).

copy_filters(PrologFile) :- /* copy filters from .filters file into .ann file */
   atom_concat(PrologFile,'.filters',FilterFile),
   file_exists(FilterFile),!,
   open(FilterFile, read, FilterStream),
   write_filters(FilterStream),
   close(FilterStream).
copy_filters(PrologFile) :-
   print_error('No file declaring entry filters. Please generate file:'),
   atom_concat(PrologFile,'.filters',FilterFile),
   print_error(FilterFile).


write_filters(F) :-
	read_term(F,Filter,[]),
	(Filter == end_of_file ->
	    true
	;
	    %% If its the norm then just set it and continue
	    (Filter = norm(_Norm) ->
		  true %set_convex_norm(_Norm)
	    ;
		assert(delay(portray_clause(Filter)))
	    ),
	    write_filters(F)
	).

	
	
	
rename(Old,New) :-
	%portray_clause(user_error,rename_file(Old,New)),
	(file_exists(New) -> delete_file(New) ; true),
	rename_file(Old,New).

write_annotateHeader(PrologFilePath) :-
  remove_directory(PrologFilePath,FileName),
   name(FileName,AF),
   (append(AAF,".pl",AF) -> name(ModuleName,AAF) ; ModuleName=FileName),
  assert(delay((print(':- module(\''), print(ModuleName), print('\',[]).'), nl))).
  %assert(delay((print(':- include(\'../logen_source/logen\').'), nl))).


map_vars([]).
map_vars([Name=Var|Tail]) :- Var = '$VAR'(Name), map_vars(Tail).

/* --------------------------------------------------- *
 annotate/1: annotate a clause
 Nr: (in) clause number,
 * --------------------------------------------------- */
 %% steve --- keep variable names as its annoying to loose them....
annotate(ClauseNr) :- /* print_message(ClauseNr), */
	((read_term(RTerm, [variable_names(Vars)]), not(RTerm = end_of_file), 
	  transform_dcg_term(RTerm,Term), map_vars(Vars))
	-> (Term =.. [Functor|Args],
	    ((Functor=(':-'))
	    -> (/* we have a clause */
		((Args = [Head,BodyCommaList])
		 -> (annotate_clause(ClauseNr,Head,BodyCommaList))
		 ;  (annotate_query(ClauseNr,Args))
		)
	       )
	    ;  (/* we have a fact */
		     annotate_clause(ClauseNr,Term,true)
	       )
	    ),
	    CN1 is ClauseNr + 1,
	    (annotate(CN1) -> true ; (print_error('Annotate failed on clause nr'),print_error(CN1)))
	   )
	;  (true,print_message('Annotation done'))
	).

sicstus( (transform_dcg_term(Term,ExpTerm) :-
  expand_term(Term,ExpTerm)) ).  

ciao( (transform_dcg_term(Term,ExpTerm) :-
  dcg_translation( Term , ExpTerm )) ).
	
annotate_query(_,[op(Prio,Assoc,Op)]) :- print_message(executing(op(Prio,Assoc,Op))),!,
    op(Prio,Assoc,Op), 
	assert(delay((print(':- '), print(op(Prio,Assoc,Op)), print('.'), nl))).
annotate_query(_, [include('../logen_source/logen')]) :- !. % ignore declaration
annotate_query(_, [residual(Foo)]) :- !,
	assert(delay((print(':- '), print_quoted(residual(Foo)), print('.'), nl))).
annotate_query(_, [(filter Call:ID)]) :-
	assert(already_encountered(Call)),!,
	assert(delay((print(':- '), print_quoted(filter(Call:ID)), print('.'), nl))).
annotate_query(_, [assert_pre(Pre,Cond)]) :- !,
    print_message(ignoring(assert_pre(Pre))),
	assert(delay((print('/* :- '), print_quoted(assert_pre(Pre,Cond)), print('. */'), nl))).
annotate_query(_, [assert_post(Pre,Cond)]) :- !,
    print_message(ignoring(assert_post(Pre))),
	assert(delay((print('/* :- '), print_quoted(assert_pre(Pre,Cond)), print('. */'), nl))).
annotate_query(_, [Query]) :- !,
	assert(delay((print(':- '), print_quoted(Query), print('.'), nl))).
annotate_query(_,Q) :- print_error('Annotate query failed: '), print_error(Q).



%% --- to annotate a query (query body or a body of a clause or even a fact)

%%% STEVE ADDED CODE HERE TO PRINT FACTS PROPERLY NOT WITH :- TRUE
annotate_clause(_Number,Head, true) :-
	add_head_predicate(Head),
	functor(Head,Func,_),
	assert(delay(portray_clause((logen(Func,Head))))).
annotate_clause(_Number,Head,Body) :-
	add_head_predicate(Head),
	annotate_literal(Body,ABody),
	Head =.. [Func|_],!,
	assert(delay(portray_clause((logen(Func,Head) :- ABody)))).

annotate_clause(_Number,Head,Body) :-
    print_error('Could not annotate clause: '),
    print_error(Head),
    print_error(Body).




%% --- annotate every literal
annotate_literal(logen(Ann,Call), logen(Ann,Call)).

annotate_literal(true,logen(call,true)).
annotate_literal((A,B),(AA,AB)) :-
	annotate_literal(A,AA),
	annotate_literal(B,AB).
annotate_literal((If -> Then ; Else),
		 resif(HAIf,HAThen,HAElse)) :-
	!,annotate_literal(If,AIf),
	hide_nf_literal(AIf,HAIf),
	annotate_literal(Then,AThen),
	hide_nf_literal(AThen,HAThen),
	annotate_literal(Else,AElse),
	hide_nf_literal(AElse,HAElse).
annotate_literal(if(If,Then,Else),
		 reslogif(HAIf,HAThen,HAElse)) :-
	!,annotate_literal(If,AIf),
	hide_nf_literal(AIf,HAIf),
	annotate_literal(Then,AThen),
	hide_nf_literal(AThen,HAThen),
	annotate_literal(Else,AElse),
	hide_nf_literal(AElse,HAElse).
annotate_literal((A;B),resdisj(HAA,HAB)) :-
	annotate_literal(A,AA),
	hide_nf_literal(AA,HAA),
	annotate_literal(B,AB),
	hide_nf_literal(AB,HAB).
annotate_literal(findall(A,B,C),resfindall(A,HAB,C)) :-
	annotate_literal(B,AB),
	hide_nf_literal(AB,HAB).
annotate_literal(pp_cll(Call),pp_cll(Ann)) :- !, /* handle pre_post annotations from ProB/Ecce */
  annotate_literal(Call,Ann).
annotate_literal(pp_mnf(Call),pp_mnf(Ann)) :- !,
  annotate_literal(Call,Ann).
annotate_literal(mnf(Call),mnf(Ann)) :- !,
  annotate_literal(Call,Ann).
annotate_literal(Module:Call,Module:Ann) :- !,  /* check whether this is what we want */
  annotate_literal(Call,Ann).
  
%annotate_literal('='(X,Y),logen(call,'='(X,Y))) :- !.
annotate_literal(Call,logen(Ann,Call)) :- built_in_ann(Call,Ann),!.

annotate_literal(not(X),resnot(AX)) :- !, annotate_literal(X,AX).
annotate_literal(\+(X),resnot(AX)) :- !, annotate_literal(X,AX).
annotate_literal(when(Cond,X),reswhen(Cond,AX)) :- !, annotate_literal(X,AX).
annotate_literal(time_out(X,T,R),time_out(AX,T,R)) :- !, annotate_literal(X,AX).

%annotate_literal(Call,logen(rescall,Call)) :-
annotate_literal(Call,logen(BuiltinAnn,Call)) :-	
	is_built_in_literal(Call),
	(annotation_mode(safe) ->
	    BuiltinAnn = rescall
	;
	    BuiltinAnn = call
	),
	!.

annotate_literal({CLP}, logen(call, {CLP})).
annotate_literal(X,logen(memo,X)) :- annotation_mode(safe),!.
annotate_literal(X,logen(unfold,X)) :- annotation_mode(auto_bta),!.

built_in_ann('='(_,_),call).
built_in_ann('=..'(_,_),rescall).
built_in_ann('!',call).
built_in_ann(fail,rescall).
built_in_ann(print(_),rescall).
built_in_ann(nl,rescall).
built_in_ann(debug_nl,rescall).
built_in_ann(debug_nl(_),rescall).
built_in_ann(debug_print(_),rescall).
built_in_ann(debug_print(_,_),rescall).
built_in_ann(call(_),rescall).



commaToList((A, B) , [A|Tail]) :- commaToList(B, Tail),!.
commaToList(A,[A]).




%% --- an important clause: starting to annotate by given the head
%% --- of the clause
add_head_predicate(Head) :- 
	Head =.. [Pred|Args],
	gen_new_args(Args,NewArgs),
	NewHead =.. [Pred|NewArgs],
	(already_encountered(NewHead)
	 -> (true)
	 ;  (assert(already_encountered(NewHead)),%nl,
	    % numbervars(NewHead,0,_),
	     %functor(NewHead,_,Arity),  
	     %assert(delay((print(':- residual '), print(Pred), print('/'), print(Arity), print('.'), nl))),
	     (annotation_mode(safe) ->
		 (gen_dynamic_args(NewArgs,Status),		
		     Filter =.. [Pred|Status],
		     assert(delay((print(':- filter '),print(Filter), print(':'), print(Pred), print('.'), nl)))
		 )
		 ;
		     true%%% dont create any filter points
	     )
	     

	    )
	).

%length([],0).
%length([_|Tail], X) :- length(Tail, Y), X is Y +1.


%% --- generate a new fresh argument
gen_new_args([],[]).
gen_new_args([_H|T],[_NA|NT]) :-
	gen_new_args(T,NT).
	
%% --- generate dynamic arguments (all are dynamic)
gen_dynamic_args([],[]).
gen_dynamic_args([_H|T],[dynamic|NT]) :-
	gen_dynamic_args(T,NT).

%% ----
:- dynamic already_encountered/1.

reset_already_encountered :-
	retract(already_encountered(_X)),fail.
reset_already_encountered.

:- dynamic delay/1.

reset_delay :-
	retract_all(delay(_)).

ciao(retract_all(X)):-
	retractall(X).

do_delay :-
	(delay(Call) ->
	    (call(Call),retract(delay(Call)), do_delay)
	;
	    true).



