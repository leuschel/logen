%%%% Stephen-John Craig 2005
%%% This module loads annotations, and returns lists of annotations and filters
%%% load_annotations/4 takes Filename and returns two lists, one of annotations the other filters
%%% The STATE variable is used for saving annotations back to file
%%% Annotations can be modified as long as the lists are the correct length they can be saved...

:- module('ann_manager', [load_annotations/6,
                          load_annotations/7,
                          save_configuration/3,
                          save_annotations/4,
                          save_annotations/5]).


:- use_module('../annfile.pl').
:- use_module(library(lists)).

save_configuration(AnnFile, Ann, Filters) :-
	load_annotations(AnnFile, _,_,_,_,State),
	save_annotations(AnnFile, Ann,Filters, State).


%%% Load annotations from a annfile
%%% returns a list of Annotations and Filters in the order they occur
%%% these can be modified, call save_annotations/4, with STATE variable
%%% and annotations to save.
load_annotations(AnnFileOld, Annotations,AnnSigs, Filters,FilSigs, STATE) :-
    load_annotations(AnnFileOld, Annotations,AnnSigs, Filters,FilSigs, STATE, "../").

load_annotations(AnnFileOld, Annotations,AnnSigs, Filters,FilSigs, STATE, Prefix) :-
	name(AnnFileOld, AnnFileS),
	append(Prefix, AnnFileS, AnnS),
	name(AnnFile, AnnS),
	get_annotations(AnnFile, Annotations,AnnSigs, AnnClauses, Filters,FilSigs, FilterClauses),
	this_module_name(Module),
	STATE = file(Module,AnnClauses, FilterClauses).


% split a path into Directory, Base and Extension components.
% Dir is either empty or ends with '/'.
% Extension is either empty or starts with '.'.
split_components_1(Dir, Base, Ext, FullPath) :-
        name(FullPath, SFullPath),
        append(SDir, SBaseExt, SFullPath),
        \+ memberchk(47, SBaseExt),    % 47 = '/'
        \+ memberchk(92, SBaseExt),    % 92 = '\'  for windows
        !,
        (memberchk(46, SBaseExt) ->    % 46 = '.'
            append(SBase, SExt, SBaseExt),
            SExt = [46 | SExtTail],
            \+ memberchk(46, SExtTail),    % 46 = '.'
            !
        ;
            SBase = SBaseExt,
            SExt = []
        ),
        name(Dir, SDir),
        name(Base, SBase),
        name(Ext, SExt).

%%% +AnnFile: file name to save to
%%% +Annotations: ground list of annotations
%%% +Filters: should be ground list of filters
%%% +STATE: State as returnned by load_annotations/4
save_annotations(AnnFileOld, Annotations, Filters, STATE) :-
	save_annotations(AnnFileOld, Annotations, Filters, STATE,"../").
save_annotations(AnnFileOld, Annotations, Filters, STATE,PREFIX) :-
	(var(STATE) ->
	    raise_exception(instantiation_error(save_annotations(AnnFileOld, Annotations, Filters, STATE),4))
	;
	    true
	),
	name(AnnFileOld, AnnFileS),
	append(PREFIX, AnnFileS, AnnS),
	name(AnnFile, AnnS),
	copy_term(STATE, COPYSTATE),
	COPYSTATE = file(Module,AnnClauses, FilterClauses),
	AnnClauses = clauses(AnnVars, LiftAnn),
	FilterClauses = filters(FilVars, LiftFil),

	split_components_1(Dir,Base,Ext, AnnFile),
	atom_concat(NewModule, '.pl', Base),
	
	%% Ground the annotations to the correct file
	Annotations = AnnVars,
	Filters = FilVars,
	%load_new_annclauses(Module, LiftAnn, LiftFil),
	load_new_annclauses(NewModule, LiftAnn, LiftFil),
	save_current_annotations(AnnFile).


% +AnnFile  The annotation file to load
% ?Annotations, A list of annotations in the file: [memo, unfold, rescall, memo] in order they occur
% ?clause(VarList, LiftAnn) : LiftAnn is a set of ann_clause/3 clauses with variable placeholders instead
%                             of annotations.  Unifying VarList with a list of annotations will instantiate
% ?Filters, A list of filters in the file, in the order they occur
% filters(FilVars, LiftFil) : Same as clause(VarList, LiftAnn) but for filters
get_annotations(AnnFile, Annotations, AnnSigs,clauses(VarList,LiftAnn), Filters, FilSigs,filters(FilVars, LiftFil)) :-
	%% load up a file then get a list of annotations and filters
	load_annfile(AnnFile),
	findall(ann_clause(ID, Head, Body),
		ann_clause(ID, Head, Body),
		Anns),
	lift_ann_l(Anns, LiftAnn, VarList, Annotations, AnnSigs),
	findall(filter(A,B), filter(A,B), Fils),
	lift_filters_l(Fils, LiftFil, FilVars, Filters, FilSigs).


lift_filters_l([],[],[],[], []).
lift_filters_l([filter(A,Filter)|As], [filter(A,LiftFilter)|Bs], Vars, Filters, [Func/Arity|SigR]) :-
	functor(A, Func,Arity),
	length(Filter, Length),
	length(LiftFilter, Length),
	append(Filter, FilterR, Filters),
	append(LiftFilter, VarsR, Vars),
	lift_filters_l(As, Bs,VarsR, FilterR, SigR).

	
	

%%%  Lift a list of annotations
%%% should replace [logen(memo, call(A))] with [logen(Ann, call(A))], [A], [memo]
%%% allows the mutations of annotations in a simple manner
lift_ann_l([], [], [], [], []).
lift_ann_l([ann_clause(ID, H, Body)|As], [ann_clause(ID, H, LiftBody)|Bs], VarList, AnnList, SigList) :-	
	lift_ann(Body, LiftBody, Vars, Anns, Sigs),
	append(Vars, VarsR, VarList),
	append(Anns, AnnsR, AnnList),
	append(Sigs, SigR, SigList),
	lift_ann_l(As, Bs, VarsR, AnnsR, SigR).


%%% lift a single clause
%%% should replace logen(memo, call(A)) with logen(Ann, call(A)), [A], [memo]
lift_ann(true, true,[],[], []) :- !.

lift_ann(resnot(Call),resnot(LiftCall), Vars, Anns,Sigs) :-
	lift_ann(Call,LiftCall, Vars, Anns,Sigs).
	
lift_ann(logen(Ann1, Call), logen(Var1, Call), [Var1], [Ann1],[Func/Arity]) :-
	functor(Call, Func, Arity).

lift_ann((A,B), (NA, NB),Vars, Anns, Sigs) :-
	lift_ann(A, NA, Vars1, Anns1, Sig1),
	lift_ann(B, NB, Vars2, Anns2, Sig2),
	append(Vars1, Vars2, Vars),
	append(Anns1, Anns2, Anns),
	append(Sig1, Sig2, Sigs).





		   

