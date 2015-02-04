:- module(driver,
	[ analyze/1,
	  analyze/2,
	  module/1,
	  transform/1,
	  transform/2
	],
	[assertions,basicmodes]).

%------------------------------------------------------------------------

:- comment(bug,"Remember_do_the_cleaning_up_lazily").

%------------------------------------------------------------------------
% Reexports.
%------------------------------------------------------------------------
%% :- reexport(ciaopp(options),[
%% 	check_options/0,
%% 	get_option/2,
%% 	get_options/1,
%% 	set_option/2,
%% 	set_options/1]).
:- reexport(ciaopp(preprocess_flags),[
	current_pp_flag/2,
	set_pp_flag/2]).
:- reexport(plai(trace_fixp),[trace_fixp/1]).
:- comment(doinclude,trace_fixp/1).

%------------------------------------------------------------------------
% Basic modules.
%------------------------------------------------------------------------
:- use_module(program(p_unit),
	[ preprocessing_unit/2, 
	  program/2,
	  update_program/2,
	  replace_program/2]). 
:- use_module(program(itf_db),[curr_file/2]).
:- use_module(program(p_abs),[cleanup_p_abs/0]).

%------------------------------------------------------------------------
% Preprocessing modules.
%------------------------------------------------------------------------
:- use_module(plai(plai),[plai/4,cleanup_plai/1]).

:- use_module(typeslib(typeslib),[ undoall_types/0 ]).

:- use_module(spec(spec),[simplify_specialize/6]).

:- use_module(infer(modes),[gather_modes/4]).
:- use_module(infer(vartypes),[gather_vartypes/2]).
:- use_module(infer(infer_db),[cleanup_infer_db/1,domain/1]).
:- use_module(infer(inferseff),[analyze_side_effects/1,cleanup_seff/0]).

:- use_module(ciaopp(infercost)).
:- use_module(ciaopp(infernf)).

:- use_module(granul(tr_granul),[annotate_granul/6]).

:- use_module(myspecializer,[specialize/4]).
:- use_module(tp,[tpr/1]).

%------------------------------------------------------------------------
:- pred module(+File)
	# "Gets all the information needed to preprocess @var{File}
           as the current module.".

module(Module):-
	% cleanup database 
        cleanup_plai(_),
	cleanup_infer_db(_),
	cleanup_seff,
	cleanup_p_abs,
	undoall_types,
	retractall_fact(curr_file(_,_)),
	retractall_fact(domain(_)),
	% load 
	absolute_file_name(Module,'','.pl','.',AbsoluteName,_Base,_Dir),
	inform_user(['{Loading current module from ',AbsoluteName]),
	preprocessing_unit(AbsoluteName,M),
	asserta_fact(curr_file(AbsoluteName,M)),
	inform_user(['}']).

%------------------------------------------------------------------------

:- pred analyze(+Analysis) : analysis
	# "Analyzes the current module with @var{Analysis}.".

analyze(Analysis):-
	curr_file(File,_),
	inform_user(['{Analyzing ',File]),
	analyze(Analysis,_),
	inform_user(['}']).

:- comment(analyze(+Analysis,-Info),"Same as analyze(@var{Analysis})
	but returns information that can be used to check the results
        of the analysis.").

% take care of incompatibilities here!
analyze(cost,_Info):- !,
	current_pp_flag(cost_approximation,X),
	cleanup_infer_db(X),
	assert_domain(X),
	program(Cls,Ds),
	cleanup_infer_db(modes),
	gather_modes(Cls,Ds,NewCls,NewDs),
	complexity_analysis(NewCls,NewDs).
analyze(nfg,nfinfo(Num_Pred,Num_NF_Pred,NCov)):- !,
	assert_domain(nfg),
	program(Cls,_Ds),
	cleanup_infer_db(nfg),
	cleanup_infer_db(vartypes),
	gather_vartypes(Cls,Trusts),
        non_failure_analysis(Cls,Trusts,_TimeNf,Num_Pred,Num_NF_Pred,NCov).
analyze(seff,_Info):- !,
	assert_domain(seff),
	program(Cls,_Ds),
	analyze_side_effects(Cls).
analyze(tp,_Info):- !,
	assert_domain(tp),
	program(Cls,Ds),
	jpg_program_format(Cls,Ds,Cls1),
	tpr(Cls1).
analyze(AbsInt,_Info):-
	assert_domain(AbsInt),
	current_pp_flag(fixpoint,Fixp),
	% some domains may change widen and lub:
	current_pp_flag(widen,W),
	current_pp_flag(lub,L),
	program(Cls,Ds),
	plai(Cls,Ds,Fixp,AbsInt),
	set_pp_flag(lub,L),
	set_pp_flag(widen,W).

assert_domain(AbsInt):-
	current_fact(domain(AbsInt)), !.
assert_domain(AbsInt):-
	asserta_fact(domain(AbsInt)).

:- prop analysis(Analysis)
	# "@var{Analysis} is cost, nfg, det, or an abstract domain.".
:- impl_defined(analysis/1).

%------------------------------------------------------------------------

:- pred transform(+Trans) : transformation
	# "Performs transformation @var{Trans} on the current module.".

transform(Trans):-
	curr_file(File,_),
	inform_user(['{Transforming ',File]),
	transform(Trans,_),
	inform_user(['}']).

:- comment(transform(+Trans,-Info),"Same as transform(@var{Trans})
	but returns information that can be used to check the results
        of the transformation.").

:- multifile transform/2.
:- dynamic transform/2.

transform(myspec,_Info):-
	program(Cls,Ds),
	specialize(Cls,Ds,NewCls,NewDs),
	replace_program(NewCls,NewDs).
transform(spec,Info):-
	simpspec(spec,Info).
transform(simp,Info):-
	simpspec(simp,Info).
transform(grain,Info):-
	program(Cls,Ds),
        annotate_granul(Cls,Ds,OutFile,QKey,NewCls,NewDs),
	update_program(NewCls,NewDs).

simpspec(Spec,_Info):-
	program(Cls,Ds),
	domain(AbsInt), !, % last domain used...
	simplify_specialize(AbsInt,Spec,Cls,Ds,NewCls,NewDs),
	replace_program(NewCls,NewDs).

:- prop transformation(Transformation)
	# "@var{Transformation} is spec, simp or grain.".
:- impl_defined(transformation/1).

%------------------------------------------------------------------------
