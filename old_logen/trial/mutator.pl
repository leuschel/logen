
:- module(mutator, [mutate/2, get_all_mutations/3, get_all_mutations_l/4,get_base_case/2]).

:- use_module(library(lists)).

%%% the defined mutations
mutations(unfold, memo).
mutations(call, rescall).

mutations(static, dynamic).
%mutations(static, nonvar).
mutations(nonvar, dynamic).
mutations(list(_), dynamic).
mutations(type(list(_)), dynamic).
%mutations(type(list(nonvar)), (type(list(dynamic)))).


%mutations(list(static), list(dynamic)).
%mutations(list(static), dynamic).
%mutations(list(dynamic), dynamic).



get_base_case(ann(Ann,Fil), ann(BAnn,BFil)) :-
	get_filter_base_case(Fil,BFil),
	get_memo_base_case(Ann,BAnn).


get_filter_base_case([], []).
get_filter_base_case([_|As], [dynamic|Bs]) :-
	get_filter_base_case(As,Bs).

	
get_memo_base_case([], []).
get_memo_base_case([call|As], [rescall|Bs]) :-
	!, get_memo_base_case(As,Bs).
get_memo_base_case([unfold|As], [memo|Bs]) :-
	!, get_memo_base_case(As,Bs).
get_memo_base_case([A|As], [A|Bs]) :-
	get_memo_base_case(As,Bs).





get_all_mutations_l([], [], _,_).
get_all_mutations_l([ann(Ann,Fil)| As], Mutations, Signatures, QuerySig) :-
	get_all_mutations(Ann,Fil, M1, Signatures, QuerySig),
	get_all_mutations_l(As, M2, Signatures, QuerySig),
	append(M1, M2, Mutations).

	

get_all_mutations(Ann, Fil, [ann(Ann,Fil)|Mutations], signatures(AnnSig, FilSig), QuerySig) :-
	findall(ann(A,Fil), mutate(Ann, A), AnnMutations),
	get_memoed_sigs(Ann, AnnSig, MemoedSigs),
	findall(ann(Ann,F), mutate_filters(Fil, F,FilSig,[QuerySig|MemoedSigs]), FilMutations),
	append(AnnMutations, FilMutations, Mutations).


%% find all call signatures that have been memoed, need for deciding which filters to mutate
get_memoed_sigs([],[],[]).
get_memoed_sigs([memo|As],[Sig|Bs],[Sig|Cs]) :-
	!,
	get_memoed_sigs(As,Bs,Cs).
get_memoed_sigs([_|As],[_|Bs],Cs) :-
	get_memoed_sigs(As,Bs,Cs).


	



	


%%% returns mutations, not including original
%mutate([], []).
mutate([Ann|As], [MAnn|As]) :- mutations(Ann, MAnn).
mutate([Ann|As], [Ann| Bs]) :-
	mutate(As, Bs).

mutate_filters([Ann|As], [MAnn|As],[Sig|_], Memoed) :-
	(member(Sig, Memoed) ->	mutations(Ann, MAnn)).
mutate_filters([Ann|As], [Ann| Bs], [_|Cs], Memoed) :-
	mutate_filters(As, Bs, Cs, Memoed).



