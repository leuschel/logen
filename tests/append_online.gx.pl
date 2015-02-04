(:-use_module(library(lists))).
:- dynamic
        gx_data/2.
generalise_call(Filters, Call, GenCall) :-
        Call=..[F|Args],
        gen_filters_l(Filters, Args, GArgs, _),
        GenCall=..[F|GArgs].
filter_call(Filters, Call, GCall, FCall) :-
        Call=..[F|Args],
        GCall=..[F|GArgs],
        gen_filters_l(Filters, Args, GArgs, FArgs),
        gensym(F, NewF),
        FCall=..[NewF|FArgs].
generalise_and_filter(Filters, Call, GenCall, FCall) :-
        Call=..[F|Args],
        gen_filters_l(Filters, Args, GenArgs, FilArgs),
        GenCall=..[F|GenArgs],
        gensym(F, NewF),
        FCall=..[NewF|FilArgs].
gen_filters_l([Fil|___Fils], Args, GenArgs, FilArgs) :-
        gen_filter_l(Fil, Args, GenArgs, FilArgs).
gen_filter_l([], [], [], []).
gen_filter_l([static|Fs], [A|Args], [A|GenArgs], FArgs) :-
        gen_filter_l(Fs, Args, GenArgs, FArgs).
gen_filter_l([dynamic|Fs], [_|Args], [A|GenArgs], [A|FArgs]) :-
        gen_filter_l(Fs, Args, GenArgs, FArgs).
gensym(H, NewHead) :-
        (   gx_data(sym, Sym) ->
            NewSym is Sym+1,
            retract(gx_data(sym,Sym))
        ;   NewSym=0
        ),
        assert(gx_data(sym,NewSym)),
        add_id(H, NewSym, NewHead).
add_id(H, Sym, NH) :-
        atom_concat(H, '__', H1),
        name(H1, H1S),
        name(Sym, SymS),
        append(H1S, SymS, NHS),
        name(NH, NHS).
build_unfold_call(Call, SpecCode, LogenData, UnfoldCall) :-
        Call=..[Func|Args],
        atom_concat(Func, '_u', NewFunc),
        append(Args, [SpecCode,LogenData], NewArgs),
        UnfoldCall=..[NewFunc|NewArgs].
build_request_call(Call, Req, ResCall, LogenData, RequestCall) :-
        Call=..[Func|Args],
        atom_concat(Func, '_request', NewFunc),
        append(Args, [Req,ResCall,LogenData], NewArgs),
        RequestCall=..[NewFunc|NewArgs].
:- dynamic
        memo_table/4.
:- dynamic
        spec_clause/1.
(:-use_module(library(terms))).
find_pattern(ID, Call, ResCall, _Requestor) :-
        copy_term(Call, CallCopy),
        memo_table(ID, Call, ResCall, _MEMODATA),
        variant(CallCopy, Call).
insert_pattern(ID, GCall, FCall, MEMODATA) :-
        assert(memo_table(ID,GCall,FCall,MEMODATA)).
update_status(ID, GCall, FCall, Req) :-
        retract(memo_table(ID,GCall,FCall,MEMODATA)),
        get_memodata_requestor(MEMODATA, pending(Req)),
        set_memodata_requestor(MEMODATA, Req, MEMODATAPRIME),
        assert(memo_table(ID,GCall,FCall,MEMODATAPRIME)).
spec_driver :-
        memo_table(ID, GCall, FCall, [pending(Req)|_MEMODATA]),
        update_status(ID, GCall, FCall, Req), !,
        generate_code(GCall, FCall),
        spec_driver.
spec_driver.
generate_code(Call, ResCall) :-
        copy_term((Call,ResCall), (CCall,CResCall)),
        build_unfold_call(Call, Res, [[CCall],CResCall], UnfoldCall),
        findall((ResCall:-Res), UnfoldCall, Clauses),
        save_clauses(Clauses).
save_clauses([]).
save_clauses([C|Cs]) :-
        assert(spec_clause(C)),
        save_clauses(Cs).
print_clauses(S) :-
        spec_clause(C),
        portray_clause(S, C),
        fail.
print_clauses(_).
get_logendata_id([_,ID|_LogenData], ID).
set_logendata_id(ID, [H,_OLDID|LogenData], [H,ID|LogenData]).
get_logendata_history([History|_LogenData], History).
set_logendata_history(History, [_|LogenData], [CHistory|LogenData]) :-
        copy_term(History, CHistory).
get_memodata_requestor([REQ|_MEMODATA], REQ).
set_memodata_requestor([_OLD|MEMODATA], Req, [Req|MEMODATA]).
get_memodata_id([_Req,ID|_MEMODATA], ID).
set_memodata_id([Req,_|MEMODATA], ID, [Req,ID,MEMODATA]).
init_memodata(Requestor, Parent, [Requestor,Parent]).
confirm_user(online_unsafe_unfold, [Call,History]) :-
        format(user, [126,110,126,110,33,33,32,67,97,108,108,32,126,119,32,117,110,102,111,108,100,101,100,44,32,108,111,111,107,115,32,117,110,115,97,102,101,63,126,110,72,105,115,116,111,114,121,58,32,126,119,126,110], [Call,History]),
        flush_output(user),
        halt.
(:-use_module('../homeomorphic.pl')).
generalise_online(Call, GenCall, ParentID) :-
        portray_clause(generalise_online(Call,ParentID)),
        get_history(ParentID, History),
        portray_clause(history(History)),
        generalise_online_history(Call, GenCall, History).
(:-use_module('../msg.pl')).
generalise_online_history(Call, Call, []).
generalise_online_history(Call, GCall, [H|Hs]) :-
        (   homeomorphic_embedded(H, Call) ->
            msg(H, Call, MGU),
            generalise_online_history(MGU, GCall, Hs)
        ;   generalise_online_history(Call, GCall, Hs)
        ).
get_history(entry, []) :- !.
get_history(ID, [History|Hs]) :-
        memo_table(_, History, ID, MEMODATA),
        get_memodata_id(MEMODATA, ParentID),
        get_history(ParentID, Hs).
filter_online(_Call, GenCall, ResCall) :-
        term_variables(GenCall, Variables),
        GenCall=..[F|_Args],
        gensym(F, NewF),
        ResCall=..[NewF|Variables].
is_safecall(_Call, []).
is_safecall(Call, [H|Hs]) :-
        (   homeomorphic_embedded(H, Call) ->
            fail
        ;   is_safecall(Call, Hs)
        ).
append_request(A, B, C, Requestor, ResidualCall, __LOGENDATA) :-
        (   find_pattern(append, append(A,B,C), ResidualCall, Requestor) ->
            true
        ;   __LOGENDATA=[_,D|_],
            generalise_online(append(A,B,C), GenCall, D),
            filter_online(append(A,B,C), GenCall, ResidualCall),
            init_memodata(pending(Requestor), D, E),
            insert_pattern(append, GenCall, ResidualCall, E)
        ),
        GenCall=append(A,B,C).
test_request(A, B, Requestor, ResidualCall, __LOGENDATA) :-
        generalise_call([[dynamic,dynamic]], test(A,B), GenCall),
        (   find_pattern(append, test(A,B), ResidualCall, Requestor) ->
            true
        ;   __LOGENDATA=[_,C|_],
            filter_call([[dynamic,dynamic]], test(A,B), GenCall, ResidualCall),
            init_memodata(pending(Requestor), C, D),
            insert_pattern(append, GenCall, ResidualCall, D)
        ),
        GenCall=test(A,B).
test_u(A, B, C, __LOGENDATA) :-
        append_u([a,a,c|A], [x,y], B, C, __LOGENDATA).
append_u([], A, A, true, __LOGENDATA).
append_u([A|B], C, [A|D], E, __LOGENDATA) :-
        get_logendata_history(__LOGENDATA, HISTORY),
        set_logendata_history([append(B,C,D)|HISTORY], __LOGENDATA, LOGENDATAPRIME),
        (   is_safecall(append(B,C,D), HISTORY) ->
            append_u(B, C, D, E, LOGENDATAPRIME)
        ;   confirm_user(online_unsafe_unfold, [append(B,C,D),HISTORY]),
            append_u(B, C, D, E, LOGENDATAPRIME)
        ).
append_entry(Goal, ResCall) :-
        build_request_call(Goal, crossmodule, ResCall, [[],entry], REQ),
        REQ,
        spec_driver,
        print_clauses(user).
