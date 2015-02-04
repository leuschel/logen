%% [PM] 970306 There has been a number of broken versions of this
%% floating around under the name apc.pl. In particular some of them
%% contain a definition of expand_term that will fail as called
%% from init_ptree/2 via cls_to_ptrees/2.

%% Note, even in this version there are approx. 200 unreached
%% predicates.


% :- public main/0.                               % for xref
%% This is needed as the original code makes an op declaration of \=
%% using the syntax '\='. Unfortunately the backslash is interpreted
%% as the start of an escape sequence in SICStus and the result of
%% doing ":- op( 700, xfx, ['\='])." is to (re)define the operator
%% precedence of = (ie, in effect, it becomes a no-op)
:- op( 700, xfx, [(\=)]).

%%%%% BELOW IS EXACTLY AS FOUND IN
%%%%% <http://www.info.ucl.ac.be/people/PVR/aquarius_compiler.pl>
% Aquarius Prolog compiler
% Copyright (C) 1989-91 P. Van Roy and Regents of the University of California
% All rights reserved.
% Creation date Fri Sep 20 10:16:37 PDT 1991

% See /n/ps-dec3/local1/vanroy/Aquarius1.0.msg/AquaOld/Ralph.Article/FixPoint 

% :-mips.
%:-option(float).
%%:-option(analyze).
%:-notoption(comment).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Entry point for extended DCG expansion:
%:- entry((term_expansion(_,_) :- true)).

% Entry points for correct dataflow analysis:
%:- entry((op_table(_,_,_) :- true)).
%:- entry((range(_,_,_) :- true)).
%:- entry((test_arg(_,_) :- true)).
%:- entry((a_head(_,_) :- true)).
%:- entry((valid_testset(_,_,_,_,_,_,_,_,_,_) :- true)).
%:- entry((entry_data(_) :- true)).

% Operator declarations:
:- op(1200, xfx, ['-->>']).   % Same as ':-'.
:- op(1200, xfx, ['-->']).   % Same as ':-'.
:- op( 850, xfx, [':']).      % Slightly tighter than ',' and '\+'.
:- op( 700, xfx, ['\=']).
:- op( 500, yfx, [and,or,xor]).
:- op(1150,  fx, [(init_directive)]). % Same op declaration as 'dynamic'.
:- op(1150,  fx, [(mode)]). % Same op declaration as 'dynamic'.

% The dynamic predicates: (needed for Quintus only)
:- dynamic(compile_cputime/3).        % See stats in 'utility'.
:- dynamic(gensym_integer/1).         % See gensym in 'utility'.
:- dynamic(include_stack/1).
:- dynamic(compile_option/1).
:- dynamic(mode_option/5).
:- dynamic(modal_entry/2).
:- dynamic(save_clause/2).       % For piped compiler.
:- dynamic(macro/6).
:- dynamic(select_option/2).     % For evaluation of determinism selection.
:- dynamic(dyn_pred/1).          % Set of declared dynamic predicates.
:- dynamic(init_clause/1).       % Set of clauses to be executed during init.
:- dynamic((init_directive)/1).    % Directive that is allowed during init.
:- dynamic(global_copy/1).

% Top level loop:
main :- adefault, pipe.

constC(A,B,C) :- A=[B|C].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

term_expansion((A-->>B),(C:-D)) :-
        functor(A,E,F),
        'u_has_hidden'(A,G),
        'u_new_goal'(A,G,H,C),
        'u_create_acc_pass'(G,H,C,I,J),
        'u_flat_conj'(B,K),
        'u_expand_body'(K,L,E/F,G,I,J),
        'u_flat_conj'(L,D),
        !.

library_directory('/hprg/q2.0/library').
library_directory('/hprg/q2.0/tools').
library_directory('/hprg/q2.0/IPC').

copyright(1,'Aquarius Prolog compiler').
copyright(2,'Copyright (C) 1989-91 P. Van Roy and Regents of the University of California').
copyright(3,'All rights reserved.').

compiler_version('Fri Sep 20 10:16:37 PDT 1991').

'u_has_hidden'(A,B) :-
        functor(A,C,D),
        pred_info(C,D,B).
'u_has_hidden'(A,[]) :-
        functor(A,B,C),
        \+pred_info(B,C,D).

'u_new_goal'(A,B,C,D) :-
        functor(A,E,C),
        'u_number_args'(B,C,F),
        functor(D,E,F),
        'u_match'(1,C,A,D).

'u_create_acc_pass'([],A,B,[],[]).
'u_create_acc_pass'([A|B],C,D,[acc(A,E,F)|G],H) :-
        'u_is_acc'(A),
        !,
        I is C+1,
        arg(I,D,E),
        J is C+2,
        arg(J,D,F),
        'u_create_acc_pass'(B,J,D,G,H).
'u_create_acc_pass'([A|B],C,D,E,[pass(A,F)|G]) :-
        'u_is_pass'(A),
        !,
        H is C+1,
        arg(H,D,F),
        'u_create_acc_pass'(B,H,D,E,G).
'u_create_acc_pass'([A|B],C,D,E,F) :-
        \+'u_is_acc'(A),
        \+'u_is_pass'(A),
        write('*** Error: '),
        write(A),
        write(' is not a hidden parameter.'),
        nl.

'u_flat_conj'(A,B) :-
        'u_flat_conj'(A,B,true).

'u_expand_body'(true,true,A,B,C,D) :-
        'u_finish_acc'(C).
'u_expand_body'((A ',' B),(C ',' D),E,F,G,H) :-
        'u_expand_goal'(A,C,E,F,G,I,H),
        'u_expand_body'(B,D,E,F,I,H).

'u_finish_acc'([]).
'u_finish_acc'([acc(A,B,B)|C]) :-
        'u_finish_acc'(C).

'u_expand_goal'({A},A,B,C,D,D,E) :- !.
'u_expand_goal'(insert(A,B),C=A,D,E,F,G,H) :-
        'u_replace_acc'(dcg,C,I,B,I,F,G),
        !.
'u_expand_goal'(insert(A,B):C,D=A,E,F,G,H,I) :-
        'u_replace_acc'(C,D,J,B,J,G,H),
        !.
'u_expand_goal'(A:B,C,D,E,F,G,H) :-
        \+'u_list'(A),
        'u_has_hidden'(A,[]),
        !,
        'u_make_list'(B,I),
        'u_new_goal'(A,I,J,C),
        'u_use_acc_pass'(I,J,C,F,G,H).
'u_expand_goal'(A:B,C,D,E,F,G,H) :-
        \+'u_list'(A),
        'u_has_hidden'(A,I),
        I\==[],
        !,
        'u_make_list'(B,J),
        'u_new_goal'(A,I,K,C),
        'u_replace_defaults'(I,L,J),
        'u_use_acc_pass'(L,K,C,F,G,H).
'u_expand_goal'(A:B,C,D,E,F,G,H) :-
        'u_list'(A),
        !,
        'u_joiner'(A,B,D,C,F,G).
'u_expand_goal'(A,B,C,D,E,F,G) :-
        'u_list'(A),
        !,
        'u_joiner'(A,dcg,C,B,E,F).
'u_expand_goal'(A/B,true,C,D,E,E,F) :-
        var(A),
        nonvar(B),
        'u_member'(acc(B,A,G),E),
        !.
'u_expand_goal'(A/B,true,C,D,E,E,F) :-
        var(A),
        nonvar(B),
        'u_member'(pass(B,A),F),
        !.
'u_expand_goal'(A/B,true,C,D,E,E,F) :-
        var(B),
        nonvar(A),
        'u_member'(acc(A,G,B),E),
        !.
'u_expand_goal'(A/B/C,true,D,E,F,F,G) :-
        var(A),
        var(C),
        nonvar(B),
        'u_member'(acc(B,A,C),F),
        !.
'u_expand_goal'(A/B,true,C,D,E,E,F) :-
        write('*** Warning: in '),
        write(C),
        write(' the term '),
        write(A/B),
        write(' uses a non-existent hidden parameter.'),
        nl.
'u_expand_goal'(A,B,C,D,E,F,G) :-
        'u_has_hidden'(A,H),
        !,
        'u_new_goal'(A,H,I,B),
        'u_use_acc_pass'(H,I,B,E,F,G).

'u_replace_acc'(A,B,C,D,E,F,G) :-
        'u_member'(acc(A,B,C),F),
        !,
        'u_replace'(acc(A,H,I),acc(A,D,E),F,G).

'u_list'(A) :-
        nonvar(A),
        A=[B|C],
        !.
'u_list'(A) :-
        A==[],
        !.

'u_make_list'(A,[A]) :-
        \+'u_list'(A),
        !.
'u_make_list'(A,A) :-
        'u_list'(A),
        !.

'u_use_acc_pass'([],A,B,C,C,D).
'u_use_acc_pass'([A|B],C,D,E,F,G) :-
        'u_replace_acc'(A,H,I,J,I,E,K),
        !,
        L is C+1,
        arg(L,D,H),
        M is C+2,
        arg(M,D,J),
        'u_use_acc_pass'(B,M,D,K,F,G).
'u_use_acc_pass'([A|B],C,D,E,F,G) :-
        'u_acc_info'(A,H,I),
        !,
        J is C+1,
        arg(J,D,H),
        K is C+2,
        arg(K,D,I),
        'u_use_acc_pass'(B,K,D,E,F,G).
'u_use_acc_pass'([A|B],C,D,E,F,G) :-
        'u_is_pass'(A),
        'u_member'(pass(A,H),G),
        !,
        I is C+1,
        arg(I,D,H),
        'u_use_acc_pass'(B,I,D,E,F,G).
'u_use_acc_pass'([A|B],C,D,E,F,G) :-
        'u_pass_info'(A,H),
        !,
        I is C+1,
        arg(I,D,H),
        'u_use_acc_pass'(B,I,D,E,F,G).
'u_use_acc_pass'([A|B],C,D,E,E,F) :-
        write('*** Error: the hidden parameter '),
        write(A),
        write(' does not exist.'),
        nl.

'u_replace_defaults'([],[],A).
'u_replace_defaults'([A|B],[C|D],E) :-
        'u_replace_default'(A,C,E),
        'u_replace_defaults'(B,D,E).

'u_joiner'([],A,B,true,C,C).
'u_joiner'([A|B],C,D,(E ',' F),G,H) :-
        'u_replace_acc'(C,I,J,K,J,G,L),
        'u_acc_info'(C,A,I,K,E,M,N),
        !,
        'u_joiner'(B,C,D,F,L,H).
'u_joiner'([A|B],C,D,E,F,G) :-
        write('*** Warning: in '),
        write(D),
        write(' the accumulator '),
        write(C),
        write(' does not exist.'),
        nl,
        'u_joiner'(B,C,D,E,F,G).

'u_member'(A,[A|B]).
'u_member'(A,[B|C]) :-
        'u_member'(A,C).

'u_is_acc'(A) :-
        atomic(A),
        !,
        'u_acc_info'(A,B,C,D,E,F,G).
'u_is_acc'(A) :-
        functor(A,B,2),
        !,
        'u_acc_info'(B,C,D,E,F,G,H).

'u_is_pass'(A) :-
        atomic(A),
        !,
        'u_pass_info'(A,B).
'u_is_pass'(A) :-
        functor(A,B,1),
        !,
        'u_pass_info'(B,C).

'u_acc_info'(A,B,C) :-
        functor(A,D,2),
        'u_is_acc'(D),
        !,
        arg(1,A,B),
        arg(2,A,C).
'u_acc_info'(A,B,C) :-
        'u_acc_info'(A,D,E,F,G,B,C).

'u_pass_info'(A,B) :-
        functor(A,C,1),
        'u_is_pass'(C),
        !,
        arg(1,A,B).
'u_pass_info'(A,B) :-
        pass_info(A,B).
'u_pass_info'(A,B) :-
        pass_info(A).

'u_replace'(A,B,[],[]).
'u_replace'(A,B,[A|C],[B|D]) :- !,
        'u_replace'(A,B,C,D).
'u_replace'(A,B,[C|D],[C|E]) :-
        \+C=A,
        !,
        'u_replace'(A,B,D,E).

'u_number_args'([],A,A).
'u_number_args'([A|B],C,D) :-
        'u_is_acc'(A),
        !,
        E is C+2,
        'u_number_args'(B,E,D).
'u_number_args'([A|B],C,D) :-
        'u_is_pass'(A),
        !,
        E is C+1,
        'u_number_args'(B,E,D).

'u_match'(A,B,C,D) :-
        A>B,
        !.
'u_match'(A,B,C,D) :-
        A=<B,
        !,
        arg(A,C,E),
        arg(A,D,E),
        F is A+1,
        'u_match'(F,B,C,D).

pred_info('u_dummy',0,A).

'u_acc_info'(A,B,C,D,E,F,G) :-
        acc_info(A,B,C,D,E,F,G).
'u_acc_info'(A,B,C,D,E,F,G) :-
        acc_info(A,B,C,D,E).
'u_acc_info'(dcg,A,B,C,B=[A|C],D,[]).

acc_info('u_dummy',A,B,C,D,E,F).

acc_info('u_dummy',A,B,C,D).

pass_info('u_dummy',A).

pass_info('u_dummy').

'u_replace_default'(A,B,C) :-
        functor(B,A,2),
        'u_member'(B,C),
        !.
'u_replace_default'(A,B,C) :-
        functor(B,A,1),
        'u_member'(B,C),
        !.
'u_replace_default'(A,B,C) :-
        A=B.

'u_flat_conj'(true,A,A).
'u_flat_conj'((A ',' B),C,D) :-
        'u_flat_conj'(A,C,E),
        'u_flat_conj'(B,E,D).
'u_flat_conj'(A,(A ',' B),B) :-
        \+A=true,
        \+A= (C ',' D).

'u_append'([],A,A).
'u_append'([A|B],C,[A|D]) :-
        'u_append'(B,C,D).

include_stack([]).

default_option(flat).
default_option(write).
default_option(source).
default_option(peep).
default_option(low_reg(0)).
default_option(high_reg(1000)).
default_option(low_perm(0)).
default_option(align(2)).
default_option(compile).
default_option(factor).
default_option(uni).
default_option(select_limit(1)).
default_option(hash_size(5)).
default_option(functor_limit(5)).
default_option(include_limit(8)).
default_option(depth_limit(2)).
default_option(short_block(6)).
default_option(write_once).
default_option(same_number_solutions).
default_option(same_order_solutions).
default_option(split_integer).
default_option(system(quintus)).
default_option(user_test_size(1,2)).
default_option(analyze_uninit_reg).
default_option(protect_builtins).

low_reg(A) :-
        compile_option(low_reg(B)),
        !,
        A=B.
low_reg(A) :-
        default_option(low_reg(B)),
        !,
        A=B.

high_reg(A) :-
        compile_option(high_reg(B)),
        !,
        A=B.
high_reg(A) :-
        default_option(high_reg(B)),
        !,
        A=B.

low_perm(A) :-
        compile_option(low_perm(B)),
        !,
        A=B.
low_perm(A) :-
        default_option(low_perm(B)),
        !,
        A=B.

align(A) :-
        compile_option(align(B)),
        !,
        A=B.
align(A) :-
        default_option(align(B)),
        !,
        A=B.

the_system(A) :-
        compile_option(system(B)),
        !,
        A=B.
the_system(A) :-
        default_option(system(B)),
        !,
        A=B.

select_limit(A) :-
        compile_option(select_limit(B)),
        !,
        A=B.
select_limit(A) :-
        default_option(select_limit(B)),
        !,
        A=B.

depth_limit(A) :-
        compile_option(depth_limit(B)),
        !,
        A=B.
depth_limit(A) :-
        default_option(depth_limit(B)),
        !,
        A=B.

arith_error_check :-
        compile_option(arithmetic_error_check),
        !.

split_integer :-
        compile_option(split_integer),
        !.

float :-
        compile_option(float),
        !.

merge_add :-
        compile_option(merge_add),
        !.

write_once :-
        compile_option(write_once),
        !.

same_number :-
        compile_option(same_number_solutions),
        !.

same_order :-
        compile_option(same_order_solutions),
        !.

mips :-
        compile_option(mips),
        !.

sparc :-
        compile_option(sparc),
        !.

mc68020 :-
        compile_option(mc68020),
        !.

pragma_write_once(write_once) :- !,
        w((:-write_once)),
        wn('.'),
        nl.
pragma_write_once(A).

w(A) :-
        write(A).

wn(A) :-
        write(A),
        nl.

pragma_no_write_once(write_once) :- !,
        w((:-no_write_once)),
        wn('.'),
        nl.
pragma_no_write_once(A).

pipe :-
        prompt(A,''),
        print_version,
        read_expand(B),
        pipe_loop(B).

print_version :-
        nl,
        print_copyright,
        print_date,
        nl.

read_expand(A) :-
        read(B),
        expand_term(B,A).

pipe_loop(end_of_file) :-
        pop_incl_stack(A),
        !,
        seen,
        see(A),
        read_expand(B),
        pipe_loop(B).
pipe_loop(end_of_file) :- !.
pipe_loop(A) :-
        pipe_loop_one(A,B),
        asserta(save_clause(B)),
        fail.
pipe_loop(A) :-
        retract(save_clause(B)),
        pipe_loop(B).

pop_incl_stack(A) :-
        include_stack([A|B]),
        my_retractall(include_stack(C)),
        asserta(include_stack(B)).

pipe_loop_one(A,B) :-
        collect_clauses(A,B,C),
        internal_comp(C),
        !.

collect_clauses(end_of_file,A,B) :-
        pop_incl_stack(C),
        !,
        seen,
        see(C),
        read_expand(D),
        collect_clauses(D,A,B).
collect_clauses(end_of_file,end_of_file,[]) :- !.
collect_clauses(A,B,C) :-
        one_step(A,D,C,E),
        collect_clauses(D,B,E).

internal_comp(A) :-
        internal_comp(write,A,B).

one_step((:-A),B,[(:-A)|C],C) :-
        after_collect(A),
        !,
        handle_init(A),
        read_expand(B).
one_step((:-A),B,C,C) :- !,
        handle_dir(A),
        handle_init(A),
        read_expand(B).
one_step(A,B,[A|C],D) :-
        getname(A,E),
        read_clauses(E,B,C,D).

after_collect((A ',' B)) :- !,
        after_collect(A),
        after_collect(B).
after_collect(A) :-
        nonvar(A),
        A=..[B|C],
        after_collect(B,C).

handle_init(A) :-
        init_directives(A),
        !,
        translate(A,B),
        add_init(B).
handle_init(A).

handle_dir(A) :-
        var(A),
        !,
        warning('Variable directive ignored').
handle_dir((A ',' B)) :- !,
        handle_dir(A),
        handle_dir(B).
handle_dir(A) :-
        add_mode_option(A),
        !.
handle_dir(A) :-
        add_entry_option(A),
        !.
handle_dir(A) :-
        add_modal_entry(A),
        !.
handle_dir(A) :-
        add_macro_def(A),
        !.
handle_dir(A) :-
        A=..[B|C],
        handle_dir(B,C),
        !.
handle_dir(A) :-
        \+ (init_directive A),
        !,
        warning(['Non-existent directive ''',A,''' ignored']).
handle_dir(A) :-
        (init_directive A),
        !.

getname(A,B/C) :-
        split(A,D,E),
        functor(D,B,C),
        !.

read_clauses(A,B,C,D) :-
        read_expand(E),
        getname(E,F),
        (   F=A ->
            C=[E|G],
            read_clauses(A,B,G,D)
        ;   C=D,
            B=E
        ),
        !.

split(A,B,C) :-
        (   A= (B:-C) ->
            true
        ;   A=B,
            C=true
        ).

warning(A) :-
        make_msg(A,B),
        w,
        msg_list(B,w).

add_mode_option((mode true)) :- !.
add_mode_option((mode (A:-B))) :-
        check_non_builtin(A),
        !,
        my_retractall(mode_option(A,C,D,E,F)),
        make_req_bef(B,G,H),
        keep_uninit(A,G,I),
        asserta(mode_option(A,I,H,true,n)).
add_mode_option((mode A)).
add_mode_option(analyze_mode(A,B,C,D)) :-
        check_non_builtin(A),
        !,
        survive(A,E),
        my_retractall(mode_option(A,F,G,H,I)),
        keep_uninit(A,B,J),
        keep_uninit(A,C,K),
        translate(J,L),
        flat_conj(L,M),
        logical_simplify(M,N),
        translate(K,O),
        flat_conj(O,P),
        logical_simplify(P,Q),
        translate(D,R),
        flat_conj(R,S),
        logical_simplify(S,T),
        asserta(mode_option(A,N,Q,T,E)).
add_mode_option(analyze_mode(A,B,C,D)).
add_mode_option(dummy_mode(A,B,C)) :-
        check_non_builtin(A),
        !,
        survive(A,D),
        after(A,E),
        my_retractall(mode_option(A,F,G,H,I)),
        keep_uninit(A,B,J),
        split_deref(J,K,L),
        keep_uninit(A,C,M),
        translate(L,N),
        flat_conj(N,O),
        logical_simplify(O,P),
        translate((K ',' M),Q),
        flat_conj(Q,R),
        logical_simplify(R,S),
        asserta(mode_option(A,P,S,E,D)).
add_mode_option(dummy_mode(A,B,C)).
add_mode_option(mode(A,B,C,D,E)) :-
        check_non_builtin(A),
        !,
        my_retractall(mode_option(A,F,G,H,I)),
        check_req_bef(B,C,J,K,A),
        keep_uninit(A,J,L),
        translate(L,M),
        logical_simplify(M,N),
        keep_uninit(A,K,O),
        translate(O,P),
        logical_simplify(P,Q),
        translate(D,R),
        logical_simplify(R,S),
        asserta(mode_option(A,N,Q,S,E)).
add_mode_option(mode(A,B,C,D,E)).

add_entry_option(entry(A,B,C,D,E)) :-
        add_mode_option(mode(A,B,C,D,E)),
        nox(entry(A,F)),
        translate((B ',' C),G),
        flat_conj(G,H),
        logical_simplify(H,I),
        asserta(compile_option(entry(A,I))).
add_entry_option(entry((A:-B))) :-
        add_mode_option((mode (A:-B))),
        nox(entry(A,C)),
        translate(B,D),
        flat_conj(D,E),
        logical_simplify(E,F),
        asserta(compile_option(entry(A,F))).
add_entry_option(entry(A/B)) :-
        atom(A),
        nonnegative(B),
        functor(C,A,B),
        add_mode_option((mode (C:-true))),
        nox(entry(C,D)),
        asserta(compile_option(entry(C,true))).

add_modal_entry(modal_entry(A,B)) :-
        check_modal_entry(A,B),
        !,
        my_retractall(modal_entry(A,C)),
        asserta(modal_entry(A,B)).
add_modal_entry(modal_entry(A,B)).

add_macro_def(macro((A:-B))) :-
        check_macro_def(A),
        !,
        my_retractall(macro(A,C,D,E,F,G)),
        conjlist(B,H,[]),
        remove_double_indirect(A,I,J,H),
        varset(J,K),
        difflist(J,L,M),
        macro_varlist(J,N,O),
        asserta(macro(I,L,M,N,O,K)).
add_macro_def(macro(A)).

handle_dir(vlsi_bam,[]) :-
        vlsi_bam.
handle_dir(mips,[]) :-
        mips_options.
handle_dir(sparc,[]) :-
        sparc_options.
handle_dir(mc68020,[]) :-
        mc68020_options.
handle_dir(version,[]) :-
        print_version.
handle_dir(help,[]) :-
        print_help.
handle_dir(helpoptions,[]) :-
        print_helpoptions.
handle_dir(options,[]) :-
        po.
handle_dir(option,A) :-
        cox_list(A).
handle_dir(notoption,A) :-
        nox_list(A).
handle_dir(include,[A]) :-
        atom(A),
        seeing(B),
        push_incl_stack(B),
        see(A).
handle_dir(op,[A,B,C]) :-
        op(A,B,C).
handle_dir(prolog_flag,[A,B,C]) :-
        (   A=character_escapes ->
            copy([B,C],[D,E]),
            prolog_flag(A,D,E)
        ;   true
        ).
handle_dir((dynamic),[A]) :-
        check_spec_list(A),
        dynamic_list(A).
handle_dir((init_directive),[A]) :-
        check_spec_list(A),
        init_dir_list(A).
handle_dir(o,A) :-
        cox_list(A).
handle_dir(no,A) :-
        nox_list(A).
handle_dir(ex,[A]) :-
        ground(A),
        ex(A).

vlsi_bam :-
        compile_option(system(cprolog)),
        !,
        cdefault.
vlsi_bam :-
        compile_option(system(quintus)),
        !,
        qdefault.
vlsi_bam :-
        compile_option(system(aquarius)),
        !,
        adefault.

mips_options :-
        comment(['Compiling for the MIPS processor']),
        vlsi_bam,
        cox_list([mips,arithmetic_error_check,align(1)]),
        nox_list([split_integer]).

sparc_options :-
        comment(['Compiling for the SPARC processor']),
        vlsi_bam,
        cox_list([sparc,arithmetic_error_check,align(2)]),
        nox_list([split_integer]).

mc68020_options :-
        comment(['Compiling for the MC68020 processor']),
        vlsi_bam,
        cox_list([mc68020,arithmetic_error_check,align(1)]),
        nox_list([split_integer]).

print_help :-
        nl,
        wn('% List of useful directives:'),
        wn('% option(OptList)        Enable the listed options.'),
        wn('% notoption(OptList)     Disable the listed options.'),
        wn('% options                Print the current options.'),
        wn('% helpoptions            Give information about the possible options.'),
        wn('% op(A,B,C)              Operator declaration.'),
        wn('% prolog_flag(F,Old,New) Modify execution parameters of the system.'),
        wn('% dynamic(Name/Arity)    Declare a dynamic predicate.'),
        wn('% mode((Head:-Formula))  Mode information for a predicate.'),
        wn('% entry((Head:-Formula)) As above and also used for flow analysis.'),
        wn('% mode(H,R,B,A,S)        More detailed mode declaration.'),
        wn('% entry(H,R,B,A,S)       More detailed entry declaration.'),
        wn('% macro((Head:-BAMCode)) Define a predicate as BAM assembly code.'),
        wn('% include(F)             Insert the contents of file F.'),
        wn('% version                Print the creation date of this version.'),
        wn('% Directives normally only used by the system:'),
        wn('% mips                   Set options for the MIPS.'),
        wn('% sparc                  Set options for the SPARC.'),
        wn('% mc68020                Set options for the MC68020.'),
        wn('% vlsi_bam               Set options for the VLSI-BAM.'),
        wn('% modal_entry(H,Tree)    Discrimination tree for efficient builtins.'),
        nl.

print_helpoptions :-
        nl,
        wn('% List of useful options (default in parentheses):'),
        wn('% float (off)   Enable use of floating point.'),
        wn('% analyze (off) Perform flow analysis on the input program.'),
        wn('% factor (on)   Perform factoring transformation.'),
        wn('% compile (on)  Compile the input program.'),
        wn('% arithmetic_error_check (on)'),
        wn('%               Perform type checking on arithmetic operations.'),
        wn('% same_number_solutions (on)'),
        wn('%               Keep the same number of solutions as standard Prolog.'),
        wn('% same_order_solutions (on)'),
        wn('%               Keep the same order of solutions as standard Prolog.'),
        nl.

po :-
        pox(A),
        write('% Options = '),
        write(A),
        nl,
        pmodes,
        !.

cox_list([]) :- !.
cox_list([A|B]) :- !,
        cox_list(A),
        cox_list(B).
cox_list(A) :-
        \+list(A),
        !,
        cox(A).

nox_list([]) :- !.
nox_list([A|B]) :- !,
        nox_list(A),
        nox_list(B).
nox_list(A) :-
        \+list(A),
        !,
        nox(A).

push_incl_stack(A) :-
        include_stack(B),
        my_retractall(include_stack(C)),
        D=[A|B],
        asserta(include_stack(D)),
        (   length(D,E),
            compile_option(include_limit(F)),
            E>F ->
            warning(['The inclusion stack ',D,nl,'has exceeded the limit of ',F,' nested includes.'])
        ;   true
        ).

copy(A,B) :-
        copyB(A,B).

check_spec_list((A ',' B)) :-
        check_spec_list(A),
        check_spec_list(B).
check_spec_list(A/B) :-
        atom(A),
        integer(B),
        B>=0.

dynamic_list((A ',' B)) :-
        dynamic_list(A),
        dynamic_list(B).
dynamic_list(A/B) :-
        functor(C,A,B),
        assert(dyn_pred(C)).

init_dir_list((A ',' B)) :-
        init_dir_list(A),
        init_dir_list(B).
init_dir_list(A/B) :-
        functor(C,A,B),
        assert((init_directive C)).

% PVR -- already a built-in.
% ground(A) :-
%         nonvar(A),
%         functor(A,B,C),
%         ground(C,A).

ex(A) :-
        ex(A,B,C),
        !,
        comp(B,C).

after_collect(options,[]).
after_collect(option,A) :-
        \+do_it_now(A).
after_collect(o,A) :-
        \+do_it_now(A).
after_collect(notoption,A) :-
        \+do_it_now(A).
after_collect(no,A) :-
        \+do_it_now(A).

do_it_now(A) :-
        now_list(B),
        member(C,B),
        member(C,A).
do_it_now([A]) :-
        now_list(B),
        member(C,B),
        member(C,A).

now_list([arithmetic_error_check,stats(A),analyze,analyze_uninit_reg,compile,comment,uni,float,factor,test,test_unify,test_arith,test_typecheck,firstarg]).

member(A,[A|B]).
member(A,[B|C]) :-
        member(A,C).

comment(A) :-
	true.
%         compile_option(comment),
%         !,
%         make_msg(A,B),
%         cm,
%         msg_list(B,cm).
% comment(A) :-
%         \+compile_option(comment),
%         !.

print_copyright :-
        copyright(A,B),
        w('% '),
        wn(B),
        fail.
print_copyright.

print_date :-
        compiler_version(A),
        !,
        w('% Creation date '),
        wn(A).
print_date.

my_retractall(A) :-
        copy(A,B),
        retract(B),
        !,
        my_retractall(A).
my_retractall(A).

cf(A,B) :-
        cf(A,B,[]).

cf(A,B,C) :-
        tell(A),
        comp(B,C),
        !,
        told.
cf(A,B,C) :-
        told.

comp(A,B) :-
        make_list(B,C),
        comp(A,C,D),
        write_source(A,C),
        write_code(D).

comp(A) :-
        comp(A,[]).

make_list(A,[A]) :-
        \+list(A),
        !.
make_list(A,A) :-
        list(A),
        !.

comp(A,B,C) :-
        clr_mode,
        add_mode_options(B),
        internal_comp(A,C).

write_source(A,B) :-
        \+compile_option(source),
        !.
write_source(A,B) :-
        compile_option(source),
        inst_vars(B,C),
        xwrite_modes(C),
        inst_vars(A,D),
        xwrite_source(D).

write_code([]) :- !.
write_code(A) :-
        \+compile_option(write),
        !.
write_code(A) :-
        compile_option(write),
        \+compile_option(flat),
        !,
        write_code(A,4).
write_code(A) :-
        compile_option(write),
        compile_option(flat),
        !,
        write_output(A).

clr_mode :-
        my_retractall(mode_option(A,B,C,D,E)).

add_mode_options([]) :- !.
add_mode_options([(A:-B)|C]) :- !,
        add_mode_option((mode (A:-B))),
        add_mode_options(C).
add_mode_options([true|A]) :- !,
        add_mode_options(A).
add_mode_options([A|B]) :-
        add_mode_option(A),
        add_mode_options(B).

internal_comp(A,B) :-
        internal_comp(nowrite,A,B).

ex(pa,[p(a)],(p(A):-uninit(A))).
ex(1,[(a(A):-A<1 ',' b(A,A))],true).
ex(2,[(a(A):-A@<[B]',' b(A))],true).
ex(up,[(p([A|B],C,[A|D],E):-C>=A ',' p(B,C,D,E)),(p([A|B],C,D,[A|E]):-C<A ',' p(B,C,D,E)),p([],F,[],[])],(p(G,H,I,J):-list(G)',' uninit(I)',' uninit(J))).
ex(dis,[(a(A):-var(A)',' (nonvar(A);b(A)))],true).
ex(aba,[a(a),a(b),a(a)],true).
ex(ab,[a(a),a(b)],true).
ex(h1,[a(a),a(b),a(c),a(d),a(1),a(2)],(a(A):-nonvar(A))).
ex(h2,[(a(s(A)):-s(A)),(a(t(A)):-t(A))],(a(B):-nonvar(B))).
ex(nrev,[(nreverse([A|B],C):-nreverse(B,D)',' concatenate(D,[A],C)),nreverse([],[])],[(nreverse(E,F):-list(E)',' uninit(F)),(concatenate(E,F,G):-list(E)',' list(F)',' uninit(G))]).
ex(ncon,[(concatenate([A|B],C,[A|D]):-concatenate(B,C,D)),concatenate([],E,E)],(concatenate(F,G,H):-list(F)',' list(G)',' uninit(H))).

compex(A,B) :-
        tell(B),
        compex(A),
        told.

compex([]).
compex([A|B]) :-
        ex(A),
        fail.
compex([A|B]) :-
        compex(B).

internal_comp(A,B,C) :-
        % init_stats,
        cont_cls(B,D),
        cls_to_ptrees(D,E),
        dynamic_ptrees(E,F,G,[]),
        dynamic_nodef(G),
        init_ptrees(E,F,H),
        comp_steps(A,H,C).

init_stats :-
        abolish(compile_cputime,3),
        get_cputime(A),
        !,
        assert(compile_cputime(start,none,A)).
init_stats.

cont_cls(A,A) :-
        compile_option(contiguous),
        !.
cont_cls(A,B) :-
        % stats(cc,1),
        keylist_cls(A,C),
        % stats(cc,2),
        keysort(C,D),
        % stats(cc,3),
        merge_cls(D,B).
        % stats(cc,4).

cls_to_ptrees(A,B) :-
        expand_clauses(A,C),
        translate_clauses(C,D),
        cls_to_ptrees_loop(D,B),
        add_modes(B),
        !.

dynamic_ptrees([A|B],C,D,E) :-
        dynamic_ptree(A,F),
        !,
        constC(D,F,G),
        dynamic_ptrees(B,C,G,E).
dynamic_ptrees([A|B],[A|C],D,E) :-
        dynamic_ptrees(B,C,D,E).
dynamic_ptrees([],[],A,A).

dynamic_nodef(A) :-
        dyn_pred(B),
        functor(B,C,D),
        \+member(C/D,A),
        add_init('$ declare_dynamic'(C/D)),
        fail.
dynamic_nodef(A).

init_ptrees(A,B,C) :-
        init_ptree(A,D),
        !,
        append(B,D,C).

comp_steps(A,B,C) :-
        % stats(t,1),
        transform_cut_ptrees(B,D),
        % stats(t,2),
        factor_ptrees(D,E),
        % stats(t,3),
        ptrees_to_strees(E,F),
        % stats(t,4),
        flatten_strees(F,G),
        % stats(t,5),
        inline_strees(G,H),
        % stats(t,6),
        analyze_strees(H,I),
        % stats(t,7),
        compile_strees(A,I,C),
        % done_stats,
        !.

stats(A,B) :-
        get_cputime(C),
        compile_option(stats(D)),
        (   member(A,D)
        ;   D=A
        ),
        !,
        retract(compile_cputime(E,F,G)),
        w('% Cputime between '),
        w(E),
        write_num(F),
        w(' and '),
        w(A),
        write_num(B),
        w(' is '),
        H is C-G,
        wn(H),
        ttyflush_quintus,
        get_cputime(I),
        assert(compile_cputime(A,B,I)),
        !.
stats(A,B).

transform_cut_ptrees([],[]).
transform_cut_ptrees([A|B],[C|D]) :-
        transform_cut_ptree(A,C),
        transform_cut_ptrees(B,D).

factor_ptrees(A,A) :-
        fail, % \+compile_option(factor),
        !.
factor_ptrees([],[]) :-
        true, % compile_option(factor),
        !.
factor_ptrees([A|B],[A|C]) :-
        true, % compile_option(factor),
        directive(A),
        !,
        factor_ptrees(B,C).
factor_ptrees([A|B],[C|D]) :-
        true, % compile_option(factor),
        !,
        factor_dlist(A,E),
        get_factor_args(A,F),
        factor_args_ptree(F,E,C),
        factor_ptrees(B,D).

ptrees_to_strees([],[]).
ptrees_to_strees([A|B],[C|D]) :-
        ptree_to_stree(A,C),
        ptrees_to_strees(B,D).

flatten_strees([],[]).
flatten_strees([A|B],[C|D]) :-
        flatten_stree(A,C),
        flatten_strees(B,D).

inline_strees(A,B) :-
        fail, % compile_option(inline),
        !,
        gather_single(A,C),
        inline_replace_strees(A,B,C).
inline_strees(A,A) :-
        true, % \+compile_option(inline),
        !.

analyze_strees(A,B) :-
        compile_option(analyze),
        !,
        comment(['Starting dataflow analysis']),
        analyze(A,B).
analyze_strees(A,A).

compile_strees(A,B,C) :-
        compile_strees(B,peep,return,header,A,C,[]).

done_stats :-
        get_cputime(A),
        !,
        retract(compile_cputime(B,C,D)),
        nl,
        w('% Cputime between '),
        w(B),
        write_num(C),
        w(' and finish is '),
        E is A-D,
        wn(E),
        ttyflush_quintus.
done_stats.

compile_strees([],A,B,C,D,E,E) :- !.
compile_strees([A|B],C,D,E,F,G,H) :-
        A=stree(I,J,K,L,M,N),
        !,
        compile_comment(I),
        print_stree(A),
        init_gensym,
        compile_stree(A,C,D,E,F,G,O),
        compile_strees(B,C,D,E,F,O,H).
compile_strees([(:-A)|B],C,D,E,F,G,H) :- !,
        handle_dir(A),
        compile_strees(B,C,D,E,F,G,H).

compile_comment(A) :-
        compile_option(compile),
        !,
        nl,
        comment(['Compiling ',A,' ...']).
compile_comment(A).

print_stree(A) :-
        compile_option(print_stree),
        !,
        write('% '),
        q_inst_writeq(A),
        wn('.').
print_stree(A).

init_gensym :-
        abolish(gensym_integer,1),
        asserta(gensym_integer(1)).

compile_stree(A,B,C,D,E,F,G) :-
        compile_stree(A,B,C,D,H),
        lbl_sets(H,I),
        map_external_lbls(H,I,J,K),
        (   E=write ->
            K=[],
            write_code(J),
            F=G
        ;   F=J,
            G=K
        ).

q_inst_writeq(A) :-
        compile_option(system(quintus)),
        !,
        copy(A,B),
        numbervars(B,0,C),
        writeq(B).
q_inst_writeq(A) :-
        compile_option(system(aquarius)),
        !,
        copy(A,B),
        numbervars(B,0,C),
        writeq(B).
q_inst_writeq(A) :-
        inst_writeq(A).

compile_stree(A,B,C,D,E) :-
        compile_option(compile),
        builtin_recompile_error(A),
        !,
        calc_select_depth(A),
        % stats(c,1),
        trim_stree(A,F),
        % stats(c,2),
        select_stree(F,G,H),
        G=stree(I,(J:-K),L,M,N,O),
        % stats(c,3),
        segment_disj(J,K,P,L,Q,H),
        % stats(c,4),
        selection_disj(J,P,L,R),
        % stats(c,5),
        proc_header(D,I,S,T),
        proc_code(J,R,L,N,C,T,U),
        % stats(c,6),
        clause_code_list(Q,N,U,V),
        end_label(C,V,[]),
        % stats(c,7),
        peephole(I,S,E,B),
        % stats(c,8),
        !.
compile_stree(A,B,C,D,[]).

lbl_sets(A,B) :-
        lbl_sets(A,B,C).

map_external_lbls(A,B,C,D) :-
        map_external_lbls(A,B,E,C,D).

compile_stree(A,B,C,D,E,F) :-
        compile_stree(A,B,C,D,G),
        difflist(G,E,F).

difflist([],A,A).
difflist([A|B],C,D) :-
        constC(C,A,E),
        difflist(B,E,D).

builtin_recompile_error(stree(A/B,C,D,E,F,G)) :-
        compile_option(protect_builtins),
        functor(H,A,B),
        builtin(H),
        !,
        warning(['The predicate ',A/B,' is a builtin.  It is not compiled.']),
        fail.
builtin_recompile_error(A).

calc_select_depth(stree(A,B,C,D,E,F)) :-
        var(F),
        !,
        select_limit(F).
calc_select_depth(stree(A,B,C,D,E,F)) :-
        nonvar(F).

trim_stree(stree(A,B,C,D,E,F),stree(A,B,G,D,E,F)) :-
        trim_mode(C,G).

select_stree(A,B,C) :-
        A=stree(D/E,(F:-G),(F:-H),I,J,K),
        K>0,
        E>0,
        length_disj(G,L),
        L>1,
        segment_all_disj(G,F,H,M,C,N),
        not_nonvar(N,H,O,P),
        select_limit(Q),
        (   P<Q
        ;   arg(1,F,R),
            inv(R,O)
        ),
        range_option(1,S,E),
        arg(S,F,T),
        memberv(T,O),
        !,
        comment(['Doing selection on argument ',S,' of ',D/E]),
        asserta(select_option(F,S)),
        subsume(var(T),M,U),
        standard_disj(U,V),
        subsume(nonvar(T),M,W),
        standard_disj(W,X),
        new_head([36,118,95],F,[],Y),
        functor(Y,Z,A1),
        new_head([36,110,118,95],F,[],B1),
        functor(B1,C1,D1),
        E1 is K-1,
        after(var(T),F1),
        after(nonvar(T),G1),
        combine_formula(F1,H,H1),
        combine_formula(G1,H,I1),
        J1=stree(Z/A1,(Y:-V),(Y:-H1),[],J,E1),
        K1=stree(C1/D1,(B1:-X),(B1:-I1),[],J,E1),
        add_mode_option(F,Y,true,F1,yes),
        add_mode_option(F,B1,true,G1,yes),
        standard_disj((var(T)',' Y;nonvar(T)',' B1),L1),
        B=stree(D/E,(F:-L1),(F:-H),[],[J1,K1|J],K),
        !.
select_stree(A,A,[]).

segment_disj(A,B,C,D,E,F) :-
        D= (A:-G),
        varset(A,H),
        create_bodies(B,I,J,K,L,H,G,E,F),
        create_heads(J,A,H,K,L),
        standard_disj(I,C).

selection_disj(A,B,C,D) :-
        copy(C,(A:-E)),
        varset(A,F),
        subsume(E,B,G),
        selection_3(G,A,[],F,E,D),
        !.

proc_header(noheader,A,B,B) :- !.
proc_header(header,A,B,C) :-
        constC(B,procedure(A),C).

proc_code(A,B,C,D,E,F,G) :-
        copy(C,(A:-H)),
        code(A,H,D,E,B,F,G),
        !.

clause_code_list([],A,B,B) :- !.
clause_code_list([body((A:-B),C)|D],E,F,G) :-
        nonvar(C),
        !,
        entry_comment(A,B,C),
        split_uninit_list(C,H,I,J,K),
        intersectv_list(H,L),
        intersectv_list(I,M),
        functor(A,N,O),
        insert_uninit(H,A,L,C,N/O,F,P),
        intersect_formula_list(K,Q),
        uninit_origins(J,R),
        make_uninit_mem_formula(L,R,S,Q),
        make_uninit_reg_formula(M,T,S),
        constC(P,label(N/O),U),
        clause_code((A:-B),E,return,T,U,V),
        clause_code_list(D,E,V,G).
clause_code_list([body((A:-B),C)|D],E,F,G) :-
        var(C),
        !,
        clause_code_list(D,E,F,G).

end_label(return,A,B) :-
        constC(A,return,B).
end_label(jump(A),B,C) :-
        constC(B,label(A),C).

peephole(A,B,B,nopeep) :- !.
peephole(A,B,C,peep) :-
        \+compile_option(peep),
        !,
        peep_flat(B,D),
        inst_labl(A,D,C).
peephole(A,B,C,peep) :-
        compile_option(peep),
        !,
        write_debug('Before peephole optimization:'),
        nl_debug,
        xpeephole(A,B,C),
        (   fail -> % compile_option(debug) ->
            write_code(B),
            nl
        ;   true
        ),
        write_debug('After peephole optimization:').

builtin(A) :-
        info(A,B,C,y,D),
        !.

trim_mode((A:-B),(A:-C)) :-
        keep_uninit(A,B,C).

length_disj(A,B) :-
        length_disj(A,0,B).

segment_all_disj(A,B,C,D,E,F) :-
        varset(B,G),
        unbound_set(C,H),
        segment_all_disj(A,B,G,H,D,I,[],E),
        sort(I,J),
        intersectv(J,G,F).

not_nonvar(A,B,C,D) :-
        not_nonvar(A,B,C,0,D).

inv(A,[B|C]) :-
        compare(D,A,B),
        inv_2(D,A,C).

range_option(A,B,C) :-
        fail, % compile_option(firstarg),
        !,
        B=1.
range_option(A,B,C) :-
        range(A,B,C).

memberv(A,[B|C]) :-
        (   A==B ->
            true
        ;   memberv(A,C)
        ).

subsume(true,A,B) :- !,
        simplify(A,B).
subsume(A,B,C) :-
        sub(A,B,D,prolog,E),
        simplify(D,C).

standard_disj(A,B) :-
        disj(A,B,fail).

new_head(A,B,C,D) :-
        functor(B,E,F),
        B=..[E|G],
        append(G,C,H),
        gensym(A,E/F,I),
        D=..[I|H].

after(A,B) :-
        info(A,C,D,E,F,G,H,I),
        !,
        logical_simplify(I,B).
after(A,B) :-
        \+info(A),
        test(A),
        !,
        logical_simplify(A,B).
after(A,true) :-
        \+info(A),
        \+test(A),
        !.

combine_formula(A,B,C) :-
        unionv_conj(A,B,C).

add_mode_option(A,B,C,D,E) :-
        mode_option(A,F,G,H,I),
        !,
        split_deref(F,J,K),
        split_deref(C,L,M),
        flat_conj((M ',' K),N),
        flat_conj((L ',' J ',' D ',' G),O),
        make_after(E,H,P),
        add_mode_option(mode(B,N,O,P,I)).
add_mode_option(A,B,C,D,E) :-
        split_deref(C,F,G),
        flat_conj((F ',' D),H),
        add_mode_option(mode(B,G,H,true,n)).

not_nonvar([],A,[],B,B).
not_nonvar([A|B],C,D,E,F) :-
        implies(C,nonvar(A)),
        !,
        G is E+1,
        not_nonvar(B,C,D,G,F).
not_nonvar([A|B],C,[A|D],E,F) :-
        not_nonvar(B,C,D,E,F).

implies(A,B) :-
        logical_simplify(not(B),C),
        mutex(A,C,left),
        !.

range(A,A,B).
range(A,B,C) :-
        A<C,
        D is A+1,
        range(D,B,C).

check_modal_entry(A,B) :-
        nonvar(A),
        nonvar(B),
        check_tree_entry(B),
        !.
check_modal_entry(A,B) :-
        error(['Modal entry ',A,' has incorrect syntax.']),
        fail.

clr_modal_entry :-
        my_retractall(modal_entry(A,B)).

check_tree_entry(entry(A)) :-
        nonvar(A).
check_tree_entry(mode(A,B,C)) :-
        nonvar(A),
        nonvar(B),
        nonvar(C),
        check_tree_entry(B),
        check_tree_entry(C).

error(A) :-
        make_msg(A,B),
        e,
        msg_list(B,e).

nox(A) :-
        nox(A,B).

translate(A,call(A)) :-
        var(A),
        !.
translate(A^B,C) :- !,
        translate(B,C).
translate(c(A,B,C),A=[B|C]) :-
        compile_option(system(cprolog)),
        !.
translate(constC(A,B,C),A=[B|C]) :-
        compile_option(system(quintus)),
        !.
translate(constC(A,B,C),A=[B|C]) :-
        compile_option(system(aquarius)),
        !.
translate(A is B,C) :-
        number(A),
        !,
        top_expr(B,D,C,D=:=A).
translate(A is B,C) :-
        var(A),
        !,
        top_expr(B,D,C,D=A).
translate(\+A is B,C) :-
        number(A),
        !,
        top_expr(B,D,C,D=\=A).
translate(put(A),B) :- !,
        expr(A,C,B,put(C)).
translate(A,B) :-
        arith_test(A,C,D,E),
        !,
        expr(C,F,B,G),
        expr(D,H,G,I),
        arith_test(I,F,H,E).
translate(A=B,C) :- !,
        translate_unify(A,B,C).
translate((A ',' B),(C ',' D)) :- !,
        translate(A,C),
        translate(B,D).
translate((A;B),(C;D)) :- !,
        translate(A,C),
        translate(B,D).
translate((A->B),(C->D)) :- !,
        translate(A,C),
        translate(B,D).
translate(not(A),not(B)) :- !,
        translate(A,B).
translate(\+A,\+B) :- !,
        translate(A,B).
translate(false,fail) :- !.
translate(otherwise,true) :- !.
translate(A,'$ call_dynamic'(A)) :-
        dyn_pred(A),
        !.
translate(A,A).

flat_conj(A,B) :-
        flat_conj(A,B,true).

logical_simplify(A,B) :-
        simp_upv(A,B,logical),
        !.

nonnegative(A) :-
        integer(A),
        A>=0.

clr_entry :-
        my_retractall(compile_option(entry(A,B))).

check_non_builtin(A) :-
        nonvar(A),
        functor(A,B,C),
        check_nb(A,['Attempt to give modes for builtin ',B/C,' is ignored.']).

make_req_bef(A,B,C) :-
        translate(A,D),
        flat_conj(D,E),
        split_uninit_deref(E,F,G),
        logical_simplify(F,B),
        logical_simplify(G,C).

keep_uninit(A,B,C) :-
        varbag(A,D),
        keep_uninit(D,B,E,true),
        squeeze_conj(E,C).

survive(A,y) :-
        survive(A),
        !.
survive(A,n) :-
        \+survive(A),
        !.

split_deref((A ',' B),(C ',' D),(E ',' F)) :- !,
        split_deref(A,C,E),
        split_deref(B,D,F).
split_deref(deref(A),deref(A),true) :- !.
split_deref(rderef(A),rderef(A),true) :- !.
split_deref(A,true,A).

check_req_bef(A,B,C,D,E) :-
        split_uninit_deref(A,C,F),
        squeeze_conj(F,G),
        warning_req(G,E),
        squeeze_conj((G ',' B),D).

split_uninit_deref((A ',' B),(C ',' D),(E ',' F)) :- !,
        split_uninit_deref(A,C,E),
        split_uninit_deref(B,D,F).
split_uninit_deref(deref(A),deref(A),true) :- !.
split_uninit_deref(A,A,true) :-
        an_uninit_mode(A),
        !.
split_uninit_deref(A,true,A).

squeeze_conj((A ',' B),C) :-
        \+all_true(B),
        !,
        squeeze_conj(B,D),
        flat_conj(A,C,D).
squeeze_conj((A ',' B),C) :-
        \+all_true(A),
        !,
        squeeze_conj(A,D),
        flat_conj(B,C,D).
squeeze_conj(A,true) :-
        all_true(A),
        !.
squeeze_conj(A,A).

warning_req(true,A) :- !.
warning_req(A,B) :-
        functor(B,C,D),
        warning(['Illegal Require modes converted to Before in ',C/D,'.',nl,'Require modes can only be uninit(X), uninit_reg(X) or deref(X).']).

make_after(yes,A,A).
make_after(no,A,true).

check_nb(A,B) :-
        builtin(A),
        !,
        warning(B),
        fail.
check_nb(A,B).

o(A) :-
        nonvar(A),
        cox(A,B),
        po,
        !.

cox(A,true) :-
        atomic(A),
        compile_option(A),
        !.
cox(A,B) :-
        functor(A,C,D),
        functor(E,C,D),
        nox(E,B),
        !,
        ox(A,F).
cox(A,B) :-
        ox(A).

no(A) :-
        nonvar(A),
        nox(A,B),
        po,
        !.

nox(A,true) :-
        copy(A,B),
        compile_option(B),
        !,
        pragma_no_write_once(A),
        my_retractall(compile_option(A)).
nox(A,false) :-
        \+compile_option(A).

co(A) :-
        nonvar(A),
        cox(A,true),
        po,
        !.

pox(A) :-
        setof(B,compile_option(B),A).

pmodes :-
        mode_option(A,B,C,D,E),
        w('% '),
        inst_writeq(mode(A,B,C,D,E)),
        nl,
        fail.
pmodes :-
        modal_entry(A,B),
        w('% '),
        inst_writeq(modal_entry(A,B)),
        nl,
        fail.
pmodes.

inst_writeq(A) :-
        copy(A,B),
        varset(B,C),
        inst_vars_names_list(0,C,D),
        inst_writeq(B,C,D).

ox(A) :-
        ox(A,B).

ox(A,true) :-
        compile_option(A),
        !.
ox(A,false) :-
        \+compile_option(A),
        pragma_write_once(A),
        asserta(compile_option(A)).

list(A) :-
        (   nil(A)
        ;   cons(A)
        ).

cox(A) :-
        cox(A,true).

cdefault :-
        qdefault,
        cox(system(cprolog)).

qdefault :-
        my_retractall(compile_option(A)),
        my_retractall(mode_option(B,C,D,E,F)),
        my_retractall(modal_entry(G,H)),
        default_option(I),
        asserta(compile_option(I)),
        fail.
qdefault.

adefault :-
        qdefault,
        cox(system(aquarius)).

interactive_default(cprolog) :-
        cdefault,
        nox(flat),
        po.
interactive_default(quintus) :-
        qdefault,
        nox(flat),
        po.
interactive_default(aquarius) :-
        adefault,
        nox(flat),
        po.

check_macro_def(A) :-
        nonvar(A),
        functor(A,B,C),
        check_nb(A,['Attempt to give macro definition for builtin ',B/C,' is ignored.']).

conjlist((A ',' B),C,D) :- !,
        conjlist(A,C,E),
        conjlist(B,E,D).
conjlist(A,B,C) :-
        constC(B,A,C).

remove_double_indirect(A,B,C,D) :-
        functor(A,E,F),
        functor(B,E,F),
        require(A,G),
        uninit_set_type(reg,G,H),
        match_heads(1,F,A,B,H,C,D).

varset(A,B) :-
        varbag(A,C),
        sort(C,B).

macro_varlist([move(A,B)|C],D,E) :-
        var(A),
        var(B),
        !,
        constC(D,pref,F),
        constC(F,A,G),
        constC(G,B,H),
        macro_varlist(C,H,E).
macro_varlist([label(A)|B],C,D) :- !,
        macro_varlist(B,C,D).
macro_varlist([A|B],C,D) :-
        branch(A,E),
        !,
        varbag(A,F),
        diffbag(F,E,G),
        difflist(G,C,H),
        macro_varlist(B,H,D).
macro_varlist([A|B],C,D) :-
        varbag(A,C,E),
        macro_varlist(B,E,D).
macro_varlist([],A,A).

require(A,B) :-
        info(A,C,D,E,F,G,H,I),
        !,
        logical_simplify(H,B).
require(A,true) :-
        \+info(A),
        !.

uninit_set_type(A,B,C) :-
        uninit_bag_type(A,B,D),
        sort(D,C).

match_heads(A,B,C,D,E,F,F) :-
        A>B,
        !.
match_heads(A,B,C,D,E,F,G) :-
        A=<B,
        arg(A,C,H),
        inv(H,E),
        !,
        arg(A,D,H),
        I is A+1,
        match_heads(I,B,C,D,E,F,G).
match_heads(A,B,C,D,E,F,G) :-
        A=<B,
        arg(A,D,H),
        arg(A,C,I),
        constC(F,move(H,I),J),
        K is A+1,
        match_heads(K,B,C,D,E,J,G).

branch(A,B) :-
        branch(A,B,[]).

varbag(A,B) :-
        varbag(A,B,[]).

diffbag([],A,[]).
diffbag([A|B],C,D) :-
        memberv(A,C),
        !,
        diffbag(B,C,D).
diffbag([A|B],C,[A|D]) :-
        diffbag(B,C,D).

varbag(A,B,C) :-
        var(A),
        !,
        constC(B,A,C).
varbag(A,B,C) :-
        nonvar(A),
        !,
        functor(A,D,E),
        varbag(A,1,E,B,C).

lbl_sets([procedure(A)|B],C,D) :- !,
        seal(D),
        get(C,A,E),
        get(E,A,dummy),
        lbl_sets(B,C,E).
lbl_sets([label(A)|B],C,D) :-
        A=E/F,
        !,
        get(D,A,dummy),
        lbl_sets(B,C,D).
lbl_sets([A|B],C,D) :-
        lbl_sets(B,C,D).
lbl_sets([],A,B) :-
        seal(A),
        seal(B).

seal(leaf) :- !.
seal(node(A,B,C,D)) :-
        seal(C),
        seal(D).

get(node(A,B,C,D),E,F) :-
        get(A,B,C,D,E,F).

map_external_lbls([procedure(A)|B],C,D,E,F) :- !,
        fget(C,A,G),
        constC(E,procedure(A),H),
        map_external_lbls(B,C,G,H,F).
map_external_lbls([A|B],C,D,E,F) :-
        map_external_lbls_i(A,G,D),
        constC(E,G,H),
        map_external_lbls(B,C,D,H,F).
map_external_lbls([],A,B,C,C).

fget(node(A,B,C,D),E,F) :-
        compare(G,E,A),
        fget_2(G,E,F,B,C,D).

map_external_lbls_i(A,B,C) :-
        map_branches(A,D,B,E),
        !,
        map_the_lbls(D,E,C).
map_external_lbls_i(A,A,B).

map_branches(jump(A),[A],jump(B),[B]).
map_branches(jump(A,B,C,D),[D],jump(A,B,C,E),[E]).
map_branches(jump_nt(A,B,C,D),[D],jump_nt(A,B,C,E),[E]).
map_branches(call(A),[A],call(B),[B]).
map_branches(test(A,B,C,D),[D],test(A,B,C,E),[E]).
map_branches(equal(A,B,C),[C],equal(A,B,D),[D]).
map_branches(unify(A,B,C,D,E),[E],unify(A,B,C,D,F),[F]).
map_branches(unify_atomic(A,B,C),[C],unify_atomic(A,B,D),[D]).
map_branches(choice(A,B,C),[C],choice(A,B,D),[D]).
map_branches(hash(A,B,C,D),[D],hash(A,B,C,E),[E]).
map_branches(pair(A,B),[B],pair(A,C),[C]).
map_branches(switch(A,B,C,D,E),[C,D,E],switch(A,B,F,G,H),[F,G,H]).

map_the_lbls([A|B],[C|D],E) :-
        A=F/G,
        \+fget(E,A,H),
        !,
        C=e(A),
        map_the_lbls(B,D,E).
map_the_lbls([A|B],[A|C],D) :-
        map_the_lbls(B,C,D).
map_the_lbls([],[],A).

init_directives((A ',' B)) :- !,
        init_directives(A),
        init_directives(B).
init_directives(A) :-
        init_directive A.

add_init(A) :-
        assertz(init_clause(A)).

dynamic_ptree(ptree(A,B,C,D),A) :-
        A=E/F,
        functor(G,E,F),
        dyn_pred(G),
        !,
        add_pred(B).

add_pred([]).
add_pred([A|B]) :-
        add_init('$ assertz'(A)),
        add_pred(B).

init_ptree(A,B) :-
        (   bagof(C,init_clause(C),D) ->
            true
        ;   D=[]
        ),
        (   first_name(A,E) ->
            genatom([36,32,105,110,105,116,95],E,F)
        ;   F='$ init'
        ),
        list_to_conj(D,G),
        cls_to_ptrees([(F:-G)],B).

append(A,B,C) :-
        difflist(A,C,B).

first_name([ptree(A,B,C,D)|E],A) :- !.
first_name([A|B],C) :-
        first_name(B,C).

genatom(A,B/C,D) :-
        atomic(B),
        atomic(C),
        !,
        name(B,E),
        name(C,F),
        append(A,E,[47],F,G),
        name(D,G).

list_to_conj([],true).
list_to_conj([A|B],C) :-
        flat_conj(A,C,D),
        list_to_conj(B,D).

list_ex :-
        list_ex(A),
        nl,
        fail.
list_ex.

list_ex(A) :-
        ex(A,B,C),
        w('% Example '),
        w(A),
        wn(:),
        inst_writeqs(B),
        (   \+C=true ->
            wn('% Modes:'),
            inst_writeqs(C)
        ;   true
        ).

inst_writeqs(A) :-
        cons(A),
        !,
        inst_writeq_list(A).
inst_writeqs(A) :-
        \+cons(A),
        !,
        inst_writeq(A),
        wn('.').

gensym_integer(1).

max_int(32767).

min_int(-32768).

directive((:-A)).

stree(stree(A,B,C,D,E,F)).

find_arg(A,B,C) :-
        functor(B,D,E),
        find_arg(1,E,A,B,C).

find_arg(A,B,C,D,E) :-
        A=<B,
        arg(A,D,F),
        F==E,
        !,
        C=A.
find_arg(A,B,C,D,E) :-
        A=<B,
        arg(A,D,F),
        F\==E,
        !,
        G is A+1,
        find_arg(G,B,C,D,E).

outfilename(A,B,C) :-
        name(A,D),
        (   append(E,[46],F,D) ->
            append(E,[46],B,G)
        ;   append(D,[46],B,G)
        ),
        name(C,G).

append([],A,B,C) :-
        append(A,B,C).
append([A|B],C,D,[A|E]) :-
        append(B,C,D,E).

newfilename(A,B,C) :-
        name(A,D),
        append(D,B,E),
        name(C,E).

grounds_in_form(A,B) :-
        ground_set(A,C),
        vars_in_unify(C,A,B).

ground_set(A,B) :-
        ground_bag(A,C,[]),
        sort(C,D),
        filter_vars(D,B).

vars_in_unify(A,B,C) :-
        make_graph((D=A ',' B),E,[]),
        warshall(E,F),
        member_w(D-C,F).

make_graph((A ',' B),C,D) :- !,
        make_graph(A,C,E),
        make_graph(B,E,D).
make_graph(A,B,C) :-
        graph_node(A,B,C).

warshall(A,B) :-
        warshall(A,A,B).

member_w(A-B,[C-B|D]) :-
        A==C,
        !.
member_w(A-B,[C|D]) :-
        member_w(A-B,D).

warshall([],A,A) :- !.
warshall([A-B|C],D,E) :-
        member_w(A-F,D),
        warshall(D,A,F,G),
        warshall(C,G,E).

warshall([],A,B,[]) :- !.
warshall([A-B|C],D,E,[A-F|G]) :-
        inv(D,B),
        !,
        unionv(B,E,F),
        warshall(C,D,E,G).
warshall([A-B|C],D,E,[A-B|F]) :-
        warshall(C,D,E,F).

unionv([],A,A).
unionv([A|B],C,D) :-
        unionv_2(C,A,B,D).

graph_node(A=B,C,D) :- !,
        varset(B,E),
        constC(C,A-E,D).
graph_node(A==B,C,D) :- !,
        varset(B,E),
        constC(C,A-E,D).
graph_node(A,B,B).

intersectv([],A,[]).
intersectv([A|B],C,D) :-
        intersectv_2(C,A,B,D).

intersectv_2([],A,B,[]).
intersectv_2([A|B],C,D,E) :-
        compare(F,C,A),
        intersectv_3(F,C,D,A,B,E).

intersectv_3(<,A,B,C,D,E) :-
        intersectv_2(B,C,D,E).
intersectv_3(=,A,B,C,D,[A|E]) :-
        intersectv(B,D,E).
intersectv_3(>,A,B,C,D,E) :-
        intersectv_2(D,A,B,E).

intersectv_list([],[]).
intersectv_list([A|B],C) :-
        intersectv_list(B,A,C).

intersectv_list([],A,A).
intersectv_list([A|B],C,D) :-
        intersectv(A,C,E),
        intersectv_list(B,E,D).

disjointv([],A).
disjointv([A|B],C) :-
        disjointv_2(C,A,B).

disjointv_2([],A,B).
disjointv_2([A|B],C,D) :-
        compare(E,C,A),
        disjointv_3(E,C,D,A,B).

disjointv_3(<,A,B,C,D) :-
        disjointv_2(B,C,D).
disjointv_3(>,A,B,C,D) :-
        disjointv_2(D,A,B).

diffv([],A,[]).
diffv([A|B],C,D) :-
        diffv_2(C,A,B,D).

diffv_2([],A,B,[A|B]).
diffv_2([A|B],C,D,E) :-
        compare(F,C,A),
        diffv_3(F,C,D,A,B,E).

diffv_3(<,A,B,C,D,[A|E]) :-
        diffv(B,[C|D],E).
diffv_3(=,A,B,C,D,E) :-
        diffv(B,D,E).
diffv_3(>,A,B,C,D,E) :-
        diffv_2(D,A,B,E).

unionv_2([],A,B,[A|B]).
unionv_2([A|B],C,D,E) :-
        compare(F,C,A),
        unionv_3(F,C,D,A,B,E).

unionv_3(<,A,B,C,D,[A|E]) :-
        unionv_2(B,C,D,E).
unionv_3(=,A,B,C,D,[A|E]) :-
        unionv(B,D,E).
unionv_3(>,A,B,C,D,[C|E]) :-
        unionv_2(D,A,B,E).

includev(A,B,C) :-
        includev_2(B,A,C).

includev_2([],A,[A]).
includev_2([A|B],C,D) :-
        compare(E,C,A),
        includev_3(E,C,A,B,D).

includev_3(<,A,B,C,[A,B|C]).
includev_3(=,A,B,C,[B|C]).
includev_3(>,A,B,C,[B|D]) :-
        includev_2(C,A,D).

excludev(A,B,C) :-
        excludev_2(B,A,C,B).

excludev_2([],A,[],B).
excludev_2([A|B],C,D,E) :-
        compare(F,C,A),
        excludev_3(F,C,A,B,D,E).

excludev_3(<,A,B,C,D,D).
excludev_3(=,A,B,C,C,D).
excludev_3(>,A,B,C,[B|D],E) :-
        excludev_2(C,A,D,C).

subsetv([],A).
subsetv([A|B],[C|D]) :-
        compare(E,A,C),
        subsetv_2(E,A,B,D).

subsetv_2(=,A,B,C) :-
        subsetv(B,C).
subsetv_2(>,A,B,C) :-
        subsetv([A|B],C).

small_subsetv([],A).
small_subsetv([A|B],C) :-
        inv(A,C),
        small_subsetv(B,C).

inv_2(=,A,B).
inv_2(>,A,B) :-
        inv(A,B).

uniqvar([],[]).
uniqvar([A|B],C) :-
        uniqvar(A,B,C,[]).

uniqvar(A,[],B,C) :- !,
        constC(B,A,C).
uniqvar(A,[B|C],D,E) :-
        one_uniqvar(A,B,D,F),
        uniqvar(B,C,F,E).

one_uniqvar(A,B,C,C) :-
        A==B,
        !.
one_uniqvar(A,B,C,D) :-
        A\==B,
        constC(C,A,D),
        !.

filter_vars(A,B) :-
        filter_vars(A,B,[]).

filter_vars([],A,A).
filter_vars([A|B],C,D) :-
        var(A),
        !,
        constC(C,A,E),
        filter_vars(B,E,D).
filter_vars([A|B],C,D) :-
        nonvar(A),
        !,
        filter_vars(B,C,D).

filter_vars([],[],[],[]).
filter_vars([A|B],[C|D],[A|E],[C|F]) :-
        var(A),
        !,
        filter_vars(B,D,E,F).
filter_vars([A|B],[C|D],E,F) :-
        nonvar(A),
        !,
        filter_vars(B,D,E,F).

varbag(A,B,C,D,D) :-
        B>C,
        !.
varbag(A,B,C,D,E) :-
        B=<C,
        !,
        arg(B,A,F),
        varbag(F,D,G),
        H is B+1,
        varbag(A,H,C,G,E).

union([A|B],[C|D],[A|E]) :-
        A@<C,
        !,
        union(B,[C|D],E).
union([A|B],[C|D],[A|E]) :-
        A=C,
        !,
        union(B,D,E).
union([A|B],[C|D],[C|E]) :-
        C@<A,
        !,
        union([A|B],D,E).
union([],A,A) :- !.
union(A,[],A).

intersect([A|B],[C|D],E) :-
        A@<C,
        !,
        intersect(B,[C|D],E).
intersect([A|B],[C|D],[A|E]) :-
        A=C,
        !,
        intersect(B,D,E).
intersect([A|B],[C|D],E) :-
        C@<A,
        !,
        intersect([A|B],D,E).
intersect([],A,[]) :- !.
intersect(A,[],[]).

diff([A|B],[C|D],[A|E]) :-
        A@<C,
        !,
        diff(B,[C|D],E).
diff([A|B],[C|D],E) :-
        A=C,
        !,
        diff(B,D,E).
diff([A|B],[C|D],E) :-
        C@<A,
        !,
        diff([A|B],D,E).
diff([],A,[]) :- !.
diff(A,[],A).

in(A,[A|B]) :- !.
in(A,[B|C]) :-
        A@>B,
        in(A,C).

include(A,[B|C],[A,B|C]) :-
        A@<B,
        !.
include(A,[B|C],[B|C]) :-
        A==B,
        !.
include(A,[B|C],[B|D]) :-
        A@>B,
        !,
        include(A,C,D).
include(A,[],[A]).

subset([A|B],[C|D]) :-
        A=C,
        !,
        subset(B,D).
subset([A|B],[C|D]) :-
        A@>C,
        !,
        subset([A|B],D).
subset([],A).

not_disjoint([A|B],[A|C]) :- !.
not_disjoint([A|B],[C|D]) :-
        A@>C,
        !,
        not_disjoint([A|B],D).
not_disjoint([A|B],[C|D]) :-
        A@<C,
        !,
        not_disjoint(B,[C|D]).

filter_dups(A,B) :-
        list_to_key(A,C),
        keysort(C,D),
        filter_dups(D,f,f,E,B,[]).

list_to_key([],[]).
list_to_key([A|B],[A-A|C]) :-
        list_to_key(B,C).

filter_dups([],A,B,C,D,E) :-
        dups_state_last(A,B,C,D,E).
filter_dups([A-B|C],D,E,F,G,H) :-
        dups_state(D,E,I,J,F,A,G,K),
        filter_dups(C,I,J,A,K,H).

dups_state_last(A,f,B,C,C) :- !.
dups_state_last(A,t,B,C,D) :-
        out_if_true(A,B,C,D).

dups_state(f,f,f,t,A,B,C,C) :- !.
dups_state(A,t,t,t,B,C,D,D) :-
        B==C,
        !.
dups_state(A,t,f,t,B,C,D,E) :-
        out_if_true(A,B,D,E).

out_if_true(t,A,B,C) :-
        constC(B,A,C).
out_if_true(f,A,B,B).

filter_uniq(A,B) :-
        list_to_key(A,C),
        keysort(C,D),
        filter_uniq(D,f,f,E,B,[]).

filter_uniq([],A,B,C,D,E) :-
        uniq_state_last(A,B,C,D,E).
filter_uniq([A-B|C],D,E,F,G,H) :-
        uniq_state(D,E,I,J,F,A,G,K),
        filter_uniq(C,I,J,A,K,H).

uniq_state_last(A,f,B,C,C) :- !.
uniq_state_last(A,t,B,C,D) :-
        out_if_false(A,B,C,D).

uniq_state(f,f,f,t,A,B,C,C) :- !.
uniq_state(A,t,t,t,B,C,D,D) :-
        B==C,
        !.
uniq_state(A,t,f,t,B,C,D,E) :-
        out_if_false(A,B,D,E).

out_if_false(f,A,B,C) :-
        constC(B,A,C).
out_if_false(t,A,B,B).

get(A,B,C,D,E,B) :-
        E=A,
        !.
get(A,B,C,D,E,F) :-
        compare(G,E,A),
        get(G,E,F,C,D).

get(<,A,B,C,D) :-
        get(C,A,B).
get(>,A,B,C,D) :-
        get(D,A,B).

fget_2(<,A,B,C,D,E) :-
        fget(D,A,B).
fget_2(=,A,B,C,D,E) :-
        B=C.
fget_2(>,A,B,C,D,E) :-
        fget(E,A,B).

set(node(A,B,C,D),E,F,node(A,F,C,D)) :-
        E=A,
        !.
set(node(A,B,C,D),E,F,node(A,B,G,D)) :-
        E@<A,
        !,
        set(C,E,F,G).
set(node(A,B,C,D),E,F,node(A,B,C,G)) :-
        E@>A,
        !,
        set(D,E,F,G).
set(leaf,A,B,node(A,B,leaf,leaf)).

fset(leaf,A,B,node(A,B,leaf,leaf)).
fset(node(A,B,C,D),E,F,node(A,G,H,I)) :-
        compare(J,E,A),
        fset_2(J,E,F,B,C,D,G,H,I).

fset_2(<,A,B,C,D,E,C,F,E) :-
        fset(D,A,B,F).
fset_2(=,A,B,C,D,E,B,D,E).
fset_2(>,A,B,C,D,E,C,D,F) :-
        fset(E,A,B,F).

create_array(A,B) :-
        (   key_list(A) ->
            C=A
        ;   list_to_key(A,C)
        ),
        random_permute(C,D),
        list_to_tree(D,B).

key_list([]).
key_list([A-B|C]).

random_permute(A,B) :-
        ran_keys(A,23141,C),
        keysort(C,D),
        key_to_list(D,B).

list_to_tree([],A) :-
        seal(A).
list_to_tree([A-B|C],D) :-
        get(D,A,B),
        list_to_tree(C,D).

non_empty_array(node(A,B,C,D)).

gensym(A) :-
        gensym([36,105,110,116,101,114,110,97,108,95,115,121,109,95],A).

gensym(A,B) :-
        gennum(C),
        name(C,D),
        append(A,D,E),
        name(F,E),
        !,
        B=F.

gennum(A) :-
        gensym_integer(A),
        abolish(gensym_integer,1),
        B is A+1,
        asserta(gensym_integer(B)).

gensym(A,B/C,D) :-
        atomic(B),
        atomic(C),
        !,
        name(B,E),
        name(C,F),
        append(A,E,[47],F,[95],G),
        gensym(G,D).
gensym(A,B,C) :-
        atomic(B),
        !,
        name(B,D),
        append(A,D,E),
        gensym(E,C).
gensym(A,B,C) :-
        warning(['Erroneous second argument to gensym/3: ',B]),
        gensym(A,C).

append([],A,B,C,D,E) :-
        append(A,B,C,D,E).
append([A|B],C,D,E,F,[A|G]) :-
        append(B,C,D,E,F,G).

append([],A,B,C,D) :-
        append(A,B,C,D).
append([A|B],C,D,E,[A|F]) :-
        append(B,C,D,E,F).

inst_vars(A,B) :-
        copy(A,B),
        inst_vars(B).

inst_vars(A) :-
        varset(A,B),
        inst_vars_list(0,B).

inst_vars_list(A,[]) :- !.
inst_vars_list(A,[B|C]) :- !,
        var_name(A,B),
        D is A+1,
        inst_vars_list(D,C).

cons(A) :-
        nonvar(A),
        A=[B|C].

inst_writeq_list([]).
inst_writeq_list([A|B]) :-
        inst_writeq(A),
        wn('.'),
        inst_writeq_list(B).

inst_vars_names_list(A,[],[]) :- !.
inst_vars_names_list(A,[B|C],[D|E]) :- !,
        var_name(A,D),
        F is A+1,
        inst_vars_names_list(F,C,E).

inst_writeq(A,B,C) :-
        var(A),
        !,
        memberv2(A,B,D,C),
        write(D).
inst_writeq(A,B,C) :-
        atomic(A),
        !,
        writeq(A).
inst_writeq(A,B,C) :-
        cons(A),
        !,
        write('['),
        inst_writeq_listterm(A,B,C).
inst_writeq(A,B,C) :-
        binary_op(A,D,E,F),
        paren_op(E),
        !,
        list_op(A,E,G,[]),
        length(G,H),
        (   H>1 ->
            write('(')
        ;   true
        ),
        inst_writeq_oplist(G,E,B,C),
        (   H>1 ->
            write(')')
        ;   true
        ).
inst_writeq(A,B,C) :-
        binary_op(A,D,E,F),
        !,
        inst_writeq(D,B,C),
        write(' '),
        write(E),
        write(' '),
        inst_writeq(F,B,C).
inst_writeq(A,B,C) :-
        structure(A),
        !,
        functor(A,D,E),
        writeq(D),
        write('('),
        inst_writeq(1,E,A,B,C),
        write(')').

memberv2(A,[B|C],D,[D|E]) :-
        A==B,
        !.
memberv2(A,[B|C],D,[E|F]) :-
        memberv2(A,C,D,F).

inst_writeq_listterm(A,B,C) :-
        \+list(A),
        !,
        write('|'),
        inst_writeq(A,B,C),
        write(']').
inst_writeq_listterm([],A,B) :- !,
        write(']').
inst_writeq_listterm([A|B],C,D) :-
        cons(B),
        !,
        inst_writeq(A,C,D),
        write(','),
        inst_writeq_listterm(B,C,D).
inst_writeq_listterm([A|B],C,D) :-
        \+cons(B),
        !,
        inst_writeq(A,C,D),
        inst_writeq_listterm(B,C,D).

binary_op((A-->B),A,(-->),B).
binary_op((A ',' B),A,',',B).
binary_op((A;B),A,;,B).
binary_op(A^B,A,^,B).
binary_op(A=B,A,=,B).
binary_op(A\=B,A,\=,B).
binary_op((A:-B),A,(:-),B).
binary_op((A->B),A,->,B).
binary_op(A=..B,A,=..,B).
binary_op(A@<B,A,@<,B).
binary_op(A@=<B,A,@=<,B).
binary_op(A@>B,A,@>,B).
binary_op(A@>=B,A,@>=,B).
binary_op(A<B,A,<,B).
binary_op(A=<B,A,=<,B).
binary_op(A>B,A,>,B).
binary_op(A>=B,A,>=,B).
binary_op(A=:=B,A,=:=,B).
binary_op(A=\=B,A,=\=,B).
binary_op(A\==B,A,\==,B).
binary_op(A==B,A,==,B).
binary_op(A+B,A,+,B).
binary_op(A-B,A,-,B).
binary_op(A/B,A,/,B).
binary_op(A*B,A,*,B).
binary_op(A//B,A,//,B).
binary_op(A/\B,A,/\,B).
binary_op(A\/B,A,\/,B).
binary_op(A mod B,A,' mod ',B).
binary_op(A is B,A,' is ',B).

paren_op(',').
paren_op(;).
paren_op((:-)).

list_op(A,B,C,D) :-
        nonvar(A),
        binary_op(A,E,B,F),
        !,
        list_op(E,B,C,G),
        list_op(F,B,G,D).
list_op(A,B,C,D) :-
        constC(C,A,D).

inst_writeq_oplist([],A,B,C).
inst_writeq_oplist([A|B],C,D,E) :-
        cons(B),
        !,
        inst_writeq(A,D,E),
        write(C),
        inst_writeq_oplist(B,C,D,E).
inst_writeq_oplist([A|B],C,D,E) :-
        nil(B),
        !,
        inst_writeq(A,D,E).

structure(A) :-
        nonvar(A),
        \+atomic(A),
        \+A=[B|C].

inst_writeq(A,B,C,D,E) :-
        A>B,
        !.
inst_writeq(A,B,C,D,E) :-
        A=:=B,
        !,
        arg(A,C,F),
        inst_writeq(F,D,E).
inst_writeq(A,B,C,D,E) :-
        A<B,
        !,
        arg(A,C,F),
        inst_writeq(F,D,E),
        write(','),
        G is A+1,
        inst_writeq(G,B,C,D,E).

nil(A) :-
        atom(A),
        A=[].

var_name(A,B) :-
        0=<A,
        A<26,
        !,
        C is A+[65],
        name(B,[C]).
var_name(A,B) :-
        A>=26,
        !,
        C is A mod 26+[65],
        D is A//26,
        name(D,E),
        name(B,[C|E]).

copyB(A,B) :-
        bagof(A,true,[B]).

copy1(A,B) :-
        assert(global_copy(A)),
        global_copy(C),
        abolish(global_copy,1),
        !,
        C=B.

copy2(A,B) :-
        varset(A,C),
        make_sym(C,D),
        copy2(A,B,D),
        !.

make_sym([],[]).
make_sym([A|B],[p(A,C)|D]) :-
        make_sym(B,D).

copy2(A,B,C) :-
        var(A),
        !,
        retrieve_sym(A,C,B).
copy2(A,B,C) :-
        nonvar(A),
        !,
        functor(A,D,E),
        functor(B,D,E),
        copy2(A,B,C,1,E).

copy3(A,B) :-
        varbag(A,C),
        make_sym(C,D),
        copy2(A,B,D),
        !.

retrieve_sym(A,[p(B,C)|D],C) :-
        A==B,
        !.
retrieve_sym(A,[B|C],D) :-
        retrieve_sym(A,C,D).

copy2(A,B,C,D,E) :-
        D>E,
        !.
copy2(A,B,C,D,E) :-
        D=<E,
        !,
        arg(D,A,F),
        arg(D,B,G),
        copy2(F,G,C),
        H is D+1,
        copy2(A,B,C,H,E).

map_vars(A,B,C,D) :-
        filter_vars(A,B,E,F),
        map_terms(E,F,C,D).

map_terms(A,B,C,D) :-
        memberv2(C,A,D,B),
        !.
map_terms(A,B,C,D) :-
        var(C),
        !,
        D=C.
map_terms(A,B,C,D) :-
        nonvar(C),
        !,
        functor(C,E,F),
        functor(D,E,F),
        map_terms_seq(1,F,A,B,C,D).

map_terms_seq(A,B,C,D,E,F) :-
        A>B,
        !.
map_terms_seq(A,B,C,D,E,F) :-
        A=<B,
        !,
        arg(A,E,G),
        arg(A,F,H),
        map_terms(C,D,G,H),
        I is A+1,
        map_terms_seq(I,B,C,D,E,F).

map_terms(A,B,C,D,E,F) :-
        memberv2(C,A,D,B),
        !,
        constC(E,C,F).
map_terms(A,B,C,D,E,E) :-
        var(C),
        !,
        D=C.
map_terms(A,B,C,D,E,F) :-
        nonvar(C),
        !,
        functor(C,G,H),
        functor(D,G,H),
        map_terms_seq(1,H,A,B,C,D,E,F).

map_terms_seq(A,B,C,D,E,F,G,G) :-
        A>B,
        !.
map_terms_seq(A,B,C,D,E,F,G,H) :-
        A=<B,
        !,
        arg(A,E,I),
        arg(A,F,J),
        map_terms(C,D,I,J,G,K),
        L is A+1,
        map_terms_seq(L,B,C,D,E,F,K,H).

map_instr_list([],A,B,C,C).
map_instr_list([A|B],C,D,E,F) :-
        map_terms(C,D,A,G,H,[]),
        pragma_tag_list(H,E,I),
        constC(I,G,J),
        map_instr_list(B,C,D,J,F).

pragma_tag_list([],A,A).
pragma_tag_list([A|B],C,D) :-
        pragma_tag(A,var,C,E),
        pragma_tag_list(B,E,D).

pragma_tag(A,B,C,D) :-
        tag(B,E),
        C=[pragma(tag(A,E))|D],
        true.

c_append(true,A,A).
c_append((A ',' B),C,(A ',' D)) :-
        c_append(B,C,D).

d_append(fail,A,A).
d_append((A;B),C,(A;D)) :-
        d_append(B,C,D).

insert(A,B,C) :-
        difflist(A,B,C).

insertlist(A,B,C) :-
        difflist(A,B,C).

insert(A,B,A,B).

add(A,B,C) :-
        C is B+A.

sub(A,B,C) :-
        C is B-A.

delete([],A,[]).
delete([A|B],A,C) :- !,
        delete(B,A,C).
delete([A|B],C,[A|D]) :-
        delete(B,C,D).

reverse(A,B) :-
        reverse(A,[],B).

reverse([A|B],C,D) :-
        reverse(B,[A|C],D).
reverse([],A,A).

last([A],A).
last([A|B],C) :-
        last(B,C).

same([]).
same([A|B]) :-
        same(B,A).

same([],A).
same([A|B],A) :-
        same(B,A).

range_list(A,B,[]) :-
        A>B,
        !.
range_list(A,B,[B]) :-
        A=B,
        !.
range_list(A,B,[A|C]) :-
        A<B,
        !,
        D is A+1,
        range_list(D,B,C).

downrange_list(A,B,[]) :-
        A<B,
        !.
downrange_list(A,B,[B]) :-
        A=B,
        !.
downrange_list(A,B,[A|C]) :-
        A>B,
        !,
        D is A-1,
        downrange_list(D,B,C).

ran_keys([],A,[]).
ran_keys([A|B],C,[C-A|D]) :-
        random_step(C,E),
        ran_keys(B,E,D).

key_to_list([],[]).
key_to_list([A-B|C],[B|D]) :-
        key_to_list(C,D).

random_step(A,B) :-
        B is(A*1237+2116)mod 44449.

member_conj(A,(B ',' C)) :-
        member_conj(A,B),
        !.
member_conj(A,(B ',' C)) :-
        member_conj(A,C),
        !.
member_conj(A,A).

memberv_conj(A,(B ',' C)) :-
        memberv_conj(A,B),
        !.
memberv_conj(A,(B ',' C)) :-
        memberv_conj(A,C),
        !.
memberv_conj(A,B) :-
        \+B= (C ',' D),
        A==B,
        !.

reverse_conj(A,B) :-
        reverse_conj(A,true,B).

reverse_conj((A ',' B),C,D) :-
        reverse_conj(B,(A ',' C),D).
reverse_conj(true,A,A).

append_conj(true,A,A).
append_conj((A ',' B),C,(A ',' D)) :-
        append_conj(B,C,D).

append_conj(true,A,B,C) :-
        append_conj(A,B,C).
append_conj((A ',' B),C,D,(A ',' E)) :-
        append_conj(B,C,D,E).

append_conj(true,A,B,C,D) :-
        append_conj(A,B,C,D).
append_conj((A ',' B),C,D,E,(A ',' F)) :-
        append_conj(B,C,D,E,F).

last_conj((A ',' B),A) :-
        all_true(B),
        !.
last_conj((A ',' B),C) :-
        last_conj(B,C).

all_true(true).
all_true((A ',' B)) :-
        all_true(A),
        all_true(B).

intersectv_conj(A,B,C) :-
        intersectv_conj(A,B,C,true).

intersectv_conj((A ',' B),C,D,E) :- !,
        intersectv_conj(A,C,D,F),
        intersectv_conj(B,C,F,E).
intersectv_conj(A,B,C,D) :-
        \+A= (E ',' F),
        memberv_conj(A,B),
        !,
        co(A,C,D).
intersectv_conj(A,B,C,C) :-
        \+A= (D ',' E),
        !.

co(A,(A ',' B),B).

unionv_conj(A,B,fail) :-
        memberv_conj(fail,A),
        !.
unionv_conj(A,B,fail) :-
        memberv_conj(fail,B),
        !.
unionv_conj(A,B,C) :-
        unionv_conj(A,B,C,B).

unionv_conj((A ',' B),C,D,E) :- !,
        unionv_conj(A,C,D,F),
        unionv_conj(B,C,F,E).
unionv_conj(true,A,B,B) :- !.
unionv_conj(A,B,C,C) :-
        succeeds(A),
        !.
unionv_conj(A,B,C,D) :-
        \+succeeds(A),
        \+A= (E ',' F),
        \+A=true,
        !,
        conj_if_nomember(A,B,C,D).

succeeds(true).
succeeds((A ',' B)) :-
        succeeds(A),
        succeeds(B).
succeeds((A;B)) :-
        succeeds(A).
succeeds((A;B)) :-
        succeeds(B).
succeeds(functor(A,B,C)) :-
        nonvar(A),
        nonvar(B),
        nonvar(C),
        \+errs(functor(A,B,C)),
        functor(A,B,C).
succeeds('$name_arity'(A,B,C)) :-
        nonvar(A),
        nonvar(B),
        nonvar(C),
        \+errs(functor(A,B,C)),
        functor(A,B,C).
succeeds(nonvar(A)) :-
        nonvar(A).
succeeds(ground(A)) :-
        ground(A).
succeeds(atom(A)) :-
        atom(A).
succeeds(nil(A)) :-
        nil(A).
succeeds(integer(A)) :-
        integer(A).
succeeds(negative(A)) :-
        negative(A).
succeeds(nonnegative(A)) :-
        nonnegative(A).
succeeds(float(A)) :-
        float(A).
succeeds(number(A)) :-
        number(A).
succeeds(atomic(A)) :-
        atomic(A).
succeeds(list(A)) :-
        list(A).
succeeds(cons(A)) :-
        cons(A).
succeeds(structure(A)) :-
        structure(A).
succeeds(compound(A)) :-
        compound(A).
succeeds(simple(A)) :-
        nonvar(A),
        simple(A).
succeeds(A) :-
        encode_relop(A,B,C,D),
        test_relop(B,C,D),
        \+errs(A).

conj_if_nomember(A,B,C,C) :-
        memberv_conj(A,B),
        !.
conj_if_nomember(A,B,C,D) :-
        \+memberv_conj(A,B),
        !,
        co(A,C,D).

flat_conj(true,A,A) :- !.
flat_conj((A ',' B),C,D) :- !,
        flat_conj(A,C,E),
        flat_conj(B,E,D).
flat_conj(A,B,C) :-
        co(A,B,C).

replace_start_conj(true,A,A).
replace_start_conj((A ',' B),(C ',' D),(A ',' E)) :-
        replace_start_conj(B,D,E).

split_disj((A;B),A,B) :- !.
split_disj(A,A,fail).

ground(0,A) :- !.
ground(A,B) :-
        A>0,
        !,
        arg(A,B,C),
        ground(C),
        D is A-1,
        ground(D,B).

compound(A) :-
        nonvar(A),
        \+atomic(A).

simple(A) :-
        var(A),
        !.
simple(A) :-
        atomic(A),
        !.

denumerable(A) :-
        atom(A),
        !.
denumerable(A) :-
        integer(A),
        !.

negative(A) :-
        integer(A),
        A<0.

positive(A) :-
        integer(A),
        A>0.

nonpositive(A) :-
        integer(A),
        A=<0.

full_list([]).
full_list([A|B]) :-
        full_list(B).

conj_p(A) :-
        nonvar(A),
        (   A= (B ',' C)
        ;   A=true
        ),
        !.

disj_p(A) :-
        nonvar(A),
        (   A= (B;C)
        ;   A=fail
        ),
        !.

strong_disj_p(A) :-
        nonvar(A),
        A= (B;C).

conj_p((A ',' B),A,B).

disj_p((A;B),A,B).

di(A,(A;B),B).

unify_p(A=B).

call_p(A) :-
        \+unify_p(A).

split_unify(A=B,A,B).
split_unify(A=B,B,A).

split_unify(A,B,A,B).
split_unify(A,B,B,A).

split_unify_v(A=B,A,B) :-
        var(A).
split_unify_v(A=B,B,A) :-
        var(B).

split_unify_v(A,B,A,B) :-
        var(A).
split_unify_v(A,B,B,A) :-
        var(B).

split_unify_v_nv(A=B,A,B) :-
        var(A),
        nonvar(B).
split_unify_v_nv(A=B,B,A) :-
        var(B),
        nonvar(A).

split_unify_v_nv(A,B,A,B) :-
        var(A),
        nonvar(B).
split_unify_v_nv(A,B,B,A) :-
        var(B),
        nonvar(A).

downrange(A,B,B).
downrange(A,B,C) :-
        A<C,
        D is C-1,
        downrange(A,B,D).

crange(A,A,A).
crange(A,B,C) :-
        A<C,
        D is(A+C)//2,
        crange(A,B,D).
crange(A,B,C) :-
        A<C,
        D is(A+C)//2+1,
        crange(D,B,C).

gen_integer(A) :-
        range(1,A,10000).

max(A,B,A) :-
        A@>=B,
        !.
max(A,B,B) :-
        B@>A.

maximum(A,B,C) :-
        max(A,B,C).

min(A,B,A) :-
        A@=<B,
        !.
min(A,B,B) :-
        B@<A.

minimum(A,B,C) :-
        min(A,B,C).

min_integer(A,B,A) :-
        A<B,
        !.
min_integer(A,B,B) :-
        A>=B,
        !.

maxlist([A|B],C) :-
        maxlist(B,A,C),
        !.

maxlist([],A,A).
maxlist([A|B],C,D) :-
        max(A,C,E),
        maxlist(B,E,D).

minlist([A|B],C) :-
        minlist(B,A,C),
        !.

minlist([],A,A).
minlist([A|B],C,D) :-
        min(A,C,E),
        minlist(B,E,D).

shorter_than([],A) :-
        A>0.
shorter_than([A|B],C) :-
        C>0,
        D is C-1,
        shorter_than(B,D).

longer_than([A|B],C) :-
        C=<0.
longer_than([A|B],C) :-
        C>0,
        D is C-1,
        longer_than(B,D).

length_disj(fail,A,A) :- !.
length_disj((A;B),C,D) :- !,
        length_disj(A,C,E),
        length_disj(B,E,D).
length_disj(A,B,C) :-
        \+disj_p(A),
        !,
        add(1,B,C).

length_conj(A,B) :-
        length_conj(A,0,B).

length_conj(true,A,A) :- !.
length_conj((A ',' B),C,D) :- !,
        length_conj(A,C,E),
        length_conj(B,E,D).
length_conj(A,B,B) :-
        cut_p(A),
        !.
length_conj(A,B,C) :-
        \+conj_p(A),
        !,
        add(1,B,C).

cut_p('$cut_deep'(A)).
cut_p('$cut_shallow'(A)).

length_test_user(A,B,C) :-
        length_test(A,D,0,B),
        length_conj(D,C).

length_test(true,true,A,A) :- !.
length_test((A ',' B),C,D,E) :-
        test(A),
        !,
        add(1,D,F),
        length_test(B,C,F,E).
length_test(A,true,B,C) :-
        test(A),
        !,
        add(1,B,C).
length_test(A,A,B,B).

test(A) :-
        encode_relop(A,B,C,D),
        !.
test(A) :-
        encode_test(A,B,C,D),
        !.
test(A) :-
        encode_name_arity(A,B,C,D,E),
        !.

or(true,true,true).
or(true,false,true).
or(false,true,true).
or(false,false,false).

or(A,B,C,D) :-
        or(A,B,E),
        or(E,C,D).

and(true,true,true).
and(true,false,false).
and(false,true,false).
and(false,false,false).

and(A,B,C,D) :-
        and(A,B,E),
        and(E,C,D).

write_code([],A).
write_code([A|B],C) :-
        write_code(A,B,C).

write_output([]).
write_output([A|B]) :-
        nl_if_procedure(A),
        tab_if_nonlbl(A),
        wq(A),
        wn('.'),
        write_output(B).

write_code(switch(unify,A,B,C,D,E),F,G) :- !,
        H is G+4,
        tab(G),
        w('switch('),
        w(B),
        wn(') {'),
        tab(G),
        tag(var,I),
        w(I),
        wn(:),
        write_code(C,H),
        tab(G),
        w(A),
        wn(:),
        write_code(D,H),
        tab(G),
        w('else: '),
        wn(E),
        tab(G),
        wn('}'),
        write_code(F,G).
write_code(label(A),B,C) :-
        D is C-4,
        tab(D),
        wq(A),
        wn(:),
        write_code(B,C).
write_code(procedure(A),B,C) :-
        D is C-4,
        tab(D),
        nl,
        wqn(procedure(A)),
        nl,
        write_code(B,C).
write_code(A,B,C) :-
        \+A=label(D),
        \+A=procedure(E),
        \+A=switch(unify,F,G,H,I,J),
        tab(C),
        wqn(A),
        write_code(B,C).

tag(A,B) :-
        tag_type_test(B,A,C,D).
tag(A,B) :-
        tag_type_test(B,C,D,A).

wq(A) :-
        writeq(A).

wqn(A) :-
        writeq(A),
        nl.

nl_if_procedure(procedure(A)) :- !,
        nl.
nl_if_procedure(A).

tab_if_nonlbl(label(A)) :- !.
tab_if_nonlbl(procedure(A)) :- !.
tab_if_nonlbl(A) :-
        \+A=label(B),
        \+A=procedure(C),
        !,
        put(9).

xwrite_modes([]) :- !.
xwrite_modes(A) :-
        cons(A),
        !,
        nl,
        write('% Modes:'),
        nl,
        xwrite_clauses(A),
        nl,
        write('% Source:').
xwrite_modes(A) :-
        \+cons(A),
        !,
        xwrite_modes([A]).

xwrite_source([]) :- !.
xwrite_source(A) :-
        cons(A),
        nl,
        xwrite_clauses(A).

xwrite_clauses([]).
xwrite_clauses([A|B]) :-
        write('%    '),
        write(A),
        write('.'),
        nl,
        xwrite_clauses(B).

make_msg(A,B) :-
        inst_vars(A,C),
        make_list(C,B).

cm :-
        w('% *** ').

msg_list([],A) :- !,
        nl.
msg_list([A|B],C) :-
        msg_one(A,C),
        msg_list(B,C).

aesthetic(A) :-
        make_msg(A,B),
        ae,
        msg_list(B,ae).

ae :-
        w('% *** Aesthetic: ').

w :-
        w('% *** Warning: ').

e :-
        w('% *** Error: ').

comment(A,B) :-
	true.
%         compile_option(comment),
%         !,
%         make_msg(A,B,C),
%         cm,
%         msg_list(C,cm).
% comment(A,B) :-
%         \+compile_option(comment),
%         !.

make_msg(A,B,C) :-
        inst_vars(s(A,B),s(D,E)),
        make_list(E,C).

aesthetic(A,B) :-
        make_msg(A,B,C),
        ae,
        msg_list(C,ae).

warning(A,B) :-
        make_msg(A,B,C),
        w,
        msg_list(C,w).

error(A,B) :-
        make_msg(A,B,C),
        e,
        msg_list(C,e).

msg_one(tab,A) :- !,
        put(9).
msg_one(nl,cm) :- !,
        nl,
        cms.
msg_one(nl,ae) :- !,
        nl,
        aes.
msg_one(nl,w) :- !,
        nl,
        ws.
msg_one(nl,e) :- !,
        nl,
        es.
msg_one(A,B) :- !,
        w(A).

cms :-
        w('% *** ').

aes :-
        w('% ***            ').

ws :-
        w('% ***          ').

es :-
        w('% ***        ').

f :-
        wn(' ***').

wd(A,B,B) :-
        wd(A).

wd(A) :-
        fail, % compile_option(debug),
        !,
        w(A).
wd(A) :-
        true. % \+compile_option(debug).

write_debug(A,B,B) :-
        write_debug(A).

write_debug(A) :-
        wd(A),
        nl_debug.

nl_debug :-
        (   fail -> % compile_option(debug) ->
            nl
        ;   true
        ).

write_list([]).
write_list([A|B]) :-
        w(A),
        nl,
        write_list(B).

not_used(A) :-
        w(A),
        wn('- not used!').

not_used(A,B,B) :-
        not_used(A).

get_cputime(A) :-
        the_system(cprolog),
        !,
        A=0.
get_cputime(A) :-
        the_system(quintus),
        !,
        A=0.
get_cputime(A) :-
        the_system(aquarius),
        !,
        A=0.
get_cputime(A) :-
        A=0.

write_num(none) :- !.
write_num(A) :-
        w(-),
        w(A).

ttyflush_quintus :-
        the_system(quintus),
        !,
        ttyflush.
ttyflush_quintus.

stats(A,B,C,C) :-
        stats(A,B).

survive(A) :-
        info(A,y,B,C,D),
        !.
survive(not(A)) :-
        info(A,y,B,C,D),
        !.

survive_form(A==B,C) :- !,
        implies(C,(simple(A);simple(B))).
survive_form(A\==B,C) :- !,
        implies(C,(simple(A);simple(B))).
survive_form(A,B) :-
        survive(A).

anyregs(A,y) :-
        anyregs(A),
        !.
anyregs(A,n) :-
        \+anyregs(A),
        !.

anyregs(A) :-
        info(A,B,y,C,D),
        !.

builtin(A,y) :-
        builtin(A),
        !.
builtin(A,n) :-
        \+builtin(A),
        !.

expanded(A,y) :-
        expanded(A),
        !.
expanded(A,n) :-
        \+expanded(A),
        !.

expanded(A) :-
        info(A,B,C,D,y),
        !.

fixregs(A) :-
        \+anyregs(A).
fixregs(A) :-
        \+builtin(A).

info(A,B,C,D,E) :-
        info(A,B,C,D,E,F,G,H).

compmodes(A,B) :-
        info(A,C,D,E,F,G,H,B),
        !.

info(A,B,n,n,n,C,D,E) :-
        mode_option(A,D,F,E,B),
        !,
        A=..[G|C].
info('$cut_load'(A),y,y,y,y,[A],uninit_reg(A),true).
info('$cut_deep'(A),y,y,y,y,[],true,true).
info('$cut_shallow'(A),y,y,y,y,[],true,true).
info('$if2f'(A,B),y,y,y,y,[B],C,D) :-
        ar_mf(A,B,C,D).
info('$if2i'(A,B),y,y,y,y,[B],C,D) :-
        ar_mf(A,B,C,D).
info('$add'(A,B,C),y,y,y,y,[C],D,E) :-
        ar_mf(A,B,C,D,E).
info('$sub'(A,B,C),y,y,y,y,[C],D,E) :-
        ar_mf(A,B,C,D,E).
info('$mul'(A,B,C),y,y,y,y,[C],D,E) :-
        ar_mf(A,B,C,D,E).
info('$fdiv'(A,B,C),y,y,y,y,[C],D,E) :-
        ar_mf(A,B,C,D,E).
info('$idiv'(A,B,C),y,y,y,y,[C],D,E) :-
        ar_mf(A,B,C,D,E).
info('$mod'(A,B,C),y,y,y,y,[C],D,E) :-
        ar_mf(A,B,C,D,E).
info('$and'(A,B,C),y,y,y,y,[C],D,E) :-
        ar_m(A,B,C,D,E).
info('$or'(A,B,C),y,y,y,y,[C],D,E) :-
        ar_m(A,B,C,D,E).
info('$xor'(A,B,C),y,y,y,y,[C],D,E) :-
        ar_m(A,B,C,D,E).
info('$sll'(A,B,C),y,y,y,y,[C],D,E) :-
        ar_m(A,B,C,D,E).
info('$sra'(A,B,C),y,y,y,y,[C],D,E) :-
        ar_m(A,B,C,D,E).
info('$not'(A,B),y,y,y,y,[B],C,D) :-
        ar_m(A,B,C,D).
info(A@<B,n,n,y,n,[],C,A@<B) :-
        d_m([A,B],C).
info(A@>B,n,n,y,n,[],C,A@>B) :-
        d_m([A,B],C).
info(A@=<B,n,n,y,n,[],C,A@=<B) :-
        d_m([A,B],C).
info(A@>=B,n,n,y,n,[],C,A@>=B) :-
        d_m([A,B],C).
info(A==B,n,n,y,y,[],C,A==B) :-
        d_m([A,B],C).
info(A\==B,n,n,y,y,[],C,A\==B) :-
        d_m([A,B],C).
info(A<B,y,y,y,y,[],C,(D ',' A<B)) :-
        rel_mf(A,B,C,D).
info(A>B,y,y,y,y,[],C,(D ',' A>B)) :-
        rel_mf(A,B,C,D).
info(A=<B,y,y,y,y,[],C,(D ',' A=<B)) :-
        rel_mf(A,B,C,D).
info(A>=B,y,y,y,y,[],C,(D ',' A>=B)) :-
        rel_mf(A,B,C,D).
info(A=\=B,y,y,y,y,[],C,(D ',' A=\=B)) :-
        rel_mf(A,B,C,D).
info(A=:=B,y,y,y,y,[],C,(D ',' A=:=B)) :-
        rel_mf(A,B,C,D).
info(A=B,y,y,y,y,[A,B],C,A==B) :-
        d_m([A,B],C).
info('$unify'(A,B),y,y,y,n,[A,B],C,A==B) :-
        d_m([A,B],C).
info(A\=B,n,n,y,y,[],C,A\=B) :-
        d_m([A,B],C).
info('$test'(A,B),y,y,y,y,[],C,('$test'(A,B)',' C)) :-
        d_m([A],C).
info('$equal'(A,B),y,y,y,y,[],C,(A==B ',' D)) :-
        rd_m([A,B],C,D).
info(var(A),y,y,y,y,[],B,(var(A)',' C)) :-
        rd_m([A],B,C).
info(nonvar(A),y,y,y,y,[],B,(nonvar(A)',' B)) :-
        d_m([A],B).
info(atom(A),y,y,y,y,[],B,(atom(A)',' C)) :-
        rd_m([A],B,C).
info('$atom_nonnil'(A),y,y,y,y,[],B,('$atom_nonnil'(A)',' C)) :-
        rd_m([A],B,C).
info(atomic(A),y,y,y,y,[],B,(atomic(A)',' C)) :-
        rd_m([A],B,C).
info(denumerable(A),y,y,y,y,[],B,(denumerable(A)',' C)) :-
        rd_m([A],B,C).
info(nil(A),y,y,y,y,[],B,(nil(A)',' C)) :-
        rd_m([A],B,C).
info(cons(A),y,y,y,y,[],B,(cons(A)',' B)) :-
        d_m([A],B).
info(list(A),y,y,y,y,[],B,(list(A)',' B)) :-
        d_m([A],B).
info(structure(A),y,y,y,y,[],B,(structure(A)',' B)) :-
        d_m([A],B).
info(compound(A),y,y,y,y,[],B,(compound(A)',' B)) :-
        d_m([A],B).
info(simple(A),y,y,y,y,[],B,(simple(A)',' C)) :-
        rd_m([A],B,C).
info(ground(A),n,n,y,y,[],B,(ground(A)',' B)) :-
        d_m([A],B).
info(number(A),y,y,y,y,[],B,(number(A)',' C)) :-
        rd_m([A],B,C).
info(float(A),y,y,y,y,[],B,(float(A)',' C)) :-
        rd_m([A],B,C).
info(integer(A),y,y,y,y,[],B,(integer(A)',' C)) :-
        rd_m([A],B,C).
info(negative(A),y,y,y,y,[],B,(negative(A)',' C)) :-
        rd_m([A],B,C).
info(nonnegative(A),y,y,y,y,[],B,(nonnegative(A)',' C)) :-
        rd_m([A],B,C).
info(functor(A,B,C),n,n,y,y,[A,B,C],D,('$name_arity'(A,B,C)',' atomic(B)',' integer(C))) :-
        d_m([A,B,C],D).
info('$name_arity'(A,B,C),y,y,y,y,[],D,('$name_arity'(A,B,C)',' D)) :-
        d_m([A],D).
info(arg(A,B,C),n,n,y,y,[A,B,C],D,(integer(A)',' nonvar(B))) :-
        d_m([A,B,C],D).
info(A is B,n,n,y,y,[A],(deref(A)',' deref(B)),integer(A)).
info(A=..B,n,n,y,n,[A,B],(deref(A)',' deref(B)),(nonvar(A)',' cons(B))).
info(!,y,y,y,y,[],true,true).
info(true,y,y,y,y,[],true,true).
info(otherwise,y,y,y,y,[],true,true).
info(fail,y,y,y,y,[],true,fail).
info(false,y,y,y,y,[],true,fail).
info(copy_term(A,B),n,n,y,n,[B],(deref(A)',' deref(B)),true).
info(call(A),n,n,y,n,[A],deref(A),nonvar(A)).
info(length(A,B),n,n,y,n,[A,B],(deref(A)',' deref(B)),(list(A)',' integer(B))).
info(compare(A,B,C),n,n,y,n,[A],(deref(A)',' deref(B)',' deref(C)),atom(A)).
info(expand_term(A,B),n,n,y,n,[A,B],(deref(A)',' deref(B)),true).
info(sort(A,B),n,n,y,n,[B],(deref(A)',' deref(B)),(list(A)',' list(B))).
info(keysort(A,B),n,n,y,n,[B],(deref(A)',' deref(B)),(list(A)',' list(B))).
info(msort(A,B),n,n,y,n,[B],(deref(A)',' deref(B)),(list(A)',' list(B))).
info(name(A,B),n,n,y,n,[A,B],(deref(A)',' deref(B)),(atomic(A)',' list(B))).
info(atom_chars(A,B),n,n,y,n,[A,B],(deref(A)',' deref(B)),(atom(A)',' list(B))).
info(number_chars(A,B),n,n,y,n,[A,B],(deref(A)',' deref(B)),(number(A)',' list(B))).
info(numbervars(A,B,C),n,n,y,n,[A,C],(deref(A)',' deref(B)',' deref(C)),(integer(B)',' integer(C))).
info(repeat,n,n,y,n,[],true,true).
info(is_list(A),n,n,y,n,[],deref(A),(var(A);list(A))).
info(is_proper_list(A),n,n,y,n,[],deref(A),list(A)).
info(is_partial_list(A),n,n,y,n,[],deref(A),(var(A);cons(A))).
info(abolish(A),n,n,y,n,[],deref(A),ground(A)).
info(abolish(A,B),n,n,y,n,[],(deref(A)',' deref(B)),(atomic(A)',' integer(B))).
info(assert(A),n,n,y,n,[],deref(A),nonvar(A)).
info(assert(A,B),n,n,y,n,[B],(deref(A)',' deref(B)),(nonvar(A)',' ground(B))).
info(asserta(A),n,n,y,n,[],deref(A),nonvar(A)).
info(asserta(A,B),n,n,y,n,[B],(deref(A)',' deref(B)),(nonvar(A)',' ground(B))).
info(assertz(A),n,n,y,n,[],deref(A),nonvar(A)).
info(assertz(A,B),n,n,y,n,[B],(deref(A)',' deref(B)),(nonvar(A)',' ground(B))).
info(retract(A),n,n,y,n,[A],deref(A),nonvar(A)).
info(retractall(A),n,n,y,n,[],deref(A),nonvar(A)).
info(A^B,n,n,y,n,[A,B],(deref(A)',' deref(B)),true).
info(bagof(A,B,C),n,n,y,n,[C],(deref(A)',' deref(B)',' deref(C)),(nonvar(B)',' cons(C))).
info(setof(A,B,C),n,n,y,n,[C],(deref(A)',' deref(B)',' deref(C)),(nonvar(B)',' cons(C))).
info(findall(A,B,C),n,n,y,n,[C],(deref(A)',' deref(B)',' deref(C)),(nonvar(B)',' cons(C))).
info(clause(A,B),n,n,y,n,[A,B],(deref(A)',' deref(B)),(nonvar(A)',' nonvar(B))).
info(clause(A,B,C),n,n,y,n,[A,B,C],(deref(A)',' deref(B)',' deref(C)),(nonvar(A)',' nonvar(B)',' ground(C))).
info(abort,n,n,y,n,[],true,true).
info(break,n,n,y,n,[],true,true).
info(halt,n,n,y,n,[],true,true).
info(trace,n,n,y,n,[],true,true).
info(error_data(A,B,C,D),n,n,y,n,[A,B,C,D],(deref(A)',' deref(B)',' deref(C)',' deref(D)),true).
info(file_error_condition(A),n,n,y,n,[A],deref(A),true).
info(type_failure_condition(A),n,n,y,n,[A],deref(A),true).
info(unknown_predicate_condition(A),n,n,y,n,[A],deref(A),true).
info(unknown(A,B),n,n,y,n,[A],(deref(A)',' deref(B)),(atom(A)',' atom(B))).
info(nodebug,n,n,y,n,[],true,true).
info(debug,n,n,y,n,[],true,true).
info(leash(A),n,n,y,n,[],deref(A),true).
info(debugging,n,n,y,n,[],true,true).
info(nofileerrors,n,n,y,n,[],true,true).
info(fileerrors,n,n,y,n,[],true,true).
%info(nospy A,n,n,y,n,[],deref(A),nonvar(A)).
%info(spy A,n,n,y,n,[],deref(A),nonvar(A)).
info(op(A,B,C),n,n,y,n,[],(deref(A)',' deref(B)',' deref(C)),true).
info(prompt(A,B),n,n,y,n,[A],(deref(A)',' deref(B)),(atom(A)',' atom(B))).
info(listing,n,n,y,n,[],true,true).
info(listing(A),n,n,y,n,[],deref(A),ground(A)).
info(current_atom(A),n,n,y,n,[A],deref(A),atom(A)).
info(current_functor(A,B),n,n,y,n,[A,B],(deref(A)',' deref(B)),(atomic(A)',' nonvar(B))).
info(current_predicate(A,B),n,n,y,n,[A,B],(deref(A)',' deref(B)),(atomic(A)',' nonvar(B))).
info(current_op(A,B,C),n,n,y,n,[A,B,C],(deref(A)',' deref(B)',' deref(C)),(integer(A)',' atom(B)',' atom(C))).
info(current_key(A,B),n,n,y,n,[A,B],(deref(A)',' deref(B)),(atomic(A)',' nonvar(B))).
info(predicate_property(A,B),n,n,y,n,[A,B],(deref(A)',' deref(B)),true).
info(prolog_flag(A,B,C),n,n,y,n,[B],(deref(A)',' deref(B)',' deref(C)),(atom(A)',' ground(B)',' ground(C))).
info(prolog_flag(A,B),n,n,y,n,[A,B],(deref(A)',' deref(B)),(atom(A)',' ground(B))).
info([A|B],n,n,y,y,[],(deref(A)',' deref(B)),(ground(A)',' ground(B))).
info(consult(A),n,n,y,n,[],deref(A),ground(A)).
info(ensure_loaded(A),n,n,y,n,[],deref(A),ground(A)).
info(close(A),n,n,y,n,[],deref(A),ground(A)).
info(exists(A),n,n,y,n,[],deref(A),ground(A)).
info(reconsult(A),n,n,y,n,[],deref(A),ground(A)).
info(rename(A,B),n,n,y,n,[],(deref(A)',' deref(B)),(ground(A)',' ground(B))).
info(save(A),n,n,y,n,[],deref(A),ground(A)).
info(see(A),n,n,y,n,[],deref(A),ground(A)).
info(seeing(A),n,n,y,n,[A],deref(A),ground(A)).
info(seen,n,n,y,n,[],true,true).
info(tell(A),n,n,y,n,[],deref(A),ground(A)).
info(telling(A),n,n,y,n,[A],deref(A),ground(A)).
info(told,n,n,y,n,[],true,true).
info(get(A),n,n,y,n,[A],deref(A),integer(A)).
info(get0(A),n,n,y,n,[A],deref(A),integer(A)).
info(skip(A),n,n,y,n,[],deref(A),integer(A)).
info(read(A),n,n,y,n,[A],deref(A),true).
info(nl,n,n,y,n,[],true,true).
info(tab(A),n,n,y,n,[],deref(A),integer(A)).
info(put(A),n,n,y,n,[],deref(A),integer(A)).
info(print(A),n,n,y,n,[],deref(A),true).
info(write(A),n,n,y,n,[],deref(A),true).
info(writeq(A),n,n,y,n,[],deref(A),true).
info(write_canonical(A),n,n,y,n,[],deref(A),true).
info(display(A),n,n,y,n,[],deref(A),true).
info(format(A,B),n,n,y,n,[],(deref(A)',' deref(B)),ground(A)).
info(portray_clause(A),n,n,y,n,[],deref(A),true).
info(recorda(A,B,C),n,n,y,n,[B,C],(deref(A)',' deref(B)',' deref(C)),ground(C)).
info(recordz(A,B,C),n,n,y,n,[B,C],(deref(A)',' deref(B)',' deref(C)),ground(C)).
info(recorded(A,B,C),n,n,y,n,[A,B,C],(deref(A)',' deref(B)',' deref(C)),ground(C)).
info(erase(A),n,n,y,n,[],deref(A),ground(A)).
info(current_key(A,B),n,n,y,n,[A,B],(deref(A)',' deref(B)),(atomic(A)',' nonvar(B))).
info(instance(A,B),n,n,y,n,[B],(deref(A)',' deref(B)),ground(A)).

bindset(A,B) :-
        bindbag(A,C),
        sort(C,B).

bindbag(A,B) :-
        info(A,C,D,E,F,G,H,I),
        !,
        varbag(G,B).
bindbag(A,B) :-
        varbag(A,B),
        !.

binds(A) :-
        bindbag(A,B),
        cons(B).

info(A) :-
        info(A,B,C,D,E).

before(A,B) :-
        mode_option(A,C,D,E,F),
        !,
        logical_simplify(D,B).
before(A,true) :-
        \+mode_option(A,B,C,D,E),
        !.

ar_mf(A,B,C,D) :-
        C= (deref(A)',' uninit_reg(B)),
        D= (number(A)',' number(B)',' rderef(A)',' rderef(B)).

ar_mf(A,B,C,D,E) :-
        D= (deref(A)',' deref(B)',' uninit_reg(C)),
        E= (number(A)',' number(B)',' number(C)',' rderef(A)',' rderef(B)',' rderef(C)).

ar_m(A,B,C,D,E) :-
        D= (deref(A)',' deref(B)',' uninit_reg(C)),
        E= (integer(A)',' integer(B)',' integer(C)',' rderef(A)',' rderef(B)',' rderef(C)).

ar_m(A,B,C,D) :-
        C= (deref(A)',' uninit_reg(B)),
        D= (integer(A)',' integer(B)',' rderef(A)',' rderef(B)).

d_m([A],deref(A)).
d_m([A|B],(deref(A)',' C)) :-
        d_m(B,C).

rel_mf(A,B,C,D) :-
        C= (deref(A)',' deref(B)),
        D= (number(A)',' number(B)',' rderef(A)',' rderef(B)).

rd_m([A],deref(A),rderef(A)).
rd_m([A|B],(deref(A)',' C),(rderef(A)',' D)) :-
        rd_m(B,C,D).

ar_ff(A,B,C,D,E) :-
        D= (deref(A)',' deref(B)',' uninit_reg(C)),
        E= (float(A)',' float(B)',' float(C)',' rderef(A)',' rderef(B)',' rderef(C)).

rel_m(A,B,C,D) :-
        C= (deref(A)',' deref(B)),
        D= (integer(A)',' integer(B)',' rderef(A)',' rderef(B)).

control((A ',' B)).
control((A;B)).
control((A->B)).
control(\+A).
control(not(A)).

branch(A) :-
        branch(A,B,C).

branch(fail,A,A).
branch(return,A,A).
branch(jump(A),B,C) :-
        constC(B,A,C).
branch(jump(A,B,C,D),E,F) :-
        constC(E,D,F).
branch(jump_nt(A,B,C,D),E,F) :-
        constC(E,D,F).
branch(call(A),B,C) :-
        constC(B,A,C).
branch(test(A,B,C,D),E,F) :-
        constC(E,D,F).
branch(equal(A,B,C),D,E) :-
        constC(D,C,E).
branch(unify(A,B,C,D,E),F,G) :-
        constC(F,E,G).
branch(unify_atomic(A,B,C),D,E) :-
        constC(D,C,E).
branch(choice(A,B,C),D,E) :-
        constC(D,C,E).
branch(hash(A,B,C,D),E,F) :-
        constC(E,D,F).
branch(pair(A,B),C,D) :-
        constC(C,B,D).
branch(switch(A,B,C,D,E),F,G) :-
        constC(F,C,H),
        constC(H,D,I),
        constC(I,E,G).
branch(switch(unify,A,B,C,D,E),F,G) :-
        constC(F,E,G).

map_branch(jump(A),A,jump(B),B).
map_branch(jump(A,B,C,D),D,jump(A,B,C,E),E).
map_branch(jump_nt(A,B,C,D),D,jump_nt(A,B,C,E),E).
map_branch(call(A),A,call(B),B).
map_branch(test(A,B,C,D),D,test(A,B,C,E),E).
map_branch(equal(A,B,C),C,equal(A,B,D),D).
map_branch(unify(A,B,C,D,E),E,unify(A,B,C,D,F),F).
map_branch(unify_atomic(A,B,C),C,unify_atomic(A,B,D),D).
map_branch(choice(A,B,C),C,choice(A,B,D),D).
map_branch(hash(A,B,C,D),D,hash(A,B,C,E),E).
map_branch(pair(A,B),B,pair(A,C),C).
map_branch(switch(A,B,C,D,E),C,switch(A,B,F,D,E),F).
map_branch(switch(A,B,C,D,E),D,switch(A,B,C,F,E),F).
map_branch(switch(A,B,C,D,E),E,switch(A,B,C,D,F),F).

pure_branch(A) :-
        pure_branch(A,B,C).

pure_branch(jump(A),B,C) :-
        constC(B,A,C).
pure_branch(jump(A,B,C,D),E,F) :-
        constC(E,D,F).
pure_branch(jump_nt(A,B,C,D),E,F) :-
        constC(E,D,F).
pure_branch(test(A,B,C,D),E,F) :-
        constC(E,D,F).
pure_branch(equal(A,B,C),D,E) :-
        constC(D,C,E).
pure_branch(unify(A,B,C,D,E),F,G) :-
        constC(F,E,G).
pure_branch(switch(A,B,C,D,E),F,G) :-
        constC(F,C,H),
        constC(H,D,I),
        constC(I,E,G).
pure_branch(switch(unify,A,B,C,D,E),F,G) :-
        constC(F,E,G).
pure_branch(unify_atomic(A,B,C),D,E) :-
        constC(D,C,E).

pure_branch(A,B) :-
        pure_branch(A,B,[]).

distant_branch(fail).
distant_branch(return).
distant_branch(jump(A)).
distant_branch(switch(A,B,C,D,E)).

local_instr(A) :-
        local_instr(A,B,C).

local_instr(pragma(A),B,B).
local_instr(deref(A),B,C) :-
        constC(B,A,C).
local_instr(deref(A,B),C,D) :-
        constC(C,A,D).
local_instr(trail(A),B,C) :-
        constC(B,A,C).
local_instr(move(A,B),C,D) :-
        constC(C,A,D).
local_instr(push(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(adda(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(pad(A),B,C) :-
        constC(B,r(h),C).
local_instr(allocate(A),B,B).
local_instr(deallocate(A),B,B).
local_instr(f2i(A,B),C,D) :-
        constC(C,A,D).
local_instr(i2f(A,B),C,D) :-
        constC(C,A,D).
local_instr(fadd(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(fsub(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(fmul(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(fdiv(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(add(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(sub(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(mul(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(div(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(and(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(or(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(xor(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(sll(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(sra(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(not(A,B),C,D) :-
        constC(C,A,D).
local_instr(ord(A,B),C,D) :-
        constC(C,A,D).
local_instr(val(A,B,C),D,E) :-
        constC(D,B,E).
local_instr(add_nt(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(sub_nt(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(and_nt(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(or_nt(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(xor_nt(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(sll_nt(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(sra_nt(A,B,C),D,E) :-
        constC(D,A,F),
        constC(F,B,E).
local_instr(not_nt(A,B),C,D) :-
        constC(C,A,D).
local_instr(trail_bda(A),B,C) :-
        constC(B,A,C).

tag_type_test(tflt,float,A,float(A)) :-
        true. % float.
tag_type_test(tint,integer,A,integer(A)).
tag_type_test(tpos,nonnegative,A,nonnegative(A)) :-
        fail. % split_integer.
tag_type_test(tneg,negative,A,negative(A)) :-
        fail. % split_integer.
tag_type_test(tatm,atom,A,atom(A)).
tag_type_test(tstr,structure,A,structure(A)).
tag_type_test(tlst,cons,A,cons(A)).
tag_type_test(tvar,var,A,var(A)).

term_tag(A,tflt) :-
        float(A),
        true. % float.
term_tag(A,tint) :-
        integer(A),
        true. % \+split_integer.
term_tag(A,tpos) :-
        nonnegative(A),
        fail. % split_integer.
term_tag(A,tneg) :-
        negative(A),
        fail. % split_integer.
term_tag(A,tatm) :-
        atom(A).
term_tag(A,tstr) :-
        structure(A).
term_tag(A,tlst) :-
        cons(A).

pointer_tag(tstr).
pointer_tag(tlst).
pointer_tag(tvar).

tag_always(integer,tint).
tag_always(nonnegative,tpos).
tag_always(negative,tneg).
tag_always(float,tflt).
tag_always(integer(A),tint).
tag_always(nonnegative(A),tpos).
tag_always(negative(A),tneg).
tag_always(float(A),tflt).

tag(A,B,B) :-
        tag(A).

tag(A) :-
        tag_type_test(A,B,C,D),
        !.

tag(A,B,C,C) :-
        tag_type_test(B,A,D,E).
tag(A,B,C,C) :-
        tag_type_test(B,D,E,A).

test_tag(A,B) :-
        tag_type_test(B,C,D,A).

test_tag(A,B,C) :-
        tag_type_test(C,D,B,A).

term_tag(A,B,C,C) :-
        term_tag(A,B).

atomic_type(A) :-
        tag_type_test(B,A,C,D),
        \+pointer_tag(B),
        !.
atomic_type(number).
atomic_type(atomic).

denumerable_type(A) :-
        tag_type_test(B,A,C,D),
        \+pointer_tag(B),
        B\==float,
        !.

tag_type(A) :-
        tag_type_test(B,A,C,D).

type(A) :-
        type_test(A,B,C).

type_test(A,B,C) :-
        tag_type_test(D,A,B,C),
        !.
type_test(denumerable,A,denumerable(A)).
type_test(number,A,number(A)).
type_test(float,A,float(A)).
type_test(atomic,A,atomic(A)).
type_test(simple,A,simple(A)).
type_test(compound,A,compound(A)).

term_type(A,B) :-
        term_tag(A,C),
        tag_type_test(C,B,D,E).

term_test(A,B,C,D,D) :-
        term_test(A,B,C).

term_test(A,B,C) :-
        term_tag(A,D),
        tag_type_test(D,E,B,C).

arith_test(A) :-
        arith_test(A,B).

arith_test(A,B) :-
        arith_test(A,C,D,B).

arith_test(A=:=B,A,B,eq).
arith_test(A=\=B,A,B,ne).
arith_test(A<B,A,B,lts).
arith_test(A=<B,A,B,les).
arith_test(A>=B,A,B,ges).
arith_test(A>B,A,B,gts).

arith_test(A,B,C) :-
        arith_test(A,B,C,D).

special_cond(eq).
special_cond(lts).
special_cond(gts).
special_cond(feq).
special_cond(flts).
special_cond(fgts).

cond(A) :-
        cond(A,B).

cond(lts,ges).
cond(les,gts).
cond(gts,les).
cond(ges,lts).
cond(eq,ne).
cond(ne,eq).
cond(flts,fges).
cond(fles,fgts).
cond(fgts,fles).
cond(fges,flts).
cond(feq,fne).
cond(fne,feq).

cond_to_float(lts,flts).
cond_to_float(les,fles).
cond_to_float(gts,fgts).
cond_to_float(ges,fges).
cond_to_float(eq,feq).
cond_to_float(ne,fne).

mutex(A,B,C) :-
        mutex(A,B,C,logical).

implies(A,B,C) :-
        logical_simplify(not(B),D),
        mutex(A,D,C),
        !.

prolog_implies(A,B) :-
        simplify(not(B),C),
        prolog_mutex(A,C,left),
        !.

simplify(A,B) :-
        simp_upv(A,B,prolog),
        !.

prolog_mutex(A,B,C) :-
        mutex(A,B,C,prolog).

prolog_implies(A,B,C) :-
        simplify(not(B),D),
        prolog_mutex(A,D,C),
        !.

mutex(A,B,C,D) :-
        mutex2(A,B,C,D),
        !.

mutex2(fail,A,B,C).
mutex2(A,fail,B,C).
mutex2('$test'(A,0),B,C,D).
mutex2(A,'$test'(B,0),C,D).
mutex2((A->B;C),D,E,F) :-
        mutex2((A ',' B),D,E,F),
        mutex2((\+A ',' C),D,E,F).
mutex2(A,(B->C;D),E,F) :-
        mutex2(A,(B ',' C),E,F),
        mutex2(A,(\+B ',' D),E,F).
mutex2((A->B),C,D,E) :-
        mutex2(A,C,D,E).
mutex2((A->B),C,D,E) :-
        mutex2(B,C,D,E).
mutex2(A,(B->C),D,E) :-
        mutex2(A,B,D,E).
mutex2(A,(B->C),D,E) :-
        mutex2(A,C,D,E).
mutex2((A ',' B),C,D,E) :-
        mutex2(A,C,D,E).
mutex2((A ',' B),C,D,E) :-
        mutex2(B,C,D,E).
mutex2(A,(B ',' C),D,E) :-
        mutex2(A,B,D,E).
mutex2(A,(B ',' C),D,E) :-
        mutex2(A,C,D,E).
mutex2((A;B),C,D,E) :-
        mutex2(A,C,D,E),
        mutex2(B,C,D,E).
mutex2(A,(B;C),D,E) :-
        mutex2(A,B,D,E),
        mutex2(A,C,D,E).
mutex2(A,B,C,logical) :-
        logical_simplify(A,D),
        logical_simplify(B,E),
        (   D==not(E)
        ;   E==not(D)
        ).
mutex2(A,B,C,prolog) :-
        simplify(A,D),
        simplify(B,E),
        (   D==not(E)
        ;   E==not(D)
        ).
mutex2(not((A;B)),C,D,E) :-
        mutex2(not(A),C,D,E).
mutex2(not((A;B)),C,D,E) :-
        mutex2(not(B),C,D,E).
mutex2(A,not((B;C)),D,E) :-
        mutex2(A,not(B),D,E).
mutex2(A,not((B;C)),D,E) :-
        mutex2(A,not(C),D,E).
mutex2(not((A ',' B)),C,D,E) :-
        mutex2(not(A),C,D,E),
        mutex2(not(B),C,D,E).
mutex2(A,not((B ',' C)),D,E) :-
        mutex2(A,not(B),D,E),
        mutex2(A,not(C),D,E).
mutex2(\+ (A;B),C,D,E) :-
        mutex2(\+A,C,D,E).
mutex2(\+ (A;B),C,D,E) :-
        mutex2(\+B,C,D,E).
mutex2(A,\+ (B;C),D,E) :-
        mutex2(A,\+B,D,E).
mutex2(A,\+ (B;C),D,E) :-
        mutex2(A,\+C,D,E).
mutex2(\+ (A ',' B),C,D,E) :-
        mutex2(\+A,C,D,E),
        mutex2(\+B,C,D,E).
mutex2(A,\+ (B ',' C),D,E) :-
        mutex2(A,\+B,D,E),
        mutex2(A,\+C,D,E).
mutex2(A,not(B),C,D) :-
        u_match(A,B).
mutex2(A,not(B),C,D) :-
        u_match(B,A).
mutex2(A,not(B),C,D) :-
        u_implies(A,B).
mutex2(not(A),B,C,D) :-
        u_match(A,B).
mutex2(not(A),B,C,D) :-
        u_match(B,A).
mutex2(not(A),B,C,D) :-
        u_implies(B,A).
mutex2(A,B,C,D) :-
        encode_relop(A,E,F,G),
        encode_relop(B,H,I,J),
        check_relops(E,F,G,H,I,J).
mutex2(A,B,C,D) :-
        encode_test(A,E,F,G,H),
        encode_test(B,I,J,K,L),
        G==K,
        \+can_overlap(C,E,F,I,J,H,L).
mutex2(A,B,C,D) :-
        encode_name_arity(A,E,F,G,H),
        encode_name_arity(B,I,J,K,L),
        E==I,
        check_name_arity(H,L,F,G,J,K).

u_match(trail(A),trail_if_var(B)) :-
        A==B.
u_match(uninit(A),uninit(mem,B)) :-
        A==B.
u_match(uninit(A),uninit(mem,B,C)) :-
        A==B.
u_match(uninit(mem,A),uninit(mem,B,C)) :-
        A==B.
u_match(uninit(reg,A),uninit_reg(B)) :-
        A==B.

u_implies(var(A),unbound(B)) :-
        A==B.
u_implies(rderef(A),deref(B)) :-
        A==B.
u_implies(uninit(either,A),B) :-
        an_uninit_mode(B,C,D),
        A==D.
u_implies(A,B) :-
        an_uninit_mode(A,C,D),
        u_implies_goal(B,E),
        D==E.

encode_relop(A,B,C,D) :-
        encode_relop(A,B,C,D,E).

check_relops(A,B,C,D,E,F) :-
        A==F,
        flip(E,G),
        get_relop(B,G,H),
        test_relop(C,H,D).
check_relops(A,B,C,D,E,F) :-
        A==D,
        get_relop(B,E,G),
        test_relop(C,G,F).
check_relops(A,B,C,D,E,F) :-
        C==F,
        flip(B,G),
        flip(E,H),
        get_relop(G,H,I),
        test_relop(A,I,D).
check_relops(A,B,C,D,E,F) :-
        C==D,
        flip(B,G),
        get_relop(G,E,H),
        test_relop(A,H,F).

encode_test('$test'(A,B),B,C,A,n) :-
        C is\(B)/\511.
encode_test('$name_arity'(A,B,C),128,511,A,n) :-
        atom(B),
        B\==[],
        C==0.
encode_test('$name_arity'(A,B,C),64,447,A,n) :-
        B==[],
        C==0.
encode_test('$name_arity'(A,B,C),32,511,A,n) :-
        nonnegative(B),
        C==0.
encode_test('$name_arity'(A,B,C),16,511,A,n) :-
        negative(B),
        C==0.
encode_test('$name_arity'(A,B,C),8,511,A,n) :-
        number(B),
        \+integer(B),
        C==0,
        B>=0.
encode_test('$name_arity'(A,B,C),4,511,A,n) :-
        number(B),
        \+integer(B),
        C==0,
        B<0.
encode_test('$name_arity'(A,B,C),2,509,A,n) :-
        atom(B),
        integer(C),
        B='.',
        C=:=2.
encode_test('$name_arity'(A,B,C),1,511,A,n) :-
        atom(B),
        integer(C),
        C>0,
        (   B\=='.'
        ;   C=\=2
        ).
encode_test(functor(A,B,C),128,511,A,y) :-
        atom(B),
        B\==[],
        C==0.
encode_test(functor(A,B,C),64,447,A,y) :-
        B==[],
        C==0.
encode_test(functor(A,B,C),32,511,A,y) :-
        nonnegative(B),
        C==0.
encode_test(functor(A,B,C),16,511,A,y) :-
        negative(B),
        C==0.
encode_test(functor(A,B,C),8,511,A,y) :-
        number(B),
        \+integer(B),
        C==0,
        B>=0.
encode_test(functor(A,B,C),4,511,A,y) :-
        number(B),
        \+integer(B),
        C==0,
        B<0.
encode_test(functor(A,B,C),2,509,A,y) :-
        atom(B),
        integer(C),
        B='.',
        C=:=2.
encode_test(functor(A,B,C),1,511,A,y) :-
        atom(B),
        integer(C),
        C>0,
        (   B\=='.'
        ;   C=\=2
        ).
encode_test(functor(A,B,C),32,511,C,y) :-
        atomic(A).
encode_test(functor(A,B,C),32,511,C,y) :-
        compound(A).
encode_test(functor(A,B,C),128,511,B,y) :-
        atom(A),
        A\==[].
encode_test(functor(A,B,C),64,447,B,y) :-
        A==[].
encode_test(functor(A,B,C),32,511,B,y) :-
        nonnegative(A).
encode_test(functor(A,B,C),16,511,B,y) :-
        negative(A).
encode_test(functor(A,B,C),192,511,B,y) :-
        compound(A).
encode_test(functor(A,B,C),252,511,B,y).
encode_test(functor(A,B,C),32,511,C,y).
encode_test(A=..B,255,511,A,y).
encode_test(A=..B,2,511,B,y).
encode_test(var(A),256,255,A,n).
encode_test(nonvar(A),255,256,A,n).
encode_test(ground(A),255,259,A,n).
encode_test(atom(A),192,319,A,n).
encode_test('$atom_nonnil'(A),128,383,A,n).
encode_test(nil(A),64,447,A,n).
encode_test(integer(A),48,463,A,n).
encode_test(negative(A),16,495,A,n).
encode_test(nonnegative(A),32,479,A,n).
encode_test(number(A),60,451,A,n).
encode_test(denumerable(A),240,271,A,n).
encode_test(float(A),12,499,A,n).
encode_test(atomic(A),252,259,A,n).
encode_test(list(A),66,445,A,n).
encode_test(cons(A),2,509,A,n).
encode_test(structure(A),1,510,A,n).
encode_test(compound(A),3,508,A,n).
encode_test(simple(A),508,3,A,n).
encode_test(A is B,60,255,A,y).
encode_test(A,B,C,D,n) :-
        sign_flags(A,B,C,D).
encode_test(A,48,48,B,n) :-
        fail, % \+float,
        \+sign_flags(A,C,D,E),
        arith_test(A,B,F).
encode_test(A,60,60,B,n) :-
        true, % float,
        \+sign_flags(A,C,D,E),
        arith_test(A,B,F).
encode_test(A,48,48,B,n) :-
        fail, % \+float,
        \+sign_flags(A,C,D,E),
        arith_test(A,F,B).
encode_test(A,60,60,B,n) :-
        true, % float,
        \+sign_flags(A,C,D,E),
        arith_test(A,F,B).
encode_test(A=B,C,D,A,y) :-
        type_flags(B,E,D),
        C is 0\/E.
encode_test(A=B,C,D,B,y) :-
        type_flags(A,E,D),
        C is 0\/E.
encode_test(A\=B,C,D,A,n) :-
        type_flags(B,E,C),
        D is 256\/E.
encode_test(A\=B,C,D,B,n) :-
        type_flags(A,E,C),
        D is 256\/E.
encode_test(A==B,C,D,A,n) :-
        type_flags(B,C,E),
        D is 256\/E.
encode_test(A==B,C,D,B,n) :-
        type_flags(A,C,E),
        D is 256\/E.
encode_test(A\==B,C,D,A,n) :-
        type_flags(B,D,E),
        C is 256\/E.
encode_test(A\==B,C,D,B,n) :-
        type_flags(A,D,E),
        C is 256\/E.
encode_test(not(A),B,C,D,E) :-
        nonvar(A),
        encode_test(A,C,B,D,E).
encode_test(\+A,B,C,D,n) :-
        nonvar(A),
        encode_test(A,C,B,D,E).

can_overlap(A,B,C,D,E,F,G) :-
        0=\=B/\D.
can_overlap(left,A,B,C,D,E,y) :-
        0=\=256/\A.
can_overlap(right,A,B,C,D,y,E) :-
        0=\=256/\C.
can_overlap(before,A,B,C,D,E,y) :-
        0=\=256/\A.
can_overlap(before,A,B,C,D,y,E) :-
        0=\=256/\C.

encode_name_arity('$test'(A,B),C,D,E,true) :-
        bitmap_name_arity(B,C,D,E).
encode_name_arity(A=..B,B,'.',2,true).
encode_name_arity(A=B,A,C,D,true) :-
        nonvar(B),
        functor(B,C,D).
encode_name_arity(A=B,B,C,D,true) :-
        nonvar(A),
        functor(A,C,D).
encode_name_arity(A==B,A,C,D,true) :-
        nonvar(B),
        functor(B,C,D).
encode_name_arity(A==B,B,C,D,true) :-
        nonvar(A),
        functor(A,C,D).
encode_name_arity(A=:=B,A,C,D,true) :-
        number(B),
        functor(B,C,D).
encode_name_arity(A=:=B,B,C,D,true) :-
        number(A),
        functor(A,C,D).
encode_name_arity(A=\=B,A,not(C),not(D),false) :-
        number(B),
        functor(B,C,D).
encode_name_arity(A=\=B,B,not(C),not(D),false) :-
        number(A),
        functor(A,C,D).
encode_name_arity(A\==B,A,not(C),not(D),false) :-
        atomic(B),
        functor(B,C,D).
encode_name_arity(A\==B,B,not(C),not(D),false) :-
        atomic(A),
        functor(A,C,D).
encode_name_arity(functor(A,B,C),A,B,C,true).
encode_name_arity(functor(A,B,C),B,B,0,true) :-
        var(A).
encode_name_arity(functor(A,B,C),B,D,0,true) :-
        nonvar(A),
        functor(A,D,E).
encode_name_arity(functor(A,B,C),C,C,0,true) :-
        var(A).
encode_name_arity(functor(A,B,C),C,D,0,true) :-
        nonvar(A),
        functor(A,E,D).
encode_name_arity('$name_arity'(A,B,C),A,B,C,true).
encode_name_arity(atom(A),A,A,0,true).
encode_name_arity('$atom_nonnil'(A),A,A,0,true).
encode_name_arity(nil(A),A,[],0,true).
encode_name_arity(integer(A),A,A,0,true).
encode_name_arity(negative(A),A,A,0,true).
encode_name_arity(nonnegative(A),A,A,0,true).
encode_name_arity(float(A),A,A,0,true).
encode_name_arity(number(A),A,A,0,true).
encode_name_arity(atomic(A),A,A,0,true).
encode_name_arity(cons(A),A,'.',2,true).
encode_name_arity(not(A),B,C,D,E) :-
        nonvar(A),
        \+invalid_negation(A),
        encode_name_arity(A,B,F,G,H),
        negate(F,C),
        negate(G,D),
        negate_boolean(H,E).

check_name_arity(true,true,A,B,C,D) :-
        (   check_different(A,C)
        ;   check_different(B,D)
        ),
        !.
check_name_arity(true,false,A,B,C,D) :-
        check_different(A,C),
        check_different(B,D).
check_name_arity(false,true,A,B,C,D) :-
        check_different(A,C),
        check_different(B,D).

an_uninit_mode(uninit(A),mem,A).
an_uninit_mode(uninit_reg(A),reg,A).
an_uninit_mode(uninit(A,B),A,B) :-
        mem_reg(A).
an_uninit_mode(uninit(A,B,C),A,B) :-
        mem_reg(A).

u_implies_goal(unbound(A),A).
u_implies_goal(uninit(any,A),A).
u_implies_goal(deref(A),A).
u_implies_goal(rderef(A),A).
u_implies_goal(trail(A),A).
u_implies_goal(trail_if_var(A),A).

encode_test(A,B,C,D) :-
        encode_test(A,B,C,D,E).

test_nobind(A) :-
        test(A),
        \+bind_test(A).

bind_test(A=B).
bind_test(A is B).
bind_test(functor(A,B,C)).
bind_test(A=..B).

test_varset(A,B) :-
        test_varbag(A,C),
        sort(C,B).

test_varbag(A,B) :-
        test_varbag(A,B,[]).

test_varbag(A,B,C) :-
        encode_relop(A,D,E,F,G),
        filter_vars([D,F],B,C),
        !.
test_varbag(A,B,C) :-
        encode_test(A,D,E,F,G),
        filter_vars([F],B,C),
        !.
test_varbag(A,B,C) :-
        encode_name_arity(A,D,E,F,G),
        filter_vars([D],B,C),
        !.

encode_relop('$name_arity'(A,B,C),A,2,B,unify) :-
        atomic(B),
        C==0.
encode_relop(functor(A,B,C),A,2,B,unify) :-
        number(B),
        C==0.
encode_relop(functor(A,B,C),A,2,B,unify) :-
        atom(B),
        C==0.
encode_relop(functor(A,B,C),A,2,B,unify) :-
        var(B),
        C==0.
encode_relop(functor(A,B,C),C,2,D,unify) :-
        nonvar(A),
        functor(A,E,D).
encode_relop(functor(A,B,C),B,2,D,unify) :-
        nonvar(A),
        functor(A,D,E).
encode_relop(A>B,A,1,B,arith).
encode_relop(A@>B,A,1,B,stand).
encode_relop(A=:=B,A,2,B,arith).
encode_relop(A==B,A,2,B,stand).
encode_relop(A=B,A,2,B,unify).
encode_relop(A is B,A,2,B,arith).
encode_relop(A>=B,A,3,B,arith).
encode_relop(A@>=B,A,3,B,stand).
encode_relop(A<B,A,4,B,arith).
encode_relop(A@<B,A,4,B,stand).
encode_relop(A=\=B,A,5,B,arith).
encode_relop(A\==B,A,5,B,stand).
encode_relop(A\=B,A,5,B,unify).
encode_relop(A=<B,A,6,B,arith).
encode_relop(A@=<B,A,6,B,stand).
encode_relop(not(A),B,C,D,E) :-
        nonvar(A),
        encode_relop(A,B,F,D,E),
        negate_relop(F,C).
encode_relop(\+A,B,C,D,E) :-
        nonvar(A),
        encode_relop(A,B,F,D,E),
        negate_relop(F,C).

relational_test(A,B,C) :-
        encode_relop(A,B,D,C,arith),
        !.

atomic_value((A ',' B),C,D) :-
        atomic_value(A,C,D).
atomic_value((A ',' B),C,D) :-
        atomic_value(B,C,D).
atomic_value(A,B,C) :-
        encode_name_arity(A,D,E,F,true),
        B==D,
        F=0,
        atomic(E),
        C=E.

standard_order(A) :-
        encode_relop(A,B,C,D,stand).

negate_relop(A,B) :-
        B is\(A)/\7.

flip(A,B) :-
        B is(A/\1)<<2\/ (A/\2)\/(A/\4)>>2.

get_relop(1,1,0).
get_relop(1,2,3).
get_relop(1,3,0).
get_relop(1,4,3).
get_relop(1,5,0).
get_relop(1,6,3).
get_relop(2,1,6).
get_relop(2,2,5).
get_relop(2,3,4).
get_relop(2,4,3).
get_relop(2,5,2).
get_relop(2,6,1).
get_relop(3,1,0).
get_relop(3,2,1).
get_relop(3,3,0).
get_relop(3,4,3).
get_relop(3,5,0).
get_relop(3,6,1).
get_relop(4,1,6).
get_relop(4,2,6).
get_relop(4,3,6).
get_relop(4,4,0).
get_relop(4,5,0).
get_relop(4,6,0).
get_relop(5,1,0).
get_relop(5,2,2).
get_relop(5,3,0).
get_relop(5,4,0).
get_relop(5,5,0).
get_relop(5,6,0).
get_relop(6,1,6).
get_relop(6,2,4).
get_relop(6,3,4).
get_relop(6,4,0).
get_relop(6,5,0).
get_relop(6,6,0).

test_relop(A,B,C) :-
        strong_compare(A,D,C),
        0 is\(B)/\7/\D.

bagof_get_relop(A,B,C) :-
        bagof(D,op_table(A,B,D),E),
        !,
        and_list(E,7,C).
bagof_get_relop(A,B,0).

and_list([],A,A).
and_list([A|B],C,D) :-
        E is A/\C,
        and_list(B,E,D).

op_table(A,B,0) :-
        lt(A),
        lt(B).
op_table(A,B,6) :-
        lt(A),
        eq(B).
op_table(A,B,6) :-
        lt(A),
        gt(B).
op_table(A,B,3) :-
        eq(A),
        lt(B).
op_table(A,B,5) :-
        eq(A),
        eq(B).
op_table(A,B,6) :-
        eq(A),
        gt(B).
op_table(A,B,3) :-
        gt(A),
        lt(B).
op_table(A,B,3) :-
        gt(A),
        eq(B).
op_table(A,B,0) :-
        gt(A),
        gt(B).

lt(A) :-
        4 is 4/\A.

eq(A) :-
        2 is 2/\A.

gt(A) :-
        1 is 1/\A.

strong_compare(A,2,B) :-
        A==B,
        !.
strong_compare(A,7,B) :-
        A\==B,
        var(A),
        !.
strong_compare(A,7,B) :-
        A\==B,
        var(B),
        !.
strong_compare(A,B,C) :-
        A\==C,
        nonvar(A),
        nonvar(C),
        !,
        functor(A,D,E),
        functor(C,F,G),
        weak_compare(E/D,H,G/F),
        (   H=2 ->
            strong_compare_args(1,E,A,B,C)
        ;   B=H
        ).

weak_compare(A,4,B) :-
        A@<B.
weak_compare(A,2,B) :-
        A==B.
weak_compare(A,1,B) :-
        A@>B.

strong_compare_args(A,B,C,2,D) :-
        A>B,
        !.
strong_compare_args(A,B,C,D,E) :-
        A=<B,
        !,
        arg(A,C,F),
        arg(A,E,G),
        strong_compare(F,H,G),
        (   H=7 ->
            D=H
        ;   H=2 ->
            I is A+1,
            strong_compare_args(I,B,C,D,E)
        ;   D=H
        ).

get_type_test(A,B,'$test'(B,C)) :-
        get_type(A,B,511,C).

get_type((A ',' B),C,D,E) :- !,
        get_type(A,C,D,F),
        get_type(B,C,F,E).
get_type(A,B,C,D) :-
        encode_test(A,E,F,G,H),
        B==G,
        !,
        and_bits(E,C,D).
get_type(A,B,C,C).

get_tag(A,B,C) :-
        get_type(A,B,511,D),
        bitmap_type(D,E),
        tag(E,C).

bitmap_type(256,var).
bitmap_type(2,cons).
bitmap_type(1,structure).
bitmap_type(192,atom).
bitmap_type(64,atom).
bitmap_type(128,atom).
bitmap_type(16,negative) :-
        fail. % split_integer.
bitmap_type(32,nonnegative) :-
        fail. % split_integer.
bitmap_type(48,integer) :-
        true. % \+split_integer.
bitmap_type(16,integer) :-
        true. % \+split_integer.
bitmap_type(32,integer) :-
        true. % \+split_integer.
bitmap_type(12,float) :-
        true. % float.

and_bits(A,B,C) :-
        C is A/\B.

conj_test(A,B,'$test'(C,D)) :-
        encode_test(A,E,F,C,G),
        encode_test(B,H,I,J,K),
        C==J,
        !,
        D is E/\H.

disj_test(A,B,'$test'(C,D)) :-
        encode_test(A,E,F,C,G),
        encode_test(B,H,I,J,K),
        C==J,
        !,
        D is E\/H.

not_test(A,'$test'(B,C)) :-
        encode_test(A,D,C,B,E),
        !.

bitmap_test(A,'$test'(B,C)) :-
        encode_test(A,C,D,B,E),
        !.

merge_test(A,'$test'(B,C)) :-
        exact_bitmap(A,B,C),
        !.
merge_test((A ',' B),C) :-
        merge_test(A,D),
        merge_test(B,E),
        !,
        conj_test(D,E,C).
merge_test((A;B),C) :-
        merge_test(A,D),
        merge_test(B,E),
        !,
        disj_test(D,E,C).
merge_test(\+A,B) :-
        merge_test(A,C),
        !,
        not_test(C,B).

exact_bitmap(nonvar(A),A,255).
exact_bitmap(atom(A),A,192).
exact_bitmap(var(A),A,256).
exact_bitmap(cons(A),A,2).
exact_bitmap(structure(A),A,1).
exact_bitmap(nil(A),A,64).
exact_bitmap(A==B,A,64) :-
        B==[].
exact_bitmap(A==B,B,64) :-
        A==[].
exact_bitmap(negative(A),A,16).
exact_bitmap(nonnegative(A),A,32).
exact_bitmap(denumerable(A),A,240).
exact_bitmap(float(A),A,12).
exact_bitmap(simple(A),A,508).
exact_bitmap(compound(A),A,3).
exact_bitmap(list(A),A,66).
exact_bitmap(atomic(A),A,252).
exact_bitmap(number(A),A,60).
exact_bitmap(integer(A),A,48).
exact_bitmap('$atom_nonnil'(A),A,128).

bitmap_simplify('$test'(A,0),fail).
bitmap_simplify('$test'(A,511),true).

bitmap_combine('$test'(A,B),C,D) :-
        bitmap_combine(C,A,B,D).

bitmap_combine('$test'(A,B),C,D,'$test'(A,E)) :-
        C==A,
        !,
        E is B/\D.
bitmap_combine((A ',' B),C,D,(E ',' B)) :-
        bitmap_combine(A,C,D,E),
        !.
bitmap_combine((A ',' B),C,D,(A ',' E)) :-
        bitmap_combine(B,C,D,E),
        !.

bitmap_name_arity(2,A,'.',2) :- !.
bitmap_name_arity(64,A,[],0) :- !.
bitmap_name_arity(A,B,B,0) :-
        A=\=0,
        A/\259=:=0.

i2f_formula((A ',' B),C,(D ',' E)) :- !,
        i2f_formula(A,C,D),
        i2f_formula(B,C,E).
i2f_formula((A;B),C,(D;E)) :- !,
        i2f_formula(A,C,D),
        i2f_formula(B,C,E).
i2f_formula(not(A),B,not(C)) :- !,
        f2i_formula(A,B,C).
i2f_formula('$test'(A,B),C,'$test'(A,D)) :-
        C==A,
        !,
        (   B/\48=\=0 ->
            E is B/\463,
            D is E\/12
        ;   D=B
        ).
i2f_formula(integer(A),B,float(A)) :-
        B==A,
        !.
i2f_formula(nonnegative(A),B,float(A)) :-
        B==A,
        !.
i2f_formula(negative(A),B,float(A)) :-
        B==A,
        !.
i2f_formula(denumerable(A),B,atomic(A)) :-
        B==A,
        !.
i2f_formula(A=B,C,A=D) :-
        C==A,
        integer(B),
        !,
        D is float(B).
i2f_formula(A=B,C,B=D) :-
        C==B,
        integer(A),
        !,
        D is float(A).
i2f_formula(A==B,C,A==D) :-
        C==A,
        integer(B),
        !,
        D is float(B).
i2f_formula(A==B,C,B==D) :-
        C==B,
        integer(A),
        !,
        D is float(A).
i2f_formula(A=:=B,C,A==D) :-
        C==A,
        integer(B),
        !,
        D is float(B).
i2f_formula(A=:=B,C,B==D) :-
        C==B,
        integer(A),
        !,
        D is float(A).
i2f_formula(A,B,A).

f2i_formula((A ',' B),C,(D ',' E)) :- !,
        f2i_formula(A,C,D),
        f2i_formula(B,C,E).
f2i_formula((A;B),C,(D;E)) :- !,
        f2i_formula(A,C,D),
        f2i_formula(B,C,E).
f2i_formula(not(A),B,not(C)) :- !,
        i2f_formula(A,B,C).
f2i_formula('$test'(A,B),C,'$test'(A,D)) :-
        C==A,
        !,
        (   B/\12=\=0 ->
            E is B/\499,
            D is E\/48
        ;   D=B
        ).
f2i_formula(float(A),B,integer(A)) :-
        B==A,
        !.
f2i_formula(atomic(A),B,denumerable(A)) :-
        B==A,
        !.
f2i_formula(A=B,C,A=D) :-
        C==A,
        !,
        D is integer(B).
f2i_formula(A=B,C,B=D) :-
        C==B,
        !,
        D is integer(A).
f2i_formula(A==B,C,A==D) :-
        C==A,
        !,
        D is integer(B).
f2i_formula(A==B,C,B==D) :-
        C==B,
        !,
        D is integer(A).
f2i_formula(A=:=B,C,A==D) :-
        C==A,
        !,
        D is integer(B).
f2i_formula(A=:=B,C,B==D) :-
        C==B,
        !,
        D is integer(A).
f2i_formula(A,B,A).

test_to_disj('$test'(A,B),A,C) :- !,
        bitmap_to_disj(A,B,C).
test_to_disj(A,B,C) :-
        exact_bitmap(A,B,D),
        bitmap_to_disj(B,D,C).

bitmap_to_disj(A,B,C) :-
        bitmap_to_disj(B,B,A,C,fail).

bitmap_to_disj(0,A,B,C,C) :- !.
bitmap_to_disj(A,B,C,D,E) :-
        A=\=0,
        exact_bitmap(F,C,G),
        \+complex_bitmap(F),
        0=:=G/\ \(B),
        H is A/\ \(G),
        H=\=A,
        !,
        di(F,D,I),
        bitmap_to_disj(H,B,C,I,E).
bitmap_to_disj(A,B,C,D,D) :-
        error(['Could not convert bitmap ''',A,''' to a disjunction.',nl,'Replacing by fail.']).

complex_bitmap(simple(A)).
complex_bitmap(compound(A)).
complex_bitmap(list(A)).
complex_bitmap(atomic(A)).
complex_bitmap(number(A)).
complex_bitmap(denumerable(A)).
complex_bitmap(integer(A)) :-
        fail. % split_integer.
complex_bitmap(negative(A)) :-
        true. % \+split_integer.
complex_bitmap(nonnegative(A)) :-
        true. % \+split_integer.

exact_bitmap(A) :-
        exact_bitmap(A,B,C).

exact_bitmap(A,B) :-
        exact_bitmap(A,C,B).

ground_bag((A;B),C,D) :- !,
        ground_set(A,E),
        ground_set(B,F),
        intersectv(E,F,G),
        difflist(G,C,D).
ground_bag((A ',' B),C,D) :- !,
        ground_bag(A,C,E),
        ground_bag(B,E,D).
ground_bag(A,B,C) :-
        encode_relop(A,D,E,F,arith),
        !,
        constC(B,D,G),
        constC(G,F,C).
ground_bag(A,B,C) :-
        encode_test(A,D,E,F,G),
        0=:=D/\259,
        !,
        constC(B,F,C).
ground_bag(ground(A),B,C) :- !,
        constC(B,A,C).
ground_bag(A,B,B).

nonvar_set(A,B) :-
        nonvar_bag(A,C,[]),
        sort(C,D),
        filter_vars(D,B).

nonvar_bag((A;B),C,D) :- !,
        nonvar_set(A,E),
        nonvar_set(B,F),
        intersectv(E,F,G),
        difflist(G,C,D).
nonvar_bag((A ',' B),C,D) :- !,
        nonvar_bag(A,C,E),
        nonvar_bag(B,E,D).
nonvar_bag(A,B,C) :-
        encode_test(A,D,E,F,G),
        0=:=D/\256,
        !,
        constC(B,F,C).
nonvar_bag(A,B,B).

deref_set(A,B) :-
        deref_bag(A,C,[]),
        sort(C,D),
        filter_vars(D,B).

deref_bag((A;B),C,D) :- !,
        deref_set(A,E),
        deref_set(B,F),
        intersectv(E,F,G),
        difflist(G,C,D).
deref_bag((A ',' B),C,D) :- !,
        deref_bag(A,C,E),
        deref_bag(B,E,D).
deref_bag(rderef(A),B,C) :- !,
        constC(B,A,C).
deref_bag(deref(A),B,C) :- !,
        constC(B,A,C).
deref_bag(A,B,B).

rderef_set(A,B) :-
        rderef_bag(A,C,[]),
        sort(C,D),
        filter_vars(D,E),
        simple_set(A,F),
        deref_set(A,G),
        intersectv(F,G,H),
        unionv(H,E,B).

rderef_bag((A;B),C,D) :- !,
        rderef_set(A,E),
        rderef_set(B,F),
        intersectv(E,F,G),
        difflist(G,C,D).
rderef_bag((A ',' B),C,D) :- !,
        rderef_bag(A,C,E),
        rderef_bag(B,E,D).
rderef_bag(rderef(A),B,C) :- !,
        constC(B,A,C).
rderef_bag(A,B,B).

simple_set(A,B) :-
        simple_bag(A,C,[]),
        sort(C,D),
        filter_vars(D,B).

atomic_set(A,B) :-
        atomic_bag(A,C,[]),
        sort(C,D),
        filter_vars(D,B).

atomic_bag((A;B),C,D) :- !,
        atomic_set(A,E),
        atomic_set(B,F),
        intersectv(E,F,G),
        difflist(G,C,D).
atomic_bag((A ',' B),C,D) :- !,
        atomic_bag(A,C,E),
        atomic_bag(B,E,D).
atomic_bag(A,B,C) :-
        encode_relop(A,D,E,F,arith),
        !,
        constC(B,D,G),
        constC(G,F,C).
atomic_bag(A,B,C) :-
        encode_test(A,D,E,F,G),
        0=:=D/\259,
        !,
        constC(B,F,C).
atomic_bag(A,B,B).

simple_bag((A;B),C,D) :- !,
        simple_set(A,E),
        simple_set(B,F),
        intersectv(E,F,G),
        difflist(G,C,D).
simple_bag((A ',' B),C,D) :- !,
        simple_bag(A,C,E),
        simple_bag(B,E,D).
simple_bag(A,B,C) :-
        encode_relop(A,D,E,F,arith),
        !,
        constC(B,D,G),
        constC(G,F,C).
simple_bag(A,B,C) :-
        encode_test(A,D,E,F,G),
        0=:=D/\3,
        !,
        constC(B,F,C).
simple_bag(A,B,B).

subsuming_test(A,B,C,true) :-
        encode_test(A,D,E,F,n),
        type_test(C,F,B),
        encode_test(B,G,H,F,n),
        0=:= \(G)/\D.
subsuming_test(A,not(B),C,false) :-
        encode_test(A,D,E,F,n),
        type_test(C,F,B),
        encode_test(B,G,H,F,n),
        0=:= \(H)/\D.

sign_flags(A>B,C,D,A) :-
        number(B),
        B>=0,
        sign_bits(C,D).
sign_flags(A>=B,C,D,A) :-
        number(B),
        B>=0,
        sign_bits(C,D).
sign_flags(A<B,C,D,B) :-
        number(A),
        A>=0,
        sign_bits(C,D).
sign_flags(A=<B,C,D,B) :-
        number(A),
        A>=0,
        sign_bits(C,D).
sign_flags(A>B,C,D,B) :-
        number(A),
        A=<0,
        sign_bits(D,C).
sign_flags(A>=B,C,D,B) :-
        number(A),
        A<0,
        sign_bits(D,C).
sign_flags(A<B,C,D,A) :-
        number(B),
        B=<0,
        sign_bits(D,C).
sign_flags(A=<B,C,D,A) :-
        number(B),
        B<0,
        sign_bits(D,C).
sign_flags(A=:=B,C,D,B) :-
        number(A),
        A>=0,
        sign_bits(C,D).
sign_flags(A=:=B,C,D,A) :-
        number(B),
        B>=0,
        sign_bits(C,D).
sign_flags(A=:=B,C,D,B) :-
        number(A),
        A<0,
        sign_bits(D,C).
sign_flags(A=:=B,C,D,A) :-
        number(B),
        B<0,
        sign_bits(D,C).

type_flags(A,128,255) :-
        atom(A),
        A\==[].
type_flags(A,64,191) :-
        nil(A).
type_flags(A,32,255) :-
        nonnegative(A).
type_flags(A,16,255) :-
        negative(A).
type_flags(A,8,255) :-
        float(A),
        A>=0.
type_flags(A,4,255) :-
        float(A),
        A<0.
type_flags(A,2,255) :-
        cons(A).
type_flags(A,1,255) :-
        structure(A).

sign_bits(A,B) :-
        (   true -> % float ->
            A=40,
            B=20
        ;   A=32,
            B=16
        ).

left_overlap(A,B,C,D) :-
        0=\=A/\ (C\/ \(C\/D)).

right_overlap(A,B,C,D) :-
        0=\=C/\ (A\/ \(A\/B)).

invalid_negation(A=..B).
invalid_negation(A=B).
invalid_negation(A\=B).
invalid_negation(A==B) :-
        \+one_atomic(A,B).
invalid_negation(A\==B) :-
        \+one_atomic(A,B).

negate(A,B) :-
        nonvar(A),
        A=not(B),
        !.
negate(A,not(A)) :-
        var(A),
        !.
negate(A,not(A)) :-
        \+A=not(B),
        !.

negate_boolean(true,false).
negate_boolean(false,true).

one_atomic(A,B) :-
        atomic(A).
one_atomic(A,B) :-
        atomic(B).

check_different(A,B) :-
        atomic(A),
        atomic(B),
        A\==B.
check_different(A,B) :-
        nonvar(A),
        A=not(C),
        atomic(C),
        C==B.
check_different(A,B) :-
        nonvar(B),
        B=not(C),
        atomic(C),
        A==C.

ctest(A,B,C,C) :-
        ctest(A,B).

ctest(A,B) :-
        prolog_implies(B,A).

ctest(A,B,B) :-
        ctest(A,B).

nonvartest(A,B) :-
        ctest(nonvar(A),B).

vartest(A,B) :-
        ctest(var(A),B).

rtest_in(A,B,C,D,E,F) :-
        rtest_in(A,B,C,G,D,E,F).

rtest_in(A,B,C,C,D,E,F) :-
        core_rtest(A,B,D,C,G,no_update,E,F).

core_rtest(A,B,C,D,E,F,G,H) :-
        \+prolog_implies(D,A),
        expand_test(A,I,B,D,C,G,H),
        var(I),
        !,
        update_choice(F,A,D,E).
core_rtest(A,B,C,D,D,E,F,F).

rtests_in(A,B,C,C,D,E,F) :-
        \+A= (G ',' H),
        !,
        rtest_in(A,B,C,D,E,F).
rtests_in((A ',' B),C,D,D,E,F,G) :- !,
        rtest_in(A,C,D,E,F,H),
        rtests_in(B,C,D,I,E,H,G).

rtest(A,B,C,D,E,F,G) :-
        core_rtest(A,B,E,C,D,update,F,G).

rtest_in_deref(A,B,[pref,A,B|C],C,D,D,E,F) :-
        write_once,
        var(A),
        \+prolog_implies(D,deref(A)),
        !,
        constC(E,deref(A,B),F).
rtest_in_deref(A,A,[A|B],B,C,C,D,E) :-
        \+write_once,
        !,
        constC(D,deref(A),E).
rtest_in_deref(A,A,B,B,C,C,D,D).

rtest_deref(A,B,[pref,A,B|C],C,D,E,F,G) :-
        write_once,
        var(A),
        \+prolog_implies(D,deref(A)),
        !,
        constC(F,deref(A,B),G),
        update_formula(deref(B),D,E).
rtest_deref(A,A,[A|B],B,C,D,E,F) :-
        \+write_once,
        !,
        constC(E,deref(A),F),
        update_formula(deref(A),C,D).
rtest_deref(A,A,B,B,C,C,D,D).

update_formula(A,B,C) :-
        xupdate_formula(A,prolog,B,yes,D,C).

rtests(A,B,C,D,E,F,G) :-
        \+A= (H ',' I),
        !,
        rtest(A,B,C,D,E,F,G).
rtests((A ',' B),C,D,E,F,G,H) :- !,
        rtest(A,C,D,I,F,G,J),
        rtests(B,C,I,E,F,J,H).

rtest_1(true,A,B,B,C,C) :- !.
rtest_1(A,B,C,D,E,F) :-
        arg(1,A,G),
        rtest(A,G,C,D,B,E,F).

rtests_1(A,B,C,D,E,F) :-
        \+A= (G ',' H),
        !,
        rtest_1(A,B,C,D,E,F).
rtests_1((A ',' B),C,D,E,F,G) :- !,
        rtest_1(A,C,D,H,F,I),
        rtests_1(B,C,H,E,I,G).

expand_test(deref(A),A,B,C,D,E,F) :- !,
        constC(E,deref(B,B),F).
expand_test(trail(A),A,B,C,D,E,F) :- !,
        constC(E,trail(B),F).
expand_test(trail_if_var(A),A,B,C,D,E,F) :- !,
        tag(var,G),
        constC(E,test(ne,G,B,H),I),
        constC(I,trail(B),J),
        constC(J,label(H),F).
expand_test(nil(A),A,B,C,D,E,F) :- !,
        constC(E,equal(B,G^[],D),F),
        tag(atom,G).
expand_test(A==B,A,C,D,E,F,G) :-
        atomic(B),
        !,
        atomic_word(B,H),
        constC(F,equal(C,H,E),G).
expand_test(A==B,B,C,D,E,F,G) :-
        atomic(A),
        !,
        atomic_word(A,H),
        constC(F,equal(C,H,E),G).
expand_test(negative(A),A,B,C,D,E,F) :-
        true, % \+split_integer,
        !,
        expand_test(integer(A),A,B,G,D,E,H),
        arith_test(I>=J,K),
        constC(H,jump(K,B,0,D),F).
expand_test(nonnegative(A),A,B,C,D,E,F) :-
        true, % \+split_integer,
        !,
        expand_test(integer(A),A,B,G,D,E,H),
        arith_test(I<J,K),
        constC(H,jump(K,B,0,D),F).
expand_test(A,B,C,D,E,F,G) :-
        test_tag(A,B,H),
        !,
        constC(F,test(ne,H,C,E),G).
expand_test('$atom_nonnil'(A),A,B,C,D,E,F) :- !,
        tag(atom,G),
        constC(E,test(ne,G,B,D),H),
        constC(H,equal(B,G^[],I),J),
        constC(J,jump(D),K),
        constC(K,label(I),F).
expand_test(float(A),A,B,C,D,E,F) :-
        \+float(A),
        !,
        constC(E,jump(D),F).
expand_test(nonvar(A),A,B,C,D,E,F) :- !,
        constC(E,test(eq,G,B,D),F),
        tag(var,G).
expand_test(functor(A,B,C),A,D,E,F,G,H) :-
        atom(B),
        integer(C),
        B=='.',
        C==2,
        ctest(nonvar(A),E,G,I),
        !,
        constC(I,test(ne,J,D,F),H),
        tag(cons,J).
expand_test(functor(A,B,C),A,D,E,F,G,H) :-
        atom(B),
        integer(C),
        C>0,
        (   B\=='.'
        ;   C\==2
        ),
        ctest(nonvar(A),E,G,I),
        !,
        tag(atom,J,I,K),
        test_one_tag(structure(A),D,E,F,K,L),
        tag(structure,M),
        constC(L,pragma(tag(D,M)),N),
        O=1, % align(O),
        constC(N,pragma(align(D,O)),P),
        constC(P,equal([D],J^ (B/C),F),H).
expand_test(functor(A,B,C),A,D,E,F,G,H) :-
        atom(B),
        integer(C),
        C==0,
        ctest(nonvar(A),E,G,I),
        !,
        tag(atom,J,I,K),
        constC(K,equal(D,J^B,F),H).
expand_test(functor(A,B,C),A,D,E,F,G,H) :-
        number(B),
        integer(C),
        C==0,
        ctest(nonvar(A),E,G,I),
        !,
        constC(I,equal(D,B,F),H).
expand_test('$name_arity'(A,B,C),A,D,E,F,G,H) :-
        atom(B),
        integer(C),
        B=='.',
        C==2,
        !,
        constC(G,test(ne,I,D,F),H),
        tag(cons,I).
expand_test('$name_arity'(A,B,C),A,D,E,F,G,H) :-
        atom(B),
        integer(C),
        C>0,
        (   B\=='.'
        ;   C\==2
        ),
        !,
        tag(atom,I,G,J),
        test_one_tag(structure(A),D,E,F,J,K),
        tag(structure,L),
        constC(K,pragma(tag(D,L)),M),
        N=1, % align(N),
        constC(M,pragma(align(D,N)),O),
        constC(O,equal([D],I^ (B/C),F),H).
expand_test('$name_arity'(A,B,C),A,D,E,F,G,H) :-
        atom(B),
        integer(C),
        C==0,
        !,
        tag(atom,I,G,J),
        constC(J,equal(D,I^B,F),H).
expand_test('$name_arity'(A,B,C),A,D,E,F,G,H) :-
        number(B),
        integer(C),
        C==0,
        !,
        constC(G,equal(D,B,F),H).
expand_test(A,B,C,D,E,F,G) :-
        test_to_disj(A,B,H),
        expand_test_disj(H,C,D,E,F,G).

update_choice(no_update,A,B,B) :- !.
update_choice(update,A,B,C) :-
        update_formula(A,B,C).

rtest_m(A,B,C,D,E,F) :-
        rtest_m_nf(A,B,C,E,F),
        update_formula(A,C,D).

rtest_m_nf(A,'$varlist'([]),fail,B,C) :- !,
        constC(B,fail,C).
rtest_m_nf(A,B,C,D,E) :-
        expand(A,A,B,C,F,D,E).

rtests_m(A,B,C,D,E,F) :-
        rtests_m_nf(A,B,C,E,F),
        update_formula(A,C,D).

rtests_m_nf((A ',' B),(C ',' D),E,F,G) :- !,
        rtest_m_nf(A,C,E,F,H),
        rtests_m_nf(B,D,E,H,G).
rtests_m_nf(A,B,C,D,E) :-
        rtest_m_nf(A,B,C,D,E).

expand(true,true,true,A,A,B,B) :- !,
        true.
expand(A,'$equal'(B,C),'$equal'(B,C),D,D,E,F) :- !,
        E=[equal(B,C,fail)|F],
        true.
expand(A,B,'$varlist'([C]),D,D,E,F) :-
        arg(1,B,C),
        expand_test(A,C,D,E,F),
        !,
        true.
expand(repeat,repeat,'$varlist'([]),A,A,B,C) :- !,
        B=[choice(1/2,[],D)|E],
        E=[label(D)|C],
        true.
expand(A,fail,fail,B,B,C,D) :- !,
        C=[fail|D],
        true.
expand(A==B,C==D,'$varlist'([C,D]),E,E,F,G) :-
        implies(E,(simple(A);simple(B))),
        !,
        F=[equal(C,D,fail)|G],
        true.
expand(A\==B,C\==D,'$varlist'([C,D]),E,E,F,G) :-
        implies(E,(simple(A);simple(B))),
        !,
        F=[equal(C,D,H)|I],
        I=[fail|J],
        J=[label(H)|G],
        true.
expand(A,'$cut_load'(B),'$varlist'([B]),C,C,D,E) :- !,
        D=[move(r(b),B)|E],
        true.
expand(A,'$cut_deep'(B),'$varlist'([B]),C,C,D,E) :- !,
        D=[cut(B)|E],
        true.
expand(A,'$cut_shallow'(B),'$varlist'([B]),C,C,D,E) :- !,
        D=[cut(B)|E],
        true.
expand('$and'(A,B,C),'$and'(D,E,F),'$varlist'(G),H,I,J,K) :- !,
        expand_integer(and,A,B,C,D,E,F,G,[],H,I,J,K),
        true.
expand('$or'(A,B,C),'$or'(D,E,F),'$varlist'(G),H,I,J,K) :- !,
        expand_integer(or,A,B,C,D,E,F,G,[],H,I,J,K),
        true.
expand('$xor'(A,B,C),'$xor'(D,E,F),'$varlist'(G),H,I,J,K) :- !,
        expand_integer(xor,A,B,C,D,E,F,G,[],H,I,J,K),
        true.
expand('$sll'(A,B,C),'$sll'(D,E,F),'$varlist'(G),H,I,J,K) :- !,
        expand_integer(sll,A,B,C,D,E,F,G,[],H,I,J,K),
        true.
expand('$sra'(A,B,C),'$sra'(D,E,F),'$varlist'(G),H,I,J,K) :- !,
        expand_integer(sra,A,B,C,D,E,F,G,[],H,I,J,K),
        true.
expand('$not'(A,B),'$not'(C,D),'$varlist'(E),F,G,H,I) :- !,
        expand_integer(not,A,B,C,D,E,[],F,G,H,I),
        true.
expand('$idiv'(A,B,C),'$idiv'(D,E,F),'$varlist'(G),H,I,J,K) :-
        expand_integer(div,A,B,C,D,E,F,G,[],H,I,J,K),
        true.
expand('$mod'(A,B,C),'$mod'(D,E,F),'$varlist'(G),H,I,J,K) :-
        expand_integer(mod,A,B,C,D,E,F,G,[],H,I,J,K),
        true.
expand(A,B,'$varlist'(C),D,E,F,G) :-
        arith_test(A,H,I,J),
        !,
        arith_test(B,K,L,M),
        expand_cmp(J,H,I,K,L,false,fail,C,[],D,E,F,G),
        true.
expand('$add'(A,B,C),'$add'(D,E,F),'$varlist'(G),H,I,J,K) :- !,
        expand_arith(add,A,B,C,D,E,F,G,[],H,I,J,K),
        true.
expand('$sub'(A,B,C),'$sub'(D,E,F),'$varlist'(G),H,I,J,K) :- !,
        expand_arith(sub,A,B,C,D,E,F,G,[],H,I,J,K),
        true.
expand('$mul'(A,B,C),'$mul'(D,E,F),'$varlist'(G),H,I,J,K) :- !,
        expand_arith(mul,A,B,C,D,E,F,G,[],H,I,J,K),
        true.
expand('$fdiv'(A,B,C),'$fdiv'(D,E,F),'$varlist'(G),H,I,J,K) :- !,
        expand_force(div,float,A,B,C,D,E,F,G,[],H,I,J,K),
        true.
expand('$if2f'(A,B),'$if2f'(C,D),'$varlist'([pref,C,D]),E,F,G,H) :-
        true, % float,
        implies(E,integer(A)),
        !,
        G=[i2f(C,D)|H],
        update_formula(float(B),E,F),
        true.
expand('$if2f'(A,B),'$if2f'(C,D),'$varlist'([pref,C,D]),E,F,G,H) :-
        true, % float,
        implies(E,float(A)),
        !,
        G=[move(C,D)|H],
        update_formula(float(B),E,F),
        true.
expand('$if2f'(A,B),'$if2f'(C,D),'$varlist'([pref,C,D]),E,F,G,H) :-
        true, % float,
        !,
        tag(integer,I),
        G=[test(ne,I,C,J)|K],
        K=[i2f(C,D)|L],
        L=[jump(M)|N],
        N=[label(J)|O],
        arith_error(not([float(A),C]),yes,i2f(C,D),E,P,O,Q),
        Q=[move(C,D)|R],
        R=[label(M)|H],
        update_formula(float(B),P,F),
        true.
expand('$if2f'(A,B),'$if2f'(C,D),'$varlist'([pref,C,D]),E,F,G,H) :-
        fail, % \+float,
        !,
        warning(['Illegal use of function float(X): float option is disabled.']),
        arith_error(true,no,i2f(C,D),E,I,G,H),
        update_formula(fail,I,F),
        true.
expand('$if2i'(A,B),'$if2i'(C,D),'$varlist'([pref,C,D]),E,F,G,H) :-
        true, % float,
        implies(E,float(A)),
        !,
        G=[f2i(C,D)|H],
        update_formula(integer(B),E,F),
        true.
expand('$if2i'(A,B),'$if2i'(C,D),'$varlist'([pref,C,D]),E,F,G,H) :-
        true, % float,
        implies(E,integer(A)),
        !,
        G=[move(C,D)|H],
        update_formula(integer(B),E,F),
        true.
expand('$if2i'(A,B),'$if2i'(C,D),'$varlist'([pref,C,D]),E,F,G,H) :-
        true, % float,
        !,
        tag(float,I),
        G=[test(ne,I,C,J)|K],
        K=[f2i(C,D)|L],
        L=[jump(M)|N],
        N=[label(J)|O],
        arith_error(not([integer(A),C]),yes,f2i(C,D),E,P,O,Q),
        Q=[move(C,D)|R],
        R=[label(M)|H],
        update_formula(integer(B),P,F),
        true.
expand('$if2i'(A,B),'$if2i'(C,D),'$varlist'([pref,C,D]),E,F,G,H) :-
        fail, % \+float,
        !,
        arith_error(not([integer(A),C]),no,f2i(C,D),E,F,G,I),
        I=[move(C,D)|H],
        true.

expand_test(A,B,C,D,E) :-
        expand_test(A,B,C,fail,D,E).

expand_test(A,B,C,D,E,E) :-
        prolog_implies(C,A),
        !.
expand_test(A,B,C,D,E,F) :-
        prolog_implies(C,not(A)),
        !,
        constC(E,jump(D),F).
expand_test(A,B,C,D,E,F) :-
        expand_test(A,G,B,C,D,E,F).

atomic_word(A,A) :-
        number(A),
        !.
atomic_word(A,B^A) :-
        atom(A),
        !,
        term_tag(atom,B).

test_one_tag(A,B,C,D,E,F) :-
        test_one_tag(ne,A,B,C,D,E,F).

expand_test_disj(A,B,C,D,E,F) :-
        expand_test_disj(A,B,C,G,D,E,F).

expand_test_disj(fail,A,B,C,D,E,F) :-
        constC(E,jump(D),G),
        constC(G,label(C),F).
expand_test_disj((A;B),C,D,E,F,G,H) :-
        expand_test(A,C,D,I,G,J),
        constC(J,jump(E),K),
        constC(K,label(I),L),
        expand_test_disj(B,C,D,E,F,L,H).

test_one_tag(A,B,C,D,E,F,G) :-
        tag(B,H,F,I),
        \+ctest(B,D),
        !,
        constC(I,test(A,H,C,E),G).
test_one_tag(A,B,C,D,E,F,F).

expand_integer(A,B,C,D,E,F,G,H,I,J,K,L,M) :-
        arith_instr(A,E,F,G,integer,N),
        H=[E|O],
        O=[F|P],
        P=[G|I],
        (   true -> % float ->
            Q=yes
        ;   Q=no
        ),
        arith_error(not([integer(B),E])or not([integer(C),F]),Q,N,J,R,L,S),
        S=[N|M],
        update_formula(integer(B),R,T),
        update_formula(integer(C),T,U),
        update_formula(integer(D),U,K),
        true.

expand_integer(A,B,C,D,E,F,G,H,I,J,K) :-
        arith_instr(A,D,L,E,integer,M),
        F=[D|N],
        N=[E|G],
        (   true -> % float ->
            O=yes
        ;   O=no
        ),
        arith_error(not([integer(B),D]),O,M,H,P,J,Q),
        Q=[M|K],
        update_formula(integer(B),P,R),
        update_formula(integer(C),R,I),
        true.

expand_cmp(A,B,C,D,E,F,G,H,I,J,K,L,M) :-
        fail, % \+float,
        !,
        arith_cmp(A,D,E,F,G,N),
        arith_error(not([integer(B),D])or not([integer(C),E]),no,N,J,O,L,P),
        P=[N|M],
        H=[D|Q],
        Q=[E|I],
        update_formula(integer(B),O,R),
        update_formula(integer(C),R,K),
        true.
expand_cmp(A,B,C,D,E,F,G,H,I,J,J,K,L) :-
        true, % float,
        implies(J,(integer(B)',' integer(C))),
        !,
        arith_cmp(A,D,E,F,G,M),
        K=[M|L],
        H=[D|N],
        N=[E|I],
        true.
expand_cmp(A,B,C,D,E,F,G,H,I,J,K,L,M) :-
        true, % float,
        implies(J,(float(B);float(C))),
        !,
        cond_to_float(A,N),
        arith_cmp(N,O,P,F,G,Q),
        convert2float(B,D,O,Q,H,R,J,S,L,T),
        convert2float(C,E,P,Q,R,U,S,V,T,W),
        arith_error(not([float(B),O])or not([float(C),P]),yes,Q,V,X,W,Y),
        Y=[Q|M],
        U=[O|Z],
        Z=[P|I],
        make_float_types([B,C],A1),
        update_formula(A1,X,K),
        true.
expand_cmp(A,B,C,D,E,F,G,H,I,J,K,L,M) :-
        true, % float,
        !,
        tag(integer,N),
        cond_to_float(A,O),
        arith_cmp(A,D,E,F,G,P),
        arith_cmp(O,Q,R,F,G,S),
        L=[test(ne,N,D,T)|U],
        U=[test(ne,N,E,V)|W],
        W=[P|X],
        X=[jump(Y)|Z],
        Z=[label(V)|A1],
        convert2float(B,D,Q,S,H,B1,J,C1,A1,D1),
        D1=[jump(E1)|F1],
        F1=[label(T)|G1],
        convert2float(C,E,R,S,B1,H1,C1,I1,G1,J1),
        J1=[label(E1)|K1],
        arith_error(not([float(B),Q])or not([float(C),R]),yes,S,I1,K,K1,L1),
        L1=[S|M1],
        H1=[Q|N1],
        N1=[R|I],
        M1=[label(Y)|M],
        true.

expand_arith(A,B,C,D,E,F,G,H,I,J,K,L,M) :-
        fail, % \+float,
        !,
        arith_instr(A,E,F,G,integer,N),
        arith_error(not([integer(B),E])or not([integer(C),F]),no,N,J,O,L,P),
        P=[N|M],
        H=[E|Q],
        Q=[F|R],
        R=[G|I],
        update_formula(integer(B),O,S),
        update_formula(integer(C),S,T),
        update_formula(integer(D),T,K),
        true.
expand_arith(A,B,C,D,E,F,G,H,I,J,K,L,M) :-
        true, % float,
        implies(J,(integer(B)',' integer(C))),
        !,
        arith_instr(A,E,F,G,integer,N),
        L=[N|M],
        H=[E|O],
        O=[F|P],
        P=[G|I],
        update_formula(integer(D),J,K),
        true.
expand_arith(A,B,C,D,E,F,G,H,I,J,K,L,M) :-
        true, % float,
        implies(J,(float(B);float(C))),
        !,
        arith_instr(A,N,O,G,float,P),
        convert2float(B,E,N,P,H,Q,J,R,L,S),
        convert2float(C,F,O,P,Q,T,R,U,S,V),
        arith_error(not([float(B),N])or not([float(C),O]),yes,P,U,W,V,X),
        X=[P|M],
        T=[N|Y],
        Y=[O|Z],
        Z=[G|I],
        make_float_types([B,C,D],A1),
        update_formula(A1,W,K),
        true.
expand_arith(A,B,C,D,E,F,G,H,I,J,K,L,M) :-
        true, % float,
        !,
        tag(integer,N),
        arith_instr(A,E,F,G,integer,O),
        arith_instr(A,P,Q,G,float,R),
        L=[test(ne,N,E,S)|T],
        T=[test(ne,N,F,U)|V],
        V=[O|W],
        W=[jump(X)|Y],
        Y=[label(U)|Z],
        convert2float(B,E,P,R,H,A1,J,B1,Z,C1),
        C1=[jump(D1)|E1],
        E1=[label(S)|F1],
        convert2float(C,F,Q,R,A1,G1,B1,H1,F1,I1),
        I1=[label(D1)|J1],
        arith_error(not([float(B),P])or not([float(C),Q]),yes,R,H1,K,J1,K1),
        K1=[R|L1],
        G1=[P|M1],
        M1=[Q|N1],
        N1=[G|I],
        L1=[label(X)|M],
        true.

expand_force(div,float,A,B,C,D,E,F,G,H,I,J,K,L) :-
        true, % float,
        !,
        arith_instr(div,M,N,F,float,O),
        convert2float(A,D,M,O,G,P,I,Q,K,R),
        convert2float(B,E,N,O,P,S,Q,T,R,U),
        arith_error(not([float(A),M])or not([float(B),N]),yes,O,T,V,U,W),
        W=[O|L],
        S=[M|X],
        X=[N|Y],
        Y=[F|H],
        make_float_types([A,B,C],Z),
        update_formula(Z,V,J),
        true.
expand_force(div,float,A,B,C,D,E,F,G,H,I,J,K,L) :-
        fail, % \+float,
        !,
        warning(['Illegal use of X/Y: float option is disabled.']),
        arith_instr(div,D,E,F,float,M),
        arith_error(true,no,M,I,N,K,L),
        G=[D|O],
        O=[E|P],
        P=[F|H],
        update_formula(fail,N,J),
        true.

arith_error(A,B,C,D,D,E,E) :-
        fail, % \+arith_error_check,
        !,
        true.
arith_error(A,B,nop,C,C,D,D) :- !,
        true.
arith_error(true,A,B,C,C,D,E) :- !,
        error_routine(B,F,G),
        D=[error_jump(F,G)|E],
        true.
arith_error(not(A)or not(B),C,D,E,F,G,H) :-
        single_check(A,C,I,J,D,E,K,G,L),
        single_check(B,C,I,J,D,K,F,L,M),
        J==yes,
        !,
        M=[jump(N)|O],
        O=[label(I)|P],
        error_routine(D,Q,R),
        P=[error_jump(Q,R)|S],
        S=[label(N)|H],
        true.
arith_error(not(A),B,C,D,E,F,G) :-
        single_check(A,B,H,I,C,D,E,F,J),
        I==yes,
        !,
        J=[jump(K)|L],
        L=[label(H)|M],
        error_routine(C,N,O),
        M=[error_jump(N,O)|P],
        P=[label(K)|G],
        true.
arith_error(A,B,C,D,D,E,E).

expand_cmp_det(A,B,C,D,E,F,G,H,I,J,K) :-
        expand_cmp(A,B,C,D,E,F,G,L,M,H,I,J,K),
        true.

arith_cmp(A,B,C,D,E,F) :-
        arith_comparison(A,B,C,D,E,[F],[]).

convert2float(A,B,B,C,D,D,E,E,F,F) :-
        implies(E,float(A)),
        !,
        true.
convert2float(A,B,C,D,E,E,F,F,G,G) :-
        integer(B),
        !,
        C is float(B),
        true.
convert2float(A,B,B,C,D,D,E,E,F,G) :-
        implies(E,integer(A)),
        !,
        F=[i2f(B,B)|G],
        true.
convert2float(A,B,B,C,D,D,E,E,F,G) :-
        tag(integer,H),
        F=[test(ne,H,B,I)|J],
        J=[i2f(B,B)|K],
        K=[label(I)|G],
        true.

make_float_types([],true).
make_float_types([A|B],C) :-
        nonvar(A),
        !,
        make_float_types(B,C).
make_float_types([A|B],(float(A)',' C)) :-
        var(A),
        !,
        make_float_types(B,C).

arith_comparison(A,B,C,true,D,E,F) :- !,
        constC(E,jump(A,B,C,D),F).
arith_comparison(A,B,C,false,D,E,F) :- !,
        cond(A,G),
        constC(E,jump(G,B,C,D),F).

arith_instr(and,A,B,C,integer,and(A,B,C)).
arith_instr(or,A,B,C,integer,or(A,B,C)).
arith_instr(xor,A,B,C,integer,xor(A,B,C)).
arith_instr(sll,A,B,C,integer,sll(A,B,C)).
arith_instr(sra,A,B,C,integer,sra(A,B,C)).
arith_instr(not,A,B,C,integer,not(A,C)).
arith_instr(add,A,B,C,integer,add(A,B,C)).
arith_instr(sub,A,B,C,integer,sub(A,B,C)).
arith_instr(mul,A,B,C,integer,mul(A,B,C)).
arith_instr(div,A,B,C,integer,div(A,B,C)).
arith_instr(mod,A,B,C,integer,mod(A,B,C)).
arith_instr(add,A,B,C,float,fadd(A,B,C)).
arith_instr(sub,A,B,C,float,fsub(A,B,C)).
arith_instr(mul,A,B,C,float,fmul(A,B,C)).
arith_instr(div,A,B,C,float,fdiv(A,B,C)).

smart_jump(A,B,C,D,D,E,E) :-
        implies(D,A),
        !,
        true.
smart_jump(A,B,C,D,D,E,F) :-
        implies(D,not(A)),
        !,
        E=[jump(C)|F],
        true.
smart_jump(A,B,C,D,D,E,F) :-
        tag_always(A,G),
        E=[test(ne,G,B,C)|F],
        true.

make_integer_types([],true).
make_integer_types([A|B],C) :-
        nonvar(A),
        !,
        make_integer_types(B,C).
make_integer_types([A|B],(integer(A)',' C)) :-
        var(A),
        !,
        make_integer_types(B,C).

arith_comparison(A,B,C,D,E) :-
        arith_comparison(A,B,C,false,fail,D,E).

arith_comparison(A,B,C,D,E,F) :-
        arith_comparison(A,B,C,true,D,E,F).

error_routine(jump(eq,A,B,C),'$ eq_error'/2,[A,B]).
error_routine(jump(ne,A,B,C),'$ ne_error'/2,[A,B]).
error_routine(jump(lts,A,B,C),'$ lt_error'/2,[A,B]).
error_routine(jump(ges,A,B,C),'$ ge_error'/2,[A,B]).
error_routine(jump(gts,A,B,C),'$ gt_error'/2,[A,B]).
error_routine(jump(les,A,B,C),'$ le_error'/2,[A,B]).
error_routine(jump(feq,A,B,C),'$ feq_error'/2,[A,B]).
error_routine(jump(fne,A,B,C),'$ fne_error'/2,[A,B]).
error_routine(jump(flts,A,B,C),'$ flt_error'/2,[A,B]).
error_routine(jump(fges,A,B,C),'$ fge_error'/2,[A,B]).
error_routine(jump(fgts,A,B,C),'$ fgt_error'/2,[A,B]).
error_routine(jump(fles,A,B,C),'$ fle_error'/2,[A,B]).
error_routine(add(A,B,C),'$ add_error'/2,[A,B]).
error_routine(sub(A,B,C),'$ sub_error'/2,[A,B]).
error_routine(mul(A,B,C),'$ mul_error'/2,[A,B]).
error_routine(div(A,B,C),'$ div_error'/2,[A,B]).
error_routine(mod(A,B,C),'$ mod_error'/2,[A,B]).
error_routine(fadd(A,B,C),'$ fadd_error'/2,[A,B]).
error_routine(fsub(A,B,C),'$ fsub_error'/2,[A,B]).
error_routine(fmul(A,B,C),'$ fmul_error'/2,[A,B]).
error_routine(fdiv(A,B,C),'$ fdiv_error'/2,[A,B]).
error_routine(f2i(A,B),'$ int_error'/1,[A]).
error_routine(i2f(A,B),'$ flt_error'/1,[A]).
error_routine(not(A,B),'$ not_error'/1,[A]).
error_routine(and(A,B,C),'$ and_error'/2,[A,B]).
error_routine(or(A,B,C),'$ or_error'/2,[A,B]).
error_routine(xor(A,B,C),'$ xor_error'/2,[A,B]).
error_routine(sll(A,B,C),'$ sll_error'/2,[A,B]).
error_routine(sra(A,B,C),'$ sra_error'/2,[A,B]).

single_check([integer(A),B],C,D,E,div(F,G,H),I,J,K,L) :-
        B==G,
        !,
        divmod_check(integer(A),B,D,E,I,J,K,L),
        true.
single_check([integer(A),B],C,D,E,mod(F,G,H),I,J,K,L) :-
        B==G,
        !,
        divmod_check(integer(A),B,D,E,I,J,K,L),
        true.
single_check([float(A),B],C,D,E,fdiv(F,G,H),I,J,K,L) :-
        B==G,
        !,
        divmod_check(float(A),B,D,E,I,J,K,L),
        true.
single_check([A,B],C,D,E,F,G,G,H,H) :-
        arg(1,A,I),
        do_nothing_check(C,G,I),
        !,
        true.
single_check([A,B],C,D,yes,E,F,F,G,H) :-
        jump_if(ne,A,B,D,G,H),
        true.

divmod_check(A,B,C,D,E,F,G,H) :-
        divmod_typecheck(A,B,C,D,E,I,G,J),
        get_typed_zero(A,K),
        divmod_zerocheck(A,B,C,D,K,I,F,J,H),
        true.

do_nothing_check(yes,A,B) :-
        implies(A,number(B)),
        !.
do_nothing_check(no,A,B) :-
        implies(A,integer(B)),
        !.

jump_if(A,float(B),C,D,E,F) :-
        tag(float,G),
        constC(E,test(A,G,C,D),F).
jump_if(A,integer(B),C,D,E,F) :-
        true, % \+split_integer,
        !,
        tag(integer,G),
        constC(E,test(A,G,C,D),F).
jump_if(eq,integer(A),B,C,D,E) :-
        fail, % split_integer,
        !,
        tag(negative,F),
        tag(nonnegative,G),
        constC(D,test(eq,F,B,C),H),
        constC(H,test(eq,G,B,C),E).
jump_if(ne,integer(A),B,C,D,E) :-
        fail, % split_integer,
        !,
        tag(negative,F),
        tag(nonnegative,G),
        constC(D,test(eq,G,B,H),I),
        constC(I,test(ne,F,B,C),J),
        constC(J,label(H),E).

divmod_typecheck(A,B,C,D,E,E,F,F) :-
        implies(E,A),
        !,
        true.
divmod_typecheck(A,B,C,yes,D,D,E,F) :-
        jump_if(ne,A,B,C,E,F),
        true.

get_typed_zero(float(A),0.0).
get_typed_zero(integer(A),0).

divmod_zerocheck(A,B,C,D,E,F,F,G,G) :-
        arg(1,A,H),
        implies(F,H\==E),
        !,
        true.
divmod_zerocheck(A,B,C,yes,D,E,E,F,G) :-
        F=[jump(eq,B,D,C)|G],
        true.

sub(A,'$case'(B,C,D),'$case'(B,C,E),F,G) :- !,
        sub(A,D,E,F,G).
sub(A,'$test'(B,C,D,E,F),'$test'(B,C,D,E,fail),prolog,G) :-
        mutex(A,D,left,prolog),
        !,
        stopflag(A,D,G).
sub(A,'$test'(B,C,D,E,F),'$test'(B,C,D,E,fail),logical,G) :-
        mutex(A,D,after,logical),
        !,
        stopflag(A,D,G).
sub(A,'$test'(B,C,D,E,F),'$test'(B,C,D,E,G),H,I) :- !,
        sub(A,F,G,H,I).
sub(A,'$else'(B,C,D),'$else'(B,C,E),F,G) :- !,
        sub(A,D,E,F,G).
sub(A,(B;C),(D;E),F,G) :- !,
        sub(A,B,D,F,H),
        sub(A,C,E,F,I),
        or(H,I,G).
sub(A,(B ',' C),(D ',' E),F,G) :- !,
        sub(A,B,D,F,H),
        end_sub(H,A,C,E,F,G).
sub(A,(B->C),(D->E),F,G) :- !,
        sub(A,B,D,F,H),
        end_sub(H,A,C,E,F,G).
sub(A,\+B,\+C,D,false) :- !,
        sub(A,B,C,D,E).
sub(A,not(B),not(C),D,E) :- !,
        sub(A,B,C,D,E).
sub(A,B,C,D,E) :-
        sub_goal(A,B,C,D,E).

logical_subsume(true,A,B) :- !,
        logical_simplify(A,B).
logical_subsume(A,B,C) :-
        sub(A,B,D,logical,E),
        logical_simplify(D,C).

subsume_list(A,[],[]).
subsume_list(A,[B|C],[(D:-E)|F]) :-
        split(B,D,G),
        subsume(A,G,E),
        subsume_list(A,C,F).

subdisj(A,B,C) :-
        sub(A,B,D,prolog,E),
        simplify(D,F),
        (   disj_p(F) ->
            C=F
        ;   C= (F;fail)
        ).

stopflag(A,B,C) :-
        binding_affected(A),
        \+test_nobind(B),
        !,
        C=true.
stopflag(A,B,false).

binding_affected((A ',' B)) :-
        binding_affected(A),
        !.
binding_affected((A ',' B)) :-
        binding_affected(B),
        !.
binding_affected((A;B)) :-
        binding_affected(A),
        !.
binding_affected((A;B)) :-
        binding_affected(B),
        !.
binding_affected(var(A)) :- !.
binding_affected(deref(A)) :- !.
binding_affected(rderef(A)) :- !.
binding_affected(A) :-
        an_uninit_mode(A).

an_uninit_mode(A) :-
        an_uninit_mode(A,B,C).

end_sub(true,A,B,B,C,true).
end_sub(false,A,B,C,D,E) :-
        sub(A,B,C,D,E).

sub_goal(A,B,fail,prolog,C) :-
        mutex(A,B,left,prolog),
        !,
        stopflag(A,B,C).
sub_goal(A,B,fail,logical,C) :-
        mutex(A,B,after,logical),
        !,
        stopflag(A,B,C).
sub_goal(A,B,true,prolog,C) :-
        subsume_work(A,B,left),
        bindbag(B,A,[]),
        !,
        stopflag(A,B,C).
sub_goal(A,B,true,logical,C) :-
        logical_subsume_work(A,B,after),
        !,
        stopflag(A,B,C).
sub_goal(A,B,B,C,D) :-
        stopflag(A,B,D).

subsume_work(A,B,C) :-
        memberv_conj(B,A),
        !.
subsume_work(A,B,C) :-
        prolog_implies(A,B,C).

bindbag(A,B,C) :-
        bindset(A,B,C).

logical_subsume_work(A,B,C) :-
        memberv_conj(B,A),
        !.
logical_subsume_work(A,B,C) :-
        implies(A,B,C).

else(A,B,C) :-
        simplify(not(A),D),
        subsume(D,B,C).

simp_upv(A,B,C) :-
        nonvar(A),
        !,
        simp_up(A,B,C).
simp_upv(A,call(A),B) :-
        var(A),
        !.

simpdisj(A,B) :-
        simplify(A,C),
        (   disj_p(C) ->
            B=C
        ;   B= (C;fail)
        ).

simp_up('$case'(A,B,C),D,E) :- !,
        simp_upv(C,F,E),
        simp_one('$case'(A,B,F),D,E).
simp_up('$test'(A,B,C,D,E),F,G) :- !,
        simp_upv(E,H,G),
        simp_one('$test'(A,B,C,D,H),F,G).
simp_up('$else'(A,B,C),D,E) :- !,
        simp_upv(C,F,E),
        simp_one('$else'(A,B,F),D,E).
simp_up((A ',' B),C,D) :- !,
        simp_upv(A,E,D),
        simp_upv(B,F,D),
        simp_one((E ',' F),C,D).
simp_up((A;B),C,D) :- !,
        simp_upv(A,E,D),
        simp_upv(B,F,D),
        simp_one((E;F),C,D).
simp_up((A->B),C,D) :- !,
        simp_upv(A,E,D),
        simp_upv(B,F,D),
        simp_one((E->F),C,D).
simp_up(\+A,B,C) :- !,
        simp_upv(A,D,C),
        simp_one(\+D,B,C).
simp_up(not(A),B,C) :- !,
        simp_upv(A,D,C),
        simp_one(not(D),B,C).
simp_up(A,B,C) :-
        simp_one(A,B,C).

simp_one(not(not(A)),A,B).
simp_one(not(A=B),A\=B,logical).
simp_one(\+A=B,A\=B,logical).
simp_one(not(A),B,C) :-
        opposite(A,B).
simp_one(\+A,B,C) :-
        opposite(A,B).
simp_one((A;B),A,prolog) :-
        A==B,
        diff_sol,
        no_side_effects(A).
simp_one((A;B),A,logical) :-
        A==B.
simp_one((A ',' B),A,prolog) :-
        A==B,
        deterministic(A),
        no_side_effects(A).
simp_one((A ',' B),A,prolog) :-
        A==B,
        diff_sol,
        no_side_effects(A).
simp_one((A ',' B),A,logical) :-
        A==B.
simp_one((true ',' A),A,B).
simp_one((A ',' true),A,B).
simp_one((true;A),true,prolog) :-
        diff_sol,
        no_side_effects(A),
        no_bind(A).
simp_one((true;A),true,logical).
simp_one((A;true),true,prolog) :-
        diff_sol,
        no_side_effects(A),
        no_bind(A).
simp_one((A;true),true,logical).
simp_one((A ',' fail),fail,prolog) :-
        no_side_effects(A).
simp_one((A ',' fail),fail,logical).
simp_one((fail ',' A),fail,B).
simp_one((fail;A),A,B).
simp_one((A;fail),A,B).
simp_one(('$cut_shallow'(A)',' B;C),('$cut_shallow'(A)',' B),prolog).
simp_one(('$cut_deep'(A)',' B;C),('$cut_deep'(A)',' B),prolog).
simp_one(('$cut_shallow'(A);B),'$cut_shallow'(A),prolog).
simp_one(('$cut_deep'(A);B),'$cut_deep'(A),prolog).
simp_one('$cut_shallow'(A),true,logical).
simp_one('$cut_deep'(A),true,logical).
simp_one((true->A;B),A,C).
simp_one((fail->A;B),B,C).
simp_one((A->true;B),A,prolog) :-
        deterministic(A),
        succeeds(A).
simp_one((A->true;B),A,logical) :-
        succeeds(A).
simp_one((A->fail;B),fail,prolog) :-
        no_side_effects(A),
        succeeds(A).
simp_one((A->fail;B),fail,logical) :-
        succeeds(A).
simp_one((A->B;C),C,prolog) :-
        no_side_effects(A),
        fails(A).
simp_one((A->B;C),C,logical) :-
        fails(A).
simp_one(not((A;B)),(not(A)',' not(B)),C).
simp_one(A,fail,prolog) :-
        fails(A),
        no_side_effects(A).
simp_one(A,fail,logical) :-
        fails(A).
simp_one(A,true,prolog) :-
        diff_sol,
        succeeds(A),
        no_side_effects(A),
        no_bind(A).
simp_one(A,true,logical) :-
        succeeds(A).
simp_one(A,true,prolog) :-
        deterministic(A),
        succeeds(A),
        no_bind(A).
simp_one(A,true,logical) :-
        succeeds(A).
simp_one(deref(A),true,B) :-
        nonvar(A).
simp_one('$case'(A,B,true),true,C).
simp_one('$case'(A,B,fail),fail,C).
simp_one('$test'(A,B,C,D,fail),fail,E).
simp_one(A==B,'$name_arity'(A,B,0),prolog) :-
        var(A),
        atomic(B).
simp_one(A==B,'$name_arity'(B,A,0),prolog) :-
        var(B),
        atomic(A).
simp_one(functor(A,B,C),A=B,prolog) :-
        atomic(B),
        integer(C),
        C=:=0.
simp_one(functor(A,B,C),A=D,prolog) :-
        atom(B),
        integer(C),
        C>=0,
        E=5, % compile_option(functor_limit(E)),
        C=<E,
        !,
        functor(D,B,C).
simp_one(A,B,C) :-
        bitmap_simplify(A,B).
simp_one(A=B,true,C) :-
        A==B.
simp_one(A,A,B).

det_control('$case'(A,B,C)).
det_control('$test'(A,B,C,D,E)).
det_control('$else'(A,B,C)).

diff_sol :-
        \+same_number.

opposite(A,B) :-
        opp(A,B),
        !.
opposite(A,B) :-
        opp(B,A),
        !.
opposite(A,B) :-
        exact_bitmap(A),
        not_test(A,B).

no_side_effects(true) :- !.
no_side_effects(fail) :- !.
no_side_effects((A ',' B)) :- !,
        no_side_effects(A),
        no_side_effects(B).
no_side_effects((A;B)) :- !,
        no_side_effects(A),
        no_side_effects(B).
no_side_effects(A) :-
        test(A).

deterministic(true) :- !.
deterministic(fail) :- !.
deterministic((A ',' B)) :- !,
        deterministic(A),
        deterministic(B).
deterministic(A) :-
        test(A).

no_bind(true) :- !.
no_bind(fail) :- !.
no_bind((A ',' B)) :- !,
        no_bind(A),
        no_bind(B).
no_bind((A;B)) :- !,
        no_bind(A),
        no_bind(B).
no_bind(A) :-
        \+binds(A).

fails(fail).
fails((A ',' B)) :-
        fails(A).
fails((A ',' B)) :-
        no_side_effects(A),
        fails(B).
fails((A;B)) :-
        fails(A),
        fails(B).
fails(A) :-
        encode_test(A,B,C,D),
        type_flags(D,E,F),
        0 is B/\E,
        \+errs(A).
fails(A) :-
        encode_relop(A,B,C,D),
        negate_relop(C,E),
        test_relop(B,E,D),
        \+errs(A).

errs(A) :-
        encode_relop(A,B,C,D,arith),
        (   atom(B)
        ;   atom(D)
        ;   compound(B)
        ;   compound(D)
        ).
errs(functor(A,B,C)) :-
        (   atom(C)
        ;   compound(C)
        ).
errs(arg(A,B,C)) :-
        (   atom(A)
        ;   compound(A)
        ).

opp(true,fail).
opp(A<B,A>=B).
opp(A>B,A=<B).
opp(A=:=B,A=\=B).
opp(A@<B,A@>=B).
opp(A@>B,A@=<B).
opp(A==B,A\==B).
opp(var(A),nonvar(A)).

update_formula(A,B,C,D,D) :-
        update_formula(A,B,C).

xupdate_formula(fail,A,B,C,D,fail) :- !.
xupdate_formula(A,B,fail,C,D,fail) :- !.
xupdate_formula(A,B,true,C,D,A) :- !.
xupdate_formula(true,A,B,C,D,B) :- !.
xupdate_formula(var(A),B,C,D,E,fail) :-
        implies(C,nonvar(A)),
        !.
xupdate_formula(var(A),B,C,D,E,(var(A)',' C)) :- !.
xupdate_formula(A,B,C,D,E,C) :-
        memberv_conj(A,C),
        !.
xupdate_formula(deref(A),B,C,D,E,C) :-
        memberv_conj(rderef(A),C),
        !.
xupdate_formula(deref(A),B,C,D,E,(deref(A)',' C)) :- !.
xupdate_formula(rderef(A),B,C,D,E,(rderef(A)',' F)) :- !,
        remove_formula(deref(A),C,F).
xupdate_formula((A ',' B),C,D,E,F,G) :-
        nonvar(E),
        !,
        xupdate_formula(A,C,D,E,H,I),
        xupdate_formula(B,C,I,E,J,G).
xupdate_formula((A ',' B),C,D,E,F,G) :-
        var(E),
        !,
        xupdate_formula(A,C,D,H,F,I),
        xupdate_formula(B,C,I,J,F,G).
xupdate_formula(float(A),B,C,D,E,F) :- !,
        i2f_formula(C,A,G),
        (   implies(G,float(A)) ->
            F=G
        ;   F= (float(A)',' G)
        ).
xupdate_formula(A,B,C,D,E,F) :-
        (   B=logical ->
            (   mutex(A,C,after) ->
                F=fail
            ;   bitmap_combine(A,C,F) ->
                true
            ;   F= (A ',' C)
            )
        ;   B=prolog ->
            (   var(D) ->
                extended_bindset(A,C,E,G)
            ;   nonvar(D) ->
                extended_bindset(A,C,G)
            ),
            (   G=[] ->
                (   mutex(A,C,left,prolog) ->
                    F=fail
                ;   bitmap_combine(A,C,F) ->
                    true
                ;   F= (A ',' C)
                )
            ;   cons(G) ->
                split_formula(D,E,G,C,H,I),
                remove_vars(H,J),
                (   mutex(A,J,left,prolog) ->
                    F=fail
                ;   combine_formula((A ',' J),I,F)
                )
            ;   G=all ->
                remove_vars(C,J),
                (   mutex(A,J,left,prolog) ->
                    F=fail
                ;   F= (A ',' J)
                )
            )
        ).

logical_update_formula(A,B,C) :-
        xupdate_formula(A,logical,B,yes,D,C).

update_formula(A,B,C,D) :-
        xupdate_formula(A,prolog,C,E,B,D).

remove_formula(A,B,C) :-
        an_uninit_mode(A,D,E),
        !,
        remove_uninit(D,[E],B,C).
remove_formula(A,B,C) :-
        pred_exists(A,B),
        !,
        remove_form(A,B,D),
        flat_conj(D,C).
remove_formula(A,B,B).

extended_bindset(A=B,C,D,E) :-
        uninit_set(C,F),
        e_bindset(A,B,F,D,E),
        !.
extended_bindset(A,B,C,D) :-
        bindset(A,E),
        grounds_in_form(B,F),
        diffv(E,F,D),
        disjointv(D,C),
        !.
extended_bindset(A,B,C,all).

extended_bindset(A=B,C,D) :-
        uninit_set(C,E),
        e_bindset(A,B,E,D),
        !.
extended_bindset(A,B,C) :-
        bindset(A,D),
        grounds_in_form(B,E),
        diffv(D,E,C),
        C=[],
        !.
extended_bindset(A,B,all).

split_formula(A,B,C,D,E,F) :-
        vars_in_unify(C,D,G),
        % stats(x,data(A,C)),
        (   var(A) ->
            uninit_set(D,H),
            diffv(B,H,I),
            aliasing_flag(I,G,A)
        ;   true
        ),
        notnonvar_ground_formula(D,J,K),
        % stats(x,data(notnv(J),notgnd(K))),
        split_form_vars(A,J,K,G,D,L,M),
        % stats(x,data(f1(L),sf1(M))),
        logical_simplify(L,N),
        % stats(x,data(f2(N))),
        flat_conj(N,O),
        % stats(x,data(f3(O))),
        squeeze_conj(O,E),
        % stats(x,6),
        flat_conj(M,F),
        % stats(x,7),
        !.

remove_vars(A,B) :-
        remove_all_vars(A,C),
        notnonvar_ground_formula(A,D,E),
        remove_all_derefs(C,F,D,E),
        squeeze_conj(F,B),
        !.

step_formula(A,B,C,D) :-
        extended_bindset(A,C,B,E),
        (   E=[] ->
            D=C
        ;   cons(E) ->
            split_formula(F,B,E,C,G,H),
            remove_vars(G,I),
            combine_formula(I,H,D)
        ;   E=all ->
            remove_vars(C,D)
        ).

remove_all_vars((A ',' B),(C ',' D)) :- !,
        remove_all_vars(A,C),
        remove_all_vars(B,D).
remove_all_vars(var(A),true) :-
        var(A),
        !.
remove_all_vars(A,A).

notnonvar_ground_formula(A,B,C) :-
        varset(A,D),
        nonvar_set(A,E),
        ground_set(A,F),
        diffv(D,E,B),
        diffv(D,F,C).

remove_all_derefs((A ',' B),(C ',' D),E,F) :- !,
        remove_all_derefs(A,C,E,F),
        remove_all_derefs(B,D,E,F).
remove_all_derefs(deref(A),true,B,C) :-
        var(A),
        inv(A,B),
        !.
remove_all_derefs(rderef(A),B,C,D) :-
        var(A),
        inv(A,D),
        !,
        (   inv(A,C) ->
            B=true
        ;   B=deref(A)
        ).
remove_all_derefs(A,A,B,C).

remove_uninit(A,(B ',' C),D,E) :- !,
        remove_uninit(A,B,D,F),
        remove_uninit(A,C,F,E).
remove_uninit(A,B,C,C) :-
        an_uninit_mode(B,D,E),
        var(E),
        memberv(E,A),
        !.
remove_uninit(A,B,C,D) :-
        co(B,C,D).
remove_uninit(A,B,C,D) :-
        remove_uninit(A,B,C,E,true),
        flat_conj(E,D).

pred_exists(A,(B ',' C)) :-
        pred_exists(A,B),
        !.
pred_exists(A,(B ',' C)) :-
        pred_exists(A,C),
        !.
pred_exists(A,B) :-
        A==B,
        !.

remove_form(A,(B ',' C),(D ',' E)) :- !,
        remove_form(A,B,D),
        remove_form(A,C,E).
remove_form(A,B,true) :-
        A==B,
        !.
remove_form(A,B,B) :- !.

split_from_formula(A,(B ',' C),(D ',' E),(F ',' G)) :- !,
        split_from_formula(A,B,D,F),
        split_from_formula(A,C,E,G).
split_from_formula(A,B,true,B) :-
        varbag(B,C),
        memberv(A,C),
        !.
split_from_formula(A,B,B,true) :- !.

intersect_formula(A,B,C) :-
        intersect_formula(A,B,C,true).

intersect_formula((A ',' B),C,D,E) :- !,
        intersect_formula(A,C,D,F),
        intersect_formula(B,C,F,E).
intersect_formula(A,B,C,D) :-
        memberv_conj(A,B),
        !,
        co(A,C,D).
intersect_formula(A,B,C,C).

intersect_formula_list([A],A) :- !.
intersect_formula_list([A|B],C) :-
        cons(B),
        !,
        intersect_formula_list(B,D),
        intersect_formula(A,D,C).

union_formula(A,B,C) :-
        unionv_conj(A,B,C).

split_deref((A ',' B),(C ',' D)) :- !,
        split_deref(A,C),
        split_deref(B,D).
split_deref(deref(A),deref(A)) :- !.
split_deref(rderef(A),rderef(A)) :- !.
split_deref(A,true).

split_formula(A,B,C,D,E) :-
        split_formula(F,A,B,C,D,E).

uninit_set(A,B) :-
        uninit_bag(A,C),
        sort(C,B).

aliasing_flag(A,B,C) :-
        disjointv(A,B),
        !,
        C=no.
aliasing_flag(A,B,yes).

split_form_vars(A,B,C,D,(E ',' F),(G ',' H),(I ',' J)) :- !,
        split_form_vars(A,B,C,D,E,G,I),
        split_form_vars(A,B,C,D,F,H,J).
split_form_vars(A,B,C,D,E,F,G) :-
        split_one(A,B,C,D,E,F,G).

split_one(A,B,C,D,true,true,true) :- !.
split_one(A,B,C,D,fail,fail,true) :- !.
split_one(yes,A,B,C,var(D),var(D),true) :- !.
split_one(yes,A,B,C,deref(D),deref(D),true) :-
        inv(D,A),
        !.
split_one(yes,A,B,C,deref(D),deref(D),deref(D)) :- !.
split_one(yes,A,B,C,rderef(D),rderef(D),true) :-
        inv(D,B),
        !.
split_one(yes,A,B,C,rderef(D),rderef(D),rderef(D)) :- !.
split_one(A,B,C,D,E,true,E) :-
        varset(E,F),
        disjointv(F,D),
        !.
split_one(A,B,C,D,E,E,true).

notnonvar_formula(A,B) :-
        varset(A,C),
        nonvar_set(A,D),
        diffv(C,D,B).

mem_reg(mem).
mem_reg(reg).

split_uninit((A ',' B),(C ',' D),(E ',' F)) :- !,
        split_uninit(A,C,E),
        split_uninit(B,D,F).
split_uninit(A,A,true) :-
        an_uninit_mode(A),
        !.
split_uninit(A,true,A).

split_unbound((A ',' B),(C ',' D),(E ',' F)) :- !,
        split_unbound(A,C,E),
        split_unbound(B,D,F).
split_unbound(var(A),var(A),true) :- !.
split_unbound(A,A,true) :-
        an_uninit_mode(A),
        !.
split_unbound(A,true,A).

uninit_bag(A,B) :-
        uninit_bag(A,B,[]).

uninit_bag((A ',' B),C,D) :- !,
        uninit_bag(A,C,E),
        uninit_bag(B,E,D).
uninit_bag(A,B,C) :-
        an_uninit_mode(A,D,E),
        !,
        constC(B,E,C).
uninit_bag(A,B,B).

uninit_bag_type(A,B,C) :-
        uninit_bag_type(A,B,C,[]).

uninit_bag_type(A,(B ',' C),D,E) :- !,
        uninit_bag_type(A,B,D,F),
        uninit_bag_type(A,C,F,E).
uninit_bag_type(A,B,C,D) :-
        an_uninit_mode(B,A,E),
        !,
        constC(C,E,D).
uninit_bag_type(A,B,C,C).

unbound_set(A,B) :-
        unbound_bag(A,C),
        sort(C,B).

unbound_bag(A,B) :-
        unbound_bag(A,B,[]).

unbound_bag((A ',' B),C,D) :- !,
        unbound_bag(A,C,E),
        unbound_bag(B,E,D).
unbound_bag(var(A),B,C) :- !,
        constC(B,A,C).
unbound_bag(A,B,C) :-
        an_uninit_mode(A,D,E),
        !,
        constC(B,E,C).
unbound_bag(A,B,B).

remove_uninit(A,B,C) :-
        remove_uninit(A,B,D,true),
        flat_conj(D,C).

remove_uninit(A,B,(C ',' D),E,F) :- !,
        remove_uninit(A,B,C,E,G),
        remove_uninit(A,B,D,G,F).
remove_uninit(A,B,C,D,D) :-
        an_uninit_mode(C,A,E),
        memberv(E,B),
        !.
remove_uninit(A,B,C,D,E) :-
        co(C,D,E).

keep_uninit(A,(B ',' C),D,E) :- !,
        keep_uninit(A,B,D,F),
        keep_uninit(A,C,F,E).
keep_uninit(A,B,C,C) :-
        an_uninit_mode(B,D,E),
        var(E),
        \+memberv(E,A),
        !.
keep_uninit(A,B,C,D) :-
        co(B,C,D).

remove_all_uninit(A,B) :-
        uninit_set(A,C),
        remove_uninit(C,A,B).

e_bindset(A,B,C,D,E) :-
        is_new(A,C,D),
        is_new(B,C,D),
        !,
        E=[].
e_bindset(A,B,C,D,E) :-
        is_new(A,C,D),
        !,
        E=[A].
e_bindset(A,B,C,D,E) :-
        is_new(B,C,D),
        !,
        E=[B].

e_bindset(A,B,C,D) :-
        inv(A,C),
        inv(B,C),
        !,
        D=[].
e_bindset(A,B,C,D) :-
        inv(A,C),
        !,
        D=[A].
e_bindset(A,B,C,D) :-
        inv(B,C),
        !,
        D=[B].

is_new(A,B,C) :-
        var(A),
        \+inv(A,C),
        !.
is_new(A,B,C) :-
        inv(A,B),
        !.

bindset(A=B,C,D) :-
        uninit_set(C,E),
        (   inv(A,E),
            !,
            D=[A]
        ;   inv(B,E),
            !,
            D=[B]
        ).
bindset(A,B,C) :-
        bindset(A,D),
        nonvar_set(B,E),
        diffv(D,E,C).

bindset(A=B,C,D,E) :-
        var(A),
        \+inv(A,D),
        !,
        E=[A].
bindset(A=B,C,D,E) :-
        var(B),
        \+inv(B,D),
        !,
        E=[B].
bindset(A,B,C,D) :-
        bindset(A,B,D).

keylist_cls(A,B) :-
        extract_directives(A,C,D,E),
        first_namearity(C,F),
        keylist_cls(C,B,D,E,F).

merge_cls([],[]).
merge_cls(A,B) :-
        extract_merge(A,C,D,B,E,E,F),
        merge_cls(D,F).

extract_directives([],[],A,A).
extract_directives([A|B],C,D,E) :-
        directive(A),
        !,
        constC(D,A,F),
        extract_directives(B,C,F,E).
extract_directives([A|B],[A|B],C,C).

first_namearity([A|B],C/D) :- !,
        split(A,E,F),
        functor(E,C,D).
first_namearity(A,B).

keylist_cls([],A,B,C,D) :-
        (   B==C ->
            A=[]
        ;   A=[key(3,E,end)-pair(B,C,F,F)]
        ).
keylist_cls([A|B],[C-pair(D,E,[A|F],F)|G],D,E,H) :-
        split(A,I,J),
        functor(I,K,L),
        hash_name(K,L,M),
        (   K/L=H ->
            N=1
        ;   N=2
        ),
        C=key(N,M,K/L),
        extract_directives(B,O,P,Q),
        keylist_cls(O,G,P,Q,H).

hash_name(A,B,0).

diff_sum([],A,A).
diff_sum([A],B,C) :-
        add(A,B,C).
diff_sum([A,B|C],D,E) :-
        add(A,D,F),
        sub(B,F,G),
        diff_sum(C,G,E).

extract_merge([],A,[],B,B,C,C) :- !,
        true.
extract_merge([key(A,B,C)-pair(D,E,F,G)|H],C,I,J,K,L,M) :- !,
        J=D,
        L=F,
        extract_merge(H,C,I,E,K,G,M),
        true.
extract_merge([A-B|C],D,[A-B|C],E,E,F,F) :- !,
        true.

expand_clauses([],[]).
expand_clauses([A|B],[C|D]) :-
        expand_term(A,C),
        expand_clauses(B,D).

translate_clauses([],[]).
translate_clauses([A|B],[A|C]) :-
        directive(A),
        !,
        translate_clauses(B,C).
translate_clauses([A|B],[(C:-D)|E]) :-
        split(A,C,F),
        translate(F,G),
        simplify(G,D),
        translate_clauses(B,E).

cls_to_ptrees_loop(A,B) :-
        (   next_name(A,C,D,B,E),
            cls_to_proc(C,F,D,G) ->
            E=[ptree(D,G,H,[])|I],
            cls_to_ptrees_loop(F,I)
        ;   B=A
        ).

add_modes([]).
add_modes([A|B]) :-
        directive(A),
        !,
        add_modes(B).
add_modes([ptree(A/B,C,(D:-E),F)|G]) :-
        functor(D,A,B),
        (   require(D,H),
            before(D,I),
            combine_formula(H,I,E)
        ;   E=true
        ),
        !,
        add_modes(G).

next_name([A|B],C,D,E,F) :-
        directive(A),
        !,
        constC(E,A,G),
        next_name(B,C,D,G,F).
next_name([A|B],[A|B],C/D,E,E) :-
        split(A,F,G),
        functor(F,C,D).

cls_to_proc([],[],A,[]).
cls_to_proc([A|B],C,D,E) :-
        split(A,F,G),
        functor(F,H,I),
        D=H/I,
        !,
        E=[A|J],
        cls_to_proc(B,C,D,J).
cls_to_proc([A|B],[A|B],C,[]).

top_expr(A,B,C,D) :-
        arith_eval(A,E),
        xtop_expr(E,B,C,D).

expr(A,B,C,D) :-
        arith_eval(A,E),
        xexpr(E,B,C,D).

translate_unify(A,B,C) :-
        nonvar(A),
        nonvar(B),
        functor(A,D,E),
        functor(B,D,E),
        !,
        translate_unify(A,B,1,E,C).
translate_unify(A,B,fail) :-
        nonvar(A),
        nonvar(B),
        !.
translate_unify(A,B,A=B) :-
        var(A),
        !.
translate_unify(A,B,B=A) :-
        var(B),
        !.

translate_unify(A,B,C,D,true) :-
        C>D,
        !.
translate_unify(A,B,C,D,(E ',' F)) :-
        C=<D,
        !,
        arg(C,A,G),
        arg(C,B,H),
        translate_unify(G,H,E),
        I is C+1,
        translate_unify(A,B,I,D,F).

arith_eval(A,A) :-
        var(A),
        !.
arith_eval(A,B) :-
        nonvar(A),
        !,
        functor(A,C,D),
        functor(E,C,D),
        eval_args(1,D,A,E),
        arith_one(E,B).

xtop_expr(A,B,C,D) :-
        number(A),
        !,
        co(B=A,C,D).
xtop_expr(A,B,C,D) :-
        var(A),
        !,
        co(B is A,C,D).
xtop_expr(A,B,C,D) :-
        atom(A),
        !,
        co(B is A,C,D).
xtop_expr(A,B,C,D) :-
        compound(A),
        !,
        xexpr(A,B,C,D).

xexpr(A,A,B,B) :-
        var(A),
        !.
xexpr(A,A,B,B) :-
        number(A),
        !.
xexpr([A],B,C,D) :- !,
        xexpr(A,B,C,D).
xexpr(A,B,C,D) :-
        arith_operation(A,E,F,B,G,H),
        !,
        xexpr_list(E,F,C,I),
        insert(G,H,I,D).
xexpr(A,B,C,D) :-
        co(B is A,C,D).

arith_operation(A+B,[A,B],[C,D],E,F,G) :-
        co('$add'(C,D,E),F,G).
arith_operation(A-B,[A,B],[C,D],E,F,G) :-
        co('$sub'(C,D,E),F,G).
arith_operation(A*B,[A,B],[C,D],E,F,G) :-
        co('$mul'(C,D,E),F,G).
arith_operation(A/B,[A,B],[C,D],E,F,G) :-
        co('$fdiv'(C,D,E),F,G).
arith_operation(A//B,[A,B],[C,D],E,F,G) :-
        co('$idiv'(C,D,E),F,G).
arith_operation(A mod B,[A,B],[C,D],E,F,G) :-
        co('$mod'(C,D,E),F,G).
arith_operation(A/\B,[A,B],[C,D],E,F,G) :-
        co('$and'(C,D,E),F,G).
arith_operation(A and B,[A,B],[C,D],E,F,G) :-
        co('$and'(C,D,E),F,G).
arith_operation(A\/B,[A,B],[C,D],E,F,G) :-
        co('$or'(C,D,E),F,G).
arith_operation(A or B,[A,B],[C,D],E,F,G) :-
        co('$or'(C,D,E),F,G).
arith_operation(A xor B,[A,B],[C,D],E,F,G) :-
        co('$xor'(C,D,E),F,G).
arith_operation(-A,[A],[B],C,D,E) :-
        co('$sub'(0,B,C),D,E).
arith_operation(\(A),[A],[B],C,D,E) :-
        co('$not'(B,C),D,E).
arith_operation(float(A),[A],[B],C,D,E) :-
        co('$if2f'(B,C),D,E).
arith_operation(integer(A),[A],[B],C,D,E) :-
        co('$if2i'(B,C),D,E).
arith_operation(A<<B,[A,B],[C,D],E,F,G) :- !,
        co('$sll'(C,D,E),F,G).
arith_operation(A>>B,[A,B],[C,D],E,F,G) :- !,
        co('$sra'(C,D,E),F,G).

xexpr_list([],[],A,A).
xexpr_list([A|B],[C|D],E,F) :-
        xexpr(A,C,E,G),
        xexpr_list(B,D,G,F).

eval_args(A,B,C,D) :-
        A>B,
        !.
eval_args(A,B,C,D) :-
        A=<B,
        !,
        arg(A,C,E),
        arg(A,D,F),
        arith_eval(E,F),
        G is A+1,
        eval_args(G,B,C,D).

arith_one(A,B) :-
        arith_one_tab(A,B),
        !.
arith_one(A,A).

arith_one_tab([A],A).
arith_one_tab(float(A),A) :-
        true, % float,
        float(A).
arith_one_tab(float(A),B) :-
        true, % float,
        integer(A),
        B is float(A).
arith_one_tab(integer(A),A) :-
        integer(A).
arith_one_tab(integer(A),B) :-
        true, % float,
        float(A),
        B is integer(A).
arith_one_tab(A+B,C) :-
        number(A),
        number(B),
        C is A+B.
arith_one_tab(A-B,C) :-
        number(A),
        number(B),
        C is A-B.
arith_one_tab(A*B,C) :-
        number(A),
        number(B),
        C is A*B.
arith_one_tab(A/B,C) :-
        number(A),
        number(B),
        C is A/B.
arith_one_tab(A//B,C) :-
        integer(A),
        integer(B),
        C is A//B.
arith_one_tab(A mod B,C) :-
        integer(A),
        integer(B),
        C is A mod B.
arith_one_tab(A/\B,C) :-
        integer(A),
        integer(B),
        C is A/\B.
arith_one_tab(A and B,C) :-
        integer(A),
        integer(B),
        C is A/\B.
arith_one_tab(A\/B,C) :-
        integer(A),
        integer(B),
        C is A\/B.
arith_one_tab(A or B,C) :-
        integer(A),
        integer(B),
        C is A\/B.
arith_one_tab(A xor B,C) :-
        integer(A),
        integer(B),
        xor(A,B,C).
arith_one_tab(-A,B) :-
        integer(A),
        B is-A.
arith_one_tab(\(A),B) :-
        integer(A),
        B is\(A).
arith_one_tab(A<<B,C) :-
        integer(A),
        integer(B),
        C is A<<B.
arith_one_tab(A>>B,C) :-
        integer(A),
        integer(B),
        C is A>>B.

xor(A,B,C) :-
        C is A/\ \(B)\/ (B/\ \(A)).

transform_cut_ptree(A,B) :-
        A=ptree(C,D,E,F),
        !,
        transform_cut_cls(D,G,H,I,J),
        K=ptree(C,G,E,L),
        transform_cut_ptrees(F,L),
        transform_2(J,H,K,B).
transform_cut_ptree(A,A) :-
        directive(A),
        !.

transform_cut_cls([],[],A,false,false).
transform_cut_cls([A|B],[(C:-D)|E],F,G,H) :-
        split(A,C,I),
        standard_conj(I,J),
        transform_cut_conj(J,D,true,F,K,L,true,M),
        transform_cut_cls(B,E,F,N,O),
        or(K,N,G),
        or(L,O,H).

transform_2(false,A,B,B) :- !.
transform_2(true,A,B,C) :-
        B=ptree(D/E,F,(G:-H),I),
        gensym([36,99,117,116,95],D/E,J),
        K is E+1,
        dup_head(G,J,K,L,M),
        C=ptree(D/E,[(G:-'$cut_load'(L)',' M)],(G:-H),[N]),
        dup_head(G,J,K,A,O),
        replace_heads(F,P,J,K,A),
        N=ptree(J/K,P,(O:-H),I),
        add_mode_option(G,O,true,true,yes).

dup_head(A,B,C,D,E) :-
        functor(A,F,G),
        functor(E,B,C),
        match_all(1,G,A,E),
        arg(C,E,D).

replace_heads([],[],A,B,C).
replace_heads([A|B],[(C:-D)|E],F,G,H) :-
        split(A,I,D),
        dup_head(I,F,G,H,C),
        replace_heads(B,E,F,G,H).

match_all(A,B,C,D) :-
        A>B,
        !.
match_all(A,B,C,D) :-
        A=<B,
        !,
        arg(A,C,E),
        arg(A,D,E),
        F is A+1,
        match_all(F,B,C,D).

standard_conj(A,B) :-
        conj(A,B,true).

transform_cut_conj(true,A,A,B,false,false,C,C).
transform_cut_conj((A ',' B),(A ',' C),D,E,F,G,H,I) :-
        test(A),
        !,
        transform_cut_conj(B,C,D,E,F,G,H,I).
transform_cut_conj((A ',' B),(C ',' D),E,F,G,H,I,J) :-
        disj_p(A),
        !,
        transform_cut_disj(A,C,F,K,L,false),
        transform_cut_conj(B,D,E,F,M,N,false,J),
        or(K,M,G),
        or(L,N,H).
transform_cut_conj((!',' A),B,C,D,true,true,E,F) :-
        insert_cut(E,D,B,G),
        transform_cut_conj(A,G,C,D,H,I,E,F).
transform_cut_conj((A ',' B),(A ',' C),D,E,F,G,H,I) :-
        \+disj_p(A),
        \+test(A),
        \+A=!,
        transform_cut_conj(B,C,D,E,F,G,false,I).

transform_cut_disj(A,('$cut_load'(B)',' C),D,E,F,G) :-
        contains_if(A),
        !,
        transform_cut_disj(A,C,D,B,E,F,G).
transform_cut_disj(A,B,C,D,E,F) :-
        transform_cut_disj(A,B,C,G,D,E,F).

contains_if((A->B;C)) :- !.
contains_if((A;B)) :-
        contains_if(B).

transform_cut_disj(fail,fail,A,B,false,false,C).
transform_cut_disj((A->B;C),(D;E),F,G,H,I,J) :-
        transform_cut_conj(A,D,K,F,L,M,J,N),
        insert_cut(N,G,K,O),
        transform_cut_conj(B,O,true,F,P,Q,N,R),
        transform_cut_disj(C,E,F,G,S,T,J),
        or(L,P,S,H),
        or(M,Q,T,I).
transform_cut_disj((A;B),(C;D),E,F,G,H,I) :-
        transform_cut_conj(A,C,true,E,J,K,I,L),
        transform_cut_disj(B,D,E,F,M,N,I),
        or(J,M,G),
        or(K,N,H).

insert_cut(true,A,('$cut_shallow'(A)',' B),B).
insert_cut(false,A,('$cut_deep'(A)',' B),B).

factor_dlist(ptree(A,B,C,D),ptree(A,B,C,E)) :-
        factor_ptrees(D,E).

get_factor_args(ptree(A,B,(C:-D),E),F) :-
        a_head(B,G),
        functor(G,H,I),
        bagof(J,(range(1,J,I)',' test_arg(J,B)),F),
        !.
get_factor_args(A,[]).

factor_args_ptree([],A,A).
factor_args_ptree([A|B],ptree(C,D,E,F),ptree(C,G,E,H)) :-
        factor_arg_cls(C,A,D,G,E,I,[]),
        factor_args_ptrees(B,I,H,F).

factor_arg_cls(A,B,C,D,E,F,G) :-
        check_arity(C,B),
        get_arg(C,B,H),
        solution_order(H,I),
        collect_info(A,I,J,K),
        cons(J),
        check_heuristic(K),
        !,
        transform(C,1,B,J,D,L),
        keysort(L,M),
        collect_ptrees(M,B,E,F,G).
factor_arg_cls(A,B,C,C,D,E,E).

factor_args_ptrees(A,[],B,B).
factor_args_ptrees(A,[B|C],[D|E],F) :-
        factor_args_ptree(A,B,D),
        factor_args_ptrees(A,C,E,F).

check_arity([A|B],C) :-
        split(A,D,E),
        functor(D,F,G),
        C=<G.

get_arg(A,B,C) :-
        get_arg(A,B,C,1).

solution_order(A,A) :-
        same_order,
        !.
solution_order(A,B) :-
        \+same_order,
        !,
        keysort(A,B).

collect_info(A,B,C,D) :-
        collect_msg(B,D),
        remove_single(D,E),
        list_clauses(E,A,F,[]),
        keysort(F,C).

check_heuristic(A) :-
        keysort(A,B),
        \+adjacent_nontrivial(B).

transform(A,B,C,[],A,[]) :- !.
transform([A|B],C,D,[E-F|G],H,[I|J]) :-
        C=:=E,
        !,
        split(A,K,L),
        F=info(M,N,O,P),
        functor(K,Q,R),
        functor(S,Q,R),
        calc_darity(K,O,T),
        functor(U,N,T),
        functor(V,N,T),
        minimum(R,T,W),
        match_most(1,D,W,K,V),
        match_most(1,D,W,S,U),
        match_rest(D,R,K,S,U,V,M),
        I=N/T-info2(M,O,(V:-L)),
        (   P=first ->
            H=[(S:-U)|X]
        ;   H=X
        ),
        Y is C+1,
        transform(B,Y,D,G,X,J).
transform([A|B],C,D,[E-F|G],[A|H],I) :-
        C<E,
        !,
        J is C+1,
        transform(B,J,D,[E-F|G],H,I).
transform([A|B],C,D,[E-F|G],[A|H],I) :-
        C>E,
        !,
        J is C+1,
        transform(B,J,D,G,H,I).

collect_ptrees([],A,B,C,C).
collect_ptrees([A-B|C],D,E,F,G) :-
        B=info2(H,I,J),
        E= (K:-L),
        require(K,M),
        before(K,N),
        make_dformula((K:-M),A,H,D,I,(O:-P)),
        make_dformula((K:-N),A,H,D,I,(O:-Q)),
        flat_conj((P ',' Q),R),
        constC(F,ptree(A,[J|S],(O:-R),[]),T),
        add_mode_option(dummy_mode(O,P,Q)),
        collect_proc(C,A,U,S),
        collect_ptrees(U,D,E,T,G).

adjacent_nontrivial([A-B,C-D|E]) :-
        msg(A,C,F),
        compound(F).
adjacent_nontrivial([A|B]) :-
        adjacent_nontrivial(B).

msg(A,B,C) :-
        nonvar(A),
        nonvar(B),
        !,
        (   functor(A,D,E),
            functor(B,D,E) ->
            A=..[D|F],
            B=..[D|G],
            msg_args(F,G,H),
            C=..[D|H]
        ;   true
        ).
msg(A,B,A) :-
        A==B,
        !.
msg(A,B,C) :-
        \+A==B,
        !.

a_head(A,B) :-
        member(C,A),
        split(C,B,D).

test_arg(A,B) :-
        bagof(C-nop,D^ (a_head(B,D)',' arg(A,D,C)',' compound(C)),E),
        keysort(E,F),
        similar_args(F),
        !.

similar_args([A-nop,B-nop|C]) :-
        similar(A,B),
        !.
similar_args([A|B]) :-
        similar_args(B).

similar(A,B) :-
        functor(A,C,D),
        functor(B,C,D).

get_arg([],A,[],B).
get_arg([A|B],C,[D-E|F],E) :-
        head_arg(C,A,D),
        G is E+1,
        get_arg(B,C,F,G).

head_arg(A,B,C) :-
        (   B= (D:-E) ->
            arg(A,D,C)
        ;   arg(A,B,C)
        ).

collect_msg([],[]).
collect_msg([A-B|C],D) :-
        collect_msg(C,A,[B],D).

remove_single([],[]).
remove_single([A|B],C) :-
        A=D-[E],
        !,
        remove_single(B,C).
remove_single([A|B],[A|C]) :-
        \+A=D-[E],
        !,
        remove_single(B,C).

list_clauses([],A,B,B).
list_clauses([A-B|C],D,E,F) :-
        gensym([36,102,97,99,95],D,G),
        varset(A,H),
        length(H,I),
        expand_clauses(B,A,G,I,first,E,J),
        list_clauses(C,D,J,F).

collect_msg([],A,B,[A-B]) :-
        compound(A),
        !.
collect_msg([],A,B,[]) :-
        \+compound(A),
        !.
collect_msg(A,B,C,D) :-
        \+compound(B),
        !,
        collect_msg(A,D).
collect_msg([A-B|C],D,E,F) :-
        compound(D),
        msg(A,D,G),
        (   nonvar(G) ->
            collect_msg(C,G,[B|E],F)
        ;   F=[D-E|H],
            collect_msg(C,A,[B],H)
        ).

expand_clauses([],A,B,C,D,E,E).
expand_clauses([A|B],C,D,E,F,G,H) :-
        copy(C,I),
        constC(G,A-info(I,D,E,F),J),
        expand_clauses(B,C,D,E,notfirst,J,H).

calc_darity(A,B,C) :-
        functor(A,D,E),
        F is E+B-1,
        maximum(F,E,C).

match_most(A,B,C,D,E) :-
        A>C,
        !.
match_most(A,B,C,D,E) :-
        A=\=B,
        A=<C,
        !,
        arg(A,D,F),
        arg(A,E,F),
        G is A+1,
        match_most(G,B,C,D,E).
match_most(A,B,C,D,E) :-
        A=:=B,
        A=<C,
        !,
        F is A+1,
        match_most(F,B,C,D,E).

match_rest(A,B,C,D,E,F,G) :-
        A>B,
        !.
match_rest(A,B,C,D,E,F,G) :-
        A=<B,
        !,
        arg(A,D,G),
        arg(A,C,H),
        corresponding_vars(G,H,I,J),
        (   cons(I) ->
            K is B+1,
            arg(A,F,L),
            J=[L|M],
            end_fill_args(M,F,K),
            arg(A,E,N),
            I=[N|O],
            end_fill_args(O,E,K)
        ;   true
        ).

match_offset(A,B,C,D,E) :-
        A>B,
        !.
match_offset(A,B,C,D,E) :-
        A=<B,
        !,
        arg(A,C,F),
        arg(D,E,F),
        G is A+1,
        H is D+1,
        match_offset(G,B,C,H,E).

corresponding_vars(A,B,C,D) :-
        corr_vars(A,B,[],C,[],D).

end_fill_args([],A,B) :- !.
end_fill_args([A|B],C,D) :-
        arg(D,C,A),
        E is D+1,
        end_fill_args(B,C,E).

corr_vars(A,B,C,D,E,F) :-
        nonvar(A),
        !,
        functor(A,G,H),
        functor(B,G,H),
        corr_args(1,H,A,B,C,D,E,F).
corr_vars(A,B,C,[A|C],D,[B|D]) :-
        var(A).

corr_args(A,B,C,D,E,E,F,F) :-
        A>B,
        !.
corr_args(A,B,C,D,E,F,G,H) :-
        A=<B,
        !,
        arg(A,C,I),
        arg(A,D,J),
        corr_vars(I,J,E,K,G,L),
        M is A+1,
        corr_args(M,B,C,D,K,F,L,H).

msg_vars(A,B) :-
        msg_vars(A,[],B).

msg_vars(A,B,C) :-
        nonvar(A),
        !,
        functor(A,D,E),
        msg_args(1,E,A,B,C).
msg_vars(A,B,[A|B]) :-
        var(A).

msg_args(A,B,C,D,D) :-
        A>B,
        !.
msg_args(A,B,C,D,E) :-
        A=<B,
        !,
        arg(A,C,F),
        msg_vars(F,D,G),
        H is A+1,
        msg_args(H,B,C,G,E).

make_dformula((A:-B),C/D,E,F,G,(H:-I)) :-
        functor(A,J,K),
        functor(H,C,D),
        minimum(K,D,L),
        match_most(1,F,L,A,H),
        arg(F,A,M),
        msg_vars(E,N),
        (   cons(N) ->
            N=[O|P],
            arg(F,H,O),
            Q is K+1,
            end_fill_args(P,H,Q)
        ;   true
        ),
        (   type_propagate(B,M,R) ->
            mode_propagate(N,R,I,B)
        ;   I=B
        ).

collect_proc([],A,[],[]).
collect_proc([A|B],C,[A|B],[]) :-
        A=D-info2(E,F,G),
        H=info2(I,J,G),
        C\==D,
        !.
collect_proc([A|B],C,D,[E|F]) :-
        A=C-info2(G,H,E),
        collect_proc(B,C,D,F).

type_propagate(A,B,uninit) :-
        implies(A,uninit(B)),
        !.
type_propagate(A,B,var) :-
        implies(A,var(B)),
        !.
type_propagate(A,B,ground) :-
        implies(A,ground(B)),
        !.
type_propagate(A,B,rderef) :-
        implies(A,rderef(B)),
        !.

mode_propagate([],A,B,B) :- !.
mode_propagate([A|B],C,D,E) :-
        one_propagate(C,A,D,F),
        mode_propagate(B,C,F,E).

one_propagate(uninit,A,B,C) :-
        co(uninit(A),B,C),
        !.
one_propagate(var,A,B,C) :-
        co(var(A),B,C),
        !.
one_propagate(ground,A,B,C) :-
        co(ground(A),B,C),
        !.
one_propagate(rderef,A,B,C) :-
        co(rderef(A),B,C),
        !.

msg_args([],[],[]).
msg_args([A|B],[C|D],[E|F]) :-
        msg(A,C,E),
        msg_args(B,D,F).

ptree_to_stree(ptree(A,B,C,D),stree(A,(E:-F),(E:-G),H,I,J)) :- !,
        standard_cls(B,C,E,H,F),
        copy(C,(E:-G)),
        ptrees_to_strees(D,I).
ptree_to_stree(A,A) :-
        directive(A),
        !.

standard_cls([],A,B,[],fail).
standard_cls([A|B],C,D,[E|F],(G;H)) :-
        expand_term(A,I),
        copy(I,J),
        standard_form(J,C,E,D,G),
        standard_cls(B,C,D,F,H).

standard_form(A,B,C,D,E) :-
        split(A,C,F),
        simplify(F,G),
        unr_head(C,B,D,E,H),
        standard_conj(G,H).

standard_form(A,B,C) :-
        split(A,B,D),
        simplify(D,E),
        standard_conj(E,C).

unr_head(A,(B:-true),C,D,E) :- !,
        functor(A,F,G),
        downrange_list(G,1,H),
        match_unr(H,[],[],A,C,D,E).
unr_head(A,(B:-C),D,E,F) :-
        functor(A,G,H),
        split_nonvars(1,C,B,A,H,I,[],J,[],K,[]),
        reverse(J,L),
        reverse(K,M),
        append(I,L,M,N),
        match_unr(N,K,I,A,D,E,F).

conj((A ',' B),C,D) :- !,
        conj(A,C,E),
        conj(B,E,D).
conj(true,A,A) :- !.
conj(A,B,C) :-
        \+conj_p(A),
        inside_conj(A,B,C).

disj((A;B),C,D) :- !,
        disj(A,C,E),
        disj(B,E,D).
disj(fail,A,A) :- !.
disj(A,B,C) :-
        \+disj_p(A),
        inside_disj(A,B,C).

inside_conj(\+A,B,C) :-
        !,
        simplify(\+A,D),
        negation_as_failure_conj(D,B,C).
inside_conj((A->B),C,D) :-
        !,
        conj(A,E,true),
        conj(B,F,true),
        co((E->F;fail),C,D).
inside_conj(A,B,C) :-
        disj_p(A),
        !,
        disj(A,D,fail),
        co(D,B,C).
inside_conj(A,B,C) :-
        \+disj_p(A),
        \+anyregs(A),
        !,
        co(A,B,C).
inside_conj(A,B,C) :-
        \+disj_p(A),
        anyregs(A),
        !,
        unr_goal(A,B,C).

inside_disj(\+A,B,C) :-
        !,
        simplify(\+A,D),
        negation_as_failure_disj(D,B,C).
inside_disj((A->B),C,D) :-
        !,
        conj(A,E,true),
        conj(B,F,true),
        di((E->F),C,D).
inside_disj(A,B,C) :-
        conj_p(A),
        !,
        conj(A,D,true),
        di(D,B,C).
inside_disj(A,B,C) :-
        \+conj_p(A),
        !,
        co(A,D,true),
        di(D,B,C).

negation_as_failure_disj(\+A,B,C) :- !,
        conj(A,D,true),
        di(((D->fail ',' true;true;fail)',' true),B,C).
negation_as_failure_disj(A,B,C) :-
        disj(A,B,C).

negation_as_failure_conj(\+A,B,C) :- !,
        conj(A,D,true),
        co((D->fail ',' true;true;fail),B,C).
negation_as_failure_conj(A,B,C) :-
        conj(A,B,C).

unr_goal(A,B,C) :-
        unr_goal(A,D,B,E),
        co(D,E,C).

match_unr(A,B,C,D,E,F,G) :-
        functor(D,H,I),
        functor(E,H,I),
        match_unr(A,B,C,D,E,J,true,K,true,L,G),
        reverse_conj(J,M),
        reverse_conj(K,N),
        append_conj(M,N,L,F).

split_nonvars(A,B,C,D,E,F,F,G,G,H,H) :-
        A>E,
        !,
        true.
split_nonvars(A,B,C,D,E,F,G,H,I,J,K) :-
        A=<E,
        arg(A,C,L),
        implies(B,nonvar(L)),
        !,
        J=[A|M],
        N is A+1,
        split_nonvars(N,B,C,D,E,F,G,H,I,M,K),
        true.
split_nonvars(A,B,C,D,E,F,G,H,I,J,K) :-
        A=<E,
        arg(A,C,L),
        implies(B,unbound(L)),
        !,
        F=[A|M],
        N is A+1,
        split_nonvars(N,B,C,D,E,M,G,H,I,J,K),
        true.
split_nonvars(A,B,C,D,E,F,G,H,I,J,K) :-
        A=<E,
        !,
        H=[A|L],
        M is A+1,
        split_nonvars(M,B,C,D,E,F,G,L,I,J,K),
        true.

match_unr([],A,B,C,D,E,E,F,F,G,G) :- !,
        true.
match_unr([A|B],C,D,E,F,G,H,I,J,K,L) :-
        arg(A,E,M),
        var(M),
        member(N,B),
        arg(N,E,O),
        M==O,
        !,
        arg(A,F,P),
        conj_unr(P=M,A,C,D,G,Q,I,R,K,S),
        match_unr(B,C,D,E,F,Q,H,R,J,S,L),
        true.
match_unr([A|B],C,D,E,F,G,H,I,J,K,L) :-
        arg(A,E,M),
        var(M),
        !,
        arg(A,F,M),
        match_unr(B,C,D,E,F,G,H,I,J,K,L),
        true.
match_unr([A|B],C,D,E,F,G,H,I,J,K,L) :-
        arg(A,E,M),
        nonvar(M),
        !,
        arg(A,F,N),
        conj_unr(N=M,A,C,D,G,O,I,P,K,Q),
        match_unr(B,C,D,E,F,O,H,P,J,Q,L),
        true.

conj_unr(A,B,C,D,E,F,G,G,H,H) :-
        member(B,C),
        !,
        E= (A ',' F),
        true.
conj_unr(A,B,C,D,E,E,F,F,G,H) :-
        member(B,D),
        !,
        G= (A ',' H),
        true.
conj_unr(A,B,C,D,E,E,F,G,H,H) :-
        F= (A ',' G),
        true.

unr_goal(A=B,fail,C,C) :-
        \+A=B,
        !.
unr_goal(A=B,A=B,C,C) :-
        var(A),
        \+ \+A=B,
        !.
unr_goal(A=B,B=A,C,C) :-
        var(B),
        \+ \+A=B,
        !.
unr_goal(A=B,A=B,C,C) :-
        nonvar(A),
        nonvar(B),
        !.
unr_goal(A,B,C,D) :-
        \+unify_p(A),
        !,
        unr_str(A,B,C,D).

unr_str(A,B,C,D) :-
        A=..[E|F],
        unr_args(F,G,C,D),
        B=..[E|G].

unr_sng(A,A,B,B) :-
        single_word(A),
        !.
unr_sng(A,B,C,D) :-
        \+single_word(A),
        !,
        co(B=A,C,D).

single_word(A) :-
        var(A),
        !.
single_word(A) :-
        atomic(A),
        !.

unr_args([],[],A,A).
unr_args([A|B],[C|D],E,F) :-
        unr_sng(A,C,E,G),
        unr_args(B,D,G,F).

re_unr_strees(A,B) :-
        true, % compile_option(compile),
        !,
        re_unr_strees_2(A,B).
re_unr_strees(A,A) :-
        fail, % \+compile_option(compile),
        !.

re_unr_strees_2([],[]).
re_unr_strees_2([A|B],[C|D]) :-
        re_unr_stree(A,C),
        re_unr_strees_2(B,D).

re_unr_stree(stree(A,(B:-C),D,E,F,G),stree(A,(B:-H),D,E,I,G)) :-
        cons(E),
        non_trivial(D),
        !,
        re_unr_disj(E,B,C,H,D),
        re_unr_strees(F,I).
re_unr_stree(stree(A,B,C,D,E,F),stree(A,B,C,D,G,F)) :- !,
        re_unr_strees(E,G).
re_unr_stree(A,A) :-
        directive(A),
        !.

non_trivial((A:-B)) :-
        \+B=true.

re_unr_disj([],A,fail,fail,B).
re_unr_disj([A|B],C,(D;E),(F;G),H) :-
        copy((A:-D),(I:-J)),
        unr_head(I,H,C,K,true),
        replace_start_conj(K,J,F),
        re_unr_disj(B,C,E,G,H).

flatten_stree(stree(A,(B:-C),D,E,F,G),stree(A,(B:-H),(B:-I),E,J,G)) :- !,
        copy(D,(B:-I)),
        varset(B,K),
        require(B,L),
        before(B,M),
        flatten_disj(A,C,H,L,M,K,[],G,J,N),
        flatten_strees(F,N).
flatten_stree(A,A) :-
        directive(A),
        !.

flatten_disj(A,(B;C),(D;E),F,G,H,I,J,K,L) :-
        disj_inside(B),
        !,
        flatten(A,B,D,F,G,H,M,I,J,K,N),
        flatten_disj(A,C,E,F,G,H,I,J,N,L).
flatten_disj(A,(B;C),(B;D),E,F,G,H,I,J,K) :-
        \+disj_inside(B),
        !,
        flatten_disj(A,C,D,E,F,G,H,I,J,K).
flatten_disj(A,fail,fail,B,C,D,E,F,G,G).

disj_inside((A ',' B)) :-
        disj_inside(A),
        !.
disj_inside((A ',' B)) :-
        disj_inside(B),
        !.
disj_inside((A;B)).

flatten(A,(B ',' C),(D ',' E),F,G,H,I,J,K,L,M) :-
        strong_disj_p(B),
        !,
        varset(B,N),
        unionv(N,H,O),
        split_formula2(H,B,F,G,P),
        remove_vars(G,Q),
        flatten(A,C,E,P,Q,O,R,J,K,L,S),
        unionv(N,R,I),
        dummy_proc(A,B,N,F,G,H,R,D,K,S,M).
flatten(A,(B ',' C),(B ',' D),E,F,G,H,I,J,K,L) :-
        \+strong_disj_p(B),
        !,
        varset(B,M),
        unionv(M,G,N),
        after(B,O),
        split_formula2(G,B,E,F,P),
        step_formula(B,G,F,Q),
        update_formula(O,G,Q,R),
        flatten(A,C,D,P,R,N,S,I,J,K,L),
        unionv(M,S,H).
flatten(A,true,true,B,C,D,E,E,F,G,G).

split_formula2(A,B,C,D,E) :-
        unionv_conj(C,D,F),
        split_formula(yes,A,B,F,G,H),
        intersectv_conj(H,C,E).

dummy_proc(A,B,C,D,E,F,G,H,I,[J|K],K) :-
        flatten_disj(A,B,L,D,E,F,G,I,M,[]),
        unionv(F,G,N),
        intersectv(C,N,O),
        gensym([36,102,108,97,95],A,P),
        H=..[P|O],
        functor(H,P,Q),
        flat_conj((D ',' E),R),
        trim_mode((H:-R),S),
        copy(stree(P/Q,(H:-L),S,[],M,T),J),
        add_mode_option(dummy_mode(H,D,E)).

disj_exists((A ',' B)) :-
        (   disj_p(A) ->
            true
        ;   disj_exists(B)
        ).

gather_single([],A) :-
        seal(A).
gather_single([stree(A,(B:-C;fail),D,E,[],F)|G],H) :-
        length_test_user(C,I,J),
        K is I+J,
        K=<2,
        \+builtin(B),
        !,
        get(H,A,L),
        enter_def(L,info((B:-C),I,J),A),
        gather_single(G,H).
gather_single([A|B],C) :-
        gather_single(B,C).

inline_replace_strees([],[],A).
inline_replace_strees([stree(A,(B:-C),D,E,F,G)|H],[stree(A,(B:-I),D,E,J,G)|K],L) :-
        inline_replace_disj(C,I,L),
        inline_replace_strees(F,J,L),
        inline_replace_strees(H,K,L).

enter_def(A,B,C) :-
        var(A),
        !,
        B=A.
enter_def(A,B,C) :-
        nonvar(A),
        !,
        warning(['The predicate ',C,' has multiple definitions.',nl,'Only the first definition will be used.']).

inline_replace_disj(fail,fail,A).
inline_replace_disj((A;B),(C;D),E) :-
        inline_replace_conj(A,F,E),
        flat_conj(F,C),
        inline_replace_disj(B,D,E).

inline_replace_conj(true,true,A).
inline_replace_conj((A ',' B),(A ',' C),D) :-
        test(A),
        !,
        inline_replace_conj(B,C,D).
inline_replace_conj((A ',' B),(C ',' D),E) :-
        functor(A,F,G),
        fget(E,F/G,info(H,I,J)),
        !,
        copy(H,(A:-C)),
        (   J=:=0 ->
            inline_replace_conj(B,D,E)
        ;   B=D
        ).
inline_replace_conj((A ',' B),(A ',' B),C).

complexity(A,A) :-
        fail, % compile_option(complexity),
        !,
        init_mult_strees(A,B),
        mult_strees(A,B,C,D,E,0,F),
        seal(E),
        comment(['Total complexity of program is ',F]),
        print_mult_strees(E,C).
complexity(A,A).

init_mult_strees(A,B) :-
        init_mult_strees(A,C,B),
        seal(B).

mult_strees([],A,A,B,B,C,C).
mult_strees([A|B],C,D,E,F,G,H) :-
        mult_stree(A,C,I,E,J,G,K),
        mult_strees(B,I,D,J,F,K,H),
        true.

print_mult_strees(node(A,[B|C],D,E),node(A,F,G,H)) :- !,
        I is B+C,
        comment(['uc= ',B,' gc= ',C,' tc= ',I,' times_called= ',F,' pred= ',A]),
        print_mult_strees(D,G),
        print_mult_strees(E,H).
print_mult_strees(leaf,leaf) :- !.
print_mult_strees(A,B) :- !,
        error(['Mismatch in complexity measurement.']).

mult_stree(stree(A,(B:-C),D,E,F,G),H,I,J,K,L,M) :- !,
        mult_disj(C,H,N,A,0,O,0,P),
        table_command(get(A,[P|Q]),J,R),
        S is L+P,
        T is S+O,
        mult_strees(F,N,I,R,K,T,M),
        Q is O+ (M-T),
        true.
mult_stree(A,B,B,C,C,D,D) :-
        directive(A),
        !,
        true.

mult_disj(fail,A,A,B,C,C,D,D).
mult_disj((A;B),C,D,E,F,G,H,I) :-
        mult_conj_head(A,C,J,E,F,K,H,L),
        mult_disj(B,J,D,E,K,G,L,I),
        true.

table_command(get(A,B),C,C) :-
        get(C,A,B).
table_command(fget(A,B),C,C) :-
        fget(C,A,B).
table_command(set(A,B),C,D) :-
        fset(C,A,B,D).
table_command(add(A,B),C,D) :-
        get(C,A,E),
        includev(B,E,F),
        fset(C,A,F,D),
        !.

mult_conj_head(true,A,A,B,C,C,D,D) :- !,
        true.
mult_conj_head((A ',' B),C,D,E,F,G,H,I) :-
        test(A),
        !,
        J is H+1,
        mult_conj_head(B,C,D,E,F,G,J,I),
        true.
mult_conj_head(A,B,C,D,E,F,G,G) :-
        mult_conj_body(A,B,C,D,E,F),
        true.

mult_conj_body(true,A,A,B,C,C).
mult_conj_body((A ',' B),C,D,E,F,G) :-
        mult_goal(A,C,H,E),
        I is F+1,
        mult_conj_body(B,H,D,E,I,G),
        true.

mult_goal(A,B,C,D) :-
        functor(A,E,F),
        G=E/F,
        D\==G,
        table_command(fget(G,H),B,I),
        J is H+1,
        table_command(set(G,J),I,C),
        !,
        true.
mult_goal(A,B,B,C).

init_mult_strees([],A,A).
init_mult_strees([A|B],C,D) :-
        init_mult_stree(A,C,E),
        init_mult_strees(B,E,D),
        true.

init_mult_stree(stree(A,(B:-C),D,E,F,G),H,I) :- !,
        table_command(get(A,0),H,J),
        init_mult_strees(F,J,I),
        true.
init_mult_stree(A,B,B) :-
        directive(A),
        !,
        true.

set_command(sub(A),B,C) :-
        excludev(A,B,C).
set_command(add(A),B,C) :-
        includev(A,B,C).
set_command(sub_set(A),B,C) :-
        diffv(B,A,C).
set_command(add_set(A),B,C) :-
        unionv(A,B,C).

create_mode_strees(A,B,C,D,E) :-
        create_mode_strees(A,B,C,D,E,top).

create_mode_strees([],[],A,B,C,D) :- !.
create_mode_strees([A|B],[C|D],E,F,G,H) :-
        create_mode_stree(A,C,E,F,G,H),
        create_mode_strees(B,D,E,F,G,H).

create_mode_stree(stree(A,B,(C:-D),E,F,G),stree(A,B,(C:-H),E,I,G),J,K,L,M) :- !,
        lattice_modes_table(A,J,C,N),
        lattice_modes_table(A,K,C,O),
        new_formula(C,N,O,B,L,P,Q,R),
        add_mode_option(analyze_mode(C,P,Q,R)),
        flat_conj((P ',' Q),H),
        write_mode(M,C),
        create_mode_strees(F,I,J,K,L,nontop).
create_mode_stree(A,A,B,C,D,E) :-
        directive(A).

lattice_modes_table(A/B,C,D,E) :-
        functor(D,A,B),
        get(C,A/B,F),
        lattice_modes_call(1,B,F,D,mem,E,true).

new_formula(A,B,C,D,E,F,G,H) :-
        require(A,I),
        before(A,J),
        split_unbound(I,K,L),
        combine_formula(K,J,M),
        update_mode(B,M,A,N),
        squeeze_conj(N,O),
        convert_uninit(D,E,O,P),
        split_uninit(P,Q,G),
        logical_subsume(B,L,R),
        combine_formula(Q,R,F),
        after(A,S),
        update_mode(C,S,A,T),
        squeeze_conj(T,H).

write_mode(top,A) :-
        fail, % \+compile_option(compile),
        !,
        require(A,B),
        before(A,C),
        after(A,D),
        survive(A,E),
        w(':- '),
        inst_writeq(mode(A,B,C,D,E)),
        wn('.').
write_mode(A,B) :-
        fail, % \+compile_option(compile),
        !,
        require(B,C),
        before(B,D),
        after(B,E),
        survive(B,F),
        w('%  '),
        inst_writeq(mode(B,C,D,E,F)),
        nl.
write_mode(A,B) :-
        true, % compile_option(compile),
        !,
        require(B,C),
        before(B,D),
        after(B,E),
        survive(B,F),
        w('% '),
        inst_writeq(mode(B,C,D,E,F)),
        nl.

update_mode(A,fail,B,fail) :- !.
update_mode(fail,A,B,fail) :- !.
update_mode(A,true,B,A) :- !.
update_mode(true,A,B,A) :- !.
update_mode((A ',' B),C,D,E) :- !,
        update_one(A,C,D,F),
        update_mode(B,F,D,E).

convert_uninit((A:-B),C,D,E) :-
        compile_option(analyze_uninit_reg),
        !,
        functor(A,F,G),
        get(C,F/G,H),
        convert_form(D,E,H).
convert_uninit(A,B,C,C).

update_one(fail,A,B,fail) :- !.
update_one(ground(A),B,C,D) :-
        implies(B,unbound(A)),
        !,
        incorrect_mode(A,C,ground(A),B,D).
update_one(nonvar(A),B,C,D) :-
        implies(B,unbound(A)),
        !,
        incorrect_mode(A,C,nonvar(A),B,D).
update_one(uninit(A),B,C,D) :-
        implies(B,nonvar(A)),
        !,
        incorrect_mode(A,C,uninit(A),B,D).
update_one(uninit_reg(A),B,C,D) :-
        implies(B,nonvar(A)),
        !,
        incorrect_mode(A,C,uninit_reg(A),B,D).
update_one(ground(A),B,C,B) :-
        pred_exists(ground(A),B),
        !.
update_one(nonvar(A),B,C,B) :-
        pred_exists(nonvar(A),B),
        !.
update_one(uninit(A),B,C,B) :-
        pred_exists(uninit(A),B),
        !.
update_one(uninit_reg(A),B,C,B) :-
        pred_exists(uninit_reg(A),B),
        !.
update_one(rderef(A),B,C,B) :-
        pred_exists(rderef(A),B),
        !.
update_one(uninit(A),B,C,D) :-
        pred_exists(var(A),B),
        !,
        D= (uninit(A)',' E),
        split_from_formula(A,B,E,F),
        squeeze_conj(F,G),
        warning(C,['Mode ',G,' of ',C,' replaced by ',uninit(A)]).
update_one(uninit_reg(A),B,C,D) :-
        pred_exists(var(A),B),
        !,
        D= (uninit_reg(A)',' E),
        split_from_formula(A,B,E,F),
        squeeze_conj(F,G),
        warning(C,['Mode ',G,' of ',C,' replaced by ',uninit_reg(A)]).
update_one(ground(A),B,C,(ground(A)',' B)) :- !.
update_one(nonvar(A),B,C,(nonvar(A)',' B)) :- !.
update_one(uninit(A),B,C,(uninit(A)',' B)) :- !.
update_one(uninit_reg(A),B,C,(uninit_reg(A)',' B)) :- !.
update_one(rderef(A),B,C,(rderef(A)',' B)) :- !.

incorrect_mode(A,B,C,D,(C ',' E)) :-
        split_from_formula(A,D,E,F),
        squeeze_conj(F,G),
        warning(B,['Mode ',G,' of ',B,' is incorrect.',nl,'Compilation continued with corrected mode ',C]).

convert_form((A ',' B),(C ',' D),E) :- !,
        convert_form(A,C,E),
        convert_form(B,D,E).
convert_form(uninit(A),uninit_reg(A),B) :-
        inv(A,B),
        !.
convert_form(uninit_mem(A),uninit_reg(A),B) :-
        inv(A,B),
        !.
convert_form(A,A,B).

convert_uninit_strees(A,B,C,D,E) :-
        compile_option(analyze_uninit_reg),
        !,
        % stats(a,1),
        init_convert(A,B,F,G,H),
        % stats(a,2),
        convert_closure(F,C,I,D,J,G,K,H,E).
convert_uninit_strees(A,B,C,D,E) :-
        seal(E).

init_convert(A,B,C,D,E) :-
        init_convert_strees(A,B,F,C,[],G,D,H,E),
        seal(D),
        seal(E).

convert_closure([],A,A,B,B,C,C,D,D).
convert_closure(A,B,C,D,E,F,G,H,I) :-
        cons(A),
        !,
        % stats(a,3),
        conv_preds(A,B,J,F,K,H,L,[],M),
        % stats(a,4),
        length(M,N),
        comment(['Uninit(reg) conversion pass--changed ',N,' predicates.']),
        new_preds(M,D,O,[],P),
        convert_closure(P,J,C,O,E,K,G,L,I),
        true.

conv_preds([],A,A,B,B,C,C,D,D).
conv_preds([A|B],C,D,E,F,G,H,I,J) :-
        table_command(fget(A,K),G,L),
        table_command(fget(A,(M:-N)),C,O),
        % stats(a,data4(A)),
        calc_convert_set(N,M,A,O,P,E,Q,L,R,I,S,K,T),
        % stats(a,5),
        update_ureg(A,K,T,R,U,S,V),
        % stats(a,6),
        conv_preds(B,P,D,Q,F,U,H,V,J),
        true.

new_preds([],A,A,B,B) :- !,
        true.
new_preds([entry(A)|B],C,D,E,F) :- !,
        set_command(add(A),E,G),
        new_preds(B,C,D,G,F),
        true.
new_preds([exit(A)|B],C,D,E,F) :-
        table_command(fget(A,G),C,H),
        !,
        set_command(add_set(G),E,I),
        new_preds(B,H,D,I,F),
        true.
new_preds([A|B],C,D,E,F) :-
        new_preds(B,C,D,E,F),
        true.

calc_convert_set(fail,A,B,C,C,D,D,E,E,F,F,G,G) :- !,
        true.
calc_convert_set((A;B),C,D,E,F,G,H,I,J,K,L,M,N) :-
        cons(M),
        split_conj_begin_end(A,O,P),
        last_conj(P,Q),
        !,
        varset(O,R),
        set_command(sub_set(R),M,S),
        term_dupset(P,T),
        set_command(sub_set(T),S,U),
        % stats(a,data7(D)),
        last_goal_ureg(Q,C,D,E,V,G,W,I,X,K,Y,U,Z),
        % stats(a,8),
        update_fast_goal(Q,C,D,V,A1,W,B1,X,C1,Y,D1),
        % stats(a,9),
        calc_convert_set(B,C,D,A1,F,B1,H,C1,J,D1,L,Z,N),
        true.
calc_convert_set((A;B),C,D,E,E,F,F,G,G,H,H,I,I).

update_ureg(A,B,C,D,E,F,G) :-
        B\==C,
        !,
        table_command(set(A,C),D,E),
        set_command(add(exit(A)),F,G),
        true.
update_ureg(A,B,C,D,D,E,E).

split_conj_begin_end(true,true,true).
split_conj_begin_end((A ',' B),true,(A ',' B)) :-
        all_survive(B),
        !.
split_conj_begin_end((A ',' B),(A ',' C),D) :-
        split_conj_begin_end(B,C,D).

term_dupset(A,B) :-
        term_dupset_varbag(A,B,C).

last_goal_ureg(A,B,C,D,D,E,E,F,F,G,G,H,H) :-
        survive(A),
        !,
        true.
last_goal_ureg(A,B,C/D,E,F,G,G,H,I,J,J,K,L) :-
        cons(K),
        functor(A,M,N),
        table_command(fget(M/N,O),H,I),
        table_command(fget(M/N,(P:-Q)),E,F),
        !,
        % stats(a,data10(M/N,P,A,O)),
        map_args(P,O,A,R),
        % stats(a,11),
        % stats(a,data12(R)),
        intersectv(R,K,S),
        % stats(a,13),
        min_integer(N,D,T),
        % stats(a,data14(N,D,T)),
        match_corresponding_args(1,T,B,A,S,L),
        % stats(a,15),
        true.
last_goal_ureg(A,B,C,D,D,E,E,F,F,G,G,H,[]) :-
        H=I,
        true.

update_fast_goal(A,B,C/D,E,F,G,H,I,J,K,L) :-
        \+survive(A),
        functor(A,M,N),
        table_command(fget(M/N,O),G,H),
        table_command(fget(M/N,P),I,Q),
        table_command(fget(M/N,(R:-S)),E,F),
        !,
        map_args(R,P,A,T),
        min_integer(N,D,U),
        match_corresponding_args(1,U,B,A,T,V),
        map_args(A,V,R,W),
        update_ureg(M/N,P,W,Q,J,K,L),
        true.
update_fast_goal(A,B,C,D,D,E,E,F,F,G,G).

map_args(A,B,C,D) :-
        functor(A,E,F),
        map_args(1,F,A,B,C,G,[]),
        sort(G,D).

match_corresponding_args(A,B,C,D,E,[]) :-
        A>B,
        !.
match_corresponding_args(A,B,C,D,E,F) :-
        A=<B,
        arg(A,C,G),
        inv(G,E),
        arg(A,D,H),
        G==H,
        !,
        F=[G|I],
        J is A+1,
        match_corresponding_args(J,B,C,D,E,I).
match_corresponding_args(A,B,C,D,E,F) :-
        A=<B,
        G is A+1,
        match_corresponding_args(G,B,C,D,E,F).

all_survive(true).
all_survive((A ',' B)) :-
        survive(A),
        all_survive(B).

init_convert_strees([],A,A,B,B,C,C,D,D).
init_convert_strees([A|B],C,D,E,F,G,H,I,J) :-
        A=stree(K,(L:-M),N,O,P,Q),
        !,
        E=[K|R],
        enter_fast(K,M,G,S),
        table_command(fget(K,T),C,U),
        get_argvars(uninit,L,T,V),
        table_command(get(K,W),I,X),
        enter_ureg(W,V),
        init_convert_strees(P,U,Y,R,Z,S,A1,X,B1),
        init_convert_strees(B,Y,D,Z,F,A1,H,B1,J),
        true.
init_convert_strees([A|B],C,D,E,F,G,H,I,J) :-
        directive(A),
        !,
        init_convert_strees(B,C,D,E,F,G,H,I,J),
        true.

enter_fast(A,B,C,D) :-
        fast_routine(B),
        !,
        table_command(get(A,dummy),C,D),
        true.
enter_fast(A,B,C,C).

get_argvars(A,B,C,D) :-
        functor(B,E,F),
        get_argvars(1,F,A,B,C,G,[]),
        sort(G,D).

enter_ureg(A,B) :-
        var(A),
        !,
        A=B.
enter_ureg(A,B) :-
        nonvar(A).

fast_routine((A;B)) :- !,
        fast_routine(A),
        fast_routine(B).
fast_routine((A ',' B)) :- !,
        fast_routine(A),
        fast_routine(B).
fast_routine(A) :-
        survive(A),
        !.
fast_routine(A) :-
        builtin(A),
        !.

init_tables(A,B,C,D,E) :-
        init_strees(A,F,B,G,C,H,D,I,E),
        seal(B),
        seal(C),
        seal(D),
        seal(E),
        !.

init_strees([],A,A,B,B,C,C,D,D).
init_strees([A|B],C,D,E,F,G,H,I,J) :-
        A=stree(K,(L:-M),N,O,P,Q),
        !,
        bottom_call(K,R),
        table_command(get(K,R),C,S),
        table_command(get(K,R),E,T),
        table_command(get(K,U),G,V),
        enter_def(U,(L:-M),K),
        init_disj(M,S,W,T,X,V,Y,I,Z,K),
        init_strees(P,W,A1,X,B1,Y,C1,Z,D1),
        init_strees(B,A1,D,B1,F,C1,H,D1,J),
        true.
init_strees([A|B],C,D,E,F,G,H,I,J) :-
        directive(A),
        !,
        init_strees(B,C,D,E,F,G,H,I,J),
        true.

bottom_call(A/B,C) :-
        functor(C,A,B),
        bottom_call(1,B,C).

init_disj(fail,A,A,B,B,C,C,D,D,E).
init_disj((A;B),C,D,E,F,G,H,I,J,K) :-
        init_conj(A,C,L,E,M,G,N,I,O,K),
        init_disj(B,L,D,M,F,N,H,O,J,K),
        true.

init_conj(true,A,A,B,B,C,C,D,D,E).
init_conj((A ',' B),C,D,E,F,G,H,I,J,K) :-
        init_goal(A,C,L,E,M,G,N,I,O,K),
        init_conj(B,L,D,M,F,N,H,O,J,K),
        true.

init_goal(A,B,C,D,E,F,F,G,H,I) :-
        call_p(A),
        !,
        functor(A,J,K),
        bottom_call(J/K,L),
        table_command(get(J/K,L),B,C),
        table_command(get(J/K,L),D,E),
        table_command(add(J/K,I),G,H),
        true.
init_goal(A,B,B,C,C,D,D,E,E,F) :-
        unify_p(A),
        !,
        true.

entry_data(A,B,C,D) :-
        (   bagof(E,entry_data(E),F) ->
            true
        ;   F=[]
        ),
        filter_defs(F,C,G,B),
        entry_zero(A,H,G),
        sort(H,D).

filter_defs([],[],[],A).
filter_defs([A|B],[A|C],[D/E|F],G) :-
        A=entry(H,I,J,K,L,M),
        functor(H,D,E),
        get(G,D/E,N),
        !,
        filter_defs(B,C,F,G).
filter_defs([A|B],C,D,E) :-
        filter_defs(B,C,D,E).

entry_zero([],A,A).
entry_zero([stree(A/0,B,C,D,E,F)|G],H,I) :- !,
        constC(H,A/0,J),
        entry_zero(G,J,K),
        entry_zero(E,K,I).
entry_zero([stree(A/B,C,D,E,F,G)|H],I,J) :-
        B>0,
        !,
        entry_zero(H,I,K),
        entry_zero(F,K,J).
entry_zero([A|B],C,D) :-
        entry_zero(B,C,D).

entry_data(entry(A,B,C,D,E,F)) :-
        compile_option(entry(A,G)),
        varset(A,B),
        ground_set(G,C),
        nonvar_set(G,D),
        uninit_set(G,E),
        rderef_set(G,F).

entry_init([],A,A).
entry_init([A|B],C,D) :-
        entry_init_one(A,C,E),
        entry_init(B,E,D),
        true.

entry_init_one(entry(A,B,C,D,E,F),G,H) :-
        update_entry(A,I,J,G,H,[],K,C,L,D,M,E,N,F,O,B,P),
        true.

update_entry(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        calc_entry(A,R,H,I,J,K,L,M,N,O,P,Q),
        functor(A,S,T),
        table_command(fget(S/T,U),D,V),
        lub_call(U,R,B),
        update_entry(S/T,U,B,C,V,E,F,G),
        true.

analyze(A,B) :-
        % stats(an,1),
        init_tables(A,C,D,E,F),
        entry_data(A,E,G,H),
        cons(H),
        !,
        entry_init(G,C,I),
        % stats(an,2),
        analyze_closure(H,I,J,D,K,E,L,M,N,F,O),
        % stats(an,3),
        seal(N),
        spec_strees(A,P,N),
        % stats(an,4),
        convert_uninit_strees(P,J,E,F,Q),
        % stats(an,5),
        wn('% Modes generated:'),
        create_mode_strees(P,R,J,K,Q),
        % stats(an,6),
        re_unr_strees(R,B).
        % stats(an,7).
analyze(A,A) :-
        warning(['There are no usable entry points, so no flow analysis was done.']).

analyze_closure([],A,A,B,B,C,C,D,D,E,E) :- !,
        true.
analyze_closure(A,B,C,D,E,F,G,H,I,J,K) :-
        cons(A),
        !,
        trav_preds(A,B,L,D,M,F,N,H,O,[],P),
        % stats(an,'2_5'),
        length(P,Q),
        comment(['Analysis pass--changed ',Q,' entry and exit modes.']),
        new_preds(P,J,R,[],S),
        analyze_closure(S,L,C,M,E,N,G,O,I,R,K),
        true.

spec_strees([],[],A).
spec_strees([A|B],[C|D],E) :-
        spec_stree(A,C,E),
        spec_strees(B,D,E).

trav_preds([],A,A,B,B,C,C,D,D,E,E) :- !,
        true.
trav_preds([A|B],C,D,E,F,G,H,I,J,K,L) :-
        table_command(fget(A,M),C,N),
        no_bottom(A,M),
        table_command(fget(A,O),G,P),
        !,
        copy(O,(Q:-R)),
        % stats(trav_pred,A),
        trav_pred(R,Q,M,A,N,S,E,T,P,U,I,V,K,W,[],X,[],Y,[],Z,[],A1,[],B1),
        trav_preds(B,S,D,T,F,U,H,V,J,W,L),
        true.
trav_preds([A|B],C,D,E,F,G,H,I,J,K,L) :-
        trav_preds(B,C,D,E,F,G,H,I,J,K,L),
        true.

no_bottom(A/B,C) :-
        bottom(D),
        no_bottom(B,C,D).

trav_pred(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :-
        new_sets(B,C,O,Y,Q,Z,S,A1,U,B1,W,C1),
        trav_disj(A,B,D,1,[],D1,E1,F1,E,F,G,G1,I,J,K,L,M,H1,Y,I1,Z,J1,A1,T,B1,K1,C1,L1),
        set_command(add_set(D1),I1,M1),
        set_command(add_set(E1),J1,N1),
        intersectv(B1,Y,O1),
        unionv(O1,F1,P1),
        K1=Q1,
        update_exit(B,G1,H,H1,N,M1,P,N1,R,F1,V,L1,X),
        true.

bottom(unknown).

no_bottom(0,A,B) :- !.
no_bottom(A,B,C) :-
        A>0,
        arg(A,B,D),
        D\==C,
        E is A-1,
        no_bottom(E,B,C).

new_sets(A,B,C,D,E,F,G,H,I,J,K,L) :-
        C=M,
        E=N,
        G=O,
        I=P,
        K=Q,
        get_argvars(ground,A,B,D),
        get_args(nonvar,A,B,F),
        get_argvars(uninit,A,B,H),
        get_argvars(rderef,A,B,J),
        varset(A,L),
        true.

trav_disj(fail,A,B,C,D,E,E,E,F,F,G,G,H,H,I,I,J,J,K,K,L,L,M,M,N,N,O,O) :- !,
        varset(A,E),
        true.
trav_disj((A;B),C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :-
        member(caller(D,E),F),
        !,
        D1 is E+1,
        trav_disj(B,C,D,D1,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1),
        true.
trav_disj((A;B),C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :-
        save_sets(D1,T,E1,V,F1,X,G1,Z,H1,B1,I1),
        term_dupset(A,J1),
        % stats(trav_conj,d(D,E)),
        trav_conj(A,D,E,1,[caller(D,E)|F],J,K1,L,L1,N,M1,P,N1,R,O1,E1,P1,F1,Q1,G1,R1,H1,S1,I1,T1,U1,[],J1),
        back_propagate(U1,P1,V1,Q1,W1,S1,X1),
        restore_sets(D1,V1,Y1,W1,Z1,R1,A2,X1,B2,T1,C2),
        D2 is E+1,
        trav_disj(B,C,D,D2,F,E2,F2,G2,K1,K,L1,M,M1,O,N1,Q,O1,S,Y1,U,Z1,W,A2,Y,B2,A1,C2,C1),
        intersectv(V1,E2,G),
        intersectv(W1,F2,H),
        intersectv(X1,G2,I),
        true.

update_exit(A,B,C,D,E,F,G,H,I,J,K,L,L) :-
        functor(A,M,N),
        table_command(fget(M/N,O),B,P),
        !,
        calc_exit(A,Q,F,G,H,I,J,K),
        lub_call(O,Q,R),
        update_exit(M/N,O,R,P,C,D,E),
        true.
update_exit(A,B,B,C,C,D,D,E,E,F,F,G,G).

save_sets(state(A,B,C,D,E),A,A,B,B,C,C,D,D,E,E).

trav_conj(true,A,B,C,D,E,E,F,F,G,G,H,H,I,I,J,J,K,K,L,L,M,N,O,O,P,P,Q) :- !,
        set_command(add_set(L),M,N),
        true.
trav_conj((A ',' B),C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1) :-
        varset(A,D1),
        % stats(trav_goal,d(C,D,E)),
        trav_goal(A,C,D,E,F,D1,G,E1,I,F1,K,G1,M,H1,O,I1,Q,J1,S,K1,U,L1,W,M1,Y,N1,A1,O1,C1),
        P1 is E+1,
        trav_conj(B,C,D,P1,F,E1,H,F1,J,G1,L,H1,N,I1,P,J1,R,K1,T,L1,V,M1,X,N1,Z,O1,B1,C1),
        true.

back_propagate(A,B,C,D,E,F,G) :-
        back_prop_d_cl(A,F,G),
        back_prop_g_cl(A,B,C),
        set_command(add_set(C),D,H),
        back_prop_n_cl(A,H,E),
        true.

restore_sets(state(A,B,C,D,E),F,A,G,B,H,C,I,D,J,E) :-
        F=K,
        G=L,
        H=M,
        I=N,
        J=O,
        true.

back_prop_d_cl(A,B,C) :-
        back_prop_d(A,D,B,E,0,F),
        back_prop_d_cl(F,D,E,C),
        true.

back_prop_g_cl(A,B,C) :-
        back_prop_g(A,D,B,E,0,F),
        back_prop_g_cl(F,D,E,C),
        true.

back_prop_n_cl(A,B,C) :-
        back_prop_n(A,D,B,E,0,F),
        back_prop_n_cl(F,D,E,C),
        true.

back_prop_d([],[],A,A,B,B) :- !,
        true.
back_prop_d([unify(yes,A,B,C)|D],E,F,G,H,I) :-
        subsetv(C,F),
        !,
        set_command(add(A),F,J),
        K is H+1,
        back_prop_d(D,E,J,G,K,I),
        true.
back_prop_d([unify(no,A,B,C)|D],E,F,G,H,I) :- !,
        back_prop_d(D,E,F,G,H,I),
        true.
back_prop_d([A|B],[A|C],D,E,F,G) :-
        back_prop_d(B,C,D,E,F,G),
        true.

back_prop_d_cl(0,A,B,B) :- !,
        true.
back_prop_d_cl(A,B,C,D) :-
        A>0,
        !,
        back_prop_d_cl(B,C,D),
        true.

back_prop_g([],[],A,A,B,B) :- !,
        true.
back_prop_g([unify(A,B,C,D)|E],F,G,H,I,J) :-
        nonvar(C),
        subsetv(D,G),
        !,
        set_command(add(B),G,K),
        L is I+1,
        back_prop_g(E,F,K,H,L,J),
        true.
back_prop_g([unify(A,B,C,D)|E],F,G,H,I,J) :-
        inv(B,G),
        !,
        set_command(add_set(D),G,K),
        L is I+1,
        back_prop_g(E,F,K,H,L,J),
        true.
back_prop_g([A|B],[A|C],D,E,F,G) :-
        back_prop_g(B,C,D,E,F,G),
        true.

back_prop_g_cl(0,A,B,B) :- !,
        true.
back_prop_g_cl(A,B,C,D) :-
        A>0,
        !,
        back_prop_g_cl(B,C,D),
        true.

back_prop_n([],[],A,A,B,B) :- !,
        true.
back_prop_n([unify(A,B,C,D)|E],F,G,H,I,J) :-
        var(B),
        var(C),
        inv(C,G),
        !,
        set_command(add(B),G,K),
        L is I+1,
        back_prop_n(E,F,K,H,L,J),
        true.
back_prop_n([A|B],[A|C],D,E,F,G) :-
        back_prop_n(B,C,D,E,F,G),
        true.

back_prop_n_cl(0,A,B,B) :- !,
        true.
back_prop_n_cl(A,B,C,D) :-
        A>0,
        !,
        back_prop_n_cl(B,C,D),
        true.

trav_goal(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,A1,B1) :-
        call_p(A),
        !,
        trav_call(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,C1,W,X,Y,D1),
        set_command(sub_set(F),C1,V),
        unionv(F,D1,Z),
        true.
trav_goal(A=B,C,D,E,F,G,H,H,I,I,J,J,K,K,L,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) :-
        make_uni(A,B,Z,Q,A1,U,B1,W,C1,Y),
        make_uni(B,A,D1,A1,E1,B1,F1,C1,X,Y),
        term_dupset(A=B,G1),
        trav_goal_d(A,B,G,G1,M,H1,O,I1,E1,J1,S,K1,F1,L1),
        trav_goal_u(A,B,G,H1,N,J1,M1,K1,T,L1,N1,Y),
        set_command(add_set(N),I1,O1),
        trav_goal_n(A,B,O1,P),
        set_command(sub_set(G1),M1,R),
        unionv(G,N1,V),
        true.

trav_call(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :-
        functor(A,A1,B1),
        table_command(fget(A1/B1,C1),K,D1),
        !,
        update_entry(A,E1,F1,G,G1,O,H1,Q,I1,S,J1,U,K1,W,L1,Y,M1),
        trav_def(A,A1/B1,C1,E,E1,F1,F,G1,H,I,J,D1,L,M,N,H1,P,I1,R,J1,T,K1,V,L1,X,M1,Z),
        true.
trav_call(A,B,C,D,E,F,G,G,H,I,J,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :-
        spec_goal(B,C,D,A,Y,O,Z,Q,A1,S,T,U,B1,W,C1,K,L),
        after(Y,D1),
        bindset(Y,E1),
        diffv(E1,Z,F1),
        set_command(sub_set(F1),B1,G1),
        rderef_set(D1,H1),
        set_command(add_set(H1),G1,I1),
        ground_set(D1,J1),
        set_command(add_set(J1),Z,K1),
        nonvar_set(D1,L1),
        set_command(add_set(L1),A1,M1),
        update_exit(Y,H,I,M,N,K1,P,M1,R,I1,V,C1,X),
        true.

make_uni(A,B,C,D,E,F,G,H,I,J) :-
        var(A),
        !,
        dref_prop_flag(A,K,D,E,F,G,H,L,J),
        varset(B,C),
        L=[unify(K,A,B,C)|I],
        true.
make_uni(A,B,C,D,D,E,E,F,F,G).

trav_goal_d(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-
        split_unify_v(A,B,O,P),
        varset(P,Q),
        trav_drf1(O,P,Q,C,E,F,G,H,I,J,K,L,M,N),
        !,
        true.
trav_goal_d(A,B,C,D,E,F,G,G,H,I,J,K,L,M) :-
        split_unify_v(A,B,N,O),
        varset(O,P),
        trav_drf2(N,O,P,C,D,E,F,H,I,J,K,L,M),
        !,
        true.
trav_goal_d(A,B,C,D,E,E,F,F,G,G,H,I,J,J) :- !,
        intersectv(E,H,I),
        true.

trav_goal_u(A,B,C,D,E,F,G,H,I,J,K,L) :-
        split_unify_v(A,B,M,N),
        varset(N,O),
        trav_unif(M,N,O,C,D,E,F,G,H,I,J,K,L),
        !,
        true.
trav_goal_u(A,B,C,D,D,E,F,G,G,H,H,I) :-
        set_command(sub_set(C),E,F),
        true.

trav_goal_n(A,B,C,D) :-
        split_unify_v(A,B,E,F),
        trav_non(E,F,C,D),
        !,
        true.
trav_goal_n(A,B,C,C).

dref_prop_flag(A,no,B,B,C,C,D,D,E) :-
        inv(A,E),
        !,
        true.
dref_prop_flag(A,yes,B,B,C,C,D,D,E) :-
        \+inv(A,C),
        !,
        true.
dref_prop_flag(A,yes,B,B,C,C,D,D,E) :-
        inv(A,B),
        !,
        true.
dref_prop_flag(A,no,B,B,C,C,D,D,E).

trav_non(A,B,C,D) :-
        nonvar(B),
        !,
        set_command(add(A),C,D),
        true.
trav_non(A,B,C,D) :-
        var(B),
        inv(B,C),
        !,
        set_command(add(A),C,D),
        true.

trav_drf1(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-
        \+inv(A,M),
        !,
        new_dref(A,B,C,E,F,G,H,I,O,K,P,M,Q),
        add_dref(C,O,J,P,L,Q,N),
        true.
trav_drf1(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-
        inv(A,I),
        !,
        new_dref(A,B,C,E,F,G,H,I,O,K,P,M,Q),
        add_dref(C,O,J,P,L,Q,N),
        true.

trav_drf2(A,B,C,D,E,F,F,G,H,I,J,K,L) :-
        inv(A,I),
        inv(A,F),
        !,
        add_dref(C,G,H,I,J,K,L),
        true.
trav_drf2(A,B,C,D,E,F,F,G,H,I,J,K,L) :-
        E=[],
        inv(A,I),
        intersectv(K,C,M),
        subsetv(M,G),
        !,
        add_dref(C,G,H,I,N,K,L),
        set_command(sub(A),N,J),
        true.

new_dref(A,B,C,D,D,E,E,F,F,G,H,I,I) :-
        var(B),
        intersectv(C,I,J),
        unionv(G,F,K),
        subsetv(J,K),
        !,
        set_command(add(A),G,H),
        true.
new_dref(A,B,C,D,D,E,E,F,F,G,H,I,I) :-
        nonvar(B),
        intersectv(C,I,J),
        unionv(E,D,K),
        intersectv(G,K,L),
        subsetv(J,L),
        !,
        set_command(add(A),G,H),
        true.
new_dref(A,B,C,D,D,E,E,F,F,G,G,H,H).

add_dref(A,B,B,C,D,E,E) :-
        diffv(E,B,F),
        diffv(A,F,G),
        set_command(add_set(G),C,D),
        true.

trav_unif(A,B,C,D,E,F,G,H,I,I,J,J,K) :-
        subsetv(C,E),
        !,
        set_command(add(A),E,F),
        set_command(sub(A),G,H),
        true.
trav_unif(A,B,C,D,E,E,F,G,H,H,I,I,J) :-
        inv(A,F),
        \+inv(A,J),
        !,
        diffv(D,I,K),
        set_command(add_set(K),F,L),
        set_command(sub(A),L,M),
        intersectv(D,I,N),
        set_command(sub_set(N),M,G),
        true.
trav_unif(A,B,C,D,E,E,F,G,H,H,I,I,J) :-
        \+inv(A,I),
        \+inv(A,J),
        !,
        diffv(D,I,K),
        set_command(add_set(K),F,L),
        set_command(sub(A),L,M),
        intersectv(D,I,N),
        set_command(sub_set(N),M,G),
        true.
trav_unif(A,B,C,D,E,F,G,H,I,I,J,J,K) :-
        inv(A,E),
        !,
        set_command(add_set(D),E,F),
        set_command(sub_set(D),G,H),
        true.

trav_def(A,B/C,D,E,F,no,G,H,H,I,J,K,K,L,L,M,M,N,O,P,Q,R,S,T,U,V,W) :- !,
        table_command(fget(B/C,X),I,J),
        get_argvars(rderef,A,X,Y),
        get_argvars(ground,A,X,Z),
        get_args(nonvar,A,X,A1),
        new_drf_gnd(Y,Z,A1,G,N,O,P,Q,R,S,T,U,V,W),
        true.
trav_def(A,B/C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1) :-
        copy(D,(C1:-D1)),
        save_sets(E1,S,F1,U,G1,W,H1,Y,I1,A1,J1),
        new_sets(C1,F,F1,K1,G1,L1,H1,M1,I1,N1,J1,O1),
        % stats(trav_disj,B/C),
        trav_disj(D1,C1,B/C,1,E,P1,Q1,R1,I,J,K,S1,M,N,O,P,Q,T1,K1,U1,L1,V1,M1,W1,N1,X1,O1,Y1),
        restore_sets(E1,U1,Z1,V1,A2,W1,B2,X1,C2,Y1,D2),
        map_argvars(C1,R1,A,E2),
        map_argvars(C1,P1,A,F2),
        map_args(C1,Q1,A,G2),
        new_drf_gnd(E2,F2,G2,H,Z1,H2,A2,I2,B2,X,C2,J2,D2,K2),
        update_exit(A,S1,L,T1,R,H2,T,I2,V,J2,Z,K2,B1),
        true.

spec_goal(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        modal_entry(D,R),
        !,
        S=index(A,B,C),
        table_command(get(S,T),P,U),
        functor(D,V,W),
        calc_entry(D,X,F,Y,H,Z,J,A1,L,B1,N,C1),
        spec_goal2(D,V/W,S,T,X,E,Y,G,Z,I,A1,K,B1,M,C1,O,U,Q),
        true.
spec_goal(A,B,C,D,D,E,E,F,F,G,G,H,H,I,I,J,J).

get_args(A,B,C,D) :-
        functor(B,E,F),
        get_args(1,F,A,B,C,G,[]),
        sort(G,D).

new_drf_gnd(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-
        new_drf_set(A,D,E,O,I,J,K,L,M,N),
        set_command(add_set(B),O,F),
        set_command(add_set(C),G,H),
        true.

map_argvars(A,B,C,D) :-
        functor(A,E,F),
        map_argvars(1,F,A,B,C,G,[]),
        sort(G,D).

new_drf_set([],A,B,B,C,C,D,E,F,F) :- !,
        intersectv(B,D,E),
        true.
new_drf_set(A,B,C,C,D,D,E,F,G,G) :-
        cons(A),
        !,
        E=H,
        unionv(D,H,I),
        diffv(B,G,J),
        unionv(J,I,K),
        intersectv(A,K,L),
        intersectv(C,H,M),
        unionv(L,M,F),
        true.

calc_entry(A,B,C,C,D,D,E,E,F,F,G,G) :-
        term_dupset(A,H),
        functor(A,I,J),
        functor(B,I,J),
        calc_entry_2(1,J,C,D,H,E,F,G,A,B),
        true.

spec_goal2(A,B,C,D,E,F,G,G,H,H,I,I,J,J,K,K,L,L) :-
        var(D),
        !,
        lattice_modes_entry(B,E,A,M),
        efficient_entry(A,F,M),
        D=value(A,E,F),
        true.
spec_goal2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :-
        nonvar(D),
        !,
        copy(D,value(A,S,T)),
        spec_update(A,B,C,S,T,E,F,G,H,I,J,K,L,M,N,O,P,Q,R),
        true.

lattice_modes_entry(A/B,C,D,E) :-
        lattice_modes_call(1,B,C,D,either,E,true).

efficient_entry(A,B,C) :-
        modal_entry(A,D),
        tree_trav_entry(D,C,B),
        !.
efficient_entry(A,A,B).

spec_update(A,B,C,D,E,F,G,H,H,I,I,J,J,K,K,L,L,M,N) :-
        D\==F,
        !,
        lattice_modes_entry(B,F,A,O),
        efficient_entry(A,G,O),
        table_command(set(C,value(A,F,G)),M,N),
        true.
spec_update(A,B,C,D,E,D,E,F,F,G,G,H,H,I,I,J,J,K,K).

spec_stree(stree(A,(B:-C),D,E,F,G),stree(A,(B:-H),D,E,I,G),J) :- !,
        spec_disj(C,H,A,1,J),
        spec_strees(F,I,J).
spec_stree(A,A,B) :-
        directive(A).

spec_disj(fail,fail,A,B,C).
spec_disj((A;B),(C;D),E,F,G) :-
        spec_conj(A,C,E,F,1,G),
        H is F+1,
        spec_disj(B,D,E,H,G).

spec_conj(true,true,A,B,C,D).
spec_conj((A ',' B),(C ',' D),E,F,G,H) :-
        spec_goal(A,C,E,F,G,H),
        I is G+1,
        spec_conj(B,D,E,F,I,H).

spec_goal(A,B,C,D,E,F) :-
        get(F,index(C,D,E),G),
        !,
        copy(G,value(A,H,B)).
spec_goal(A,A,B,C,D,E).

map_argvars(A,B,C,D,E,F,G) :-
        A=<B,
        arg(A,C,H),
        inv(H,D),
        !,
        arg(A,E,I),
        varbag(I,F,J),
        K is A+1,
        map_argvars(K,B,C,D,E,J,G).
map_argvars(A,B,C,D,E,F,G) :-
        A=<B,
        !,
        H is A+1,
        map_argvars(H,B,C,D,E,F,G).
map_argvars(A,B,C,D,E,F,F) :-
        A>B,
        !.

map_args(A,B,C,D,E,F,G) :-
        A=<B,
        arg(A,C,H),
        inv(H,D),
        arg(A,E,I),
        var(I),
        !,
        constC(F,I,J),
        K is A+1,
        map_args(K,B,C,D,E,J,G).
map_args(A,B,C,D,E,F,G) :-
        A=<B,
        !,
        H is A+1,
        map_args(H,B,C,D,E,F,G).
map_args(A,B,C,D,E,F,F) :-
        A>B,
        !.

get_argvars(A,B,C,D,E,F,G) :-
        A=<B,
        arg(A,E,H),
        greater_eq(C,H),
        !,
        arg(A,D,I),
        varbag(I,F,J),
        K is A+1,
        get_argvars(K,B,C,D,E,J,G).
get_argvars(A,B,C,D,E,F,G) :-
        A=<B,
        !,
        H is A+1,
        get_argvars(H,B,C,D,E,F,G).
get_argvars(A,B,C,D,E,F,F) :-
        A>B,
        !.

greater_eq(ground,gnddrf) :- !.
greater_eq(rderef,gnddrf) :- !.
greater_eq(rderef,nondrf) :- !.
greater_eq(nonvar,gnddrf) :- !.
greater_eq(nonvar,nondrf) :- !.
greater_eq(nonvar,ground) :- !.
greater_eq(A,A) :- !.
greater_eq(A,unknown) :- !.
greater_eq(any,A) :- !.

get_args(A,B,C,D,E,F,G) :-
        A=<B,
        arg(A,E,H),
        greater_eq(C,H),
        arg(A,D,I),
        var(I),
        !,
        constC(F,I,J),
        K is A+1,
        get_args(K,B,C,D,E,J,G).
get_args(A,B,C,D,E,F,G) :-
        A=<B,
        !,
        H is A+1,
        get_args(H,B,C,D,E,F,G).
get_args(A,B,C,D,E,F,F) :-
        A>B,
        !.

lub_call(A,B,C) :-
        functor(A,D,E),
        functor(B,D,E),
        functor(C,D,E),
        lub_call(1,E,A,B,C).

update_entry(A,B,C,yes,D,E,F,G) :-
        B\==C,
        !,
        table_command(set(A,C),D,E),
        set_command(add(entry(A)),F,G),
        true.
update_entry(A,B,C,no,D,D,E,E).

calc_exit(A,B,C,C,D,D,E,E) :-
        functor(A,F,G),
        functor(B,F,G),
        calc_exit_2(1,G,C,D,E,A,B),
        true.

update_exit(A,B,C,D,E,F,G) :-
        B\==C,
        !,
        table_command(set(A,C),D,E),
        set_command(add(exit(A)),F,G),
        true.
update_exit(A,B,C,D,D,E,E).

calc_exit_2(A,B,C,D,E,F,G) :-
        A>B,
        !.
calc_exit_2(A,B,C,D,E,F,G) :-
        A=<B,
        !,
        arg(A,F,H),
        arg(A,G,I),
        varset(H,J),
        subset_flag(J,C,K),
        subset_flag(J,E,L),
        calc_exit_arg(K,L,M),
        fix_nonvar(H,D,M,I),
        N is A+1,
        calc_exit_2(N,B,C,D,E,F,G).

subset_flag(A,B,yes) :-
        subsetv(A,B),
        !.
subset_flag(A,B,no).

calc_exit_arg(yes,yes,gnddrf) :- !.
calc_exit_arg(yes,no,ground) :- !.
calc_exit_arg(no,yes,rderef) :- !.
calc_exit_arg(no,no,any) :- !.

fix_nonvar(A,B,C,D) :-
        nonvar(A),
        !,
        add_nonvar_info(yes,C,D).
fix_nonvar(A,B,C,D) :-
        var(A),
        !,
        membership_flag(A,B,E),
        add_nonvar_info(E,C,D).

calc_entry_2(A,B,C,D,E,F,G,H,I,J) :-
        A>B,
        !.
calc_entry_2(A,B,C,D,E,F,G,H,I,J) :-
        A=<B,
        !,
        arg(A,I,K),
        arg(A,J,L),
        varset(K,M),
        subset_flag(M,C,N),
        var_flag(K,O),
        diffv(H,F,P),
        unionv(P,E,Q),
        membership_flag(K,Q,R),
        intersectv(M,H,S),
        subset_flag(S,G,T),
        unionv(C,D,U),
        intersectv(G,U,V),
        subset_flag(S,V,W),
        calc_entry_arg(N,O,R,T,W,X),
        fix_nonvar(K,D,X,L),
        Y is A+1,
        calc_entry_2(Y,B,C,D,E,F,G,H,I,J).

var_flag(A,yes) :-
        var(A),
        !.
var_flag(A,no) :-
        nonvar(A),
        !.

membership_flag(A,B,yes) :-
        inv(A,B),
        !.
membership_flag(A,B,no).

calc_entry_arg(yes,A,B,yes,C,gnddrf) :- !.
calc_entry_arg(yes,A,B,no,C,ground) :- !.
calc_entry_arg(no,no,A,B,yes,rderef) :- !.
calc_entry_arg(no,no,A,B,no,any) :- !.
calc_entry_arg(no,yes,no,A,B,uninit) :- !.
calc_entry_arg(no,yes,yes,yes,A,rderef) :- !.
calc_entry_arg(no,yes,yes,no,A,any) :- !.

add_nonvar_info(no,A,A) :- !.
add_nonvar_info(yes,rderef,nondrf) :- !.
add_nonvar_info(yes,any,nonvar) :- !.
add_nonvar_info(yes,A,A) :- !.

lub(unknown,A,A) :- !.
lub(A,unknown,A) :- !.
lub(any,A,any) :- !.
lub(A,any,any) :- !.
lub(A,A,A) :- !.
lub(nonvar,ground,nonvar) :- !.
lub(ground,nonvar,nonvar) :- !.
lub(nonvar,nondrf,nonvar) :- !.
lub(nondrf,nonvar,nonvar) :- !.
lub(nonvar,gnddrf,nonvar) :- !.
lub(gnddrf,nonvar,nonvar) :- !.
lub(ground,nondrf,nonvar) :- !.
lub(nondrf,ground,nonvar) :- !.
lub(ground,gnddrf,ground) :- !.
lub(gnddrf,ground,ground) :- !.
lub(nondrf,gnddrf,nondrf) :- !.
lub(gnddrf,nondrf,nondrf) :- !.
lub(rderef,nondrf,rderef) :- !.
lub(nondrf,rderef,rderef) :- !.
lub(rderef,gnddrf,rderef) :- !.
lub(gnddrf,rderef,rderef) :- !.
lub(rderef,uninit,rderef) :- !.
lub(uninit,rderef,rderef) :- !.
lub(uninit,gnddrf,rderef) :- !.
lub(gnddrf,uninit,rderef) :- !.
lub(uninit,nondrf,rderef) :- !.
lub(nondrf,uninit,rderef) :- !.
lub(nonvar,rderef,any) :- !.
lub(rderef,nonvar,any) :- !.
lub(nonvar,uninit,any) :- !.
lub(uninit,nonvar,any) :- !.
lub(ground,rderef,any) :- !.
lub(rderef,ground,any) :- !.
lub(ground,uninit,any) :- !.
lub(uninit,ground,any) :- !.
lub(A,B,any) :-
        error(['Bug in lub with ',lub(A,B,C)]).

lub_call(A,B,C,D,E) :-
        A>B,
        !.
lub_call(A,B,C,D,E) :-
        A=<B,
        !,
        arg(A,C,F),
        arg(A,D,G),
        arg(A,E,H),
        lub(F,G,H),
        I is A+1,
        lub_call(I,B,C,D,E).

bottom_call(A,B,C) :-
        A>B,
        !.
bottom_call(A,B,C) :-
        A=<B,
        !,
        bottom(D),
        arg(A,C,D),
        E is A+1,
        bottom_call(E,B,C).

lattice_modes_call(A,B,C,D,E,F,F) :-
        A>B,
        !.
lattice_modes_call(A,B,C,D,E,F,G) :-
        A=<B,
        !,
        arg(A,C,H),
        arg(A,D,I),
        lattice_modes_arg(H,E,I,F,J),
        K is A+1,
        lattice_modes_call(K,B,C,D,E,J,G).

lattice_modes_arg(uninit,mem,A,B,C) :- !,
        co(uninit(A),B,C).
lattice_modes_arg(uninit,A,B,C,D) :- !,
        co(uninit(A,B),C,D).
lattice_modes_arg(ground,A,B,C,D) :- !,
        co(ground(B),C,D).
lattice_modes_arg(rderef,A,B,C,D) :- !,
        co(rderef(B),C,D).
lattice_modes_arg(gnddrf,A,B,C,D) :- !,
        co(ground(B),C,E),
        co(rderef(B),E,D).
lattice_modes_arg(nonvar,A,B,C,D) :- !,
        co(nonvar(B),C,D).
lattice_modes_arg(nondrf,A,B,C,D) :- !,
        co(nonvar(B),C,E),
        co(rderef(B),E,D).
lattice_modes_arg(unknown,A,B,C,D) :- !,
        co(fail,C,D).
lattice_modes_arg(any,A,B,C,C).

disjoint_flag(A,B,yes) :-
        disjointv(A,B),
        !.
disjoint_flag(A,B,no).

var_args(A,B) :-
        A=..[C|D],
        filter_vars(D,E),
        sort(E,B).

term_dupset_varbag(A,B,C) :-
        varbag(A,C),
        filter_dups(C,B).

term_dupset_varset(A,B,C) :-
        term_dupset_varbag(A,B,D),
        sort(D,C).

create_bodies(fail,fail,[],[],[],A,B,C,C) :- !.
create_bodies((A;B),(C;D),E,F,G,H,I,J,K) :-
        segment_test(A,L,M,H,I),
        (   length_test_user(L,N,O),
            P=1, Q=2, % compile_option(user_test_size(P,Q)),
            O=<P,
            N=<Q ->
            flat_conj((M ',' L),C),
            J=R,
            E=S,
            F=T,
            G=U
        ;   flat_conj(L,V),
            varset(M,W),
            varset(V,X),
            C= (M ',' '$body'((Y:-V),X,Z)),
            J=[body((Y:-V),Z)|R],
            E=[Y|S],
            F=[W|T],
            G=[X|U]
        ),
        create_bodies(B,D,S,T,U,H,I,R,K).

create_heads([],A,B,[],[]).
create_heads([A|B],C,D,[E|F],[G|H]) :-
        intersectv(E,G,I),
        diffv(I,D,J),
        unionv(E,D,K),
        intersectv(K,G,L),
        new_head(L,[36,98,95],C,J,A),
        create_heads(B,C,D,F,H).

new_head(A,B,C,D,E) :-
        cons(A),
        !,
        new_head(B,C,D,E).
new_head(A,B,C,D,E) :-
        nil(A),
        !,
        functor(C,F,G),
        gensym(B,F/G,E).

segment_test(A,B,C,D,E) :-
        F=true,
        B=G,
        H=I,
        J=K,
        segment_1(A,K,C,F,G,H,I,J,[],L,D,M,E,N).

st(A,B,C) :-
        varset(B,D),
        segment_test(A,E,F,D,C),
        wn(F),
        wn(E).

segment_1(true,true,A,A,B,B,C,C,D,D,E,E,F,F) :- !,
        true.
segment_1(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-
        next_goal(A,O,P),
        split_unify(O,Q,R),
        var(Q),
        atomic(R),
        implies(M,nonvar(Q)),
        !,
        C= ('$name_arity'(Q,R,0)',' S),
        update_formula('$name_arity'(Q,R,0),M,T),
        segment_1(P,B,S,D,E,F,G,H,I,J,K,L,T,N),
        true.
segment_1(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-
        next_goal(A,O,P),
        test(O),
        \+standard_order(O),
        varset(O,Q),
        intersectv(Q,I,[]),
        subsetv(Q,K),
        bindset(O,M,[]),
        !,
        add_name_arity(O,C,R,M,S),
        R= (O ',' T),
        update_formula(O,S,U),
        segment_1(P,B,T,D,E,F,G,H,I,J,K,L,U,N),
        true.
segment_1(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-
        next_goal(A,O,P),
        split_unify(O,Q,R),
        var(Q),
        compound(R),
        implies(M,nonvar(Q)),
        varset(R,S),
        intersectv(S,K,[]),
        !,
        functor(R,T,U),
        C= ('$name_arity'(Q,T,U)',' V),
        V= (O ',' W),
        M=X,
        update_formula(O,K,X,Y),
        unionv([Q],K,Z),
        unionv(S,Z,A1),
        segment_1(P,B,W,D,E,F,G,H,I,J,A1,L,Y,N),
        true.
segment_1(A,A,B,C,D,D,E,E,F,F,G,G,H,I) :-
        next_goal(A,J,K),
        split_unify(J,L,M),
        var(L),
        compound(M),
        implies(H,nonvar(L)),
        !,
        functor(M,N,O),
        B= ('$name_arity'(L,N,O)',' C),
        update_formula('$name_arity'(L,N,O),H,I),
        true.
segment_1(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-
        next_goal(A,O,P),
        split_unify(O,Q,R),
        var(Q),
        implies(M,var(Q)),
        !,
        E= (O ',' S),
        includev(Q,I,T),
        segment_1(P,B,C,D,S,F,G,H,T,J,K,L,M,N),
        true.
segment_1(A,B,C,D,E,F,G,H,I,J,K,L,M,N) :-
        next_goal(A,O,P),
        split_unify(O,Q,R),
        var(Q),
        implies(M,uninit(any,Q)),
        !,
        G= (O ',' S),
        includev(Q,I,T),
        segment_1(P,B,C,D,E,F,S,H,T,J,K,L,M,N),
        true.
segment_1(A,A,B,B,C,C,D,D,E,E,F,F,G,G) :- !,
        true.

next_goal((A ',' B),A,B) :- !.
next_goal(A,A,true).

add_name_arity(A,B,C,D,E) :-
        split_unify(A,F,G),
        var(F),
        nonvar(G),
        implies(D,nonvar(F)),
        !,
        functor(G,H,I),
        B= ('$name_arity'(F,H,I)',' C),
        update_formula('$name_arity'(F,H,I),D,E),
        true.
add_name_arity(A,B,B,C,C).

segment_all_disj(fail,A,B,C,fail,D,D,[]).
segment_all_disj((A;B),C,D,E,(F;G),H,I,J) :-
        segment_all_conj(A,K,L,E,H,M),
        varset(K,N),
        (   length_test_user(L,O,P),
            Q=1, R=2, % compile_option(user_test_size(Q,R)),
            P=<Q,
            O=<R ->
            F= (K ',' L),
            J=S
        ;   varset(L,T),
            intersectv(N,T,U),
            diffv(U,D,V),
            new_head([36,115,95],C,V,W),
            F= (K ',' '$body'((W:-L),T,X)),
            J=[body((W:-L),X)|S]
        ),
        segment_all_disj(B,C,D,E,G,M,I,S).

segment_all_conj(true,true,true,A,B,B) :- !.
segment_all_conj((A ',' B),(A ',' C),D,E,F,G) :-
        A= (H=I),
        filter_vars([H,I],J),
        sort(J,K),
        disjointv(K,E),
        !,
        insert(K,F,L),
        segment_all_conj(B,C,D,E,L,G).
segment_all_conj((A ',' B),(A ',' C),D,E,F,G) :-
        test(A),
        !,
        segment_all_conj(B,C,D,E,F,G).
segment_all_conj((A ',' B),true,(A ',' B),C,D,D) :-
        \+test(A),
        !.

selection_3(A,B,C,D,E,F) :-
        length_disj(A,G),
        G>1,
        % stats(s,1),
        find_testset(A,B,C,D,E,H,I),
        % stats(s,2),
        pick_testset(I,H,J),
        !,
        % stats(s,3),
        thin_testset(J,K),
        % stats(s,4),
        code_testset(K,B,C,D,E,A,F),
        % stats(s,5),
        (   fail -> % compile_option(debug) ->
            write('Selection code:'),
            nl,
            write(F),
            nl
        ;   true
        ),
        !.
selection_3(A,B,C,D,E,A).

selection(A,B,C,D,E,F) :-
        simplify(A,G),
        selection_3(G,B,C,D,E,F),
        !.

find_testset(A,B,C,D,E,F,G) :-
        unbound_set(E,H),
        pred_gather(A,B,C,D,H,E,1,F,[]),
        keysort(F,I),
        part_testset(I,G).

pick_testset(A,B,C) :-
        first_test(B,D-E),
        comment(['First test is ',D-E]),
        rel_flag(D,F),
        pick_eq(none-[],0,A,C,G,F),
        G>0,
        !.

thin_testset(A-B,A-C) :-
        show_tests(B,D),
        keysort(D,E),
        make_thin(E,C),
        !.

code_testset(A-B,C,D,E,F,G,'$case'(H,I,J)) :-
        A=key(H,I),
        get_clause_nums(B,K,[]),
        sort(K,L),
        extract_else_disj(L,1,G,M,N),
        full_testset(H,I,B,O),
        (   H=var ->
            P=G
        ;   P=M
        ),
        code_testset(O,C,[A|D],E,F,[],N,M,P,J).

pred_gather((A;B),C,D,E,F,G,H,I,J) :- !,
        flat_conj(A,K),
        clause_gather(K,C,A,D,E,F,G,H,1,L,I,M),
        N is H+1,
        pred_gather(B,C,D,E,F,G,N,M,J).
pred_gather(A,B,C,D,E,F,G,H,I) :-
        flat_conj(A,J),
        clause_gather(J,B,A,C,D,E,F,G,1,K,H,I).

part_testset(A,[B-[C|D]|E]) :-
        A=[B-C|F],
        !,
        one_testset(B,C,F,D,G),
        part_testset(G,E).
part_testset([],[]).

clause_gather(true,A,B,C,D,E,F,G,H,H,I,I).
clause_gather((A ',' B),C,D,E,F,G,H,I,J,K,L,L) :-
        cut_p(A),
        !,
        J=K.
clause_gather((A ',' B),C,D,E,F,G,H,I,J,K,L,M) :-
        one_clause_gather(A,C,D,E,F,G,H,I,J,N,L,O),
        !,
        clause_gather(B,C,D,E,F,G,H,I,N,K,O,M).
clause_gather((A ',' B),C,D,E,F,G,H,I,J,K,L,M) :-
        N is J+1,
        clause_gather(B,C,D,E,F,G,H,I,N,K,L,M).

one_clause_gather(A,B,C,D,E,F,G,H,I,J,K,L) :-
        J is I+1,
        bagof(key(M,N)-val(A,O,H,I,P,Q),valid_testset(A,B,G,M,N,P,O,D,E,F),R),
        !,
        make_correct(A,C,R),
        difflist(R,K,L).

make_correct(A,B,[C-val(A,D,E,F,G,B)|H]) :- !,
        make_correct(A,B,H).
make_correct(A,B,[]).

valid_testset(A,B,C,D,E,F,G,H,I,J) :-
        internal_testset(A,C,D,E,F,K),
        logical_simplify(K,G),
        relational_check(A,C,D),
        \+memberv(key(D,E),H),
        \+implies(C,G),
        survive_form(G,C),
        varset(A,L),
        subsetv(L,I),
        varset(E,M),
        disjointv(M,J),
        valid_option(A,B,L).

internal_testset(A,B,C,D,E,F) :-
        bindbag(A,B,[]),
        testset(A,F,B,C,D,E).
internal_testset(A=B,C,equal,v(A,B),true,'$equal'(A,B)) :-
        implies(C,(atomic(A)',' atomic(B))),
        !.
internal_testset(A,B,C,D,E,F) :-
        \+unify_p(A),
        bindbag(A,B,[]),
        testset(A,F,B,C,D,E).

relational_check(A,B,C) :-
        \+relational_test(A,D,E),
        !.
relational_check(A,B,C) :-
        relational_test(A,D,E),
        relational_testset(C),
        !.
relational_check(A,B,C) :-
        relational_test(A,D,E),
        \+relational_testset(C),
        logical_simplify((integer(D)',' integer(E)),F),
        implies(B,F),
        !.

valid_option(A,B,C) :-
        goal_type(B,A,D),
        test_option(D),
        !.

goal_type(A,B,unify) :-
        split_unify_v_nv(B,C,D),
        !.
goal_type(A,var(B),unify) :-
        select_option(A,C),
        !.
goal_type(A,nonvar(B),unify) :-
        select_option(A,C),
        !.
goal_type(A,'$name_arity'(B,C,D),unify) :- !.
goal_type(A,B,arith) :-
        encode_relop(B,C,D,E,arith),
        !.
goal_type(A,B,typecheck).

test_option(unify) :-
        fail, % compile_option(test),
        fail, % compile_option(test_unify),
        !.
test_option(arith) :-
        fail, % compile_option(test),
        fail, % compile_option(test_arith),
        !.
test_option(typecheck) :-
        fail, % compile_option(test),
        fail, % compile_option(test_typecheck),
        !.
test_option(A) :-
        true. % \+compile_option(test).

firstarg_option(A,B,C) :-
        fail, % compile_option(firstarg),
        \+unify_p(B),
        !,
        arg(1,A,D),
        memberv(D,C).
firstarg_option(A,B,C) :-
        fail, % compile_option(firstarg),
        unify_p(B),
        !,
        split_unify(B,D,E),
        arg(1,A,F),
        D==F.
firstarg_option(A,B,C).

testset(A,B,C,D,E,F) :-
        testset(A,B,C,D,E,F,G).

relational_testset(comparison(A,arith)).

one_testset(A,B,[C-D|E],F,G) :-
        A==C,
        B==D,
        !,
        one_testset(A,D,E,F,G).
one_testset(A,B,[C-D|E],[D|F],G) :-
        A==C,
        B\==D,
        !,
        one_testset(A,D,E,F,G).
one_testset(A,B,[C-D|E],[],[C-D|E]) :-
        A\==C,
        !.
one_testset(A,B,[],[],[]).

first_test(A,B) :-
        A=[C|D],
        C=E-val(F,G,H,I,J,K),
        first_test(D,H,I,C,B).

rel_flag(key(A,B),yes) :-
        relational_testset(A),
        !.
rel_flag(key(A,B),no) :-
        \+relational_testset(A),
        !.

pick_eq(A,B,[C-D|E],F,G,H) :- !,
        (   rel_flag(C,I),
            I=H,
            goodness_testset(C,D,J),
            J>B ->
            pick_eq(C-D,J,E,F,G,H)
        ;   pick_eq(A,B,E,F,G,H)
        ).
pick_eq(A,B,[],A,B,C).

goodness_testset(key(A,B),C,D) :-
        number_of_direcs(C,E),
        goodness_key(A,F),
        (   A=hash(G),
            H=5, % compile_option(hash_size(H)),
            E<H ->
            D=0
        ;   D is 1000*E+F
        ),
        !,
        comment(['Goodness of ',A,' is ',D]).

first_test([],A,B,C,C).
first_test([A|B],C,D,E,F) :-
        A=G-val(H,I,J,K,L,M),
        (   J<C
        ;   J=:=C,
            K<D
        ),
        !,
        first_test(B,J,K,A,F).
first_test([A|B],C,D,E,F) :-
        first_test(B,C,D,E,F).

show_tests([],[]).
show_tests([val(A,B,C,D,E,F)|G],[[E|B]-val(A,B,C,D,E,F)|H]) :-
        show_tests(G,H).

make_thin([A|B],C) :-
        make_thin(A,B,C).

show_clnum([],[]).
show_clnum([val(A,B,C,D,E,F)|G],[C-val(A,B,C,D,E,F)|H]) :-
        show_clnum(G,H).

make_thin(A-B,[],[B]).
make_thin(A-B,[C-D|E],F) :-
        A==C,
        !,
        make_thin(C-D,E,F).
make_thin(A-B,[C-D|E],[B|F]) :-
        A\==C,
        !,
        make_thin(C-D,E,F).

get_clause_nums([],A,A).
get_clause_nums([val(A,B,C,D,E,F)|G],H,I) :-
        constC(H,C,J),
        get_clause_nums(G,J,I).

extract_else_disj([A|B],C,(D;E),(D;F),[C|G]) :-
        C<A,
        !,
        H is C+1,
        extract_else_disj([A|B],H,E,F,G).
extract_else_disj([A|B],A,(C;D),E,F) :- !,
        G is A+1,
        extract_else_disj(B,G,D,E,F).
extract_else_disj([A],A,B,fail,[]).
extract_else_disj([],A,B,B,C) :-
        number_disj(B,A,C).

full_testset(switch(A),v(B),C,D) :- !,
        switch_testset_list(A,B,E),
        update_testset(C,E,C,D).
full_testset(A,B,C,C).

code_testset([val(A,B,C,D,E,F)|G],H,I,J,K,L,M,N,O,('$test'(E,P,B,Q,R);S)) :-
        needset(B,J,T),
        bindset(B,K,Q),
        restore_block(T,L,P,U),
        save_block(Q,U,true),
        unionv(L,Q,V),
        merge_disj_list(M,C,N,F,W),
        subsume(B,W,X),
        else(B,O,Y),
        (   fail ->
            R=X
        ;   unionv(T,J,Z),
            logical_subsume(B,K,A1),
            logical_simplify((B ',' A1),B1),
            selection(X,H,I,Z,B1,R)
        ),
        code_testset(G,H,I,J,K,V,M,N,Y,S),
        !.
code_testset([],A,B,C,D,E,F,G,H,'$else'(I,E,J)) :-
        restore_block(E,E,I,true),
        selection(H,A,B,C,D,J).

needset(A,B,C) :-
        varset(A,D),
        intersectv(B,D,C).

restore_block([A|B],[C|D],E,F) :-
        A@<C,
        restore_block(B,[C|D],E,F).
restore_block([A|B],[C|D],E,F) :-
        A==C,
        co(restore(A),E,G),
        restore_block(B,D,G,F).
restore_block([A|B],[C|D],E,F) :-
        A@>C,
        restore_block([A|B],D,E,F).
restore_block(A,[],B,B).
restore_block([],A,B,B).

save_block([A|B],C,D) :-
        co(save(A),C,E),
        save_block(B,E,D).
save_block([],A,A).

merge_disj_list(A,none,B,C,B) :- !.
merge_disj_list([A|B],C,(D;E),F,(D;G)) :-
        A<C,
        !,
        merge_disj_list(B,C,E,F,G).
merge_disj_list(A,B,C,D,(D;C)).

number_disj(fail,A,[]) :- !.
number_disj((A;B),C,[C|D]) :- !,
        E is C+1,
        number_disj(B,E,D).
number_disj(A,B,[B]).

peep_flat(A,B) :-
        true, % compile_option(flat),
        !,
        peep_flat(A,B,[]).
peep_flat(A,A) :-
        fail. % \+compile_option(flat).

inst_labl(A,B,B) :-
        varset(B,C),
        inst_labl_list(C,A).

xpeephole(A,B,C) :-
        write_debug('peephole started',B,D),
        D=E, % stats(p,1,D,E),
        peep_flat(E,F),
        F=G, % stats(p,2,F,G),
        inst_labl(A,G,H),
        H=I, % stats(p,3,H,I),
        insert_error_jump(I,J),
        J=K, % stats(p,4,J,K),
        peep_simp(K,L),
        L=M, % stats(p,5,L,M),
        peep_closure(A,M,N),
        N=C, % stats(p,6,N,C),
        !.

insert_error_jump(A,B) :-
        insert_error_jump(A,B,[]).

peep_simp([],[]).
peep_simp([A|B],C) :-
        peep_simp(A,B,C).

peep_closure(A,B,C) :-
        peep_seq(A,B,D),
        peep_end_closure(A,B,D,C).

peep_seq(A,B,C) :-
        B=D, % stats(p1,1,B,D),
        peep_uniq(D,E),
        E=F, % stats(p1,2,E,F),
        peep_dead(F,G),
        G=H, % stats(p1,3,G,H),
        peep_jump_closure(A,H,I),
        peep_inst(call,I,J),
        peep_jump_closure(A,J,K),
        K=L, % stats(p1,4,K,L),
        peep_labl(L,M),
        M=N, % stats(p1,5,M,N),
        synonym(N,O),
        O=P, % stats(p1,6,O,P),
        peep_inst(call,P,Q),
        Q=C, % stats(p1,7,Q,C),
        !.

peep_end_closure(A,B,C,D) :-
        B=C,
        !,
        D=B.
peep_end_closure(A,B,C,D) :-
        peep_closure(A,C,D).

peep_uniq(A,B) :-
        make_large_blocks(A,C),
        reverse(C,D),
        make_unique_array(D,E,0,F),
        (   F>0 ->
            comment(['Number of duplicate blocks = ',F]),
            map_blocks(A,E,B)
        ;   A=B
        ).

peep_dead(A,B) :-
        gather_closure(A,C),
        peep_dead(A,C,B).

peep_jump_closure(A,B,C) :-
        peep_jump(A,B,D),
        peep_jump_end_closure(A,B,D,C).

peep_inst(A,B,C) :-
        make_block_array(B,D),
        peep_inst(B,D,A,C,[]).

peep_labl(A,B) :-
        branch_dest_set(A,C),
        list_to_key(C,D),
        create_array(D,E),
        remove_label_inst(A,E,B).

synonym(A,B) :-
        synonym(A,B,[]),
        !.

wc(A,A) :-
        write_code(A).

inst_labl_list([],A).
inst_labl_list([l(A,B)|C],A) :-
        gennum(B),
        inst_labl_list(C,A).

insert_error_jump([],A,A).
insert_error_jump([error_jump(A,B)|C],D,E) :- !,
        error_jump_moves(B,F,G,D,H),
        G=[],
        local_allocate(F),
        constC(H,jump(A),I),
        insert_error_jump(C,I,E).
insert_error_jump([A|B],C,D) :-
        constC(C,A,E),
        insert_error_jump(B,E,D).

error_jump_moves(A,B,C,D,E) :-
        error_jump_1(A,F,B,G,D,H),
        low_reg(I),
        error_jump_2(F,I,G,C,H,E).

local_allocate(A) :-
        local_allocate(A,B,C).

error_jump_1([],[],A,A,B,B).
error_jump_1([A|B],[C|D],[pref,A,C|E],F,G,H) :-
        constC(G,move(A,C),I),
        error_jump_1(B,D,E,F,I,H).

error_jump_2([],A,B,B,C,C).
error_jump_2([A|B],C,[pref,A,r(C)|D],E,F,G) :-
        constC(F,move(A,r(C)),H),
        I is C+1,
        error_jump_2(B,I,D,E,H,G).

make_large_blocks([],[]).
make_large_blocks([label(A)|B],[A-C|D]) :-
        first_block(B,C),
        \+has_label(C),
        \+small_block(C),
        !,
        make_large_blocks(B,D).
make_large_blocks([A|B],C) :-
        make_large_blocks(B,C).

make_unique_array([],A,B,B) :-
        seal(A).
make_unique_array([A-B|C],D,E,F) :-
        get(D,B,G),
        !,
        (   var(G) ->
            G=A,
            H=E
        ;   H is E+1
        ),
        make_unique_array(C,D,H,F).
make_unique_array([A|B],C,D,E) :-
        make_unique_array(B,C,D,E).

map_blocks([],A,[]).
map_blocks([label(A)|B],C,D) :-
        first_block(B,E,F),
        fget(C,E,G),
        A\==G,
        !,
        D=[label(A),jump(G)|H],
        map_blocks(F,C,H).
map_blocks([A|B],C,D) :-
        branch(A),
        \+distant_branch(A),
        \+B=[label(E)|F],
        first_block(B,G,H),
        fget(C,G,I),
        !,
        comment(['Able to remove a block after ',A]),
        D=[A,jump(I)|J],
        map_blocks(H,C,J).
map_blocks([A|B],C,[A|D]) :-
        map_blocks(B,C,D).

first_block([A],[A],[]) :- !.
first_block([A|B],[A|C],D) :-
        \+end_block(A),
        !,
        first_block(B,C,D).
first_block([A|B],[A],B) :-
        end_block(A),
        !.

first_block(A,B) :-
        first_block(A,B,C).

has_label([label(A)|B]).
has_label([A|B]) :-
        has_label(B).

small_block([jump(A)]).
small_block([return]).
small_block([fail]).

peep_jump(A,B,C) :-
        make_block_array(B,D),
        non_empty_array(D),
        !,
        (   fail -> % compile_option(rearr_debug) ->
            write_code(B)
        ;   true
        ),
        get_uniq_labels(B,E),
        rearr_jump(B,A,E,D,F),
        peep_dead(F,G),
        peep_choice(G,A,E,D,C).
peep_jump(A,B,B).

peep_jump_end_closure(A,B,C,D) :-
        B=C,
        !,
        D=B.
peep_jump_end_closure(A,B,C,D) :-
        peep_jump_closure(A,C,D).

make_block_array(A,B) :-
        make_blocks(A,C),
        create_array(C,B).

get_uniq_labels(A,B) :-
        get_label_bag(A,C,[]),
        filter_uniq(C,D),
        length(D,E),
        comment(['Number of unique labels = ',E]),
        create_array(D,B).

rearr_jump([],A,B,C,[]).
rearr_jump([jump(A)|B],C,D,E,F) :-
        \+B=[label(A)|G],
        fget(E,A,H),
        \+no_opt_block(H),
        first_block(H,I),
        (   fget(D,A,J),
            \+last(I,jump(A))
        ;   short_block(I),
            \+last(I,jump(K))
        ),
        !,
        insert_block(I,C,F,L),
        rearr_jump(B,C,D,E,L).
rearr_jump([call(A)|B],C,D,E,F) :-
        \+B=[return,label(A)|G],
        fget(E,A,H),
        first_block(H,I),
        (   fget(D,A,J)
        ;   short_block(I)
        ),
        \+has_recursion(I,A),
        returns_or_fails(I,K),
        !,
        insert_block(K,C,F,L),
        rearr_jump(B,C,D,E,L).
rearr_jump([A|B],C,D,E,F) :-
        branch(A,[G]),
        fget(E,G,H),
        merge_branch(A,H,I,F,J),
        !,
        inst_labl_list(I,C),
        rearr_jump(B,C,D,E,J).
rearr_jump([A|B],C,D,E,F) :-
        map_branch(A,G,H,I),
        fget(E,G,J),
        (   J=[jump(I)|K]
        ;   J=[fail|L],
            I=fail
        ),
        !,
        F=[H|M],
        rearr_jump(B,C,D,E,M).
rearr_jump([A|B],C,D,E,[A|F]) :-
        rearr_jump(B,C,D,E,F).

peep_choice(A,B,C,D,E) :-
        rearr_choice(A,C,D,F,G),
        (   F\==[] ->
            repl_choice(G,F,E,0,H),
            comment(['Number of choice-fail replacements = ',H])
        ;   G=E
        ).

get_label_bag([],A,A) :- !.
get_label_bag([A|B],C,D) :-
        branch(A,C,E),
        !,
        get_label_bag(B,E,D).
get_label_bag([A|B],C,D) :-
        get_label_bag(B,C,D).

get_label_set(A,B) :-
        get_label_bag(A,C,[]),
        sort(C,B).

rearr_choice([],A,B,[],[]).
rearr_choice([choice(1/A,B,C),fail|D],E,F,G,[jump(C)|H]) :-
        fget(E,C,I),
        fget(F,C,J),
        J=[choice(K/L,M,N)|O],
        1<K,
        K=<L,
        !,
        (   K=:=L ->
            G=[rep(C,P,P)|Q]
        ;   G=[rep(C,[choice(1/L,M,N)|P],P)|Q]
        ),
        rearr_choice(D,E,F,Q,H).
rearr_choice([A|B],C,D,E,[A|F]) :-
        rearr_choice(B,C,D,E,F).

repl_choice([],A,[],B,B).
repl_choice([label(A),B|C],D,[label(A)|E],F,G) :-
        member(rep(A,E,H),D),
        !,
        I is F+1,
        repl_choice(C,D,H,I,G).
repl_choice([A|B],C,[A|D],E,F) :-
        repl_choice(B,C,D,E,F).

no_opt_block([pragma(push(cons))|A]).
no_opt_block([pragma(push(structure(A)))|B]).
no_opt_block([unify_atomic(A,B,fail)|C]).

short_block(A) :-
        B=6, % compile_option(short_block(B)),
        shorter_than(A,B).

insert_block(A,B,C,D) :-
        get_labels(A,E,[]),
        assoc_labels(E,F),
        inst_labl_list(F,B),
        replace_labels(A,E,F,C,D).

has_recursion([call(A)|B],A) :- !.
has_recursion([A|B],C) :-
        has_recursion(B,C).

returns_or_fails(A,B) :-
        returns_or_fails(A,B,[]).

merge_branch(test(A,B,C,D),[test(A,B,C,E)|F],[],G,H) :-
        constC(G,test(A,B,C,E),H).
merge_branch(test(ne,A,B,C),[switch(D,B,fail,E,F)|G],[H],I,J) :-
        constC(I,switch(D,B,H,E,F),K),
        constC(K,label(H),J),
        tag(var,A).
merge_branch(choice(A/B,C,D),[jump(E)|F],[],G,H) :-
        constC(G,choice(A/B,C,E),H).
merge_branch(jump(A),[test(B,C,D,E),label(F)|G],[],H,I) :-
	true,
        % (   mips
        % ;   sparc
        % ;   mc68020
        % ),
        constC(H,test(B,C,D,E),J),
        constC(J,jump(F),I).
merge_branch(jump(A),[switch(B,C,D,E,F)|G],[],H,I) :-
	true,
        % (   mips
        % ;   sparc
        % ;   mc68020
        % ),
        constC(H,switch(B,C,D,E,F),I).
merge_branch(jump(A),[deref(B,C),test(D,E,C,F),label(G)|H],[],I,J) :-
	true,
        % (   mips
        % ;   sparc
        % ;   mc68020
        % ),
        constC(I,deref(B,C),K),
        constC(K,test(D,E,C,F),L),
        constC(L,jump(G),J).
merge_branch(jump(A),[deref(B,C),switch(D,C,E,F,G)|H],[],I,J) :-
	true,
        % (   mips
        % ;   sparc
        % ;   mc68020
        % ),
        constC(I,deref(B,C),K),
        constC(K,switch(D,C,E,F,G),J).
merge_branch(jump(A),[jump(B,C,D,E),label(F)|G],[],H,I) :-
	true,
        % (   mips
        % ;   sparc
        % ;   mc68020
        % ),
        constC(H,jump(B,C,D,E),J),
        constC(J,jump(F),I).

make_blocks([],[]).
make_blocks([label(A)|B],[A-B|C]) :- !,
        make_blocks(B,C).
make_blocks([procedure(A)|B],[A-B|C]) :- !,
        make_blocks(B,C).
make_blocks([A|B],C) :-
        make_blocks(B,C).

basic_block(A,A) :-
        A=[label(B)|C].
basic_block([A|B],C) :-
        basic_block(B,C).

end_block(A) :-
        distant_branch(A).

get_labels([],A,A) :- !.
get_labels([label(A)|B],C,D) :- !,
        constC(C,A,E),
        get_labels(B,E,D).
get_labels([A|B],C,D) :-
        get_labels(B,C,D).

assoc_labels([],[]) :- !.
assoc_labels([A|B],[C|D]) :-
        assoc_labels(B,D).

replace_labels([],A,B,C,C) :- !.
replace_labels([A|B],C,D,E,F) :-
        replace_labels_one(A,C,D,E,G),
        replace_labels(B,C,D,G,F).

replace_labels_one(label(A),B,C,[label(D)|E],E) :-
        memberv2(A,B,D,C),
        !.
replace_labels_one(A,B,C,[D|E],E) :-
        map_branches(A,F,D,G),
        !,
        map_terms(B,C,F,G).
replace_labels_one(A,B,C,[A|D],D).

returns_or_fails([fail],A,B) :- !,
        constC(A,fail,B).
returns_or_fails([return],A,A) :- !.
returns_or_fails([call(A)|B],C,D) :- !,
        constC(C,call(A),E),
        returns_or_fails(B,E,D).
returns_or_fails([A|B],C,D) :-
        \+branch(A),
        !,
        constC(C,A,E),
        returns_or_fails(B,E,D).
returns_or_fails([A|B],C,D) :-
        pure_branch(A,E),
        all_fails(E),
        !,
        constC(C,A,F),
        returns_or_fails(B,F,D).

all_fails([]).
all_fails([A|B]) :-
        nonvar(A),
        A=fail,
        all_fails(B).

gather_closure(A,B) :-
        gather_closure(A,[],B).

peep_dead([],A,[]).
peep_dead([A|B],C,[A|D]) :-
        distant_branch(A),
        !,
        to_next_label(B,C,E),
        peep_dead(E,C,D).
peep_dead([A|B],C,[A|D]) :-
        peep_dead(B,C,D).

to_next_label([],A,[]).
to_next_label([A|B],C,D) :-
        (   A=label(E),
            member(E,C) ->
            D=[A|B]
        ;   to_next_label(B,C,D)
        ).

gather_closure(A,B,C) :-
        gather_labels(A,B,D),
        sort(D,E),
        gather_end_closure(A,B,E,C).

gather_labels([],A,A).
gather_labels([A|B],C,D) :-
        branch(A,E,C),
        !,
        next_code(A,B,F,E),
        gather_labels(F,E,D).
gather_labels([label(A)|B],C,D) :-
        member(A,C),
        !,
        gather_labels(B,C,D).
gather_labels([A|B],C,D) :-
        gather_labels(B,C,D).

gather_end_closure(A,B,C,D) :-
        B=C,
        !,
        D=C.
gather_end_closure(A,B,C,D) :-
        gather_closure(A,C,D).

next_code(A,B,C,D) :-
        distant_branch(A),
        !,
        to_next_label(B,D,C).
next_code(A,B,B,C).

branch_dest_set(A,B) :-
        branch_dest_bag(A,C),
        sort(C,B).

remove_label_inst([],A,[]).
remove_label_inst([A|B],C,D) :-
        (   A=label(E),
            \+fget(C,E,F) ->
            D=G
        ;   D=[A|G]
        ),
        remove_label_inst(B,C,G).

branch_dest_bag([],[]).
branch_dest_bag([A|B],C) :-
        (   branch(A,C,D) ->
            branch_dest_bag(B,D)
        ;   branch_dest_bag(B,C)
        ).

peep_inst([],A,B,C,C).
peep_inst([choice(1/A,B,C)|D],E,F,G,H) :-
        success_to_cut(D,E),
        !,
        peep_inst(D,E,F,G,H).
peep_inst([choice(A/B,C,D)|E],F,G,H,I) :-
        1<A,
        A<B,
        success_to_cut(E,F),
        !,
        peep_inst([choice(B/B,C,fail)|E],F,G,H,I).
peep_inst([call(A)|B],C,call,D,E) :-
        rearr_dealloc(B,A,F),
        !,
        peep_inst(F,C,call,D,E).
peep_inst([jump(A)|B],C,D,E,F) :-
        lbl(A),
        contains_label(B,A),
        !,
        peep_inst(B,C,D,E,F).
peep_inst([A,B,C|D],E,call,F,G) :-
        cpeep3(A,B,C,H,D),
        !,
        peep_inst(H,E,call,F,G).
peep_inst([A,B,C,D|E],F,G,H,I) :-
        peep3r(B,C,D,J,E),
        !,
        peep_inst([A|J],F,G,H,I).
peep_inst([A,B,C|D],E,F,G,H) :-
        peep3r(A,B,C,I,D),
        !,
        peep_inst(I,E,F,G,H).
peep_inst([A,B,C|D],E,F,G,H) :-
        peep3(A,B,C,I,D),
        !,
        peep_inst(I,E,F,G,H).
peep_inst([A,B,C|D],E,F,G,H) :-
        peep2r(B,C,I,D),
        !,
        peep_inst([A|I],E,F,G,H).
peep_inst([A,B|C],D,E,F,G) :-
        peep2r(A,B,H,C),
        !,
        peep_inst(H,D,E,F,G).
peep_inst([A,B|C],D,E,F,G) :-
        peep2(A,B,H,C),
        !,
        peep_inst(H,D,E,F,G).
peep_inst([A,B|C],D,E,F,G) :-
        peep1r(B,F,H),
        !,
        peep_inst([A|C],D,E,H,G).
peep_inst([A|B],C,D,E,F) :-
        peep1r(A,E,G),
        !,
        peep_inst(B,C,D,G,F).
peep_inst([A|B],C,D,E,F) :-
        peep1(A,E,G),
        !,
        peep_inst(B,C,D,G,F).
peep_inst([A|B],C,D,E,F) :-
        constC(E,A,G),
        peep_inst(B,C,D,G,F).

peep_simp(move(A,A),B,C) :- !,
        peep_simp(B,C).
peep_simp(nop,A,B) :- !,
        peep_simp(A,B).
peep_simp(A,B,[A|C]) :-
        peep_simp(B,C).

success_to_cut([cut(A)|B],C) :- !.
success_to_cut([move(A,r(b))|B],C) :- !,
        fail.
success_to_cut([jump(A)|B],C) :-
        fget(C,A,D),
        !,
        success_to_cut(D,C).
success_to_cut([call(A)|B],C) :-
        fget(C,A,D),
        !,
        success_to_cut(D,C).
success_to_cut([A|B],C) :-
        local_instr(A),
        !,
        success_to_cut(B,C).

rearr_dealloc([deallocate(A)|B],C,[deallocate(A)|D]) :- !,
        rearr_dealloc(B,C,D).
rearr_dealloc([return|A],B,[jump(B)|A]).

lbl(fail).
lbl(A/B) :-
        atom(A),
        integer(B),
        B>=0.
lbl(l(A/B,C)) :-
        atom(A),
        integer(B),
        B>=0,
        integer(C).

contains_label([label(A)|B],A) :- !.
contains_label([label(A)|B],C) :-
        contains_label(B,C).
contains_label([pragma(A)|B],C) :-
        contains_label(B,C).

cpeep3(call(A),label(B),return,C,D) :-
        constC(C,jump(A),E),
        constC(E,label(B),F),
        constC(F,return,D).

peep3r(test(A,B,C,D),E,test(A,B,C,F),G,H) :-
        no_mod(E,C),
        \+E=label(I),
        constC(G,test(A,B,C,D),J),
        constC(J,E,H).
peep3r(A,label(B),A,C,D) :-
        distant_branch(A),
        constC(C,label(B),E),
        constC(E,A,D).
peep3r(jump(A,B,C,D),jump(E),label(D),F,G) :-
        cond(A,H),
        lbl(E),
        constC(F,jump(H,B,C,E),I),
        constC(I,label(D),G).
peep3r(jump_nt(A,B,C,D),jump(E),label(D),F,G) :-
        cond(A,H),
        lbl(E),
        constC(F,jump_nt(H,B,C,E),I),
        constC(I,label(D),G).
peep3r(jump(A,B,C,D),fail,label(D),E,F) :-
        cond(A,G),
        constC(E,jump(G,B,C,fail),H),
        constC(H,label(D),F).
peep3r(jump_nt(A,B,C,D),fail,label(D),E,F) :-
        cond(A,G),
        constC(E,jump_nt(G,B,C,fail),H),
        constC(H,label(D),F).
peep3r(test(A,B,C,D),fail,label(D),E,F) :-
        eq_ne(A,G),
        constC(E,test(G,B,C,fail),H),
        constC(H,label(D),F).
peep3r(test(A,B,C,D),jump(E),label(D),F,G) :-
        eq_ne(A,H),
        lbl(E),
        constC(F,test(H,B,C,E),I),
        constC(I,label(D),G).
peep3r(move(A^B,C),adda(B,D,B),move(C,[C]),E,F) :-
        constC(E,move(A^B,C),G),
        constC(G,push(C,B,D),F),
        tag(A),
        integer(D),
        reg(C),
        reg(B),
        C\==B.
peep3r(move(A,B),move(B,C),move(D,B),E,F) :-
        reg(B),
        \+is_in(B,C),
        \+is_in(B,D),
        constC(E,move(A,C),G),
        constC(G,move(D,B),F).
peep3r(pragma(tag(A,B)),pragma(align(A,C)),move(D,r(void)),E,E) :-
        is_in(A,D).
peep3r(pragma(tag(A,B)),pragma(align(A,C)),deref(D,r(void)),E,E) :-
        is_in(A,D).

peep3(A,label(B),fail,C,D) :-
        local_instr(A),
        \+A=cut(E),
        \+A=fail,
        constC(C,fail,F),
        constC(F,label(B),G),
        constC(G,fail,D).
peep3(pragma(tag(A,B)),move(C,D),move(C,E),F,G) :-
        reg(E),
        ind(D,A),
        a_var(A),
        \+reg(C),
        \+is_in(E,D),
        constC(F,move(C,E),H),
        constC(H,pragma(tag(A,B)),I),
        constC(I,move(E,D),G).
peep3(move(A,B),pragma(tag(A,C)),move(B,[B]),D,E) :-
        perm(A),
        reg(B),
        constC(D,move(A,B),F),
        constC(F,pragma(tag(B,C)),G),
        constC(G,move(B,[B]),E).
peep3(move(A^r(h),B),push(B,r(h),C),deref(B,D),E,F) :-
        reg(B),
        a_var(D),
        tag(A),
        constC(E,move(A^r(h),B),G),
        constC(G,push(B,r(h),C),H),
        constC(H,move(B,D),F).

peep2r(add(A,B,C),test(ne,D,C,E),F,G) :-
        constC(F,add(A,B,C),G),
        tag(integer,D).
peep2r(sub(A,B,C),test(ne,D,C,E),F,G) :-
        constC(F,sub(A,B,C),G),
        tag(integer,D).
peep2r(div(A,B,C),test(ne,D,C,E),F,G) :-
        constC(F,div(A,B,C),G),
        tag(integer,D).
peep2r(mul(A,B,C),test(ne,D,C,E),F,G) :-
        constC(F,mul(A,B,C),G),
        tag(integer,D).
peep2r(pragma(A),pragma(A),B,C) :-
        constC(B,pragma(A),C).
peep2r(pragma(tag(A,B)),deref(C,D),E,F) :-
        \+ind(C),
        \+ind(D),
        constC(E,deref(C,D),F).
peep2r(allocate(A),deallocate(A),B,B).
peep2r(jump_nt(A,B,C,D),label(D),E,F) :-
        constC(E,label(D),F).
peep2r(A,B,C,D) :-
        distant_branch(A),
        \+B=label(E),
        constC(C,A,D).
peep2r(label(A),label(A),B,C) :-
        constC(B,label(A),C).
peep2r(move(A,B),move(B,A),C,D) :-
        \+is_in(B,A),
        constC(C,move(A,B),D).
peep2r(move(A,B),move(C,B),D,E) :-
        \+is_in(B,C),
        constC(D,move(C,B),E).
peep2r(move(A,B),move(A,B),C,D) :-
        \+is_in(B,A),
        constC(C,move(A,B),D).
peep2r(return,return,A,B) :-
        constC(A,return,B).
peep2r(move(A,B),jump(C/D),E,F) :-
        reg(B,G),
        low_reg(H),
        G>=D+H,
        functor(I,C,D),
        \+survive(I),
        constC(E,jump(C/D),F).
peep2r(move(A,B),call(C/D),E,F) :-
        reg(B,G),
        low_reg(H),
        G>=D+H,
        functor(I,C,D),
        \+survive(I),
        constC(E,call(C/D),F).
peep2r(A,fail,B,C) :-
        local_instr(A),
        \+A=cut(D),
        constC(B,fail,C).
peep2r(A,fail,B,C) :-
        pure_branch(A,D),
        all_fails(D),
        constC(B,fail,C).
peep2r(pragma(A),fail,B,C) :-
        constC(B,fail,C).
peep2r(fail,fail,A,B) :-
        constC(A,fail,B).
peep2r(fail,A,B,C) :-
        \+A=label(D),
        constC(B,fail,C).
peep2r(pair(A,B),C,D,E) :-
        \+C=pair(F,G),
        \+C=label(H),
        constC(D,pair(A,B),E).
peep2r(choice(1/A,B,C),cut(D),E,F) :-
        constC(E,cut(D),F).
peep2r(choice(A/B,C,D),fail,E,F) :-
        constC(E,jump(D),F),
        A>1,
        A<B.
peep2r(test(ne,A,B,C),equal(B,A^D,C),E,F) :-
        tag(atom,A),
        constC(E,equal(B,A^D,C),F).
peep2r(test(A,B,C,D),label(D),E,F) :-
        constC(E,label(D),F).
peep2r(equal(A,B,C),label(C),D,E) :-
        constC(D,label(C),E).
peep2r(push(A,B,C),adda(B,D,B),E,F) :-
        merge_add,
        constC(E,push(A,B,G),F),
        G is C+D.
peep2r(push(A,r(h),B),pad(C),D,E) :-
        merge_add,
        constC(D,push(A,r(h),F),E),
        F is B+C.
peep2r(adda(A,B,A),adda(A,C,A),D,E) :-
        merge_add,
        constC(D,adda(A,F,A),E),
        F is B+C.
peep2r(pad(A),pad(B),C,D) :-
        constC(C,pad(E),D),
        E is A+B.
peep2r(test(eq,A,B,C),switch(D,B,E,F,G),H,I) :-
        tag(var,A),
        constC(H,switch(D,B,C,F,G),I).
peep2r(test(eq,A,B,C),switch(A,B,D,E,F),G,H) :-
        constC(G,switch(A,B,D,C,F),H).
peep2r(test(eq,A,B,C),switch(D,B,E,F,C),G,H) :-
        tag(var,I),
        A\==I,
        A\==D,
        constC(G,switch(D,B,E,F,C),H).
peep2r(test(A,B,C,D),label(D),E,F) :-
        constC(E,label(D),F),
        D\==fail.
peep2r(test(A,B,C,D),test(A,B,C,E),F,G) :-
        constC(F,test(A,B,C,D),G).
peep2r(pragma(tag(A,B)),move(C,r(void)),D,D) :-
        is_in(A,C).
peep2r(pragma(tag(A,B)),deref(C,r(void)),D,D) :-
        is_in(A,C).
peep2r(move(A,B),deref(B,B),C,D) :-
        reg(B),
        (   integer(A)
        ;   A=E^F,
            tag(var,G),
            E\==G
        ),
        constC(C,move(A,B),D).

peep2(fadd(A,B,C),test(ne,D,C,E),F,G) :-
        constC(F,fadd(A,B,C),H),
        constC(H,jump(E),G),
        tag(integer,D).
peep2(fsub(A,B,C),test(ne,D,C,E),F,G) :-
        constC(F,fsub(A,B,C),H),
        constC(H,jump(E),G),
        tag(integer,D).
peep2(fdiv(A,B,C),test(ne,D,C,E),F,G) :-
        constC(F,fdiv(A,B,C),H),
        constC(H,jump(E),G),
        tag(integer,D).
peep2(fmul(A,B,C),test(ne,D,C,E),F,G) :-
        constC(F,fmul(A,B,C),H),
        constC(H,jump(E),G),
        tag(integer,D).
peep2(test(A,B,C,fail),jump(D),E,F) :-
        lbl(D),
        constC(E,test(G,B,C,D),H),
        constC(H,fail,F),
        eq_ne(A,G).
peep2(move(A,B),move(B,C),D,E) :-
        perm(B),
        reg(C),
        constC(D,move(A,C),F),
        constC(F,move(C,B),E).
peep2(deref(A,B),move(B,C),D,E) :-
        perm(B),
        reg(C),
        constC(D,deref(A,C),F),
        constC(F,move(C,B),E).
peep2(deref(A,B),deref(B,C),D,E) :-
        constC(D,deref(A,B),F),
        constC(F,move(B,C),E).
peep2(move(A,B),move(A,C),D,E) :-
        reg(C),
        perm(B),
        \+reg(A),
        \+is_in(B,A),
        constC(D,move(A,C),F),
        constC(F,move(C,B),E).
peep2(move(A,B),move(A,C),D,E) :-
        reg(C),
        ind(B),
        \+reg(A),
        \+is_in(C,B),
        constC(D,move(A,C),F),
        constC(F,move(C,B),E).
peep2(move(A,B),move(B,C),D,E) :-
        reg(A),
        \+reg(C),
        ind(B),
        constC(D,move(A,B),F),
        constC(F,move(A,C),E).
peep2(jump(A,B,C,fail),jump(D),E,F) :-
        cond(A,G),
        lbl(D),
        constC(E,jump(G,B,C,D),H),
        constC(H,fail,F).
peep2(jump_nt(A,B,C,fail),jump(D),E,F) :-
        cond(A,G),
        lbl(D),
        constC(E,jump_nt(G,B,C,D),H),
        constC(H,fail,F).
peep2(switch(A,B,fail,C,C),label(C),D,E) :-
        constC(D,test(eq,F,B,fail),G),
        constC(G,label(C),E),
        tag(var,F).
peep2(switch(A,B,C,fail,fail),label(C),D,E) :-
        constC(D,test(ne,F,B,fail),G),
        constC(G,label(C),E),
        tag(var,F).
peep2(switch(A,B,fail,C,fail),label(C),D,E) :-
        constC(D,test(ne,A,B,fail),F),
        constC(F,label(C),E).
peep2(switch(A,B,C,fail,C),label(C),D,E) :-
        constC(D,test(eq,A,B,fail),F),
        constC(F,label(C),E).
peep2(switch(A,B,C,D,C),label(C),E,F) :-
        constC(E,test(eq,A,B,D),G),
        constC(G,label(C),F).
peep2(switch(A,B,C,D,C),label(D),E,F) :-
        constC(E,test(ne,A,B,C),G),
        constC(G,label(D),F).
peep2(switch(A,B,C,D,D),label(C),E,F) :-
        constC(E,test(ne,G,B,D),H),
        constC(H,label(C),F),
        tag(var,G).
peep2(switch(A,B,C,D,D),label(D),E,F) :-
        constC(E,test(eq,G,B,C),H),
        constC(H,label(D),F),
        tag(var,G).
peep2(A,adda(B,C,B),D,E) :-
        merge_add,
        constC(D,adda(B,C,B),F),
        constC(F,G,E),
        inc_reg(A,B,C,G).
peep2(A,pad(B),C,D) :-
        constC(C,pad(B),E),
        constC(E,F,D),
        inc_reg(A,r(h),B,F).
peep2(jump(A,B,C,D),test(E,F,G,H),I,J) :-
        (   G=B
        ;   G=C
        ),
        cond_ftest(A,K),
        tag_ftest(F,L),
        ftest(E,K,L,H,M),
        constC(I,jump(A,B,C,D),N),
        constC(N,M,J).
peep2(A,test(B,C,D,E),F,G) :-
        arith_ftest(A,H,I,J,K),
        (   D=I
        ;   D=J
        ;   D=K
        ),
        tag_ftest(C,L),
        ftest(B,H,L,E,M),
        constC(F,A,N),
        constC(N,M,G).

peep1r(test(A,B,C,D),E,E) :-
        get_tag(C,F),
        eq_ne(A,G),
        tmatch(G,B,F).
peep1r(nop,A,A).
peep1r(cut(r(b)),A,A).
peep1r(move(A,A),B,B).
peep1r(unify(A,A,B,C,D),E,E).
peep1r(equal(A,A,B),C,C).
peep1r(pad(0),A,A).
peep1r(pragma(align(A,1)),B,B).
peep1r(jump(ne,A,A,B),C,C).
peep1r(jump_nt(ne,A,A,B),C,C).
peep1r(move(A,r(void)),B,B).
peep1r(deref(A,r(void)),B,B).

peep1(test(A,B,C,D),E,F) :-
        get_tag(C,G),
        tmatch(A,B,G),
        constC(E,jump(D),F).
peep1(deref(A,A),B,C) :-
        \+write_once,
        constC(B,deref(A),C).
peep1(move(A,B),C,D) :-
        A\==B,
        s_oper(A,E),
        s_oper(B,F),
        constC(C,move(E,F),D).
peep1(equal(A,B,C),D,E) :-
        s_oper(A,F),
        s_oper(B,G),
        constC(D,equal(F,G,C),E).
peep1(unify(A,B,C,D,E),F,G) :-
        s_oper(A,H),
        s_oper(B,I),
        constC(F,unify(H,I,C,D,E),G).
peep1(jump(fail),A,B) :-
        constC(A,fail,B).
peep1(switch(A,B,fail,fail,fail),C,D) :-
        constC(C,fail,D).
peep1(switch(A,B,C,C,C),D,E) :-
        C\==fail,
        constC(D,jump(C),E).
peep1(switch(A,B,fail,C,C),D,E) :-
        constC(D,test(ne,F,B,C),G),
        constC(G,fail,E),
        tag(var,F).
peep1(switch(A,B,C,fail,fail),D,E) :-
        constC(D,test(eq,F,B,C),G),
        constC(G,fail,E),
        tag(var,F).
peep1(switch(A,B,fail,C,fail),D,E) :-
        constC(D,test(eq,A,B,C),F),
        constC(F,fail,E).
peep1(switch(A,B,C,fail,C),D,E) :-
        constC(D,test(ne,A,B,C),F),
        constC(F,fail,E).
peep1(pad(A),B,C) :-
        D=1, % align(D),
        E is A mod D,
        E<A,
        E=\=0,
        constC(B,pad(E),C).
peep1(jump(eq,A,A,fail),B,C) :-
        constC(B,fail,C).
peep1(jump_nt(eq,A,A,fail),B,C) :-
        constC(B,fail,C).
peep1(add(A,B,C),D,E) :-
        integer(A),
        integer(B),
        F is A+B,
        constC(D,move(F,C),E).
peep1(sub(A,B,C),D,E) :-
        integer(A),
        integer(B),
        F is A-B,
        constC(D,move(F,C),E).
peep1(mul(A,B,C),D,E) :-
        integer(A),
        integer(B),
        F is A*B,
        constC(D,move(F,C),E).
peep1(div(A,B,C),D,E) :-
        integer(A),
        integer(B),
        F is A//B,
        constC(D,move(F,C),E).
peep1(and(A,B,C),D,E) :-
        integer(A),
        integer(B),
        F is A/\B,
        constC(D,move(F,C),E).
peep1(or(A,B,C),D,E) :-
        integer(A),
        integer(B),
        F is A\/B,
        constC(D,move(F,C),E).
peep1(xor(A,B,C),D,E) :-
        integer(A),
        integer(B),
        xor(A,B,F),
        constC(D,move(F,C),E).
peep1(sll(A,B,C),D,E) :-
        integer(A),
        integer(B),
        F is A<<B,
        constC(D,move(F,C),E).
peep1(sra(A,B,C),D,E) :-
        integer(A),
        integer(B),
        F is A>>B,
        constC(D,move(F,C),E).

get_tag(A,B) :-
        integer(A),
        A>0,
        tag_always(nonnegative,B).
get_tag(A,B) :-
        integer(A),
        A=<0,
        tag_always(negative,B).
get_tag(A,B) :-
        float(A),
        tag_always(float,B).
get_tag(A^B,A) :-
        tag(atom,A).

eq_ne(eq,ne).
eq_ne(ne,eq).

tmatch(eq,A,A).
tmatch(eq,A,B) :-
        tag(integer,A),
        tag_always(nonnegative,B).
tmatch(eq,A,B) :-
        tag(integer,A),
        tag_always(negative,B).
tmatch(eq,A,B) :-
        tag(integer,B),
        tag_always(nonnegative,A).
tmatch(eq,A,B) :-
        tag(integer,B),
        tag_always(negative,A).
tmatch(ne,A,B) :-
        \+tmatch(eq,A,B).

s_oper(A,B) :-
        (   s_o(A,B) ->
            true
        ;   A=B
        ).

ind([A]).

is_in(A,A).
is_in(A,A+B).
is_in(A,A-B).
is_in(A,[A]).
is_in(A,[A+B]).
is_in(A,[A-B]).
is_in(A,B^A) :-
        pointer_tag(B).
is_in(A,B^ (A+C)) :-
        pointer_tag(B).
is_in(A,B^ (A-C)) :-
        pointer_tag(B).

reg(r(A),A) :-
        integer(A).

reg(r(A)) :-
        atomic(A).

perm(p(A)) :-
        integer(A).

inc_reg(move(A,B),C,D,move(E,F)) :-
        add_ea(A,C,D,E),
        add_ea(B,C,D,F).
inc_reg(pragma(A),B,C,pragma(A)).

cond_ftest(A,f) :-
        cond_to_float(B,A).
cond_ftest(A,i) :-
        cond_to_float(A,B).

tag_ftest(A,f) :-
        tag(float,A).
tag_ftest(A,i) :-
        tag(integer,A).
tag_ftest(A,i) :-
        tag(negative,A).
tag_ftest(A,i) :-
        tag(nonnegative,A).

ftest(eq,f,i,A,nop).
ftest(eq,i,f,A,nop).
ftest(ne,f,f,A,nop).
ftest(ne,i,i,A,nop).
ftest(eq,f,f,A,jump(A)).
ftest(eq,i,i,A,jump(A)).
ftest(ne,i,f,A,jump(A)).
ftest(ne,f,i,A,jump(A)).

arith_ftest(add(A,B,C),i,A,B,C).
arith_ftest(sub(A,B,C),i,A,B,C).
arith_ftest(mul(A,B,C),i,A,B,C).
arith_ftest(div(A,B,C),i,A,B,C).
arith_ftest(mod(A,B,C),i,A,B,C).
arith_ftest(and(A,B,C),i,A,B,C).
arith_ftest(or(A,B,C),i,A,B,C).
arith_ftest(xor(A,B,C),i,A,B,C).
arith_ftest(not(A,B),i,A,B,dummy).
arith_ftest(sll(A,B,C),i,A,B,C).
arith_ftest(sra(A,B,C),i,A,B,C).
arith_ftest(fadd(A,B,C),f,A,B,C).
arith_ftest(fsub(A,B,C),f,A,B,C).
arith_ftest(fdiv(A,B,C),f,A,B,C).
arith_ftest(fmul(A,B,C),f,A,B,C).

no_mod(move(A,B),C) :-
        C\==B.
no_mod(f2i(A,B),C) :-
        C\==B.
no_mod(i2f(A,B),C) :-
        C\==B.
no_mod(add(A,B,C),D) :-
        D\==C.
no_mod(sub(A,B,C),D) :-
        D\==C.
no_mod(mul(A,B,C),D) :-
        D\==C.
no_mod(div(A,B,C),D) :-
        D\==C.
no_mod(mod(A,B,C),D) :-
        D\==C.
no_mod(and(A,B,C),D) :-
        D\==C.
no_mod(or(A,B,C),D) :-
        D\==C.
no_mod(xor(A,B,C),D) :-
        D\==C.
no_mod(sll(A,B,C),D) :-
        D\==C.
no_mod(sra(A,B,C),D) :-
        D\==C.
no_mod(not(A,B),C) :-
        C\==B.
no_mod(fadd(A,B,C),D) :-
        D\==C.
no_mod(fsub(A,B,C),D) :-
        D\==C.
no_mod(fdiv(A,B,C),D) :-
        D\==C.
no_mod(fmul(A,B,C),D) :-
        D\==C.
no_mod(jump(A,B,C,D),E).
no_mod(test(A,B,C,D),E).
no_mod(switch(A,B,C,D,E),F).
no_mod(nop,A).

ind([A],A).

a_var(A) :-
        reg(A).
a_var(A) :-
        perm(A).

tf_ft(true,false).
tf_ft(false,true).

add_ea(A^B,C,D,A^E) :-
        pointer_tag(A),
        add_reg(B,C,D,E).
add_ea([A],B,C,[D]) :-
        add_reg(A,B,C,D).
add_ea(A,B,C,D) :-
        add_reg(A,B,C,D).
add_ea(A,B,C,A).

add_reg(A+B,A,C,D) :-
        reg(A),
        integer(B),
        E is B-C,
        make_reg(A,E,D).
add_reg(A-B,A,C,D) :-
        reg(A),
        integer(B),
        E is-B-C,
        make_reg(A,E,D).
add_reg(A,A,B,C) :-
        reg(A),
        D is-B,
        make_reg(A,D,C).

make_reg(A,B,A+B) :-
        B>0,
        !.
make_reg(A,B,A) :-
        B=:=0,
        !.
make_reg(A,B,A-C) :-
        B<0,
        !,
        C is-B.

peep_flat([],A,A).
peep_flat([A|B],C,D) :-
        peep_flat(A,B,C,D).

peep_flat(switch(unify,A,B,C,D,E),F,G,H) :- !,
        constC(G,switch(A,B,I,J,E),K),
        constC(K,label(I),L),
        peep_flat(C,L,M),
        constC(M,jump(N),O),
        constC(O,label(J),P),
        peep_flat(D,P,Q),
        constC(Q,jump(N),R),
        constC(R,label(N),S),
        peep_flat(F,S,H).
peep_flat(A,B,C,D) :-
        \+A=switch(unify,E,F,G,H,I),
        constC(C,A,J),
        peep_flat(B,J,D).

an_atom(A) :-
        integer(A).
an_atom(A^B) :-
        atom(B),
        tag(atom,A).
an_atom(A^ (B/C)) :-
        atom(B),
        positive(C),
        tag(atom,A).

complex(A^B).
complex([A]).

s_o(A+0,A) :- !.
s_o(A-0,A) :- !.
s_o([A+0],[A]) :- !.
s_o([A-0],[A]) :- !.
s_o(A+B,A-C) :-
        neg_abs(B,C),
        !.
s_o(A-B,A+C) :-
        neg_abs(B,C),
        !.
s_o([A+B],[A-C]) :-
        neg_abs(B,C),
        !.
s_o([A-B],[A+C]) :-
        neg_abs(B,C),
        !.
s_o(A^ (r(h)+0),A^r(h)) :- !.
s_o(A^ (r(h)-0),A^r(h)) :- !.
s_o(A^ (r(h)+B),A^ (r(h)-C)) :-
        neg_abs(B,C),
        !.
s_o(A^ (r(h)-B),A^ (r(h)+C)) :-
        neg_abs(B,C),
        !.

neg_abs(A,B) :-
        integer(A),
        A<0,
        B is-A.

synonym([],[],A).
synonym([A|B],C,D) :-
        synonym(A,B,C,D).

synonym(A,B,C,D) :-
        equal_maker(A,E,F),
        !,
        make_set(E,F,G),
        equal_synonym(A,E,F,G,D,H,C,I),
        synonym(B,I,H).
synonym(label(A),B,[label(A)|C],D) :- !,
        synonym(B,C,[]).
synonym(A,B,C,D) :-
        synonym_step(A,D,E,C,F),
        synonym(B,F,E).

equal_maker(move(A,B),A,B).
equal_maker(equal(A,B,C),A,B).
equal_maker(unify(A,B,C,D,E),A,B).

make_set(A,B,C) :-
        A@<B,
        !,
        C=[A,B].
make_set(A,B,C) :-
        A@>=B,
        !,
        C=[B,A].

equal_synonym(A,B,C,D,E,F,[G|H],H) :-
        not_independent(A),
        !,
        map_equal_maker(A,I,G,J),
        cheapest_list(I,E,J),
        remove_if_move(A,E,C,F).
equal_synonym(A,B,C,D,E,F,[G|H],H) :-
        \+is_syn(D,E),
        !,
        map_equal_maker(A,I,G,J),
        cheapest_list(I,E,J),
        remove_if_move(A,E,C,K),
        add_set(K,D,F).
equal_synonym(A,B,C,D,E,E,F,F).

synonym_step(A,B,C,D,E) :-
        synonym_step1(A,B,D,E),
        synonym_step2(A,B,C).

synonym_step1(A,B,[C|D],D) :-
        map_instruction(A,E,C,F),
        !,
        cheapest_list(E,B,F).
synonym_step1(A,B,[A|C],C).

synonym_step2(A,B,C) :-
        destr_instruction(A,D),
        !,
        remove_all(B,D,C).
synonym_step2(A,B,B).

map_instruction(pragma(tag(A,B)),[A],pragma(tag(C,B)),[C]).
map_instruction(pragma(align(A,B)),[A],pragma(align(C,B)),[C]).
map_instruction(cut(A),[A],cut(B),[B]).
map_instruction(trail(A),[A],trail(B),[B]).
map_instruction(trail_if_var(A),[A],trail_if_var(B),[B]).
map_instruction(unify_atomic(A,B,C),[A],unify_atomic(D,B,C),[D]).
map_instruction(test(A,B,C,D),[C],test(A,B,E,D),[E]).
map_instruction(jump(A,B,C,D),[B,C],jump(A,E,F,D),[E,F]).
map_instruction(hash(A,B,C,D),[B],hash(A,E,C,D),[E]).
map_instruction(switch(A,B,C,D,E),[B],switch(A,F,C,D,E),[F]).
map_instruction(add(A,B,C),[A,B],add(D,E,C),[D,E]).
map_instruction(sub(A,B,C),[A,B],sub(D,E,C),[D,E]).
map_instruction(mul(A,B,C),[A,B],mul(D,E,C),[D,E]).
map_instruction(div(A,B,C),[A,B],div(D,E,C),[D,E]).
map_instruction(and(A,B,C),[A,B],and(D,E,C),[D,E]).
map_instruction(or(A,B,C),[A,B],or(D,E,C),[D,E]).
map_instruction(xor(A,B,C),[A,B],xor(D,E,C),[D,E]).
map_instruction(not(A,B),[A],not(C,B),[C]).
map_instruction(sll(A,B,C),[A,B],sll(D,E,C),[D,E]).
map_instruction(sra(A,B,C),[A,B],sra(D,E,C),[D,E]).
map_instruction(push(A,B,C),[A,B],push(D,E,C),[D,E]).
map_instruction(jump_nt(A,B,C,D),[B,C],jump_nt(A,E,F,D),[E,F]).
map_instruction(add_nt(A,B,C),[A,B],add_nt(D,E,C),[D,E]).
map_instruction(sub_nt(A,B,C),[A,B],sub_nt(D,E,C),[D,E]).
map_instruction(and_nt(A,B,C),[A,B],and_nt(D,E,C),[D,E]).
map_instruction(or_nt(A,B,C),[A,B],or_nt(D,E,C),[D,E]).
map_instruction(xor_nt(A,B,C),[A,B],xor_nt(D,E,C),[D,E]).
map_instruction(not_nt(A,B),[A],not_nt(C,B),[C]).
map_instruction(sll_nt(A,B,C),[A,B],sll_nt(D,E,C),[D,E]).
map_instruction(sra_nt(A,B,C),[A,B],sra_nt(D,E,C),[D,E]).
map_instruction(trail_bda(A,B),[A],trail_bda(C,B),[C]).
map_instruction(queue_sda(A,B,C),[A,B],queue_sda(D,E,C),[D,E]).

cheapest_list([],A,[]).
cheapest_list([A|B],C,[D|E]) :-
        cheapest(A,C,D),
        cheapest_list(B,C,E).

destr_instruction(simple_call(A/B),C) :-
        functor(D,A,B),
        require(D,E),
        uninit_set_type(reg,E,F),
        head_regs(D,F,C),
        cons(C).
destr_instruction(cut(A),[r(b)]) :-
        \+A=r(b).
destr_instruction(deref(A),[A]).
destr_instruction(deref(A,B),[B]).
destr_instruction(add(A,B,C),[C]).
destr_instruction(adda(A,B,C),[C]).
destr_instruction(sub(A,B,C),[C]).
destr_instruction(mul(A,B,C),[C]).
destr_instruction(div(A,B,C),[C]).
destr_instruction(and(A,B,C),[C]).
destr_instruction(or(A,B,C),[C]).
destr_instruction(xor(A,B,C),[C]).
destr_instruction(not(A,B),[B]).
destr_instruction(sll(A,B,C),[C]).
destr_instruction(sra(A,B,C),[C]).
destr_instruction(push(A,B,C),[B]) :-
        C=\=0.
destr_instruction(pad(A),[r(h)]) :-
        A=\=0.
destr_instruction(choice(1/A,B,C),[r(b)]).
destr_instruction(choice(A/B,C,D),E) :-
        1<A,
        A<B,
        regset_to_regs(C,E),
        cons(E).
destr_instruction(choice(A/A,B,C),[r(b)|D]) :-
        regset_to_regs(B,D).
destr_instruction(call(A),[r(b),r(h),r(B)]).
destr_instruction(allocate(A),[r(e),p(B)]).
destr_instruction(deallocate(A),[r(e),p(B)]).
destr_instruction(ord(A,B,C),[C]).
destr_instruction(val(A,B,C),[C]).
destr_instruction(val(A,B,C,D),[D]).
destr_instruction(add_nt(A,B,C),[C]).
destr_instruction(sub_nt(A,B,C),[C]).
destr_instruction(and_nt(A,B,C),[C]).
destr_instruction(or_nt(A,B,C),[C]).
destr_instruction(xor_nt(A,B,C),[C]).
destr_instruction(not_nt(A,B),[B]).
destr_instruction(sll_nt(A,B,C),[C]).
destr_instruction(sra_nt(A,B,C),[C]).

remove_all([],A,[]).
remove_all([A|B],C,D) :-
        remove_one(A,C,E),
        (   E=[F] ->
            D=G
        ;   D=[E|G]
        ),
        remove_all(B,C,G).

not_independent(move(A,B)) :-
        ind(A),
        is_in(B,A).

map_equal_maker(move(A,[B]),[A,B],move(C,[D]),[C,D]) :- !.
map_equal_maker(move(A,B),[A],move(C,B),[C]) :-
        \+B=[D],
        !.
map_equal_maker(equal(A,B,C),[A,B],equal(D,E,C),[D,E]) :- !.
map_equal_maker(unify(A,B,C,D,E),[A,B],unify(F,G,C,D,E),[F,G]) :- !.

remove_if_move(move(A,B),C,D,E) :- !,
        remove_all(C,[D],E).
remove_if_move(A,B,C,B) :-
        \+A=move(D,E).

is_syn(A,[B|C]) :-
        subset(A,B),
        !.
is_syn(A,[B|C]) :-
        is_syn(A,C).

add_set(A,[],A) :- !.
add_set(A,[B],A) :- !.
add_set([A|B],C,[D|B]) :-
        not_disjoint(C,A),
        !,
        union(C,A,D).
add_set([A|B],C,[A|D]) :-
        add_set(B,C,D).
add_set([],A,[A]).

head_regs(A,B,C) :-
        head_regs(A,B,C,[]).

regset_to_regs([],[]).
regset_to_regs([A|B],[r(A)|C]) :-
        integer(A),
        !,
        regset_to_regs(B,C).
regset_to_regs([no|A],B) :-
        regset_to_regs(A,B).

intersect_syn([A|B],C,[D|E]) :-
        member(F,C),
        intersect(A,F,D),
        \+D=[],
        !,
        diff(C,[F],G),
        intersect_syn(B,C,E).
intersect_syn([A|B],C,D) :-
        intersect_syn(B,C,D).
intersect_syn([],A,[]).

remove_one([],A,[]).
remove_one([A|B],C,D) :-
        uses_el(A,C),
        !,
        remove_one(B,C,D).
remove_one([A|B],C,[A|D]) :-
        remove_one(B,C,D).

uses_el([A],B) :-
        uses_el(A,B).
uses_el(A+B,C) :-
        uses_el(A,C).
uses_el(A^B,C) :-
        pointer_tag(A),
        uses_el(B,C).
uses_el(p(A),B) :-
        member(p(C),B),
        var(C),
        !.
uses_el(r(A),B) :-
        member(r(C),B),
        var(C),
        !.
uses_el([A],B) :- !,
        member([C],B).
uses_el(A,B) :-
        memberv(A,B).

less_p(A,B) :-
        cost_syn(A,C),
        cost_syn(B,D),
        C@<D.

cost_syn(r(b),a(0)) :- !.
cost_syn(r(h),b(0)) :- !.
cost_syn(r(A),b(A)) :-
        integer(A),
        !.
cost_syn(A,c(0)) :-
        an_atom(A),
        !.
cost_syn(A^B,c(0)) :-
        pointer_tag(A),
        !.
cost_syn(r(A),d(0)) :-
        atom(A),
        !.
cost_syn(p(A),d(A)) :-
        integer(A),
        !.
cost_syn([r(A)],d(A)) :-
        integer(A),
        !.
cost_syn([r(A)+B],d(A)) :-
        integer(A),
        !.
cost_syn([r(A)],e(0)) :-
        atom(A),
        !.
cost_syn([r(A)+B],e(0)) :-
        atom(A),
        !.
cost_syn([p(A)],e(A)) :- !.
cost_syn([p(A)+B],e(A)) :- !.
cost_syn(r(void),f(0)) :- !.

cheapest(A,B,C) :-
        map_ea(A,D,C,E),
        member(F,B),
        member(D,F),
        !,
        minimum_el(F,E).
cheapest(A,B,A).

map_ea(A,A,B,B).
map_ea([A],A,[B],B).
map_ea([A+B],A,[C+B],C).
map_ea(A^B,B,A^C,C) :-
        pointer_tag(A).
map_ea(A^ (B+C),B,A^ (D+C),D) :-
        pointer_tag(A).

minimum_el([A|B],C) :-
        minimum_el(B,A,C).

minimum_el([],A,A).
minimum_el([A|B],C,D) :-
        less_p(A,C),
        !,
        minimum_el(B,A,D).
minimum_el([A|B],C,D) :-
        minimum_el(B,C,D).

code(A,B,C,D,'$case'(E,F,G),H,I) :- !,
        % stats(d,det_code),
        det_code(A,B,C,D,E,F,G,H,I).
code(A,B,C,D,E,F,G) :-
        disj_p(E),
        !,
        % stats(d,choice_block),
        choice_block(A,B,C,D,E,F,G).
code(A,B,C,D,E,F,G) :-
        \+disj_p(E),
        \+E='$case'(H,I,J),
        !,
        flat_conj(E,K),
        % stats(d,clause_code),
        clause_code((A:-K),C,D,B,F,G).

det_code(A,B,C,D,E,v(F),G,H,I) :-
        type(E),
        !,
        % stats(d,E),
        head_reg(A,F,J),
        rtest(deref(F),J,B,K,fail,H,L),
        type_test(E,F,M),
        rtest_in(M,J,K,N,O,L,P),
        logical_simplify(not(M),Q),
        choice(true,A,M,K,C,D,G,R,P,S),
        choice(false,A,Q,K,C,D,G,O,S,I).
det_code(A,B,C,D,comparison(E,arith),v(F,G),H,I,J) :- !,
        % stats(d,comparison(E,arith)),
        head_reg(A,F,K),
        head_reg(A,G,L),
        rtest(deref(F),K,B,M,fail,I,N),
        rtest(deref(G),L,M,O,fail,N,P),
        expand_cmp_det(E,F,G,K,L,true,Q,O,R,P,S),
        arith_test(T,F,G,E),
        logical_simplify(not(T),U),
        choice(false,A,U,R,C,D,H,V,S,W),
        choice(true,A,T,R,C,D,H,Q,W,J).
det_code(A,B,C,D,hash(E),v(F),G,H,I) :- !,
        % stats(d,hash(E)),
        head_reg(A,F,J),
        rtest(deref(F),J,B,K,fail,H,L),
        type_test(E,F,M),
        rtest_if_structure(E,M,J,K,N,O,L,P),
        constC(P,hash(E,J,Q,R),S),
        constC(S,label(O),T),
        insert(U,V,T,W),
        constC(W,label(R),X),
        constC(X,pragma(hash_length(Q)),Y),
        insert(Z,A1,Y,B1),
        hash_table(A,N,C,D,G,C1,0,Q,Z,A1,B1,I),
        code(A,K,C,D,C1,U,V).
det_code(A,B,C,D,switch(E),v(F),G,H,I) :-
        tag(E,J),
        !,
        % stats(d,switch(E)),
        head_reg(A,F,K),
        rtest(deref(F),K,B,L,fail,H,M),
        constC(M,switch(J,K,N,O,P),Q),
        type_test(E,F,R),
        disj_test(var(F),R,S),
        not_test(S,T),
        choice(other,A,T,L,C,D,G,P,Q,U),
        choice(var,A,var(F),L,C,D,G,N,U,V),
        choice(E,A,R,L,C,D,G,O,V,I).
det_code(A,B,C,D,equal,v(E,F),G,H,I) :- !,
        % stats(d,equal),
        head_reg(A,E,J),
        head_reg(A,F,K),
        rtest(deref(E),J,B,L,fail,H,M),
        rtest(deref(F),K,L,N,fail,M,O),
        constC(O,equal(J,K,P),Q),
        choice(true,A,E==F,N,C,D,G,R,Q,S),
        choice(false,A,E\==F,N,C,D,G,P,S,I).
det_code(A,B,C,D,equal(atomic,E),v(F),G,H,I) :- !,
        % stats(d,equal(atomic,E)),
        head_reg(A,F,J),
        rtest(deref(F),J,B,K,fail,H,L),
        atomic_word(E,M),
        constC(L,equal(J,M,N),O),
        choice(true,A,F=E,K,C,D,G,P,O,Q),
        choice(false,A,F\=E,K,C,D,G,N,Q,I).
det_code(A,B,C,D,equal(structure,E/F),v(G),H,I,J) :- !,
        % stats(d,equal(structure,E/F)),
        head_reg(A,G,K),
        rtests((deref(G)',' structure(G)),K,B,L,fail,I,M),
        tag(atom,N),
        pragma_tag(K,structure,M,O),
        P=1, % align(P),
        constC(O,pragma(align(K,P)),Q),
        constC(Q,equal([K],N^ (E/F),R),S),
        choice(true,A,'$name_arity'(G,E,F),L,C,D,H,T,S,U),
        choice(false,A,not('$name_arity'(G,E,F)),L,C,D,H,R,U,J).

choice_block(A,B,C,D,E,F,G) :-
        require_derefs(E,B,H),
        insert_derefs(A,H,B,I,F,J),
        split_disj(E,K,L),
        varset_disj(L,M),
        uninit_set_type(mem,B,N),
        uninit_set_type(reg,B,O),
        unionv(N,M,P),
        diffv(P,O,Q),
        varset_numset(A,Q,R),
        choice_block(A,I,R,N,O,C,D,E,S,0,T,J,G).

clause_code(A,B,C,D,E,F) :-
        clause_code(A,B,C,D,G,E,F).

require_derefs(A,B,C) :-
        get_kind_d(A,B,deref,D,[]),
        sort(D,C).

insert_derefs(A,B,C,D,E,F) :-
        varset(A,G),
        intersectv(B,G,H),
        insert_derefs_loop(A,H,C,D,E,F).

varset_disj(A,B) :-
        varbag_disj(A,C,[]),
        sort(C,B).

varset_numset(A,B,C) :-
        functor(A,D,E),
        low_reg(F),
        varset_numset(1,E,A,B,F,C).

choice_block(A,B,C,D,E,F,G,fail,H,I,I,J,K) :-
        I=0,
        !,
        constC(J,fail,K).
choice_block(A,B,C,D,E,F,G,fail,fail,H,H,I,I) :-
        H>0,
        !.
choice_block(A,B,C,D,E,F,G,(H;I),J,K,L,M,N) :- !,
        O is K+1,
        constC(M,label(J),P),
        functor(A,Q,R),
        cp_instr(C,D,E,H,A,O,L,I,S,P,T),
        % stats(d,O),
        code(A,B,F,G,H,T,U),
        choice_block(A,B,C,D,E,F,G,I,S,O,L,U,N).
choice_block(A,B,C,D,E,F,G,H,I,J,K,L,M) :-
        \+disj_p(H),
        !,
        choice_block(A,B,C,D,E,F,G,(H;fail),I,J,K,L,M).

cp_instr(A,B,C,D,E,1,F,fail,G,H,H) :- !.
cp_instr(A,B,C,D,E,F,G,H,I,J,K) :-
        F=:=1,
        !,
        constC(J,choice(F/G,A,I),K).
cp_instr(A,B,C,D,E,F,G,H,I,J,K) :-
        F>1,
        !,
        varset_conj(D,L),
        unionv(B,L,M),
        diffv(M,C,N),
        varset_numset(E,N,O),
        format_numset(A,O,P),
        constC(J,choice(F/G,P,I),K).

varset_conj(A,B) :-
        varbag_conj(A,C,[]),
        sort(C,B).

format_numset([A|B],[A|C],[A|D]) :- !,
        format_numset(B,C,D).
format_numset([A|B],[C|D],[no|E]) :-
        A<C,
        !,
        format_numset(B,[C|D],E).
format_numset([A|B],[],[no|C]) :- !,
        format_numset(B,[],C).
format_numset([],[],[]).

varset_numset(A,B,C,D,E,[]) :-
        A>B,
        !.
varset_numset(A,B,C,D,E,F) :-
        A=<B,
        !,
        arg(A,C,G),
        H is E+ (A-1),
        incl_if_inv(G,D,H,F,I),
        J is A+1,
        varset_numset(J,B,C,D,E,I).

incl_if_inv(A,B,C,D,E) :-
        inv(A,B),
        !,
        constC(D,C,E).
incl_if_inv(A,B,C,D,D) :-
        \+inv(A,B),
        !.

varbag_disj((A;B),C,D) :- !,
        varbag_disj(A,C,E),
        varbag_disj(B,E,D).
varbag_disj(A,B,C) :-
        varbag_conj(A,B,C).

varbag_conj((A ',' B),C,D) :- !,
        varbag_conj(A,C,E),
        varbag_conj(B,E,D).
varbag_conj('$body'(A,B,C),D,E) :- !,
        difflist(B,D,E).
varbag_conj(A,B,C) :-
        varbag(A,B,C).

get_kind_d((A;B),C,D,E,F) :- !,
        get_kind_c(A,C,D,E,G),
        get_kind_d_stop(A,B,C,D,G,F).
get_kind_d(A,B,C,D,E) :-
        get_kind_c(A,B,C,D,E).

get_kind_c((A ',' B),C,D,E,F) :- !,
        get_kind_g(A,D,E,G),
        get_kind_c_stop(A,B,C,D,G,F).
get_kind_c(A,B,C,D,E) :-
        get_kind_g(A,C,D,E).

get_kind_d_stop(A,B,C,D,E,E) :-
        success_to_cut(A),
        !.
get_kind_d_stop(A,B,C,D,E,F) :-
        get_kind_d(B,C,D,E,F).

success_to_cut('$cut_shallow'(A)).
success_to_cut('$cut_deep'(A)).
success_to_cut(('$cut_shallow'(A)',' B)).
success_to_cut(('$cut_deep'(A)',' B)).
success_to_cut((A ',' B)) :-
        succeeds(A),
        success_to_cut(B).

get_kind_g(A,B,C,D) :-
        require(A,E),
        filter_kind(E,B,C,D).

get_kind_c_stop(A=B,C,D,E,F,F) :-
        \+implies(D,(uninit(any,A);uninit(any,B))),
        !.
get_kind_c_stop(A,B,C,D,E,F) :-
        get_kind_c(B,C,D,E,F).

filter_kind((A ',' B),C,D,E) :- !,
        filter_kind(A,C,D,F),
        filter_kind(B,C,F,E).
filter_kind((A;B),C,D,E) :- !,
        filter_kind(A,C,D,F),
        filter_kind(B,C,F,E).
filter_kind(deref(A),deref,B,C) :-
        var(A),
        !,
        constC(B,A,C).
filter_kind(rderef(A),deref,B,C) :-
        var(A),
        !,
        constC(B,A,C).
filter_kind(A,B,C,C).

insert_derefs_loop(A,[],B,B,C,C) :- !.
insert_derefs_loop(A,[B|C],D,E,F,G) :-
        head_reg(A,B,H),
        rtest(deref(B),H,D,I,fail,F,J),
        insert_derefs_loop(A,C,I,E,J,G).

head_reg(A,B,C) :-
        var(B),
        !,
        functor(A,D,E),
        head_reg(1,E,A,B,F),
        low_reg(G),
        H is F+G-1,
        C=r(H).
head_reg(A,B,C^B) :-
        atom(B),
        !,
        term_tag(B,C).
head_reg(A,B,B) :-
        number(B),
        !.
head_reg(A,B,r(complex_type_error)) :-
        compound(B),
        !.

choice(A,B,C,D,E,F,G,fail,H,I) :-
        mutex(C,D,left),
        !,
        comment(B,['Mutex of ',C,' and ',D,' in choice']),
        % stats(d,A/C-mutex_fail),
        constC(H,fail,I).
choice(A,B,C,D,E,F,G,H,I,J) :-
        choice('$test'(A,K,L,M,N),G),
        !,
        constC(I,label(H),O),
        update_head(M,L,D,P,E,F,B,Q,O,R),
        subsume(L,N,S),
        % stats(d,A/C),
        code(Q,P,E,F,S,R,J).
choice(A,B,C,D,E,F,G,fail,H,I) :-
        \+choice('$test'(A,J,K,L,M),G),
        choice('$else'(N,O,fail),G),
        !,
        % stats(d,A/C-else_fail),
        constC(H,fail,I).
choice(A,B,C,D,E,F,G,H,I,J) :-
        \+choice('$test'(A,K,L,M,N),G),
        choice('$else'(O,P,Q),G),
        \+Q=fail,
        !,
        constC(I,label(H),R),
        update_head(P,C,D,S,E,F,B,T,R,U),
        subsume(C,Q,V),
        % stats(d,A/C),
        code(T,S,E,F,V,U,J).

rtest_if_structure(structure,A,B,C,D,E,F,G) :- !,
        rtest(A,B,C,D,E,F,G).
rtest_if_structure(A,B,C,D,D,E,F,F) :- !.

hash_table(A,B,C,D,'$else'(E,F,G),G,H,H,I,I,J,J) :- !.
hash_table(A,B,C,D,('$test'(E,F,G,H,I);J),K,L,M,[pair(N,O)|P],Q,R,S) :- !,
        make_hash_entry(E,N),
        T is L+1,
        constC(R,label(O),U),
        update_head(H,G,B,V,C,D,A,W,U,X),
        code(W,V,C,D,I,X,Y),
        hash_table(A,B,C,D,J,K,T,M,P,Q,Y,S).

choice(A,A) :-
        \+A= (B;C).
choice(A,A) :-
        \+A= (B;C).
choice(A,(B;C)) :-
        choice(A,C).
choice(A,(B;C)) :-
        choice(A,B).

update_head([],A,B,C,D,E,F,F,G,G) :- !,
        update_formula(A,B,C).
update_head(A,B,C,D,E,F,G,G,H,H) :-
        \+A=[],
        !,
        update_formula(B,C,D).

make_hash_entry(A,A) :-
        integer(A),
        !.
make_hash_entry(A,A) :-
        float(A),
        !.
make_hash_entry(A,B^A) :-
        atom(A),
        !,
        tag(atom,B).
make_hash_entry(A/B,C^ (A/B)) :-
        tag(atom,C).

head_reg(A,B,C,D,E) :-
        A>B,
        !,
        E=r(arity_error).
head_reg(A,B,C,D,E) :-
        A=<B,
        arg(A,C,F),
        F==D,
        !,
        E=A.
head_reg(A,B,C,D,E) :-
        A=<B,
        arg(A,C,F),
        F\==D,
        !,
        G is A+1,
        head_reg(G,B,C,D,E).

head_regs(A,B,C,D) :-
        functor(A,E,F),
        low_reg(G),
        head_regs(F,A,G,B,C,D).

head_regs(A,B,C,D,E,E) :-
        A=<0,
        !.
head_regs(A,B,C,D,E,F) :-
        A>0,
        arg(A,B,G),
        inv(G,D),
        !,
        constC(E,r(H),I),
        J is A-1,
        H is A+C-1,
        head_regs(J,B,C,D,I,F).
head_regs(A,B,C,D,E,F) :-
        A>0,
        !,
        G is A-1,
        head_regs(G,B,C,D,E,F).

testset(A) :-
        testset(A,B,C,D,E).

testset(A,B,C,D,E) :-
        testset(A,B,true,C,D,E).

testset(A,'$name_arity'(B,C,0),D,equal(atomic,C),v(B),true,false) :-
        test_type_name_arity(A,B,E,C,0),
        atomic_type(E).
testset(A,not('$name_arity'(B,C,0)),D,equal(atomic,C),v(B),false,true) :-
        logical_simplify(not(A),E),
        test_type_name_arity(E,B,F,C,0),
        atomic_type(F).
testset(A,'$name_arity'(B,C,D),E,equal(structure,C/D),v(B),true,false) :-
        test_type_name_arity(A,B,structure,C,D).
testset(A,not('$name_arity'(B,C,D)),E,equal(structure,C/D),v(B),false,true) :-
        logical_simplify(not(A),F),
        test_type_name_arity(F,B,structure,C,D).
testset(A,'$name_arity'(B,C,D),E,hash(F),v(B),G,not(G)) :-
        test_type_name_arity(A,B,H,C,D),
        H\==cons,
        (   H==float,
            true -> % float ->
            F=float
        ;   H==float,
            fail -> % \+float ->
            F=denumerable
        ;   denumerable_type(H) ->
            F=denumerable
        ;   F=structure
        ),
        (   D>0 ->
            G=C/D
        ;   G=C
        ).
testset(A,A,B,comparison(C,D),v(E,F),G,H) :-
        D=arith,
        encode_relop(A,E,I,F,D),
        E@=<F,
        map_relop(A,C,G,H).
testset(A,A,B,comparison(C,D),v(E,F),G,H) :-
        D=arith,
        encode_relop(A,F,I,E,D),
        E@=<F,
        flip(I,J),
        encode_relop(K,E,J,F,D),
        map_relop(K,C,G,H).
testset(A,B,C,D,v(E),F,G) :-
        many_way(A,B,E,D,F,G).
testset(A,B,C,switch(D),v(E),F,not(F)) :-
        many_way(A,B,E,G,true,false),
        tag_type(D),
        D\==var,
        \+implies(C,nonvar(E)),
        map_three(D,G,F).

number_of_direcs(A,B) :-
        get_direcs(A,C),
        sort(C,D),
        length(D,B).

goodness_key(equal(atomic,[]),132) :-
        true. % mips.
goodness_key(switch(negative),130).
goodness_key(switch(nonnegative),130).
goodness_key(switch(atom),130).
goodness_key(switch(float),130) :-
        true. % float.
goodness_key(switch(integer),129).
goodness_key(switch(A),131).
goodness_key(cons,122).
goodness_key(structure,121).
goodness_key(var,120).
goodness_key(atom,120).
goodness_key(float,120) :-
        true. % float.
goodness_key(integer,120) :-
        true. % \+split_integer.
goodness_key(negative,120) :-
        fail. % split_integer.
goodness_key(nonnegative,120) :-
        fail. % split_integer.
goodness_key(equal,85).
goodness_key(equal(atomic,A),80).
goodness_key(comparison(A,B),80).
goodness_key(integer,79) :-
        fail. % split_integer.
goodness_key(atomic,79).
goodness_key(compound,79).
goodness_key(negative,78) :-
        true. % \+split_integer.
goodness_key(nonnegative,78) :-
        true. % \+split_integer.
goodness_key(equal(structure,A),60).
goodness_key(simple,50).
goodness_key(hash(denumerable),41).
goodness_key(hash(float),41) :-
        true. % float.
goodness_key(hash(structure),40).

get_direcs([],[]).
get_direcs([val(A,B,C,D,E,F)|G],[E|H]) :-
        get_direcs(G,H).

switch_testset_list(A,B,[val(C,var(B),none,D,var,E),val(F,G,none,H,A,E),val(I,J,none,K,other,E)]) :-
        E= ('$sel_error' ',' true),
        type_test(A,B,G),
        disj_test(var(B),G,L),
        not_test(L,J).

update_testset([val(A,B,C,D,E,F)|G],H,I,J) :-
        delete(H,val(K,L,M,N,E,O),P),
        update_testset(G,P,I,J).
update_testset([],A,B,C) :-
        append(B,A,C).

test_type_name_arity(A,B,C,D,E) :-
        encode_name_arity(A,B,D,E,true),
        name_arity_type(D,E,C).

map_relop(A,B,true,false) :-
        arith_test(A,B),
        special_cond(B).
map_relop(A,B,false,true) :-
        opposite(A,C),
        arith_test(C,B),
        special_cond(B).

many_way(A,A,B,C,true,false) :-
        type_test(C,B,A).
many_way(A,B,C,D,true,false) :-
        test_type_name_arity(A,C,D,E,F),
        type_test(D,C,B).
many_way(A,B,C,D,E,F) :-
        negation(A,G),
        many_way(G,H,C,D,F,E),
        out_negation(H,B).
many_way(A,nonvar(B),B,var,false,true) :-
        test_varbag(A,C),
        member(B,C),
        implies(A,nonvar(B)).

map_three(A,var,var).
map_three(A,A,A).

name_arity_type(A,B,atom) :-
        atom(A),
        integer(B),
        B=:=0.
name_arity_type(A,B,float) :-
        float(A),
        integer(B),
        B=:=0.
name_arity_type(A,B,integer) :-
        integer(A),
        integer(B),
        B=:=0.
name_arity_type(A,B,negative) :-
        fail, % split_integer,
        negative(A),
        integer(B),
        B=:=0.
name_arity_type(A,B,nonnegative) :-
        fail, % split_integer,
        negative(A),
        integer(B),
        B=:=0.
name_arity_type(A,B,structure) :-
        nonvar(A),
        integer(B),
        B>0,
        \+ (A='.' ',' B=:=2).
name_arity_type(A,B,cons) :-
        A=='.',
        integer(B),
        B=:=2.

negation(\+A,A) :- !.
negation(not(A),A) :- !.

out_negation(not(A),A) :- !.
out_negation(A,not(A)) :- !.

entry_comment(A,B,C) :-
        fail, % compile_option(entry_comment),
        !,
        length(C,D),
        comment(A,['(',B,') is called with ',C]).
entry_comment(A,B,C) :-
        true, % \+compile_option(entry_comment),
        !.

split_uninit_list([],[],[],[],[]).
split_uninit_list([entry(A,B)|C],[D|E],[F|G],[H|I],[J|K]) :-
        split_uninit(B,H,J),
        uninit_set_type(mem,H,D),
        uninit_set_type(reg,H,F),
        split_uninit_list(C,E,G,I,K).

insert_uninit([],A,B,[],C,D,D).
insert_uninit([A|B],C,D,[entry(E,F)|G],H,I,J) :-
        diffv(A,D,K),
        constC(I,label(E),L),
        init_code_block(K,C,L,M),
        constC(M,jump(H),N),
        insert_uninit(B,C,D,G,H,N,J).

uninit_origins(A,B) :-
        origin_fragments(A,C,[]),
        keysort(C,D),
        origin_merge(D,B).

make_uninit_mem_formula([],A,B,B).
make_uninit_mem_formula([A|B],C,D,E) :-
        member_origin(A-F,C),
        !,
        co(uninit(mem,A,F),D,G),
        make_uninit_mem_formula(B,C,G,E).
make_uninit_mem_formula([A|B],C,D,E) :-
        co(uninit(A),D,F),
        make_uninit_mem_formula(B,C,F,E).

make_uninit_reg_formula([],A,A).
make_uninit_reg_formula([A|B],C,D) :-
        co(uninit(reg,A),C,E),
        make_uninit_reg_formula(B,E,D).

init_code_block([],A,B,B).
init_code_block([A|B],C,D,E) :-
        find_arg(F,C,A),
        !,
        low_reg(G),
        H is G+F-1,
        pragma_tag(H,var,D,I),
        constC(I,move(H,[H]),J),
        init_code_block(B,C,J,E).
init_code_block([A|B],C,D,E) :-
        error(C,['Variable ',A,' is not initialized.']),
        init_code_block(B,C,D,E).

member_origin(A-B,[C-D|E]) :-
        A==C,
        !,
        B=D.
member_origin(A-B,[C|D]) :-
        member_origin(A-B,D).

origin_fragments([],A,A).
origin_fragments([A|B],C,D) :-
        origin_formula(A,C,E),
        origin_fragments(B,E,D).

origin_merge([],[]).
origin_merge([A-B|C],D) :-
        origin_merge(C,A,E,D,[]).

origin_formula((A ',' B),C,D) :- !,
        origin_formula(A,C,E),
        origin_formula(B,E,D).
origin_formula(uninit(mem,A,B),C,D) :- !,
        constC(C,A-B,D).
origin_formula(A,B,B).

origin_merge([],A,B,C,D) :-
        constC(C,A-B,D).
origin_merge([A-B|C],D,E,F,G) :-
        D==A,
        !,
        append(B,E,H),
        origin_merge(C,D,H,F,G).
origin_merge([A-B|C],D,E,F,G) :-
        D\==A,
        !,
        constC(F,D-E,H),
        origin_merge(C,A,B,H,G).

clause_code(A,B,C,D,E,F,G) :-
        standard_form(A,H,I),
        logical_simplify(D,J),
        clause_code_3((H:-I),B,C,J,E,F,G).

clause_code_3(A,B,C,fail,fail,D,E) :- !,
        constC(D,fail,E).
clause_code_3(A,B,C,D,E,F,G) :-
        \+D=fail,
        constC(F,H,I),
        copy([A|D],[J|K]),
        clause(J,B,L,K,M,I,N),
        copy([J|M],[A|E]),
        constC(N,O,P),
        return_jump(C,P,G),
        clause_allocate(L,Q,R),
        environment(Q,R,H,O),
        !.

clause((A:-B),C,(A:-D),E,F,G,H) :-
        uninit_set_type(reg,E,I),
        head_moves_pre(A,I,J,K,G,L),
        find_gperms((A:-B),M),
        uninit_allowset(B,C,N),
        varbag((A:-B),O),
        filter_uniq(O,P),
        body(B,no,Q,M,N,C,P,[],R,[],S,[],T,[],U,J,V,E,F,L,W),
        head_moves_post_c(A,I,X,T,U,W,H),
        flat_conj(('$varlist'(K)',' Q ',' '$varlist'(X)),D).

return_jump(return,A,B) :-
        constC(A,return,B).
return_jump(jump(A),B,C) :-
        constC(B,jump(A),C).

clause_allocate(A,B,C) :-
        % stats(reg,1),
        varlist(A,D),
        local_allocate(D,B,C).
clause_allocate(A,B,C) :-
        error(['Register allocation could not be done for the clause:',nl,A,nl,'because of a bug in the register allocation routine.']).

environment(no,A,nop,nop).
environment(yes,A,allocate(A),deallocate(A)).

head_moves_pre(A,B,C,D,E,F) :-
        A=..[G|H],
        low_reg(I),
        head_moves_pre_2(H,I,B,D,E,F),
        sort(H,J),
        diffv(J,B,C).

find_gperms(A,B) :-
        split(A,C,D),
        varset(C,E),
        gpermvars(D,E,F,[],G,[],B).

uninit_allowset(A,B,C) :-
        uninit_allowbag(A,B,D,E,[],[],F),
        sort(E,C).

body(A,B,fail,C,D,E,F,G,G,H,H,I,I,J,J,K,K,L,L,M,N) :-
        L=fail,
        !,
        M=[fail|N],
        true.
body(true,no,'$varlist'(A),B,C,D,E,F,F,G,G,H,H,I,I,J,K,L,M,N,O) :- !,
        init_uninit([],A,[],J,K,L,M,N,O),
        true.
body((A ',' B),no,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- !,
        efficient_entry(A,V,G,R,W,P,X),
        flat_conj(V,Y),
        union_conj_eff(Y,B,Z,A1),
        body(Z,A1,C,D,E,F,G,H,I,J,K,L,M,N,O,X,Q,W,S,T,U),
        true.
body((A ',' B),(yes ',' C),(D ',' E ',' F ',' G ',' H),I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z) :-
        % stats(goal,A),
        W=A1,
        split_formula(B1,U,A,A1,C1,D1),
        % stats(x,8),
        last_goal_adjust(B,C1,D1,E1,F1),
        % stats(x,9),
        perm_c(A,D,I,M,G1,O,H1,Q,I1,S,J1,U,K1,E1,L1,Y,M1),
        % stats(x,10),
        map_terms(G1,H1,A,N1),
        % stats(x,11),
        map_terms(G1,H1,L1,O1),
        % stats(x,12),
        uninit_bag_type(mem,O1,P1),
        % stats(x,13),
        goal_c(N1,E,Q1,R1,S1,J,K,T1,P1,I1,U1,J1,V1,K1,W1,O1,X1,M1,Y1),
        sort(T1,Z1),
        make_indirect(Z1,A2),
        append(Z1,U1,B2),
        append(A2,V1,C2),
        x_call_moves_post_c2(Q1,F,B2,D2,C2,E2,Y1,F2),
        args_post_c2(R1,G,D2,G2,E2,H2,W1,I2,X1,J2,F2,K2),
        update_formula(S1,J2,L2),
        map_terms(H1,G1,L2,M2),
        M2=N2,
        combine_formula(N2,F1,O2),
        body(B,C,H,I,J,K,L,G1,N,H1,P,G2,R,H2,T,I2,V,O2,X,K2,Z),
        true.

head_moves_post_c(A,B,C,D,E,F,G) :-
        cons(D),
        !,
        head_moves_post(A,B,H,I,[]),
        map_instr_list(I,D,E,F,G),
        map_varlist_list(H,C,D).
head_moves_post_c(A,B,C,D,E,F,G) :-
        head_moves_post(A,B,C,F,G).

uninit_allowbag((A ',' B),C,D,E,F,G,H) :- !,
        uninit_allowbag(A,C,I,E,J,G,K),
        uninit_allowbag(B,I,D,J,F,K,H),
        true.
uninit_allowbag((A;B),C,D,E,F,G,H) :- !,
        uninit_allowbag(A,C,I,E,J,G,K),
        uninit_allowbag(B,I,D,J,F,K,H),
        true.
uninit_allowbag('$body'((A:-B),C,D),E,F,G,H,I,J) :- !,
        uninit_allowbag(B,E,F,G,H,I,J),
        true.
uninit_allowbag(A,B,B,C,D,E,F) :-
        functor(A,G,H),
        member(stree(G/H,(I:-J),(K:-L),M,N,O),B),
        !,
        copy((K:-L),(A:-P)),
        uninit_bag(P,C,Q),
        copy((I:-J),(A:-R)),
        B=S,
        uninit_allowbag(R,N,T,Q,D,E,F),
        T=U,
        true.
uninit_allowbag(A,B,B,C,D,E,E) :-
        require(A,F),
        uninit_bag(F,C,D),
        true.

init_uninit(A,B,C,D,E,F,G,H,I) :-
        varbag(A,J),
        init_uninit(J,F,B,C,D,E,H,I,K,[]),
        remove_uninit(K,F,G),
        true.

efficient_entry(A,B,C,D,D,E,E) :-
        modal_entry(A,F),
        add_uninit_void(A,E,C,D,G),
        tree_trav_entry(F,G,B),
        !,
        comment(['Replaced ',A,' by ',B]),
        comment(A,['The mode formula at this point is ',G]).
efficient_entry(A,A,B,C,C,D,D).

union_conj_eff((A ',' B),C,(A ',' D),(yes ',' E)) :-
        union_conj_eff(B,C,D,E).
union_conj_eff(true,A,A,no).

last_goal_adjust(true,A,B,C,D) :- !,
        split_uninit(B,E,D),
        combine_formula(E,A,C).
last_goal_adjust(A,B,C,B,C) :-
        \+A=true,
        !.

perm_c(A,B,C,D,E,F,G,H,H,I,I,J,K,L,M,N,O) :-
        cons(H),
        !,
        perm(A,P,C,D,E,F,G,J,K,L,M,Q,[]),
        map_instr_list(Q,H,I,N,O),
        map_varlist(P,B,H),
        true.
perm_c(A,B,C,D,E,F,G,H,H,I,I,J,K,L,M,N,O) :-
        perm(A,B,C,D,E,F,G,J,K,L,M,N,O),
        true.

goal_c(A,B,C,D,E,F,G,H,I,J,K,L,L,M,N,O,P,Q,R) :-
        cons(J),
        !,
        goal(A,S,C,D,E,F,G,H,I,J,K,M,N,O,P,T,[]),
        map_instr_list(T,J,L,Q,R),
        map_varlist(S,B,J),
        true.
goal_c(A,B,C,D,E,F,G,H,I,J,K,L,L,M,N,O,P,Q,R) :-
        goal(A,B,C,D,E,F,G,H,I,J,K,M,N,O,P,Q,R),
        true.

make_indirect([],[]).
make_indirect([A|B],[[A]|C]) :-
        make_indirect(B,C).

x_call_moves_post_c2(data(A,B),C,D,D,E,E,F,G) :-
        cons(D),
        !,
        x_call_moves_post(A,B,H,[],I,[]),
        map_instr_list(I,D,E,F,G),
        map_varlist_goal(H,C,D),
        true.
x_call_moves_post_c2(data(A,B),'$varlist'(C),D,D,E,E,F,G) :- !,
        x_call_moves_post(A,B,C,[],F,G),
        true.
x_call_moves_post_c2(none,'$varlist'([]),A,A,B,B,C,C).

args_post_c2(data(A,B,C),D,E,E,F,F,G,H,I,J,K,L) :-
        cons(E),
        !,
        args_post(A,B,C,M,[],G,H,I,J,N,[]),
        map_instr_list(N,E,F,K,L),
        map_varlist_goal(M,D,E),
        true.
args_post_c2(data(A,B,C),'$varlist'(D),E,E,F,F,G,H,I,J,K,L) :- !,
        args_post(A,B,C,D,[],G,H,I,J,K,L),
        true.
args_post_c2(none,'$varlist'([]),A,A,B,B,C,C,D,D,E,E).

goal(A,'$varlist'(B),C,none,true,D,E,F,F,G,G,H,I,J,K,L,M) :-
        call_p(A),
        unwrap_goal(A,N,yes),
        !,
        init_uninit(N,B,O,H,P,J,Q,L,R),
        functor(N,S,T),
        gensym([36,101,95],S/T,U),
        unwrap_form(A,U/T,Q,V),
        uninit_set_type(reg,V,W),
        x_call_moves_pre(N,W,O,X,R,Y),
        fence_if_die(N,X,[]),
        Y=[call(U/T)|M],
        x_call_moves_post_c1(N,W,C),
        unionv(W,P,I),
        remove_vars(V,Z),
        remove_all_uninit(Z,K),
        true.
goal(A,B,none,C,D,E,F,G,H,I,I,J,K,L,M,N,O) :-
        call_p(A),
        unwrap_goal(A,P,no),
        !,
        functor(P,Q,R),
        P=..[Q|S],
        B= ('$varlist'(T)',' U ',' '$varlist'(V)',' W),
        parameter_setup(P,X,Y,T,Z,J,A1,L,B1,N,C1),
        % stats(x,15),
        args_pre(X,S,D1,G,H,Z,[],A1,E1,B1,F1,C1,G1),
        % stats(x,data16(ireq(Y),form(F1))),
        H1=..[Q|D1],
        rtests_m(Y,U,F1,I1,G1,J1),
        % stats(x,17),
        warn_fail(A,I1,K1),
        init_uninit(pair(S,D1),V,[],E1,K,K1,L1,J1,M1),
        % stats(x,18),
        the_call(P,H1,W,X,D1,F,L1,N1,M1,O),
        remove_all_uninit(N1,O1),
        after(P,D),
        map_vars(S,D1,D,P1),
        update_formula(P1,O1,M),
        args_post_c1(X,S,D1,C),
        true.
goal(A=B,'$varlist'(C),none,none,true,D,E,F,F,G,H,I,J,K,L,M,N) :-
        init_uninit(A=B,C,O,I,P,K,Q,M,R),
        unify_g(A=B,O,D,P,J,Q,L,R,N,G,H),
        true.

map_varlist(A,B,C) :-
        bodylist(A,D,[]),
        map_varlist_goal(D,B,C).

head_moves_post(A,B,C,D,E) :-
        head_moves_post_2(B,A,C,D,E).

map_varlist_list(A,B,C) :-
        map_varlist_list(A,C,B,[]).

perm(A,'$varlist'(B),C,D,E,F,G,H,I,J,K,L,M) :-
        write_once,
        !,
        vars_to_rename(A,J,N),
        intersectv(N,C,O),
        intersectv(O,H,P),
        copy(P,Q),
        sort(Q,R),
        update_mapping(P,R,S,D,E,F,G),
        init_extra_vars(P,R,S,B,[],H,I,J,K,L,M),
        true.
perm(A,'$varlist'([]),B,C,C,D,D,E,E,F,F,G,G) :-
        \+write_once,
        !,
        true.

x_call_moves_post_c1(A,B,data(A,B)).

x_call_moves_post(A,B,C,D,E,F) :-
        x_call_moves_post_2(B,A,C,D,E,F).

map_varlist_goal(A,'$varlist'(B,C),D) :-
        map_varlist_list(A,D,B,C).

args_post_c1(A,B,C,data(A,B,C)).

args_post([],[],[],A,A,B,B,C,C,D,D) :- !,
        true.
args_post([flag(A,B)|C],[D|E],[F|G],H,I,J,K,L,M,N,O) :-
        one_arg_post(A,B,D,F,H,P,J,Q,L,R,N,S),
        args_post(C,E,G,P,I,Q,K,R,M,S,O),
        true.

bodylist((A ',' B),C,D) :- !,
        bodylist(A,C,E),
        bodylist(B,E,D).
bodylist(A,B,C) :-
        goallist(A,B,C).

map_varlist_list([],A,B,B).
map_varlist_list([A,B,C|D],E,F,G) :-
        A==pref,
        (   memberv(B,E)
        ;   memberv(C,E)
        ),
        !,
        constC(F,B,H),
        constC(H,C,I),
        map_varlist_list(D,E,I,G).
map_varlist_list([A|B],C,D,E) :-
        constC(D,A,F),
        map_varlist_list(B,C,F,E).

unwrap_goal('$body'((A:-B),C,D),A,yes) :- !.
unwrap_goal(A,A,no).

unwrap_form('$body'((A:-B),C,D),E,F,F) :- !,
        end_of_list(D,G),
        G=[entry(E,F)|H].
unwrap_form(A,B,C,C).

x_call_moves_pre(A,B,C,D,E,F) :-
        varset(A,G),
        diffv(G,B,H),
        x_call_moves_pre_2(H,A,C,D,E,F).

fence_if_die(A,B,B) :-
        survive(A),
        !.
fence_if_die(A,B,C) :-
        \+survive(A),
        !,
        constC(B,fence,C).

parameter_setup(A,B,C,D,E,F,G,H,I,J,K) :-
        functor(A,L,M),
        A=..[L|N],
        uninit_set(H,O),
        uninit_set_type(mem,H,P),
        term_dupset_varset(A,Q,R),
        diffv(R,F,S),
        unionv(S,O,T),
        intersectv(Q,T,U),
        warn_dup_uninit(A,U),
        init_uninit_set(U,P,D,E,F,G,H,V,J,K),
        remove_uninit(U,V,I),
        uninit_set_type(reg,I,W),
        uninit_set_type(mem,I,X),
        get_given_flags(W,X,G,N,B),
        require(A,Y),
        split_uninit(Y,Z,C),
        uninit_set_type(reg,Z,A1),
        uninit_set_type(mem,Z,B1),
        get_req_flags(A1,B1,N,B),
        warn_req_uninit(1,A,B,L/M),
        true.

args_pre([],[],[],A,A,B,B,C,C,D,D,E,E) :- !,
        true.
args_pre([flag(A,B)|C],[D|E],[F|G],H,I,J,K,L,M,N,O,P,Q) :-
        one_arg_pre(A,B,D,F,H,R,J,S,L,T,N,U,P,V),
        args_pre(C,E,G,R,I,S,K,T,M,U,O,V,Q),
        true.

warn_fail(A,B,B) :-
        B=fail,
        !,
        warning(['Bad require modes--the goal ',A,' can never be reached.']).
warn_fail(A,B,B).

the_call(A,B,C,D,E,F,G,H,I,J) :-
        expand(A,B,C,G,H,I,J),
        !,
        true.
the_call(A,B,'$varlist'(C,D),E,F,G,H,H,I,J) :-
        macro_expand(B,I,J,C,K,L,M),
        !,
        fence_if_die(B,K,D),
        true.
the_call(A,B,'$varlist'(C),D,E,F,G,H,I,J) :-
        \+anyregs(B),
        !,
        low_reg(K),
        call_moves_pre(K,D,E,C,L,I,M),
        fence_if_die(B,L,N),
        functor(B,O,P),
        % stats(x,19),
        call_instruction(O/P,B,F,M,Q),
        call_moves_post(K,D,E,N,[],Q,J),
        remove_vars(G,H),
        true.
the_call(A,B,B,C,D,E,F,G,H,I) :-
        anyregs(B),
        !,
        H=[call(B)|I],
        functor(B,J,K),
        remove_vars(F,G),
        true.

unify_g(A=B,C,D,E,F,G,H,I,J,K,K) :-
        var(A),
        compound(B),
        memberv(A,K),
        \+implies(G,unbound(A)),
        !,
        unify_goal((A=L ',' L=B),C,D,E,F,G,H,I,J),
        true.
unify_g(A=B,C,D,E,F,G,H,I,J,K,K) :-
        var(B),
        compound(A),
        memberv(B,K),
        \+implies(G,unbound(B)),
        !,
        unify_goal((B=L ',' L=A),C,D,E,F,G,H,I,J),
        true.
unify_g(A=B,C,D,E,F,G,H,I,J,K,K) :-
        unify_goal(A=B,C,D,E,F,G,H,I,J),
        true.

unify_goal(A,B,C,D,E,F,G,H,I) :-
        unify(A,C,D,E,B,[],F,G,fail,H,I).

end_of_list(A,B) :-
        var(A),
        !,
        A=B.
end_of_list(A,B) :-
        nonvar(A),
        !,
        A=[C|D],
        end_of_list(D,B).

init_uninit(A,(B ',' C),D,E,F,G,H,I,J,K) :- !,
        init_uninit(A,B,D,L,F,M,H,N,J,O),
        init_uninit(A,C,L,E,M,G,N,I,O,K),
        true.
init_uninit(A,B,C,D,E,F,G,H,I,J) :-
        an_uninit_mode(B,mem,K),
        \+memberv(K,A),
        !,
        pragma_tag(K,var,G,L),
        I=[K|J],
        C=[K|D],
        includev(K,E,F),
        L=[move(K,[K])|H],
        true.
init_uninit(A,B,C,D,E,F,G,H,I,J) :-
        an_uninit_mode(B,reg,K),
        \+memberv(K,A),
        !,
        I=[K|J],
        new_var_nf(K,E,F,C,D,G,H),
        true.
init_uninit(A,B,C,C,D,D,E,E,F,F).

new_var_nf(A,B,C,D,E,F,G) :-
        includev(A,B,C),
        D=[A|H],
        H=[A|E],
        tag(var,I),
        F=[move(I^r(h),A)|J],
        J=[pragma(push(variable))|K],
        K=[push(A,r(h),1)|L],
        align_calc(1,M,N),
        align_pad(N,L,G),
        true.

init_uninit_set([],A,B,B,C,C,D,D,E,E) :- !,
        true.
init_uninit_set([A|B],C,D,E,F,G,H,I,J,K) :-
        inv(A,C),
        !,
        pragma_tag(A,var,J,L),
        D=[A|M],
        includev(A,F,N),
        L=[move(A,[A])|O],
        init_uninit_set(B,C,M,E,N,G,H,I,O,K),
        true.
init_uninit_set([A|B],C,D,E,F,G,H,I,J,K) :-
        \+inv(A,C),
        !,
        new_var_nf(A,F,L,D,M,J,N),
        init_uninit_set(B,C,M,E,L,G,H,I,N,K),
        true.

macro_expand(A,B,C,D,E,F,G) :-
        macro(A,B,C,D,E,H),
        sort(H,I),
        unionv(I,F,G).

call_moves_pre(A,[],[],B,B,C,C) :- !.
call_moves_pre(A,[flag(B,reg)|C],[D|E],F,G,H,I) :- !,
        J is A+1,
        call_moves_pre(J,C,E,F,G,H,I).
call_moves_pre(A,[B|C],[D|E],[pref,D,r(A)|F],G,H,I) :-
        constC(H,move(D,r(A)),J),
        K is A+1,
        call_moves_pre(K,C,E,F,G,J,I).

call_instruction(A,B,C,D,E) :-
        F=stree(A,G,H,I,J,K),
        member(F,C),
        !,
        compile_stree(F,nopeep,jump(L),noheader,D,E),
        true.
call_instruction(A,B,C,D,E) :-
        survive(B),
        !,
        D=[simple_call(A)|E],
        true.
call_instruction(A,B,C,D,E) :-
        \+survive(B),
        !,
        D=[call(A)|E],
        true.

call_moves_post(A,[],[],B,B,C,C) :- !.
call_moves_post(A,[flag(B,reg)|C],[D|E],[pref,r(A),D|F],G,H,I) :- !,
        constC(H,move(r(A),D),J),
        K is A+1,
        call_moves_post(K,C,E,F,G,J,I).
call_moves_post(A,[B|C],[D|E],F,G,H,I) :-
        J is A+1,
        call_moves_post(J,C,E,F,G,H,I).

warn_dup_uninit(A,[]) :- !.
warn_dup_uninit(A,[B]) :- !,
        comment(A,['Argument ',B,' of ',A,' is duplicated.']).
warn_dup_uninit(A,B) :-
        cons(B),
        !,
        comment(A,['Arguments ',B,' of ',A,' are duplicated.']).

get_given_flags(A,B,C,[],[]) :- !.
get_given_flags(A,B,C,[D|E],[flag(F,G)|H]) :-
        get_given_one(A,B,C,D,F),
        get_given_flags(A,B,C,E,H).

get_req_flags(A,B,[],[]) :- !.
get_req_flags(A,B,[C|D],[flag(E,F)|G]) :-
        get_req_one(A,B,C,F),
        get_req_flags(A,B,D,G).

warn_req_uninit(A,B,[],C) :- !.
warn_req_uninit(A,B,[flag(ini,mem)|C],D) :- !,
        arg(A,B,E),
        warning(B,['Argument ',E,' of ',B,' is given an init but requires an uninit.']),
        F is A+1,
        warn_req_uninit(F,B,C,D).
warn_req_uninit(A,B,[C|D],E) :-
        F is A+1,
        warn_req_uninit(F,B,D,E).

get_given_one(A,B,C,D,ini) :-
        nonvar(D),
        !.
get_given_one(A,B,C,D,mem) :-
        var(D),
        inv(D,B),
        !.
get_given_one(A,B,C,D,ini) :-
        var(D),
        inv(D,C),
        !.
get_given_one(A,B,C,D,reg(sf)) :-
        var(D),
        \+inv(D,C),
        !.
get_given_one(A,B,C,D,reg(uni)) :-
        var(D),
        inv(D,A),
        !.

get_req_one(A,B,C,mem) :-
        inv(C,B),
        !.
get_req_one(A,B,C,reg) :-
        inv(C,A),
        !.
get_req_one(A,B,C,ini).

one_arg_pre(reg(A),reg,B,B,C,C,D,D,E,E,F,F,G,G) :- !,
        true.
one_arg_pre(mem,reg,A,B,C,D,E,E,F,F,G,G,H,H) :- !,
        C=[A|D],
        true.
one_arg_pre(ini,reg,A,B,C,C,D,D,E,E,F,F,G,G) :- !,
        true.
one_arg_pre(reg(uni),mem,A,B,C,D,E,F,G,H,I,I,J,K) :- !,
        includev(B,G,H),
        C=[B|D],
        E=[B|L],
        L=[B|F],
        tag(var,M),
        N=1, % align(N),
        O is N-1,
        J=[move(M^r(h),B)|P],
        P=[adda(r(h),1,r(h))|Q],
        Q=[pad(O)|K],
        true.
one_arg_pre(reg(sf),mem,A,B,C,D,E,F,G,H,I,I,J,K) :- !,
        includev(A,G,L),
        includev(B,L,H),
        C=[A|M],
        M=[B|D],
        E=[A|N],
        N=[pref|O],
        O=[A|P],
        P=[B|Q],
        Q=[B|F],
        tag(var,R),
        S=1, % align(S),
        T is S-1,
        J=[move(R^r(h),A)|U],
        U=[move(R^r(h),B)|V],
        V=[adda(r(h),1,r(h))|W],
        W=[pad(T)|K],
        true.
one_arg_pre(mem,mem,A,A,B,C,D,D,E,E,F,F,G,G) :- !,
        B=[A|C],
        true.
one_arg_pre(ini,mem,A,B,C,D,E,F,G,H,I,J,K,L) :- !,
        includev(B,G,H),
        C=[B|D],
        E=[B|M],
        M=[B|F],
        tag(var,N),
        O=1, % align(O),
        P is O-1,
        K=[move(N^r(h),B)|Q],
        Q=[adda(r(h),1,r(h))|R],
        R=[pad(P)|L],
        combine_formula(uninit(B),I,J),
        true.
one_arg_pre(reg(uni),ini,A,B,C,D,E,F,G,H,I,J,K,L) :- !,
        C=[B|D],
        new_var(B,G,H,E,F,I,J,K,L),
        true.
one_arg_pre(reg(sf),ini,A,B,C,D,E,F,G,H,I,J,K,L) :- !,
        C=[A|M],
        M=[B|D],
        new_var(A,B,G,H,E,F,I,J,K,L),
        true.
one_arg_pre(mem,ini,A,B,C,D,E,F,G,H,I,J,K,L) :- !,
        includev(B,G,H),
        C=[A|M],
        M=[B|D],
        E=[A|N],
        N=[pref|O],
        O=[A|P],
        P=[B|F],
        pragma_tag(A,var,K,Q),
        Q=[move(A,[A])|R],
        R=[move(A,B)|L],
        combine_formula((deref(A)',' var(A)',' A==B),I,J),
        true.
one_arg_pre(ini,ini,A,B,C,D,E,F,G,H,I,J,K,L) :- !,
        varset(A,M),
        diffv(M,G,N),
        difflist(N,C,D),
        create_arg(A,B,G,H,E,F,I,J,K,L),
        true.

one_arg_post(reg(A),reg,B,B,C,C,D,E,F,F,G,G) :- !,
        includev(B,D,E),
        true.
one_arg_post(mem,reg,A,B,C,D,E,F,G,G,H,I) :- !,
        includev(B,E,F),
        C=[B|J],
        J=[A|D],
        H=[move(B,A)|I],
        true.
one_arg_post(ini,reg,A,B,C,D,E,F,G,H,I,J) :- !,
        includev(B,E,K),
        unify(B=A,K,F,C,D,G,H,I,J),
        true.
one_arg_post(reg(uni),mem,A,B,C,D,E,F,G,G,H,I) :- !,
        includev(A,E,F),
        C=[B|J],
        J=[A|D],
        H=[move(B,A)|I],
        true.
one_arg_post(reg(sf),mem,A,B,C,C,D,D,E,E,F,F) :- !,
        true.
one_arg_post(mem,mem,A,B,C,C,D,D,E,E,F,F) :- !,
        true.
one_arg_post(ini,mem,A,B,C,D,E,F,G,H,I,J) :- !,
        unify(B=A,E,F,C,D,G,H,I,J),
        true.
one_arg_post(reg(uni),ini,A,B,C,D,E,F,G,G,H,I) :- !,
        includev(A,E,F),
        C=[B|J],
        J=[A|D],
        H=[move(B,A)|I],
        true.
one_arg_post(reg(sf),ini,A,B,C,C,D,D,E,E,F,F) :- !,
        true.
one_arg_post(mem,ini,A,B,C,C,D,D,E,E,F,F) :- !,
        true.
one_arg_post(ini,ini,A,B,C,C,D,D,E,E,F,F) :- !,
        true.

new_var(A,B,C,D,E,F,G,H,I) :-
        combine_formula((deref(A)',' var(A)),F,G),
        new_var_nf(A,B,C,D,E,H,I),
        true.

new_var(A,B,C,D,E,F,G,H,I,J) :-
        A\==B,
        !,
        combine_formula((deref(A)',' var(A)',' deref(B)',' var(B)),G,H),
        new_var_nf(A,B,C,D,E,F,I,J),
        true.
new_var(A,B,C,D,E,F,G,H,I,J) :-
        A==B,
        !,
        new_var(A,C,D,E,F,G,H,I,J),
        true.

create_arg(A,A,B,B,C,C,D,D,E,E) :-
        var(A),
        inv(A,B),
        !,
        true.
create_arg(A,B,C,D,E,F,G,G,H,I) :-
        var(A),
        \+inv(A,C),
        !,
        new_var_nf(A,B,C,D,E,F,H,I),
        true.
create_arg(A,B,C,C,D,D,E,E,F,F) :-
        atomic(A),
        !,
        make_word(A,B),
        true.
create_arg(A,B,C,D,E,F,G,H,I,J) :-
        compound(A),
        !,
        term_formula(B,A,G,K),
        combine_formula(deref(B),K,H),
        E=[B|L],
        includev(B,C,M),
        make_word(A,N),
        I=[move(N,B)|O],
        M=P,
        uninit_set(H,Q),
        intersectv(P,Q,R),
        diffv(P,R,S),
        uninit_set_type(mem,H,T),
        writemode(A,T,S,U,L,F,O,J),
        U=V,
        unionv(V,R,D),
        true.

unify(A=B,C,D,E,F,G,H,I,J) :-
        varset(A=B,K),
        unify(A=B,K,C,D,E,F,G,H,fail,I,J).

call_moves(A,[],B,B) :- !.
call_moves(A,[B|C],D,E) :- !,
        constC(D,move(B,r(A)),F),
        G is A+1,
        call_moves(G,C,F,E).

head_moves_pre_2([],A,B,[],C,C) :- !.
head_moves_pre_2([A|B],C,D,E,F,G) :-
        inv(A,D),
        !,
        H is C+1,
        head_moves_pre_2(B,H,D,E,F,G).
head_moves_pre_2([A|B],C,D,[pref,r(C),A|E],F,G) :-
        constC(F,move(r(C),A),H),
        I is C+1,
        head_moves_pre_2(B,I,D,E,H,G).

head_moves_post_2([],A,[],B,B).
head_moves_post_2([A|B],C,[pref,A,D|E],F,G) :-
        head_reg(C,A,D),
        constC(F,move(A,D),H),
        head_moves_post_2(B,C,E,H,G).

x_call_moves_pre_2([],A,B,B,C,C).
x_call_moves_pre_2([A|B],C,[pref,A,D|E],F,G,H) :-
        head_reg(C,A,D),
        constC(G,move(A,D),I),
        x_call_moves_pre_2(B,C,E,F,I,H).

x_call_moves_post_2([],A,B,B,C,C).
x_call_moves_post_2([A|B],C,[pref,D,A|E],F,G,H) :-
        head_reg(C,A,D),
        constC(G,move(D,A),I),
        x_call_moves_post_2(B,C,E,F,I,H).

vars_to_rename(A,B,C) :-
        survive(A),
        !,
        vars_to_be_dereffed(A,B,D),
        bindset(A,B,E),
        unionv(D,E,C).
vars_to_rename(A,B,C) :-
        \+survive(A),
        !,
        vars_to_be_dereffed(A,B,C).

update_mapping(A,B,C,D,E,F,G) :-
        map_terms(D,F,A,C),
        map_terms(C,B,F,H),
        diffv(A,D,I),
        map_terms(A,B,I,J),
        append(J,H,G),
        append(I,D,E).

init_extra_vars([],[],[],A,A,B,B,C,C,D,D) :- !,
        true.
init_extra_vars([A|B],[C|D],[E|F],G,H,I,J,K,L,M,N) :-
        init_extra_var(A,C,E,G,O,I,P,K,Q,M,R),
        init_extra_vars(B,D,F,O,H,P,J,Q,L,R,N),
        true.

vars_to_be_dereffed(A,B,C) :-
        require(A,D),
        split_deref(D,E),
        varset(E,F),
        not_deref(F,B,C).

not_deref([],A,[]) :- !.
not_deref([A|B],C,[A|D]) :-
        \+implies(C,deref(A)),
        !,
        not_deref(B,C,D).
not_deref([A|B],C,D) :-
        not_deref(B,C,D).

init_extra_var(A,B,C,D,E,F,G,H,H,I,J) :-
        implies(H,deref(A)),
        !,
        includev(B,F,G),
        D=[pref|K],
        K=[C|L],
        L=[B|E],
        I=[move(C,B)|J],
        true.
init_extra_var(A,B,C,D,E,F,G,H,I,J,K) :-
        includev(B,F,G),
        D=[pref|L],
        L=[C|M],
        M=[B|E],
        J=[deref(C,B)|K],
        update_formula(deref(A),H,I),
        true.

gpermvars(true,A,A,B,B,C,C) :- !,
        true.
gpermvars((A ',' B),C,D,E,F,G,H) :-
        gpermstep(A,C,I,E,J,G,K),
        gpermsurv(A,I,L,J,M),
        gpermvars(B,L,D,M,F,K,H),
        true.

gpermstep(A,B,C,D,D,E,F) :-
        varset(A,G),
        intersectv(G,D,H),
        unionv(E,H,F),
        unionv(B,G,C),
        true.

gpermsurv(A,B,B,C,C) :-
        survive(A),
        !,
        true.
gpermsurv(A,B,B,C,D) :-
        \+survive(A),
        !,
        unionv(C,B,D),
        true.

add_uninit_void(A,B,C,D,E) :-
        var_args(A,F),
        term_dupset(A,G),
        diffv(F,G,H),
        diffv(H,B,I),
        uninit_set(D,J),
        diffv(I,J,K),
        make_uninit(K,L,true),
        intersectv(I,C,M),
        make_voids(M,N,L),
        union_formula(N,D,O),
        squeeze_conj(O,E).

tree_trav_entry(entry(A),B,A).
tree_trav_entry(mode(A,B,C),D,E) :-
        (   implies(D,A) ->
            tree_trav_entry(B,D,E)
        ;   tree_trav_entry(C,D,E)
        ).

make_uninit([],A,A).
make_uninit([A|B],C,D) :-
        co(uninit(either,A),C,E),
        make_uninit(B,E,D).

make_voids([],A,A).
make_voids([A|B],C,D) :-
        co(void(A),C,E),
        make_voids(B,E,D).

clause_allocate(A) :-
        clause_allocate(A,B,C).

varlist(A,B) :-
        varlist(A,B,[]).

local_allocate(A,B,C) :-
        % stats(reg,2),
        usage(A,D),
        % stats(reg,3),
        alloc_voids(D),
        % stats(reg,4),
        find_perms(A,E),
        % stats(reg,5),
        split_temps(D,E,F),
        alloc_temps(F,A),
        % stats(reg,6),
        split_perms(D,G),
        alloc_perms(G,A),
        % stats(reg,7),
        num_perms(G,C),
        env_needed(A,C,B),
        % stats(reg,8),
        !.

usage(A,B) :-
        varset(A,C),
        init_usage(C,B),
        first_usage(A,1,B),
        length(A,D),
        reverse(A,E),
        last_usage(E,D,B).

alloc_voids([]).
alloc_voids([A|B]) :-
        get_usage(A,C,D,E),
        (   D=E ->
            C=r(void)
        ;   true
        ),
        alloc_voids(B).

find_perms(A,B) :-
        permvars(A,[],C,[],D,[],B).

split_temps([],A,[]) :- !.
split_temps([A|B],C,[A|D]) :-
        get_usage(A,E,F,G),
        \+inv(E,C),
        !,
        split_temps(B,C,D).
split_temps([A|B],C,D) :-
        split_temps(B,C,D).

alloc_temps(A,B) :-
        pref_closure(A,B),
        rest_temps(A,B,A).

split_perms([],[]).
split_perms([A|B],[A|C]) :-
        get_usage(A,D,E,F),
        var(D),
        !,
        split_perms(B,C).
split_perms([A|B],C) :-
        split_perms(B,C).

alloc_perms(A,B) :-
        write_once,
        !,
        naive_alloc_perms(B).
alloc_perms(A,B) :-
        \+write_once,
        !,
        soph_alloc_perms(A).

num_perms(A,B) :-
        low_perm(C),
        D is C-1,
        num_perms(A,D,E),
        B is E-D.

env_needed(A,B,yes) :-
        B>0,
        !.
env_needed(A,B,yes) :-
        env_needed(A),
        !.
env_needed(A,B,no).

varlist((A:-B),C,D) :-
        bodylist(B,C,D),
        !.

goallist('$varlist'(A),B,C) :- !,
        difflist(A,B,C).
goallist('$varlist'(A,B),C,D) :- !,
        insert(A,B,C,D).
goallist(A,B,C) :-
        anyregs(A),
        !,
        varbag(A,B,D),
        fence_if_die(A,D,C).
goallist(A,B,C) :-
        \+anyregs(A),
        !,
        A=..[D|E],
        low_reg(F),
        goallist(F,E,B,G),
        fence_if_die(A,G,C).

goallist(A,[],B,B).
goallist(A,[B|C],D,E) :-
        var(B),
        !,
        constC(D,pref,F),
        constC(F,B,G),
        constC(G,r(A),H),
        I is A+1,
        goallist(I,C,H,E).
goallist(A,[B|C],D,E) :-
        nonvar(B),
        !,
        constC(D,r(A),F),
        G is A+1,
        goallist(G,C,F,E).

env_needed([A|B]) :-
        A==fence,
        \+trivial_moves(B).
env_needed([A|B]) :-
        env_needed(B).

trivial_moves([]).
trivial_moves([pref,A,A|B]) :-
        trivial_moves(B).

get_usage(var(A,B,C),C,B,A).

init_usage([],[]) :- !.
init_usage([A|B],[var(C,D,A)|E]) :-
        init_usage(B,E).

first_usage([],A,B) :- !.
first_usage([A|B],C,D) :-
        var(A),
        !,
        E is C+1,
        find_usage(A,D,F,G),
        fix_it(C,F),
        first_usage(B,E,D).
first_usage([A|B],C,D) :-
        nonvar(A),
        !,
        E is C+1,
        first_usage(B,E,D).

last_usage([],A,B) :- !.
last_usage([A|B],C,D) :-
        var(A),
        !,
        E is C-1,
        find_usage(A,D,F,G),
        fix_it(C,G),
        last_usage(B,E,D).
last_usage([A|B],C,D) :-
        nonvar(A),
        !,
        E is C-1,
        last_usage(B,E,D).

find_usage(A,[var(B,C,D)|E],F,G) :-
        (   A==D ->
            F=C,
            G=B
        ;   find_usage(A,E,F,G)
        ).

fix_it(A,B) :-
        (   var(B) ->
            A=B
        ;   true
        ).

permvars([],A,A,B,B,C,C) :- !,
        true.
permvars([A|B],C,D,E,F,G,H) :-
        var(A),
        !,
        includev(A,C,I),
        permstep(A,E,J,G,K),
        permvars(B,I,D,J,F,K,H),
        true.
permvars([A|B],C,D,E,F,G,H) :-
        A==fence,
        !,
        unionv(E,C,I),
        permvars(B,C,D,I,F,G,H),
        true.
permvars([A|B],C,D,E,F,G,H) :-
        nonvar(A),
        A\==fence,
        !,
        permvars(B,C,D,E,F,G,H),
        true.

permstep(A,B,B,C,D) :-
        inv(A,B),
        !,
        includev(A,C,D),
        true.
permstep(A,B,B,C,C) :- !,
        true.

naive_alloc_perms(A) :-
        reverse(A,B),
        low_perm(C),
        naive_alloc_perms(B,C,D).

soph_alloc_perms(A) :-
        sort(A,B),
        reverse(B,C),
        soph_alloc_perms(C,C).

naive_alloc_perms([],A,A) :- !.
naive_alloc_perms([A|B],C,D) :-
        alloc_if_var(A,C,E),
        naive_alloc_perms(B,E,D).

alloc_if_var(A,B,C) :-
        (   var(A) ->
            C is B+1,
            A=p(B)
        ;   C=B
        ).

soph_alloc_perms([],A) :- !.
soph_alloc_perms([A|B],C) :-
        get_usage(A,D,E,F),
        low_perm(G),
        max_int(H),
        range(G,I,H),
        J=p(I),
        \+usage_overlap(E,F,C,J),
        D=J,
        soph_alloc_perms(B,C).

usage_overlap(A,B,[C|D],E) :-
        get_usage(C,F,G,H),
        nonvar(F),
        overlap(A,B,G,H),
        F=E,
        !.
usage_overlap(A,B,[C|D],E) :-
        usage_overlap(A,B,D,E).

num_perms([],A,A).
num_perms([A|B],C,D) :-
        get_usage(A,p(E),F,G),
        max(C,E,H),
        num_perms(B,H,D).

pref_closure(A,B) :-
        pref_temps(A,B,A,0,C),
        pref_closure(A,B,C).

rest_temps([],A,B) :- !.
rest_temps([A|B],C,D) :-
        get_usage(A,E,F,G),
        (   var(E),
            rest_allocate(E,C,D,F,G) ->
            pref_closure(D,C)
        ;   true
        ),
        rest_temps(B,C,D).

pref_temps([],A,B,C,C) :- !.
pref_temps([A|B],C,D,E,F) :-
        get_usage(A,G,H,I),
        (   var(G),
            pref_allocate(G,C,D,H,I) ->
            J is E+1
        ;   J=E
        ),
        pref_temps(B,C,D,J,F).

pref_closure(A,B,C) :-
        C>0,
        !,
        pref_closure(A,B).
pref_closure(A,B,C) :-
        C=0,
        !.

pref_allocate(A,B,C,D,E) :-
        D=:=E,
        prefer(A,B,F),
        A=F.
pref_allocate(A,B,C,D,E) :-
        D=\=E,
        prefer(A,B,F),
        \+already_used(A,F,D,E,B,C),
        A=F.

prefer(A,[B,C,D|E],D) :-
        pref(B),
        A==C,
        temp_register(D).
prefer(A,[B,C,D|E],C) :-
        pref(B),
        A==D,
        temp_register(C).
prefer(A,[B|C],D) :-
        prefer(A,C,D).

already_used(A,B,C,D,E,F) :-
        usage_overlap(C,D,F,B),
        !.
already_used(A,B,C,D,E,F) :-
        reg_overlap(A,B,1,C,D,E),
        !.

rest_allocate(A,B,C,D,E) :-
        low_reg(F),
        high_reg(G),
        range(F,H,G),
        I=r(H),
        \+already_used(A,I,D,E,B,C),
        A=I.

reg_overlap(A,B,C,D,E,[F|G]) :-
        C=<D,
        H is C+1,
        reg_overlap(A,B,H,D,E,G).
reg_overlap(A,B,C,D,E,F) :-
        D<C,
        C<E,
        no_conflict(A,B,F,G),
        H is C+1,
        reg_overlap(A,B,H,D,E,G).
reg_overlap(A,B,C,D,E,[F|G]) :-
        D<C,
        C<E,
        B==F.

no_conflict(A,B,[C,D,E|F],F) :-
        pref(C),
        (   A==D,
            B==E
        ;   A==E,
            B==D
        ),
        !.
no_conflict(A,B,[C|D],D) :-
        B\==C,
        !.

pref(A) :-
        A==pref.

overlap(A,B,C,D) :-
        A<C,
        C<B.
overlap(A,B,C,D) :-
        A<D,
        D<B.
overlap(A,B,C,D) :-
        A<C,
        D<B.
overlap(A,B,C,D) :-
        C<A,
        B<D.

temp_register(A) :-
        nonvar(A),
        A=r(B),
        integer(B).

ct_u(nonvar(A),B,B,C,C) :-
        inv(A,C),
        !,
        true.
ct_u(A,B,C,D,D) :-
        ctest(A,B,C),
        true.

rt_d(A,A,B,B,C,C,D,D,E,E) :-
        inv(A,E),
        !,
        true.
rt_d(A,B,C,D,E,F,G,H,I,I) :-
        rtest_deref(A,B,C,D,E,F,G,H),
        true.

rt_in_d(A,A,B,B,C,C,D,D,E,E) :-
        inv(A,E),
        !,
        true.
rt_in_d(A,B,C,D,E,F,G,H,I,I) :-
        rtest_in_deref(A,B,C,D,E,F,G,H),
        true.

unify(A=B,C,D,E,F,G) :-
        unify(A=B,C,D,H,E,F,G).

unify(A=B,C,D,E,F,G,H) :-
        unify(A=B,E,C,D,F,I,G,H).

unify(A=B,C,D,E,F,G,H,I) :-
        unify(A=B,D,E,C,[],F,G,H,I).

unify(A=B,C,D,E,F,G,H,I,J,K,L) :-
        D=M,
        uninit_set(H,N),
        diffv(M,N,O),
        grounds_in_form(H,P),
        rderef_set(H,Q),
        intersectv(Q,P,R),
        uninit_set_type(mem,H,S),
        unify_2(A,B,R,T,P,U,S,C,O,V,F,G,H,W,J,K,L),
        V=X,
        varset(A=B,Y),
        unionv(X,Y,E),
        intersectv(N,X,Z),
        remove_uninit(Z,W,A1),
        update_formula(A=B,M,A1,I),
        true.
unify((A ',' B),C,D,E,F,G,H,I,J,K,L) :-
        unify(A,C,D,M,F,N,H,O,J,K,P),
        unify(B,C,M,E,N,G,O,I,J,P,L),
        true.

unify_2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        var(A),
        var(B),
        !,
        unify_var(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
        true.
unify_2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        var(A),
        nonvar(B),
        !,
        unify_3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
        true.
unify_2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        nonvar(A),
        var(B),
        !,
        unify_3(B,A,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
        true.
unify_2(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        nonvar(A),
        nonvar(B),
        !,
        unify_nonvar(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
        true.

unify_var(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        inv(A,I),
        inv(B,I),
        !,
        unify_var_i(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
        true.
unify_var(A,B,C,C,D,D,E,F,G,H,I,J,K,L,M,N,O) :-
        inv(A,G),
        \+inv(B,G),
        !,
        uninit_dest(B,P,Q,E),
        cond_pref(Q,I,R),
        R=[A|S],
        S=[B|J],
        map_formula(A,B,K,L),
        cond_pragma_tag(Q,B,N,T),
        T=[move(A,P)|O],
        includev(B,G,H),
        true.
unify_var(A,B,C,C,D,D,E,F,G,H,I,J,K,L,M,N,O) :-
        \+inv(A,G),
        inv(B,G),
        !,
        uninit_dest(A,P,Q,E),
        cond_pref(Q,I,R),
        R=[B|S],
        S=[A|J],
        map_formula(B,A,K,L),
        cond_pragma_tag(Q,A,N,T),
        T=[move(B,P)|O],
        includev(A,G,H),
        true.
unify_var(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        \+inv(A,I),
        \+inv(B,I),
        !,
        unify_var_u(A,B,C,D,E,F,G,I,J,K,L,M,N,P,Q),
        true.

unify_3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        \+inv(A,I),
        !,
        uninit_unify(A,B,nonlast,R,C,D,E,F,G,H,I,S,K,L,M,N,P,Q),
        includev(A,S,J),
        true.
unify_3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        inv(A,E),
        !,
        init_unify(A,B,nonlast,R,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
        true.
unify_3(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        unify_depth(A,B,R,S,T),
        init_unify(A,R,nonlast,U,C,V,E,W,G,H,I,X,K,Y,M,Z,O,P,A1),
        unify_3_conj(T,V,D,W,F,G,H,X,J,Y,L,Z,N,O,A1,Q),
        true.

unify_nonvar(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        functor(A,R,S),
        functor(B,R,S),
        !,
        unify_nonvar(A,B,1,S,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
        true.
unify_nonvar(A,B,C,C,D,D,E,F,G,G,H,H,I,J,K,L,M) :-
        L=[fail|M],
        update_formula(fail,I,J),
        true.

unify_nonvar(A,B,C,D,E,E,F,F,G,H,I,I,J,J,K,K,L,M,M) :-
        C>D,
        !,
        true.
unify_nonvar(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :-
        C=<D,
        !,
        arg(C,A,T),
        arg(C,B,U),
        unify_2(T,U,E,V,G,W,I,J,K,X,M,Y,O,Z,Q,R,A1),
        B1 is C+1,
        unify_nonvar(A,B,B1,D,V,F,W,H,I,J,X,L,Y,N,Z,P,Q,A1,S),
        true.

uninit_unify(A,B,C,[D|E],F,F,G,G,H,I,J,K,L,M,N,O,P,Q) :-
        compound(B),
        !,
        uninit_bind(A,B,H,L,R,N,S,P,T),
        writemode(C,A,B,D,E,H,I,J,K,R,M,S,O,T,Q),
        true.
uninit_unify(A,B,C,D,E,E,F,F,G,H,I,I,J,K,L,M,N,O) :-
        atomic(B),
        !,
        uninit_bind(A,B,G,J,K,L,M,N,O),
        true.
uninit_unify(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :-
        var(B),
        !,
        unify_var(A,B,E,F,G,H,I,J,K,L,M,N,O,P,S,Q,R),
        true.

init_unify(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :-
        O=T,
        split_formula(U,K,A=B,T,V,W),
        xinit_unify(A,B,C,D,E,F,G,H,I,J,K,L,M,N,V,X,Q,R,S),
        X=Y,
        combine_formula(Y,W,P),
        true.

unify_depth(A,B,C,D,E) :-
        unify_depth_2(0,B,C,F,true,G,[]),
        sort(G,D),
        flat_conj(F,E).

unify_3_conj(true,A,A,B,B,C,D,E,E,F,F,G,G,H,I,I).
unify_3_conj((A=B ',' C),D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) :-
        unify_2(A,B,D,S,F,T,H,I,J,U,L,V,N,W,P,Q,X),
        unify_3_conj(C,S,E,T,G,H,I,U,K,V,M,W,O,P,X,R),
        true.

unify_var_i(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        atomic_value(M,A,R),
        !,
        unify_2(B,R,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
        true.
unify_var_i(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        atomic_value(M,B,R),
        !,
        unify_2(A,R,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
        true.
unify_var_i(A,B,C,C,D,D,E,F,G,G,H,I,J,K,L,M,N) :-
        ctest(atomic(A),J,O),
        ctest(atomic(B),O,P),
        !,
        rtest_in_deref(A,Q,H,R,P,S,M,T),
        rtest_in_deref(B,U,R,V,S,K,T,W),
        V=[Q|X],
        X=[U|I],
        W=[equal(Q,U,L)|N],
        true.
unify_var_i(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) :-
        unify_flag(M,A,R),
        unify_flag(M,B,S),
        unify_var_i(R,S,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
        true.

uninit_dest(A,A,yes,B) :-
        \+inv(A,B),
        !,
        true.
uninit_dest(A,[A],no,B) :-
        inv(A,B),
        !,
        true.

cond_pref(yes,A,B) :- !,
        A=[pref|B],
        true.
cond_pref(no,A,A) :- !,
        true.

map_formula(A,B,C,D) :-
        map_terms([A],[B],C,E),
        union_formula(C,E,D).

cond_pragma_tag(yes,A,B,B).
cond_pragma_tag(no,A,B,C) :-
        pragma_tag(A,var,B,C),
        true.

unify_var_u(A,B,C,C,D,D,E,F,G,H,I,J,K,L,M) :-
        inv(A,E),
        inv(B,E),
        !,
        includev(A,F,N),
        includev(B,N,G),
        update_formula(deref(A),J,O),
        update_formula(deref(B),O,P),
        update_formula(var(A),P,Q),
        update_formula(var(B),Q,K),
        H=[A|R],
        R=[pref|S],
        S=[A|T],
        T=[B|I],
        pragma_tag(A,var,L,U),
        U=[move(A,[A])|V],
        pragma_tag(B,var,V,W),
        W=[move(A,[B])|M],
        true.
unify_var_u(A,B,C,C,D,D,E,F,G,H,I,J,K,L,M) :-
        inv(A,E),
        \+inv(B,E),
        !,
        includev(A,F,N),
        includev(B,N,G),
        update_formula(deref(A),J,O),
        update_formula(deref(B),O,P),
        update_formula(var(A),P,Q),
        update_formula(var(B),Q,K),
        H=[A|R],
        R=[pref|S],
        S=[A|T],
        T=[B|I],
        pragma_tag(A,var,L,U),
        U=[move(A,[A])|V],
        V=[move(A,B)|M],
        true.
unify_var_u(A,B,C,C,D,D,E,F,G,H,I,J,K,L,M) :-
        \+inv(A,E),
        inv(B,E),
        !,
        includev(A,F,N),
        includev(B,N,G),
        update_formula(deref(A),J,O),
        update_formula(deref(B),O,P),
        update_formula(var(A),P,Q),
        update_formula(var(B),Q,K),
        H=[pref|R],
        R=[B|S],
        S=[A|T],
        T=[A|I],
        L=[move(B,A)|U],
        pragma_tag(A,var,U,V),
        V=[move(A,[A])|M],
        true.
unify_var_u(A,B,C,C,D,D,E,F,G,H,I,J,J,K,L) :-
        \+inv(A,E),
        \+inv(B,E),
        !,
        new_var_nf(A,B,F,G,H,I,K,L),
        true.

unify_flag(A,B,nonvar) :-
        implies(A,nonvar(B)),
        !.
unify_flag(A,B,var) :-
        implies(A,var(B)),
        !.
unify_flag(A,B,?).

unify_var_i(var,nonvar,A,B,C,C,D,D,E,F,G,G,H,I,J,K,L,M,N) :- !,
        rtest_in_deref(A,O,H,P,J,Q,M,R),
        rtest_in(trail(O),O,Q,S,L,R,T),
        P=[B|U],
        U=[O|I],
        pragma_tag_form(O,S,K,T,V),
        V=[move(B,[O])|N],
        true.
unify_var_i(nonvar,var,A,B,C,C,D,D,E,F,G,G,H,I,J,K,L,M,N) :- !,
        rtest_in_deref(B,O,H,P,J,Q,M,R),
        rtest_in(trail(O),O,Q,S,L,R,T),
        P=[A|U],
        U=[O|I],
        pragma_tag_form(O,S,K,T,V),
        V=[move(A,[O])|N],
        true.
unify_var_i(A,B,C,D,E,E,F,F,G,H,I,I,J,K,L,M,N,O,P) :-
        rtest_in_deref(C,Q,J,R,L,S,O,T),
        rtest_in_deref(D,U,R,V,S,W,T,X),
        V=[pref|Y],
        Y=[Q|Z],
        Z=[U|A1],
        fence_if_nosurvive(A1,K),
        X=[unify(Q,U,A,B,N)|P],
        remove_vars(W,M),
        true.

pragma_tag_form(A,B,B,C,D) :-
        get_tag(B,A,E),
        !,
        C=[pragma(tag(A,E))|D],
        true.
pragma_tag_form(A,B,B,C,C).

fence_if_nosurvive(A,A) :-
        survive('$unify'(B,C)),
        !,
        true.
fence_if_nosurvive(A,B) :-
        \+survive('$unify'(C,D)),
        !,
        A=[fence|B],
        true.

new_var_nf(A,B,C,D,E,F,G,H) :-
        A\==B,
        !,
        includev(A,C,I),
        includev(B,I,D),
        E=[A|J],
        J=[pref|K],
        K=[A|L],
        L=[B|M],
        M=[B|F],
        tag(var,N),
        G=[move(N^r(h),A)|O],
        O=[move(N^r(h),B)|P],
        P=[pragma(push(variable))|Q],
        Q=[push(B,r(h),1)|R],
        align_calc(1,S,T),
        align_pad(T,R,H),
        true.
new_var_nf(A,B,C,D,E,F,G,H) :-
        A==B,
        !,
        new_var_nf(A,C,D,E,F,G,H),
        true.

uninit_bind(A,B,C,D,E,F,G,H,I) :-
        D=[A|E],
        make_word(B,J),
        uninit_move(J,A,C,F,K,H,I),
        term_formula(A,B,K,G),
        true.

writemode(last,A,B,C,D,E,F,G,G,H,H,I,I,J,K) :- !,
        J=[jump(C)|K],
        true.
writemode(nonlast,A,B,C,D,E,F,G,H,I,J,K,L,M,N) :- !,
        M=[pragma(push(term(O)))|P],
        b_writemode(B,D,E,F,G,H,I,J,K,L,P,N,A,0,O),
        true.

make_word(A,B^r(h)) :-
        compound(A),
        term_tag(A,B),
        !.
make_word(A,B^A) :-
        atom(A),
        term_tag(A,B),
        !.
make_word(A,A) :-
        number(A),
        !.
make_word(A,A) :-
        var(A),
        !.

uninit_move(A,B,C,D,E,F,G) :-
        \+inv(B,C),
        !,
        update_formula(deref(B),D,E),
        F=[move(A,B)|G],
        true.
uninit_move(A,B,C,D,D,E,F) :-
        inv(B,C),
        !,
        pragma_tag(B,var,E,G),
        G=[move(A,[B])|F],
        true.

term_formula(A,B,C,D) :-
        nonvar(B),
        !,
        functor(B,E,F),
        update_formula('$name_arity'(A,E,F),C,D),
        true.
term_formula(A,B,C,C) :-
        var(B),
        !,
        true.

xinit_unify(A,B,C,[D|E],F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :-
        atomic(B),
        ct_u(nonvar(A),P,U,H,V),
        !,
        rtest_deref(A,W,N,X,U,Y,S,Z),
        unify_rm(W,B,C,E,no,F,G,V,I,J,K,L,M,X,O,Y,Q,R,Z,T),
        true.
xinit_unify(A,B,C,[D|E],F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :-
        compound(B),
        ct_u(nonvar(A),P,U,H,V),
        !,
        rtest_deref(A,W,N,X,U,Y,S,Z),
        term_test(B,W,A1),
        rtest(A1,W,Y,B1,R,Z,C1),
        unify_rm(W,B,C,E,no,F,G,V,I,J,K,L,M,X,O,B1,Q,R,C1,T),
        true.
xinit_unify(A,B,C,[D|E],F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :-
        nonvar(B),
        ctest(var(A),P,U),
        !,
        rtest_deref(A,V,N,W,U,X,S,Y),
        unify_wm(V,B,C,E,F,G,H,I,J,K,L,M,W,O,X,Q,R,Y,T),
        true.
xinit_unify(A,B,C,[D|E],F,F,G,G,H,I,J,J,K,L,M,N,O,P,Q) :-
        true, % compile_option(uni),
        atomic(B),
        !,
        rtest_deref(A,R,K,S,M,T,P,U),
        rtest_in(trail_if_var(R),R,T,N,O,U,V),
        term_tag(B,W),
        make_word(B,X,Y),
        S=[R|L],
        V=[unify_atomic(R,Y,O)|Q],
        true.
xinit_unify(A,B,C,[D|E],F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :-
        nonvar(B),
        !,
        rtest_deref(A,U,N,V,P,W,S,X),
        term_tag(B,Y),
        term_test(B,U,Z),
        V=[U|A1],
        X=[switch(unify,Y,U,B1,C1,R)|T],
        W=D1,
        update_formula(var(U),D1,E1),
        unify_wm(U,B,C,E,L,E1,F1,B1,F,G1,H,H1,J,A1,I1,R),
        update_formula(Z,D1,J1),
        unify_rm(U,B,C,E,yes,J1,K1,C1,G1,G,H1,I,J,K,L,M,I1,O,R),
        intersect_formula(F1,K1,Q),
        true.
xinit_unify(A,B,C,[D|E],F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :-
        var(B),
        !,
        unify_var(A,B,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T),
        true.

unify_rm(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :-
        structure(B),
        !,
        N=[A|U],
        functor(B,V,W),
        B=..[V|X],
        rtest_in('$name_arity'(A,V,W),A,P,Y,R,S,Z),
        unify_args(X,1,A,D,E,F,G,H,I,J,K,L,M,U,O,Y,Q,R,Z,T,structure),
        true.
unify_rm(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :-
        cons(B),
        !,
        N=[A|U],
        B=[V|W],
        X=[V,W],
        unify_args(X,0,A,D,E,F,G,H,I,J,K,L,M,U,O,P,Q,R,S,T,cons),
        true.
unify_rm(A,B,C,D,E,F,F,G,G,H,I,J,J,K,L,M,N,O,P,Q) :-
        atom(B),
        !,
        K=[A|L],
        rtest_in('$name_arity'(A,B,0),A,M,N,O,P,Q),
        true.
unify_rm(A,B,C,D,E,F,F,G,G,H,I,J,J,K,L,M,N,O,P,Q) :-
        number(B),
        !,
        K=[A|L],
        rtest_in('$name_arity'(A,B,0),A,M,N,O,P,Q),
        true.

unify_wm(A,B,C,[D|E],F,F,G,G,H,I,J,K,L,M,N,O,P,Q,R) :-
        compound(B),
        !,
        L=[A|S],
        rtest_in(trail(A),A,N,T,P,Q,U),
        term_tag(B,V),
        pragma_tag(A,var,U,W),
        W=[move(V^r(h),[A])|X],
        writemode(C,A,B,D,E,H,I,J,K,S,M,T,O,X,R),
        true.
unify_wm(A,B,C,D,E,E,F,F,G,H,I,I,J,K,L,M,N,O,P) :-
        atom(B),
        !,
        J=[A|K],
        term_tag(B,Q),
        rtest_in(trail(A),A,L,M,N,O,R),
        pragma_tag(A,var,R,S),
        S=[move(Q^B,[A])|P],
        true.
unify_wm(A,B,C,D,E,E,F,F,G,H,I,I,J,K,L,M,N,O,P) :-
        number(B),
        !,
        J=[A|K],
        rtest_in(trail(A),A,L,M,N,O,Q),
        pragma_tag(A,var,Q,R),
        R=[move(B,[A])|P],
        true.

make_word(A,B,C^ (r(h)+B)) :-
        compound(A),
        term_tag(A,C),
        !.
make_word(A,B,C^A) :-
        atom(A),
        term_tag(A,C),
        !.
make_word(A,B,A) :-
        number(A),
        !.
make_word(A,B,A) :-
        var(A),
        !.

unify_wm(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :-
        unify_wm(A,B,C,D,I,J,K,L,M,[],E,Q,N,O,F,G,P,H,[]),
        true.

unify_rm(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) :-
        unify_rm(A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,F,G,S,H,[]),
        true.

unify_args(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :-
        unify_args([],A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U),
        true.

unify_args([],[],A,B,C,D,E,E,F,F,G,H,I,I,J,J,K,K,L,M,M,N) :- !,
        true.
unify_args(A,[B],C,D,E,no,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- !,
        unify_arg(A,V,B,C,D,nonlast,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U),
        true.
unify_args(A,[B],C,D,E,yes,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U) :- !,
        unify_arg(A,V,B,C,D,last,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U),
        true.
unify_args([],[A,B|C],D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :-
        fail, % align(2),
        0 is D mod 2,
        !,
        move_arg(E,D,X,H,Y,J,Z,N,A1,P,B1,R,C1,U,D1,W),
        E1 is D+1,
        move_arg(E,E1,F1,Y,G1,Z,H1,A1,I1,B1,J1,C1,K1,D1,L1,W),
        unify_args([X,F1],[A,B|C],D,E,F,G,G1,I,H1,K,L,M,I1,O,J1,Q,K1,S,T,L1,V,W),
        true.
unify_args(A,[B,C|D],E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) :- !,
        unify_arg(A,Y,B,E,F,nonlast,Z,I,A1,K,B1,M,N,O,C1,Q,D1,S,E1,U,V,F1,X),
        G1 is E+1,
        unify_args(Y,[C|D],G1,F,G,H,A1,J,B1,L,M,N,C1,P,D1,R,E1,T,U,F1,W,X),
        true.

unify_arg(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :-
        var(C),
        \+inv(C,N),
        \+inv(C,L),
        !,
        move_arg(A,B,E,D,C,H,I,J,K,N,X,P,Q,R,S,U,V,W),
        sort(X,O),
        true.
unify_arg(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) :-
        move_arg(A,B,E,D,X,H,Y,J,Z,N,A1,P,B1,R,C1,U,D1,W),
        init_unify(X,C,F,G,Y,I,Z,K,L,M,A1,O,B1,Q,C1,S,T,D1,V),
        true.

move_arg(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :-
        pragma_tag(A,P,N,Q),
        pragma_align(A,B,Q,R),
        R=[move([A+B],C)|O],
        J=[A|S],
        S=[C|K],
        includev(C,H,I),
        gnd_drf_update(C,A,D,E,F,G,L,M),
        true.

move_arg([A|B],B,C,D,A,E,E,F,F,G,G,H,H,I,I,J,J,K).
move_arg([],[],A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) :-
        move_arg(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P),
        true.

pragma_align(A,B,C,D) :-
        E=1, % align(E),
        0 is B mod E,
        !,
        C=[pragma(align(A,E))|D],
        true.
pragma_align(A,B,C,C).

gnd_drf_update(A,B,C,D,E,F,G,H) :-
        inv(B,E),
        !,
        includev(A,E,F),
        combine_formula(ground(A),G,I),
        drf_update(A,B,C,D,I,H),
        true.
gnd_drf_update(A,B,C,C,D,D,E,E).

drf_update(A,B,C,D,E,F) :-
        inv(B,C),
        !,
        includev(A,C,D),
        combine_formula(rderef(A),E,F),
        true.
drf_update(A,B,C,C,D,D).

writemode(A,B,C,D,E,F,G,H) :-
        writemode(nonlast,none,A,I,J,B,[],C,D,E,F,true,K,G,H),
        true.

align_calc(A,B,C) :-
        D=1, % align(D),
        B is(A+D-1)//D*D,
        C is B-A.

align_pad(0,A,A) :- !,
        true.
align_pad(A,B,C) :-
        A=\=0,
        !,
        B=[pad(A)|C],
        true.

new_umemvar_nf(A,B,C,D,E,F,G) :-
        includev(A,B,C),
        D=[A|H],
        H=[A|E],
        tag(var,I),
        F=[move(I^r(h),A)|J],
        J=[adda(r(h),1,r(h))|K],
        align_calc(1,L,M),
        align_pad(M,K,G),
        true.

new_vars(A,B,C,D,E,F,G,H,I) :-
        sort(A,J),
        diffv(J,B,K),
        uninit_set_type(mem,F,L),
        uninit_set_type(reg,F,M),
        diffv(K,M,N),
        new_var_list(N,L,B,C,D,E,F,G,H,I),
        true.

new_var_list([],A,B,B,C,C,D,D,E,E).
new_var_list([A|B],C,D,E,F,G,H,I,J,K) :-
        inv(A,C),
        !,
        new_umemvar_nf(A,D,L,F,M,J,N),
        new_var_list(B,C,L,E,M,G,H,I,N,K),
        true.
new_var_list([A|B],C,D,E,F,G,H,I,J,K) :-
        \+inv(A,C),
        !,
        new_var(A,D,L,F,M,H,N,J,O),
        new_var_list(B,C,L,E,M,G,N,I,O,K),
        true.

b_writemode(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :-
        structure(A),
        !,
        tag(atom,P),
        functor(A,Q,R),
        K=[pragma(push(structure(R)))|S],
        S=[push(P^ (Q/R),r(h),1)|T],
        A=..[Q|U],
        V is R+1,
        align_calc(V,W,X),
        Y is N+1,
        Z is N+W,
        fill_slots(U,A1,C,D,E,B1,G,C1,I,D1,T,E1,M),
        align_pad(X,E1,F1),
        b_writeargs(U,Y,A1,B,C,D,B1,F,C1,H,D1,J,F1,L,M,Z,O),
        true.
b_writemode(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) :-
        cons(A),
        !,
        A=[P|Q],
        R=[P,Q],
        align_calc(2,S,T),
        U is N,
        V is N+S,
        K=[pragma(push(cons))|W],
        fill_slots(R,X,C,D,E,Y,G,Z,I,A1,W,B1,M),
        align_pad(T,B1,C1),
        b_writeargs(R,U,X,B,C,D,Y,F,Z,H,A1,J,C1,L,M,V,O),
        true.
b_writemode(A,[],B,C,D,D,E,E,F,F,G,G,H,I,I) :-
        atomic(A),
        !,
        true.
b_writemode(A,[],B,C,D,D,E,E,F,F,G,G,H,I,I) :-
        var(A),
        !,
        true.

fill_slots([],[],A,B,C,C,D,D,E,E,F,F,G) :- !,
        true.
fill_slots([A|B],[C|D],E,F,G,H,I,J,K,L,M,N,O) :- !,
        make_word(A,C,P),
        initialize_var(A,P,E,F,G,Q,I,R,K,S,M,T,O),
        fill_slots(B,D,E,F,Q,H,R,J,S,L,T,N,O),
        true.

b_writeargs([],A,[],[],B,C,D,D,E,E,F,F,G,G,H,I,I) :- !,
        true.
b_writeargs([A],B,[C],[D|E],F,G,H,I,J,K,L,M,N,O,P,Q,R) :- !,
        C is Q-B,
        N=[label(D)|S],
        b_writemode(A,E,F,G,H,I,J,K,L,M,S,O,P,Q,R),
        true.
b_writeargs([A,B|C],D,[E|F],G,H,I,J,K,L,M,N,O,P,Q,R,S,T) :- !,
        E is S-D,
        U is D+1,
        b_writemode(A,V,H,I,J,W,L,X,N,Y,P,Z,R,S,A1),
        b_writeargs([B|C],U,F,G,H,I,W,K,X,M,Y,O,Z,Q,R,A1,T),
        true.

initialize_var(A,B,C,D,E,F,G,H,I,J,K,L,M) :-
        is_uninit_mem(A,C,E,N,I,O),
        !,
        remove_uninit(mem,[A],O,P),
        combine_formula(var(A),P,J),
        G=[A|Q],
        Q=[A|H],
        includev(A,N,F),
        pragma_tag(A,var,K,R),
        R=[move(A,[A])|S],
        S=[push(B,r(h),1)|L],
        true.
initialize_var(A,B,C,D,E,F,G,H,I,J,K,L,M) :-
        var(A),
        \+inv(A,E),
        !,
        tag(var,N),
        G=[A|O],
        O=[A|P],
        K=[move(N^r(h),A)|Q],
        push_if_init(A,B,C,D,E,F,P,H,I,J,Q,L,M),
        true.
initialize_var(A,B,C,D,E,E,F,G,H,H,I,J,K) :-
        var(A),
        inv(A,E),
        !,
        F=[A|G],
        I=[push(B,r(h),1)|J],
        true.
initialize_var(A,B,C,D,E,E,F,F,G,G,H,I,J) :-
        nonvar(A),
        !,
        H=[push(B,r(h),1)|I],
        true.

is_uninit_mem(A,B,C,C,D,D) :-
        var(A),
        inv(A,B),
        \+inv(A,C),
        !,
        true.
is_uninit_mem(A,B,C,C,D,E) :-
        var(A),
        ctest(uninit(mem,A),D,E),
        !,
        true.

push_if_init(A,B,C,D,E,E,F,F,G,H,I,J,K) :-
        inv(A,D),
        !,
        I=[adda(r(h),1,r(h))|J],
        combine_formula(uninit(mem,A,[K]),G,H),
        true.
push_if_init(A,B,C,D,E,F,G,G,H,I,J,K,L) :-
        \+inv(A,D),
        !,
        J=[push(B,r(h),1)|K],
        includev(A,E,F),
        combine_formula((var(A)',' deref(A)),H,I),
        true.

unify_depth_2(A,B,C,D,D,E,E) :-
        simple(B),
        !,
        C=B,
        true.
unify_depth_2(A,B,C,D,E,F,G) :-
        depth_limit(H),
        A>=H,
        !,
        D= (I=B ',' J),
        J= (I=C ',' E),
        F=[C|G],
        comment(['Found structure ',B,' at depth ',A]),
        true.
unify_depth_2(A,B,C,D,E,F,G) :-
        functor(B,H,I),
        functor(C,H,I),
        J is A+1,
        unify_depth_2(1,I,J,B,C,D,E,F,G),
        true.

unify_depth_2(A,B,C,D,E,F,F,G,G) :-
        A>B,
        !,
        true.
unify_depth_2(A,B,C,D,E,F,G,H,I) :-
        A=<B,
        !,
        arg(A,D,J),
        arg(A,E,K),
        unify_depth_2(C,J,K,F,L,H,M),
        N is A+1,
        unify_depth_2(N,B,C,D,E,L,G,M,I),
        true.
