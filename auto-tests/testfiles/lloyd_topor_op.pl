/* A more user friendly interpreter for Lloyd-Topor: does add safe negation */

:- op(950,yfx,'=>').
:- op(950,yfx,'<=').
:- op(850,yfx,'or').
:- op(800,yfx,'&').
:- op(750,fy,'~').


solve1(Atom) :- solve_file_atom('lloyd_topor/app',Atom).
s1(X) :- solve1(app([a],[b],X)).
solve2(Atom) :- solve_file_atom('lloyd_topor/subset.pl',Atom).
solve3(Atom) :- solve_file_atom('lloyd_topor/db_ic.pl',Atom).

% solve2(~el(X,[b,c]) & X=a).

solve_file_atom_opts(File,Atom,Options) :-
   set_options(Options),
   solve_file_atom(File,Atom).

set_options([]).
set_options(['-C'|T]) :- !,assert(use_co_routining(yes)),set_options(T).
set_options(['-D'|T]) :- !,assert(debugging(on)),set_options(T).
set_options([argv(_)|T]) :- !,set_options(T). /* argument not specified */
set_options([O|T]) :- print('% unknown option: '), print(O), nl, set_options(T).


solve_file_atom(File,Atom) :-
   lt_load_file(File),
   is_user_pred(Atom,AtomWO),
   solve_atom(AtomWO).
solve_file_atom(_File,Atom) :-
   \+(is_user_pred(Atom,_)),
   print('### Error, Predicate undefined: '),
   functor(Atom,P,A),
   print(P/A),nl,
   fail.
   
:- use_module(library(toplevel)).

:- dynamic lt_loaded_file/1.

lt_load_file(File) :- 
    (File=argv(_) -> (print('% ### Please supply filename as argument'),nl,fail)
                  ;  true),
    absolute_file_name(File,AFile), atom_concat(AAFile,'.pl',AFile),
    assert(lt_loaded_file(AAFile)),
    print(lt_loaded_file(AAFile)),nl,
	prolog_flag(single_var_warnings, Old, off),
	%prolog_flag(redefine_warnings,OldR,off),
    %load_files(File,[compilation_mode(assert_all)]),
    print(ensure_loaded(File)),nl,
    ensure_loaded(File),!,
    print('% loaded file: '),print(File),print(' : '), print(AAFile),nl,
	set_prolog_flag(single_var_warnings, Old).
	%set_prolog_flag(redefine_warnings, OldR).
lt_load_file(File) :- print('********************'),nl,
                      print('*** Could not load: '), print(File), nl,
                      print('********************'),nl,nl,fail.

%solve(true).
%solve(X) :- X=..L,debug_print(solve(L)),fail.
solve(','(A,T)) :- solve(A), solve(T).
solve(A) :- nonvar(A), A\= ','(_,_), solve_literal(A).

solve_literal('basiccontrol:true').
solve_literal(true).
solve_literal(false) :- fail.
solve_literal('~'(L)) :- not_solve_literal(L).
solve_literal('&'(A,B)) :- solve_literal(A), solve_literal(B).
solve_literal(or(A,_)) :- solve_literal(A).
solve_literal(or(_,B)) :- solve_literal(B).
solve_literal('=>'(A,B)) :- solve_literal(or('~'(A),B)).
solve_literal('<='(A,B)) :- solve_literal(or(A,'~'(B))).
solve_literal('user:forall'(X,A)) :- not_solve_literal(exists(X,'~'(A))).
solve_literal(exists(X,A)) :- rename(X,A,CopyA),solve_literal(CopyA).
solve_literal(A) :- is_user_pred(A,UA),solve_atom(UA).
solve_literal(A) :- is_built_in(A),debug_print(calling(A)),call(A).

not_solve_literal('basiccontrol:true') :- fail.
not_solve_literal(true) :- fail.
not_solve_literal(false).
not_solve_literal('~'(L)) :- solve_literal(L).
not_solve_literal(or(A,B)) :- not_solve_literal(A), not_solve_literal(B).
not_solve_literal('&'(A,_)) :- not_solve_literal(A).
not_solve_literal('&'(_,B)) :- not_solve_literal(B).
not_solve_literal('=>'(A,B)) :- not_solve_literal(or('~'(A),B)).
not_solve_literal('<='(A,B)) :- not_solve_literal(or(A,'~'(B))).
not_solve_literal('user:forall'(X,A)) :- rename(X,A,CA),not_solve_literal(CA).
not_solve_literal(exists(X,A)) :- force_not_solve_literal(exists(X,A)).  /* the tricky part */
not_solve_literal(A) :- is_user_pred(A,UA), not_solve_atom(UA).
not_solve_literal('='(A,B)) :- use_co_routining(yes), A \= B.  /* dif may be useful */
not_solve_literal(A) :- is_built_in(A),
    (\+(use_co_routining(yes)) ; A \= '='(_,_)),
    debug_print(calling_not(A)),
    \+(call(A)).

:- dynamic use_co_routining/1.
%use_co_routining(yes).

:- use_module(library(terms)).
:- use_module(library(when)).

force_not_solve_literal(Formula) :- use_co_routining(yes),
    get_free_variables(Formula,[],[],Vars),
    debug_print(force_not_solve_literal(Formula,Vars)),
    when(ground(Vars), \+(solve_literal(Formula))).
force_not_solve_literal(Formula) :- \+(use_co_routining(yes)),
    \+(solve_literal(Formula)).
    
get_free_variables(V,Hidden,In,Out) :- var(V), 
   (not_member(V,Hidden) -> add_var(V,In,Out) ; Out=In).
get_free_variables(F,Hidden,In,Out) :-
    nonvar(F), (F=exists(X,Formula) ; F='user:forall'(X,Formula)),
    get_free_variables(Formula,[X|Hidden],In,Out).
get_free_variables(F,Hidden,In,Out) :- nonvar(F),
    F \= exists(_,_), F\= 'user:forall'(_,_),
    F =.. [_Func|Args],
    l_get_free_variables(Args,Hidden,In,Out).

l_get_free_variables([],_,R,R).
l_get_free_variables([H|T],Hidden,In,Out) :-
   get_free_variables(H,Hidden,In,In2),
   l_get_free_variables(T,Hidden,In2,Out).
   
not_member(_,[]).
not_member(X,[H|T]) :- X \== H, not_member(X,T).

add_var(V,In,Out) :-
  (not_member(V,In) -> Out=[V|In] ; Out=In).

delete_var(_,[],[]).
delete_var(X,[V|T],R) :-
  (X==V -> R=RT ; R=[V|RT]),
  delete_var(X,T,RT).

not_solve_atom(A) :- use_co_routining(yes), debug_print('~'(A)),
   head_normal_form(A,Formula),
   debug_print(hnf(Formula)),
   not_solve_literal(Formula).
%not_solve_atom(A) :- use_co_routining(yes), when(ground(A),\+(solve_atom(A))).
not_solve_atom(A) :- \+(use_co_routining(yes)), \+(solve_atom(A)).


solve_atom(A) :- debug_print(A),clause(A,B), solve(B).



is_user_pred(P,PwithoutUser) :- lt_loaded_file(File),
    current_predicate(Pred/Arity,user(File)),/* different order from SICStus !! */
    (atom_concat('user:',Pred,UPred) ; UPred=Pred),
    functor(P,UPred,Arity),
    P =.. [UPred|Args], PwithoutUser =.. [Pred|Args],
    %predicate_property(P,dynamic),
    Pred \= use_co_routining, Pred \= debugging.
is_built_in(A) :- nonvar(A), \+(is_user_pred(A,_)),
                  A \= true, A \= false, A \= 'basiccontrol:true',
                  A \= '~'(_), A \= 'or'(_,_), A\= '&'(_,_),
                  A \= '=>'(_,_), A \= '<='(_,_), 
                  A \= 'user:forall'(_,_), A\= exists(_,_).

rename(X,Expression,RenamedExpr) :- %print(call_rename(X,Expression,RenamedExpr)),nl,
  rename2(Expression,RenamedExpr,X,_).
  %print(rename_res(X,Expression,RenamedExpr)),nl.
  
rename2(V,Res,Var,RVar) :- var(V),
  (V==Var  -> (Res=RVar) ; (Res = V)).
rename2(E,ResE,Var,RVar) :- nonvar(E), E=..[Func|Args],
     l_rename(Args,RArgs,Var,RVar),
     ResE =.. [Func|RArgs].
l_rename([],[],_,_).
l_rename([H|T],[RH|RT],V,RV) :- rename2(H,RH,V,RV), l_rename(T,RT,V,RV).

/* 
| ?- rename(X,p(X,Y),R).
R = p(_A,Y) ? 
yes
*/

/* :- type literal ---> (or(type(literal),type(literal)) ; not(type(literal)) ;
                        '&'(type(literal),type(literal)) ;
                        call(semi) ; built_in(semi) ;
                        '=>'(type(literal),type(literal)) ; '<='(type(literal),type(literal)) ;
                        forall(semi,type(literal)) ; exists(semi,type(literal)) ) .
                        
                        
  :- type lit ---> ( exists(free,semi) ; semi).
:- use_module(library(lists)).

:- dynamic subset/2.
subset(X,Y) :- forall(U,el(U,Y) <= el(U,X)).
:- dynamic el/2.
el(X,Y) :- member(X,Y).
:- dynamic p/1.
p(a).
:- dynamic q/1.
q(b).
:- dynamic r/1.
r(a).
r(b).
:- dynamic pq/1.
pq(X) :- p(X) or q(X).
pq(X) :- r(X), p(X) or q(X).
pq(X) :- p(X), print(X), nl.
:- dynamic t/0.
t :- exists(U, r(U) & ~p(U)).
:- dynamic app/3.
app([],X,X).
app([H|X],Y,[H|Z]) :- app(X,Y,Z).
:- dynamic app_hnf/3.
app_hnf(X,Y,Z) :- (X=[] & Y=Z) or (exists(H,exists(TX,X=[H|TX] & exists(TZ,Z=[H|TZ] & app_hnf(TX,Y,TZ))))).


                     */
                     
                     
%:- dynamic app/3.
%app([],X,X).
%app([H|X],Y,[H|Z]) :- app(X,Y,Z).

% :- solve(app_hnf([a,b],[c],R)).
% :- solve(~app([a],[X],[Y,b])).
% solve(~app([X,Y],[],[c,d]) & X=c & Y=X)


hnf(Call,HNF) :- head_normal_form(Call,HNF).

head_normal_form(Call,HNF) :-
   findall(clause(Call,Body),clause(Call,Body), HNF_List),
     /* there is a slight issue here: making findall static means that you may get a longer
       HNF_List than at runtime, as more clauses match at specialisation time;
       this does not affect overall correctness ?; but this is subtle. One could rewrite
       the program to compute the hnf without taking the call pattern Call into account */
   convert_list_into_disjunction(HNF_List,Call,HNF).
   
convert_list_into_disjunction([],_Call,false).
convert_list_into_disjunction([H],Call,HD) :- generate_disjunct(Call,H,HD).
convert_list_into_disjunction([H,H2|T],Call,or(HD,Rest)) :-
   generate_disjunct(Call,H,HD),
   convert_list_into_disjunction([H2|T],Call,Rest).
   
generate_disjunct(Call,clause(Copy,Body),Disjunct) :-
                  generate_equality_formula(Call,Copy,EqFormula),
                  clever_and(EqFormula,Body,Res),
                  varset(Copy,CallVars),
                  get_free_variables(Body,[],CallVars,FreeVars),
                  generate_exists(FreeVars,Res,Disjunct).

generate_equality_formula(A,B,Formula) :-
  A =.. [Func|AArgs],
  B =.. [Func|BArgs],
  gen_equalities(AArgs,BArgs,Formula).
  
gen_equalities([],[],true).
gen_equalities([A|TA],[B|TB],Res) :-
   gen_equalities(TA,TB,TT),
   (A==B -> Res=TT
         ;  clever_and('='(A,B),TT,Res)
   ).

clever_and(true,X,X).
clever_and(Y,true,Y) :- Y \= true.
clever_and(X,Y,'&'(X,Y)) :- X \= true, Y \= true.


generate_exists([],F,F).
generate_exists([H|T],F,exists(H,Res)) :- 
   (var(H) -> true ; print(nonvar_in_generate_exists(H,F))),
   generate_exists(T,F,Res).

/*  -------  */

:- dynamic debugging/1.
debugging(on).

debug_print(X) :- (debugging(on) -> print_message(informational,X) ; true).

print_message(_,X) :- print('% '), print(X), nl.
/*  -------  */


pc :- print_clauses.
print_clauses :- print('/*  -------  */'),nl,
   is_user_pred(P,_),
   portray_clause(my_is_user_pred(P)),
   fail.
print_clauses :- nl,
   is_user_pred(P,_),
   clause(P,Body),
   portray_clause(my_clause(P,Body)),
   fail.
print_clauses :- print('/*  ------  */'),nl.

% solve(forall(U, p(U) => exists(V, q(V) & (V=U)))).   -> no
% solve(forall(U, p(U) => exists(V, r(V) & (V=U)))).   -> yes
% solve(~ exists(X,app([a],[],[X]))).
% solve(exists(X,app([a],[],[X]))).
% solve(exists(X,~app([a],[],[X]))).
% solve(exists(X,~app([a],[X],[a,b]))).



/* For easier PE: */
/*  -------  */
my_is_user_pred(pq(_)).
my_is_user_pred(el(_,_)).
my_is_user_pred(subset(_,_)).
my_is_user_pred(r(_)).
my_is_user_pred(q(_)).
my_is_user_pred(t).
my_is_user_pred(p(_)).
my_is_user_pred(portray_message(_,_)).

my_clause(pq(A), p(A)or q(A)).
my_clause(pq(A), (r(A),p(A)or q(A))).
my_clause(pq(A), (p(A),print(A),nl)).
my_clause(el(A,B), member(A,B)).
my_clause(subset(A,B), forall(C,el(C,B)<=el(C,A))).
my_clause(r(a), true).
my_clause(r(b), true).
my_clause(q(b), true).
my_clause(t, exists(A,r(A)& ~p(A))).
my_clause(p(a), true).
/*  ------  */