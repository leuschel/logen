/* file: pp.pl */
/* Pretty Printer */

:- module(pp, 
	    [reset_pp/0,
	     direct_pp/1,
	     make_fresh_variables/2,
	     pp/1,	     pp_term/1,
	     flush_pp/0,
	     pp_consults/1,
	     simplify_equality/3,
	     setprintmode/1,
	     assert_code_generated/1,
	     flush_pp_external/2,
	     direct_pp_fail/1,
	     print_quoted/1
	     ]).

%% deprecated :- use_module('clpvar').

%% Module Imports
%% System
:- use_module(library(lists)).

%% Logen
%:- use_module('memoizer.pl',[memo_table/3]).
:- use_module('cogen-tools.pl').
%:- use_module('clp.pl').
:- use_module('flags.pl').
:- use_module('logen_messages.pl').



% should be either printmode(clpr) or printmode(clpq)
% will either print  1 or rat(1,1)
:- dynamic printmode/1.
setprintmode(X) :- retractall(printmode(_)), assert(printmode(X)).


:- dynamic term_to_pp/1.

term_to_pp([]).

:- dynamic fact_generated/0.
fact_generated.

%% should not run in gx mode
pppp_resnot(X,X) :- query_logen_flag(pppp(gx)), !.		    
pppp_resnot([],[]).
pppp_resnot([clause(H,B)|T], [clause(H,NB)|NT]) :-
	pppp_resnot_body(B,NB),
	pppp_resnot(T,NT).

pppp_resnot_body((B1,B2), (NB1,NB2)) :-
	pppp_resnot_body(B1,NB1),
	pppp_resnot_body(B2,NB2).

%pppp_resnot_body(newnot(Call,Var), Res) :- resnot(Call,Var,Res), !.
pppp_resnot_body(X,X).


pppp_flatten([],[]).
pppp_flatten([L1,L2],NL) :-
	append(L1,L2,NL).
pppp_flatten([L1|Tail], New) :-
	pppp_flatten(Tail, NL1),
	append(L1,NL1, New).

pppp(Flag,In, Out, Func) :-
	(query_logen_flag(pppp(Flag)) ->
	    (Call =.. [Func, In, Out],
	    Call)
	;
	    In = Out
	).

post_processing(X,NX) :-
	pppp(flatten, X,X1, pppp_flatten),
	pppp(resnot,X1,X2, pppp_resnot),
	NX = X2.

%% collects lists of lists of clauses
collect_term_to_pp(Clauses) :-
	findall(X, term_to_pp(X), Clauses).



flush_pp :-
	collect_term_to_pp(Clauses),
	post_processing(Clauses, NewC),
	direct_pp(NewC).
	%%%print_failing_predicate_declarations.  %%XXX re-introduce -- steve removed again.... 14/07/04


%flush_pp_external(_Module, []):-
%	print_failing_predicate_declarations.
flush_pp_external(Module, [C|Cs]):-
	flush_pp_external1(Module, C),
	flush_pp_external(Module, Cs).

flush_pp_external1(Module, Clause):-
	functor(Clause, _Func,1),
	arg(1, Clause, Decl),
	use_module(Module),
	collect_term_to_pp(Clause,decl(Decl), Cs,Module),
	post_processing([Cs], NewCs),
	direct_pp(NewCs).

flush_pp_external1(Module, Clause):-
	functor(Clause, _Func,2),
	arg(1, Clause, Head),
	arg(2, Clause, Body),
	use_module(Module),
	collect_term_to_pp(Clause,clause(Head,Body), Cs, Module),
	post_processing([Cs], NewCs),
	direct_pp(NewCs).

collect_term_to_pp(C,Arg,Clauses, Module) :-
	findall(Arg, Module:C, Clauses).	


%flush_pp([]).
%flush_pp([X|T]) :- direct_pp(X), flush_pp(T).


reset_pp :- retractall(term_to_pp(_X)),
            retractall(fact_generated),
	    retractall(code_generated(_)).

%flush_pp :- %retractall(code_generated(_)),
%            term_to_pp(X),direct_pp(X),
%	        fail.
%flush_pp :- print_failing_predicate_declarations.
            /* (fact_generated -> true
             ;  (nl,print('/'), print('* no facts generated !! *'),
                 print('/'),nl)). */


/* pp does not print immediately */
pp(Prog) :- assert(term_to_pp(Prog)).


:- dynamic code_generated/1.
code_generated(nothing).

assert_code_generated(Head) :-
   Head =.. [P|Args],
   make_fresh_variables(Args,NArgs),
   NH =.. [P|NArgs],
   assert(code_generated(NH)).


%%% print_failing_predicate_declarations :-
%%%     %memo_table_entry(_Id,Call,ResidualCall,_,_),
%%%     memoizer:memo_table(_Module,Call,ResidualCall),
%%%     \+(code_generated(ResidualCall)),
%%% % temp additional til numbersvar problem is resolved....
%%%     copy((Call,ResidualCall), (_Call1, ResidualCall1), _),
%%%  %   numbervars((Call,ResidualCall),0,_),
%%% %    numbervars((_Call1,ResidualCall1), 0,_),
%%% %    print(ResidualCall),
%%%     print(ResidualCall1),	       
%%%     print(' :- fail.'),nl, /* to avoid undefined predicated errors */
%%%     fail.
	      
%%% print_failing_predicate_declarations.

/* to avoid undefined predicated errors */
direct_pp_fail(RC) :-
	portray_clause(':-'(RC,fail)).

    
	
	
direct_pp(Prog) :- 
	'cogen-tools':copy(Prog, NewProg), /* copy(Prog, NewProg, _),*/
%	numbervars(NewProg,1,_),
	!,	
	pp2(NewProg),!.

pp2([]).
pp2([Clause|Rest]) :- 
	pp_clause(Clause),
	pp2(Rest).
pp2(X) :-
	print('*** type error in pp2: '),print(X),nl,fail.


:- dynamic abstract/1.
%abstract(bin_solve_atom(A,B)).


%%% Quick hack to get lambda working again :-)
%%% NOT WORTH IT :-), misses singleton vars like this, better to actually add
%%% the post-processor
pp_portray_clause(Clause,_,_) :- !,
	portray_clause(Clause).
%%% pp_portray_clause(Clause, Portray, IsClause) :-
%%% 	numbervars(Clause,0,_),
%%% 	write_term(Clause, [quoted(true), numbervars(true), indented(true), portrayed(Portray)]),
%%% 	(IsClause = true -> write('.'),nl ; true).
%%% :- multifile user:portray/1.

%%% %user:portray((T1=T2)) :-
%%% %	simplify_equality(T1,T2,EqCode),
%%% %	pp_portray_clause(EqCode, true).
          
%%% user:portray(simplified__equals(A,B)) :-
%%% 	pp_portray_clause(A=B, false,false).


pp_clause((:-Directive)) :-
        print(':- '), pp_term(Directive), print('.'), nl, !.

pp_clause(decl(Decl)) :-
	portray_clause(':-'(Decl)).

%:- use_module('denotes.pl').
%	pp_clause(clause(Head, Body)) :-	
%		copy_term(Head, NHead),
%		abstract(AH),	
%		memo_table_entry(_,AH, NHead,_,_),
%		denotes_clause1(Head,Body, DH, DB),
%		flatten(DB,DBF),
%		assert_code_generated(Head),
%		%portray_clause((DH:-DBF)).
%		pp_portray_clause((DH:-DBF),true,true).
	
	
pp_clause(clause(Head,Body)) :-
	assert_code_generated(Head),
	flatten(Body,FlatBody),
	%portray_clause((Head:-FlatBody)).
	pp_portray_clause((Head:-FlatBody),true,true).





%%% STEVE????? Why is tis called 111??? is it an error or on purpose??
%%% pp_clause_111(clause(Head,Body)) :-
%%% 	numbervars((Head,Body), 1, _),
%%% 	pp_term(Head),
%%%         assert_code_generated(Head), /* remember that we have gen code */
%%% 	flatten(Body,Flatbody),
%%% 	((Flatbody=true)
%%% 	 -> (fact_generated -> true ; assert(fact_generated))
%%% 	 ;  (print(' :- '),nl,
%%% 	     pp_body(Flatbody,0)
		
%%% 	    )
%%% 	),
%%% 	print('.'),nl,!.



pp_clause(Clause) :-
    print_error('Could not pretty print clause:'),
    print_error(Clause).

pp_body('{}'(CLPCALL),Level) :- !,
	pp_indent(Level),
%%	portray_clause({CLPCALL}).
	print('{'),nl,
	pp_indent(Level),
	pp_body(CLPCALL,Level),
	nl, pp_indent(Level),
	print('}').

pp_body(('{}'(CLP),'{}'(CLP1)), Level):- !,
	pp_body(({CLP,CLP1}), Level).
pp_body(('{}'(CLP),'{}'(CLP1),Rest), Level):- !,
	pp_body(({CLP,CLP1}, Rest), Level).



pp_body(true,Level) :- !,
	pp_indent(Level),print('true').
pp_body([],Level) :- !,
	pp_indent(Level),print('true').
pp_body([Literal|Rest],Level) :- 
	pp_body(Literal,Level),!,
	((Rest=[])
	 -> (true)
	 ;  (print(','),nl,pp_body(Rest,Level))
	).

pp_body((Literal,Rest),Level) :-
	pp_body(Literal,Level),!,
	print(','),nl,pp_body(Rest,Level).
pp_body((Test -> Then ; Else),Level) :- !,
	pp_indent(Level),print('(('),nl,
	flatten(Test,FlatTest),
	pp_body(FlatTest,s(Level)),nl,
	pp_indent(Level),print(' ) -> ('),nl,
	flatten(Then,FlatThen),
	pp_body(FlatThen,s(Level)),nl,
	pp_indent(Level),print(' ) ; ('),nl,
	flatten(Else,FlatElse),
	pp_body(FlatElse,s(Level)),nl,
	pp_indent(Level),print('))').
pp_body((Literal;Rest),Level) :- !,
        print('(('),nl,
	pp_body(Literal,Level),!,  /* mal: why no flatten ? */
	print(') ;'),nl,print('('),nl,
        pp_body(Rest,Level),
        print('))').
pp_body(findall(ToCollect,Call,Res),Level) :- !,
	pp_indent(Level),
	print('findall('),pp_term(ToCollect),print(', ('),nl,
	flatten(Call,FlatCall),
	pp_body(FlatCall,s(Level)),
	print('),'), pp_term(Res),
	print(')').
pp_body(':'(Module,Call), _Level) :-
	print('\''),print(Module), print('\''), print(':'), print_quoted(Call).


pp_body(when(_DCall,RCall),Level) :- xsb_mode_is_on, !,
        /* XSB does not support when */
        pp_body(RCall,Level).
pp_body(when(DCall,RCall),Level) :- !,
        pp_indent(Level),  print('when(('),nl,
	NewLevel = s(Level),
	pp_body(DCall,NewLevel),!,
	print(') ,'),nl,
	pp_indent(Level), print('('),nl,
        pp_body(RCall,NewLevel),
        print('))').
pp_body(\+(Literals),Level) :- !, /*print(pp_not(Literals)),*/
	pp_indent(Level),
	flatten(Literals,FlatLiterals),
        ((FlatLiterals==true)
        -> print(fail)
        ;
        ( print('\\+(('),nl,
	  pp_body(FlatLiterals,s(Level)),nl,
	  pp_indent(Level),print('))')
        )).
pp_body(not(Literals),Level) :- !, /*print(pp_not(Literals)),*/
	pp_indent(Level),
	print('\\+(('),nl,
	flatten(Literals,FlatLiterals),
	pp_body(FlatLiterals,s(Level)),nl,
	pp_indent(Level),print('))').
pp_body(tnot(Literals),Level) :- !, /*print(pp_not(Literals)),*/
	pp_indent(Level),
	print('sk_not(('),nl,  /* TO DO: use a not with no floundering check */
	flatten(Literals,FlatLiterals),
	pp_body(FlatLiterals,s(Level)),nl,
	pp_indent(Level),print('))').
pp_body((T1=T2),Level) :- !,
	pp_indent(Level), print('('),nl,
	simplify_equality(T1,T2,EqCode),
	pp_body(EqCode,Level),print(') ').
pp_body(simplified__equals(T1,T2),Level) :- !,
	pp_indent(Level),
	paren_pp_term(T1),print(' = '),paren_pp_term(T2).
pp_body(is(X,Y),Level) :- !,
	pp_indent(Level),
	paren_pp_term(X),print(' is '),paren_pp_term(Y).
pp_body(dif(T1,T2),_Level) :- xsb_mode_is_on,!, /* XSB does not support dif */
	paren_pp_term(T1),print(' \\= '),paren_pp_term(T2).
pp_body(Literal,Level) :-
	pp_indent(Level),
	pp_term(Literal).

pp_indent(0) :- print('  ').
pp_indent(s(X)) :- print('  '),pp_indent(X).


paren_pp_term(X) :- (var(X) ; atomic(X)),!,pp_term(X).
paren_pp_term(X) :- nonvar(X),X='$VAR'(_), pp_term(X).
paren_pp_term(X) :- print('('),pp_term(X),print(')').


pp_term(X) :- var(X),!,
	print(X).

pp_term(rat(X, 1)) :- printmode(clpr),!,
	print_quoted(X).
pp_term(rat(X, Y)) :- printmode(clpr),!,
	print_quoted(X), print('/'), print_quoted(Y).



pp_term(not(Term)) :- !,
	print('not('),pp_term(Term),print(')').
pp_term(\+(Term)) :- !,
	print('\\+('),pp_term(Term),print(')').
pp_term('$VAR'(X)) :- !,
	print('$VAR'(X)).

pp_term(call(C)) :-
	nonvar(C),
	!,
	pp_term(C).
pp_term(Term) :- nonvar(Term),
        Term =.. [Op,T1,T2],
	binary_infix_operator(Op),!,
	paren_pp_term(T1),print(' '),print(Op),print(' '),paren_pp_term(T2).
pp_term(C) :- C=..[_Cst],!,
        (quote_functor(C)
	-> (print(''''),print(C),print(''''))
	;  (print_quoted(C))
	).
pp_term([H|T]) :- !,
	print('['),
	l_pp_term([H|T]),
	print(']').
pp_term(T) :- not(T=[_|_]),T=..[F|Args],!,
	(quote_functor(F)
	 -> (print(''''),print(F),print(''''))
	 ;  (print_quoted(F))
	),
	print('('),
	l_pp_term(Args),
	print(')').
pp_term(X) :- print_quoted(X),!.  /* <---- check ! */
pp_term(T) :- print(err(T)).

l_pp_term([A|R]) :-
	pp_term(A),
	((var(R) ; (R = '$VAR'(_X))) -> (print('|'),print_quoted(R))
		;  ((R=[]) -> true ; (print(','),l_pp_term(R)))
	).
	
	
print_quoted_with_max_depth(X,Max) :- write_term(X,
                            [quoted(true),max_depth(Max),portrayed(true)]).
print_quoted(X) :- write_term(X,
                            [quoted(true),max_depth(0),
                             numbervars(true),portrayed(true)]). %,ignore_ops(true)]).
                             
                             

quote_functor(X) :- number(X),!,fail.
quote_functor('.') :- !.
quote_functor(',') :- !.
quote_functor(';') :- !.
quote_functor('+') :- !.
quote_functor('$VAR') :- !.
quote_functor('=') :- !.
quote_functor('<') :- !.
quote_functor('=<') :- !.
quote_functor('>') :- !.
quote_functor('>=') :- !.
quote_functor('=..') :- !.
quote_functor('?') :- !.
quote_functor([]) :- !,fail. /* don't quote nil */
quote_functor('\\==') :- !,fail. /* don't quote: otherwise backslash gets lost */
quote_functor('\\=') :- !,fail. /* don't quote: otherwise backslash gets lost */
quote_functor(F) :-
	atomic(F),
	name(F,Name),
	Name = [FirstChar|_],
	((FirstChar>64, FirstChar<91) /* uppercase */
	 ; (FirstChar = 95)), /* underscore */
	!.
quote_functor(F) :-
	atomic(F),
	name(F,Name),
	member(AsciiChar,Name),
	strange_ascii_for_name(AsciiChar),!.

strange_ascii_for_name(X) :-
	X<48,!.
strange_ascii_for_name(X) :-
	X>122,!.
strange_ascii_for_name(X) :-
	X>57,X<65,!.
strange_ascii_for_name(X) :-
	X>90,X<97,not(X=95),!.
	

name_contains_space(N) :-
 name(N,CharList),
 member(32,CharList).

binary_infix_operator('=').
binary_infix_operator('+').
binary_infix_operator('-').
binary_infix_operator('*').
binary_infix_operator('/').
binary_infix_operator('//').
binary_infix_operator('==').
binary_infix_operator('\\==').
binary_infix_operator('\\=').
binary_infix_operator('mod').
binary_infix_operator('is').
binary_infix_operator('<').
binary_infix_operator('=<').
binary_infix_operator('>').
binary_infix_operator('>=').

/* pp_consults(FileList)  prints a list of consult statements */
pp_consults([]).
pp_consults([module(File)|Rest]) :- !,
 	print(':'),print('- use_module('),print(''''),
	print(File),print(''''),print(').'),nl,
	pp_consults(Rest).
pp_consults([module(File,PList)|Rest]) :- !,
 	print(':'),print('- use_module('),print(''''),
	print(File),print(''''),
	print(', '),print(PList),
	print(').'),nl,
	pp_consults(Rest).
pp_consults([File|Rest]) :-
 	print(':'),print('- consult('),print(''''),
	print(File),print(''''),print(').'),nl,
	pp_consults(Rest).



simplify_equality(X1,X2,EqCode) :- % print_message(seq(X1,X2)),
   (simplify_equality2(X1,X2,EqCode) -> true ; (EqCode = fail)).
  % print_message(simpl(EqCode)).

%simplify_equality2(X1,X2, simplified__equals(X1,X2)) :-  pp no longer used ?!
simplify_equality2(X1,X2, X1=X2) :-
	(var(X1) ; X1 = '$VAR'(_) ; var(X2); X2 = '$VAR'(_)),!.
simplify_equality2(X1,X2,EqCode) :-
	X1 =.. [Pred|Args1],
	X2 =.. [Pred|Args2],
	l_simplify_equality(Args1,Args2,EqCode).

l_simplify_equality([],[],true).
l_simplify_equality([H1|T1],[H2|T2],EqCode) :-
	simplify_equality2(H1,H2,EqCodeH),
	l_simplify_equality(T1,T2,EqCodeT),
	((EqCodeH = true)
	  -> (EqCode = EqCodeT)
	  ;  ((EqCodeT = true) -> (EqCode = EqCodeH)
		; (EqCode = (EqCodeH, EqCodeT)))).


/* moved from cogen-ml*/
make_fresh_variables([],[]).
make_fresh_variables([_|T],[_|FT]) :-
    make_fresh_variables(T,FT).



