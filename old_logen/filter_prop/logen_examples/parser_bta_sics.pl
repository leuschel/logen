/* version of parser_bta.P which can run in SICStus  and
   might be at the basis of a tabling interpreter for SICStus/Ciao.
   However, it currently does not work yet. I am not sure it
   can be made to work easily, as co-routining does not work
   across failure.
*/


:- use_module(library(terms)).
:- use_module(library(lists)).

/* tool to check whether a call is in the table + return local table for call if it is */
is_in_table(Call,Table,LocalTable,Res) :-
	(nonvar(Table) -> is_in_table2(Call,Table,LocalTable,Res)
	               ; (Table = [table(Call,LocalTable)|_]),Res=no).
is_in_table2(Call,[table(C,LT)|T],LocalTable,Res) :-
	 (variant_of(Call,C) -> (LocalTable = LT,Res=yes)
	                     ;  (is_in_table(Call,T,LocalTable,Res))).


variant_of(X,Y) :- copy_term(X,X2),variant(X2,Y).
add_argument(Call,Arg,NewCall) :- Call =.. [Pred|Args], append(Args,[Arg],NA), NewCall =.. [Pred|NA].

tabled_call(C,Table) :- print(tabled_call(C)),nl,
	is_in_table(C,Table,LocalTable,Answer),
	((Answer=yes)
		-> (print(call_in_table(C)),nl,get_table_answers(C,LocalTable))
	    ; (add_argument(C,Table,NewC),
			print(new_call(C)),nl,
		    call(NewC),
		    add_answer(C,LocalTable))).

get_table_answers(C,LocalTable) :- when(nonvar(LocalTable),get2(C,LocalTable)).
get2(C,[Answer|_]) :- copy_term(Answer,CA),CA=C, print(got_answer(C)),nl.
get2(C,[_|T]) :- get_table_answers(C,T).

add_answer(C,LT) :- (var(LT) -> (copy_term(C,CC),LT = [CC|_], print(add_new_answer(C)),nl)
					         ;  (LT = [Answer|LTT],
								 (variant_of(C,Answer) -> (print(old_answer(C)),nl)
									  ; add_answer(C,LTT)))
					).


/* a very simple test: */
test1(X) :- p(X,T), print(T),nl.
p(a,_).
p(X,T) :- tabled_call(p(X),T).


test2(X) :- q(X,T), print(T),nl.
q(X,T) :- tabled_call(q(X),T).
q(a,_).




test(A,B,C) :- tabled_call(nont(A,B,C),Table),
	 print(Table),nl.

/* :- table nont/3.*/
nont(A,B,C,Table) :- 
    iff(D),   /* D is always ground */
    t(D,B,E), 
    tabled_call(nont(A,E,C),Table), 
    true.
nont(A,B,C,_Table) :- 
    t(A,B,C), 
    true.
    
/* :- table t/3. */

t(A,B,C) :- 
    iff(B,A,C),   /* B is ground iff A and C are ground */
    true.
true.
fail_iff :- 
    fail.
iff(true).
iff(true,true).
iff(false,false).
iff(true,true,true).
iff(false,true,false).
iff(false,false,true).
iff(false,false,false).
iff(true,true,true,true).
iff(false,true,false,false).
iff(false,false,true,false).
iff(false,false,false,true).
iff(false,true,true,false).
iff(false,true,false,true).
iff(false,false,true,true).
iff(false,false,false,false).



/*


XSB Version 2.5 (Okocim) of March 11, 2002
[powerpc-apple-darwin6.4; mode: optimal; engine: slg-wam; gc: indirection; scheduling: local]

| ?- [parser_bta].
[Compiling ./parser_bta]
++Warning[XSB]: [Compiler] ./parser_bta: Redefining the standard predicate: true/0
[parser_bta compiled, cpu time used: 0.0700 seconds]
[parser_bta loaded]

yes
| ?- nont(true,X,Y).

X = true
Y = true;

X = false
Y = false;

no

-----------> after execution either both X and Y are ground or both X and Y are not ground

| ?- [tables].
[tables loaded]

yes
| ?- get_calls(t(X,Y,Z),C,R).

X = true
Y = true
Z = _h121
C = 3255528
R = ret(_h121);

X = true
Y = false
Z = _h121
C = 3255424
R = ret(_h121);

X = true
Y = _h107
Z = _h121
C = 3255320
R = ret(_h107,_h121);

no


As you can see: t always called with first argument ground


| ?- get_calls(nont(X,Y,Z),C,R).

X = true
Y = true
Z = _h121
C = 3255476
R = ret(_h121);

X = true
Y = false
Z = _h121
C = 3255372
R = ret(_h121);

X = true
Y = _h107
Z = _h121
C = 3255268
R = ret(_h107,_h121);

no

Ditto for nont

*/
