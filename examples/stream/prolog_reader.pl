:- module(prolog_reader,
          [load_file/1, is_user_pred/1, is_built_in/1,
           get_clause/2, clause/3],[assertions]).

get_clause(H,B) :- clause(_File,H,B).

is_built_in(P):- functor(P,F,A),imports(_,_,F,A).

is_user_pred(P) :- var(P),!,
   clause(_,P2,_),functor(P2,F,A),functor(P,F,A).
is_user_pred(P) :- 
  functor(P,F,A), functor(P2,F,A), clause(_,P2,_).

:- pred load_file(FileName) 
# "Loads source file FileName and populates clause/3 and imports/4 data predicates".

:- pred clause(BaseName,Head,Body) 
# "Clause Head :- Body is defined in the file BaseName.pl".

:- pred imports(ImportMod,ExportMod,F,A)
# "Module ImportMod imports predicate F/A from ExportMod".

:- pred dump
# "Dumps clause/3 and imports/4 on the screen.".

:- use_module(library('compiler/c_itf'),[process_file/7, clause_of/7,false/1, imports/5, defines_module/2]).
:- use_module(library(format)).

:- data clause/3, imports/4.

load_file(F):-
	retractall_fact(clause(_,_,_)),
	retractall_fact(imports(_,_,_,_)),
	process_file(F, itf, any, proc_file, false, false, do_nothing),
	dump.

do_nothing(_).

proc_file(BaseName):-
	save_clauses(BaseName),
	defines_module(BaseName,Mod),
	save_imports(Mod).

save_clauses(Base):-
	clause_of(Base,Head,Body,_VarNames,_Source,_Line0,_Line1),
	\+ number(Head),
	assertz_fact(clause(Base,Head,Body)),
	fail.
save_clauses(_Base).

save_imports(Mod):-
	imports(Mod, Mod2, F, A, _EndMod),
	assertz_fact(imports(Mod, Mod2, F, A)),
	fail.
save_imports(_Mod).

dump:-
	clause(Base,H,B),
	\+ number(H),
	format("~w :- ~w. \%\% defined in ~w~n",[H,B,Base]),
	fail.
dump:-  fail,
	display('-------------------------------------'),nl,
	imports(_Mod, Mod2, F, A),
	format("~w/~w, imported from: ~w~n",[F,A,Mod2]),
	fail.
dump.