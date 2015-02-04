
:- module('cogen-tools',
	  [ not/1,
	    string_concatenate/3,
	    logen_examples_directory/1,
	    logen_source_directory/1,
	    remove_directory/2,
	    copy/2,
            copy/3,
	    varlist/2,
	    flatten/2,
	    safe_conjunction/1,
	    get_predicate_info/2,
	    print_predicate_info/1,
	    file_is_newer/2,
	    add_two_arguments/5,
	    add_extra_argument/4,
	    res_name/3,
	    make_copies/3,
	    application_path/1,
	    host_platform/1
	  
	  ]).
%% Ciao packages
:- use_package(.(sicsignore)).

%:- ensure_loaded('sicstus_term.pl').

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(terms)).
:- use_module('logen_messages.pl').



%%%:- use_module('cogen.pl').
%% for varset/2
ciao((:- use_module(library(terms_vars)))).
ciao((:- use_module(library(write)))).


remove_filename(DirWFile, Dir) :-
	name(DirWFile,DirWFileS),
	remove_directory(DirWFile,File),
	name(File,FileS),
	append(DirS,[47|FileS],DirWFileS),
	name(Dir,DirS).
	


application_path(Dir1) :-
   \+(prolog_flag(system_type,development)),
   environ('SP_APP_DIR',Dir),!,  /* /usr/local/bin/sicstus on development systems */
   (host_platform(darwin) ->
       % on mac we must remove filename from dir
       remove_filename(Dir,Dir1)
   ;
       Dir1 = Dir
   ).

application_path('.').
	
host_platform(Res) :-
  prolog_flag(host_type,HostType),
  name(HostType,AsciiList),
  map_host_platform(AsciiList,Res),!.

map_host_platform(HT,darwin) :- append("powerpc-darwin",_,HT).
map_host_platform(HT,windows) :- append("x86-win",_,HT).
map_host_platform(HT,linux) :- append("x86-linux",_,HT).
map_host_platform(_,unknown).

   
make_copies([],_,[]).
make_copies([_|T],X,[X|FT]) :-
    make_copies(T,X,FT).

add_two_arguments(Suffix, Call, Arg1, Arg2, RCall) :-
	Call =.. [Pred|Args],	
	res_name(Suffix, Pred, ResPred),
	append(Args, [Arg1, Arg2], NewArgs),
	RCall =.. [ResPred|NewArgs].


add_extra_argument(T,Call,V,ResCall) :-
  Call =.. [Pred|Args],res_name(T,Pred,ResPred),
  append(Args,[V],NewArgs),ResCall =.. [ResPred|NewArgs].

res_name(T,Pred,ResPred) :-
  name(PE_Sep,T),string_concatenate(Pred,PE_Sep,ResPred).


sicstus((:- use_module(library(clpq)))).


file_is_newer(File1, File2) :-
    file_exists(File1),
	file_property(File1,mod_time(ModTime1)),
    file_exists(File2),
	file_property(File2,mod_time(ModTime2)),
	ModTime1 > ModTime2.
	
	
		      

ciao(copy(C,CC)) :- copy_term(C,CC).
ciao(copy(Call,CopyCall,_)) :- copy_term(Call, CopyCall).
sicstus(copy(C,CC,_Rem)) :- copy_term(C,CC). %clpq:copy_term(C,CC,_Rem).
sicstus(copy(C,CC)) :- copy_term(C,CC).

ciao(term_variables(T,VList)) :- varset(T,VList).
varlist(T,VList) :- term_variables(T,VList).


/* useful in cogen/gx to ensure that annotations do not propagate wrongly */
safe_conjunction([]).
safe_conjunction([Atom|RestConj]) :-
  varlist(RestConj,RestVars),
  call(Atom),
  (check_var(RestVars) -> true
    ; print_error('### WARNING: Call has instantiated surrounding variables'),
      (print_error(call(Atom)), print_error(vars(RestVars)))
  ),
  safe_conjunction(RestConj).
  
  
  
   

check_var([]).
check_var([H|T]) :- var(H),check_var(T).

print_predicate_info(Call) :-
    functor(Call,Pred,L),
    print(Pred),print('/'),
    print(L).

get_predicate_info(Call,Pred/L) :-
    functor(Call,Pred,L).




not(Goal) :- \+call(Goal).

string_concatenate(X,Y,XY) :-
   name(X,Xs),name(Y,Ys),append(Xs,Ys,XYs),name(XY,XYs).
/* use atom_concat library(terms)*/
   
   
flatten(X,Res) :- var(X),!,Res=X.
flatten((X,Y),Res) :- !,
	flatten(X,FX),
	((FX==fail) -> (Res=fail) 
	 ;
	 (flatten(Y,FY),
	  ((FX==true) -> (Res=FY); ((FY==true) -> (Res = FX) ; (Res = (FX,FY))))
	 )).
flatten((LHS;E), Res) :- nonvar(LHS), LHS=(I->T),!,
  flatten(I,FI),
  ((FI==fail) -> flatten(E,Res) ;
            ((FI==true) -> flatten(T,Res) ;
                (Res = (FI->FT;FE), flatten(T,FT), flatten(E,FE))
  )).
flatten(call(X),X) :- nonvar(X), !.
flatten(\+(X),\+(Y)) :- flatten(X,Y),!.
flatten(when(C,Call), when(C,FCall)) :- flatten(Call,FCall), !.
flatten(X,X).

ciao(environ(Key, Value)) :- getenvstr(Key, V), name(Value, V).


logen_source_directory(Dir) :- 
     (environ(logen_source_directory,Dir) -> true ; (Dir = '../logen_source/')).
logen_examples_directory(Dir) :- environ(logen_examples_directory,Dir).



remove_directory(Path,FileName) :-
   name(Path,PathAscii),
   remove_directory_ascii(PathAscii,[],FA),
   name(FileName,FA).

remove_directory_ascii([],ResSoFar,Res) :- reverse(ResSoFar,Res).
remove_directory_ascii([47|Tail],_Acc,Res) :- !,  % /  unix
  remove_directory_ascii(Tail,[],Res).
remove_directory_ascii([92|Tail],_Acc,Res) :- !,  %  \ windows
  remove_directory_ascii(Tail,[],Res).
remove_directory_ascii([A|Tail],Acc,Res) :-
  remove_directory_ascii(Tail,[A|Acc],Res).
  
  
  



