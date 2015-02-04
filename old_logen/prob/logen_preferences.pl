:- module(logen_preferences,
    [init_preferences/0, init_and_load_preferences/1,
     tcltk_get_preference/2,
     get_preference/2, get_preferences_list/1, get_preferences_list/2,
     set_preference/2,
     preference/2,
     get_ith_preference_type/2, get_ith_preference_value/2, set_ith_preference_value/2,
     get_ith_preference_type/3, get_ith_preference_value/3, set_ith_preference_value/3,
     save_preferences/1,
     backup_preferences/0, revert_preferences/0, reset_to_defaults/1,
     add_recent_document/1,  /* use to add a file to recent documents list */
     get_recent_documents/1,
     clear_recent_documents/0
    ]).
    
:- use_module(tools).
:- use_module('error_manager').   
:- use_module(library(lists)).

:- dynamic change_flag/0.
:- dynamic obsolete_preference/1.


on_exception(Catcher, Goal, Recovery) :-
    catch(Goal, Catcher, Recovery).
        
assert_change_flag :- assert(change_flag).

init_preferences :- retractall(change_flag),reset_to_defaults, check_preferences.

check_preferences :-
%   print('% '),
   preference_description(P,_), %print(P), print(' '),
   logen_preferences:preference_default_value(P,_),
   call(logen_preferences:preference(P,Val)),
   call(logen_preferences:preference_val_type(P,T)),
   call(logen_preferences:preference_category(P,_)),
   call(logen_preferences:is_of_type(Val,T,_)),
   fail.
check_preferences :- print_message('Preferences Checked').   

init_and_load_preferences(File) :- init_preferences, load_preferences(File).

load_preferences(File) :- print_message(load_preferences(File)),
   %on_exception(_Exc,see(File),(print_message('cannot open preference file')),fail),
   on_exception(see(File),(print_message('cannot open preference file')),fail),
   read_preferences, seen.


read_preferences :-  retractall(recent_documents(_)),
   read_preferences2.
read_preferences2 :-
   read(Term), Term \= end_of_file, !,
   ((Term = preference(Pref,NVal))
      -> (retract(preference(Pref,_)) -> assert(preference(Pref,NVal))
                          ;   (obsolete_preference(Pref) -> print_message(obsolete(Pref))
                                ; add_error(load_preferences,'Unknown preference in preference file: ',Pref)))
      ;  ((Term = ':-'(dynamic(_))) -> true 
              ; ((Term=recent_documents(Doc))
                  -> (assert(recent_documents(Doc)))
                  ;   ((Term = (recent_documents(_):-fail)) -> true
                          ; add_error(load_preferences,'Unrecognised:',Term)
                      )
                 )
         )
    ),
    read_preferences2.
read_preferences2 :- retractall(change_flag), recompute_nr_of_recent_documents.
   
save_preferences(File) :- change_flag,!,
   print_message(save_preferences(File)),
   tell(File), 
   (print_preferences -> true ; true),
   told,
   retractall(change_flag).
save_preferences(File) :- print_message(preferences_unchanged(File)).

print_preferences :- print_dynamic_pred(logen_preferences,preference,2),
	 print_dynamic_pred(logen_preferences,recent_documents,1).

:- dynamic change_flag_backup/0.
:- dynamic pref_backup/2.
  
backup_preferences :- retractall(pref_backup(_,_)),retractall(change_flag_backup),
                     preference(P,Val), assert(pref_backup(P,Val)),fail.
backup_preferences :- (change_flag -> assert(change_flag_backup) ; true).

revert_preferences :- \+(pref_backup(_,_)),!, print_error('No previously backed-up preferences in revert_preferences!').
revert_preferences :- retractall(preference(_,_)),pref_backup(P,Val), assert(preference(P,Val)),fail.
revert_preferences :- retractall(change_flag), (change_flag_backup -> assert_change_flag ; true).

reset_to_defaults :- reset_to_defaults(_).

reset_to_defaults(Category) :- preference_category(Pref,Category),
   preference_default_value(Pref,Val),
   retractall(preference(Pref,_)),
   assert(preference(Pref,Val)),
   fail.
reset_to_defaults(_) :- assert_change_flag.

:- dynamic preference/2.

/* --------------------------------------------------------------------------------------- */
/* --------------------------------------------------------------------------------------- */

preference_default_value(gx_debug_mode,0).
preference_default_value(prolog_mode_flag,sicstus).
preference_default_value(persistent_memo_table_for_gx,false).
preference_default_value(memo_table_printing,true).
preference_default_value(font_size,'12').
preference_default_value(last_spec_query, 'match([a,a,b],X)').
preference_default_value(examples_path, '../logen_examples/').
preference_default_value(auto_bta_dir, 'None').
preference_default_value(recent0, 'None').
preference_default_value(recent1, 'None').
preference_default_value(recent2, 'None').
preference_default_value(recent3, 'None').
preference_default_value(recent4, 'None').
preference_default_value(modular_spec_mode,false).
preference_default_value(print_interface_clauses,true).

preference_default_value(number_of_recent_documents,10).


/* ------------------------ */

preference_description(gx_debug_mode,'Debug level within gx files (0=none to 3=max)').
preference_description(prolog_mode_flag,'Target Prolog System: sicstus, ciao, xsb').
preference_description(persistent_memo_table_for_gx,'Persistent Memo Table').
preference_description(memo_table_printing,'Put memo table summary into specialised files').
preference_description(font_size,'Font Size in Main Window').
preference_description(last_spec_query,'Default Specialisation Query').
preference_description(examples_path, 'Default Examples Directory').
preference_description(auto_bta_dir, 'Filter Propagation default dir').
preference_description(recent0, 'Recent File History').
preference_description(recent1, 'Recent File History').
preference_description(recent2, 'Recent File History').
preference_description(recent3, 'Recent File History').
preference_description(recent4, 'Recent File History').
preference_description(modular_spec_mode,'Modular Specialisation On').
preference_description(print_interface_clauses,'Print Interface Clauses for specialized predicates').

preference_description(number_of_recent_documents,'Number of recently opened file names memorised').

/* ------------------------- */

preference_update_action(_).

/* ------------------------- */


preference_val_type(gx_debug_mode,int).
preference_val_type(prolog_mode_flag,[sicstus,ciao,xsb]).
preference_val_type(persistent_memo_table_for_gx,bool).
preference_val_type(memo_table_printing,bool).
preference_val_type(font_size,string).
preference_val_type(last_spec_query,term).
preference_val_type(examples_path,string).
preference_val_type(auto_bta_dir,string).
preference_val_type(recent0,string).
preference_val_type(recent1,string).
preference_val_type(recent2,string).
preference_val_type(recent3,string).
preference_val_type(recent4,string).
preference_val_type(modular_spec_mode,bool).
preference_val_type(print_interface_clauses,bool).
preference_val_type(number_of_recent_documents,int).

/* ------------------------- */


preference_category(gx_debug_mode,general).
preference_category(prolog_mode_flag,general).
preference_category(persistent_memo_table_for_gx,general).
preference_category(memo_table_printing,general).
preference_category(examples_path,general).
preference_category(auto_bta_dir,gui_prefs).
preference_category(font_size,gui_prefs).
preference_category(last_spec_query,gui_prefs).
preference_category(recent0,gui_prefs).
preference_category(recent1,gui_prefs).
preference_category(recent2,gui_prefs).
preference_category(recent3,gui_prefs).
preference_category(recent4,gui_prefs).
preference_category(modular_spec_mode,general).
preference_category(print_interface_clauses,general).


/* ------------------------------------------------------------------------------------------- */
/* --------------------------------------------------------------------------------------- */

is_of_type(true,bool,true).
is_of_type(false,bool,false).



is_of_type(X,nat,X) :- number(X), X>=0.
is_of_type(X,nat1,X) :- number(X), X>0.
is_of_type(X,nat,Z) :- \+(number(X)),atom(X),atom_codes(X,Y), number_codes(Z,Y),Z>=0.

is_of_type(X,range(Low,Up),X) :- number(X), X>=Low, X=<Up.
is_of_type(X,int,X) :- number(X).
is_of_type(X,string,X) :- atomic(X).
is_of_type(X,term,X) :- nonvar(X).

is_of_type(X,ListOfCsts,X) :- ListOfCsts=[_|_], member(X,ListOfCsts).

is_of_type(X,dot_shape,X) :- is_of_type(X,[triangle,ellipse,box,diamond,hexagon,octagon,house,
                                         invtriangle,invhouse,invtrapez,doubleoctagon]).
                                         
is_of_type(X,rgb_color) :-
   member(X,[red,green,blue,yellow,black,white,gray,brown,violet,darkred,tomato,darkblue,
             darkgray,darkviolet,darkslateblue,lightblue])
   ;
   (atomic(X),name(X,[35,H1,H2,H3,H4,H5,H6]),
     hex(H1),hex(H2),hex(H3),hex(H4),hex(H5),hex(H6)). /* should be of the form #ff00ff */

hex(X) :- atomic(X), number(X), ((X<103,X>96) ; (X>47,X<58)).

set_preference(Pref,Val1) :-
   (preference_val_type(Pref,Type)
     -> (is_of_type(Val1,Type,Val)
         -> true
         ;  (print_error('New Preference value of incorrect type: '),
             print_error(Pref), print_error(Val),
             print_error('No changes performed!'),
             fail)
         )
      ; true
   ),
   retract(preference(Pref,OldVal)),
   assert(preference(Pref,Val)),
   ((OldVal=Val) -> true ; 
                  (assert_change_flag,
                   preference_update_action(Pref))).

%:- use_module(library(charsio)).
:- use_module(sicstus_charsio).


tcltk_get_preference(Pref,Val) :- /* same as get_preference, but convert non-atomic into atoms */
   get_preference(Pref,V),
   (atomic(V) -> Val=V
              ; (numbervars(V,0,_),
                 write_term_to_chars(V,Temp,[quoted(true),numbervars(true)]), name(Val,Temp))
   ).
   
get_preference(Pref,Val) :-
   (preference(Pref,V)
      -> (Val=V)
      ; (preference_default_value(Pref,V)
         -> (Val=V)
         ;  (print_error('Invalid Preference in get_preference:'),
             print_error(Pref),
             Val = 0)
         )
   ).
   
   
cur_pref_string(Pref,String) :-
   preference_description(Pref,Desc),
   string_concatenate(Desc,';',String).
 /*  preference(Pref,Val),
   string_concatenate(Val,';',V1),
   string_concatenate(' = ',V1,V2),
   string_concatenate(Desc,V2,String). */
   
   /* useful predicates for Tcl/Tk listbox interaction: */
get_preferences_list(List) :-
   findall(S,cur_pref_string(_,S),List).
get_preferences_list(Category,List) :-
   findall(S,(cur_pref_string(_Pref,S),preference_category(_Pref,Category)),List).
   
   
get_ith_preference_type(Pos,Type) :- get_ith_preference_type(_,Pos,Type).
get_ith_preference_type(Category,Pos,Type) :-
    findall(S, (preference_description(Pref,_Desc),preference_category(Pref,Category),preference_val_type(Pref,S)), List),
    (member_nr(FType,List,Pos) -> Type=FType
                              ; (add_error(get_ith_preference_type,'Could not find preference: ',member_nr(FType,List,Pos)),fail)).
    
get_ith_preference_value(Pos,Val) :- get_ith_preference_value(_,Pos,Val).
get_ith_preference_value(Category,Pos,Val) :-
    findall(S, (preference_description(Pref,_Desc),preference_category(Pref,Category),tcltk_get_preference(Pref,S)), List),
    (member_nr(FVal,List,Pos) -> Val=FVal
                             ; (add_error(get_ith_preference_value,'Could not find preference: ',member_nr(FVal,List,Pos)),fail)).
    
set_ith_preference_value(Pos,Val) :- set_ith_preference_value(_,Pos,Val).
set_ith_preference_value(Category,Pos,Val) :-
    findall(S, (preference_description(S,_Desc),preference_category(S,Category)), List),
    (member_nr(Pref,List,Pos) -> set_preference(Pref,Val)
                             ;  (add_error(set_ith_preference_value,'Could not find preference: ',member_nr(Pref,List,Pos)),fail)).


/* ------------------------- */

:- dynamic nr_of_recent_documents/1.
:- dynamic recent_documents/1.
nr_of_recent_documents(0).

recompute_nr_of_recent_documents :-
  retractall(nr_of_recent_documents(_)),
  findall(X,recent_documents(X),List),
  length(List,Nr),
  assert(nr_of_recent_documents(Nr)).
  
/* use to add a file to recent documents list */
add_recent_document(Document) :-
   (retract(nr_of_recent_documents(Nr)) -> true ; Nr=0),
   (retract(recent_documents(Document))
     -> N1 =Nr ; N1 is Nr+1
   ),
   (preference(number_of_recent_documents,Max) -> true ; Max=10),
   (N1>Max -> (retract(recent_documents(_)),assert(nr_of_recent_documents(Nr)))
           ;   assert(nr_of_recent_documents(N1))),
   assertz(recent_documents(Document)),
   assert_change_flag.
   
get_recent_documents(TclRes) :-
   findall(X,recent_documents(X),List),
   reverse(List,Res),
   convert_list_into_string(Res,TclRes).
   

convert_list_into_string([],'').
convert_list_into_string([H|T],Res) :-
  convert_list_into_string(T,TRes),
  %atom_concat('} ',TRes,I1),
  atom_concat(H,TRes,I2),
  atom_concat('<LISTSEP>',I2,Res).
   
clear_recent_documents :-
  retractall(recent_documents(_)),
  retractall(nr_of_recent_documents(_)),
  assert(nr_of_recent_documents(0)).
