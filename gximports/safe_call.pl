:- module(safe_call,[safe_call/1, white_list/1, is_known_builtin/1,
                     get_callable_builtin_conditions/2,
                     get_backpropagation_sensitive_conditions/2]).


/* module to allow calls to be made in a sandbox mode:
  white_list/1: a database of predicates that are safe to call
                (e.g. when logen/gx files are run in a web server)
  safe_call/1: uses white_list/1 to determine whether a call is
               safe or not: if it is the call is made otherwise the program
               halted with an error
*/

safe_call(X) :-
   (white_list(X)
     -> call(X)
     ; (format(user_error,
         "~n*** UNSAFE CALL not on whitelist: ~w ***~n*** Not executing call. ***~n*** Download standalone version of logen and specialize on your machine. ***~n~n",[X]),
        %throw('Call not on whitelist of safe calls')
        halt(1)
        )
   ).
   
 
 /* ideally we should add a flag to know which Prolog we are running */
white_list(X) :- var(X),!,fail.
white_list(call(X)) :- !,white_list(X).
white_list(once(X)) :- !,white_list(X).
white_list(if(X,Y,Z)) :- !,white_list(X),white_list(Y),white_list(Z).
white_list(\+(X)) :- !,white_list(X).
white_list('!').
white_list(true).
white_list(otherwise).
white_list(fail).
white_list(false).
white_list(repeat).
white_list(when(_,X)) :- !,white_list(X).
white_list(=(_,_)).
white_list('=..'(_,_)).
white_list('=='(_,_)).
white_list(\==(_,_)).
white_list(\=(_,_)).
white_list('=:='(_X,_Y)).
white_list('=\\='(_X,_Y)).
white_list('=<'(_,_)).
white_list('>='(_,_)).
white_list('<'(_,_)).
white_list('>'(_,_)).
white_list('@<'(_,_)).
white_list('@>'(_,_)).
white_list('@=<'(_,_)).
white_list('@>='(_,_)).
white_list('?='(_,_)).
white_list(is(_,_)).
white_list(ground(_)).
white_list(nonvar(_)).
white_list(atom(_)).
white_list(float(_)).
white_list(integer(_)).
white_list(number(_)).
white_list(num(_)).
white_list(real(_)).
white_list(atomic(_)).
white_list(simple(_)).
white_list(compound(_)).
white_list(callable(_)).
white_list(is_mutable(_)).
white_list(var(_)).
white_list(functor(_,_,_)).
white_list(arg(_,_,_)).
white_list(name(_,_)).
white_list(atom_codes(_,_)).
white_list(number_codes(_,_)).
white_list(atom_chars(_,_)).
white_list(number_chars(_,_)).
white_list(atom_length(_,_)).
white_list(atom_concat(_,_,_)).
white_list(sub_atom(_,_,_,_)).
white_list(copy(_X,_Y)). 
white_list(term_variables(_,_)).
white_list(copy_term(_,_)).
white_list(print(_)).
white_list(write(_)).
white_list(nl).
white_list(portray_clause(_)).
white_list(format(_,_)).
white_list(append(_,_,_)).
white_list(member(_,_)).
white_list(length(_,_)).
white_list(sort(_,_)).
white_list(clause(_,_)).
white_list(prolog_flag(_,_)).
white_list('C'(_,_,_)).
white_list('{}'(_)).
white_list(call(X)) :- white_list(X).

black_list(X) :- var(X),!.
black_list(write(_,_)).
black_list(write_term(_,_)).
black_list(format(_,_,_)).
black_list(assert(_X)).
black_list(asserta(_X)).
black_list(assertz(_X)).
black_list(retract(_X)).
black_list(load_files(_,_)).
black_list(tell(_)).
black_list(told).
black_list(see(_)).
black_list(seen).

black_list(predicate_property(_,_)). /* not sure about that one; probably safe */
black_list(current_predicate(_,_)). /* not sure about that one; probably safe */

black_list(call(X)) :- black_list(X).

   /* fail or halt(1) or menu */
   
is_list_skeleton(X) :- var(X),!,fail.
is_list_skeleton([]).
is_list_skeleton([_|T]) :- is_list_skeleton(T).

arithmetic_expression(_X) :- ground(_X). /* TO DO : implement */

is_known_builtin(X) :- white_list(X).
is_known_builtin(X) :- black_list(X).

/*
   :- dynamic is_callable_builtin/1.
uncovered_builtin(X) :- clause(is_callable_builtin(X),_), \+ is_known_builtin(X).
*/
/* to find builtins not in white_list or black_list:
  safe_call:uncovered_builtin(X).
*/

   %% --- database of builtin literals
   
is_callable_builtin(X) :- get_callable_builtin_conditions(X,Cond),call(Cond).

get_callable_builtin_conditions(X,_) :- var(X),!,fail.
get_callable_builtin_conditions(call(X),Cond) :-
                        get_callable_builtin_conditions(X,Cond).
get_callable_builtin_conditions('!',true).
get_callable_builtin_conditions('='(_X,_Y),true).
get_callable_builtin_conditions('{}'(_),true).
get_callable_builtin_conditions('C'(_X,_Y,_Z),true).
get_callable_builtin_conditions(true,true).
get_callable_builtin_conditions(fail,true).
get_callable_builtin_conditions('=..'(_X,_Y),
                                (nonvar(_X) ; is_list_skeleton(_Y))).
get_callable_builtin_conditions(functor(_X,_Y,_Z),
                                (nonvar(_X) ; (ground(_Y),ground(_Z)))).
get_callable_builtin_conditions(arg(Nr,_Y,_Z),(number(Nr),nonvar(_Y))).
get_callable_builtin_conditions(name(_X,_Y),true).
get_callable_builtin_conditions('is'(_X,_Y),
                                arithmetic_expression(_Y)).
get_callable_builtin_conditions('<'(_X,_Y),
                        (arithmetic_expression(_X),arithmetic_expression(_Y))).
get_callable_builtin_conditions('=<'(_X,_Y),
                        (arithmetic_expression(_X),arithmetic_expression(_Y))).
get_callable_builtin_conditions('>'(_X,_Y),
                        (arithmetic_expression(_X),arithmetic_expression(_Y))).
get_callable_builtin_conditions('>='(_X,_Y),
                        (arithmetic_expression(_X),arithmetic_expression(_Y))).
get_callable_builtin_conditions(nonvar(_X),true).   /* declarative if delays until nonvar */
get_callable_builtin_conditions(ground(_X),true).   /* declarative if delays until nonvar */
get_callable_builtin_conditions(number(_X),true).   /* declarative if delays until nonvar */
get_callable_builtin_conditions(real(_X),true).   /* declarative if delays until nonvar */
get_callable_builtin_conditions(integer(_X),true).   /* declarative if delays until nonvar */
get_callable_builtin_conditions(atomic(_X),true).   /* declarative if delays until nonvar */
get_callable_builtin_conditions(atom(_X),true).   /* declarative if delays until nonvar */
get_callable_builtin_conditions(num(_X),true).   /* declarative if delays until nonvar */
get_callable_builtin_conditions('\\=='(_X,_Y),true). /* declarative if delays until 
sufficiently ground */
get_callable_builtin_conditions('=='(_X,_Y),true). /* declarative if delays until sufficiently ground */
get_callable_builtin_conditions('\\='(_X,_Y),true). 
get_callable_builtin_conditions('=:='(_X,_Y),
      (arithmetic_expression(_X),arithmetic_expression(_Y))).
get_callable_builtin_conditions('=\\='(_X,_Y),
      (arithmetic_expression(_X),arithmetic_expression(_Y))).
get_callable_builtin_conditions(clause(_X,_Y),true). 
get_callable_builtin_conditions(print(_X),true).   	/* not declarative */
get_callable_builtin_conditions(write(_X),true).   	/* not declarative */
get_callable_builtin_conditions(write(_,_),true).   	/* not declarative */
get_callable_builtin_conditions(write_term(_,_),true).   	/* not declarative */
get_callable_builtin_conditions(format(Stream,Format,_),
                        (ground(Stream),ground(Format))).   	/* not declarative */
get_callable_builtin_conditions(format(Format,_),
                        ground(Format)).   	/* not declarative */
get_callable_builtin_conditions(nl,true). 		/* not declarative */
get_callable_builtin_conditions(assert(_X),true). 	/* not declarative */
get_callable_builtin_conditions(asserta(_X),true). 	/* not declarative */
get_callable_builtin_conditions(assertz(_X),true). 	/* not declarative */
get_callable_builtin_conditions(retract(_X),true). 	/* not declarative */
get_callable_builtin_conditions(var(_X),true). 		/* not declarative */
get_callable_builtin_conditions(copy(_X,_Y),true). 	/* not declarative */
get_callable_builtin_conditions(term_variables(_,_),true).
get_callable_builtin_conditions(copy_term(_,_),true).
get_callable_builtin_conditions(load_files(_,_),true).
get_callable_builtin_conditions(current_predicate(_,_),true).
get_callable_builtin_conditions(predicate_property(_,_),true).
get_callable_builtin_conditions(prolog_flag(_,_),true).
get_callable_builtin_conditions(portray_clause(_),true).

get_callable_builtin_conditions(append(X,_,Y),
                        (is_list_skeleton(X) ; is_list_skeleton(Y))).
get_callable_builtin_conditions(sort(X,_),
                        is_list_skeleton(X)).
get_callable_builtin_conditions(member(_,Y),
                        (is_list_skeleton(Y))).


/*  backpropagation_sensitive(BUILTIN, CONDITION) */
get_backpropagation_sensitive_conditions(var(X),var(X)).
get_backpropagation_sensitive_conditions(nonvar(X),var(X)).
get_backpropagation_sensitive_conditions(ground(_X),true).
get_backpropagation_sensitive_conditions(print(_X),true).
get_backpropagation_sensitive_conditions(write(_X),true).
get_backpropagation_sensitive_conditions(write(_,_),true).
get_backpropagation_sensitive_conditions(write_term(_,_),true).
