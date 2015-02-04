:- module(debug,
  [debug_println/1, debug_print/1, debug_nl/0,
   debug_print/2, debug_nl/1, /* extra argument with urgency of message 0: not urgent */
   
   turn_debugging_on/0, turn_debugging_on/1,
   turn_debugging_off/0,
   debug_mode/1,
   
   print_quoted/1]).


debug_println(X) :- debug_print(4,X),debug_nl(4).
debug_print(X) :- debug_print(4,X).
debug_nl :- debug_nl(4).

:- dynamic debug_print/2, debug_nl/1.

debug_print(Level,Msg) :- debug_level(CurL), Level >= CurL,!,
  print(Msg).
debug_print(_Level,_Msg).

debug_nl(Level) :- debug_level(CurL), Level >= CurL,!,
  nl.
debug_nl(_).

:- dynamic debug_mode/1.
debug_mode(off).

:- dynamic debug_level/1.

debug_level(4). /* only messages with priority of 5 or higher are printed */

turn_debugging_on :- debug_level(X),turn_debugging_on(X).

turn_debugging_on(Level) :- atomic(Level), number(Level),
       retract(debug_level(_)),
       assert(debug_level(Level)),
       retract(debug_mode(off)), !, assert(debug_mode(on)),
       retractall(debug_nl(_)),
       assert((debug_nl(U) :- (U < Level -> true ; nl))),
       retractall(debug_print(_,_)),
       assert((debug_print(U,X) :- (U < Level -> true ; print_quoted_with_max_depth(X,10)))),
       print('Debugging mode: On'),nl.
turn_debugging_on(_).

turn_debugging_off :-
       retract(debug_mode(on)), !, assert(debug_mode(off)),
       retractall(debug_nl(_)),assert(debug_nl(_)),
       retractall(debug_print(_,_)), assert(debug_print(_,_)),
       print('Debugging mode: Off'),nl.
turn_debugging_off.


print_quoted_with_max_depth(X,Max) :- write_term(X,
                            [quoted(true),max_depth(Max),portrayed(true)]).
print_quoted(X) :- write_term(X,
                            [quoted(true),max_depth(0),
                             numbervars(true),portrayed(true)]).