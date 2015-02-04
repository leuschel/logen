
% To specialise:
% logen cache_solve.pl "run_auto(X)" --spec_file auto_spec.pl


%:- module(autoplay_nocirc, [main/1], []).

:- use_module(library(iso_byte_char)).
:- use_module(library(lists)).
:- use_module(library(prolog_sys),[statistics/2]).

sample_file('sample.raw').

main(_) :- 
	sample_file(File),
	doit(File).

doit(File):-
        get_sound_samples(File, List), 
	statistics(runtime,[_,_]),  
     %   play_sound_samples(List).
     play_stream(List),fail.

doit(_) :-
	statistics(runtime,[_,PlayTime]),
	display(user_error,'Sample file processed in '),
	display(user_error,PlayTime),
	display(user_error,' ms.'),
	nl(user_error).

get_sound_samples(File, List):-  % Trick: return a circular list -> PP: no longer
        open(File, read, ReadStream),
        get_sample(ReadStream, B),
        read_bytes(B, ReadStream, List, []).

read_bytes(-1, _Stream, L, L) :-!.
read_bytes(Byte, Stream, [Byte|Bytes], Rest):-
        get_sample(Stream, B),
        read_bytes(B, Stream, Bytes, Rest).

play_sample(_Left, _Right).
/*
play_sample(Left, Right):-
        put_byte(Left),
        put_byte(Right).
*/
get_sample(Stream, Sample):-
        get_byte(Stream, Sample).


no_of_repetitions(10).

my_repeat(N,N).
my_repeat(Max,N) :-
	1 < Max,
	Max1 is Max - 1,
	my_repeat(Max1,N).


play_sound_samples(List):-
	no_of_repetitions(Max),
	my_repeat(Max,_),
%	display(user_error,I), nl(user_error),
	set_angle,
	angle(_,Delay),
        play_byte_delay(List, List, Delay).


:- data angle/2.

angle(0, -7).

get_angle(0, -7).
get_angle(1, -4).
get_angle(2, 0).
get_angle(3, 4).
get_angle(4, 7).
get_angle(5, 4).
get_angle(6, 0).
get_angle(7, -4).


set_angle:-
        retract_fact(angle(Iter,_Angle)), 
        Iter1 is (Iter + 1) mod 8,
        get_angle(Iter1, NewAngle),
        asserta_fact(angle(Iter1,NewAngle)).

play_byte_delay(SoundLeft, SoundRight, Delay):-	
%        set_angle(Iter, Iter1, Delay), !, 

	( 
            Delay >= 0 ->  %% To the right
            DLeft = 0, DRight = Delay
        ;
            DLeft is -Delay, DRight = 0
        ),
        advance(DLeft, SoundLeft, [BL|RL]),
        advance(DRight, SoundRight, [BR|RR]),
	play_sample(BL, BR),
        play_byte_delay(RL, RR, 0).                

advance(0, L, L).
advance(N, [_|Xs], L1):-
        N > 0, 
        N1 is N - 1,
        advance(N1, Xs, L1).


/* ------------------------------------------------------------------------ */
/* copied in spatial_stream.pl */

play_stream([]) :- !.
play_stream([_]) :- !.
play_stream(SoundStream) :-  !,
  shift_soundstream(SoundStream,NS),
  spatial(1.1,2.2, 20, NS, Left,Right),
  %display(out(Left,Right)),nl,
  play_stream(NS).
  
soundstream([0, 0.1, 0.2, 0.4, 0.8, 1, 0.7, 0.4, 0.2, 0.3]). 
shift_soundstream([_|T],T).

peek_stream([],_N,0).
peek_stream([H|T],N,R) :-
  (N=<1 -> R = H ; (N1 is N-1, peek_stream(T,N1,R))).
  
spatial(X,Y,C,SoundStream,  LeftOut,RightOut) :-
  att(X,Y,Att),
  SoundStream = [One|_],
  %angle(X,Y,C,Angle),
  %index(Angle,Index),
  compute_angle_index(X,Y,C,(Angle,Index)),
  peek_stream(SoundStream,Index,Other),
  (Angle<0
    -> (LeftOut is One/Att, RightOut is Other/Att)
    ;  (LeftOut is Other/Att, RightOut is Other/Att)
  ).
  


%table_function(index/2).
%table_function(att/3).
%table_function(angle/4).
%table_function(theta/3).
table_function(compute_angle_index/4).

compute_angle_index(X,Y,C,(Angle,Index)) :-
  angle(X,Y,C,Angle),
  index(Angle,Index).
  
 
index(Angle,R) :- R is abs(round(Angle/30.0)).
   


att(X,Y,A) :- Dist is X*X+Y*Y, (Dist<1 -> A=1 ; A = Dist).

angle(X,Y,C,R) :- theta(X,Y,Theta),
  R is ((Theta+C) mod 360) - 180.
  
theta(X,Y,R) :- 
  atan2(Y,X,A2),
  R is round(55.0 * A2).
  
atan2(X,Y,R) :- 
   (Y=0 -> (X>0 -> R is 3.14/2  ; R is -3.14/2)
        ;  R is atan(X/Y)).
        
        

