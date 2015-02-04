%% Example match program
%% Matches Pattern Pat in string T

match(Pat,T) :-
        match1(Pat,T,Pat,T).

match1([],_Ts,_P,_T).
match1([A|_Ps],[B|_Ts],P,[_X|T]) :- 
        A\==B,  
        match1(P,T,P,T).
match1([A|Ps],[A|Ts],P,T) :-
       match1(Ps,Ts,P,T).
