:- module(memory,[initialize_store/1, access/3, update/4]).

initialize_store([]).

%access(Id,M,V) :- print(access(Id,M,V)),nl,fail.

access(_,[],0) :- !.  
access(Id,[(Id,Val)|_ ],Val) :- !.
access(Id,[_|R],Val) :- access(Id,R,Val).


%update(Id,M,S,NS) :- print(update(Id,M,S,NS)),nl,fail.

update(Id,NewV,[],[(Id,NewV)]) :- !.
update(Id,NV,[(Id,_)|R],[(Id,NV)|R]) :- !.
update(Id,NewV,[P|R],[P|R1]) :- update(Id,NewV,R,R1).

