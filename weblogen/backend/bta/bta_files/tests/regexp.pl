
match(Regexp,String) :- regexp(Regexp,String,[]).

regexp(eps,T,T).
regexp(X,[X|T],T) :- atomic(X).
regexp(+(A,_B),Str,DStr) :- regexp(A,Str,DStr).
regexp(+(_A,B),Str,DStr) :- regexp(B,Str,DStr).
regexp(.(A,B),Str,DStr) :- regexp(A,Str,I), regexp(B,I,DStr).

regexp(*(A),S,DS) :- regexp(.(A,*(A)),S,DS).

regexp(*(A),S,S).













