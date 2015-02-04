
test(X,L) :- inboth(X,[a,b,c,d,e,f,g,h],L).

inboth(X,L1,L2) :- member_u(X,L1), mem(X,L2).

mem(X,[X|_]).
mem(X,[_|T]) :- mem(X,T).
    
member_u(X,[X|_]).
member_u(X,[_|T]) :- member_u(X,T).
    

/*
Mixtus 0.3.6
| ?- pconsult('member_glob_explosion.pl').
{consulting for mixtus: /Users/mal/cvs_root/cogen2/examples/selftune/member_glob_explosion.pl}
yes
| ?- pe(test(L)).
test(A) :-
        test1(A).

% test1(A):-test(A)
test1(A) :-
        membera1(A).
test1(A) :-
        memberb1(A).
test1(A) :-
        memberc1(A).
test1(A) :-
        memberd1(A).
test1(A) :-
        membere1(A).
test1(A) :-
        memberf1(A).
test1(A) :-
        memberg1(A).
test1(A) :-
        memberh1(A).

% membera1(A):-member(a,A)
membera1([a|_]).
membera1([_|A]) :-
        membera1(A).

% memberb1(A):-member(b,A)
memberb1([b|_]).
memberb1([_|A]) :-
        memberb1(A).

% memberc1(A):-member(c,A)
memberc1([c|_]).
memberc1([_|A]) :-
        memberc1(A).

% memberd1(A):-member(d,A)
memberd1([d|_]).
memberd1([_|A]) :-
        memberd1(A).

% membere1(A):-member(e,A)
membere1([e|_]).
membere1([_|A]) :-
        membere1(A).

% memberf1(A):-member(f,A)
memberf1([f|_]).
memberf1([_|A]) :-
        memberf1(A).

% memberg1(A):-member(g,A)
memberg1([g|_]).
memberg1([_|A]) :-
        memberg1(A).

% memberh1(A):-member(h,A)
memberh1([h|_]).
memberh1([_|A]) :-
        memberh1(A).

*/
