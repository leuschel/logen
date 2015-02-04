/* Created by Pylogen */


p(A) :- ground(A), A = b.
    

t(A,B) :- 
	var(A),
	r(A,B).

r(a,_).
r(b,_).



