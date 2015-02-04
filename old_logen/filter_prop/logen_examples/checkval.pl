

p(X,Y) :- {X>2}, checkVal(X,Y).

checkVal(X, aboveOne) :- {X>3}.
checkVal(X, above) :- {X>0}.
checkVal(X, zero) :- {X=0}.
checkVal(X,below) :- {X <0}.