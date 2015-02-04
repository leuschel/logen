



multiply(X, Y, Z) :- call({Y=0.0, Z=0.0}).
multiply(X,Y,Z) :- call({Y1 = Y-1, Z = X + Z1}), multiply(X,Y1,Z1).
