

p(X,Y) :- (if(X,Y) ->
	      then(Y)
	  ;
	      else(X)
	  ).

if([],_).

then([]).
then([X|Y]) :- then(Y).

else([]).
else([X|Y]) :- else(Y).



	