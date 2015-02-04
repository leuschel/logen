transpose__ans(X1,[]) :- 
	transpose_query(X1,[]),
	nullrows__ans(X1).
transpose__ans(X1,[X2|X3]) :- 
	transpose_query(X1,[X2|X3]),
	makerow__ans(X1,X2,X4),
	transpose__ans(X4,X3).
makerow__ans([],[],[]) :- 
	makerow_query([],[],[]).
makerow__ans([[X1|X2]|X3],[X1|X4],[X2|X5]) :- 
	makerow_query([[X1|X2]|X3],[X1|X4],[X2|X5]),
	makerow__ans(X3,X4,X5).
nullrows__ans([]) :- 
	nullrows_query([]).
nullrows__ans([[]|X1]) :- 
	nullrows_query([[]|X1]),
	nullrows__ans(X1).
nullrows_query(X1) :- 
	transpose_query(X1,[]),
	true.
transpose_query(X1,X2) :- 
	transpose_query(X3,[X4|X2]),
	makerow__ans(X3,X4,X1).
makerow_query(X1,X2,X3) :- 
	transpose_query(X1,[X2|X4]),
	true.
makerow_query(X1,X2,X3) :- 
	makerow_query([[X4|X5]|X1],[X4|X2],[X5|X3]),
	true.
nullrows_query(X1) :- 
	nullrows_query([[]|X1]),
	true.
transpose_query(X1,X2) :- 
	true.

