'denotes_[]'(list).
'denotes_.'(list,list,list).
'denotes_.'(other,list,list).
'denotes_.'(list,other,other).
'denotes_.'(other,other,other).
rev(X1,X2):-
	'denotes_[]'(X1),
	'denotes_[]'(X2).
rev(X1,X2):-
	rev(X3,X4),app(X4,X5,X2),
	'denotes_.'(X6,X3,X1),
	'denotes_[]'(X7),
	'denotes_.'(X6,X7,X5).
app(X1,X2,X2):-
	'denotes_[]'(X1).
app(X1,X2,X3):-
	app(X4,X2,X5),
	'denotes_.'(X6,X4,X1),
	'denotes_.'(X6,X5,X3).
