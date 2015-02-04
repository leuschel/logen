:- module(logenbta,[
	denotes_a/1,
	denotes_b/1,
	'denotes_$VAR'/1,
	'denotes_.'/3, 
	'denotes_[]'/1,
         denotes_symbol/1]).

denotes_symbol(a).
denotes_symbol(b).
denotes_symbol(.(_,_)).
denotes_symbol([]).


'denotes_$VAR'([dynamic,var]).
'denotes_.'(_,[dynamic],[dynamic]).
'denotes_.'(_,[dynamic,list],[dynamic,list]).
'denotes_.'([dynamic],[dynamic,list,static],[dynamic,list]).
'denotes_.'([dynamic],[dynamic,static],[dynamic]).
'denotes_.'(_,[dynamic,var],[dynamic]).
'denotes_.'([dynamic,list],[dynamic,list,static],[dynamic,list]).
'denotes_.'([dynamic,list],[dynamic,static],[dynamic]).
'denotes_.'([dynamic,list,static],[dynamic],[dynamic]).
'denotes_.'([dynamic,list,static],[dynamic,list,static],[dynamic,list,static]).
'denotes_.'([dynamic,list,static],[dynamic,static],[dynamic,static]).
'denotes_.'([dynamic,static],[dynamic,list,static],[dynamic,list,static]).
'denotes_.'([dynamic,static],[dynamic,static],[dynamic,static]).
'denotes_.'([dynamic,var],[dynamic,list,static],[dynamic,list]).
'denotes_.'([dynamic,var],[dynamic,static],[dynamic]).
'denotes_[]'([dynamic,list,static]).
denotes_a([dynamic,static]).
denotes_b([dynamic,static]).
