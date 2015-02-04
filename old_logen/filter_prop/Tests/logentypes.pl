'denotes_.'(static,static,static).
'denotes_.'(nonvar,_,nonvar).
'denotes_.'(_,nonvar,nonvar).
'denotes_.'(var,_,nonvar).
'denotes_.'(_,var,nonvar).
'denotes_.'(_,list,list).

denotes_v(var).
denotes_a(static).

'denotes_[]'(static).
'denotes_[]'(list).





