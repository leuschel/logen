
'denotes_.'(ta,lista, lista).
'denotes_.'(other,lista, list).
'denotes_.'(list,lista, list).
'denotes_.'(lista,lista, list).

'denotes_.'(other,list, list).
'denotes_.'(list,list, list).
'denotes_.'(lista,list, list).
'denotes_.'(ta,list, list).

'denotes_.'(_,other, other).
'denotes_.'(_,ta, other).


'denotes_[]'(lista).
denotes_a(ta).
denotes_b(other).
