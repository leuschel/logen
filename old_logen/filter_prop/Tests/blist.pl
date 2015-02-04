
'denotes_.'(tb,listb, listb).
'denotes_.'(ta,listb, list).
'denotes_.'(other,listb, list).
'denotes_.'(list,listb, list).
'denotes_.'(listb,listb, list).

'denotes_.'(other,list, list).
'denotes_.'(list,list, list).
'denotes_.'(listb,list, list).
'denotes_.'(ta,list, list).
'denotes_.'(tb,list, list).

'denotes_.'(_,other, other).
'denotes_.'(_,ta, other).
'denotes_.'(_,tb, other).


'denotes_[]'(listb).
denotes_a(ta).
denotes_b(tb).


[] -> listb.

[tb|listb] -> listb.
[ta|listb] -> list.
[other|listb] -> list.
[list|listb] -> list.
[listb|listb] -> list.

[other|list] -> list.
[list|list] -> list.
[listb|list] -> list.
[ta|list] -> list.
[tb|list] -> list.

[_|other] -> other.
[_|ta] -> other.
[_|tb] -> other.

a -> ta.
b -> tb.