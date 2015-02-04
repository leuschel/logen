
'denotes_.'(tb,listb, listb).
'denotes_.'(_,list, list).
'denotes_.'(_,other, other).
'denotes_.'(_,ta, other).
'denotes_.'(_,tb, other).


'denotes_[]'(listb).
'denotes_[]'(list).
denotes_a(ta).
denotes_b(tb).


[] -> listb.
[tb|listb] -> listb.
[] -> list.
[_|list] -> list.
[_|other] -> other.
[_|ta] -> other.
[_|tb] -> other.

a -> ta.
b -> tb.