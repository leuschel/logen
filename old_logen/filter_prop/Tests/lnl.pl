'denotes_[]'(list).
'denotes_.'(list,list,list).
'denotes_.'(other,list,list).
'denotes_.'(list,other,other).
'denotes_.'(other,other,other).
denotes_a(other).
denotes_0(other).
denotes_1(other).
'denotes_$VAR'(other).


a -> other.
[] -> list.
[_|list] -> list.
[_|other] -> other.
