
0 -> zero.
1 -> one.

%[] -> list.
%[zero|list] -> list.
%[one|list] -> list.

[] -> zerolist.
[zero|zerolist] -> zerolist.

[one|zerolist] -> goodlist.
[zero|goodlist] -> goodlist.

%denotes_0([zero]).
%denotes_1([one]).
%'denotes_.'([one],[goodlist,list],[list]).
%'denotes_.'([zero],[goodlist,list],[goodlist,list]).
%'denotes_.'([one],[list],[list]).
%'denotes_.'([zero],[list],[list]).
%'denotes_.'([any],_,[any]).
%'denotes_.'(_,[any],[any]).
%'denotes_.'([one],[list,zerolist],[goodlist,list]).
%'denotes_.'([zero],[list,zerolist],[list,zerolist]).
%'denotes_[]'([list,zerolist]).