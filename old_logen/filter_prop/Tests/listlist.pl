denotes_c([any]).
'denotes_.'([any],[any],[any]).
'denotes_.'([any],[any,list],[any,list]).
'denotes_.'([any],[any,list,listlist],[any,list]).
'denotes_.'([any,list],[any],[any]).
'denotes_.'([any,list],[any,list],[any,list]).
'denotes_.'([any,list],[any,list,listlist],[any,list,listlist]).
'denotes_.'([any,list,listlist],[any],[any]).
'denotes_.'([any,list,listlist],[any,list],[any,list]).
'denotes_.'([any,list,listlist],[any,list,listlist],[any,list,listlist]).
'denotes_[]'([any,list,listlist]).

c -> [any].
[[any]|[any]] -> [any].
[[any]|[any,list]] -> [any,list].
[[any]|[any,list,listlist]] -> [any,list].
[[any,list]|[any]] -> [any].
[[any,list]|[any,list]] -> [any,list].
[[any,list]|[any,list,listlist]] -> [any,list,listlist].
[[any,list,listlist]|[any]] -> [any].
[[any,list,listlist]|[any,list]] -> [any,list].
[[any,list,listlist]|[any,list,listlist]] -> [any,list,listlist].
[] -> [any,list,listlist].
