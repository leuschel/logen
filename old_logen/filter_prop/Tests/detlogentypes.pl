denotes_a([static]).
'denotes_.'(_,[list,nonvar],[list,nonvar]).
'denotes_.'(_,[var],[nonvar]).
'denotes_.'(_,[nonvar],[nonvar]).
'denotes_.'([list,static],[list,static],[list,static]).
'denotes_.'([list,nonvar],[list,static],[list,nonvar]).
'denotes_.'([list,nonvar],[static],[nonvar]).
'denotes_.'([list,static],[static],[static]).
'denotes_.'([nonvar],[list,static],[list,nonvar]).
'denotes_.'([nonvar],[static],[nonvar]).
'denotes_.'([static],[list,static],[list,static]).
'denotes_.'([static],[static],[static]).
'denotes_.'([var],[list,static],[list,nonvar]).
'denotes_.'([var],[static],[nonvar]).

'denotes_[]'([list,static]).
'denotes_$VAR'([var]).
