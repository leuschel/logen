:- module(logenbta, [
      'denotes_[]'/1,
      denotes_eps/1,
      'denotes_.'/3,
      'denotes_+'/3,
      'denotes_$CONST'/1,
      'denotes_$VAR'/1,
      denotes_symbol/1]).

denotes_symbol([]).
denotes_symbol(eps).
denotes_symbol('.'(_,_)).
denotes_symbol(+(_,_)).
denotes_symbol('$CONST').
denotes_symbol('$VAR').

'denotes_$VAR'([dynamic,var]).
'denotes_$CONST'([dynamic,nonvar,static]).
'denotes_+'([dynamic,nonvar],_,[dynamic,nonvar]).
'denotes_+'([dynamic,list,nonvar],_,[dynamic,nonvar]).
'denotes_+'([dynamic,var],_,[dynamic,nonvar]).
'denotes_+'(_,[dynamic,nonvar],[dynamic,nonvar]).
'denotes_+'(_,[dynamic,list,nonvar],[dynamic,nonvar]).
'denotes_+'(_,[dynamic,var],[dynamic,nonvar]).
'denotes_+'([dynamic,nonvar,static],[dynamic,nonvar,static],[dynamic,nonvar,static]).
'denotes_+'([dynamic,nonvar,static],[dynamic,list,nonvar,static],[dynamic,nonvar,static]).
'denotes_+'([dynamic,list,nonvar,static],[dynamic,nonvar,static],[dynamic,nonvar,static]).
'denotes_+'([dynamic,list,nonvar,static],[dynamic,list,nonvar,static],[dynamic,nonvar,static]).
'denotes_.'(_,[dynamic,nonvar],[dynamic,nonvar]).
'denotes_.'(_,[dynamic,var],[dynamic,nonvar]).
'denotes_.'(_,[dynamic,list,nonvar],[dynamic,list,nonvar]).
'denotes_.'([dynamic,nonvar,static],[dynamic,list,nonvar,static],[dynamic,list,nonvar,static]).
'denotes_.'([dynamic,list,nonvar,static],[dynamic,list,nonvar,static],[dynamic,list,nonvar,static]).
'denotes_.'([dynamic,nonvar,static],[dynamic,nonvar,static],[dynamic,nonvar,static]).
'denotes_.'([dynamic,list,nonvar,static],[dynamic,nonvar,static],[dynamic,nonvar,static]).
'denotes_.'([dynamic,nonvar],[dynamic,list,nonvar,static],[dynamic,list,nonvar]).
'denotes_.'([dynamic,list,nonvar],[dynamic,list,nonvar,static],[dynamic,list,nonvar]).
'denotes_.'([dynamic,var],[dynamic,list,nonvar,static],[dynamic,list,nonvar]).
'denotes_.'([dynamic,nonvar],[dynamic,nonvar,static],[dynamic,nonvar]).
'denotes_.'([dynamic,list,nonvar],[dynamic,nonvar,static],[dynamic,nonvar]).
'denotes_.'([dynamic,var],[dynamic,nonvar,static],[dynamic,nonvar]).
denotes_eps([dynamic,nonvar,static]).
'denotes_[]'([dynamic,list,nonvar,static]).
