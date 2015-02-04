


commaToList((A, B) , [A|Tail]) :- commaToList(B, Tail),!.
commaToList(A,[A]).