
%--------------------------
a -> static.
[] -> static.
[static|static] -> static.
% f(static,static,....,static) -> static  for each f

[] -> list.
[dynamic|list] -> list.

'$VAR' -> var.

'$VAR' -> dynamic.
a -> dynamic.
[] -> dynamic.
[dynamic|dynamic] -> dynamic.
% f(dynamic,dynamic,....,dynamic) -> dynamic  for each f


