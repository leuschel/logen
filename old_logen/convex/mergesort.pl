bin_solve_atom__2(0, [_|_], [_|_], A, [_|B], C, D, E, F) :-
        bin_solve_atom__2(0, A, B, A, B, C, D, E, F).
bin_solve_atom__2(0, [A|B], [A|C], D, E, F, G, H, I) :-
        bin_solve_atom__2(0, B, C, D, E, F, G, H, I).
bin_solve_atom__3(2, [_|_], [_|_], A, [_|B], A, B, A, B).
bin_solve_atom__3(2, [_|_], [_|_], A, [_|B], C, D, E, F) :-
        bin_solve_atom__3(2, A, B, A, B, C, D, E, F).
bin_solve_atom__3(2, [A|B], [A|C], D, E, F, G, H, I) :-
        bin_solve_atom__3(2, B, C, D, E, F, G, H, I).
bin_solve_atom__4(3, [_|_], [_|_], A, [_|B], C, D, E, F) :-
        bin_solve_atom__4(3, A, B, A, B, C, D, E, F).
bin_solve_atom__4(3, [A|B], [A|C], D, E, B, C, D, E).
bin_solve_atom__4(3, [A|B], [A|C], D, E, F, G, H, I) :-
        bin_solve_atom__4(3, B, C, D, E, F, G, H, I).



bin_solve_atom__5(0, [A|B], [A|C], D, E, F, G, H, I) :-
        bin_solve_atom__5(0, B, C, D, E, F, G, H, I).
bin_solve_atom__6(2, [A|B], [A|C], D, E, F, G, H, I) :-
        bin_solve_atom__6(2, B, C, D, E, F, G, H, I).
bin_solve_atom__7(3, [A|B], [A|C], D, E, B, C, D, E).
bin_solve_atom__7(3, [A|B], [A|C], D, E, F, G, H, I) :-
        bin_solve_atom__7(3, B, C, D, E, F, G, H, I).
