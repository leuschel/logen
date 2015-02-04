%% This file must be loaded before the rest
%% of logen to remove the ciao only statements

:-ensure_loaded('sicstus_term.pl').

%:- use_module('cogen-interface.pl').
%:- use_module('clprmemo.pl').



print_new_welcome :-
	%nl,nl
	%,print_logen(20),
	%print('* Usage :'                      ),nl,
	%print('*   run_all_wo_expansion(\'../logen_examples/mapnew.pl\', map(q,L1,L2))'), nl,
	true.
        


print_logen(I) :-
	nl
	,indent(I),print('-----------------------------' ),nl	
	,indent(I),print(' _                           ' ),nl
	,indent(I),print('| |    ___   __ _  ___ _ __  ' ),nl
	,indent(I),print('| |   / _ \\ / _` |/ _ \\ \'_ \\ ' ),nl
	,indent(I),print('| |__| (_) | (_| |  __/ | | |' ),nl
	,indent(I),print('|_____\\___/ \\__, |\\___|_| |_|' ),nl
	,indent(I),print('            |___/            ' ),nl
	,indent(I),print('-----------------------------' ),nl
	.

indent(0).
indent(X) :- print(' '), X1 is X -1, indent(X1).
	

	

:- initialization(print_new_welcome).


