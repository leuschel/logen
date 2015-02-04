%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mauricio
% Interface to logen, for generating an executable in Ciao
%
%%%%%%%%%%%%%%%%%%%%%%%%%%


% use_package(_).
% :- use_package(logen_annotation_package).
% :- ensure_loaded(logen_annotation).

:- use_module('cogen-interface').

main([Annfile,GXfile]):-
	run_cogen_and_save_wo_expansion(Annfile,GXfile).

