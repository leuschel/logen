

:- module(run_cogen,[run_cogen_on_plfile/1,run_cogen_on_plfile/3,
                     run_cogen_if_necessary/1,run_cogen_if_necessary/3]).


:- use_module(library(lists)).
:- use_module(logen_messages).


:- use_module(pp,[setprintmode/1]).
:- use_module('cogen.pl').    %% to run cogen
:- use_module(annfile).


/* -------------------------------------- */
/*             RUNNING THE COGEN          */
/* -------------------------------------- */

run_cogen_on_plfile(PLFile) :-
    run_cogen_on_plfile(PLFile,_,_).
    
run_cogen_on_plfile(PLFile,AnnFile,GxModule) :-
    get_ann_gx_name(PLFile,AnnFile,GxModule),
    print_debug_message(loading_annfile(AnnFile)),
    annfile:load_annfile(AnnFile),
    print_debug_message(writing_gx_file(GxModule)),
    open(GxModule, write, Stream),
    run_cogen_and_save(GxModule, Stream).
            
get_ann_gx_name(PLFile,AnnFile,GXFile) :- 
    name(PLFile,AF),
    \+(append(_,".pl",AF)),
    append(AF,".pl",AFPL),
    name(RealPLFile,AFPL),
    file_exists(RealPLFile),!,
    get_ann_gx_name2(RealPLFile,AnnFile,GXFile).
get_ann_gx_name(PLFile,AnnFile,GXFile) :- 
    get_ann_gx_name2(PLFile,AnnFile,GXFile).
    
get_ann_gx_name2(PLFile,AnnFile,GXFile) :- 
     name(PLFile,AF),
     append(AF,".ann",AAnnF),
     name(AnnFile,AAnnF),
     (append(ARootF,".pl",AF) -> true ; ARootF=AF),
     append(ARootF,".gx",AGF),
     name(GXFile,AGF).

run_cogen_and_save(GxModule, Stream) :-
	current_output(OldStream),
	set_output(Stream),
	run_cogen(GxModule),
	flush_output,
	set_output(OldStream),
	close(Stream).

run_cogen(Module) :-
    %use_module('cogen.pl'),
	pp:setprintmode(clpq),  %%% print rational numbers for gx files
	statistics(runtime,_),
	cogen:reset_cogen,
	cogen:cogen,!,
	cogen:flush_cogen(Module),!,
	statistics(runtime,[_,MSTime]),
	nl,write('/* GX file Generated in '), write(MSTime), write(' ms */'),
	nl.


:- use_module(library(system)).

run_cogen_if_necessary(PLFile) :- 
    run_cogen_if_necessary(PLFile,_,_).
run_cogen_if_necessary(PLFile,AnnFile,GxModule) :- 
    get_ann_gx_name(PLFile,AnnFile,GxModule),
    (file_exists(GxModule)
      -> (file_is_newer(AnnFile,GxModule) 
           -> run_cogen_on_plfile(PLFile)
           ; print_debug_message(gx_file_up_to_date(GxModule))
         )
      ;  run_cogen_on_plfile(PLFile)
    ).
    

file_is_newer(File1, File2) :-
    file_exists(File1),
	file_property(File1,mod_time(ModTime1)),
    file_exists(File2),
	file_property(File2,mod_time(ModTime2)),
	ModTime1 > ModTime2.
