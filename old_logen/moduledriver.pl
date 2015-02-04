%%%%%%%%%%%%%%%%%%%%%%%%%%
% Modular specialisation driver
%  Armin
% MODULE STUFF REMOVED BREAKING TOO MUCH...
%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(moduledriver, [
                         request_pattern/2,
                         request_pattern/3,
                         run_from_pattern/2,
                         run_from_pattern/3,
                         run_from_pattern/4,
                         rebuild_everything/0,
                         rebuild_memo/0,
                         specialize/0,
                         find_module/3,
                         ensure_gx_loaded/1,
                         ensure_memo_loaded/1,
                         
                         view_spec_file/1
			 ]).
	

%% Ciao-specific libs
:- use_package(sicsignore).
ciao((:-initialization(print(user_error,'Loading moduledriver.pl\n')))).
ciao((:- use_module(library(dec10_io)))).
ciao((:- use_module(library(prolog_sys)))).
ciao((:- use_module(library(compiler),[ensure_loaded/1]))).
ciao((:- use_package(library(clpq)))).


%%% Sicstus Specific libs
%sicstus((:- initialization(print(user_error,'Loaded Sicstus Version of Logen\n')))).
sicstus((:- use_module(library(clpq)))).

%%% shared modules
:- use_module('cogen-tools.pl').   
:- use_module(library(system)).
:- use_module('flags.pl').
:- use_module(logen_messages).
:- use_module(library(lists)).
:- use_module(memoizer).
:- use_module(logen_attributes).
% :- use_module('bta.pl').      %% to create annotated files  -> not used
:- use_module('prob/logen_preferences').

:- use_module(pp,[direct_pp_fail/1,direct_pp/1]).


% ____________________________________________________________

please_check(Goal) :-
        Goal,
        !.
please_check(Goal) :-
        print_error('*** CHECK FAILED: '),
        print_error(Goal),
        fail.


% split a path into Directory, Base and Extension components.
% Dir is either empty or ends with '/'.
% Extension is either empty or starts with '.'.
split_components(Dir, Base, Ext, FullPath) :-
        name(FullPath, SFullPath),
        append(SDir, SBaseExt, SFullPath),
        \+ memberchk(47, SBaseExt),    % 47 = '/'
        \+ memberchk(92, SBaseExt),    % 92 = '\'  for windows
        !,
        (memberchk(46, SBaseExt) ->    % 46 = '.'
            append(SBase, SExt, SBaseExt),
            SExt = [46 | SExtTail],
            \+ memberchk(46, SExtTail),    % 46 = '.'
            !
        ;
            SBase = SBaseExt,
            SExt = []
        ),
        name(Dir, SDir),
        name(Base, SBase),
        name(Ext, SExt).

% reverse of split_components/4.
join_components(Dir, Base, Ext, FullPath) :-
        string_concatenate(Base, Ext, BaseExt),
        string_concatenate(Dir, BaseExt, FullPath).

% join two directories.  The second one should be relative.
% all directory names should be empty or end with '/'.
join_dirs(Dir1, Dir2, Dir) :-
        string_concatenate(Dir1, Dir2, Dir).

% find a Prolog module using 'ParentDir' as the current directory.
find_module(ParentDir, ModuleSpecification, Module) :-
        print_debug_message(find_module(ParentDir,ModuleSpecification,Module)),
        split_components(RelativeDir, Module, ModuleExt, ModuleSpecification),
        please_check((ModuleExt = '.pl' ; ModuleExt = '')),
        (getattr(Module, file, _) ->
            true
        ;
            join_dirs(ParentDir, RelativeDir, ModuleDir),
            join_components(ModuleDir, Module, '.pl.ann', File),
            (file_exists(File) ->
                setattr(Module, directory, ModuleDir),
                setattr(Module, file, File),
                string_concatenate('Module ', Module, Msg1),
                string_concatenate(Msg1, ' found in file ', Msg2),
                string_concatenate(Msg2, File, Msg3),
                print_debug_message(Msg3)
            ;
                print_error('Cannot locate module:'),
                print_error(ModuleSpecification),
                print_error(parentDir=ParentDir),
                print_error('You may have to run the BTA first!'),
                fail
            )
        ).

% import the module from the given file.
% fix this in a Prolog-specific way.
import_file(File) :-
        load_files([File], [imports([])]).



% ____________________________________________________________
%
% Convenience predicates for interactive usage.

rebuild_everything :- 
        set_logen_flag(moduledriver(rebuild)),
        retractall(getattr(_, _, _)),
        retractall(memo_table(_, _, _)).
        
        
rebuild_memo :- print_message(rebuild_memo),
        set_logen_flag(moduledriver(clear_memo)),
        retractall(getattr(_, _, _)),
        retractall(memo_table(_, _, _)).

request_pattern(File, Spec_Pred) :-
        request_external_pattern(File, Spec_Pred, _ResCall, user).

request_pattern(File, Spec_Pred, ResCall) :-
        request_external_pattern(File, Spec_Pred, ResCall, user).



request_external_pattern(ModuleFile, CallPattern, ResCall, Requestor) :-
    print_message(request_external_pattern(ModuleFile,CallPattern)),
        (Requestor = module(PreviousModule) ->
            getattr(PreviousModule, directory, PreviousDir)
        ;
            PreviousDir = ''
        ),
        find_module(PreviousDir, ModuleFile, Module),
        ensure_gx_loaded(Module),
        ensure_memo_loaded(Module),
        add_two_arguments("_request", CallPattern, Requestor, ResCall1, Goal),
        getattr(Module, gx, GxModule),
        call(GxModule:Goal),
        string_concatenate(Module, '.spec', SpecModule),
        ResCall = SpecModule:ResCall1.



run_from_pattern(File, Spec_Pred) :-
     print_message(run_from_pattern(File,Spec_Pred)),
        request_pattern(File, Spec_Pred),
        specialize.

run_from_pattern(File, Spec_Pred, ResCall) :-
     print_message(run_from_pattern(File,Spec_Pred,ResCall)),
        request_pattern(File, Spec_Pred, ResCall),
        specialize.

run_from_pattern(File, Spec_Pred, ResCall, ResFile) :-
        find_module('', File, Module),
        request_pattern(File, Spec_Pred, ResCall),
        specialize,
        getattr(Module, file(spec), ResFile).

run_cogen_from_file(File) :-
	find_module('',File,Module),
	ensure_gx_loaded(Module).



% ____________________________________________________________

% build the name of the gx, memo and spec files, and mark which
% ones need to be regenerated
check_files_up_to_date(Module) :-
       (getattr(Module, file(spec), _) ->
            true
        ;
            find_and_check_file(Module, gx),
            find_and_check_file(Module, memo),
            find_and_check_file(Module, spec)
        ).
find_and_check_file(Module, Type) :-
        %print_debug_message(find_and_check_file(Module,Type)),
        getattr(Module, file, PLFile),
        getattr(Module, directory, Directory),
        string_concatenate('.', Type, Extension),
        join_components(Directory, Module, Extension, File),
        setattr(Module, file(Type), File),
        ((file_exists(File),
          file_is_up_to_date(File, Type, [PLFile]))
         ->
            print_debug_message(marking_as_up_to_date(Module,Type,File))
        ;
            (setattr(Module, rebuild(Type), true),
             print_debug_message(marking_for_rebuild(Module,Type,File)))
        ).

% basic 'make' capabilities.
file_is_up_to_date(_OutputFile, gx, []) :- !.  /* gx files are now rebuilt using cogen */
file_is_up_to_date(_OutputFile, Type, []) :-      
        \+ query_logen_flag(moduledriver(rebuild)),
        (Type \= memo
         ;
         \+ query_logen_flag(moduledriver(clear_memo))
        ).
file_is_up_to_date(OutputFile, Type, [InputFile|MoreInputFiles]) :-
        file_is_newer(OutputFile, InputFile),
        file_is_up_to_date(OutputFile, Type,MoreInputFiles).

%remake_file(OutputFile, InputFiles, _Goal) :-
%        file_exists(OutputFile),
%        file_is_up_to_date(OutputFile, unknown,InputFiles),
%        !.
%remake_file(_, _, Goal) :-
%        Goal.


% -----------------------------------------------------



% regenerate the gx file if needed, without loading it.
check_gx_file(Module) :-
        check_files_up_to_date(Module),
        (getattr(Module, rebuild(gx), true) ->
            %getattr(Module, file, PLFile),
            print_message('------------------------'),
            print_message('GX File not up-to-date !'),
            print_message(module(Module)),
            print_message('Rebuild it using cogen !'),
            print_message('------------------------'),
            %  < ------------------------------------------ DO MORE ????
            %getattr(Module, file(gx), GxFile),
            %use_module('annfile.pl'),
            %annfile:load_annfile(PLFile),
            %string_concatenate(Module, '.gx', GxModule),
            %open(GxFile, write, Stream),
            %run_cogen_and_save(GxModule, Stream),
            delattr(Module, rebuild(gx))
        ;
            true
        ).


% -----------------------------------------------------

% look into the gx source for the declaration of dependencies,
% which should appear near the beginning so this can be done without
% importing the whole gx file.
look_for_module_dependencies(Module, Dependencies) :-
        check_gx_file(Module),
        getattr(Module, file(gx), GxFile),
	
	open(GxFile,read,Stream),
        repeat,
           read(Stream,logen_module_dependencies(Dependencies)),
           !,
	close(Stream).
        

follow_dependencies(_PreviousModule, []).
follow_dependencies(PreviousModule, [Head|Tail]) :-
        getattr(PreviousModule, directory, PreviousDir),
        find_module(PreviousDir, Head, NextModule),
        check_memo_file(NextModule),
        follow_dependencies(PreviousModule, Tail).

% ensure that the gx file associated with the given module is loaded.
ensure_gx_loaded(Module) :-
       %portray_clause(user,ensure_gx_loaded_steve(Module)),
        prolog_flag(redefine_warnings,Old,off),
        (getattr(Module, gx, _) ->
            true
        ;
            check_gx_file(Module),
            getattr(Module, file(gx), GxFile),
            print_debug_message(loading_gx(GxFile)),
            import_file(GxFile),
            print_debug_message(finished_loading(GxFile)),
            string_concatenate(Module, '.gx', GxModule),
            setattr(Module, gx, GxModule),
            import_user_types(GxModule)
        ),
        set_prolog_flag(redefine_warnings,Old),
	!.
ensure_gx_loaded(Module) :- print_message(ensure_gx_loaded_failed(Module)).

% check if the memo and spec files are out-of-date
check_memo_file(Module) :-
        (getattr(Module, memo, _) ->
            true
        ;
            check_files_up_to_date(Module),
            ((getattr(Module, rebuild(memo), true) ;
              getattr(Module, rebuild(spec), true) ;
              query_logen_flag(moduledriver(clear_memo)) ) ->
                % if any is out of date, load the old memo table,
                % keep only cross-module entries, and mark these as pending
                getattr(Module, file(memo), MemoFile),
                ((file_exists(MemoFile),
                  \+ query_logen_flag(moduledriver(rebuild)),
                  \+ query_logen_flag(moduledriver(clear_memo))
                  ) ->
                    import_memo_file(Module, MemoFile),
                    print_debug_message('Table must be re-specialized.'),
                    keep_and_mark_crossmodule(Module)
                ;
                    print_debug_message('Ignoring Memo File.')
                ),
                setattr(Module, rebuild(spec), true),
                setattr(Module, memo, loaded)
            ;
                setattr(Module, memo, checked)
            ),
            % recursively follow the dependencies of this module
            look_for_module_dependencies(Module, Deps),
            follow_dependencies(Module, Deps)
        ).

% ensure that the memo file associated with the given module is loaded.
ensure_memo_loaded(Module) :-
        print_debug_message(memo_load(Module)),
        check_memo_file(Module),
        print_debug_message(checked_memo_file(Module)),
        (getattr(Module, memo, loaded) ->
            true
        ;
            getattr(Module, file(memo), MemoFile),
            print_debug_message(loading_memo(MemoFile)),
            import_memo_file(Module, MemoFile),
            setattr(Module, memo, loaded)
        ),!.
ensure_memo_loaded(Module) :- print_message(ensure_memo_loaded_failed(Module)).

:- use_module(annfile).

import_user_types(GxModule) :-
/* import the user_type definitions from GxModule into annfile */
   print_message(importing_user_types(GxModule)),
   annfile:clear_user_type,
   (on_exception(_,GxModule:user_type(_,_),fail) -> true 
      ; (print_message('No user_type definition.'),fail)),
   GxModule:user_type(T,Def),
   annfile:add_user_type(T,Def),
   print_message(user_type(T,Def)),
   fail.
import_user_types(GxModule) :- print_message(finished_importing_user_types(GxModule)).

% ____________________________________________________________
%
% Main loop.
%

specialize :-
    print_debug_message('specializing...'),
	statistics(runtime,[Time1,_]),
        % iteratively process the pending patterns, sorted by module
        (setof((M,CRList), collect_by_module(M,CRList), L) ->
             %print(user_output,specialize_by_module(L)), nl(user_output),
             specialize_by_module(L),
             specialize
        ;
	         statistics(runtime,[Time2,_]),
	         Time is Time2-Time1,
             print_message('Specialization complete.  Time Taken (ms):'),
	         print_message(Time)
        ).
	

collect_by_module(M, CRList) :-
        % we use setof instead of findall to enumerate all pending
        % entries first, before they are actually processed;
        % setof will succeed several times, one per module, so that
        % the returned entries will be grouped by module.
        setof(pending(C,R,Id), Id^(getattr(Id,pending,true), patid(M,C,R,Id)), CRList).

specialize_by_module([]).
specialize_by_module([(Module, CRList) | Tail]) :-
        string_concatenate('Processing module ', Module, Msg),
        print_debug_message(Msg), 
        ensure_gx_loaded(Module),
        ensure_memo_loaded(Module),
        open_spec_file(Module, Token),
        specialize_in_module(CRList, Module),
        close_spec_file(Token),
        getattr(Module, file(memo), MemoFile),
        export_memo_file(Module, MemoFile),
        % note that rewriting the memo file may be needed even with
        % no change, so that the time stamp is updated.
        specialize_by_module(Tail).

specialize_in_module([], _).
specialize_in_module([pending(Call, Res, Id) | Tail], Module) :-
        print_debug_message(specializing(Call,Res)),
        /* THIS DOES THE SPECIALIZATION: */
        print_specialized_pattern(Module, Call, Res,Id),
        patid(Module, Call, Res, Id),
        delattr(Id, pending),
        specialize_in_module(Tail, Module).


view_attr(Module) :-
        getattr(Module, Var,Val),
        print_message(getattr(Module,Var,Val)),fail.
view_attr(_) :-
        query_logen_flag(moduledriver(F)),
        print_message(query_logen_flag(moduledriver(F))),fail.
view_attr(_) :- nl.
% ____________________________________________________________
%
% spec file generation.
%


        
open_spec_file(Module, Token) :-
        % open the spec file for writing, possibly clearing it before
        getattr(Module, file(spec), SpecFile),
        string_concatenate('Writing ', SpecFile, Msg1),
        print_debug_message(Msg1),
        current_output(Token),!,
        (getattr(Module, rebuild(spec), true) ->
            open(SpecFile, write, Stream),
            set_output(Stream),
            print_spec_headers(Module),
            delattr(Module, rebuild(spec))
        ;
            open(SpecFile, append, Stream),
            set_output(Stream)
        ).

close_spec_file(Token) :-
        flush_output,
        current_output(Stream),
        set_output(Token),
        close(Stream),
        print_debug_message('Closing spec_file').



view_spec_file(FullPath) :- nl,
      print_message('----------------------'),
      print_message('Specialization Result:'),
      print_debug_message(orig_file(FullPath)),
      find_module(_ParentDir, FullPath, Module),
      print_debug_message(module(Module)),
      getattr(Module, file(spec), SpecFile),
      print_debug_message(specfile(SpecFile)),
      nl,
      show_file(SpecFile),
      print_message('----------------------').
      
show_file(File) :-
      open(File, read, Stream),
      (show_stream(Stream) -> true ; true),
      close(Stream).
show_stream(Stream) :- get_char(Stream,Char),
    ((Char = end_of_file)
      -> true
      ; (put_char(Char),
         show_stream(Stream)
        )
    ).
% ____________________________________________________________
%
% Predicates that generate clauses for the spec file by calling the gx file
%

print_specialized_pattern(Module, CallPattern, ResCall,Id) :-
        add_extra_argument("_u", CallPattern, Body, Goal),
        getattr(Module, gx, GxModule),
        findall(NClause,
            (
                call_residue(GxModule:Goal, Blocked),
                inspect_residual(GxModule,Blocked, BC),
                (BC = true ->
                    NClause = clause(ResCall, Body)
                ;
                    NClause = clause(ResCall, (BC, Body))
                )
            ),
            NClauses),
    ((getattr(Id, crossmodule, true),
     logen_preferences:get_preference(print_interface_clauses,true))
      -> direct_pp([clause(CallPattern,ResCall)])
       ; true
    ),
	(NClauses == [] ->
	    %format(user_error, "No clauses for ~w~n",[ResCall]),
	    direct_pp_fail(ResCall)
	;
	    direct_pp(NClauses)
	).

print_spec_headers(Module) :-
        % print the header code stored in the .gx file
        getattr(Module, gx, GxModule),
	%direct_pp([':-'(use_module('cogen'))]),
	%direct_pp([clause(test,true)]),
	%% The module decl is printed from logen_residual_clause????
	findall(NClause,
            call(GxModule:logen_residual_clauses(NClause)),
            NClauses),
	
	direct_pp(NClauses),
	findall((:-dynamic(Dyn)),
            call(GxModule:dynamic_gx(Dyn)),
            Dynamics),
	direct_pp(Dynamics).
	
	%print(user_output,print_spec_headers(Module,NClauses)), nl(user_output),
    

    
/* copied over from cogen */
inspect_residual(_,[],true).

/* online annotation temp code --- experimental steve */
inspect_residual(GxModule,[Res|Tail], ResCode) :-
	%print_error(match(Res)),
	Res = _-(prolog:when(_,Cond,_Module:(_=online(MGX,U),_))),
	!,
	%portray_clause(user_error, identified(online(MGX,U))),
	%getattr(Module, gx, GxModule),
	call(GxModule:MGX),
	call(U),	
	%R1 = when(Cond,Call),
	%print_error(found(Cond,Call)),
	inspect_residual(GxModule,Tail,ResCode).

inspect_residual(GxModule,[Res|Tail], ResCode) :-
	%print_error(match(Res)),
	Res = _-(prolog:when(_,Cond,_Module:(R1=R2,Call))),
	!,
	R1 = when(Cond,Call),
	%print_error(found(Cond,Call)),
	inspect_residual(GxModule,Tail,ResCode).

%%% Simple CLP Handling, doesnt check clp constraints on memo entry

inspect_residual(GxModule,[Res|Tail], ResCode) :-
	Res = _pylo-(clpq:{CST}),
	!,
	ResCode = ({CST},R1),
	inspect_residual(GxModule,Tail,R1).

	
	

inspect_residual(GxModule,[Res|T], ResCode) :-
	print_error(unregonized_Residual_discarded(Res)),
	inspect_residual(GxModule,T,ResCode).
