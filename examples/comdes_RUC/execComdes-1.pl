:- module(execComdes,_).



% clauses are either (a) box definitions, (b) machine initialisations
% (c) stream maps, or (d) actions.  
	


test(N) :-
	zeroList(N,L),trueList(N,B),
	exec(airPumpSystem(B,L,L,L,_,_,_,_)).
	
zeroList(0,[]).
zeroList(N,[0|Zs]) :-
	N > 0,
	N1 is N-1,
	zeroList(N1,Zs).
trueList(0,[]).
trueList(N,[true|Zs]) :-
	N > 0,
	N1 is N-1,
	trueList(N1,Zs).

exec(G) :-
	expandDesign((G,true),Gs),
	oneStepExec(Gs).
	
oneStepExec(Gs) :-
	Gs \== [],
	oneStepExpand(Gs,Gs1),
	write(Gs),nl,nl,
	oneStepExec(Gs1).
oneStepExec([]).
	
oneStepExpand([true|Gs],Gs1) :-	
	oneStepExpand(Gs,Gs1).
oneStepExpand([G|Gs],[G1|Gs1]) :-	
	component(G,P,G1),
	solve(P),
	%write(P),nl,
	oneStepExpand(Gs,Gs1).
oneStepExpand([],[]).

expandDesign(Bs,Gs2) :-
	conjunct(Bs,G,Gs),
	expandDesign(G,Bs1),
	expandDesign(Gs,Gs1),
	app(Bs1,Gs1,Gs2).
expandDesign(true,[]).
expandDesign(G,Bs1) :-
	box(G),
	comdesClause(G,Bs),
	expandDesign(Bs,Bs1).
expandDesign(G,[InitG]) :-
	machine(G,InitG).
expandDesign(G,[G]) :-
	basicComponent(G).
	
conjunct((G,Gs),G,Gs).
	
component(G,P,G1) :-
	basicComponent(G),
	comdesClause(G,(P,G1)). % recursive stream handler
component(G,true,true) :-
	basicComponent(G),
	comdesClause(G,true).	% base case stream handler
	
app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :-
	app(Xs,Ys,Zs).

box(X) :-
	comdesClause(box(X), true).
basicComponent(X) :-
	comdesClause(basicComponent(X), true).
	
machine(X,Y) :-
	comdesClause(machine(X,Y), true).
	
solve(true).
solve((G,Gs)) :-
	solve(G),
	solve(Gs).
solve(G) :-
	G \== true,
	builtin(G),
	%write(G),nl.
	call(G).
solve(G) :-
	comdesClause(G,B),
	solve(B).
	
builtin(G) :-
	functor(G,F,N),
	builtin_export(_,F,N,_).
	
builtin_export(arithmetic,is,2,0) .
builtin_export(arithmetic,<,2,0) .
builtin_export(arithmetic,=<,2,0) .
builtin_export(arithmetic,>,2,0) .
builtin_export(arithmetic,>=,2,0) .
builtin_export(arithmetic,=:=,2,0) .
builtin_export(arithmetic,=\=,2,0) .
builtin_export(arithmetic,arithexpression,1,0) .
builtin_export(atomic_basic,name,2,0) .
builtin_export(atomic_basic,atom_codes,2,0) .
builtin_export(atomic_basic,number_codes,2,0) .
builtin_export(atomic_basic,number_codes,3,0) .
builtin_export(atomic_basic,atom_length,2,0) .
builtin_export(atomic_basic,atom_concat,3,0) .
builtin_export(atomic_basic,sub_atom,4,0) .
builtin_export(attributes,attach_attribute,2,0) .
builtin_export(attributes,get_attribute,2,0) .
builtin_export(attributes,update_attribute,2,0) .
builtin_export(attributes,detach_attribute,1,0) .
builtin_export(basic_props,term,1,0) .
builtin_export(basic_props,int,1,0) .
builtin_export(basic_props,nnegint,1,0) .
builtin_export(basic_props,flt,1,0) .
builtin_export(basic_props,num,1,0) .
builtin_export(basic_props,atm,1,0) .
builtin_export(basic_props,struct,1,0) .
builtin_export(basic_props,gnd,1,0) .
builtin_export(basic_props,constant,1,0) .
builtin_export(basic_props,callable,1,0) .
builtin_export(basic_props,operator_specifier,1,0) .
builtin_export(basic_props,list,1,0) .
builtin_export(basic_props,list,2,list(?,pred(1))) .
builtin_export(basic_props,member,2,0) .
builtin_export(basic_props,sequence,2,sequence(?,pred(1))) .
builtin_export(basic_props,sequence_or_list,2,sequence_or_list(?,pred(1))) .
builtin_export(basic_props,character_code,1,0) .
builtin_export(basic_props,string,1,0) .
builtin_export(basic_props,predname,1,0) .
builtin_export(basic_props,atm_or_atm_list,1,0) .
builtin_export(basic_props,compat,2,compat(?,pred(1))) .
builtin_export(basic_props,iso,1,0) .
builtin_export(basic_props,not_further_inst,2,0) .
builtin_export(basic_props,regtype,1,0) .
builtin_export(basiccontrol,',',2,0) .
builtin_export(basiccontrol,;,2,0) .
builtin_export(basiccontrol,->,2,0) .
builtin_export(basiccontrol,!,0,0) .
builtin_export(basiccontrol,\+,1,0) .
builtin_export(basiccontrol,if,3,0) .
builtin_export(basiccontrol,true,0,0) .
builtin_export(basiccontrol,fail,0,0) .
builtin_export(basiccontrol,repeat,0,0) .
builtin_export(basiccontrol,call,1,call(goal)) .
builtin_export(basiccontrol,srcdbg_spy,6,srcdbg_spy(goal,?,?,?,?,?)) .
builtin_export(data_facts,asserta_fact,1,asserta_fact(fact)) .
builtin_export(data_facts,asserta_fact,2,asserta_fact(fact,?)) .
builtin_export(data_facts,assertz_fact,1,assertz_fact(fact)) .
builtin_export(data_facts,assertz_fact,2,assertz_fact(fact,?)) .
builtin_export(data_facts,current_fact,1,current_fact(fact)) .
builtin_export(data_facts,current_fact,2,current_fact(fact,?)) .
builtin_export(data_facts,retract_fact,1,retract_fact(fact)) .
builtin_export(data_facts,retractall_fact,1,retractall_fact(fact)) .
builtin_export(data_facts,current_fact_nb,1,current_fact_nb(fact)) .
builtin_export(data_facts,retract_fact_nb,1,retract_fact_nb(fact)) .
builtin_export(data_facts,close_predicate,1,close_predicate(fact)) .
builtin_export(data_facts,open_predicate,1,open_predicate(fact)) .
builtin_export(data_facts,set_fact,1,set_fact(fact)) .
builtin_export(data_facts,erase,1,0) .
builtin_export(exceptions,catch,3,catch(goal,?,goal)) .
builtin_export(exceptions,intercept,3,intercept(goal,?,goal)) .
builtin_export(exceptions,throw,1,0) .
builtin_export(exceptions,halt,0,0) .
builtin_export(exceptions,halt,1,0) .
builtin_export(exceptions,abort,0,0) .
builtin_export(io_aux,message,2,0) .
builtin_export(io_aux,message_lns,4,0) .
builtin_export(io_aux,error,1,0) .
builtin_export(io_aux,warning,1,0) .
builtin_export(io_aux,note,1,0) .
builtin_export(io_aux,message,1,0) .
builtin_export(io_aux,debug,1,0) .
builtin_export(io_aux,inform_user,1,0) .
builtin_export(io_aux,display_string,1,0) .
builtin_export(io_aux,display_list,1,0) .
builtin_export(io_aux,display_term,1,0) .
builtin_export(io_basic,get_code,2,0) .
builtin_export(io_basic,get_code,1,0) .
builtin_export(io_basic,get1_code,2,0) .
builtin_export(io_basic,get1_code,1,0) .
builtin_export(io_basic,peek_code,2,0) .
builtin_export(io_basic,peek_code,1,0) .
builtin_export(io_basic,skip_code,2,0) .
builtin_export(io_basic,skip_code,1,0) .
builtin_export(io_basic,put_code,2,0) .
builtin_export(io_basic,put_code,1,0) .
builtin_export(io_basic,nl,1,0) .
builtin_export(io_basic,nl,0,0) .
builtin_export(io_basic,tab,2,0) .
builtin_export(io_basic,tab,1,0) .
builtin_export(io_basic,code_class,2,0) .
builtin_export(io_basic,getct,2,0) .
builtin_export(io_basic,getct1,2,0) .
builtin_export(io_basic,display,2,0) .
builtin_export(io_basic,display,1,0) .
builtin_export(io_basic,displayq,2,0) .
builtin_export(io_basic,displayq,1,0) .
builtin_export(prolog_flags,set_prolog_flag,2,0) .
builtin_export(prolog_flags,current_prolog_flag,2,0) .
builtin_export(prolog_flags,prolog_flag,3,0) .
builtin_export(prolog_flags,push_prolog_flag,2,0) .
builtin_export(prolog_flags,pop_prolog_flag,1,0) .
builtin_export(prolog_flags,prompt,2,0) .
builtin_export(prolog_flags,gc,0,0) .
builtin_export(prolog_flags,nogc,0,0) .
builtin_export(prolog_flags,fileerrors,0,0) .
builtin_export(prolog_flags,nofileerrors,0,0) .
builtin_export(streams_basic,open,3,0) .
builtin_export(streams_basic,close,1,0) .
builtin_export(streams_basic,set_input,1,0) .
builtin_export(streams_basic,current_input,1,0) .
builtin_export(streams_basic,set_output,1,0) .
builtin_export(streams_basic,current_output,1,0) .
builtin_export(streams_basic,character_count,2,0) .
builtin_export(streams_basic,line_count,2,0) .
builtin_export(streams_basic,line_position,2,0) .
builtin_export(streams_basic,flush_output,1,0) .
builtin_export(streams_basic,flush_output,0,0) .
builtin_export(streams_basic,clearerr,1,0) .
builtin_export(streams_basic,current_stream,3,0) .
builtin_export(streams_basic,stream_code,2,0) .
builtin_export(streams_basic,absolute_file_name,2,0) .
builtin_export(streams_basic,absolute_file_name,7,0) .
builtin_export(streams_basic,sourcename,1,0) .
builtin_export(streams_basic,stream,1,0) .
builtin_export(streams_basic,stream_alias,1,0) .
builtin_export(streams_basic,io_mode,1,0) .
builtin_export(system_info,get_arch,1,0) .
builtin_export(system_info,get_os,1,0) .
builtin_export(system_info,this_module,1,this_module(addmodule)) .
builtin_export(system_info,current_module,1,0) .
builtin_export(system_info,ciaolibdir,1,0) .
builtin_export(term_basic,=,2,0) .
builtin_export(term_basic,arg,3,0) .
builtin_export(term_basic,functor,3,0) .
builtin_export(term_basic,=..,2,0) .
builtin_export(term_basic,copy_term,2,0) .
builtin_export(term_basic,'C',3,0) .
builtin_export(term_compare,==,2,0) .
builtin_export(term_compare,\==,2,0) .
builtin_export(term_compare,\=,2,0) .
builtin_export(term_compare,@<,2,0) .
builtin_export(term_compare,@=<,2,0) .
builtin_export(term_compare,@>,2,0) .
builtin_export(term_compare,@>=,2,0) .
builtin_export(term_compare,compare,3,0) .
builtin_export(term_typing,var,1,0) .
builtin_export(term_typing,nonvar,1,0) .
builtin_export(term_typing,atom,1,0) .
builtin_export(term_typing,integer,1,0) .
builtin_export(term_typing,float,1,0) .
builtin_export(term_typing,number,1,0) .
builtin_export(term_typing,atomic,1,0) .
builtin_export(term_typing,ground,1,0) .
builtin_export(term_typing,type,2,0) .

comdesClause(box(airPumpSystem(_,_,_,_,_,_,_,_)),true).
comdesClause(box(psu(_,_)),true).
comdesClause(box(apu(_,_)),true).
comdesClause(box(cu(_,_)),true).
comdesClause(box(cpu(_,_)),true).
comdesClause(airPumpSystem(A,B,C,D,E,F,G,H),(psu(I,C),apu((J,G,H),(K,L,D)),cpu((E,F,M,N),(A,B,J,I)),cu((K,L),(M,I,N,J)))).
comdesClause(psu(A,B),(isd11(A,C),signalCond(C,D),limitCheck(D,E),osd11(E,B))).
comdesClause(apu((A,B,C),(D,E,F)),(isd23(D,G),isd21(E,H),isd22(F,I),limitCheckBool(G,J),signalCond(I,K),limitCheck(K,L),limitCheckBool(H,M),osd21(M,B),osd23(J,C),osd22(L,A))).
comdesClause(cu((A,B),(C,D,E,F)),(isd41(C,G),isd42(D,H),isd43(E,I),isd44(F,J),tyrePressureController(G,H,I,K),cylPressureController(J,I,L),osd41(K,B),osd42(L,A))).
comdesClause(basicComponent(tyrePressureController(_,_,_,_)),true).
comdesClause(basicComponent(cylPressureController(_,_,_)),true).
comdesClause(basicComponent(copyStream(_,_)),true).
comdesClause(basicComponent(scaler1(_,_,_)),true).
comdesClause(basicComponent(scaler2(_,_,_)),true).
comdesClause(basicComponent(mux(_,_,_)),true).
comdesClause(tyrePressureController([A|B],[C|D],[E|F],[G|H]),(tyrePressureControllerLogic(A,C,E,G),tyrePressureController(B,D,F,H))).
comdesClause(tyrePressureController([],[],[],[]),true).
comdesClause(tyrePressureControllerLogic(A,B,C,D),(A<B,C=false,D=true)).
comdesClause(tyrePressureControllerLogic(A,B,_,C),(A>=B,C=false)).
comdesClause(tyrePressureControllerLogic(_,_,A,B),(A=true,B=false)).
comdesClause(cylPressureController([A|B],[C|D],[E|F]),(cylPressureControllerLogic(A,C,E),cylPressureController(B,D,F))).
comdesClause(cylPressureController([],[],[]),true).
comdesClause(cylPressureControllerLogic(A,B,C),(maxVal(D),A<D,B=false,C=true)).
comdesClause(cylPressureControllerLogic(_,A,B),(A=true,B=false)).
comdesClause(cylPressureControllerLogic(A,_,B),(maxVal(C),A>=C,B=false)).
comdesClause(box(isd11(_,_)),true).
comdesClause(box(osd11(_,_)),true).
comdesClause(box(isd21(_,_)),true).
comdesClause(box(isd22(_,_)),true).
comdesClause(box(isd23(_,_)),true).
comdesClause(box(osd21(_,_)),true).
comdesClause(box(osd22(_,_)),true).
comdesClause(box(osd23(_,_)),true).
comdesClause(box(isd31(_,_)),true).
comdesClause(box(isd32(_,_)),true).
comdesClause(box(isd33(_,_)),true).
comdesClause(box(isd34(_,_)),true).
comdesClause(box(osd31(_,_)),true).
comdesClause(box(osd32(_,_)),true).
comdesClause(box(osd33(_,_)),true).
comdesClause(box(isd41(_,_)),true).
comdesClause(box(isd42(_,_)),true).
comdesClause(box(isd43(_,_)),true).
comdesClause(box(isd44(_,_)),true).
comdesClause(box(osd41(_,_)),true).
comdesClause(box(osd42(_,_)),true).
comdesClause(box(signalCond(_,_)),true).
comdesClause(box(limitCheck(_,_)),true).
comdesClause(box(limitCheckBool(_,_)),true).
comdesClause(isd11(A,B),copyStream(A,B)).
comdesClause(osd11(A,B),copyStream(A,B)).
comdesClause(isd21(A,B),copyStream(A,B)).
comdesClause(isd22(A,B),copyStream(A,B)).
comdesClause(isd23(A,B),copyStream(A,B)).
comdesClause(osd21(A,B),copyStream(A,B)).
comdesClause(osd22(A,B),copyStream(A,B)).
comdesClause(osd23(A,B),copyStream(A,B)).
comdesClause(isd41(A,B),copyStream(A,B)).
comdesClause(isd42(A,B),copyStream(A,B)).
comdesClause(isd43(A,B),copyStream(A,B)).
comdesClause(isd44(A,B),copyStream(A,B)).
comdesClause(osd41(A,B),copyStream(A,B)).
comdesClause(osd42(A,B),copyStream(A,B)).
comdesClause(copyStream([],[]),true).
comdesClause(copyStream([A|B],[C|D]),(A=C,copyStream(B,D))).
comdesClause(signalCond(A,B),copyStream(A,B)).
comdesClause(limitCheck(A,B),copyStream(A,B)).
comdesClause(limitCheckBool(A,B),copyStream(A,B)).
comdesClause(cpu((_,A,B,C),(D,E,F,G)),(isd31(D,H),isd32(E,I),isd33(F,A),isd34(G,B),mux(H,I,J),scaler1(H,_,K),scaler2(H,_,L),osd31(J,C),osd32(K,A),osd33(L,B))).
comdesClause(mux([A|B],[C|D],[E|F]),(muxLogic(A,C,E),mux(B,D,F))).
comdesClause(mux([],[],[]),true).
comdesClause(muxLogic(A,_,B),(A=true,B is 0)).
comdesClause(muxLogic(A,B,C),(A=false,tyrePressure(B,C))).
comdesClause(tyrePressure(1,20),true).
comdesClause(tyrePressure(2,25),true).
comdesClause(tyrePressure(3,28),true).
comdesClause(tyrePressure(4,35),true).
comdesClause(tyrePressure(5,40),true).
comdesClause(scaler1([A|B],[C|D],[E|F]),(scaler1Logic(A,C,E),scaler1(B,D,F))).
comdesClause(scaler1([],[],[]),true).
comdesClause(scaler1Logic(A,_,B),(A=true,B is 0)).
comdesClause(scaler1Logic(A,B,C),(A=false,real(B),C is B/2)).
comdesClause(scaler2([A|B],[C|D],[E|F]),(scaler2Logic(A,C,E),scaler2(B,D,F))).
comdesClause(scaler2([],[],[]),true).
comdesClause(scaler2Logic(A,_,B),(A=true,B is 0)).
comdesClause(scaler2Logic(A,B,C),(A=false,real(B),C is B/3)).
comdesClause(isd31(A,B),copyStream(A,B)).
comdesClause(isd32(A,B),copyStream(A,B)).
comdesClause(isd33(A,B),copyStream(A,B)).
comdesClause(isd34(A,B),copyStream(A,B)).
comdesClause(osd31(A,B),copyStream(A,B)).
comdesClause(osd32(A,B),copyStream(A,B)).
comdesClause(osd33(A,B),copyStream(A,B)).
comdesClause(real(_),true).
comdesClause(maxVal(_),true).
