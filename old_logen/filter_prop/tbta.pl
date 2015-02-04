:- module(tbta, [
		     readtype/3,
                 readtype_sdv/4, 
                 dnftest/0,
                 emptest/0,
                 ringtest/0,
                 ringtest2/0,
                 test/1,
                 listlist1/0,
                 listlist2/0,
                 transition/2,
                 cartprodlist/3]).
:- use_module(library(lists)).
:- use_module(readfta).
:- use_module(sigma).
:- use_module(cartprod).
/*%%%CIAO
:- use_module(timer_ciao).
*/%%%CIAO
%%%SICS/*
:- use_module(timer_sics).
%%%SICS*/
:- dynamic transition/2.


/*  
	Read an fta
	Read functors from program
	output a dfta
	example: readtype('program.pl','logentypes.fta','logentypes.dfta').
	outputs a file named logentypes.dfta
*/

dnftest   :- readtype('dnf.pl','logentypes.fta','logentypes.dfta').
emptest   :- readtype('Tests/rev.pl','listtypes.fta','listtypes.dfta').
ringtest  :- readtype('Tests/parser.pl','listtypes.fta','ringtypes.dfta').
ringtest2 :- readtype('emptyprog.pl','listtypes.fta','ringtypes.dfta').
listlist1 :- readtype('peep.pl','listlist2.fta','listlist1.dfta').
listlist2 :- readtype('plan.pl','listlist2.fta','listlist2.dfta').

test(X) :- 
      start_time,
      grammar(X,F,Q,_,Delta),
	determinize((F,Q,Delta),(CQ,CDD)),
	length(CQ,QLength),
	length(CDD,DLength),
	open('blanchet_out',write,Os),
	liststodenotes(CDD,DDD),
	flattendd(DDD,FDD),
	file_fdd(Os,FDD),
	end_time('Determinization time: ',user_error),
	write(user_error,'Number of states '),write(user_error,QLength),nl(user_error),
	write(user_error,'Number of transitions '),write(user_error,DLength),nl(user_error),
	close(Os).
	

	
readtype(P,FI,FO) :-
	readtype_sdv(P,FI,FO,sdv). 	% default sdv
	
readtype_sdv(P,FI,FO,SDV) :-
	readfta(FI,JFTA),
	open(FO,write,Os),
	convertfta(JFTA,FTA),
	start_time,
	generatef(FTA,Fn),
	generateq(FTA,Qn),
	generatedelta(FTA,Deltag),


	%Now read functors from program
	readfunctors(SDV,P,Deltag,Fn,Qn,_SigT,DelT,FT,Qg),
	
	%sort(Qn,Q), %Without readfunctors
	sort(Qg,Q), %With readfunctors
	%sort(Fn,F), %Without readfunctors
	sort(FT,F), %With readfunctors
	%sort(Deltag,Delta), %Without readfunctors
	sort(DelT,Delta), %With readfunctors

	%count number of states and transitions
	length(Q,NRQ),
	length(Delta,NRD),
	write('Number of states: '),write(NRQ),nl,
	write('Number of transitions: '),write(NRD),nl,


	!, %Not needed?	
 
	determinize((F,Q,Delta),(DetQ,CDD)),
!,

	end_time('Determinization time: ',user_error),
	%count number of states and transitions
	length(DetQ,NRDQ),
	length(CDD,NRDD),
	write('Number of states: '),write(user_error,NRDQ),nl(user_error),
	write('Number of transitions: '),write(user_error,NRDD),nl(user_error),


	dt_addfacts(CDD),
	dt_filefacts(CDD),
	liststodenotes(CDD,DDD),
	flattendd(DDD,FDD),
	file_fdd(Os,FDD),
	close(Os).		

readfunctors(sdv,P,Delta,Fn,Qn,SigT,DelT,FT,QT) :-
	fsigma(P,SigT),
	append(SigT,Fn,Fs),
	sort(Fs,Ffor),
	generatedynamicfromsigma(Ffor,DeltaDyn),
	generatestaticfromsigma(Ffor,DeltaStat),	
	append(['$VAR'/0],Ffor,Fsigma),
	sort(Fsigma,Fsig),

	%any should be added to Q, print warning if any exists.
	
	!, %Not needed
	FT = Fsig,
	append(DeltaDyn,DeltaStat,DeltaSg),
	append(Delta,DeltaSg,DeltaSs),
	append([[['$VAR',[]],dynamic],[['$VAR',[]],var]],DeltaSs,DelT),
	append([static,dynamic,var],Qn,QT).
readfunctors(sd,P,Delta,Fn,Qn,SigT,DelT,FT,QT) :-
	fsigma(P,SigT),
	append(SigT,Fn,Fs),
	sort(Fs,Ffor),
	generatedynamicfromsigma(Ffor,DeltaDyn),
	generatestaticfromsigma(Ffor,DeltaStat),	
	append(['$VAR'/0],Ffor,Fsigma),
	sort(Fsigma,Fsig),

	%any should be added to Q, print warning if any exists.
	
	!, %Not needed
	FT = Fsig,
	append(DeltaDyn,DeltaStat,DeltaSg),
	append(Delta,DeltaSg,DeltaSs),
	append([[['$VAR',[]],dynamic]],DeltaSs,DelT),
	append([static,dynamic],Qn,QT).
readfunctors(dv,P,Delta,Fn,Qn,SigT,DelT,FT,QT) :-
	fsigma(P,SigT),
	append(SigT,Fn,Fs),
	sort(Fs,Ffor),
	generatedynamicfromsigma(Ffor,DeltaDyn),
	DeltaStat = [], %comment out if above line is commented in
	append(['$VAR'/0],Ffor,Fsigma),
	sort(Fsigma,Fsig),

	%any should be added to Q, print warning if any exists.
	
	!, %Not needed
	FT = Fsig,
	append(DeltaDyn,DeltaStat,DeltaSg),
	append(Delta,DeltaSg,DeltaSs),
	append([[['$VAR',[]],dynamic],[['$VAR',[]],var]],DeltaSs,DelT),
	append([dynamic,var],Qn,QT).
readfunctors(d,P,Delta,Fn,Qn,SigT,DelT,FT,QT) :-
	fsigma(P,SigT),
	append(SigT,Fn,Fs),
	sort(Fs,Ffor),
	generatedynamicfromsigma(Ffor,DeltaDyn),
	DeltaStat = [], %comment out if above line is commented in
	append(['$VAR'/0],Ffor,Fsigma),
	sort(Fsigma,Fsig),

	%any should be added to Q, print warning if any exists.
	
	!, %Not needed
	FT = Fsig,
	append(DeltaDyn,DeltaStat,DeltaSg),
	append(Delta,DeltaSg,DeltaSs),
	append([[['$VAR',[]],dynamic]],DeltaSs,DelT),
	append([dynamic],Qn,QT).

dt_addfacts([]).
dt_addfacts([[[F,A],Q]|Ts]) :-
	gentrans(F,A,T),
	assertz(transition(T,Q)),
	%assertz_fact(transition(T,Q)),
	dt_addfacts(Ts).

dt_filefacts(Fs) :-
	open('disjointtypes.pl',write,Os),
	write(Os,':- module(disjointtypes, [transition/2]).'),nl(Os),
	dt_filefact(Fs,Os),
	close(Os).

dt_filefact([],_).
dt_filefact([[[F,A],Q]|Ts],Os) :-
	F \== '$VAR',
	gentrans(F,A,T),
	write(Os,transition(T,Q)),write(Os,'.'),nl(Os),
	dt_filefact(Ts,Os).

dt_filefact([[['$VAR',A],Q]|Ts],Os) :-
	gentrans('$VAR',A,T),
	write(Os,'transition(\''),write(Os,T),write(Os,'\','),write(Os,Q),write(Os,').'),nl(Os),
	dt_filefact(Ts,Os).


gentrans(F,[],F).
gentrans(F,A,T) :-
	A \== [],
	append([F],A,L1),
	T =.. L1.


convertfta([],[]).
convertfta([[F,A,S]|Xs],[[[F,A],S]|Ys]):-
	convertfta(Xs,Ys).

generatef([],[]).
generatef([[[F,A],_]|Xs],[F/L|Ys]) :-
	length(A,L),
	generatef(Xs,Ys).

generateq([],[]).
generateq([[[_,_],S]|Xs],[S|Ys]) :-
	generateq(Xs,Ys).

generatedelta(X,[]) :- var(X).
generatedelta(X,X).

generatestaticfromsigma([],[]).
generatestaticfromsigma([F/A|Ss],[[[F,Al],static]|Ds]):-
	generatestaticlist(A,Al),
	generatestaticfromsigma(Ss,Ds).
	
generatestaticlist(0,[]).
generatestaticlist(N,[static|Al]) :-
	M is N - 1,
	generatestaticlist(M,Al).


generatedynamicfromsigma([],[]).
generatedynamicfromsigma([F/A|Ss],[[[F,Al],dynamic]|Ds]):-
	generatedynamiclist(A,Al),
	generatedynamicfromsigma(Ss,Ds).
	
generatedynamiclist(0,[]).
generatedynamiclist(N,[dynamic|Al]) :-
	M is N - 1,
	generatedynamiclist(M,Al).

%flipf([],[]).
%flipf([L/F|Xs],[F/L|Ys]):-
%	flipf(Xs,Ys).


/* End readfta */

/* Begin second version of determinization alg. */

determinize((Sigma,Q,Delta),(EndQ,EndD)) :-
	QD  = [],
	DDp = [],
	dj_const(Sigma,Q,Delta,QD,DDp,CQ,CD),
	write('determinize const completed'),nl,
	!,
	dj_repeat(Sigma,Q,Delta,CQ,CD,EndQ,EndD,[]).

/* Handle constants separately */
dj_const(Sigma,Q,Delta,QD,DDp,CQ,CD) :-
	c_foreachf(Sigma,Q,Delta,QD,DDp,CQ,CD).
	
	
dj_repeat(_,_,_,EQD,EDD,EQD,EDD,EDD).
dj_repeat(Sigma,Q,Delta,EQD,EDD,CQ,CD,_) :-
	foreachf(Sigma,Q,Delta,EQD,EDD,TQ,TD),
	write('An iteration completed'),nl,
	dj_repeat(Sigma,Q,Delta,TQ,TD,CQ,CD,EDD).

/* Constants */
c_foreachf([],_,_,Qd,DeltaD,Qd,DeltaD).

c_foreachf([_/N|Sigma],Q,Delta,Qd,DDp,CQ,CD) :-
	N>0,
	c_foreachf(Sigma,Q,Delta,Qd,DDp,CQ,CD).	
	
c_foreachf([F/0|Sigma],Q,Delta,Qd,DDp,CQ,CD) :-
	%Selected F, now select s from Qd
	!,
	chooseSs(N,Qd,Ss),
	c_foreachchoiceofs(F/N,Ss,S,Delta,DeltaDadd),
	append(DeltaDadd,DDp,EmptyDeltaD),
	stripemptyrhs(EmptyDeltaD,DoubleDeltaD),
	sort(DoubleDeltaD,NewDeltaD),
	append(S,Qd,EmptyQd),
	stripempty(EmptyQd,DoubleQd),
	sortindividuals(DoubleQd,SortedQd),
	sort(SortedQd,NewQd),
	c_foreachf(Sigma,Q,Delta,NewQd,NewDeltaD,CQ,CD).

c_foreachchoiceofs(_,[],[],_,[]).
c_foreachchoiceofs(F/N,[S|Ss],[NewS|Ns],Delta,[[[F,S],NewS]|DeltaDadd]):-
	%form possible choices
	schoices(N,S,Schoices), %more
	c_checkchoises(F,Schoices,Delta,ListNewS),
	joinlists(ListNewS,UnsortedNewS),
	sort(UnsortedNewS,NewS),
	c_foreachchoiceofs(F/N,Ss,Ns,Delta,DeltaDadd).

%skal liste laves flatten		
c_checkchoises(_,[],_,[]).		
c_checkchoises(F,[C|Cs],Delta,[Q|S]) :-
	setof(X,member([[F,C],X],Delta),Q),
	checkchoises(F,Cs,Delta,S).
c_checkchoises(F,[C|Cs],Delta,S) :-
	notmember([[F,C],_],Delta),
	checkchoises(F,Cs,Delta,S).


/* End constants */
foreachf([],_,_,Qd,DeltaD,Qd,DeltaD).

foreachf([_/0|Sigma],Q,Delta,Qd,Dd,CQ,CD) :-
	  foreachf(Sigma,Q,Delta,Qd,Dd,CQ,CD).	
	
foreachf([F/N|Sigma],Q,Delta,Qd,Dd,CQ,CD) :-
	N > 0,!,
	%Selected F, now select s from Qd
	!,
	chooseSs(N,Qd,Ss),
	foreachchoiceofs(F/N,Ss,S,Delta,DeltaDadd),
	append(DeltaDadd,Dd,EmptyDeltaD),
	stripemptyrhs(EmptyDeltaD,DoubleDeltaD),
	sort(DoubleDeltaD,NewDeltaD),
	append(S,Qd,EmptyQd),
	stripempty(EmptyQd,DoubleQd),
	sortindividuals(DoubleQd,SortedQd),
	sort(SortedQd,NewQd),
	foreachf(Sigma,Q,Delta,NewQd,NewDeltaD,CQ,CD).

foreachchoiceofs(_,[],[],_,[]).
foreachchoiceofs(F/N,[S|Ss],[NewS|Ns],Delta,[[[F,S],NewS]|DeltaDadd]):-
	%form possible choices
	%cartprodlist(N,S,Schoices), %setof+member based cart.prod.
	cartprod(S,Schoices), %Johns cartesian product pred.
	checkchoises(F,Schoices,Delta,ListNewS),
	joinlists(ListNewS,UnsortedNewS),
	sort(UnsortedNewS,NewS),
	foreachchoiceofs(F/N,Ss,Ns,Delta,DeltaDadd).

cartprodlist(_,S,Choice1) :-
	setof(X,pickset(S,X),Choice1).

pickset([],[]).
pickset([S|Ss],[X|Xs]) :-
	member(X,S),
	pickset(Ss,Xs).


%skal liste laves flatten		
checkchoises(_,[],_,[]).		
checkchoises(F,[C|Cs],Delta,[Q|S]) :-
	setof(X,member([[F,C],X],Delta),Q),
	checkchoises(F,Cs,Delta,S).
checkchoises(F,[C|Cs],Delta,S) :-
	notmember([[F,C],_],Delta),
	checkchoises(F,Cs,Delta,S).

/* begin strip empty set */
stripempty([],[]).
stripempty([[]|Ls],Rs):-
	stripempty(Ls,Rs).
stripempty([N|Ls],[N|Rs]):-
	N \== [],
	stripempty(Ls,Rs).

stripemptyrhs([],[]).
stripemptyrhs([[[_,_],[]] |Ls],Rs) :-
	stripemptyrhs(Ls,Rs).
stripemptyrhs([[[F,S],Q]|Ls],[[[F,S],Q]|Rs]) :-
	Q \== [],
	stripemptyrhs(Ls,Rs).

/* Sort disjoint type: [a,b] is the same as [b,a] */
sortindividuals([],[]).
sortindividuals([S|Ss],[L|Ls]) :-
	sort(S,L),
	sortindividuals(Ss,Ls).


/* chooseSs */
chooseSs(N,Q,Ss) :-
	schoices(N,Q,Ss).

/* Begin form cartesian product */
%schoices(3,[],X) should produce [[],[],[]]
%schoices(3,[a],X) should produce [[a,a,a]]
%schoices(3,[[a]],X) should produce [[[a],[a],[a]]]


schoices(0,_,[[]]) :- !. %was 0,empty,emptylist
schoices(N,[],[[]|S]) :-
	M is N-1,
	schoices(M,[],S).
schoices(N,QD,SC) :-
	QD \== [],
	!,
	cart1(N,1,QD,SC).

cart1(1,1,QD,SC) :-
!,
	setof([X],member(X,QD),SC).

cart1(N,1,QD,SC) :-
	N>1,
	setof([X],member(X,QD),SCn),
	cart2(N,2,QD,SCn,_,ListSC),
	sort(ListSC,SC).	

cart2(N,N,QD,SC,_,SCr) :-	
	!,
	cart3(QD,SC,SClists),
	!,
	joinlists(SClists,SCr).
	
cart2(N,M,QD,SC,SCn,SCr) :-
	M>1,
	M < N,
	O is M +1,
	cart3(QD,SC,SClists),
	joinlists(SClists,SCn),
	cart2(N,O,QD,SCn,_,SCr).
		
	
cart3([],_,[]).
cart3([Q|Qs],Sc,[Sc2|NSc]):-
	cartp2(Q,Sc,Sc2),
	cart3(Qs,Sc,NSc).

	
cartp2(_,[],[]).	
cartp2(Q,[S|Sc],[NSc|N]) :-
	append([Q],S,NSc),
	cartp2(Q,Sc,N).
/* End form cartesian product */	
	
	
/* Utils? */
joinlists([],[]). 
joinlists([A|L1],L3) :-
        myconc(A,L2,L3),
        joinlists(L1,L2).

myconc([],L,L).
myconc([X|L1],L2,[X|L3]) :-
        X \==[],
        myconc(L1,L2,L3).
myconc([X|L1],L2,L3) :-
        X ==[],
        myconc(L1,L2,L3).
	
notmember(X,L) :- \+ member(X,L).	
	
/* End second version */



pprint_fdd([]).
pprint_fdd([_X|Xs]) :-
	%writeq(X),write('.'),nl,
	pprint_fdd(Xs).

file_fdd(_,[]).
file_fdd(FO,[X|Xs]) :-
	writeq(FO,X),write(FO,'.'),nl(FO),
	file_fdd(FO,Xs).


ftaprint([]).
ftaprint([[[_A,B],_C]|Ds]) :-
	%write(A),write('('),
	argprint(B),
	%write(')-'),write(C),write(','),
	ftaprint(Ds).

argprint([]).
argprint([_X|Xs]) :-
	%write(X),write(','),
	argprint(Xs).

pprint([]).
pprint([_D|Ds]) :-
	%write(D),nl,
	pprint(Ds).


%------------------------------------------------------------------
%Lists to denotes.

liststodenotes([],[]).
liststodenotes([[[F,A],Lhs]|Ls],[D|Ds]) :-
	F \== cons,
	F \== nil,
	Rhs =.. [F|A],
	D = denotes(Rhs,Lhs),
	liststodenotes(Ls,Ds).

liststodenotes([[[F,A],Lhs]|Ls],[D|Ds]) :-
	F == cons,
	Rhs =.. [.|A],
	D = denotes(Rhs,Lhs),
	liststodenotes(Ls,Ds).

liststodenotes([[[F,A],Lhs]|Ls],[D|Ds]) :-
	F == nil,
	Rhs =.. [[]|A],
	D = denotes(Rhs,Lhs),
	liststodenotes(Ls,Ds).



flatten_denotes(denotes(T,V),Den) :-
	T =.. [F|Xs],
	name(denotes,Dn),
	name(F,Fn),
	myconc(Dn,[95|Fn],DFn),
	name(DF,DFn),
	myconc(Xs,[V],Ys),
	Den =.. [DF|Ys].

flattendd([],[]).
flattendd([D|Ds],[F|Fs]) :-
	flatten_denotes(D,F),
	flattendd(Ds,Fs).



%------------------------------------------------------------------

/* FTA completeness 
	
   A finite Tree Automaton is defined as a typle:
	(Q,F,Qf,Delta), where
	Q   : Set of states (unary)
	F   : Set of functors (eg f/2 : for f(a,b))
	Qf : Set of final states
	Delta : Set of transition rules

   	Sets of transition rules are represented as lists of rules
	Each rule A -> B is represented as A - B.
	
	Other sets are represented as lists as well.	
*/


other(beta).

ftest(X) :-
	sample(X,Y),complete(Y,Z),write_fta(Z).
		
% complete(+FTA,-CompleteFTA) : succeeds if CompleteFTA is the 
% 	same as FTA (both finite tree automata) but with all ground 
% 	terms not accepted by FTA being accepted by CompleteFTA with
%	t *-> Beta. Where other(Beta).
%	Essentially, this extends Q and Qf with Beta and F with some 
%	extra transition rules.
complete((Q,F,Qf,D),CompleteFTA) :-
	other(O),
	complete_x(([O|Q],F,[O|Qf],D),CompleteFTA).
	
complete_x((Q,[],Qf,Delta), ([Other|Q],[],[Other|Qf],Delta)) :-
	other(Other). 

complete_x((Q,[F/Arity|Fs],Qf,Delta),(NQ,[F/Arity|Fs],NQf,NewDelta)) :-
	%varlist(Arity,VarList),
	%findall(VarList,( Func =.. [F|VarList],member(Func - _,Delta) ), ArgLists),
	
	findall(VarList,member([[F,VarList],_],Delta), ArgListsD),
	sort(ArgListsD,ArgLists),
	
	find_args(ArgLists,Args),
	find_deltas_missing_args(1,F/Arity,Args,Q,ND1),
	append(ND1,ND2,ND3),
	find_deltas_odd_combinations(F,Args,Delta,ND2),
	append(ND3,ND4,NewDelta),
	complete_x((Q,Fs,Qf,Delta),(NQ,Fs,NQf,ND4)).

% find_deltas_odd_combinations(+F,+Args,+Delta,-NewDelta) : 
% succeeds if NewDelta is the set of missing reductions in the FTA with 
% Args describing possible arguments to the functor F (already in some 
% reduction) and Delta being the reductions already in the FTA.
find_deltas_odd_combinations(F,Args,Delta,NewDelta) :-
%	other(Other),
	setof(Func,(  make_func(F,Args,Func),
	 			       	 non_member([Func,_],Delta)),NewDelta).
find_deltas_odd_combinations(F,Args,Delta,[]) :-
	\+ setof(Func,(  make_func(F,Args,Func),
	 			       	 non_member([Func,_],Delta)),_).

% make_func(+F,+Args,-Func) : makes a functor, Func, of type F with arguments
% collected from Args - a list of lists of the possible...
make_func(F,Args,Func) :-
	map(member,X,Args),
	Func = [F,X].
	
% find_deltas_missing_args(+N,+F/+Arity,+Args,+Q,+Ds) :-
% finds the missing reductions on functor F with Arity, where Args is a list
% of lists with arguments used somewhere in the reductions already in the
% FTA and Q being the FTA states
find_deltas_missing_args(_,_,[],_,[]).
find_deltas_missing_args(N,F/Arity,[Arg|Args],Q,Ds) :-
	setof(A,(member(A,Q),non_member(A,Arg)),Missing),
	make_deltas1(N,F/Arity,Missing,D1),
	append(D1,X,Ds),
	N2 is N + 1,
	find_deltas_missing_args(N2,F/Arity,Args,Q,X).
find_deltas_missing_args(N,F/Arity,[Arg|Args],Q,Ds) :-
	\+ setof(A,(member(A,Q),non_member(A,Arg)),_),
	N2 is N + 1,
	find_deltas_missing_args(N2,F/Arity,Args,Q,Ds).
	
% Auxiliary predicate for find_deltas_missing_args/5
make_deltas1(_,_,[],[]).
make_deltas1(N,F/Arity,[A|T],[[Func,Other]|T2]) :-
	varlist(Arity,VarList),
	other(Other),
	nth(N,VarList,A),
	%Func =.. [F|VarList],
	Func = [F,VarList],
	make_deltas1(N,F/Arity,T,T2).

% find_args(+L1,-L2) : succeeds if L1 is a list of argument patterns for 
% some functor and L2 is a list of lists of possible arguments for 
% each argument (eg find_args([[1,2],[1,3]],[[1],[2,3]])) 	
find_args(L,[]) :-
	list_of(L,[]).
find_args(L,[H|T]) :-
	map(head,L,H1),
	sort(H1,H),
	map(tail,L,L2),
	find_args(L2,T).
	
% write_fta(FTA) : pretty-print an FTA
write_fta((Q,F,Qf,Delta)) :-
	write('Q : {'), write_Qs(Q),write('}'),nl,
	write('F : {'), write_Fs(F),write('}'),nl,
	write('Q_f : {'), write_Qs(Qf),write('}'),nl,
	write('Delta : '),nl,write('{'), nl,write_Deltas(Delta),write('}'),nl.
	
write_Qs([]).
write_Qs([A]) :- write(A).
write_Qs([A|T]) :- \+ length(T,0),write(A),write(', '),write_Qs(T).
write_Fs([]).
write_Fs([F/Arity]) :- write_F(F/Arity).
write_Fs([F/A|T]) :- \+ length(T,0),write_F(F/A),write(', '),write_Fs(T).
write_F(F/0) :- write(F).
write_F(F/A) :- A > 0,write(F),write('('),X is A - 1, writeN(X,', '),write(')').
write_Deltas([]).
write_Deltas([D|T]) :- write_D(D),nl,write_Deltas(T).
write_D([[A,B],C]) :- write(A),write(B),write(' -> '),write(C).
	
writeN(0,_).
writeN(X,K) :- X > 0, write(K),X2 is X -1, writeN(X2,K).
		
% UTILITY PREDICATES

% list_of(L,E) : List L is a list of Es
list_of([],_).
list_of([A|T],A) :- list_of(T,A).
		
% head and tail
head([H|_],H).
tail([_|T],T).
	
% map...
map(_,[],[]).
map(P,[A|T1],[B|T2]) :-
	Func =.. [P,A,B],
	call(Func),
	map(P,T1,T2).
	
% varlist(N,L) : make a list of variables N long.
varlist(0,[]).
varlist(N,[_|T]) :-
	N > 0,
	N2 is N - 1,
	varlist(N2,T).

non_member(_,[]).
non_member(A,[B|T]) :-
	A \= B,
	non_member(A,T).

% some sample FTAs
sample(1,([qa,qg,qf],[f/2,g/1,a/0],[qf],[a-qa,g(qg)-qg(g(x)),g(qa)-qg(g(x)),f(qg,qg)-qf(f(x,y))])).

% Shows a bug!!!	
sample(2,([intlist,int,list],[cons/2,int/0,nil/0],[intlist,int,list],[cons(int,list)-intlist,nil-intlist,cons(int,int)-beta])).

sample(3,([intlist,int,list],[cons/2,int/1,nil/0],[intlist],[cons(int,list)-intlist,nil-intlist])).
	
sample(4,([[q],[q,qf,qg],[q,qg]] ,[a/0,f/2,g/1] ,[[q,qf,qg]] ,[a-[q],f([q],[q])-[q],f([q],[q,qf,qg])-[q],f([q],[q,qg])-[q],
				f([q,qf,qg],[q])-[q],f([q,qf,qg],[q,qf,qg])-[q],f([q,qf,qg],[q,qg])-[q],
				f([q,qg],[q])-[q],f([q,qg],[q,qf,qg])-[q],f([q,qg],[q,qg])-[q],g([q])-[q,qg],
				g([q,qf,qg])-[q,qf,qg],g([q,qg])-[q,qf,qg]])).
				
sample(5,([q,qg,qf],[a/0,f/2,g/1],[qf],[[[a,[]],q] , [[g,[q]],q] , [[g,[q]],qg] , [[g,[qg]],qf] , [[f,[q,q]],q] ])).
sample(6,([q,qg,qf],[a/0,f/2,g/1],[qf],[a-q , g(q)-q , g(q)-qg, g(qg)-qf , f(q,q)-q ])).


%------------------------------------------------------------------



grammar(1,F, Q, QF, Delta) :- 
	F=[a/0,f/2,g/1],
	Q=[q,qg,qf],
	QF=[qf],
	Delta=[ [[a,[]],q] , [[g,[q]],q] , [[g,[q]],qg] , [[g,[qg]],qf] , [[f,[q,q]],q] ].
	
grammar(2,F, Q, QF, Delta) :- 
	F=[a/0,g/1],
	Q=[q0,q1,q],
	QF=[q0],
	Delta=[ [[a,[]],q0] , [[g,[q0]],q1] , [[g,[q1]],q0] , [[g,[q]],q0] , [[g,[q]],q1] ].
	
grammar(3,F, Q, QF, Delta) :- 
	F=[a/0,g/1],
	Q=[q0,q1],
	QF=[q0],
	Delta=[ [[a,[]],q0] , [[g,[q0]],q1] , [[g,[q1]],q0] ].
	
grammar(4,F, Q, QF, Delta) :- 
	F=[nil/0,c/0,cons/2],
	Q=[any,list,listlist],
	QF=[listlist],
	Delta=[ [[nil,[]], any] , [[nil,[]], list] , [[nil,[]], listlist] ,
			[[c,[]], any], 
			[[cons,[any,any]], any], [[cons,[any,list]], list],
			[[cons,[list,listlist]], listlist],	[[cons,[listlist,listlist]], listlist]].
	
grammar(5,F, Q, QF, Delta) :- 
	F=[zero/0,s/1,m/3,a/0,b/0,c/0,tup/2],
	Q=[nat,moves,peg,tuptwo],
	QF=[moves],
	Delta=[ [[zero,[]], nat] , [[s,[nat]], nat] , [[m,[moves,tuptwo,moves]], moves] ,
			[[tup,[peg,peg]], moves],
			[[tup,[peg,peg]], tuptwo],
			[[p,[peg,peg]], tup], [[a,[]], peg],
			[[b,[]], peg],	[[c,[]], peg]].
	
grammar(6,F, Q, QF, Delta) :- 
	F=[zero/0,one/0,s/1,jugs/2,nil/0,cons/2,fill/1,empty/1,transfer/2,two/0],
	Q=[nat,jug,state,states,move,moves],
	QF=[moves],
	Delta=[ [[zero,[]], nat] , [[s,[nat]], nat] , [[one,[]], jug] , [[two,[]], jug] ,
	        [[jugs,[nat,nat]], state] , [[nil,[]], states], [[cons,[state,states]], states] ,	 
			[[fill,[jug]], move], [[empty,[jug]], move],
			[[transfer,[jug,jug]], move],	[[nil,[]], moves],
			[[cons,[move,moves]], moves]].
		
grammar(7,F, Q, QF, Delta) :- 
	F=[nil/0,c/0,cons/2],
	Q=[any,list,onelist,twolist,empty],
	QF=[listlist],
	Delta=[ [[nil,[]], any] , [[nil,[]], list] , [[nil,[]], empty] ,
			[[c,[]], any], 
			[[cons,[any,any]], any], [[cons,[any,list]], list],
			[[cons,[any,empty]], onelist],	[[cons,[any,onelist]], twolist]].
			
grammar(11,F, Q, QF, Delta) :- 
	F=[nil/0,a/0,v/0,cons/2],
	Q=[static,nonvar,dynamic,list,listlist],
	QF=[listlist],
	Delta=[ [[nil,[]], static] , [[nil,[]], dynamic] , [[nil,[]], list] , [[nil,[]], listlist] ,
			[[a,[]], static], [[a,[]], dynamic] , [[v,[]], dynamic] , 
			[[cons,[static,static]], static], [[cons,[nonvar,static]], nonvar],
			[[cons,[static,nonvar]], nonvar],	[[cons,[nonvar,nonvar]], nonvar],
			[[cons,[dynamic,static]], nonvar], [[cons,[dynamic,nonvar]], nonvar],
			[[cons,[dynamic,dynamic]], nonvar], [[cons,[static,dynamic]], nonvar],
			[[cons,[nonvar,dynamic]], nonvar], [[cons,[dynamic,dynamic]], dynamic],
			[[cons,[dynamic,list]], list], [[cons,[list,listlist]], listlist]
			].
		
grammar(12,F, Q, QF, Delta) :- 
	F=[nil/0,a/0,v/0,cons/2],
	Q=[static,nonvar,dynamic,list],
	QF=[list],
	Delta=[ [[nil,[]], static] , [[nil,[]], dynamic] , [[nil,[]], list] ,
			[[a,[]], static], [[a,[]], dynamic] , [[v,[]], dynamic] , 
			[[cons,[static,static]], static], [[cons,[nonvar,static]], nonvar],
			[[cons,[static,nonvar]], nonvar],	[[cons,[nonvar,nonvar]], nonvar],
			[[cons,[dynamic,static]], nonvar], [[cons,[dynamic,nonvar]], nonvar],
			[[cons,[dynamic,dynamic]], nonvar], [[cons,[static,dynamic]], nonvar],
			[[cons,[nonvar,dynamic]], nonvar], [[cons,[dynamic,dynamic]], dynamic],
			[[cons,[dynamic,list]], list]
			].
		
grammar(14,F, Q, QF, Delta) :- 
	F=[nil/0,a/0,v/0,cons/2],
	Q=[static,nonvar,var,dynamic,list],
	QF=[list],
	Delta=[ [[nil,[]], static] , [[nil,[]], dynamic] , [[nil,[]], list] , [[nil,[]], listlist] ,
			[[a,[]], static], [[a,[]], dynamic], [[v,[]], dynamic], [[v,[]], var], 
			[[cons,[nonvar,static]], nonvar],
			[[cons,[static,nonvar]], nonvar],	[[cons,[nonvar,nonvar]], nonvar],
			[[cons,[var,static]], nonvar], [[cons,[var,nonvar]], nonvar],
			[[cons,[nonvar,var]], nonvar], [[cons,[static,var]], nonvar],
			[[cons,[dynamic,dynamic]], dynamic],	
			[[cons,[dynamic,list]], list], [[cons,[list,listlist]], listlist]
			].

grammar(15,F,Q,[],Delta) :-	
Delta = [
[[skA,[]],attacker1_11_1],
[[skB,[]],attacker1_11_1],
[[pencrpyt,[attacker1_11_sub3,attacker1_11_sub7]],attacker1_11_1],
[[sencrpyt,[const_s,attacker1_12_1]],attacker1_11_1],
[[pencrypt,[attacker1_1_1,attacker1_1_2]],attacker1_11_1],
[[sign,[attacker1_4_1,attacker1_4_2]],attacker1_11_1],
[[sencrypt,[attacker1_6_1,attacker1_6_2]],attacker1_11_1],
[[pk,[attacker1_2_1]],attacker1_11_1],
[[pk,[const_skA]],attacker1_11_1],
[[pk,[const_skB]],attacker1_11_1],
[[a,[]],attacker1_11_1],
[[pencrpyt,[attacker1_11_sub3,attacker1_11_sub7]],attacker1_11_sub2],
[[sign,[attacker1_11_sub4,const_skA]],attacker1_11_sub3],
[[k,[attacker1_11_sub5]],attacker1_11_sub4],
[[pk,[attacker1_11_1]],attacker1_11_sub5],
[[pk,[attacker1_11_1]],attacker1_11_sub7],
[[k,[attacker1_11_sub5]],attacker1_12_1],
[[sencrpyt,[const_s,attacker1_12_1]],attacker1_12_sub2],
[[pencrpyt,[attacker1_11_sub3,attacker1_11_sub7]],attacker1_1_1],
[[sencrpyt,[const_s,attacker1_12_1]],attacker1_1_1],
[[pencrypt,[attacker1_1_1,attacker1_1_2]],attacker1_1_1],
[[sign,[attacker1_4_1,attacker1_4_2]],attacker1_1_1],
[[sencrypt,[attacker1_6_1,attacker1_6_2]],attacker1_1_1],
[[pk,[attacker1_2_1]],attacker1_1_1],
[[pk,[const_skA]],attacker1_1_1],
[[pk,[const_skB]],attacker1_1_1],
[[a,[]],attacker1_1_1],
[[pencrpyt,[attacker1_11_sub3,attacker1_11_sub7]],attacker1_1_2],
[[sencrpyt,[const_s,attacker1_12_1]],attacker1_1_2],
[[pencrypt,[attacker1_1_1,attacker1_1_2]],attacker1_1_2],
[[sign,[attacker1_4_1,attacker1_4_2]],attacker1_1_2],
[[sencrypt,[attacker1_6_1,attacker1_6_2]],attacker1_1_2],
[[pk,[attacker1_2_1]],attacker1_1_2],
[[pk,[const_skA]],attacker1_1_2],
[[pk,[const_skB]],attacker1_1_2],
[[a,[]],attacker1_1_2],
[[pencrypt,[attacker1_1_1,attacker1_1_2]],attacker1_1_sub2],
[[pencrpyt,[attacker1_11_sub3,attacker1_11_sub7]],attacker1_2_1],
[[sencrpyt,[const_s,attacker1_12_1]],attacker1_2_1],
[[pencrypt,[attacker1_1_1,attacker1_1_2]],attacker1_2_1],
[[sign,[attacker1_4_1,attacker1_4_2]],attacker1_2_1],
[[sencrypt,[attacker1_6_1,attacker1_6_2]],attacker1_2_1],
[[pk,[attacker1_2_1]],attacker1_2_1],
[[pk,[const_skA]],attacker1_2_1],
[[pk,[const_skB]],attacker1_2_1],
[[a,[]],attacker1_2_1],
[[pk,[attacker1_2_1]],attacker1_2_sub2],
[[pencrpyt,[attacker1_11_sub3,attacker1_11_sub7]],attacker1_3_1],
[[sencrpyt,[const_s,attacker1_12_1]],attacker1_3_1],
[[pencrypt,[attacker1_1_1,attacker1_1_2]],attacker1_3_1],
[[sign,[attacker1_4_1,attacker1_4_2]],attacker1_3_1],
[[sencrypt,[attacker1_6_1,attacker1_6_2]],attacker1_3_1],
[[pk,[attacker1_2_1]],attacker1_3_1],
[[pk,[const_skA]],attacker1_3_1],
[[pk,[const_skB]],attacker1_3_1],
[[a,[]],attacker1_3_1],
[[pencrpyt,[attacker1_11_sub3,attacker1_11_sub7]],attacker1_4_1],
[[sencrpyt,[const_s,attacker1_12_1]],attacker1_4_1],
[[pencrypt,[attacker1_1_1,attacker1_1_2]],attacker1_4_1],
[[sign,[attacker1_4_1,attacker1_4_2]],attacker1_4_1],
[[sencrypt,[attacker1_6_1,attacker1_6_2]],attacker1_4_1],
[[pk,[attacker1_2_1]],attacker1_4_1],
[[pk,[const_skA]],attacker1_4_1],
[[pk,[const_skB]],attacker1_4_1],
[[a,[]],attacker1_4_1],
[[pencrpyt,[attacker1_11_sub3,attacker1_11_sub7]],attacker1_4_2],
[[sencrpyt,[const_s,attacker1_12_1]],attacker1_4_2],
[[pencrypt,[attacker1_1_1,attacker1_1_2]],attacker1_4_2],
[[sign,[attacker1_4_1,attacker1_4_2]],attacker1_4_2],
[[sencrypt,[attacker1_6_1,attacker1_6_2]],attacker1_4_2],
[[pk,[attacker1_2_1]],attacker1_4_2],
[[pk,[const_skA]],attacker1_4_2],
[[pk,[const_skB]],attacker1_4_2],
[[a,[]],attacker1_4_2],
[[sign,[attacker1_4_1,attacker1_4_2]],attacker1_4_sub2],
[[pencrpyt,[attacker1_11_sub3,attacker1_11_sub7]],attacker1_5_1],
[[sencrpyt,[const_s,attacker1_12_1]],attacker1_5_1],
[[pencrypt,[attacker1_1_1,attacker1_1_2]],attacker1_5_1],
[[sign,[attacker1_4_1,attacker1_4_2]],attacker1_5_1],
[[sencrypt,[attacker1_6_1,attacker1_6_2]],attacker1_5_1],
[[pk,[attacker1_2_1]],attacker1_5_1],
[[pk,[const_skA]],attacker1_5_1],
[[pk,[const_skB]],attacker1_5_1],
[[a,[]],attacker1_5_1],
[[pencrpyt,[attacker1_11_sub3,attacker1_11_sub7]],attacker1_6_1],
[[sencrpyt,[const_s,attacker1_12_1]],attacker1_6_1],
[[pencrypt,[attacker1_1_1,attacker1_1_2]],attacker1_6_1],
[[sign,[attacker1_4_1,attacker1_4_2]],attacker1_6_1],
[[sencrypt,[attacker1_6_1,attacker1_6_2]],attacker1_6_1],
[[pk,[attacker1_2_1]],attacker1_6_1],
[[pk,[const_skA]],attacker1_6_1],
[[pk,[const_skB]],attacker1_6_1],
[[a,[]],attacker1_6_1],
[[pencrpyt,[attacker1_11_sub3,attacker1_11_sub7]],attacker1_6_2],
[[sencrpyt,[const_s,attacker1_12_1]],attacker1_6_2],
[[pencrypt,[attacker1_1_1,attacker1_1_2]],attacker1_6_2],
[[sign,[attacker1_4_1,attacker1_4_2]],attacker1_6_2],
[[sencrypt,[attacker1_6_1,attacker1_6_2]],attacker1_6_2],
[[pk,[attacker1_2_1]],attacker1_6_2],
[[pk,[const_skA]],attacker1_6_2],
[[pk,[const_skB]],attacker1_6_2],
[[a,[]],attacker1_6_2],
[[sencrypt,[attacker1_6_1,attacker1_6_2]],attacker1_6_sub2],
[[pencrpyt,[attacker1_11_sub3,attacker1_11_sub7]],attacker1_7_1],
[[sencrpyt,[const_s,attacker1_12_1]],attacker1_7_1],
[[pencrypt,[attacker1_1_1,attacker1_1_2]],attacker1_7_1],
[[sign,[attacker1_4_1,attacker1_4_2]],attacker1_7_1],
[[sencrypt,[attacker1_6_1,attacker1_6_2]],attacker1_7_1],
[[pk,[attacker1_2_1]],attacker1_7_1],
[[pk,[const_skA]],attacker1_7_1],
[[pk,[const_skB]],attacker1_7_1],
[[a,[]],attacker1_7_1],
[[pk,[const_skA]],attacker1_8_sub2],
[[pk,[const_skB]],attacker1_9_sub2],
[[a,[]],const_a],
[[s,[]],const_s],
[[skA,[]],const_skA],
[[skB,[]],const_skB],
[[attacker,[attacker1_5_1]],type_attacker1],
[[attacker,[attacker1_12_sub2]],type_attacker1]],
Q = [attacker1_11_1,attacker1_11_sub2,attacker1_11_sub3,attacker1_11_sub4,attacker1_11_sub5,attacker1_11_sub7,attacker1_12_1,attacker1_12_sub2,attacker1_1_1,attacker1_1_2,attacker1_1_sub2,attacker1_2_1,attacker1_2_sub2,attacker1_3_1,attacker1_4_1,attacker1_4_2,attacker1_4_sub2,attacker1_5_1,attacker1_6_1,attacker1_6_2,attacker1_6_sub2,attacker1_7_1,attacker1_8_sub2,attacker1_9_sub2,const_a,const_s,const_skA,const_skB,type_attacker1],
F = [attacker/1,s/0,k/1,a/0,pk/1,sencrypt/2,sign/2,pencrypt/2,sencrpyt/2,pencrpyt/2,skB/0,skA/0].

