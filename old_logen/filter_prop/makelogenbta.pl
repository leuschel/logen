:- module(makelogenbta,[makelogenbta/1,makelogenbta/3]).

:- use_module(dfta).
:- use_module(setops).

% currently assumes listtype.pl only, but could be any type in principle

makelogenbta(Prog) :-
	makelogenbta(Prog,'logenbta.pl','listtype.pl').
	
makelogenbta(Prog,Outfile,RegType) :-
	dfta(RegType,Prog,DFTA,_,sdvn),
	open(Outfile,write,S),
	write(S,':- module(logenbta, ['),
	nl(S),
	getDenotes(DFTA,Sigma),
	writeExports(Sigma,S),
	write(S,'      denotes_symbol/1]).'),
	nl(S),
	nl(S),
	writeSymbol(Sigma,S),
	nl(S),
	writeDFTA(S,DFTA),
	close(S).
	
writeExports([],_).
writeExports([F/N|Ds],S) :-
	write(S,'      '),
	functor(T,F,N),
	flatten_denotes(denotes(T,_),D),
	functor(D,Den,M),
	writeq(S,Den/M),
	write(S,','),
	nl(S),
	writeExports(Ds,S).
	
writeSymbol([],_).
writeSymbol([F/N|Ds],S) :-
	write(S,'denotes_symbol('),
	writeq(S,F),
	writeAnonArgs(N,S),
	write(S,').'),
	nl(S),
	writeSymbol(Ds,S).
	
writeAnonArgs(0,_).
writeAnonArgs(N,S) :-
	N>0,
	write(S,'(_'),
	N1 is N-1,
	writeNArgs(N1,S),
	write(S,')').

writeNArgs(0,_).
writeNArgs(N,S) :-
	N>0,
	N1 is N-1,
	write(S,',_'),
	writeNArgs(N1,S).
	
getDenotes(DFTA,Sigma) :-
	getAllFunctors(DFTA,Fs),
	makeset(Fs,Sigma).
	
getAllFunctors([],[]).
getAllFunctors([(L->_)|Ds],[F/N|Fs]) :-
	functor(L,F,N),
	getAllFunctors(Ds,Fs).