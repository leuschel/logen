:- module(saver, [saver_to_file/2,
		  saver_to_file/0,
		  portray_spec/1,
		  portray_memo/1]).

:- use_module('../logen_post').


%% Generic mapper function
%map_pp(Pattern, Call) :-
%	Pattern,
%	Call,
%	fail ; true.


portray_spec(Out) :- map_pp(spec_clause(C), portray_clause(Out,C)).
portray_memo(Out) :- map_pp(memo_clause(C), portray_clause(Out,C)).



saver_to_file :-
	current_spec(Spec),
	current_memo(Memo),
	saver_to_file(Spec,Memo).


saver_to_file(Spec,Memo) :-
	open(Spec,write,SpecStream),
	portray_spec(SpecStream),
	close(SpecStream),
	open(Memo, write, MemoStream),
	portray_memo(MemoStream),
	close(MemoStream).


