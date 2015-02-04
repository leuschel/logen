:- module(bta, [bta_snv/2,bta_lnl/2]).

:- use_module(qa_logen).
%:- use_module(qa_logen_new).
:- use_module(snv).
:- use_module(lnl).
:- use_module(tp).

%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Two examples
%
% ?- bta_snv('logen_examples/match.ann',match(static,_)).

% Number of clauses: 9

% Finished reading logen_examples/match.ann

% denotes_./3
% match_query/2
% [match1_query/4]
% \==_query/2
% denotes_[]/1
% match1_unfold_ans/4
% match_unfold_ans/2

% Model:

% denotes_.(X1,X2,X3) :- 
%      [denotes_.(X1,nonvar,nonvar),denotes_.(X1,var,nonvar),denotes_.(nonvar,X1,nonvar),denotes_.(var,X1,nonvar),denotes_.(static,static,static)].
% match_query(X1,X2) :- 
%      [match_query(static,X1)].
% match1_query(X1,X2,X3,X4) :- 
%      [match1_query(static,X1,static,X1),match1_query(static,var,static,nonvar)].
% \==_query(X1,X2) :- 
%      [\==_query(static,X1)].
% denotes_[](X1) :- 
%      [denotes_[](static)].
% match1_unfold_ans(X1,X2,X3,X4) :- 
%      [match1_unfold_ans(static,var,static,nonvar),match1_unfold_ans(static,X1,static,X1)].
% match_unfold_ans(X1,X2) :- 
%      [match_unfold_ans(static,X1)].



% yes
% ?- bta_lnl('logen_examples/match.ann',match(list,_)).

% Number of clauses: 9

% Finished reading logen_examples/match.ann

% denotes_./3
% match_query/2
% [match1_query/4]
% \==_query/2
% denotes_[]/1
% match1_unfold_ans/4
% match_unfold_ans/2

% Model:

% denotes_.(X1,X2,X3) :- 
%      [denotes_.(X1,list,list),denotes_.(X1,nonlist,nonlist)].
% match_query(X1,X2) :- 
%      [match_query(list,X1)].
% match1_query(X1,X2,X3,X4) :- 
%      [match1_query(list,X1,list,X1)].
% \==_query(X1,X2) :- 
%      [\==_query(X1,X2)].
% denotes_[](X1) :- 
%      [denotes_[](list)].
% match1_unfold_ans(X1,X2,X3,X4) :- 
%      [match1_unfold_ans(list,X1,list,X1)].
% match_unfold_ans(X1,X2) :- 
%      [match_unfold_ans(list,X1)].



% yes
% ?- 


bta_snv(F,Q) :-
	functor(Q,P,N),
	functor(Q1,P,N),
	qaLogenTrans(F,Q1,QA),
	snvq(QA,Q,DQA),
	tpr(DQA).
	
bta_lnl(F,Q) :-
	functor(Q,P,N),
	functor(Q1,P,N),
	qaLogenTrans(F,Q1,QA),
	lnlq(QA,Q,DQA),
	tpr(DQA).