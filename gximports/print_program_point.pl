:- module(print_program_point,
     [print_program_point_error/5, print_filter_error/5, print_program_point_and_filter_error/6,
      print_program_point/1, convert_program_point_term/3,
      abort_specialization/0]).
	
% a bit of a hack: the include below allows the module to be used also in logen and not just
% imported into the gx files; however: one will always have the XML format
% independently whether the --xml flag is set or not
% TO DO: fix this.
:- include(reportgen_xml).

print_program_point_and_filter_error(Message,ErrType,Problem,Info,PP,FilterPP2) :-
  write(user_error,pp_filter_err(Problem,PP,FilterPP2)),nl(user_error),
  convert_program_point_term(PP,PPT,OtherTags1),!,
  convert_filter_term(FilterPP2,OtherTags2),
  %writeq(user_error,error([message(Message),type(ErrType),problem(Problem),PPT,PPT2|Info])), nl(user_error),
  append(OtherTags2,Info,Rest1),
  append(OtherTags1,Rest1,Rest2),
  (reportgen(user_error,error([message(Message),type(ErrType),problem(Problem),PPT|Rest2]))
    -> true ; write(user_error,' *** reportgen failed ***')),
  flush_output(user_error).
  
print_program_point_error(Message,ErrType,Problem,Info,PP) :-
 % write(user_error,pp_err(Problem,PP)),nl(user_error),
  convert_program_point_term(PP,PPT,OtherTags),!,
  %writeq(user_error,error([message(Message),type(ErrType),problem(Problem),PPT|Info])), nl(user_error),
  append(OtherTags,Info,Rest),
  (reportgen(user_error,error([message(Message),type(ErrType),problem(Problem),PPT|Rest]))
    -> true ; write(user_error,' *** reportgen failed ***')),
  flush_output(user_error).
print_program_point_error(Message,ErrType,Problem,Info,PP) :-
   write(user_error,'Could not convert PP to term'),nl(user_error),
   write(user_error,Message),nl(user_error),
   write(user_error,ErrType),nl(user_error),
   write(user_error,Problem),nl(user_error),
   write(user_error,Info),nl(user_error),
   write(user_error,PP),nl(user_error).
  
    
    
print_filter_error(Message,ErrType,Problem,Info,PP) :-
  convert_filter_term(PP,OtherTags),!,
  append(OtherTags,Info,Rest),
  (reportgen(user_error,error([message(Message),type(ErrType),problem(Problem)|Rest]))
    -> true ; write(user_error,' *** reportgen failed ***')),
  flush_output(user_error).
print_filter_error(Message,ErrType,Problem,Info,PP) :-
   write(user_error,'Could not convert Filter to term'),nl(user_error),
   write(user_error,Message),nl(user_error),
   write(user_error,ErrType),nl(user_error),
   write(user_error,Problem),nl(user_error),
   write(user_error,Info),nl(user_error),
   write(user_error,PP),nl(user_error).


print_program_point(PP) :-
  convert_program_point_term(PP,T,_),
  reportgen(user_error,T).


abort_specialization :-
   write(user_error,'*** ABORTING SPECIALIZATION ***'),nl(user),halt(1).
   
   
   
convert_filter_term(correctfilt(Corr,PP),[filter([correctfilt(Corr)|OtherFilterTags])]) :- !,
   getfiltert(PP,OtherFilterTags).
convert_filter_term(PP,[filter(OtherFilterTags)]) :-
   getfiltert(PP,OtherFilterTags).
   
   
getfiltert(filter(Pred,Arity),[predicate([arity(Arity)],Pred)]) :- !.
getfiltert(X,_) :- write(user_error,unknown_filter_term(X)), nl(user_error),fail.
  
/* convert the PP info constructed by the gx into a list form for XML printing */

convert_program_point_term(PP,programpoint(List),OtherTags) :-
   getppt(PP,List,OtherTags).
 
getppt(correctann(Corr,PP),[correctann(Corr)|PPRes],Rest) :-
   getppt(PP,PPRes,Rest).
getppt(correctfiltpp(Corr,FilterT,PP),PPRes,[filter([correctfilt(Corr)|OtherFilterTags])|Rest]) :-
   getppt(PP,PPRes,Rest),
   getfiltert(FilterT,OtherFilterTags).
getppt(unfold(Call,PP),[annotation(unfold),call(Call),path(Path)|RestPP],[]) :- !,
  getppt2(PP,Path,RestPP).
getppt(memo(Call,PP),[annotation(memo),call(Call),path(Path)|RestPP],[]) :- !,
  getppt2(PP,Path,RestPP).
getppt(call(Call,PP),[annotation(call),call(Call),path(Path)|RestPP],[]) :- !,
  getppt2(PP,Path,RestPP).
%getppt(filter(Pred,Arity),[],[filter([arity(Arity)],Pred)]) :- !,  <-- FILTERS are no longer PP
%  write(user_error,filter(Pred,Arity)).
getppt(propagates_bindings(Call,PP),[call(Call),path(Path)|Rest],[]) :- !,
 % write(user_error,propagates_bindings(PP)),
  getppt2(PP,Path,Rest).
getppt(propagated_onto(Call,PP),[call(Call),path(Path)|Rest],[]) :- !,
%  write(user_error,propagated_onto(PP)),
  getppt2(PP,Path,Rest).
getppt(X,_,_) :- write(user_error,unknown_ppt(X)), nl(user_error),fail.
  

getppt2(clause(GlobalNr,Pred,Arity,Local),[],[globalclause(GlobalNr),predicate([arity(Arity)],Pred),localclause(Local)]) :- !.
getppt2(ann(Ann,Nr,PP),FullPath,Rest) :- !,
  getppt2(PP,Path,Rest),
  append(Path,[point([arg(Nr)],Ann)],FullPath).
getppt2(PP,[],unknown(PP)) :- write(user_error,unknown_program_point(PP)),nl(user_error).


/* ---------------------------------------------------------------------- */

