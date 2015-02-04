:- module(intersectiontypes,[generalise_using_intersection_type/3,
                             filter_intersection_type/4]).

:- use_module(library(lists)).
:- use_package(.(sicsignore)).
%:- use_module('intersection/filter.pl').
:- use_module('filter_prop/logen/filter.pl').

test(R) :-
  intersectiontypes:generalise_using_intersection_type(
  [[dynamic,list],[dynamic,list,static]],
  [a,a,b],R).

generalise_using_intersection_type(X,_Arg,_GenArg) :- 
   can_be_var(X),!,
   print_message(informational,can_be_var(X)).
generalise_using_intersection_type(X,Arg,Arg) :- 
   is_definitely_static(X),!,
   print_message(informational,is_definitely_static(X)).
generalise_using_intersection_type(X,Arg,GenArg) :- 
    var(Arg),!,print(error_var(X,Arg,GenArg)),nl.
generalise_using_intersection_type(X,Arg,GenArg) :- 
   functor(Arg,F,_N),
   get_sub_types(X,Arg,SubArgs,SubTypes),
   l_generalise_using_intersection_type(SubTypes,SubArgs,GenSubArgs),
   GenArg =.. [F|GenSubArgs].


%%%%% print_message for caio

ciao(print_message( informational , A )) :-
	message( note , A ).

ciao(print_message( A , M )) :-
	message( A , M ).
	

l_generalise_using_intersection_type([],[],[]).
l_generalise_using_intersection_type([Type|RestTypes],[H|T],[GH|GT]) :-
   generalise_using_intersection_type(Type,H,GH),
   l_generalise_using_intersection_type(RestTypes,T,GT).
   
% filter_intersection_type(T,Argument,InArgList,OutArgList) 

filter_intersection_type(T,Argument,InArgList,[Argument|InArgList]) :- 
   can_be_var(T),!.
filter_intersection_type(T,_Argument,InArgList,InArgList)  :- 
   is_definitely_static(T),!.
filter_intersection_type(_T,Argument,InArgList,[Argument|InArgList]) :-
   var(Argument), !,print_message(warning,variable_in_filter_intersection_type).
filter_intersection_type(T,Argument,InArgList,OutArgList) :- 
   get_sub_types(T,Argument,SubArgs,SubTypes),
   l_filter_intersection_type(SubTypes,SubArgs,InArgList,OutArgList).
filter_intersection_type(_T,Argument,InArgList,[Argument|InArgList]).

  
  
l_filter_intersection_type([],[],Args,Args).
l_filter_intersection_type([Type|RT],[Arg|RA],InArgList,OutArgList) :-
    filter_intersection_type(Type,Arg,InArgList,IntList),
   print_message(informational,exit_filter_intersection_type(Type,Arg,InArgList,IntList)),
    l_filter_intersection_type(RT,RA,IntList,OutArgList).
     
% HOOKS for John & Kim
% ---------------------
   
%can_be_var(X) :- member([var],X).

%is_definitely_static(X) :- X=[[static]].

%definetely_nonvar(X) :- .,..

% get_sub_types(IntersectionType, Term,  SubArguments(out), Types for SubArguments(out))
% --------------------------------------------------------------------------------------
%get_sub_types([[list,static],[list,nonvar]],[],[],[]) :- !.
%get_sub_types([[list,static],[list,nonvar]],[H|T],[H,T],
%                        [[[var]],[[list,static],[list,nonvar]]]) :- !.
%get_sub_types([[list,static],[list,nonvar]],P,Args,Types) :- 
%    P =.. [F|Args],make_dynamic(Args,Types).
    
    
%make_dynamic([],[]).
%make_dynamic([_|T], [ [[var]] | DT]) :- make_dynamic(T,DT).

