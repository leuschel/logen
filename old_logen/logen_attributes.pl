:- module(logen_attributes,[setattr/3, delattr/2, getattr/3, del_all_attr/2]).

% ____________________________________________________________
%
% sorry, I can't resist introducing some object-orientation :-)
%
%   getattr(Object, Key, Value)
%
% use setattr to replace the associated Value.

:- dynamic getattr/3.
setattr(Object, Key, NewValue) :-
        delattr(Object, Key),
        assert(getattr(Object, Key, NewValue)).
delattr(Object, Key) :-
        retractall(getattr(Object, Key, _)).
        

del_all_attr(Object) :-
     retractall(getattr(Object, _, _)).