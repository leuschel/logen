


:- module('logen_create_types', [
				 convert_type/3,
				 get_types_from_file/2,
				 userdefined_type/1
				
				 ]).

/* This module should create the types from logen for the
filter propagation in the form:
[] -> list.
[dynamic|list] -> list.

example logen type:
:- filter
        eval(static, (type list(struct(/,[static,dynamic]))), dynamic).

*/

:- dynamic struct_name/2.
:- dynamic gen_sym_count/1.
:- op(750,xfx, '->').
:- op(500,xfx, '--->').
:- op(1150,fx, filter).
:- op(1150,fx, type).

:- dynamic filter_c/1.
:- dynamic type_c/1.


built_in_types([] -> list).
built_in_types([dynamic|list] -> list).
/*
built_in_types([] -> list(dynamic)).
built_in_types([dynamic|list] -> list(dynamic)).
built_in_types([] -> list(static)).
built_in_types([static|list] -> list(static)).
*/


print_built_ins(Out) :-
	built_in_types(T),
	portray_clause(Out,T),
	fail ; true.
	      	    	      
get_types_from_file(Filename,Out) :-
	reset_all,
	open(Filename, read, In),
	extract_types(In),
	print_built_ins(Out),
	process_user_types(Out),
	process_filters(Out),
	close(In).

process_user_types(Out) :-
	type_c(T--->Def),
	(assert_user_type(Out,T,Def) ->	    

	    true
	;
	    portray_clause(user_error, failed_adding_type(T,Def))
	),
	fail ;true.

process_filters(Out) :-
	filter_c(Filter),
	Filter =.. [_|Args],
	(save_types_l(Out,Args) ->
	    true
	;
	    portray_clause(user, failed_for(Filter))
	),
	fail ; true.



save_types_l(_,[]).
save_types_l(Out,[Fil|Fils]) :-
	convert_type(Out,Fil, _NewFil),
	save_types_l(Out,Fils).


extract_types(In) :-
	read_term(In, Term, []),
	(Term == end_of_file ->
	    true
	;
	    (Term = (:-filter(FilterDecl)) ->
		assert(filter_c(FilterDecl))
	    ;
		true
	    ),
	    (Term = (:-type(Decl)) ->
		assert(type_c(Decl))
	 
	    ;
		true
	    ),
	    
	    
	    extract_types(In)
	).

reset_all :-
	retractall(struct_name(_,_)),
	retractall(filter_c(_)),
	retractall(type_c(_)).
	

is_known_type(dynamic).
is_known_type(static).
%is_known_type(Type) :- struct_name(_,Type).
is_known_type(Type) :- userdefined_type(Type).

userdefined_type(Type) :- struct_name(_,Type).

convert_type(Out,(T1;T2),(NT1;NT2)) :-
	convert_type(Out,T1,NT1),
	convert_type(Out,T2,NT2).
	
convert_type(Out,type(T), NT) :- convert_type(Out,T,NT).
convert_type(_Out,T,T) :- is_known_type(T),!.



convert_type(Out,struct(Func, Types), StructName) :-
	Struct = struct(Func,Types),
	StructName = Struct, % just keep struct as name for now...
	assert(struct_name(Struct,StructName)),
	convert_type_l(Out,Types, NewTypes),	
	TypeDef =.. [Func|NewTypes],
	print_type(Out,TypeDef->StructName).

convert_type(Out,list(Type), Name) :-
	List = list(Type),
	Name = List,
	assert(struct_name(List,Name)),
	convert_type(Out,Type,TypeName),
	
	print_type(Out,([];[TypeName|Name])->Name).

	
	

	
convert_type_l(_,[],[]).
convert_type_l(Out,[T|Ts], [NT|NTs]):-
	convert_type(Out,T,NT),
	convert_type_l(Out,Ts,NTs).

assert_user_type(Out,Name, Def) :-
	assert(struct_name(Name,Name)),
	convert_type(Out,Def,NDef),
	print_type(Out,NDef->Name).


assert_type(_,Name,_Type) :-
	is_known_type(Name),!.

assert_type(Out,Name, Type):-
	convert_type(Out,Type, Def),
	print_type(Out,Def->Name).



print_type(Out,(Def;Def1)->Type) :-
	!,
	print_type(Out,Def->Type),
	print_type(Out,Def1->Type).
	
print_type(Out,Def->Type) :-		
	portray_clause(Out,Def->Type).


	


	



