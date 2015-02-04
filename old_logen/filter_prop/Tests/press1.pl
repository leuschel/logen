%here are the results of my implemention on the press1 
%benchmark of corsini et-al:
%
%(1) the propositional logic program (transformed by hand from their flat
%program).
%
%(2) the results of depth 1 abstraction of lfp(Tp) %implemented
%by bart march 19, 1991 and on the shelf since then.
%
%-------------------

solve_equation(X1 , X2 , X3 ) :-
 	iff(X1,X4,X7),
	iff(X4,X5,X6),
	iff(X7),
	iff(X8,X5,X6),
	iff(X11,true), 				%X11 = [],
	iff(X9,X10,X11),
	factorize( X8 , X2 , X9 ),
	remove_duplicates( X10 , X12 ),
	solve_factors( X12 , X2 , X3 ).
solve_equation(X1 , X2 , X3 ) :-
 	single_occurrence( X2 , X1 ),
	iff(X4,X5,X6),				%X4 =  [ X5 | X6 ] ,
	position( X2 , X1 , X4 ),
	maneuver_sides( X5 , X1 , X7 ),
	isolate( X6 , X7 , X3 ).
solve_equation(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	is_polynomial( X4 , X2 ),
	is_polynomial( X5 , X2 ),
	iff(X6,X4,X5),
	polynomial_normal_form( X6 , X2 , X7 ),
	solve_polynomial_equation( X7 , X2 , X3 ).
solve_equation(X1 , X2 , X3 ) :-
 	offenders( X1 , X2 , X4 ),
	multiple( X4 ),
	homogenize( X1 , X2 , X4 , X5 , X6 ),
	solve_equation( X5 , X6 , X7 ),
	solve_equation( X7 , X2 , X3 ).

factorize(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	iff(X3,X6,X7),
	iff(X8,X6,X9),
	factorize( X4 , X2 , X8 ),
	iff(X10,X9,X7),
	factorize( X5 , X2 , X10 ).
factorize(X1 , X2 , X3 ) :-
 	iff(X3,X4,X5),
	iff(X4,X1,X5),				%X4 =  [ X1 | X5 ] ,
	subterm( X2 , X1 ).
factorize(X1 , X2 , X3 ) :-
 	iff(X3,X4,X5),
	iff(X5,X4).				%X5 = X4.

solve_factors(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),			%X1 =  [ X4 | X5 ] ,
	iff(X7),
	iff(X6,X4,X7),
	solve_equation( X6 , X2 , X3 ).
solve_factors(X1 , X2 , X3 ) :-
	iff(X1,X4,X5),                  %X1 =  [ X4 | X5 ] ,
	solve_factors( X5 , X2 , X3 ).

single_occurrence(X1 , X2 ) :-
 	iff(X3),
	occurrence( X1 , X2 , X3 ).
single_occurrence(X1 , X2 ).               % :- .   ?

maneuver_sides(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X5),
	iff(X3,X4,X5).
maneuver_sides(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X5),
	iff(X3,X5,X4).

isolate(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),                  %X1 =  [ X4 | X5 ] ,
	isolax( X4 , X2 , X6 ),
	isolate( X5 , X6 , X3 ).
isolate(X1 , X2 , X3 ) :-
 	iff(X1,true),			%X1 = [],
	iff(X3,X2).			%X3 = X2.

isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X7),
	iff(X4,X5,X6),
	iff(X3,X5,X8),
	iff(X8,X7,X6).
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X7),
	iff(X4,X5,X6),
	iff(X3,X6,X8),
	iff(X8,X7,X5).
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X7),
	iff(X4,X5,X6),
	iff(X3,X5,X8),
	iff(X8,X7,X6).
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X7),
	iff(X4,X5,X6),
	iff(X3,X6,X8),
	iff(X8,X5,X7).
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X7),
	iff(X4,X5,X6),
	iff(X3,X5,X8),
	iff(X8,X7,X6),
	iff(X9),
	iff(true,X6,X9).			%X6 <> X9.
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X7),
	iff(X4,X5,X6),
	iff(X3,X6,X8),
	iff(X8,X7,X5),
	iff(X9),
	iff(true,X5,X9).                 %X5 <> X9.
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X7),
	iff(X4,X5,X6),
	iff(X3,X5,X8),
	iff(X8,X7,X6),
	iff(X9),
	iff(true,X6,X9).                 %X6 <> X9.
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X7),
	iff(X4,X5,X6),
	iff(X3,X6,X8),
	iff(X8,X5,X7),
	iff(X9),
	iff(true,X7,X9).                 %X7 <> X9.
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X7),
	iff(X4,X5,X6),
	iff(X3,X5,X8),
	iff(X8,X7,X6).
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X7),
	iff(X4,X5,X6),
	iff(X3,X6,X8),
	iff(X8,X9,X7),
	iff(X9,X5).
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X6),
	iff(X4,X5),
	iff(X3,X5,X7),
	iff(X7,X6).
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X6),
	iff(X4,X5),
	iff(X3,X5,X7),
	iff(X7,X8,X9),
	iff(X8),
	iff(X9,X6).
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X6),
	iff(X4,X5),
	iff(X3,X5,X7),
	iff(X7,X6).
isolax(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X6),
	iff(X4,X5),
	iff(X3,X5,X7),
	iff(X7,X6).

is_polynomial(X1 , X2 ) :-
 	iff(X2,X1).			%X2 = X1.
is_polynomial(X1 , X2 ) :-
 	my_constant( X1 ).
is_polynomial(X1 , X2 ) :-
 	iff(X1,X3,X4),
	is_polynomial( X3 , X2 ),
	is_polynomial( X4 , X2 ).
is_polynomial(X1 , X2 ) :-
 	iff(X1,X3,X4),
	is_polynomial( X3 , X2 ),
	is_polynomial( X4 , X2 ).
is_polynomial(X1 , X2 ) :-
 	iff(X1,X3,X4),
	is_polynomial( X3 , X2 ),
	is_polynomial( X4 , X2 ).
is_polynomial(X1 , X2 ) :-
 	iff(X1,X3,X4),
	is_polynomial( X3 , X2 ),
	my_constant( X4 ).
is_polynomial(X1 , X2 ) :-
 	iff(X1,X3,X4),
	natural_number( X4 ),
	is_polynomial( X3 , X2 ).

natural_number(X1 ) :-
 	iff(X2),
	iff(true,X1,X2).		%X1 > X2.

polynomial_normal_form(X1 , X2 , X3 ) :-
 	polynomial_form( X1 , X2 , X4 ),
	remove_zero_terms( X4 , X3 ).

polynomial_form(X1 , X2 , X3 ) :-
 	iff(X2,X1),			%X2 = X1,
	iff(X3,X4,X7),			%X3 =  [ X4 | X7 ] ,
	iff(X4,X5,X6),
	iff(X5),
	iff(X6),
	iff(X7,true).			%X7 = [].
polynomial_form(X1 , X2 , X3 ) :-
 	iff(X1,X2,X4),
	iff(X3,X5,X7),			%X3 =  [ X5 | X7 ] ,
	iff(X5,X6,X4),
	iff(X6),
	iff(X7,true).			%X7 = [].
polynomial_form(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	polynomial_form( X4 , X2 , X6 ),
	polynomial_form( X5 , X2 , X7 ),
	add_polynomials( X6 , X7 , X3 ).
polynomial_form(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	polynomial_form( X4 , X2 , X6 ),
	polynomial_form( X5 , X2 , X7 ),
	subtract_polynomials( X6 , X7 , X3 ).
polynomial_form(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	polynomial_form( X4 , X2 , X6 ),
	polynomial_form( X5 , X2 , X7 ),
	multiply_polynomials( X6 , X7 , X3 ).
polynomial_form(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	polynomial_form( X4 , X2 , X6 ),
	binomial( X6 , X5 , X3 ).
polynomial_form(X1 , X2 , X3 ) :-
 	iff(X3,X4,X6),			%X3 =  [ X4 | X6 ] ,
	iff(X4,X1,X5),
	iff(X5),
	iff(X6,true),			%X6 = [],
	free_of( X2 , X1 ).

remove_zero_terms(X1 , X2 ) :-
 	iff(X1,X3,X6),			%X1 =  [ X3 | X6 ] ,
	iff(X3,X4,X5),
	iff(X4),
	remove_zero_terms( X6 , X2 ).
remove_zero_terms(X1 , X2 ) :-
 	iff(X1,X3,X6),                  %X1 =  [ X3 | X6 ] ,
	iff(X3,X4,X5),
	iff(X2,X7,X8),			%X2 =  [ X7 | X8 ] ,
	iff(X7,X4,X5),
	iff(X9),
	iff(true,X4,X9),		%X4 <> X9,
	remove_zero_terms( X6 , X8 ).
remove_zero_terms(X1 , X2 ) :-
 	iff(true,X1,X2).		%X1 = [],
					%X2 = [].

add_polynomials(X1 , X2 , X3 ) :-
 	iff(X1,true),			%X1 = [],
	iff(X3,X2).			%X3 = X2.
add_polynomials(X1 , X2 , X3 ) :-
 	iff(X2,true),                   %X2 = [],
	iff(X3,X1).                     %X3 = X1.
add_polynomials(X1 , X2 , X3 ) :-
 	iff(X1,X4,X7),                  %X1 =  [ X4 | X7 ] ,
	iff(X4,X5,X6),
	iff(X2,X8,X11),			%X2 =  [ X8 | X11 ] ,
	iff(X8,X9,X10),
	iff(X3,X12,X13),		%X3 =  [ X12 | X13 ] ,
	iff(X12,X5,X6),
	iff(true,X6,X10),		%X6 > X10,
	iff(X15,X9,X10),
	iff(X14,X15,X11),		%X14 =  [ X15 | X11 ] ,
	add_polynomials( X7 , X14 , X13 ).
add_polynomials(X1 , X2 , X3 ) :-
 	iff(X1,X4,X7),                  %X1 =  [ X4 | X7 ] ,
	iff(X4,X5,X6),
	iff(X2,X8,X11),                 %X2 =  [ X8 | X11 ] ,
	iff(X8,X9,X10),
	iff(X3,X12,X14),                %X3 =  [ X12 | X14 ] ,
	iff(X12,X13,X6),
	iff(X6,X10),			%X6 =:= X10,  ???
	iff(X15,X5,X9),
	my_is(X13,X15),
	add_polynomials( X7 , X11 , X14 ).
add_polynomials(X1 , X2 , X3 ) :-
 	iff(X1,X4,X7),                  %X1 =  [ X4 | X7 ] ,
	iff(X4,X5,X6),
	iff(X2,X8,X11),                 %X2 =  [ X8 | X11 ] ,
	iff(X8,X9,X10),
	iff(X3,X12,X13),                %X3 =  [ X12 | X13 ] ,
	iff(X12,X9,X10),
	iff(true,X6,X10),               %X6 < X10,
	iff(X15,X5,X6),
	iff(X14,X15,X7),		%X14 =  [ X15 | X7 ] ,
	add_polynomials( X14 , X11 , X13 ).

subtract_polynomials(X1 , X2 , X3 ) :-
 	iff(X5),
	iff(X6),
	iff(X4,X5,X6),
	multiply_single( X2 , X4 , X7 ),
	add_polynomials( X1 , X7 , X3 ).

multiply_single(X1 , X2 , X3 ) :-
 	iff(X1,X4,X7),                  %X1 =  [ X4 | X7 ] ,
	iff(X4,X5,X6),
	iff(X2,X8,X9),
	iff(X3,X10,X13),                %X3 =  [ X10 | X13 ] ,
	iff(X10,X11,X12),
	iff(X14,X5,X8),
	my_is(X11,X14),			% ??
	iff(X15,X6,X9),
	my_is(X12,X15),
	iff(X16,X8,X9),
	multiply_single( X7 , X16 , X13 ).
multiply_single(X1 , X2 , X3 ) :-
 	iff(true,X1,X3).		%X1 = [],
					%X3 = [].

multiply_polynomials(X1 , X2 , X3 ) :-
 	iff(X1,X4,X7),                  %X1 =  [ X4 | X7 ] ,
	iff(X4,X5,X6),
	iff(X8,X5,X6),
	multiply_single( X2 , X8 , X9 ),
	multiply_polynomials( X7 , X2 , X10 ),
	add_polynomials( X9 , X10 , X3 ).
multiply_polynomials(X1 , X2 , X3 ) :-
 	iff(true,X1,X3).                %X1 = [],
	                                %X3 = [].

binomial(X1 , X2 , X3 ) :-
 	iff(X2),
	iff(X1,X3).	                %X3 = X1.

solve_polynomial_equation(X1 , X2 , X3 ) :-
 	iff(X3,X2,X4),
	iff(X4,X5),
	iff(X5,X6,X7),
	linear( X1 ),			% ?
	iff(X10),
	iff(X9,X7,X10),
	iff(X13),
	iff(X12,X6,X13),
	iff(X14,true),			%X14 = [],
	iff(X11,X12,X14),               %X11 =  [ X12 | X14 ] ,
	iff(X8,X9,X11),                 %X8 =  [ X9 | X11 ] ,
	pad( X1 , X8 ).
solve_polynomial_equation(X1 , X2 , X3 ) :-
 	quadratic( X1 ),
	iff(X7),
	iff(X5,X6,X7),
	iff(X11),
	iff(X9,X10,X11),
	iff(X15),
	iff(X13,X14,X15),
	iff(X16,true),			%X16 = [],
	iff(X12,X13,X16),		%X12 =  [ X13 | X16 ] ,
	iff(X8,X9,X12),                 %X8 =  [ X9 | X12 ] ,
	iff(X4,X5,X8),			%X4 =  [ X5 | X8 ] ,
	pad( X1 , X4 ),
	discriminant( X6 , X10 , X14 , X17 ),
	root( X2 , X6 , X10 , X14 , X17 , X3 ).

discriminant(X1 , X2 , X3 , X4 ) :-
 	iff(X7,X2),			%X7 = X2,
	iff(X6,X2,X7),
	iff(X10),
	iff(X9,X10,X1),
	iff(X8,X9,X3),
	iff(X5,X6,X8),
	my_is(X4,X5).

root(X1 , X2 , X3 , X4 , X5 , X6 ) :-
 	iff(X5),
	iff(X6,X1,X7),
	iff(X7,X8),
	iff(X8,X3,X9),
	iff(X9,X10,X2),
	iff(X10).
root(X1 , X2 , X3 , X4 , X5 , X6 ) :-
 	iff(X6,X1,X7),
	iff(X7,X8,X11),
	iff(X8,X9,X10),
	iff(X9,X3),
	iff(X10,X5),
	iff(X11,X12,X2),
	iff(X12),
	iff(X13),
	iff(true,X5,X13).		%X5 > X13.
root(X1 , X2 , X3 , X4 , X5 , X6 ) :-
 	iff(X6,X1,X7),
	iff(X7,X8,X11),
	iff(X8,X9,X10),
	iff(X9,X3),
	iff(X10,X5),
	iff(X11,X12,X2),
	iff(X12),
	iff(X13),
	iff(true,X5,X13).               %X5 > X13.

pad(X1 , X2 ) :-
 	iff(X1,X3,X6),			%X1 =  [ X3 | X6 ] ,
	iff(X3,X4,X5),
	iff(X2,X7,X8),			%X2 =  [ X7 | X8 ] ,
	iff(X7,X4,X5),
	pad( X6 , X8 ).
pad(X1 , X2 ) :-
 	iff(X2,X3,X6),                  %X2 =  [ X3 | X6 ] ,
	iff(X3,X4,X5),
	iff(X4),
	pad( X1 , X6 ).
pad(X1 , X2 ) :-
 	iff(true,X1,X2).		%X1 = [],
					%X2 = [].

linear(X1 ) :-
 	iff(X1,X2,X5),			%X1 =  [ X2 | X5 ] ,
	iff(X2,X3,X4),
	iff(X4).

quadratic(X1 ) :-
 	iff(X1,X2,X5),                  %X1 =  [ X2 | X5 ] ,
	iff(X2,X3,X4),
	iff(X4).

offenders(X1 , X2 , X3 ) :-
 	iff(X5,true),			%X5 = [],
	iff(X4,X1,X5),			%X4 =  [ X1 | X5 ] ,
	parse( X4 , X2 , X6 ),
	remove_duplicates( X6 , X3 ).

homogenize(X1 , X2 , X3 , X4 , X5 ) :-
 	reduced_term( X2 , X3 , X6 , X5 ),
	rewrite( X3 , X6 , X5 , X7 ),
	substitute( X1 , X7 , X4 ).

reduced_term(X1 , X2 , X3 , X4 ) :-
 	classify( X2 , X1 , X3 ),
	candidate( X3 , X2 , X1 , X4 ).

classify(X1 , X2 , X3 ) :-
 	iff(X3),
	exponential_offenders( X1 , X2 ).

exponential_offenders(X1 , X2 ) :-
 	iff(X1,X3,X6),                  %X1 =  [ X3 | X6 ] ,
	iff(X3,X4,X5),
	free_of( X2 , X4 ),
	subterm( X2 , X5 ),
	exponential_offenders( X6 , X2 ).
exponential_offenders(X1 , X2 ) :-
 	iff(X1,true).			%X1 = [].

candidate(X1 , X2 , X3 , X4 ) :-
 	iff(X1),
	iff(X4,X5,X3),
	base( X2 , X5 ),
	polynomial_exponents( X2 , X3 ).

base(X1 , X2 ) :-
 	iff(X1,X3,X5),                  %X1 =  [ X3 | X5 ] ,
	iff(X3,X2,X4),
	base( X5 , X2 ).
base(X1 , X2 ) :-
 	iff(X1,true).			%X1 = [].

polynomial_exponents(X1 , X2 ) :-
 	iff(X1,X3,X6),                  %X1 =  [ X3 | X6 ] ,
	iff(X3,X4,X5),
	is_polynomial( X5 , X2 ),
	polynomial_exponents( X6 , X2 ).
polynomial_exponents(X1 , X2 ) :-
 	iff(X1,true).                   %X1 = [].

substitute(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	iff(X3,X6,X7),
	substitute( X4 , X2 , X6 ),
	substitute( X5 , X2 , X7 ).
substitute(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	iff(X3,X6,X7),
	substitute( X4 , X2 , X6 ),
	substitute( X5 , X2 , X7 ).
substitute(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	iff(X3,X6,X7),
	substitute( X4 , X2 , X6 ),
	substitute( X5 , X2 , X7 ).
substitute(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	iff(X3,X6,X7),
	substitute( X4 , X2 , X6 ),
	substitute( X5 , X2 , X7 ).
substitute(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	iff(X3,X6,X5),
	my_integer( X5 ),			
	substitute( X4 , X2 , X6 ).
substitute(X1 , X2 , X3 ) :-
 	iff(X4,X1,X3),
	member( X4 , X2 ).
substitute(X1 , X2 , X3 ) :-
 	iff(X3,X1).			%X3 = X1.

rewrite(X1 , X2 , X3 , X4 ) :-
 	iff(X1,X5,X7),                  %X1 =  [ X5 | X6 ] ,
	iff(X4,X7,X9),			%X4 =  [ X7 | X9 ] ,
	iff(X7,X5,X8),
	homog_axiom( X2 , X5 , X3 , X8 ),
	rewrite( X6 , X2 , X3 , X9 ).
rewrite(X1 , X2 , X3 , X4 ) :-
 	iff(true,X1,X4).		%X1 = [],
					%X4 = [].

homog_axiom(X1 , X2 , X3 , X4 ) :-
 	iff(X1),
	iff(X2,X5,X6),
	iff(X6,X7,X8),
	iff(X3,X5,X8),
	iff(X4,X9,X7),
	iff(X9,X5,X8).
homog_axiom(X1 , X2 , X3 , X4 ) :-
 	iff(X1),
	iff(X2,X5,X6),
	iff(X6,X7),
	iff(X3,X5,X7),
	iff(X4,X8,X9),
	iff(X8),
	iff(X9,X5,X7).
homog_axiom(X1 , X2 , X3 , X4 ) :-
 	iff(X1),
	iff(X2,X5,X6),
	iff(X6,X7,X8),
	iff(X3,X5,X7),
	iff(X4,X9,X7),
	iff(X9,X10,X5),
	iff(X10,X5,X8).

subterm(X1 , X2 ) :-
 	iff(X2,X1).			%X2 = X1.
subterm(X1 , X2 ) :-
 	my_compound( X2 ),
	my_functor( X2 , X3 , X4 ),      %%%
	subterm( X4 , X1 , X2 ).

my_compound(X1 ) :-
 	iff(X1,X2,X3).
my_compound(X1 ) :-
 	iff(X1,X2,X3).
my_compound(X1 ) :-
 	iff(X1,X2,X3).
my_compound(X1 ) :-
 	iff(X1,X2,X3).
my_compound(X1 ) :-
 	iff(X1,X2,X3).
my_compound(X1 ) :-
 	iff(X1,X2,X3).
my_compound(X1 ) :-
 	iff(X1,true).
my_compound(X1 ) :-
 	iff(X1,true).

my_constant(X1 ) :-
 	iff(X1).
my_constant(X1 ) :-
 	iff(X1).

my_integer(X1 ) :-
 	iff(X1).
my_integer(X1 ) :-
 	iff(X1).

member(X1 , X2 ) :-
 	iff(X2,X1,X3).			%X2 =  [ X1 | X3 ] .
member(X1 , X2 ) :-
 	iff(X2,X3,X4),			%X2 =  [ X3 | X4 ] ,
	member( X1 , X4 ).

my_functor(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2),
	iff(X3).
my_functor(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2),
	iff(X3).
my_functor(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2),
	iff(X3).
my_functor(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2),
	iff(X3).
my_functor(X1 , X2 , X3 ) :-
 	iff(X1,X4),
	iff(X2),
	iff(X3).
my_functor(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	iff(X2),
	iff(X3).
my_functor(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	iff(X2),
	iff(X3).
my_functor(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	iff(X2),
	iff(X3).
my_functor(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	iff(X2),
	iff(X3).
my_functor(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),
	iff(X2),
	iff(X3).

my_arg(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X3,X4).
my_arg(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X3,X4).
my_arg(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X3,X4).
my_arg(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X3,X4).
my_arg(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X3,X4).
my_arg(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X3,X4).
my_arg(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X3).
my_arg(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X3).
my_arg(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X3).
my_arg(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X3).
my_arg(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X3).
my_arg(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2,X4,X3).

subterm(X1 , X2 , X3 ) :-
 	my_arg( X1 , X3 , X4 ),			%?
	subterm( X2 , X4 ).
subterm(X1 , X2 , X3 ) :-
 	iff(X4),
	iff(true,X1,X4),			%X1 > X4,
	iff(X7),
	iff(X6,X1,X7),
	my_is(X5,X6),
	subterm( X5 , X2 , X3 ).

position(X1 , X2 , X3 ) :-
 	iff(X2,X1),			%X2 = X1,
	iff(X3,true).			%X3 = [].
position(X1 , X2 , X3 ) :-
 	my_compound( X2 ),
	my_functor( X2 , X4 , X5 ),    %%%
	position( X5 , X1 , X2 , X3 ).

position(X1 , X2 , X3 , X4 ) :-
 	iff(X4,X1,X5),			%X4 =  [ X1 | X5 ] ,
	my_arg( X1 , X3 , X6 ),
	position( X2 , X6 , X5 ).
position(X1 , X2 , X3 , X4 ) :-
 	iff(X5),
	iff(true,X1,X5),                        %X1 > X5,
	iff(X8),
	iff(X7,X1,X8),
	my_is(X6,X7),
	position( X6 , X2 , X3 , X4 ).

parse(X1 , X2 , X3 ) :-
 	iff(X1,X4,X7),                  %X1 =  [ X4 | X7 ] ,
	iff(X4,X5,X6),
	iff(X9,X6,X7),			%X9 =  [ X6 | X7 ] ,
	iff(X8,X5,X9),			%X8 =  [ X5 | X9 ] ,
	parse( X8 , X2 , X3 ).
parse(X1 , X2 , X3 ) :-
 	iff(X1,X4,X7),                  %X1 =  [ X4 | X7 ] ,
	iff(X4,X5,X6),
	iff(X9,X6,X7),                  %X9 =  [ X6 | X7 ] ,
	iff(X8,X5,X9),                  %X8 =  [ X5 | X9 ] ,
	parse( X8 , X2 , X3 ).
parse(X1 , X2 , X3 ) :-
 	iff(X1,X4,X7),                  %X1 =  [ X4 | X7 ] ,
	iff(X4,X5,X6),
	iff(X9,X6,X7),                  %X9 =  [ X6 | X7 ] ,
	iff(X8,X5,X9),                  %X8 =  [ X5 | X9 ] ,
	parse( X8 , X2 , X3 ).
parse(X1 , X2 , X3 ) :-
 	iff(X1,X4,X7),                  %X1 =  [ X4 | X7 ] ,
	iff(X4,X5,X6),
	iff(X9,X6,X7),                  %X9 =  [ X6 | X7 ] ,
	iff(X8,X5,X9),                  %X8 =  [ X5 | X9 ] ,
	parse( X8 , X2 , X3 ).
parse(X1 , X2 , X3 ) :-
 	iff(X1,X4,X7),                  %X1 =  [ X4 | X7 ] ,
	iff(X4,X5,X6),
	my_integer( X6 ),
	iff(X8,X5,X7),                  %X8 =  [ X5 | X7 ] ,
	parse( X8 , X2 , X3 ).
parse(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),                  %X1 =  [ X4 | X5 ] ,
	parse( X5 , X2 , X3 ).
parse(X1 , X2 , X3 ) :-
 	iff(X1,X4,X5),                  %X1 =  [ X4 | X5 ] ,
	iff(X3,X4,X6),			%X3 =  [ X4 | X6 ] ,
	parse( X5 , X2 , X6 ).
parse(X1 , X2 , X3 ) :-
 	iff(true,X1,X3).		%X1 = [],
					%X3 = [].

free_of(X1 , X2 ) :-
 	occurrence( X1 , X2 , X3 ),
	iff(X4),
	iff(X3,X4).			%X3 = X4.

occurrence(X1 , X2 , X3 ) :-
 	iff(X2,X1),			%X2 = X1,
	iff(X3).
occurrence(X1 , X2 , X3 ) :-
 	my_compound( X2 ),
	my_functor( X2 , X4 , X5 ), %%%
	iff(X6),
	occurrence( X5 , X1 , X2 , X6 , X3 ).
occurrence(X1 , X2 , X3 ) :-
 	iff(X3).

occurrence(X1 , X2 , X3 , X4 , X5 ) :-
 	iff(X6),
	iff(true,X1,X6),		%X1 > X6,
	my_arg( X1 , X3 , X7 ),
	occurrence( X2 , X7 , X8 ),
	iff(X10,X8,X4),
	my_is(X9,X10),
	iff(X13),
	iff(X12,X1,X13),
	my_is(X11,X12),
	occurrence( X11 , X2 , X3 , X9 , X5 ).
occurrence(X1 , X2 , X3 , X4 , X5 ) :-
 	iff(X1),
	iff(X5,X4).			%X5 = X4.

multiple(X1 ) :-
 	iff(X1,X2,X3), 			%X1 =  [ X2 | X3 ] ,
	iff(X3,X4,X5).			%X3 =  [ X4 | X5 ] .

remove_duplicates(X1 , X2 ) :-
 	iff(true,X1,X2).                %X1 = [],
	                                %X2 = [].
remove_duplicates(X1 , X2 ) :-
 	iff(X1,X3,X4),                  %X1 =  [ X3 | X4 ] ,
	iff(X2,X3,X5),                  %X2 =  [ X3 | X5 ] ,
	remove_duplicates( X4 , X5 ).
remove_duplicates(X1 , X2 ) :-
 	iff(X1,X3,X4),                  %X1 =  [ X3 | X4 ] ,
	member( X3 , X4 ),
	remove_duplicates( X4 , X2 ).

test_press(X1 , X2 ) :-
 	equation( X1 , X3 , X4 ),
	solve_equation( X3 , X4 , X2 ).

equation(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2),
	iff(X3).
equation(X1 , X2 , X3 ) :-
 	iff(X1),
	iff(X2),
	iff(X3).



my_is(X,Y) :- iff(true,X,Y).


     iff(true).

     iff(true,true).
     iff(false,false).

     iff(true,true,true).
     iff(false,true,false).
     iff(false,false,true).
     iff(false,false,false).


