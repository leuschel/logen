
  solve_equation(A*B=0,X,Solution) :- 
        factorize(A*B,X,Factors - []),
        remove_duplicates(Factors,Factors1),
        solve_factors(Factors1,X,Solution).

     solve_equation(Equation,X,Solution) :-
        single_occurrence(X,Equation), 
        position(X,Equation,[Side|Position]),
        maneuver_sides(Side,Equation,Equation1),
        isolate(Position,Equation1,Solution).
 
     solve_equation(Lhs=Rhs,X,Solution) :-
        is_polynomial(Lhs,X),
        is_polynomial(Rhs,X),
        polynomial_normal_form(Lhs-Rhs,X,PolyForm),
        solve_polynomial_equation(PolyForm,X,Solution).

     solve_equation(Equation,X,Solution) :-
        offenders(Equation,X,Offenders),
        multiple(Offenders),
        homogenize(Equation,X,Offenders,Equation1,X1),
        solve_equation(Equation1,X1,Solution1),
        solve_equation(Solution1,X,Solution).

     factorize(A*B,X,Factors - Rest) :-
                factorize(A,X,Factors - Factors1),
                factorize(B,X,Factors1 - Rest),
                factorize(A,X,Factors - Factors1).
     factorize(C,X,[C|Factors] - Factors) :-
                subterm(X,C).
     factorize(C,X,Factors - Factors).

     solve_factors([Factor|Factors],X,Solution) :-
        solve_equation(Factor=0,X,Solution).
     solve_factors([Factor|Factors],X,Solution) :-
        solve_factors(Factors,X,Solution).

     single_occurrence(Subterm,Term) :-
        occurrence(Subterm,Term,1).

     maneuver_sides(1,Lhs = Rhs,Lhs = Rhs).
     maneuver_sides(2,Lhs = Rhs,Rhs = Lhs).

     isolate([N|Position],Equation,IsolatedEquation) :- 
        isolax(N,Equation,Equation1), 
        isolate(Position,Equation1,IsolatedEquation).
     isolate([],Equation,Equation).

isolax(1,Term1+Term2 = Rhs,Term1 = Rhs-Term2).        
isolax(2,Term1+Term2 = Rhs,Term2 = Rhs-Term1).         

isolax(1,Term1-Term2 = Rhs,Term1 = Rhs+Term2).        
isolax(2,Term1-Term2 = Rhs,Term2 = Term1-Rhs).         

isolax(1,Term1*Term2 = Rhs,Term1 = Rhs/Term2) :-
   Term2 \== 0.
isolax(2,Term1*Term2 = Rhs,Term2 = Rhs/Term1) :-
   Term1 \== 0.

isolax(1,Term1/Term2 = Rhs,Term1 = Rhs*Term2) :-
   Term2 \== 0.
isolax(2,Term1/Term2 = Rhs,Term2 = Term1/Rhs) :-
   Rhs \== 0. 

isolax(1,Term1^Term2 = Rhs,Term1 = Rhs^(Term2)).
isolax(2,Term1^Term2 = Rhs,Term2 = log(base(Term1),Rhs)). 

isolax(1,sin(U) = V,U = arcsin(V)).                
isolax(1,sin(U) = V,U = 180 - arcsin(V)).        
isolax(1,cos(U) = V,U = arccos(V)).                
isolax(1,cos(U) = V,U = arccos(V)).                

        is_polynomial(X,X).
        is_polynomial(Term,X) :-
                constant(Term).
        is_polynomial(Term1+Term2,X) :-
                is_polynomial(Term1,X),
                is_polynomial(Term2,X).
        is_polynomial(Term1-Term2,X) :-
                is_polynomial(Term1,X),
                is_polynomial(Term2,X).
        is_polynomial(Term1*Term2,X) :-
                is_polynomial(Term1,X),
                is_polynomial(Term2,X).
        is_polynomial(Term1/Term2,X) :-
                is_polynomial(Term1,X),
                constant(Term2).
        is_polynomial(Term^N,X) :-
                natural_number(N),is_polynomial(Term,X).

        natural_number(N) :- N > 0.

     polynomial_normal_form(Polynomial,X,NormalForm) :-
        polynomial_form(Polynomial,X,PolyForm),
        remove_zero_terms(PolyForm,NormalForm).

     polynomial_form(X,X,[p(1,1)]).
     polynomial_form(X^N,X,[p(1,N)]).
     polynomial_form(Term1+Term2,X,PolyForm) :-
        polynomial_form(Term1,X,PolyForm1),
        polynomial_form(Term2,X,PolyForm2),
        add_polynomials(PolyForm1,PolyForm2,PolyForm).
     polynomial_form(Term1-Term2,X,PolyForm) :-
        polynomial_form(Term1,X,PolyForm1),
        polynomial_form(Term2,X,PolyForm2),
        subtract_polynomials(PolyForm1,PolyForm2,PolyForm).
     polynomial_form(Term1*Term2,X,PolyForm) :-
        polynomial_form(Term1,X,PolyForm1),
        polynomial_form(Term2,X,PolyForm2),
        multiply_polynomials(PolyForm1,PolyForm2,PolyForm).
     polynomial_form(Term^N,X,PolyForm) :-
        polynomial_form(Term,X,PolyForm1),
        binomial(PolyForm1,N,PolyForm).
     polynomial_form(Term,X,[p(Term,0)]) :-
        free_of(X,Term).

   remove_zero_terms([p(0,N)|Poly],Poly1) :-
         remove_zero_terms(Poly,Poly1).
   remove_zero_terms([p(C,N)|Poly],[p(C,N)|Poly1]) :-
        C \== 0, remove_zero_terms(Poly,Poly1).
   remove_zero_terms([],[]).

   add_polynomials([],Poly,Poly).
   add_polynomials(Poly,[],Poly).
   add_polynomials([p(Ai,Ni)|Poly1],[p(Aj,Nj)|Poly2],[p(Ai,Ni)|Poly]) :-
        Ni > Nj, add_polynomials(Poly1,[p(Aj,Nj)|Poly2],Poly).
   add_polynomials([p(Ai,Ni)|Poly1],[p(Aj,Nj)|Poly2],[p(A,Ni)|Poly]) :-
        Ni =:= Nj, A is Ai+Aj, add_polynomials(Poly1,Poly2,Poly).
   add_polynomials([p(Ai,Ni)|Poly1],[p(Aj,Nj)|Poly2],[p(Aj,Nj)|Poly]) :-
        Ni < Nj, add_polynomials([p(Ai,Ni)|Poly1],Poly2,Poly).

   subtract_polynomials(Poly1,Poly2,Poly) :-
        multiply_single(Poly2,p(1,0),Poly3),
           add_polynomials(Poly1,Poly3,Poly).

   multiply_single([p(C1,N1)|Poly1],p(C,N),[p(C2,N2)|Poly]) :-
        C2 is C1*C, N2 is N1+N, multiply_single(Poly1,p(C,N),Poly).
   multiply_single([],Factor,[]).

   multiply_polynomials([p(C,N)|Poly1],Poly2,Poly) :-
        multiply_single(Poly2,p(C,N),Poly3),
        multiply_polynomials(Poly1,Poly2,Poly4),
           add_polynomials(Poly3,Poly4,Poly).
   multiply_polynomials([],P,[]).

   binomial(Poly,1,Poly).
        
   solve_polynomial_equation(PolyEquation,X,X = -B/A) :-
           linear(PolyEquation), 
        pad(PolyEquation,[p(A,1),p(B,0)]). 
   solve_polynomial_equation(PolyEquation,X,Solution) :-
        quadratic(PolyEquation),
        pad(PolyEquation,[p(A,2),p(B,1),p(C,0)]),
        discriminant(A,B,C,Discriminant),
        root(X,A,B,C,Discriminant,Solution).

        discriminant(A,B,C,D) :- D is B*B - 4*A*C.

        root(X,A,B,C,0,X= -B/(2*A)).
        root(X,A,B,C,D,X= (-B+sqrt(D))/(2*A)) :- D > 0.
        root(X,A,B,C,D,X= (-B-sqrt(D))/(2*A)) :- D > 0.

   pad([p(C,N)|Poly],[p(C,N)|Poly1]) :-
         pad(Poly,Poly1).
   pad(Poly,[p(0,N)|Poly1]) :-
        pad(Poly,Poly1).
   pad([],[]).

   linear([p(Coeff,1)|Poly]).        quadratic([p(Coeff,2)|Poly]).

     offenders(Equation,X,Offenders) :-
        parse([Equation],X,Offenders1),
        remove_duplicates(Offenders1,Offenders).

     homogenize(Equation,X,Offenders,Equation1,X1) :-
        reduced_term(X,Offenders,Type,X1),
        rewrite(Offenders,Type,X1,Substitutions),
        substitute(Equation,Substitutions,Equation1).

     reduced_term(X,Offenders,Type,X1) :-
        classify(Offenders,X,Type),
        candidate(Type,Offenders,X,X1).

    classify(Offenders,X,exponential) :-
        exponential_offenders(Offenders,X).

     exponential_offenders([A^B|Offs],X) :-
        free_of(X,A), subterm(X,B), exponential_offenders(Offs,X).
     exponential_offenders([],X).

     candidate(exponential,Offenders,X,A^X) :-
        base(Offenders,A), polynomial_exponents(Offenders,X).

     base([A^B|Offs],A) :- base(Offs,A).
     base([],A).

     polynomial_exponents([A^B|Offs],X) :-
        is_polynomial(B,X), polynomial_exponents(Offs,X).
     polynomial_exponents([],X).


     substitute(A+B,Subs,NewA+NewB) :-
         substitute(A,Subs,NewA), substitute(B,Subs,NewB).     
     substitute(A*B,Subs,NewA*NewB) :-
         substitute(A,Subs,NewA), substitute(B,Subs,NewB).     
     substitute(A-B,Subs,NewA-NewB) :-
         substitute(A,Subs,NewA), substitute(B,Subs,NewB).     
     substitute(A=B,Subs,NewA=NewB) :-
         substitute(A,Subs,NewA), substitute(B,Subs,NewB).     
     substitute(A^B,Subs,NewA^B) :-
        integer(B), substitute(A,Subs,NewA).
     substitute(A,Subs,B) :-
        member(A=B,Subs).
     substitute(A,Subs,A).

 
     rewrite([Off|Offs],Type,X1,[Off=Term|Rewrites]) :-
        homog_axiom(Type,Off,X1,Term),
        rewrite(Offs,Type,X1,Rewrites).
     rewrite([],Type,X,[]).



     homog_axiom(exponential,A^(N*X),A^X,(A^X)^N).
     homog_axiom(exponential,A^(-X),A^X,1/(A^X)).
     homog_axiom(exponential,A^(X+B),A^X,A^B*A^X).

subterm(Term,Term).
subterm(Sub,Term) :-
        my_compound(Term), my_functor(Term,F,N), subterm(N,Sub,Term).


my_compound(X * Y).
my_compound(X - Y).
my_compound(X + Y).
my_compound(X / Y).
my_compound(X ^ Y).
my_compound(X = Y).

constant(1).
constant(0).
my_compound(x).
my_compound(y).

%integer(1).
%integer(2).

member(X,[X|Y]).
member(X,[F|T]) :-
   member(X,T).

my_functor(1,1,0).
my_functor(0,0,0).
my_functor(y,y,0).
my_functor(x,x,0).
my_functor(- A,a,1).
my_functor(- A,b,1).
my_functor(B - A,b,2).
my_functor(B - A,a,2).
my_functor(B + A,c,2).
my_functor(B + A,a,2).
my_functor(B * A,d,2).
my_functor(B * A,a,2).
my_functor(B / A,e,2).
my_functor(B = A,f,2).
my_functor(B / A,a,2).
my_functor(B = A,a,2).

my_arg(1,X + Y,X).
my_arg(1,X * Y,X).
my_arg(1,X / Y,X).
my_arg(1,X ^ Y,X).
my_arg(1,X - Y,X).
my_arg(1,X = Y,X).

my_arg(2,X + Y,Y).
my_arg(2,X * Y,Y).
my_arg(2,X / Y,Y).
my_arg(2,X ^ Y,Y).
my_arg(2,X - Y,Y).
my_arg(2,X = Y,Y).


subterm(N,Sub,Term) :-
   my_arg(N,Term,Arg), subterm(Sub,Arg).
subterm(N,Sub,Term) :-
        N > 0,
        N1 is N - 1,
        subterm(N1,Sub,Term).

position(Term,Term,[]).
position(Sub,Term,Path) :-
        my_compound(Term), my_functor(Term,F,N), position(N,Sub,Term,Path).

position(N,Sub,Term,[N|Path]) :-
   my_arg(N,Term,Arg), position(Sub,Arg,Path).
position(N,Sub,Term,Path) :- 
   N > 1, N1 is N-1, position(N1,Sub,Term,Path).


     parse([A+B|Y],X,L1) :-
         parse([A,B|Y],X,L1).
     parse([A-B|Y],X,L1) :-
         parse([A,B|Y],X,L1).
     parse([A=B|Y],X,L1) :-
         parse([A,B|Y],X,L1).
     parse([A*B|Y],X,L1) :-
         parse([A,B|Y],X,L1).
     parse([A^B|Y],X,L) :-
        integer(B), parse([A|Y],X,L).
     parse([A|Y],X,L) :-
        parse(Y,X,L).
     parse([A|Y],X,[A|L]) :-
        parse(Y,X,L).
     parse([],X,[]).



     free_of(Subterm,Term) :-
        occurrence(Subterm,Term,N), N=0.

     single_occurrence(Subterm,Term).

  occurrence(Term,Term,1).
  occurrence(Sub,Term,N) :-
        my_compound(Term), my_functor(Term,F,M), occurrence(M,Sub,Term,0,N).
  occurrence(Sub,Term,0).

  occurrence(M,Sub,Term,N1,N2) :-
        M > 0, my_arg(M,Term,Arg), occurrence(Sub,Arg,N), N3 is N+N1,
                M1 is M-1, occurrence(M1,Sub,Term,N3,N2).
  occurrence(0,Sub,Term,N,N).

  multiple([X1,X2|Xs]).
        remove_duplicates([],[]).
        remove_duplicates([X|Xs],[X|Ys]) :-
                remove_duplicates(Xs,Ys).
        remove_duplicates([X|Xs],Ys) :-
                member(X,Xs),
                remove_duplicates(Xs,Ys).


test_press(X,Y) :- equation(X,E,U), solve_equation(E,U,Y).

equation(1,a,b).
equation(2,b,c).

