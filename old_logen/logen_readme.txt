 _                           
| |    ___   __ _  ___ _ __  
| |   / _ \ / _` |/ _ \ '_ \ 
| |__| (_) | (_| |  __/ | | |
|_____\___/ \__, |\___|_| |_|
            |___/            

An offline partial evaluation system for Prolog.

(C) 1996-2003 Michael Leuschel, Jesper Joergensen, Stephen Craig
All rights reserved.
Free for academic and non-commercial use and evaluation purposes.
Contact us for commercial use.

README File for LOGEN
------------------------

LOGEN is an offline partial evaluation system for Prolog written using the
so called "cogen approach".
Basically, the cogen is a system which
 1. based upon an annotated version of the program to be specialised 
 produces a specialised partial evaluator for that program. This partial
 evaluator is called a generating extension.
 2. you can then use the generating extension to specialise the program in 
 a very efficient manner.
For more details, please have a look at:
 @techreport{LeuschelJorgensen:TR99,
   author =      {Michael Leuschel and Jesper J{\o}rgensen},
   title =       {Efficient Specialisation in {P}rolog
                  Using a Hand-Written Compiler Generator},
   year =        1999,
   month = {September},
   number =      {DSSE-TR-99-6},
   type =        {Technical Report},
   institution = {Department of Electronics and Computer Science,
                  University of Southampton},
	
}
@article{LeuschelEtAl:TPLP03,
 author = "Leuschel, Michael and J{\o}rgensen, Jesper and
 Vanhoof, Wim and Bruynooghe, Maurice",
 title = "Offline Specialisation in {P}rolog Using a Hand-Written Compiler Generator",
  journal = "Theory and Practice of Logic Programming",
   year = 2004,
    note = {To appear},
}

 LOGEN now also contains a CLP(Q+R) specialiser by Stephen Craig.

------------------------

Development, Copyright and Intellectual Property Rights:
Original LOGEN:
	Michael Leuschel, Jesper Joergensen 
CLP & Module Extensions:
	Stephen Craig, Michael Leuschel
Socket Interface:
       Stephen Craig
Python Interface:
       Stephen Craig, Daniel  Elphick
Emacs mode:
	Stephen Craig, Armin Rigo
Tcl/Tk Interface:
	Michael Leuschel, Laksono Adhianto
Filter Propagation
       John Gallagher (RUC)
BTA Termination
	Stephen Craig, Michael Leuschel
Convex Hull Analyser 
       Derived from code from Samir Genaim

LIMITATIONS:
	The automatic binding-time analysis is still in testing but has shown promising results for programs with list/term based norms.
	The system is only tested with SICStus Prolog 3.11.0
	The system is (almost) working in Ciao Prolog (but not the Tcl/Tk part).
	
------------------------

Structure of the annotation files

A first approximation of the annotation can be obtained
by using the "Do Simple BTA" command in the Tcl/Tk interface
or by using the "AnnotateFile/2" predicate in cogen-interface.

A clause
  HEAD :- BODY
is transformed into a clause
  logen(id,HEAD) :- Abody

where Abody has the grammar:

Abody ::=  
	true |
	(Abody,Abody) |   # conjunction
	logen(call,BuiltinCall) |
	logen(rescall,BuiltinCall) |
	logen(semicall,BuiltinCall) |
	logen(unfold,UserCall) |
	logen(memo,UserCall) |
	if(Abody,Abody,Abody) |  # some restrictions apply
	resif(Abody,Abody,Abody) | # some restrictions apply
	(Abody ; Abody) |
	resdisj(Abody,Abody) |
	not(Abody) | # some restrictions apply
	resnot(Abody) | # some restrictions apply|
	findall(Term,Abody,Term) | # some restrictions apply
	resfindall(Term,Abody,Term) | # some restrictions apply
	reswhen(Cond,Abody) | # some restrictions apply
	when(Cond,Abody) | # some restrictions apply
	reswhen(Cond,Abody) | # some restrictions apply
	hide_nf(Abody) |
	hide(Abody)

Additional annotations that are supported are:
  :- residual p/n.
 which tells Logen that the predicate p of arity n may
 be specialiased, i.e., residual predicate definitions may
 have to be produce for it. Note: preciates can be unfolded
 without being declared residual.
 
 :- filter p(BindingType,...,BindingType).
 This tells logen the Binding Type of each argument of a predicate
 marked as residual.
 The Grammar for BindingTypes is for the moment:
 
  BindingType ::=
      static |
      dynamic |
      semi |
      nonvar |
      nonnonvar |
      ( BindingType ; BindingType ) |
      struct(Functor, [BindingType,...,BindingType]) |
      type(BuiltinType)
      
    BuiltinType ::=  list(BindingType)
    
------------------------
 

For further information or bug reports you can reach me
as follows:
============================================================
Michael Leuschel (http://www.ecs.soton.ac.uk/~mal)
Declarative Systems and Software Engineering
Department of Electronics and Computer Science
University of Southampton
Highfield, Southampton, SO17 1BJ,  U.K.
Fax: +44 01703 59 3045   Tel: +44 01703 59 3377
E-mail: mal@ecs.soton.ac.uk
============================================================