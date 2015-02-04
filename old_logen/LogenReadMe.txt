<h1>USER GUIDE TO LOGEN ANNOTATIONS</h1>


<h2>1) Filenames</h2>

  *.pl  -> the <u>original Prolog with embedded annotations</u> source file
  *.pl.gx  -> the <u>generating extension</u> produced by LOGEN
  *.pl.spec -> a <u>specialised version</u> of the .pl file produced by LOGEN
  *.pl.memo -> a <u>specialised module</u>, consulting the .spec file

<h2>2) Structure of the annotation files</h2>

A first approximation of the annotation can be obtained
by using the "Do Simple BTA" command in the Tcl/Tk interface
or by using the "AnnotateFile/2" predicate in cogen-interface.

A clause
  HEAD :- BODY
is transformed into a clause
  logen(id,HEAD) :- Abody

where Abody has the grammar:

<b>Abody</b> ::=  
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
 
  <b>BindingType</b> ::=
      static |
      dynamic |
      semi |
      nonvar |
      nonnonvar |
      ( BindingType ; BindingType ) |
      struct(Functor, [BindingType,...,BindingType]) |
      type(BuiltinType)
      
    BuiltinType ::=  list(BindingType)
    
<h2>TO DO:</h2>
	explain static_consult/2
