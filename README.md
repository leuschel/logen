# logen
LOGEN is an offline partial evaluation system for Prolog written using the so called "cogen approach".
Basically, the cogen is a system which: based upon an annotated version of the program to be specialised produces a specialised partial evaluator for that program.
This partial evaluator is called a generating extension the generating extension can be used to specialise the program in a very efficient manner.

# Building and running logen
Once downloaded you can test the installation with:

    make bin/logen
    make test

This should finish without errors.
The first line make bin/logen produces an executable version of logen in the bin folder.
You can also simply type this to compile logen:

    make

To specialise:

    bin/logen PlFile "goal(A,B)"

You should also set the LOGEN_DIR (or LOGEN_HOME) environment variable to point to this
directory (i.e., the one containing this README file). Alternatively, you can pass
the --logen_dir D switch to logen.

For help:

    bin/logen --help

Output gx files should be standalone ciao executables.  So after
specialising match.pl, match.pl.gx.cpx should be the executable.

    ./match.pl.gx.cpx --help
    ./match.pl.gx.cpx "match([a,a,b],R)"

Logen requires annotation files (ending with .ann) for every Prolog source file that has to be specialised.
To generate annotation files you may want to use the binding-time analysis provided in another project: https://github.com/leuschel/logen-bta.
You can use the -w switch for logen to supervise specialisation and provide better error feedback.

# Research
This system is based on research presented in

http://stups.hhu.de/w/Special:Publication/LeJoVaBr04_20
Michael Leuschel, Jesper Jørgensen, Wim Vanhoof, Maurice Bruynooghe
Offline Specialisation in Prolog Using a Hand-Written Compiler Generator
TPLP, 4(1-2): 139-191, 2004.

http://stups.hhu.de/w/Special:Publication/JoLe96_82
Jesper Jørgensen, Michael Leuschel
Efficiently Generating Efficient Generating Extensions in Prolog
In Dagstuhl Seminar on Partial Evaluation, volume 1110 of Lecture Notes in Computer Science, Springer-Verlag, 1996.

http://stups.hhu.de/w/Special:Publication/CrGaLeHe04_15
Stephen Craig, John Gallagher, Michael Leuschel, Kim S. Henriksen
Fully Automatic Binding Time Analysis for Prolog [PDF] [Bibtex] [Old] [Upload] [Single Page]
In LOPSTR, volume 3573 of Lecture Notes in Computer Science, Springer-Verlag, 2004.

http://stups.hhu.de/w/Special:Publication/LeElVaCrFo06_140
Michael Leuschel, Daniel Elphick, Mauricio Varea, Stephen Craig, Marc Fontaine
The Ecce and Logen Partial Evaluators and their Web Interfaces
In Proceedings PEPM 06, IBM Press, 2006.

The following articles discuss using Logen:

http://stups.hhu.de/w/Special:Publication/LeBr02_38
Michael Leuschel, Maurice Bruynooghe
Logic program specialisation through partial deduction: Control Issues
Theory and Practice of Logic Programming, 2(4-5): 461-515, 2002.

http://stups.hhu.de/w/Special:Publication/LeCrBrVa04_19
Michael Leuschel, Stephen Craig, Maurice Bruynooghe, Wim Vanhoof
Specializing Interpreters using Offline Partial Deduction
In Program Development in Computational Logic, volume 3049 of Lecture Notes in Computer Science, Springer-Verlag, 2004.

The initial version of Logen was developed by Jesper Jorgensen and Michael Leuschel.
The current version also contains some additions by Armin Rigo for module support.
Within the ASAP project several contributions from UPM were made so that logen could run on Ciao Prolog.
The Weblogen frontend was developed mainly by Dan Elphick.

# Usage

    logen [Options] File.pl ["Atom."]
      Possible Options are:
      --help: Prints this message
      -w: watch mode (supervise specialization); equivalent to -wb -wu -wm -wc
      -W: watch mode; like -w option but halt at first alarm (-w -w1)
      -wb:   watch built-ins
      -wp:   watch for back-propagation of bindings onto non-declarative builtins
      -wc:   watch connectives (if,or) for back-propagation of bindings
      -wu:   watch out for infinite unfoldings
      -wm:   watch out for infinite memoisations
      -w1:   force halt at first watchdog alarm
      -c: run cogen only (not the .gx file)
      -s: run silently (do not display specialized program,...)
      --safe: run gx in safe sandbox mode
      -v: print debugging messages
      -vv: print more debugging messages
      -vvv: print even more debugging messages
      --compile_gx: compile the gx file using ciaoc
      -m: display memo table
      -np: no post-unfolding
      -ap: aggressive post-unfolding
      -g: display gx file
      --logen_dir ARG1: path to logen files
      -o ARG1: GX filename
      --spec_file ARG1: Spec filename
      --ciao_path ARG1: Ciao binary directory
      --simple_bta ARG1 ARG2 ARG3: Run simple bta: --simple_bta unfold/memo PLfile AnnFile
      -d: debug mode for GX file
      -d2: even more debugging messages in GX file
      --version: print logen version info
      --single_process: run cogen and gx in a single process - faster
      --xml: generate (some) diagnostic messages in xml

# Filenames used and generated by logen

    *.pl  -> the original Prolog source file
    *.pl.ann  -> the annotated Prolog source file, produced by hand or by a BTA
    *.pl.gx  -> the generating extension produced by LOGEN
    *.pl.spec -> a specialised version of the .pl file produced by LOGEN
    *.pl.memo -> a specialised module, consulting the .spec file

# Structure of the annotation files

Logen requires an annotation file FILE.pl.ann corresponding to your source file FILE.pl.

You can try and use the simple BTA command of logen or use the more sophisticated size-change analysis BTA from https://github.com/leuschel/logen-bta.

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
    

