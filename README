# logen
LOGEN is an offline partial evaluation system for Prolog written using the so called "cogen approach".
Basically, the cogen is a system which: based upon an annotated version of the program to be specialised produces a specialised partial evaluator for that program.
This partial evaluator is called a generating extension the generating extension can be used to specialise the program in a very efficient manner.

# Building and running logen
Once downloaded you can test the installation with:
 make cogen
 make test

This should finish without errors.

To compile:
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
      --single_process: run cogen and gx in a single process - faster
      --xml: generate (some) diagnostic messages in xml