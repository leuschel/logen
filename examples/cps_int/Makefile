# We assume that logen and bta are in PATH already. Otherwise set them explicitly as above.
LOGEN=logen
#BTABIN=/Volumes/Data/projects/logen-bta/bin/bta
BTABIN=bta
BTA=${BTABIN} -scc


GOAL=test_dcg
test_bti: cps_int_spec.pl Makefile
	sicstus -l cps_int_spec.pl --goal "$(GOAL)__0([[a],[b],[c],R]),print(R),nl,halt."
cps_int_spec.pl: cps_int.pl.ann Makefile
	echo "Running LOGEN (using hand generated annotation file): "
	${LOGEN} cps_int.pl "$(GOAL)(X)" -ap --spec_file cps_int_spec.pl -v -w

cps_int.pl.ann2: cps_int.pl Makefile
	echo "Running BTA for $(GOAL): (not used for logen)"
	${BTA} cps_int.pl -o cps_int.pl.ann2 --entry "$(GOAL)(d)." --lib "assoc"