include Makefile.inc

CIAOC=ciaoc -s
SWIOPTS=--stand-alone --goal=main -L8m -G16m
SWIPL=swipl

all: tempdir bta/bta.sav
	$(SWIPL) $(SWIOPTS) -o show_annotated -c show_annotated.pl
	$(SWIPL) $(SWIOPTS) -o highlight -c highlight.pl
	$(CIAOC) manual_annotate.pl
	$(CIAOC) collect_anns.pl
	./create_menus.sh

clean:
	-rm show_annotated highlight tempdir manual_annotate

bta/bta.sav: bta/path.pl bta/sicstus_prolog_to_xml.pl bta/bta.pl
	if [ x$(ENABLE_BTA) == xtrue ]; \
	then \
		cd bta; /Users/leuschel/bin/bin/sicstus -l bta.pl; \
	fi

tempdir: tempdir.c
	gcc -o tempdir tempdir.c
