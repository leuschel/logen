

cogen:  *.pl gximports/*.pl bta/*.pl annotation/*.pl tools/*.pl
	ciaoc -o cogen ciao_entry.pl 

test:	
	cd auto-tests; make
	

cogen_mac:  *.pl gximports/*.pl bta/*.pl
	ciaoc -S -o cli_binaries/MacOSX/logen ciao_entry.pl
cogen_linux:  *.pl gximports/*.pl bta/*.pl
	ciaoc -S -o cli_binaries/Linux/logen ciao_entry.pl
cogen_win:  *.pl gximports/*.pl bta/*.pl
	ciaoc -S -o cli_binaries/Windows/logen ciao_entry.pl


clean:
	find . -name "*.po"  -print -exec rm {} \;
	find . -name "*.itf" -print -exec rm {} \;
	find . -name "*.ast" -print -exec rm {} \;
	-rm cogen
