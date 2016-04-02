LOGEN=logen
test:
	${LOGEN} match.pl "match([a,a,b],L)" --spec_file match_aab.pl -v
	cat match_aab.pl
	
clean:
	find . -name "*.ast"  -exec rm {} \;
	find . -name "*.itf"  -exec rm {} \;
	find . -name "*.po"  -exec rm {} \;
	find . -name "*.spec"  -exec rm {} \;
	find . -name "*.orig"  -exec rm {} \;
	find . -name "*.gx"  -exec rm {} \;
	find . -name "*.cpx"  -exec rm {} \;