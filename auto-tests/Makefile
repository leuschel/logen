
default: clean match lookup append transpose multiple_filters liftsolve hidenf lambdaint op_library_test

match: 
	source ./test match "testfiles/match.pl" "match.spec.pl" "match([a,b,c], [a,a,a,a,a,a,b,b,b,b,b,a,b,c,a,b,b,c,b,a,b,c,a,b,c,c,c,c,a,a,a,a,b,c])"

lookup:
	source ./test lookup "testfiles/lookup.pl" "lookup.spec.pl" "test(_X)"
	source ./test lookup "testfiles/lookup.pl" "lookup.spec.pl" "test(_X)" -safe

append:
	source ./test append "testfiles/append.pl" "append.spec.pl" "test([x,y,z,r],_)"

transpose:
	source ./test transpose "testfiles/transpose.pl" "transpose.spec.pl" "transpose([[1,2,3],[4,5,6],[7,8,9],[10,11,12]],_)"

multiple_filters:
	source ./test multiple_filters "testfiles/multiple_filters.pl" "multiple_filters.spec.pl" "test(_)"

liftsolve:
	source ./test liftsolve "testfiles/liftsolve.pl" "liftsolve.spec.pl" "test(term(cons,[term(1,[]),term(cons,[term(2,[]),term(null,[])])]),term(cons,[term(3,[]),term(null,[])]),_RES)"
	source ./test liftsolve "testfiles/liftsolve.pl" "liftsolve.spec.pl" "test(term(cons,[term(1,[]),term(cons,[term(2,[]),term(null,[])])]),term(cons,[term(3,[]),term(null,[])]),_RES)" -w

hidenf:
	source ./test hidenf "testfiles/hide_nf.pl" "hide_nf.spec.pl" "p(_)"
	source ./test hidenf "testfiles/hide_nf.pl" "hide_nf.spec.pl" "t(_,_)"

builtin_test_safety_error:
	source ./test builtin_test "testfiles/builtin_test.pl" "builtin_test.spec.pl" "test(X)" -safe
	
op_library_test:
	source ./test op_library_test "testfiles/op_library_test.pl" "op_library_test.spec.pl" "test([x,y,z,r],_)"
	
	
lambdaint:
	source ./test lambdaint "testfiles/lambdaint.pl" "lambdaint.spec.pl" "fib(10,_)" "fib(10,_)"  
	
lloyd_topor:
	source ./test lloyd_topor "testfiles/lloyd_topor_op.pl" "lloyd_topor_op.spec.pl" "solve_file_atom_opts('testfiles/lloyd_topor/app.pl',app(_X,_Y,[b,a,c]),[])"
lloyd_topor2:
	source ./test lloyd_topor "testfiles/lloyd_topor_op.pl" "lloyd_topor_op.spec.pl" "solve_file_atom_opts('testfiles/lloyd_topor/subset.pl',subset([a],[b,a,c]),['-C'])"

	
clean:
	find . -name "*.ast"  -exec rm {} \;
	find . -name "*.itf"  -exec rm {} \;
	find . -name "*.po"  -exec rm {} \;
	find . -name "*.spec"  -exec rm {} \;
	find . -name "*.orig"  -exec rm {} \;
	find . -name "*.gx"  -exec rm {} \;
	find . -name "*.cpx"  -exec rm {} \;
	rm -f *.pl
