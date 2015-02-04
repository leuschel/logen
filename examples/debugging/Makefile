
default: clean lambdaint task_csp_scheduler


task_csp_scheduler_err:  task_csp_scheduler_err.pl 
	../../logen --simple_bta debug task_csp_scheduler_err.pl debug.pl.ann -v
	../../logen debug.pl "main([2,3,4])" -wb -v
	
task_csp_scheduler:  task_csp_scheduler.pl 
	../../logen --simple_bta debug task_csp_scheduler.pl debug.pl.ann
	../../logen debug.pl "main([2,3,4])" -wb
	../../logen debug.pl "main([2,3,4])"

lambdaint:  lambdaint.pl 
	../../logen --simple_bta debug lambdaint.pl debug.pl.ann 
	../../logen debug.pl "main2(7)" -wb
	../../logen --simple_bta unfold lambdaint.pl debug.pl.ann 
	../../logen debug.pl "main2(7)" 
	../../logen debug.pl "main2(7)" -wb
	echo "Done lambdaint"

simple:  simple.pl
	../../logen --simple_bta debug simple.pl debug.pl.ann 
	../../logen debug.pl "main(X)" -wb
	

clean:
	find . -name "*.po"  -print -exec rm {} \;
	find . -name "*.itf" -print -exec rm {} \;
	find . -name "*.gx" -print -exec rm {} \;
	find . -name "*.cpx" -print -exec rm {} \;
	find . -name "*.ast" -print -exec rm {} \;
