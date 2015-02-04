<? 
// This file contains functions which call out to prolog.
//
// Most of these functions return an array (retval, stdout, stderr)
// retval will always be 0 if the program succeeded, and the result (if there
// was any) will be in stdout.
// If there is an error retval will be set to some non-zero value.
// An error message/logging information will be in stderr.
//
// The autobta and filter_prop functions don't give return values because
// sicstus doesn't support this.
// 
// show_annotated($filename, $extra) returns an xml representation of an
// annotated file $filename (assumes existence of $filename.ann). $extra
// is a hash containing keys (ann, pred, arity, clause, path) indicating how to
// alter the annotations before displaying them. It can be unset.
//
// highlight($filename) returns an xml representation of prolog file $filename
//
// specialise($filename, $goal, $tempdir, $watch, $logging, $safe) specialises a
// program wrt. $goal and will write out the result in the directory $tempdir.
// If $watch is true then watchdog mode will be enabled. $logging is a number
// between 0 and 2 and indicates the level of logging required. $safe indicates
// whether to create a generating extension which checks calls against the
// whitelist.
//
// run_auto_bta($filename, $norm) runs the auto bta on a file using the given
// $norm.
//
// run_filter_prop($filename) runs the filter propagation on $filename.
// 
// run_simple_bta($filename, $type) runs the simple BTA on $filename. $type
// indicates the type of annotation to use: 'unroll' means unroll/call all
// 'memo' means memo/rescall all. unroll is converted to safecall, which means
// calls which would never be executed by logen are converted to rescall.
//
// modify_annotations($plfile, $newannfile, $annotations, $filters) takes a
// source file ($fdile) and produces a new annotation file ($newannfile) using
// the list of annotations in $annotations and the filters in $filters.
// (NB this is a change from the old system which took an annfile as input
// and produced a new annfile as output. This new method is far more flexible).

function show_annotated($filename, $extra = null)
{
	global $backend_dir;
	$timeout = get_timeout('show_annotated');
	if(isset($extra))
	{
		$args = Array();
		foreach($extra as $arg)
		{
			switch($arg['type'])
			{
				case 'correctann':
					$args[] = '--correctable';
					$args[] = $arg['ann'];
					$args[] = $arg['pred'];
					$args[] = $arg['arity'];
					$args[] = $arg['clause'];
					$args[] = $arg['path'];
					$args[] = $arg['fix'];
					break;
				case 'infoann':
					$args[] = '--extra_info';
					$args[] = $arg['ann'];
					$args[] = $arg['pred'];
					$args[] = $arg['arity'];
					$args[] = $arg['clause'];
					$args[] = $arg['path'];
					break;
				case 'infofilt':
					$args[] = '--mark_filter';
					$args[] = $arg['pred'];
					$args[] = $arg['arity'];
					break;
				case 'correctfilt':
					$args[] = '--correctable_filter';
					$args[] = $arg['pred'];
					$args[] = $arg['arity'];
					$args[] = $arg['make_dyn'];
					break;
				case 'replacefilt':
					$args[] = '--correctable_filter_replace';
					$args[] = $arg['pred'];
					$args[] = $arg['arity'];
					$args[] = $arg['replacefilt'];
					break;
				case 'fix_hide_nf';
					$args[] = '--correct_hide_nf';
					$args[] = $arg['pred'];
					$args[] = $arg['arity'];
					$args[] = $arg['clause'];
					$args[] = $arg['path'];
					break;
				default:
					die("unknown extra info type '{$arg['type']}'");
			}
		}
		$args[] = $filename;
		$args[] = "$filename.ann";
		return run_process_array_args("$backend_dir/show_annotated", $timeout, $args);
	}
	else
	{
		return run_process("$backend_dir/show_annotated", $timeout,
						   $filename, "$filename.ann");
	}
}

// Specialise the program returning an array [0, xml] when it works or
// [$retval, error log] if it fails.
function specialise($filename, $goal, $tempdir, $watch_opts, $logging, $safe)
{
	global $backend_dir, $logen_dir, $ciao_dir;

	$timeout = get_timeout('specialise');

	if($goal[strlen($goal) - 1] != '.')
        $goal = $goal . '.';

	chdir($tempdir);

	$args = Array();

	$args[] = $filename;
	$args[] = $goal;

	if($logging == 1)
	{
		$args[] = '-v';
		$args[] = '-d';
	}
	else if($logging == 2)
	{
		$args[] = '-vvv';
		$args[] = '-d2';
	}


	$args[] = '--logen_dir';
	$args[] = $logen_dir;
	$args[] = '--spec_file';
	$args[] = $specfile = substr($filename, 0, strlen($filename) - 3) . '.spec';
	$args[] = '--ciao_path';
	$args[] = $ciao_dir;
	if($watch_opts['enabled'] == 'on')
	{
		$args[] = '-w1';
		if($watch_opts['builtins'] == 'on')
			$args[] = '-wb';
		if($watch_opts['backprop'] == 'on')
			$args[] = '-wp';
		if($watch_opts['connectives'] == 'on')
			$args[] = '-wc';
		if($watch_opts['infunfold'] == 'on')
			$args[] = '-wu';
		if($watch_opts['infmemo'] == 'on')
			$args[] = '-wm';
	}
	$args[] = '--single_process';
	$args[] = '--xml';
	if($safe)
	{
		$args[] = '--safe';
	}
	$retval = run_process_array_args("$logen_dir/cogen", $timeout, $args);
	if($retval[0] == 0)
	{
		$retval2 = highlight($specfile);
		$retval2[2] = $retval[2] . "\nHighlighting...\n" . $retval2[2];
		// error in highlighting has negative value!
		$retval2[0] = -$retval2[0];
		return $retval2;
	}

	return $retval;
}

// Create a gx for the program returning an array [0, xml] when it works or
// [$retval, error log] if it fails.
function create_gx($filename, $tempdir, $watch, $logging)
{
	global $backend_dir, $logen_dir, $ciao_dir;

	chdir($tempdir);

	$watch = $watch ? '-W' : '';

	if($logging == 1)
	{
		$logging = '-v';
		$debug = '-d';
	}
	else if($logging == 2)
	{
		$logging = '-vvv';
		$debug = '-d2';
	}
	else
	{
		$logging = '';
		$debug = '';
	}

	$retval = run_process("$logen_dir/cogen", get_timeout('gx'),
						  '-c', $filename,
						  '--logen_dir', $logen_dir,
						  '--ciao_path', $ciao_dir,
						  '--xml',
						  $watch, $logging, $debug);
	if($retval[0] == 0)
	{
		$retval2 = highlight("$filename.gx");
		$retval2[2] = $retval[2] . $retval2[2];
		return $retval2;
	}

	return $retval;
}

function highlight($filename)
{
	global $backend_dir;
	return run_process("$backend_dir/highlight", get_timeout('highlight'),
					   $filename);
}

function run_auto_bta($filename, $norm)
{
	global $backend_dir;
	return run_process_with_input("$backend_dir/bta/bta.sh",
								  get_timeout('auto_bta'),
								  "$filename\n$norm\n");
}

function run_simple_bta($filename, $type)
{
	global $logen_dir;
	if($type == 'unfold')
	{
		$type = 'safecall';
	}
	return run_process("$logen_dir/cogen", get_timeout('simple_bta'),
					   '--simple_bta', $type, $filename, "$filename.ann");
}

function filter_prop($filename)
{
	global $backend_dir;
	return run_process_with_input("$backend_dir/bta/filt_prop.sh",
								  get_timeout('filter_prop'),
								  "$filename\n");
}

function modify_annotations($plfile, $newannfile, $annotations, $filters)
{
	global $backend_dir;
	return run_process_with_input("$backend_dir/manual_annotate",
								  get_timeout('modify_anns'),
								  "$annotations\n$filters\n",
								  $plfile, $newannfile);
}

function run_process_with_input($executable, $maxtime, $input)
{
	$args = func_get_args();
	$args[0] = '';
	$args[1] = '';
	$args[2] = '';
	if(count($args) == 4 && is_array($args[3]))
	{
		$args = $args[3];
	}

	$command = $executable;
	for($i = 0; $i < count($args); ++$i)
	{
		if($args[$i] != '')
		{
			$command .= ' ' . escapeshellarg($args[$i]);
		}
	}
		
	$descriptorspec = array(
        0 => array("pipe", "r"),
        1 => array("file", "stdout", "w"),
        2 => array("file", "stderr", "w")
    );

	//echo "<b>$command</b><br>";

    $process = proc_open($command, $descriptorspec, $pipes);

    if(is_resource($process))
    {
        fwrite($pipes[0], $input);
        fclose($pipes[0]);

		$starttime = microtime(true);
		$return_value = 0;
		$timeout = false;

		while(true)
		{
			$status = proc_get_status($process);
			if(!$status['running'])
			{
				#print_r($status);
				break;
			}
			usleep(100000);
			if(microtime(true) - $starttime > $maxtime)
			{
				#print microtime(true) - $starttime;
				#print "<br>";
				proc_terminate($process);
				$timeout = true;
				$return_value = 1;
				break;
			}
		}
		if(!$timeout)
		{
			$return_value = proc_close($process);
			$return_value = $status['exitcode'];
		}

		$output = file_get_contents('stdout');
		$error = file_get_contents('stderr');
		if($timeout)
		{
			$error.="\nProcess timed out\n";
		}

		return array($return_value, $output, $error);
    }
    else
        die("Cannot open $executable");
}

function run_process($executable, $timeout)
{
	$args = func_get_args();
	$args[0] = '';
	$args[1] = '';
	return run_process_with_input($executable, $timeout, '', $args);
}

function run_process_array_args($executable, $timeout, $args)
{
	return run_process_with_input($executable, $timeout, '', $args);
}

?>
