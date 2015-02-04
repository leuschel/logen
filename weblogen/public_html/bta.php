<?php 
require_once('common.php');
require_once('externals.php');
require_session();

/* This handles both SIMPLE and FULL (filter and all) bta */
$btamode = $_REQUEST['btamode'];

/* Make tmp dir to work in */
$tempdir = get_directory();
chdir($tempdir);

/* save pl file to tmp dir */
$filename = basename($_SESSION['filename']);
$file = fopen("$tempdir/$filename", "w");
fwrite($file, $_SESSION['plfile']);
fclose($file);

$_SESSION['editerror'] = '';

if ($btamode == "SIMPLE")
{
	$_SESSION['annmethod'] = 'simplebta';
	$btatype = $_REQUEST['btatype'];
	if($btatype != 'unfold' && $btatype != 'memo')
	{
		die('Invalid simple BTA type given');
	}
	$_SESSION['btatype'] = $btatype;
	$retval = run_simple_bta("$tempdir/$filename", $btatype);
	if($retval[0] != 0)
	{
		echo "<b>An error occurred<b>";
		echo "<pre>$retval[2]</pre>";
		exit(1);
	}
}
elseif ($btamode == "FULL")
{
    $filter = $_POST['filter'];
    $norm = $_POST['norm'];
    if(!isset($_POST['filter_prop']))
        $filter_prop = 'off';
    else
	$filter_prop = 'on';
    $_SESSION['filter'] = $filter;
    $_SESSION['norm'] = $norm;
    $_SESSION['annmethod'] = 'bta';
    $_SESSION['filter_prop'] = $filter_prop;
    $norm = strtolower($norm);

    /* Validate the parameters */
    if($norm != 'term' && $norm != 'list' && $norm != 'both')
        die('Norm must be term, list or both.');
    if($filter == "")
        die('You must provide a valid filter.');
    if($filter[strlen($filter) - 1] != '.')
        $filter = $filter . '.';

    /* Write filters to .filters file */
    $file = fopen("$tempdir/$filename.filters", "w");
    fwrite($file, ":- filter\n\t");
    fwrite($file, $filter);
    fwrite($file, "\n");
    fclose($file);

    /* Select Process to run */
	
    if($filter_prop == 'on')
		$retval = filter_prop("$tempdir/$filename");
    else
		$retval = run_auto_bta("$tempdir/$filename", $norm);

	if($retval[0] != 0)
	{
		echo "<b>An error occurred<b>";
		echo "<pre>$retval[2]</pre>";
		exit(1);
	}
}
else
{
	// Only reachable if the user is deliberately changing the html before
	// submission, so don't worry about proper error message
	die('Bad btamode specified');
}

/* Check to see if an error occurred and report */
if(file_exists("$tempdir/error.xml"))
{
	report_xml_error('BTA Error', file_get_contents("$tempdir/error.xml"));
	delDir($tempdir);
	exit;
}

/* Use ann file for rest of session */
$_SESSION['annfile'] = file_get_contents("$tempdir/$filename.ann");
$_SESSION['stage'] = 3;

/* Clean up temp files */
delDir($tempdir);

redirect('upload_annfile.php');
?>
