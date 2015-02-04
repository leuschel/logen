<?php 
require_once('common.php');
require_once('externals.php');

if(isset($_FILES['plfile']) && is_uploaded_file($_FILES['plfile']['tmp_name']))
{
	$filename = $_FILES['plfile']['name'];
	$plfile = file_get_contents($_FILES['plfile']['tmp_name']);
}
else if(isset($_POST['plfile']))
{
	$filename = 'user.pl';
	$plfile = $_POST['plfile'];
}
else
{
	die('no pl file uploaded');
}

if(isset($_FILES['annfile']) && is_uploaded_file($_FILES['annfile']['tmp_name']))
{
	$annfile = file_get_contents($_FILES['annfile']['tmp_name']);
}
else if(isset($_POST['annfile']))
{
	$annfile = $_POST['annfile'];
}
else
{
	die('no annotation file uploaded');
}

if(isset($_POST['goal']))
{
	$goal = $_POST['goal'];
}
else
{
	die('no goal given');
}

if(isset($_POST['gx']) && $_POST['gx'] == 'on')
{
	$spec = 2;
}
else
{
	$spec = 1;
}

$tempdir = get_directory();

$file = fopen("$tempdir/$filename", "w");
fwrite($file, $plfile);
fclose($file);

$file = fopen("$tempdir/$filename.ann", "w");
fwrite($file, $annfile);
fclose($file);

if($spec == 1)
{
	$retarray = specialise($filename, $goal, $tempdir, 0, 0, $safe);
	$specfilename = substr($filename, 0, strlen($filename) - 3) . '.spec';
}
else
{
	$retarray = create_gx($filename, $tempdir, 0, 0);
	$specfilename = "$filename.gx";
}

if($retarray[0] == 0)
{
	header('Content-type: application/pl');
	header("Content-Disposition: attachment; filename=\"$specfilename\"");
	echo file_get_contents("$tempdir/$specfilename");
}
else
{
	echo '<pre>';
	echo $retarray[2];
	echo '</pre>';
}

delDir($tempdir);

?>

