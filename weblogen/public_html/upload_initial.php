<?php 
require_once('common.php');
create_session(); 

if(isset($_POST['usertyped']))
{
	$_SESSION['plfile'] = sanitise_input_file($_POST['usertyped']);
	$filename = $_POST['filename'];
	// if the filename contains slashes get the part after the slashes
	$p = strrpos($filename, '/');
	if(is_int($p))
	{
		$filename = substr($filename, $p);
	}
	$_SESSION['filename'] = $filename ? $filename : 'user.pl';
	
	$_SESSION['annerror'] = '';
	$_SESSION['logging'] = '';
	$_SESSION['editerror'] = '';
	$_SESSION['annfile'] = '';
	$_SESSION['filter'] = '';
	$_SESSION['filter_prop'] = '';
	$_SESSION['annmethod'] = '';
	$_SESSION['norm'] = '';
	$_SESSION['watch'] = '';
	$_SESSION['goal'] = '';
	$_SESSION['stock'] = $_POST['stockfile'];

	$_SESSION['stock_annfile'] = false;
	$_SESSION['stage'] = 2;

	if($_SESSION['stock'] == "true")
	{
		$_SESSION['annmethod'] = 'stock';
	}
	if(isset($_POST['save']))
	{
		redirect('get_pl.php');
	}
	else
	{
		redirect('upload.php');
	}
}
else
{
	file_upload_error('Source', 'index.html', $_FILES['userfile']['error']);
}
?>
