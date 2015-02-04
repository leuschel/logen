<?php  
require_once('common.php');
require_session();

function stock_annfile($filename, $orig_filename)
{
	global $frontend_dir;

	$orig_filename = $orig_filename . '.ann';
    // check that it is an annotation file, which is to say it has the same
	// name as the source file but with .ann* tacked on the end
    if(substr($filename, 0, strlen($orig_filename)) != $orig_filename)
        return false;

    // Check that there is no attempt to change directory.
    if(strpos($filename, '/') !== false)
        return false;

    // Check file exists and is readable
    $fullname = "$frontend_dir/examples/$filename";
    if(!is_readable($fullname))
        return false;
  
    return $fullname;
}

if(isset($_FILES['annfile']) && is_uploaded_file($_FILES['annfile']['tmp_name']))
{
	$_SESSION['annfile'] = file_get_contents($_FILES['annfile']['tmp_name']);
	$_SESSION['annmethod'] = 'upload';
	$_SESSION['editerror'] = '';
	$_SESSION['stage'] = 3;
	redirect('upload_annfile.php');	
}
elseif($_SESSION['stock'] == "true" && isset($_POST['annfile']))
{
	$annfile = $_POST['annfile'];
	$stock_file = stock_annfile($annfile, $_SESSION['filename']);
	if($stock_file !== false)
	{
		$_SESSION['annfile'] = file_get_contents($stock_file);
		$_SESSION['stock_annfile'] = $annfile;
		$_SESSION['annmethod'] = 'stock';
		$_SESSION['editerror'] = '';
		$_SESSION['goal'] = '';
		$_SESSION['stage'] = 3;
		redirect('upload_annfile.php');
	}
	else
	{
		stock_file_error('Annotation');
	}
}
else
{
	file_upload_error('Annotation', 'upload.php', $_FILES['annfile']['error']);
}

?>
