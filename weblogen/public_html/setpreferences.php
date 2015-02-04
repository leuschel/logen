<?php
require_once('common.php');
require_session();

$display = $_POST['display'];
$prefs = array('display', $display);
setcookie('weblogen_prefs', implode(':', $prefs), time() + 60*60*24*30);

switch($_POST['currentstage'])
{
	case 1:
		redirect('open.php');
		break;
	case 2:
		redirect('upload.php');
		break;
	case 3:
		redirect('upload_annfile.php');
		break;
	case 4:
		redirect('upload_spec.php');
		break;
	default:
		die('Bad stage set! Can\'t redirect');
}
?>
