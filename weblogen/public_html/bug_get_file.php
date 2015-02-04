<?php
require_once('common.php');
require_once('bug_common.php');

if(!isset($_GET['id']) || !isset($_GET['type']))
{
	die('Bug id or download type not given');
}
$id = intval($_GET['id']);
if(!is_integer($id) || $id < 1)
{
	die("Bug id '$id' is not a positive integer");
}

switch($_GET['type'])
{
	case 'plfile':
		$filename = 'user.pl';
		break;
	case 'annfile':
		$filename = 'user.pl.ann';
		break;
	default:
		die('Invalid download type');
}

$file = get_bug_file($id, $_GET['type']);

// Remove .pl and append .spec
header('Content-type: application/pl');
header("Content-Disposition: attachment; filename=\"$filename\"");
echo $file;

?>
