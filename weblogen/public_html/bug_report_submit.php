<?php 
// include "header.php";
require_once('common.php');
require_once('bug_common.php');
create_session();
html_start('WebLogen - Report Bug','','',0, "Report a bug");

if(!$_SESSION['newbug'])
{
	echo "<p>Bug already submitted</p>\n";
	html_end();
	exit();
}

echo '<p>Thank you for your bug report.</p>';

if(isset($_POST['stage']))
{
	$stage = $_POST['stage'];
	$goal = '';
	if($stage == 'upload')
	{
		$errortext = $_SESSION['plerror'];
		$plfile = $_SESSION['plfile'];
		$annfile = NULL;
		$returl = 'open.php';
	}
	else if($stage == 'uploadann')
	{
		$errortext = $_SESSION['annerror'];
		$plfile = $_SESSION['plfile'];
		$annfile = $_SESSION['annfile'];
		$returl = 'upload.php';
	}
	else if($stage == 'editann')
	{
		$errortext = $_SESSION['annerror'];
		$plfile = $_SESSION['plfile'];
		$annfile = $_SESSION['annfile'];
		$returl = 'upload_annfile.php';
	}
	else if($stage == 'spec')
	{
		$errortext = $_SESSION['specerror'];
		$plfile = $_SESSION['plfile'];
		$annfile = $_SESSION['annfile'];
		$goal = $_SESSION['goal'];
		$returl = 'upload_annfile.php';
	}
	else
	{
		die('Invalid stage');
	}
}
else
{
	$stage = 'general';
	$errortext = '';
	$plfile = '';
	$annfile = '';
	$goal = '';
	$returl = 'open.php';
}

$email = $_POST['email'];
$comment = $_POST['bugtext'];

$id = create_new_bug($stage, $email, $comment, $errortext, $plfile, $annfile, $goal);
$_SESSION['newbug'] = false;
echo "<p><a href='bug_view?id=$id'>Bug $id</a> has been filed.</p>";
echo "<p><a href='$returl'>Click here to return</a></p>";

html_end();
?>
