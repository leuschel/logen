<?php 
// include "header.php";
require_once('common.php');
create_session();
html_start('WebLogen - Report Bug','','',0, "Report a bug");

// set newbug to true, when a submission is made this will be set to false.
// if set to false bug_report_submit.php will reject it as it has been
// reported twice.
$_SESSION['newbug'] = true;

echo "<form action='bug_report_submit.php' method='post'>\n";

function show_box($text)
{
	echo '<div class="box"><pre>';
	echo htmlspecialchars($text);
	echo '</pre></div>';
}

if(isset($_POST['stage']))
{
	$stage = $_POST['stage'];
	if($stage == 'upload')
	{
		?>
		<input type="hidden" name="stage" value="upload"/>
		<p>You are reporting that a bug occurred while trying to upload 
		source code. While the error may be due to a bug in weblogen, please
		be aware that the problem may be in your source code. Make sure that
		your code is parseable before continuing further.</p>
		<p>Here is your program:</p>
		<? show_box($_SESSION['plfile']); ?>
		<p>Here is the error that was generated:</p>
		<? show_box($_SESSION['plerror']);
	}
	else if($stage == 'uploadann')
	{
		?>
		<input type="hidden" name="stage" value="uploadann"/>
		<p>You are reporting that a bug occurred while trying to upload 
		an annotation file. While the error may be due to a bug in weblogen,
		please be aware that the problem may be in your source code or
		annotations files. Make sure that your code is parseable before
		continuing further.</p>
		<p>Here is your program:</p>
		<? show_box($_SESSION['plfile']); ?>
		<p>Here is your annotation file:</p>
		<? show_box($_SESSION['annfile']); ?>
		<p>Here is the error that was generated:</p>
		<? show_box($_SESSION['annerror']);
	}
	else if($stage == 'editann')
	{
		?>
		<input type="hidden" name="stage" value="uploadann"/>
		<p>You are reporting that a bug occurred while trying to edit the 
		annotations. While the error may be due to a bug in weblogen,
		please be aware that the problem may be in your source code or
		annotations files. Make sure that your code is parseable before
		continuing further.</p>
		<p>Here is your program:</p>
		<? show_box($_SESSION['plfile']); ?>
		<p>Here is your annotation file:</p>
		<? show_box($_SESSION['annfile']); ?>
		<p>Here is the error that was generated:</p>
		<? show_box($_SESSION['annerror']);
	}
	else if($stage == 'spec')
	{
		?>
		<input type="hidden" name="stage" value="spec"/>
		<p>You are reporting that a bug occurred while trying to specialise.
		While the error may be due to a bug in weblogen, please be aware that
		the problem may be in your source code, annotations or goal. Please
		check these carefully before submitting.</p>
		<p>Here is your program:</p>
		<? show_box($_SESSION['plfile']); ?>
		<p>Here is your annotation file:</p>
		<? show_box($_SESSION['annfile']); ?>
		<p>Here is your goal:</p>
		<? show_box($_SESSION['goal']); ?>
		<p>Here is the error that was generated:</p>
		<? show_box($_SESSION['specerror']);
	}
	else
	{
		die('Invalid stage');
	}

?>
<p>If you want to add any extra information, please do so in this box:</p>
<textarea name="bugtext" style="width: 99%" rows="10"></textarea>
<?
}
else
{
?>
Please enter details of bug or request:
<textarea name="bugtext" style="width: 99%" rows="30"></textarea>
<?
}
?>
	<br/>
	<p>If you supply a contact email address we can inform you when the bug is
	fixed, or ask for further information if required.</p>
	<p>
		Email: <input type="text" name="email"/> 
		<input type="submit" value="Submit"/>
	</p>
</form>
<? html_end(); ?>
