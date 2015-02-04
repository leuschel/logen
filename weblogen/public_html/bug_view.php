<?php 
require_once('common.php');
require_once('bug_common.php');
create_session();
if(!isset($_GET['id']))
{
	die('No bug selected');
}

// Non numbers get converted to 0 and we don't want negative bug numbers
$id = intval($_GET['id']);
if($id <= 0)
{
	die('Invalid bug selected');
}

html_start("WebLogen - View Bug $id",'','',0, "View Bug $id");
$result = get_bug_details($id);
if(!$result)
{
	echo "<b>Bug $id does not exist</b>\n";
}
else
{
	$stageno = $result['stage'];
	$stage = number2stage($stageno);
	$date = $result['sdate'];
	$status = number2status($result['status']);
	$email = htmlspecialchars($result['email']);
	$comment = htmlspecialchars($result['comment']);
	$plfile = htmlspecialchars($result['plfile']);
	$annfile = htmlspecialchars($result['annfile']);
	$error = htmlspecialchars($result['errortext']);
	$goal = htmlspecialchars($result['goal']);
	$recreateable = bug_recreateable($stageno);

	echo "<table id='buglist'>\n<tr><th>Bug id:</th><td>$id</td></tr>\n";
	echo "<tr><th>Status:</th><td>$status</td></tr>\n";
	echo "<tr><th>Stage:</th><td>$stage</td></tr>\n";
	echo "<tr><th>Submitter Email:</th><td>$email</td></tr>\n";
	echo "<tr><th>Submit Date:</th><td>$date</td></tr>\n";
	echo "<tr><th>Comment:</th><td>$comment</td></tr>\n";

	js_toggleShow();

	function create_box($title, $id, $content, $bugid = 0, $type = '')
	{
		echo "<tr><th rowspan='2' valign='top'>$title</th>\n";
		echo "<td><form action='bug_get_file.php' method='get'>\n<input id='${id}_button' type='button' value='Show' onclick='toggleShow(\"$id\", \"{$id}_button\", \"Show\", \"Hide\")' />\n";
		if($bugid > 0)
		{
			echo "<input type='submit' value='Download'/>\n";
			echo "<input type='hidden' name='type' value='$type'/>\n";
			echo "<input type='hidden' name='id' value='$bugid'/>\n";
		}
		echo "</form></td></tr>\n";
		echo "<tr><td><div id='$id' class='log'><pre>$content</pre></div></td></tr>\n";
	}

	if($plfile)
	{
		create_box('Source File:', 'plfile', $plfile, $id, 'plfile');
	}
	if($annfile)
	{
		create_box('Annotation File:', 'annfile', $annfile, $id, 'annfile');
	}
	if($goal)
	{
		create_box('Goal:', 'goal', $goal);
	}
	if($error)
	{
		create_box('Error Text:', 'error', $error);
	}
	if($recreateable)
	{
		switch($stageno)
		{
			case 1:
				$url = 'upload.php';
				break;
			case 2:
				$url = 'upload_annfile.php';
				break;
		}
		
		?><tr>
		<td>&nbsp;</td>
		<td>
		<form action='<? echo $url; ?>' method='get'>
		<input type='hidden' name='id' value='<? echo $id; ?>'/>
		<input type='submit' value='Reproduce Bug'/>
		</form>
		</td>
		</tr><?
	}
	echo "</table>\n";
}

html_end(); ?>
