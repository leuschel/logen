<?
// Function taken from
// http://www.php.net/manual/en/function.mysql-real-escape-string.php
// should prevent sql injection attacks
function quote_smart($value)
{
	// Stripslashes
	if (get_magic_quotes_gpc()) {
		$value = stripslashes($value);
	}
	// Quote if not integer
	if (!is_numeric($value)) {
		$value = "'" . mysql_real_escape_string($value) . "'";
	}
	return $value;
}

function check_db_config_and_connect()
{
	global $db_name, $db_username, $db_password;
	if(!isset($db_name) || !isset($db_username) || !isset($db_password))
	{
		die('Database is not configured. Please ask administrator to set up bug reporting.');
	}
	$db = mysql_connect('localhost', $db_username, $db_password)
		or die('Could not connect: ' . mysql_error());
	mysql_select_db($db_name) or die('Could not select database');
	return $db;
}

// assumes id and type are already validated
function get_bug_file($id, $type)
{
	global $db_name, $db_username, $db_password;
	check_db_config_and_connect();
	// $id is not quoted because the calling function already checked that it
	// is valid. Do not call this function with unchecked input!
	$query = "SELECT $type FROM Bugs WHERE bugid=$id";
	$result = mysql_query($query);
	if(!$result)
	{
		die("Failed to get bug file ($type) : " . mysql_error());
	}
	$row = mysql_fetch_assoc($result);
	return $row[$type];
}

function get_bug_details($id)
{
	global $db_name, $db_username, $db_password;
	check_db_config_and_connect();
	// $id is not quoted because the calling function already checked that it
	// is valid. Do not call this function with unchecked input!
	$query = "SELECT stage, status, email, sdate, errortext, plfile, annfile, comment, goal FROM Bugs WHERE bugid=$id";
	$result = mysql_query($query);
	if(!$result)
	{
		die('Failed to get bug summaries : ' . mysql_error());
	}
	$row = mysql_fetch_assoc($result);
	return $row;
}

function get_bug_summaries()
{
	global $db_name, $db_username, $db_password;
	check_db_config_and_connect();
	$query = "SELECT bugid, stage, status, sdate FROM Bugs ORDER BY bugid";
	$result = mysql_query($query);
	if(!$result)
	{
		die('Failed to get bug summaries : ' . mysql_error());
	}
	return $result;
}

// returns the id of the bug created
function create_new_bug($stage, $email, $comment, $errortext, $plfile, $annfile, $goal='')
{
	global $db_name, $db_username, $db_password;
	check_db_config_and_connect();

	$goal = ($goal == '') ? 'NULL' : quote_smart($goal);
	if($stage == 'general')
	{
		$stage = 0;
	}
	else if($stage == 'upload')
	{
		$stage = 1;
	}
	else if($stage == 'uploadann')
	{
		$stage = 2;
	}
	else if($stage == 'editann')
	{
		$stage = 3;
	}
	else if($stage == 'spec')
	{
		$stage = 4;
	}
	else
	{
		die('unknown stage');
	}
	
	$query = sprintf("INSERT INTO Bugs (stage, status, email, sdate, errortext,
										plfile, annfile, comment, goal)
									   values
									   (%s, 1, %s, NOW(), %s, %s, %s, %s, %s)",
					 $stage,
					 quote_smart($email),
					 quote_smart($errortext),
					 quote_smart($plfile),
					 quote_smart($annfile),
					 quote_smart($comment),
					 $goal);
	mysql_query($query) or die('Failed to insert bug report: '. mysql_error());
	
	// Now send emails announcing new bug
	$bugid = mysql_insert_id();
	$subject = "Bug $bugid created";
	global $admin_email, $bug_email, $url;
	$message = <<<EOD
Bug $bugid has just been registered.

You can view this bug at:
$url/bug_view?id=$bugid

Weblogen Bug Tracking System
EOD;
	if(isset($bug_email) && $bug_email)
	{
		mail($bug_email, $subject, $message, null, "-f$admin_email");
	}
	return $bugid;
}

function number2stage($num)
{
	switch($num)
	{
		case 0:
			return 'general';
		case 1:
			return 'upload';
		case 2:
			return 'uploadann';
		case 3:
			return 'editann';
		case 4:
			return 'spec';
		default:
			return 'Unknown';
	}
}

function number2status($num)
{
	switch($num)
	{
		case 1:
			return 'new';
		default:
			return 'Unknown';
	}
}

function bug_recreateable($num)
{
	return $num >= 1 && $num <= 3;
}
?>
