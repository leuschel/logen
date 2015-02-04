<?
error_reporting(E_ALL);
require_once('header.php');
require_once('footer.php');

if(!@include('config.php'))
{
	html_start('Weblogen not configured', '', '');
	?><h1>Weblogen not configured</h1>
	<p>To configure weblogen a valid config.php file must exist.</p>
	</body></html><?
	exit(1);
}
	
if(!isset($safe))
{
	$safe = true;
}

$frontend_dir = dirname($_SERVER['SCRIPT_FILENAME']);
$CHRONO_STARTTIME = 0;

// create a uniquely named temporary directory and return the name.
// returns '' on an error.
function get_directory()
{
	global $backend_dir;
	$file = popen("$backend_dir/tempdir /tmp/weblogen-", 'r');
	if(!feof($file) && strlen($tempdir = fread($file, 8192)) > 0)
	{
		#$tempdir = substr($tempdir, strlen('Directory created : '));
		$tempdir = trim(substr($tempdir, 20));
	}
	else
		die('<b>Cannot create temporary directory!</b><br>');
	pclose($file);
	return $tempdir;
}

// Function to list files in examples dir
// outputs combo list (used on stock open)
function showStock($dir, $pattern)
{
	//$dir = "./examples/";
	//$pattern = "*.pl";    
	if (is_dir($dir))
	{
		if ($dh = opendir($dir))
		{
			while (($filename = readdir($dh)) !== false)
			{
				if (fnmatch($pattern, $filename))
				{

					echo "<option value=\"" . $filename ."\"";

					if (isset($_POST['filename']) && $filename == $_POST['filename']){
						echo "selected=\"selected\"";
					}
					echo ">" . $filename . "</option>\n";
				}
			}
			closedir($dh);
		}
	}
}

// Get full path of a stock file
function stock_file($filename, $suffix)
{
	global $frontend_dir;

	// Check the file is actually a prolog file. Don't want people tricking
	// system into loading arbitrary files on the server.
	if(substr($filename, strlen($filename) - strlen($suffix)) != $suffix)
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

// Function obtained from http://uk.php.net/manual/en/function.rmdir.php
// posted by czambran at gmail dot com (06-Mar-2005)
function delDir($dirName)
{
	// change directory because bug in sablotron means deleting current
	// directory causes problems
	chdir('/');
	global $backend_dir;
	if(empty($dirName))
		return true;
	if(file_exists($dirName))
	{
		$dir = dir($dirName);
		while($file = $dir->read())
		{
			if($file != '.' && $file != '..')
			{
				if(is_dir($dirName.'/'.$file))
					delDir($dirName.'/'.$file);
				else
					@unlink("$dirName/$file") 
						or die("File $dirName/$file couldn't be deleted!");
			}
		}
		$dir->close();

		@rmdir($dirName) or die("Folder $dirName couldn't be deleted!");
	}
	else
		return false;
	return true;
}

function chronometer()
{
	global $CHRONO_STARTTIME;
	$now = microtime(TRUE) + time();

	if ($CHRONO_STARTTIME > 0) {
		$retElapsed = round(($now - $CHRONO_STARTTIME), 4);
		$CHRONO_STARTTIME = $now;
		return $retElapsed;
	} else {
		// Start the chronometer : save the starting time
		$CHRONO_STARTTIME = $now;
		return '';
	}
}

function get_filters_from_xml($xml)
{
	return process_xml($xml, 'getfilter.xsl');
}

// Apply an xslt stylesheet to the xml and output the resulting html.
function process_xml($xml, $stylesheet)
{
	global $backend_dir;

	chronometer();
	if(function_exists('xslt_create'))
	{
		$arguments = array('/_xml' => $xml);
		$xh = xslt_create();

		$result = xslt_process($xh, 'arg:/_xml',
				"$backend_dir/$stylesheet",
				NULL, $arguments);
	}
	else if(class_exists('XSLTProcessor'))
	{
		$xsl = new XSLTProcessor();
		
		$xsl->importStyleSheet(DOMDocument::load("$backend_dir/$stylesheet"));
		$result = $xsl->transformToXML(DOMDocument::loadXML($xml));
	}
	else
	{
		?><h1>XSL support not present</h1>
		<p>To use weblogen, you need to recompile PHP with either:</p>
		<ul>
		<li><a href="http://www.php.net/manual/ref.xslt.php">php4_xslt under php4.</a></li>
		<li><a href="http://www.php.net/manual/ref.xsl.php">XSL and XML under php5.</a></li>
		</ul>
		</body></html><?
		exit(1);
	}
	//echo 'Time taken by xslt : ', chronometer(), '<br>';
	return $result;
}

function create_session()
{
	session_start();
	$_SESSION['started'] = 1;
	// This adds all the variables to the session so we don't have to check for
	// their existence before accessing them.
	$session_vars = array('filename', 'plfile', 'annfile', 'logging',
						  'annerror', 'watch', 'editerror', 'filter',
						  'annmethod', 'filter_prop', 'norm', 'goal',
						  'stock', 'stock_annfile', 'watch', 'logging',
						  'plerror', 'stage', 'specerror', 'newbug',
						  'watch_builtins', 'watch_connectives',
						  'watch_infunfold', 'watch_infmemo', 'watch_backprop');
	foreach($session_vars as $var)
	{
		if(!isset($_SESSION[$var]))
		{
			$_SESSION[$var] = '';
		}
	}
}

function require_session()
{
	session_start();
	if(!isset($_SESSION['started']))
	{
		die('Session has either expired or was never started.');
	}
}

function annotation_javascript($extras)
{
	include('annotation_js.php');
}

function upload_failure($error)
{
	switch($error)
	{
		case UPLOAD_ERR_FORM_SIZE:
		case UPLOAD_ERR_INI_SIZE:
			echo 'The specified file is too large.';
			break;
		case UPLOAD_ERR_PARTIAL:
			echo 'The specified file was only partially uploaded.';
			break;
		case UPLOAD_ERR_NO_FILE:
			echo 'No file was uploaded.';
			break;
		default:
			echo 'Internal weblogen error. Please contact system administrator.';
			break;
	}
}

function file_upload_error($filetype, $retry_url, $error)
{
	html_start($filetype . 'File Upload Failure', '', '', '', '');
	?>
		<h2><? echo $filetype; ?> File Upload Failure</h2>
		<p>The following error occurred while trying to upload a file
			to weblogen.</p>
		<p><? upload_failure($error); ?></p>
		<p><a href="<? echo $retry_url; ?>">Please try again.</a></p>
<?
	html_end();
}

function stock_file_error($filetype)
{
	html_start("Stock $filetype Selection Failure", '', '');
	?>
		<h2>Stock <? echo $filetype; ?> Selection Failure</h2>
		<p>
			An error occurred with your selection of a stock file. Please
			contact the weblogen system administrator for help.
		</p>
<?
	html_end();
}

function redirect($page)
{
	header('Location: http://' . $_SERVER['HTTP_HOST'] .
       dirname($_SERVER['PHP_SELF']) . '/' . $page);
	exit(0);
}

function report_xml_error($title, $xml)
{
	html_start($title, '', '');
	echo process_xml($xml, 'error.xsl');
	html_end();
}

function output_select($name, $options, $long_options, $default)
{
	echo "<select name='$name'>\n";
	for($i = 0; $i < count($options); $i++)
	{
		echo "<option value='$options[$i]'";
		if($options[$i] == $default)
		{
			echo " selected=\"selected\"";
		}
		echo ">$long_options[$i]</option>\n";
	}
	echo "</select>\n";
}

function get_preferences()
{
	$prefs = array();
	if(isset($_COOKIE['weblogen_prefs']))
	{
		$pref_array = explode(':', $_COOKIE['weblogen_prefs']);
		for($i = 0; $i < count($pref_array); $i += 2)
		{
			$prefs[$pref_array[$i]] = $pref_array[$i + 1];
		}
		return $prefs;
	}
	return $prefs;
}

function get_preference($prefs, $item)
{
	return isset($prefs[$item]) ? $prefs[$item] : '';
}

function annotate()
{
	require_once('externals.php');
	$filename = $_SESSION['filename'];

	$tempdir = get_directory();

	chdir($tempdir);

	$file = fopen("$tempdir/$filename", "w");
	fwrite($file, $_SESSION['plfile']);
	fclose($file);

	$annotations = $_POST['annotations'];

	$annotations = '[' . $annotations . '].';
	if(isset($_POST['filters']))
	{
	    $filters = $_POST['filters'];
	}
	else
	{
		$filters = $_SESSION['filters'];
	}

	$output = modify_annotations("$tempdir/$filename",
								 "$tempdir/$filename.ann",
								  $annotations, $filters);

	if($output[0] != 0)
	{
		$_SESSION['editerror'] = $output[2];
		delDir($tempdir);
		if(isset($_POST['textedit']))
		{
			redirect('edit_filters.php');
		}
		else
		{
			redirect('upload_annfile.php');
		}
	}

	$_SESSION['editerror'] = '';

	$_SESSION['annfile'] = file_get_contents("$tempdir/$filename.ann");
	$_SESSION['filters'] = $filters;

	delDir($tempdir);
}

function parse_and_display_upload_error($error)
{
	if(preg_match('/error\(syntax_error\((.*)\), file\((.*), ([0-9]+), ([0-9]+), ([0-9]+)\)\)/', $error, $matches))
	{
		$errtype = $matches[1];
		$file = $matches[2];
		$line = $matches[3];
		$col = $matches[4];
		$char = $matches[5];
		?>
		<script type="text/javascript">
		function setSelectedTextRange(elm, selectionStart, selectionEnd)
		{
			if(elm.setSelectionRange)
			{
				elm.focus();
				elm.setSelectionRange(selectionStart, selectionEnd);
			}
			else if(elm.createTextRange)
			{
				var range = elm.createTextRange();
				range.collapse(true);
				range.moveEnd('character', selectionEnd);
				range.moveStart('character', selectionStart);
				range.select();
			}
		}

		function scrollAndSetCaretPos(elm, pos, row)
		{
			setSelectedTextRange(elm, pos, pos);
			var charHeight = elm.offsetHeight / 30;
			elm.scrollTop = charHeight * row - elm.clientHeight / 2;
		}
		</script>
		<?
		echo "<p>Syntax error: <b>$errtype</b> in $file at line <b>$line</b> column <b>$col</b>. ";
		echo "<input type='button' value='Move to error' onclick='scrollAndSetCaretPos(document.getElementById(\"ta\"), $char, $line);'/></p>";
	}
	else
	{
		$error = htmlspecialchars($error);
		echo "\n<div class='log' style='display: block; overflow:auto'><pre>$error</pre></div>";
	}
}

function print_structure($x)
{
	echo '<pre>';
	print_r($x);
	echo "</pre>\n";
}

// writes a javascript function called toggleShow
function js_toggleShow($script = true)
{
if($script)
	echo "<script type='text/javascript'>\n";
?>
function toggleShow(logid, buttonid, showtext, hidetext)
{
	var button = document.getElementById(buttonid);
	var logbox = document.getElementById(logid);
	if(logbox.style.display == "block")
	{
		logbox.style.display = "none";
		button.value = showtext;
	}
	else
	{
		logbox.style.display = "block";
		button.value = hidetext;
	}
}
<?
if($script)
	echo "</script>\n";
}

function sanitise_input_file($file)
{
	//Convert line breaks
	$file = str_replace("\r\n", "\n", $file);
	$file = str_replace("\r", "\n", $file);
	// Remove redundant newlines
	$file = preg_replace("/\n(\n)+/", "\n\n", $file);
	$file = trim($file) . "\n";
	if(get_magic_quotes_gpc())
	{
		$plfile = stripslashes($file);
	}
	return $file;
}

$default_watch = isset($default_watchdog) && $default_watchdog ? 'on' : 'off';
$default_opt = Array('watch' => $default_watch, 'watch_builtins' => 'on',
					 'watch_connectives' => 'on', 'watch_infunfold' => 'on',
					 'watch_infmemo' => 'on', 'watch_backprop' => 'off',
					 'logging' => '', 'goal' => '');

function default_opt($opt)
{
	global $default_opt;
	return isset($default_opt[$opt]) ? $default_opt[$opt] : '';
}

function tooltip_js()
{
	?>
<script type="text/javascript" src="newtooltip.js">

/***********************************************
* Show Hint script- © Dynamic Drive (www.dynamicdrive.com)
* This notice MUST stay intact for legal use
* Visit http://www.dynamicdrive.com/ for this script and 100s more.
***********************************************/

</script>
	<?
}

function insert_tooltip($text, $size)
{
	// this will wrap it in single quotes and escape any single quotes inside it
	$text = escapeshellarg($text);
	echo "<a class='tooltip' href='#' class='hintanchor' ";
	echo "onMouseover=\"showhint($text, this, event, '$size')\">[?]</a>";
}

// You can create individual timeouts for various processes
// look in externals.php for their names, but basically you set elements in
// $timeouts to the value you want. E.g. $timeouts['specialise'] = 20;
// unset values default to $maxtime. If $maxtime doesn't exist it defaults to
// 10 seconds.
function get_timeout($program)
{
	global $timeouts, $maxtime;
	if(isset($timeouts[$program]))
		return $timeouts[$program];
	else
		return isset($maxtime) ? $maxtime : 10;
}
?>
