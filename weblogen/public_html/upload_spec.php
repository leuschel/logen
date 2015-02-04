<?php 
require_once('common.php');
require_once('externals.php');
require_session();

if(isset($_POST['annotations']))
{
	// user clicked specialise without saving the annotations
	annotate();
}
$filename = $_SESSION['filename'];
$plfile = $_SESSION['plfile'];
$annfile = $_SESSION['annfile'];

function get_opt_and_save($name)
{
	$_SESSION[$name] = isset($_POST[$name]) ? 'on' : 'off';
	return $_SESSION[$name];
}

// have to do it this way since there's no difference between a disabled
// checkbox and an uncheck one. This way all options don't revert to off
// when you disable watch mode.
if(get_opt_and_save('watch') == 'on')
{
	$watch_opts = Array('enabled' => true,
						'builtins' => get_opt_and_save('watch_builtins'),
						'connectives' => get_opt_and_save('watch_connectives'),
						'infunfold' => get_opt_and_save('watch_infunfold'),
						'infmemo' => get_opt_and_save('watch_infmemo'),
						'backprop' => get_opt_and_save('watch_backprop'));
}
else
{
	$watch_opts = Array('enabled' => false);
}

$_SESSION['logging'] = $logging = $_POST['logging'];
$_SESSION['goal'] = $goal = $_POST['goal'];

$spec = isset($_POST['submit']) && $_POST['submit'] == 'Create GX' ? 2 : 1;

$tempdir = get_directory();

$file = fopen("$tempdir/$filename", "w");
fwrite($file, $_SESSION['plfile']);
fclose($file);

$file = fopen("$tempdir/$filename.ann", "w");
fwrite($file, $annfile);
fclose($file);

chronometer();
if($spec == 1)
{
	$retarray = specialise($filename, $goal, $tempdir, $watch_opts, $logging, $safe);
}
else
{
	$retarray = create_gx($filename, $tempdir, $watch_opts, $logging);
}
$time = chronometer();

if($retarray[0] == 0)
{
	// No error
	$_SESSION['specerror'] = '';
	html_start('Specialising : ' .  $_SESSION['filename'], 'prolog.css', '', 4, '');
	echo '<div class="codeframe" style="height: 490px">';
	echo process_xml($retarray[1], 'prologtohtml.xsl');
	echo '</div>';
	$log = $retarray[2];

	print_save_link_and_save_output_to_session($filename, $spec, $tempdir);

	global $ecce_url;
	?>

	<script type="text/javascript" src="getText.js"></script>
	<script type="text/javascript">
		function setprogram(name)
		{
			var source = getText(document.getElementById('source'));
			document.getElementById(name + 'source').value = source;
			return true;
		}
	</script>
	<p>
	<form style="display: inline">
	<input id="showlog" type="button" value="Show Log" onclick="toggleShow('logbox', 'showbox', 'Show Log', 'Hide Log')"/>
	</form>
<?
function respec_button($name, $url, $text, $source, $extra_tag=0, $extra_val=0)
{
	echo "<form style='display: inline' id='$name' method='post' action='$url' onsubmit=\"return setprogram('$name');\">\n";
	echo "<input type='submit' value='$text' />\n";
	if($extra_tag)
	{
		echo "<input type='hidden' name='$extra_tag' value='$extra_val' />\n";
	}
	echo "<input type='hidden' id='{$name}source' name='$source' value=''/>\n";
	echo "<input type='hidden' name='filename' value='$_SESSION[filename]' />";
	echo "\n</form>\n";
}
respec_button('logen', 'upload_initial.php', 'Respecialise', 'usertyped');
if(isset($ecce_url))
{
	respec_button('ecce', $ecce_url, 'Upload to Ecce', 'source', 'input file', 'Input');
}
if(isset($asap_url))
{
	respec_button('asap', $asap_url, 'Upload to ASAP', 'source', 'input file', 'Input');
}
?>
	</p>
	<? js_toggleShow(); ?>
	<div style="margin-top: 8px; overflow: auto; height: 400px" id="logbox" class="log">
		<? echo nl2br(htmlspecialchars($log)); ?>
	</div><?php
}
elseif($retarray[0] > 0)
{
	// Error while running logen command
	$error = $_SESSION['specerror'] = $retarray[2];
	$goal = htmlspecialchars($goal);
	if(preg_match('/\*\*\* UNSAFE CALL: (.*) \*\*\*/', $error, $matches))
	{
		?>
		html_start('Specialising : ' .  $_SESSION['filename'], 'prolog.css', '', 4, '');
		<p>Logen attempted to execute a predicate marked <em>call</em> which
		does not appear in the <em>whitelist</em>. This list contains predicates
		which are deemed not to pose a security risk.</p>
		<p>In this case the unsafe call was <b><? echo $matches[1]; ?> </b>.
		If you think this call is actually safe. Please submit a bug report so
		that we can add it to the whitelist.</p>
		<?
	}
	else if(preg_match('/\n\<\<\n(.*?)\>\>\n/s', $error))
	{
		redirect('upload_annfile.php?unsafe=1');
		exit;
	}
	else
	{
		html_start('Specialising : ' .  $_SESSION['filename'], 'prolog.css', '', 4, '');
		$error = htmlspecialchars($error);
		echo "<p>An error occured during specialisation:</p>\n";
		echo "<div class='log' style='display:block; overflow:auto'><pre>$error</pre></div>\n";
	}
	if(isset($db_name))
	{
		?>
		<form action="bug_report.php" method="post">
		<table border=0>
		<tr>
		<td>
		<input type="submit" value="Submit Bug Report" />
		<input type="hidden" name="stage" value="spec" />
		</td>
		<td>
		If you think this error is due to a bug in weblogen you can submit
		a bug report. Please make sure you have checked your annotations and
		filters and that your specified goal is compatible with them.
		</td>
		</tr>
		</table>
		</form>
		<?
	}
}
elseif($retarray[0] < 0)
{
	$error = $_SESSION['specerror'] = $retarray[2];
	$goal = htmlspecialchars($goal);
	html_start('Specialising : ' .  $_SESSION['filename'], 'prolog.css', '', 4, '');
	echo "<p>An error occured while highlighting the specialisation output:</p>\n";
	echo "<pre>$error</pre>\n";
	if(isset($db_name))
	{
		?>
		<form action="bug_report.php" method="post">
		<table border=0>
		<tr>
		<td>
		<input type="submit" value="Submit Bug Report" />
		<input type="hidden" name="stage" value="spec" />
		</td>
		<td>
		This is almost certainly a bug in Weblogen. Please submit it so that our
		developers can solve it.
		</td>
		</tr>
		</table>
		</form>
		<?
	}
	print_save_link_and_save_output_to_session($filename, $spec, $tempdir);
}

delDir($tempdir);

html_end();

function print_save_link_and_save_output_to_session($filename, $type, $tempdir)
{
	if($type == 1)
	{
		$specfilename = substr($filename, 0, strlen($filename) - 3) . '.spec';
	}
	else
	{
		$specfilename = "$filename.gx";
	}

	$_SESSION['specfile'] = file_get_contents("$tempdir/$specfilename");

	if($type == 1)
	{
		echo '<p><a href="get_spec.php">Download residual program</a></p>';
	}
	else
	{
		echo '<p><a href="get_gx.php">Download generating extension</a></p>';
	}
}
?>
