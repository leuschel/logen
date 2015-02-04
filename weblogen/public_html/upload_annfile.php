<?php
require_once('common.php');
require_once('externals.php');
require_session();

if(isset($db_name) && isset($_GET['id']))
{
	// This page was linked from a bug report, so attempt to recreate the bug
	// ignore if no db is defined
	$id = $_GET['id'];
	if(!is_integer($id) && $id < 1)
	{
		die('Bug id must be a positive integer');
	}
	$_SESSION['filename'] = 'user.pl';
	require('bug_common.php');
	$result = get_bug_details($id);
	if(!$result)
	{
	    die("Bug $id does not exist");
	}
	$_SESSION['plfile'] = $result['plfile'];
	$_SESSION['annfile'] = $result['annfile'];
	$_SESSION['norm'] = '';
	$_SESSION['annmethod'] = '';
	$_SESSION['filter_prop'] = '';
	$_SESSION['stock'] = '';
}

if(isset($_GET['unsafe']))
{
	require('annerrors.php');
	$unsafe = parse_spec_error($info);
}
else
{
	$unsafe = false;
}

$filename = basename($_SESSION['filename']);
$annfile = $_SESSION['annfile'];

$tempdir = get_directory();

chdir($tempdir);

$file = fopen("$tempdir/$filename", "w");
fwrite($file, $_SESSION['plfile']);
fclose($file);

$file = fopen("$tempdir/$filename.ann", "w");
fwrite($file, $annfile);
fclose($file);

if($unsafe)
{
	$output = show_annotated($filename, $info);
}
else
{
	$output = show_annotated($filename);
}

if($output[0] == 0)
{
	// no error

	// Store raw filters in the session
	$xml = $output[1];
	$_SESSION['filters'] = get_filters_from_xml($xml);
	$_SESSION['filters'] = trim($_SESSION['filters']);
	$_SESSION['annerror'] = '';
}
else
{
	$_SESSION['annerror'] = $output[2];
	delDir($tempdir);
	if($unsafe)
	{
		echo "<h1>Error displaying annotations</h1><pre>{$output[2]}</pre>";
		exit;
	}
	redirect('upload.php');
}

delDir($tempdir);

html_start("Annotated Prolog file : ".$_SESSION['filename'],
	   'prolog.css', 'javascript', 3, '');

if($_SESSION['editerror'])
{
	$error = htmlspecialchars($_SESSION['editerror']);
	echo "<p>An error occurred while editing the annotations:</p>\n";
	echo "<div class='log' style='display: block; overflow:auto'><pre>$error</pre></div>\n";
	if(isset($db_name))
	{
		?>
		<form action="bug_report.php" method="post">
		<table border=0>
		<tr>
		<td>
		<input type="submit" value="Submit Bug Report" />
		<input type="hidden" name="stage" value="editann" />
		</td>
		<td>
		If you think this error is due to a bug in weblogen you can submit
		a bug report. If you edited the filters manually, then please ensure
		your new filters are structured properly.
		</td>
		</tr>
		</table>
		</form>
		<?
	}
}
else if($unsafe)
{
	display_spec_error();
}
?>
<form id="spec_form" action="upload_spec.php"
	  onsubmit="return checkAnnotationsSpecialise(this)" method="post">
<table width="100%">
<tr>
<td>
<?
if($_SESSION['stock'] == true)
{
	if(file_exists("$frontend_dir/examples/$filename.goals"))
	{
		$goals = file_get_contents("$frontend_dir/examples/$filename.goals");
		$goal_list = explode("\n", $goals);
?>
	<p>Example goals
<select name="stockgoal" onchange="useStockGoal(this.options[this.selectedIndex].value);">
<option></option>
<?
foreach($goal_list as $goal)
{
	if($goal != "")
	{
		$goal = htmlspecialchars($goal);
		print "<option value=\"$goal\">$goal</option>\n";
	}
}
?>
</select>
<? insert_tooltip('These example goals are selected to demonstrate the capabilities of Weblogen. Please note that they were designed to be used with the ready made annotations and are not guaranteed to work if other annotation methods are used or if the annotations are altered.', '400px'); ?>
</p>
<?
	}
}
function checkbox($var, $onchange = '')
{
	echo "<input type='checkbox' name='$var' ";
	$val = $_SESSION[$var] ? $_SESSION[$var] : default_opt($var);
	echo $val == 'on' ? 'checked ' : '';
	if($onchange)
		echo " onchange='$onchange'";
	echo "/>";
}
$logging = $_SESSION['logging'];
$goal = $_SESSION['goal'];
?>
<p>
Goal: 
<input type="text" name="goal" id="goal" value="<?php echo $goal; ?>"/>
<!-- match([a,b], X) -->
<input name="submit" type="submit" onclick="gx = false;" value="Specialise"/>
<input name="submit" type="submit" onclick="gx = true;" value="Create GX"/>
<? insert_tooltip('Enter the goal you wish to specialise in the entry on the left. This need not be terminated by a full stop. Clicking <b>Specialise</b> will then produce the residual program. <b>Create GX</b> will produce a generating extension that you can run yourself. This is especially useful if your program contains built-ins which are considered unsafe and which Weblogen will refuse to execute.', '400px'); ?>
</p>
</td>
<td style="border: 1px solid black; width: 220px">
<p style="margin-bottom: 4px">Watchdog Mode 
<? checkbox('watch', 'changeDisableOptions(!this.checked)'); ?>
<input id="watchmore" type="button" value="More..." onclick="toggleShow('watch_opt', 'watchmore', 'More...', 'Less...');" /></p>
<div id="watch_opt" style="margin-top: 4px; display: none">
<p style="margin-top: 4px; margin-bottom: 2px;">Builtins <? checkbox('watch_builtins'); ?>
Connectives <? checkbox('watch_connectives'); ?></p>
<p style="margin-top: 4px; margin-bottom: 2px;">Infinite unfold <? checkbox('watch_infunfold'); ?>
memo <? checkbox('watch_infmemo'); ?></p>
<p style="margin-top: 4px; margin-bottom: 2px;">Back propagation <? checkbox('watch_backprop');
if($_SESSION['watch'] == 'off')
{
	echo '<script type="text/javascript">changeDisableOptions(true)</script>';
}
insert_tooltip("<b>Watchdog Mode</b> detects problems with the specialisation process.<br/>Enable <b>builtins</b> to detect improperly instantiated builtin calls.<br/>Enable <b>connectives</b> to detect back-propagation of bindings in connectives (if,or)<br/>Enable <b>infinite unfold</b>/<b>memo</b> to detect infinite unfolding or memoisation<br/>Enable <b>back propagation</b> to watch for back-propagation of bindings onto non-declarative builtins", '450px'); ?></p>
</div>
<p>Logging <? output_select('logging', array(0, 1, 2),
							array('Normal', 'Verbose', 'Very Verbose'),
							$logging); ?></p>
</td>
</tr>
</table>
</form>
<form action="new_annotations.php" method="post" onsubmit="return submitAnnotations(this.form)">

<div id="menu" style="visibility:hidden;"></div>
<?
$prefs = get_preferences();
if(get_preference($prefs, 'display') == 'vert')
{
	?>
<h2>Source Code</h2>
<? echo process_xml($xml, 'prologtohtml.xsl'); ?>
<p id="currentline">&nbsp;</p>
<h2>Filters <a href="#" onclick="checkAnnotations('edit_filters.php')"><span style="font-size: 12px">(edit)</span></a></h2>
<? echo process_xml($xml, 'filterstohtml.xsl');
}
else
{
	?>
<table width="100%">
<tr>
<td align="center">
<span class="paneheading">Source Code</span>
</td>
<td align="center">
<span class="paneheading">Filters <a href="#" onclick="checkAnnotations('edit_filters.php')"><span style="font-size: 12px">(edit)</span></a></span>
</td>
</tr>
<tr>
<td valign="top" width="50%">
<div class="codeframe" id="sourcebox">
<? echo process_xml($xml, 'prologtohtml.xsl'); ?>
</div>
<? if($unsafe) { generate_scroll_to_error_code('sourcebox'); } ?>
</td>
<td valign="top" width="50%">
<div class="codeframe" id="filterbox">
<? echo process_xml($xml, 'filterstohtml.xsl'); ?>
</div>
<? if($unsafe) { generate_scroll_to_error_code('filterbox'); } ?>
</td>
</tr>
</table>
<?
}
?>
<p>
<input id="filters_input" type="hidden" name="filters" value=""/>
<input id="annotations" type="hidden" name="annotations" value=""/>
<input type="submit" value="Save Changes"/>
<input type="button" value="Revert Changes" onclick="window.location='upload_annfile.php'"/>
<input type="button" value="Edit Annotations Manually" onclick="window.location='manual_edit.php'"/>
</p>
</form>

<p><a href="#" onclick="checkAnnotations('get_annfile.php')">Download annotation file</a></p>
<p><a href="preferences.php">Change Preferences</a></p>
<?php
html_end();
?>
