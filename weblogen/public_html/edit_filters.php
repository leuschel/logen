<?php
require_once('common.php');
require_once('externals.php');
require_session();
html_start('Edit filters : ' . $_SESSION['filename'],
		   'prolog.css', 'javascript-always-save', 2, '');
if($_SESSION['editerror'])
{
	$error = htmlspecialchars($_SESSION['editerror']);
	echo "<p>An error occurred while editing the filters:</p>\n";
	echo "<pre>$error</pre>";
}
?>
<form action="new_annotations.php" method="post"
	  onsubmit="return submitAnnotations(this.form)">
<?php

$filename = $_SESSION['filename'];
$annfile = $_SESSION['annfile'];

$tempdir = get_directory();

chdir($tempdir);

$file = fopen("$tempdir/$filename", "w");
fwrite($file, $_SESSION['plfile']);
fclose($file);

$file = fopen("$tempdir/$filename.ann", "w");
fwrite($file, $annfile);
fclose($file);

$output = show_annotated($filename);

if($output[0] == 0)
{
	$xml = $output[1];
	$_SESSION['annerror'] = '';
}
else
{
	$_SESSION['annerror'] = $output[1];
	redirect('upload.php');
}

delDir($tempdir);
$prefs = get_preferences();
if(!isset($prefs['display']) || $prefs['display'] == 'horiz')
{
    ?>
<table width="100%">
<tr>
<td align="center">
<span class="paneheading">Source Code</span>
</td>
<td align="center">
<span class="paneheading">Filters</span>
</td>
</tr>
<tr>
<td valign="top" width="50%">
<div class="codeframe" id="sourcebox">
<? echo process_xml($xml, 'prologtohtml.xsl'); ?>
</div>
</td>
<td valign="top" width="50%">
<div class="codeframe">
<textarea name="filters" style="width: 99%; height: 98%">
<? echo $_SESSION['filters']; ?>
</textarea>
</div>
</td>
</tr>
</table>
<?
}
else
{
	echo '<h2>Source Code</h2>';
	echo process_xml($xml, 'prologtohtml.xsl');
?>
<h2>Filters</h2>
<input type="hidden" name="textedit" value="on">
<textarea rows="30" cols="80" name="filters">
<?  echo $_SESSION['filters'];
	echo "</textarea>\n";
}
?>

<p>
<input id="annotations" type="hidden" name="annotations" value=""/>
<input type="submit" value="Save Changes"/>
<input type="button" value="Revert Changes" onclick="window.location='upload_annfile.php'"/>
</p>
</form>
<?php
html_end();
?>
