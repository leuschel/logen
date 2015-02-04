<?php
require_once('common.php');
require_once('externals.php');
require_session();

$annfile = $_SESSION['annfile'];
$plfile = $_SESSION['plfile'];

html_start("Manual Edit : ".$_SESSION['filename'],
	   '', '', 3, '');

?>
<form action="upload_manual_edit.php" method="post">
<?
$prefs = get_preferences();
if(get_preference($prefs, 'display') == 'vert')
{
	?>
<h2>Source Code</h2>
<textarea name="plfile" style="width: 99%" rows="30"><?
echo $plfile;
?></textarea>
<h2>Annotation File</h2>
<textarea name="annfile" style="width: 99%" rows="30"><?
echo $annfile;
?></textarea><?
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
<span class="paneheading">Annotation File</span>
</td>
</tr>
<tr>
<td valign="top" width="50%">
<textarea name="plfile" style="width: 99%" rows="28"><?
echo $plfile;
?></textarea>
</td>
<td valign="top" width="50%">
<textarea name="annfile" style="width: 99%" rows="28"><?
echo $annfile;
?></textarea>
</td>
</tr>
</table>
<?
}
?>
<p>
<input type="submit" value="Save Changes"/>
<input type="button" value="Revert Changes" onclick="window.location='manual_edit.php'"/>
<input type="button" value="Back" onclick="window.location='upload_annfile.php'"/>
</p>
</form>

<p><a href="preferences.php">Change Preferences</a></p>
<?php
html_end();
?>
