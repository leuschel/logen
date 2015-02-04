<?php 
require_once('common.php');
require_once('bug_common.php');
create_session();
html_start('WebLogen - Bug List','','',0, "Bug List");
?>
<table>
<tr>
<th>ID</th><th>Stage</th><th>Submit Date</th><th>Status</th>
</tr>
<?
$result = get_bug_summaries();
while($row = mysql_fetch_assoc($result))
{
	$id = $row['bugid'];
	$stage = number2stage($row['stage']);
	$date = $row['sdate'];
	$status = number2status($row['status']);
	echo "<tr><td><a href='bug_view.php?id=$id'>$id</a></td>";
	echo "<td>$stage</td><td>$date</td><td>$status</td></tr>\n";
}

echo '</table>';

html_end(); ?>
