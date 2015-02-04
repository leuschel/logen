<?php 
function html_start($title, $stylesheet, $extras, $curstage, $subtitle='')
{
?><!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
    "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
	<head>
		<title><?php echo $title; ?></title>
		<meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
		<link type="text/css" rel="stylesheet" href="weblogen.css"/>
<?php if($stylesheet != '')
{
	echo '		<link type="text/css" rel="stylesheet" href="';
	echo "$stylesheet\" />\n";
}
if($extras == 'javascript' || $extras == 'javascript-always-save')
{
	annotation_javascript($extras);
}
?>	

</head>
	<body>

	<!-- Main page starts here. Add site specific content -->
<table cellpadding="2" width="100%" border="1" cellspacing="0">
<tr><td width="100%" colspan="4" class="AppHeader">
WebLogen - Partial Evaluation for Logic Programs
</td>
</tr><tr>
<?php
if($curstage > 0)
{ 
	$lateststage = max(1, $_SESSION['stage']);
	$stages = array("Open File", "Annotate", "Specialise", "Result");
	$links = array("open.php","upload.php", "upload_annfile.php","upload_spec.php");
	foreach ($stages as $i => $s){
		$reali = ($i +1);
		if ($reali == $curstage){
			$class = "selected";
		}else{
			$class = "unselected";
		}
		if ($reali <= $lateststage) {
			$link = "href=\"$links[$i]\"";
		}else{
			$link = "";
		}

		echo "<td class=\"$class\"><a $link>$reali. $s</a></td>";
	}
}else{
	/* yes this looks crappy - but at least it fits the colour scheme.....
	   we should work out what we want for misc pages */
	echo "<td class=\"selected\"><a href=\"\">$subtitle</a></td>";
}

?>

</tr>
<tr>
<td colspan="4" class="AppMain">

<?php

}
?>
