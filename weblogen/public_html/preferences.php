<?php
require_once('common.php');
require_session();

$reforig = $_SERVER['HTTP_REFERER'];
$refurl = parse_url($reforig);
$refurl = $refurl['path'];
$refurl = strrchr($refurl, '/');

switch($refurl)
{
	case '/upload.php':
		$ref = 2;
		break;
	case '/upload_annfile.php':
		$ref = 3;
		break;
	case '/upload_spec.php':
		$ref = 4;
		break;
	case '/open.php':
	default:
		$ref = 1;
		break;
}

html_start('Weblogen Preferences', '', '', 0,"Preferences");

$prefs = get_preferences();
?>
<form action="setpreferences.php" method="post">
	<p>Display Annotated Programs and Filters
<?
output_select('display', array('horiz', 'vert'),
			  array('Horizontally', 'Vertically'),
			  get_preference($prefs, 'display'));
?>
	</p>
	<p>
	<input type="hidden" name="currentstage" value="<? echo $ref; ?>"/>
	<input type="submit" value="Save Preferences"/>
	</p>
</form>
<?

html_end();
?>
