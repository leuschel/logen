<?php
require_once('common.php');
require_session();

$pl = $_SESSION['filename'];
$filename = "$pl.gx";
header('Content-type: application/pl');
header("Content-Disposition: attachment; filename=\"$filename\"");
// we're lazy and store the gx in specfile as user can't request both at the
// same time
echo $_SESSION['specfile'];

?>
