<?php
require_once('common.php');
require_session();

$pl = $_SESSION['filename'];
// Remove .pl and append .spec
$filename = substr($pl, 0, strlen($pl) - 3) . '.spec';
header('Content-type: application/pl');
header("Content-Disposition: attachment; filename=\"$filename\"");
echo $_SESSION['specfile'];

?>
