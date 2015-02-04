<?php
require_once('common.php');
require_session();

$filename = $_SESSION['filename'] . '.ann';
header('Content-type: application/pl');
header("Content-Disposition: attachment; filename=\"$filename\"");
echo $_SESSION['annfile'];

?>
