<?php
require_once('common.php');
require_session();

$_SESSION['plfile'] = $_POST['plfile'];
$_SESSION['annfile'] = $_POST['annfile'];

redirect('upload_annfile.php');
?>
