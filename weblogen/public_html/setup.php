<html>
<head>
	<title>Weblogen Testing</title>
</head>
<? 
$passed = true; $recompile = false; $config = false;
?>
<body>
	<h1>Weblogen Testing</h1>

	<ul>
		<li>
			<i>magic_quotes_gpc</i> = 
<?
if(get_magic_quotes_gpc())
{
	echo '<span style="color: #ff0000"><b>on</b></span>';
	$passed = false;
	$config = true;
}
else
{
	echo "off";
}
?>
		</li>
		<li><i>XSLT Support = </i>
<?
if(function_exists('xslt_create'))
{
	echo 'xslt_create (PHP4)';
}
else if(class_exists('XSLTProcessor'))
{
	echo 'XSLTProcessor (PHP5)';
}
else
{
	echo '<span style="color: #ff0000"><b>Not present</b></span>';
	$passed = false;
	$recompile = true;
}

?>
		</li>
		<li>Process return proper error codes: 
<?
$res1 = run_process_with_input('false', '');
$res2 = run_process_with_input('true', '');
if($res1 == 1 && $res2 == 0)
{
	echo 'Yes';
}
else
{
	echo "No ($res1 $res2)";
	$passed = false;
	$recompile = true;
}
?>
		</li>
	</ul>

<?
if($passed)
{
	echo "<p>Weblogen should work just fine.</p>";
	echo '<p>Now you can use <a href="http://';
	echo $_SERVER['HTTP_HOST'] . dirname($_SERVER['PHP_SELF']);
	echo '/">Weblogen</a>.</p>';
}
else
{
	if($config)
	{
		?><p>Some configuration options are not set up correctly. You will
		need to edit your php.ini to change any settings which are marked
		</p><?
	}
	if($recompile)
	{
		?><p>
		Some components are missing which are required by Weblogen. You need
		a version of PHP with these components, which may be available via
		your PHP distribution or may require you to recompile PHP.
		</p><?
	}
}

function run_process_with_input($command, $input)
{
	$descriptorspec = array(
        0 => array("pipe", "r"),
        1 => array("pipe", "w"),
        2 => array("pipe", "w")
    );

    $process = proc_open($command, $descriptorspec, $pipes);

    if(is_resource($process))
    {
        fclose($pipes[0]);

		$starttime = microtime(true);
		if(!isset($maxtime))
		{
			$maxtime = 10;
		}
		$return_value = 0;
		$timeout = false;

		while(true)
		{
			$status = proc_get_status($process);
			if(!$status['running'])
			{
				#print_r($status);
				break;
			}
			usleep(100000);
			if(microtime(true) - $starttime > $maxtime)
			{
				#print microtime(true) - $starttime;
				#print "<br>";
				proc_terminate($process);
				$timeout = true;
				$return_value = -1;
				break;
			}
		}
		if(!$timeout)
		{
			$return_value = proc_close($process);
			$return_value = $status['exitcode'];
		}
		else
		{
			$return_value = -1;
		}

		if($timeout)
		{
			$error.="\nProcess timed out\n";
		}

		return $return_value;
    }
    else
        die("Cannot open $executable");
}
?>

</body>
</html>
