<?php

// All code related to error reporting and correction due to annotation errors
// should go in here.

// parse_spec_error should be called first. This returns a value which should
// be passed to show_annotated.

// Then display_spec_error() should be called to display an error along with
// fix instructions.

require_once('common.php');

// The following global var and function are used to trap validation errors
// so they are not reported directly in the web interface but can be reported
// gracefully
$validate_error = '';

function validate_error_handler($errno, $errstr, $errfile, $errline)
{
	global $validate_error;
	$errstr = preg_replace('/DOMDocument::validate\(\) .*?:/', '', $errstr);
	$validate_error .= '<p>' . htmlspecialchars($errstr) . '</p>';
}

global $history;

// This checks the specialisation error log for an XML description of the error
// and sets up the state accordingly. $info is modified after being passed. Its
// contents will be destroyed so it should not be initialised first
// Function passes back true if valid XML was found.
function parse_spec_error(&$info)
{
	global $message, $errortype, $fix, $xml, $history;
	// This means that the page was redirected from upload_spec.php and
	// indicates that an error occurred during specialisation. We can thus
	// try and parse the xml error message and inform the user.
	$error = $_SESSION['specerror'];
	// first check that there is actually an error message containing XML
	if($error != '' && preg_match('/\n\<\<\n(.*?)\>\>\n/s', $error, $matches))
	{
		global $backend_dir;
		$dom = new DomDocument;
		
		$xml = "<?xml version='1.0' encoding='utf-8'?>\n" .
			   "<!DOCTYPE error SYSTEM 'file://$backend_dir/error.dtd'>\n" .
			   $matches[1];
		$dom->loadXML($xml);

		set_error_handler("validate_error_handler");
		if(!$dom->validate())
		{
			?><html><head>
			<link type="text/css" rel="stylesheet" href="xmlverbatim.css"/>
			</head><body><p><?
			global $validate_error;
			echo $validate_error;
			echo "</p>\n";
			echo process_xml($xml, 'xmlverbatim.xsl');
			echo "<p><b>XML does not validate!</b></p></body></html>";
			exit;
		}
		restore_error_handler();
		//echo process_xml($xml, 'xmlverbatim.xsl');
		
		$xml = simplexml_load_string($xml);
		// should be the following but it doesn't work!
		//$xml = simplexml_import_dom($dom);
		$message = (string)$xml->message;
		$errortype = (string)$xml->type;
		$problem = (string)$xml->problem;

		$fix = false;
		$info = Array();
		if($xml->xpath('programpoint'))
		{
			$pp = $xml->programpoint;
			$currentpred = (string)$pp->predicate;
			$currentpredarity = (string)$pp->predicate['arity'];
			$currentpredclause = (string)$pp->localclause;
			$annotation = (string)$pp->annotation;

			$path = '[';
			if($xml->xpath('/error/programpoint/path'))
			{
				$path_xml = $xml->xpath('/error/programpoint/path/point');
				for($i = 0; $i < count($path_xml); ++$i)
				{
					$ann = (string) $path_xml[$i];
					$arg = (string) $path_xml[$i]['arg'];
					if($i > 0)
					{
						$path .= ', ';
					}
					$path .= "($ann, $arg)";
				}
			}
			$path .= ']';

			$extra['ann'] = 'unsafe';
			$extra['pred'] = $currentpred;
			$extra['arity'] = $currentpredarity;
			$extra['clause'] = $currentpredclause;
			$extra['path'] = $path;
		
			if($xml->xpath('/error/programpoint/correctann'))
			{
				if($xml->xpath('/error/programpoint/correctann/annotation'))
				{
					$extra['type'] = 'correctann';
					$extra['fix'] = (string) $pp->correctann->annotation;
					$fix = true;
				}
				else
				{
					$extra['type'] = 'fix_hide_nf';
					$fix = true;
				}
			}
			else
			{
				$extra['type'] = 'infoann';
			}
			$info[] = $extra;
		}

		if($xml->xpath('filter'))
		{
			$f = $xml->filter;
			$extra = Array();
			$extra['pred'] = (string) $f->predicate;
			$extra['arity'] = (string) $f->predicate['arity'];
			if($xml->xpath('/error/filter/correctfilt'))
			{
				$make_dynamic =
					$xml->xpath('/error/filter/correctfilt/make_dynamic');
				if($make_dynamic)
				{
					if(count($info))
					{
						$info[0]['type'] = 'correctann';
						$info[0]['fix'] = (string)$xml->programpoint->annotation;
					}

					$extra['type'] = 'correctfilt';

					for($i = 0; $i < count($make_dynamic); ++$i)
					{
						$a = $extra;
						$a['make_dyn'] = (string) $make_dynamic[$i];
						$info[] = $a;
					}
					
					$fix = true;
				}
				else
				{
					$extra['type'] = 'replacefilt';
					$extra['newfilt'] = $xml->filter->correctfilt->replace_filt;
					$fix = true;
					$info[] = $extra;
				}
			}
			else
			{
				$extra['type'] = 'infofilt';
				$info[] = $extra;
			}
		}

		if($xml->xpath('history'))
		{
			$history = Array();
			$historyxml = $xml->xpath('/error/history/call');
			for($i = 0; $i < count($historyxml); ++$i)
			{
				$history[] = (string) $historyxml[$i];
			}
		}
		return true;
	}
	return false;
}

function display_spec_error()
{
	global $message, $errortype, $fix, $xml, $history;
	echo '<div id="errorblock">';
	$error = htmlspecialchars($_SESSION['specerror']);
	echo "<table><tr><td>\n";
	echo "<p>An error occurred during specialisation: <b>$message</b>"; ?>
	This will be highlighted in red (e.g. <span class="unsafe">match</span>).
	<?
	if($errortype == 'warning')
	{
		?>This is a warning produced by running in watchdog mode and so it is
		possible that your annotations are fine. If so, you should re-run
		your goal with watchdog mode disabled.<?
	}
	?>
	</p>
	<form action="bug_report.php" method="post">
	<p>
	<?
	if($fix)
	{
		$fix_js = process_xml($xml, 'get_fixes.xsl');
		echo "<input type='button' value='Automatically Fix' onclick='$fix_js changed = true; getElementById(\"errorblock\").style.display=\"none\"'/>\n";
	}
	echo "<input id='showlog' type='button' value='Show Log' onclick='toggleShow(\"speclog\", \"showlog\", \"Show Log\", \"Hide Log\")'/>\n";
	echo "<div style='display: none' id='speclog'>";
	echo "<div class='log' style='display: block; overflow:auto'><pre>$error</pre></div>\n";
	global $db_name;
	if(isset($db_name))
	{
		?>
		<p>If you think this error is due to a bug in weblogen you can submit
		a bug report.
		<input type="submit" value="Submit Bug Report" />
		<input type="hidden" name="stage" value="spec" />
		</p>
		<?
	}
	?>
	</form>
	</div></td>
	<?
	if(isset($history))
	{
		echo "<td valign='top' style='border: 1px solid black; padding-left: 10px; padding-right: 10px'><p><b>Call&nbsp;History</b></p>";
		foreach($history as $call)
		{
			echo "<tt>", htmlspecialchars($call), "</tt><br>";
		}
		echo "</td>";
	}
	?>
	</tr></table>
	</div>
	<?
}

function generate_scroll_to_error_code($pane)
{
	?>
	<script type="text/javascript">
	var pane = document.getElementById('<? echo $pane; ?>');
	var unsafetags = pane.getElementsByTagName('span');
	for(var i = 0; i < unsafetags.length; ++i)
	{
		if(unsafetags[i].className == 'unsafe' || unsafetags[i].className == 'fix_hide_nf')
		{
			pane.scrollTop = unsafetags[i].offsetTop - pane.clientHeight / 2;
			break;
		}
	}
	</script>
	<?
}

?>
