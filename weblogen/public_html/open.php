<?php
    require_once('common.php');
	create_session();

	$_SESSION['annmethod'] = '';

	if(isset($_FILES['userfile']) && is_uploaded_file($_FILES['userfile']['tmp_name']))
	{
		$_SESSION['plfile'] = sanitise_input_file(file_get_contents($_FILES['userfile']['tmp_name']));
		$_SESSION['stock'] = false;
		$_SESSION['filename'] = $_FILES['userfile']['name'];
		$_SESSION['stage'] = 2;
		redirect(isset($_POST['noedit']) ? 'upload.php' : 'open.php');
	}

	if(isset($_POST['stock']))
	{
		$stock = "true";
		$stock_file = stock_file($_POST['filename'], '.pl');
		if($stock_file == false)
		{
			die('bad stock file');
		}
		$_SESSION['plfile'] = file_get_contents($stock_file);
		$_SESSION['filename'] = $_POST['filename'];
		$_SESSION['stock'] = true;
		$_SESSION['stage'] = 2;
		redirect(isset($_POST['noedit']) ? 'upload.php' : 'open.php');
	}

    html_start('WebLogen - Open File', '', '', 1, '');

	tooltip_js();

	$default_tooltip_size = '300px';

	// warn safari 2.0 users that things don't work
	// look at following page for build numbers
	// http://developer.apple.com/internet/safari/uamatrix.html
	$agent = $_SERVER['HTTP_USER_AGENT'];
	if(preg_match('!Mozilla/5.0 \(Macintosh; U; PPC Mac OS X; .*\) AppleWebKit/(.*) \(KHTML, like Gecko\) Safari/(.*)!', $agent, $matches))
	{
		if($matches[1] >= 412)
		{
			?>
	<p>You are currently running Safari (version >=2.0). Unfortunately there
	appears to be a bug which means that on the annotation editing page,
	menus disappear as soon as the mouse moves. Apple is aware of this bug
	and hopefully will fix it soon. To work around either use an earlier
	version of Safari or use Firefox. If you get this message and yet this
	bug is not present, then please <a href="mailto:dre@ecs.soton.ac.uk">email
	me</a> so I can update the browser detection.
			<?
		}
	}

	if($_SESSION['plerror'])
	{
		echo '<p>The supplied prolog source file has errors:</p>';
		parse_and_display_upload_error($_SESSION['plerror']);

		if(isset($db_name) && isset($bug_report_on_parse_error))
		{
			// Only show bug report options if a database is configured
			?>
			<form action="bug_report.php" method="post">
			<table border=0>
			<tr>
			<td>
			<input type="submit" value="Submit Bug Report" />
			<input type="hidden" name="stage" value="upload" />
			</td>
			<td>
			If you think this error is due to a bug in weblogen you can submit
			a bug report. Please make sure that your code can be parsed by your
			prolog before you do this.
			</td>
			</tr>
			</table>
			</form>
			<?
		}
	}
?>

<!-- Stock Examples -->
<form action="open.php" method="post">
<p style="margin-top: 4px; margin-bottom: 4px">
Example: <select name="filename">
               <?php showStock("./examples/","*.pl"); ?>
              </select>
		 <input type="hidden" name="stock" value="true"/>
		 <input type="submit" name="noedit" value="Open>>" />
		 <input type="submit" name="edit" value="Open and Edit" />
<? insert_tooltip('This allows you to choose an example file, which will have ready-made annotations and goals.<br><b>Open</b> will take you straight to the annotation method selection page, while <b>Open and Edit</b> gives you the chance to edit the file first.', $default_tooltip_size); ?>

</p>
</form>

<!-- User Examples -->
<form enctype="multipart/form-data" action="open.php" method="post">
<p style="margin-top: 4px; margin-bottom: 4px">
Local File: <input name="userfile" type="file" />
	    <input type="hidden" name="MAX_FILE_SIZE" value="30000" />
	    <input type="submit" name="noedit" value="Upload>>" />
	    <input type="submit" name="edit" value="Upload and Edit" />
<? insert_tooltip('Here you can upload your own Prolog source files. <b>Upload</b>  will take you straight to the annotation method selection page, while <b>Upload and Edit</b> gives you the chance to edit the file first.', $default_tooltip_size); ?>
</p>
 </form>

<form action="upload_initial.php" method="post">
<p style="margin-top: 4px; margin-bottom: 4px">
		<?php
		   $filename = $_SESSION['filename'];
		   $stock = $_SESSION['stock'];
		   echo "<input type='hidden' name='filename' value='$filename' />";
		   echo "<input type='hidden' name='stockfile' value='$stock' />";
		?>
		<input type="submit" value="Next>>"/>
		<input type="button" value="Clear" onclick="document.getElementById('ta').value = ''" />
		<input type="submit" name="save" value="Save"/>
<? insert_tooltip('<b>Next</b> uploads whatever you type into the text box below so that you can annotate it. You can start again by clicking <b>Clear</b>, while clicking <b>Save</b> uploads your source code and then allows you to carry on editing.', $default_tooltip_size); ?>
</p>

	  <!-- The main source window - all uploaded programs should go through here -->
	    <textarea id="ta" name="usertyped" style="width: 99%" rows="30"><?php
		 if(isset($_POST['filename']))
		 {
		     $filename = $_POST['filename'];
			 $stock_file = stock_file($filename, '.pl');
			 if($stock_file !== false)
			 {
				 echo htmlspecialchars(file_get_contents($stock_file));
			 }
		 }
		 elseif($_SESSION['plfile'])
		 {
		 	echo htmlspecialchars($_SESSION['plfile']);
		 }
		 else
		 {
			 echo "/* Enter Prolog Code here */";
		 }
	       ?></textarea>
		</form>

<?php html_end(); ?>
