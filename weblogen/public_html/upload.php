<?php 
require_once('common.php');
require_once('externals.php');
require_session();
$plfile = $_SESSION['plfile'];
$filename = $_SESSION['filename'];
$norm = $_SESSION['norm'];
$filter_prop = $_SESSION['filter_prop'];
$default_opt = $_SESSION['annmethod'];
$options = array('upload', 'simplebta');

if(!isset($enable_bta))
{
	$enable_bta = false;
}
if($enable_bta)
{
	$options[] = 'bta';
}

$opt_desc = array('upload' => 'Upload File',
				  'bta' => 'Automatic Binding Time Analysis',
				  'simplebta' => 'Simple Binding Time Analysis',
				  'stock' => 'Ready Made');

$default_tooltip_size = '300px';

$tempdir = get_directory();
chdir($tempdir);

$file = fopen($filename, 'w');
fwrite($file, $plfile);
fclose($file);

$output = highlight($filename);
if($output[0] == 0)
{
	$xml = $output[1];
	$_SESSION['plerror'] = '';
}
else
{
	$_SESSION['plerror'] = $output[2];
	redirect('open.php');
}
delDir($tempdir);

html_start('Annotate uploaded file', 'prolog.css', '', 2, '');

tooltip_js();

if($_SESSION['stock'] == "true")
{
	$files = array();
	$dh = opendir($frontend_dir . "/examples/");
	$ann_prefix = "$filename.ann";
	while(false !== ($temp = readdir($dh)))
		if(substr($temp, 0, strlen($ann_prefix)) == $ann_prefix)
			$files[] = $temp;
	
	if($files)
	{
		sort($files);
		$options[] = 'stock';
	}
  }  
?>
<script type="text/javascript">
<?
	echo 'var methods = [';
	for($i = 0; $i < count($options) - 1; ++$i)
	{
		echo "'$options[$i]', ";
	}
	echo "'$options[$i]'];\n";
  
?>
function setAnnMethod(method)
{
	for(i = 0;i<methods.length;i++){
		d = methods[i];
		document.getElementById(d).style.display="none";
	}
	document.getElementById(method).style.display="block";
}
</script>
<?
if($_SESSION['annerror'])
{
	echo '<p>The supplied/generated annotation file has errors:</p>';
	parse_and_display_upload_error($_SESSION['annerror']);
	if(isset($db_name))
	{
		?>
		<form action="bug_report.php" method="post">
		<table border=0>
		<tr>
		<td>
		<input type="submit" value="Submit Bug Report" />
		<input type="hidden" name="stage" value="uploadann" />
		</td>
		<td>
		If you think this error is due to a bug in weblogen you can submit
		a bug report. If you supplied your own annotation file, please make
		sure that the annotation file is in the correct format.
		</td>
		</tr>
		</table>
		</form>
		<?
	}
}

if($default_opt == '' && count($options) > 3)
{
	$default_opt = 'stock';
}
?>
<form>
<p>Choose Annotation method: 
<select style="margin-left: 4px" name="annmethod"
		onchange="setAnnMethod(this.options[this.selectedIndex].value)"
		id="annselect">
<?


for($i = 0; $i < count($options); ++$i)
{
	$opt = $options[$i];
	if($default_opt == $opt)
	{
		$selected = ' selected';
	}
	else
	{
		$selected = '';
	}
	echo "<option value='$opt'$selected>$opt_desc[$opt]</option>\n";
}
?>
</select>

<?
$text = 'Here you choose a method of initially annotating your program:<br/>Choose <b>Upload file</b> to supply your own annotation file.<br/>Choose <b>Simple Binding Time Analysis</b> to set all annotations to simple values which you will then modify.';
if($enable_bta)
{
	$text .= '<br/>Choose <b>Automatic Binding Time Analysis</b> to find an as aggressive as possible set of annotations without termination problems.';
}
if($_SESSION['stock'] == "true")
{
	$text .= '<br/>Choose <b>Ready Made</b> to use our example annotations.';
}

insert_tooltip($text, '400px');
?>

</p>
</form>

<form enctype="multipart/form-data" action="upload_annfile_initial.php" method="post">
<div id="upload" style="display: none" class="annmethod">
Annotation File 
<input type="hidden" name="MAX_FILE_SIZE" value="60000" />
<input name="annfile" type="file" />
<input type="submit" value="Upload File" />
<? insert_tooltip('Using this method, you can create an annotation file manually or reuse one that you have previously saved.', $default_tooltip_size); ?>
</div>
</form>

<form enctype="multipart/form-data" action="bta.php" method="post">
<div id="simplebta" style="display: none" class="annmethod">
Mode
<select name="btatype">
<option value="unfold">Unfold/Call all</option>
<option value="memo">Memo/ResCall all</option>
</select>
<input type="hidden" name="btamode" value="SIMPLE"/>
<input type="submit" value="Apply BTA" />
<? insert_tooltip('Selecting Unfold/Call all will give you a very aggressively annotated program, which may not terminate. Memo/ResCall all will give you very conservative annotations, resulting in a program no different from the original.', $default_tooltip_size); ?>
</div>
</form>

<?
// Hide autobta options if it's disabled in the config
if($enable_bta)
{
?>
<form enctype="multipart/form-data" action="bta.php" method="post">
<div id="bta" style="display: none" class="annmethod">
<table>
<tr>
<td>
Predicate
</td>
<td>
<script type="text/javascript">
function set_filter(text)
{
	var index = text.lastIndexOf('/');
	var name = text.substring(0, index);
	var arity = text.substring(index + 1);
	var filter = name;
	if(arity > 0)
	{
		filter += '(dynamic';
		for(var i = 1; i < arity; ++i)
		{
			filter += ', dynamic';
		}
		filter += ')';
	}
	document.getElementById('filter').value = filter;
}
</script>
<select name="predicates"
		onchange="set_filter(this.options[this.selectedIndex].value)">
		<option></option>
<?
$predicates = process_xml($xml, 'get_heads.xsl');
$predicates = explode("\n", $predicates);
$current = '';
for($i = 0; $i < count($predicates); ++$i)
{
	if($predicates[$i] && $current != $predicates[$i])
	{
		$current = $predicates[$i];
		echo "<option>$current</option>\n";
	}
}
?>
</select>
</td>
<td>
  <?php insert_tooltip('Select a predicate to use as an initial filter.', $default_tooltip_size); ?>
  </td>
  <td rowspan="4">
	<input type="hidden" name="btamode" value="FULL" />
	<input type="submit" value="Apply BTA" />
  </td>
</tr>
<tr>
<td>Initial Filter</td>
<td>
<input id="filter" name="filter" type="text" size="50" value="<?php echo $_SESSION['filter']; ?>"/>
</td>
<td>
  <?php insert_tooltip('Enter a binding type for the program entry point. For example match(static,dynamic) specifies match/2 as an entry point with first argument known and second argument unknown.', $default_tooltip_size); ?>
  </td>
</tr>
<tr><td>Only propagate filters?</td>
<td><input type="checkbox" name="filter_prop" <?php
if($filter_prop == 'on')
	echo 'checked';
echo '/></td><td>';
insert_tooltip('If you enable this option, then your initial filter will be propagated, but no terminations checks will be performed and so the annotations will possibly be unsafe.', $default_tooltip_size);
?></td></tr>
<tr><td>Termination Norm</td>
<td><select name="norm">
<?php foreach(array('Term', 'List', 'Both') as $n)
{
	if($n == $norm)
		echo "<option selected=\"selected\">$n</option>\n";
	else
		echo "<option>$n</option>\n";
} ?>
</select></td>
<td>
<? insert_tooltip('Norms are used to calculate whether a set of annotations are safe and will therefore always terminate. If you think you should get more aggressive annotations from the BTA, try selecting a different norm.', $default_tooltip_size); ?>
</td>
</tr>
</table>
</div>
</form>

<?php
}

if($_SESSION['stock'] == "true")
{
	// It's a stock file so we already have some annotation files
  ?><form action="upload_annfile_initial.php" method="post">
<div id="stock" style="display: none" class="annmethod">
		Annotation File <select name="annfile">
<?php

$session_stock = $_SESSION['stock_annfile'];
foreach($files as $f)
{
	if($f == $session_stock)
		echo "<option selected=\"selected\">$f</option>\n";
	else
		echo "<option>$f</option>\n";
} ?>
		</select>
		<input type="submit" value="Use Ready-Made Annotations" />
  <?php insert_tooltip('Use a ready made set of annotations for the current file.', $default_tooltip_size); ?>
	</div></form><?php
}
?>
<script type="text/javascript">
      d = document.getElementById('annselect');
      setAnnMethod(d.options[d.selectedIndex].value);
      
</script>
<br/>
<?php
echo '<div style="border: 1px solid black; margin-left: 10px; margin-right: 10px; padding: 4px; background-color: #f0f0fa; overflow: auto; height: 400px">';
echo process_xml($xml, 'simpleprologtohtml.xsl');
echo '</div>';
echo "\n<p><a href='get_pl.php'>Download Source Program</a></p>";

html_end(); 
?>
