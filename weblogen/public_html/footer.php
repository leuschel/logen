<?php
function html_end()
{
  
	global $admin_email;
?>
</td>
</tr>
</table>
<p class="AppFooter">
<a href="index.php">Weblogen</a> - Part of the 
<a href="http://www.clip.dia.fi.upm.es/Projects/ASAP/">ASAP Project</a>.
		Problems? Contact the 
		<a href="mailto: <?php echo $admin_email; ?>">System Administrator</a>.

Please read the <a href="docs/manual.html">Weblogen Manual</a>.
</p>
		
	</body>
</html>
<?php
}
?>
