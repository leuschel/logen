	<script type="text/javascript">
    // <![CDATA[

var currentAnn = null;
var currentArity = null;
var currentMenu = null;

// we can't use the same map for filter types and annotations, because online
// is both a filter type and an annotation. the xslt generates javascript that
// indicates if it's a filter or annotation.

ann_menu_map = new Array();
filt_menu_map = new Array();

<? include('annmenus.php'); ?>

function addToMenuMap(map, unknown, list)
{
	for(var i in unknown)
	{
		map[unknown[i]] = list;
	}
	for(var i in list)
	{
		map[list[i]] = list;
	}
}

function getMenu(ann, menu)
{
	var list = menu[ann];
	if(list)
		return list;
	else
	{
		alert("Menu not implemented for '"+ ann +"' annotation");
		return false;
	}
}

function setupMenu(items, selected, filter)
{
	var menu = new Array();
	var func;
	if(filter)
		func = 'changeFilter';
	else
		func = 'changeAnn';

	for(var i = 0; i < items.length; ++i)
	{
		if(items[i] == selected)
		{
			menu[i] = '<a class="selected"';
		}
		else
		{
			menu[i] = '<a';
		}
		menu[i] += ' onclick="' + func + '(currentAnn, \'';
		menu[i] += items[i] + '\')">' + items[i] + '</a>';
	}
	return menu;
}

/***********************************************
* AnyLink Drop Down Menu- (c) Dynamic Drive (www.dynamicdrive.com)
* This notice MUST stay intact for legal use
* Visit http://www.dynamicdrive.com/ for full source code
***********************************************/

var menuwidth='100px' //default menu width
var menubgcolor='lightyellow' //menu bgcolor
var disappeardelay=250 //menu disappear speed onMouseout (in miliseconds)
var hidemenu_onclick="yes" //hide menu when user clicks within menu?

/////No further editting needed

var ie4=document.all
var ns6=document.getElementById&&!document.all

if (ie4||ns6)
	document.write('<div id="dropmenudiv" style="visibility:hidden;width:'+menuwidth+';background-color:'+menubgcolor+'" onMouseover="clearhidemenu()" onMouseout="dynamichide(event)"></div>')

function mygetposOffset(what, offsettype, filter){
	var boxObj = document.getElementById(filter ? 'filterbox' : 'sourcebox');
	var totaloffset=(offsettype=="left")? what.offsetLeft - boxObj.scrollLeft: what.offsetTop - boxObj.scrollTop;
	var parentEl=what.offsetParent;
	while (parentEl!=null){
		totaloffset=(offsettype=="left")? totaloffset+parentEl.offsetLeft : totaloffset+parentEl.offsetTop;
		parentEl=parentEl.offsetParent;
	}
	return totaloffset;
}


function showhide(obj, e, visible, hidden, menuwidth){
	if (ie4||ns6)
		dropmenuobj.style.left=dropmenuobj.style.top=-500
	if (menuwidth!=""){
		dropmenuobj.widthobj=dropmenuobj.style
		dropmenuobj.widthobj.width=menuwidth
	}
	if (e.type=="click" && obj.visibility==hidden || e.type=="mouseover")
		obj.visibility=visible
	else if (e.type=="click")
		obj.visibility=hidden
}

function iecompattest(){
	return (document.compatMode && document.compatMode!="BackCompat")? document.documentElement : document.body
}

function macff_clearbrowseredge(obj, whichedge, filter){
	var boxObj = document.getElementById(filter ? 'filterbox' : 'sourcebox');
	var edgeoffset=0;
	if (whichedge=="rightedge"){
		var windowedge= window.pageXOffset+window.innerWidth-15
		dropmenuobj.contentmeasure=dropmenuobj.offsetWidth
		if (windowedge-dropmenuobj.x < dropmenuobj.contentmeasure)
			edgeoffset=dropmenuobj.contentmeasure-obj.offsetWidth
	}
	else{
		var topedge= window.pageYOffset
		var windowedge= window.pageYOffset+window.innerHeight-18
		dropmenuobj.contentmeasure=dropmenuobj.offsetHeight
		// This hack ensures that the menu never overlaps the scrollbar of the frame
		// This is only necessary for FF 1.5 on Mac as the native widgets sit on top
		// of the menu obscuring it. For any browser where this works properly
		// (FF on linux and windows), clearbrowseredge should be used instead.
		if (obj.offsetTop + dropmenuobj.contentmeasure > boxObj.clientHeight){ //move up?
			edgeoffset=dropmenuobj.contentmeasure+obj.offsetHeight
			if ((dropmenuobj.y-topedge)<dropmenuobj.contentmeasure) //up no good either?
				edgeoffset=dropmenuobj.y+obj.offsetHeight-topedge
			}
	}
	return edgeoffset
}

function myclearbrowseredge(obj, whichedge){
	var edgeoffset=0;
	if (whichedge=="rightedge"){
		var windowedge=ie4 && !window.opera? iecompattest().scrollLeft+iecompattest().clientWidth-15 : window.pageXOffset+window.innerWidth-15
		dropmenuobj.contentmeasure=dropmenuobj.offsetWidth
		if (windowedge-dropmenuobj.x < dropmenuobj.contentmeasure)
			edgeoffset=dropmenuobj.contentmeasure-obj.offsetWidth
	}
	else{
		var topedge=ie4 && !window.opera? iecompattest().scrollTop : window.pageYOffset
		var windowedge=ie4 && !window.opera? iecompattest().scrollTop+iecompattest().clientHeight-15 : window.pageYOffset+window.innerHeight-18
		dropmenuobj.contentmeasure=dropmenuobj.offsetHeight
		if (windowedge-dropmenuobj.y < dropmenuobj.contentmeasure){ //move up?
			edgeoffset=dropmenuobj.contentmeasure+obj.offsetHeight
			if ((dropmenuobj.y-topedge)<dropmenuobj.contentmeasure) //up no good either?
				edgeoffset=dropmenuobj.y+obj.offsetHeight-topedge
			}
	}
	return edgeoffset
}

function populatemenu(what){
	if (ie4||ns6)
		dropmenuobj.innerHTML=what.join("")
}


function dropdownmenu(obj, e, filter, arity){
	currentAnn = obj;
	currentArity = arity;
	if(obj.className == 'directive')
	{
		menucontents = ['<a onclick="removeDirective()">Remove</a>'];
		currentDirective = obj;
	}
	else
	{
		menuitems = getMenu(obj.className, filter ? filt_menu_map :
													ann_menu_map);
		if(menuitems == false)
		{
			return false;
		}
		menucontents = setupMenu(menuitems, obj.className, filter);
	}
	
	if (window.event)
		event.cancelBubble=true
	else if (e.stopPropagation) e.stopPropagation()
		clearhidemenu()
	dropmenuobj=document.getElementById? document.getElementById("dropmenudiv") : dropmenudiv;
	populatemenu(menucontents);

	if (ie4||ns6){
		showhide(dropmenuobj.style, e, "visible", "hidden", menuwidth)
		dropmenuobj.x=mygetposOffset(obj, "left", filter)
		dropmenuobj.y=mygetposOffset(obj, "top", filter)

		// This is a horrible hack to make Firefox 1.5 on mac os x work.
		// basically menus are obscured by scrollbars so we have a special version of myclearbrowseredge
		// that shouldn't have this problem. This hack should be removed or fine-tuned when ff is fixed
		// on mac!
		if(navigator.userAgent.indexOf('Firefox/1.5') >= 0 &&
		   navigator.userAgent.indexOf('Macintosh') >= 0)
		{
			// Firefox 1.5 on Mac!
			dropmenuobj.style.left=dropmenuobj.x-macff_clearbrowseredge(obj, "rightedge", filter)+"px"
			dropmenuobj.style.top=dropmenuobj.y-macff_clearbrowseredge(obj, "bottomedge", filter)+obj.offsetHeight+"px"
		}
		else
		{
			dropmenuobj.style.left=dropmenuobj.x-myclearbrowseredge(obj, "rightedge")+"px"
			dropmenuobj.style.top=dropmenuobj.y-myclearbrowseredge(obj, "bottomedge")+obj.offsetHeight+"px"
		}
	}

	return clickreturnvalue()
}

function clickreturnvalue(){
	if (ie4||ns6) return false
	else return true
}

function contains_ns6(a, b) {
	if(!b)
		return;
	while (b.parentNode)
		if ((b = b.parentNode) == a)
			return true;
	return false;
}

function dynamichide(e){
	if (ie4&&!dropmenuobj.contains(e.toElement))
		delayhidemenu()
	else if (ns6&&e.currentTarget!= e.relatedTarget&& !contains_ns6(e.currentTarget, e.relatedTarget))
		delayhidemenu()
}

function hidemenu(e){
	if (typeof dropmenuobj!="undefined"){
		if (ie4||ns6)
			dropmenuobj.style.visibility="hidden"
	}
}

function delayhidemenu(){
	<?
	// on safari 2.0 things don't work
	// look at following page for build numbers
	// http://developer.apple.com/internet/safari/uamatrix.html
	// this code disables menus disappearing when the mouse moves away on
	// buggy browsers
	$agent = $_SERVER['HTTP_USER_AGENT'];
	$badmac = false;
	if(preg_match('!Mozilla/5.0 \(Macintosh; U; PPC Mac OS X; .*\) AppleWebKit/(.*) \(KHTML, like Gecko\) Safari/(.*)!', $agent, $matches))
	{
		if($matches[1] >= 412)
		{
			$badmac = true;
		}
	}
	
	if(!$badmac)
	{
	?>if (ie4||ns6)
		delayhide=setTimeout("hidemenu()",disappeardelay)<?
	}
	?>
}

function clearhidemenu(){
	
	if (typeof delayhide!="undefined")
		clearTimeout(delayhide)
}

if (hidemenu_onclick=="yes")
	document.onclick=hidemenu

var changed = false;

function checkUnchanged()
{
	if(changed)
	{
		//return confirm("You have made changes to the annotations which have not been saved. Do you want to continue?");

		submitAnnotations();

		var filters = document.getElementById("filters_input").cloneNode(true);
		var anns = document.getElementById('annotations').cloneNode(true);
		document.getElementById('spec_form').appendChild(filters);
		document.getElementById('spec_form').appendChild(anns);
	}
	return true;
}

var gx;

function checkAnnotationsSpecialise(form)
{
	if(!gx && form.goal.value.length == 0)
	{
		alert("You must specify a goal with which to specialise.");
		return false;
	}

	return checkUnchanged();
}

function checkAnnotations(link)
{
	if(checkUnchanged())
		window.location.href = link;

}

function mouseoutAnn()
{
	delayhidemenu();
}

function mouseoverAnn(ann)
{
	element = document.getElementById(ann);
	ann = element.className;
}

function changeFilter(obj, newfilt)
{
	obj.className = newfilt;
	obj.innerHTML = newfilt;
	changed = true;
}

function changeAnn(obj, newann)
{
	var predname = obj.innerHTML;
	if(newann == 'memo')
	{
		if(!filterExists(predname, currentArity))
		{
			setTimeout("warnNoFilter('"+ predname +"', "+ currentArity +")", 200);
		}
	}
	obj.className = newann;
	changed = true;
	return false;
}

function warnNoFilter(name, arity)
{
	if(confirm('No filter exists for '+name+'/'+arity+'. Click Ok to create one.'))
	{
		addFilter(name, currentArity);
	}
	return false;
}

function useStockGoal(goal)
{
	document.getElementById('goal').value = goal;
}

function submitAnnotations()
{
<?
// On some pages we want to be able to save even though we haven't changed
// the annotations. For instance when we are changing the filters alone.
if($extras != 'javascript-always-save')
{
?>if(!changed)
{
	alert("No changes have been made to the annotations.");
	return false;
}
var filters = getText(document.getElementById("filters"));
document.getElementById("filters_input").value = filters;

<?
}
?>

	pre = document.getElementById("source")
	annspans = pre.getElementsByTagName("span")
	annotations = ""
	var cur_hide_nf = null;
	for(i = 0; i < annspans.length; ++i)
	{
		ann = annspans[i].className
		if(ann != "predicate" && ann != "comment" && 
		   ann != "list" && ann != "dynamic" &&
		   ann != "static" && ann != "string" && ann != "")
		{
			if(ann.substring(0, 7) == 'unknown')
			{
				alert('Some unknown annotations remain. These must be changed before you can save.');
				return false;
			}
			if(ann == 'hide_nf')
			{
				cur_hide_nf = annspans[i];	
			}
			else
			{
				if(cur_hide_nf && annspans[i].parentNode != cur_hide_nf)
				{
					cur_hide_nf = null;
					annotations += ", hide_nf_stop"
				}
			}
			if(annotations.length == 0)
				annotations += ann
			else
				annotations += ", " + ann
		}
	}
	if(cur_hide_nf)
	{
		annotations += ", hide_nf_stop"
	}
	document.getElementById('annotations').value = annotations
	//alert(annotations);
	return true

}

var createdfilters=0;
function createNewType()
{
	return "<span class='dynamic' id='filtnew" + createdfilters++ +
		"' onclick='return dropdownmenu(this, event, true)'" +
		" onmouseout='delayhidemenu();'>dynamic</span>";
}

function createSpan(klass, text)
{
	var span = document.createElement('span');
	setAttr(span, 'class', klass);
	span.appendChild(document.createTextNode(text));
	return span;
}

function setAttr(node, name, value)
{
	var attr = document.createAttribute(name);
	attr.nodeValue = value;
	node.setAttributeNode(attr);
}

function createNewTypeNode()
{
	var span = createSpan('dynamic', 'dynamic');
	setAttr(span, 'id', 'filtnew' + createdfilters++);
	setAttr(span, 'onclick', 'return dropdownmenu(this, event, true)');
	setAttr(span, 'onmouseout', 'delayhidemenu()');
	return span;
}

function clickPred(obj, e, arity)
{
	var pred = obj.innerHTML;
	if(filterExists(pred, arity))
	{
		return;
	}
	addFilter(pred, arity);
}

function removeDirective()
{
	var directive = currentDirective.parentNode;
	directive.parentNode.removeChild(directive);
	var spans = directive.getElementsByTagName("span");
	var i;
	for(i = 0; i < spans.length; ++i)
	{
		var n = spans[i];
		if(n.getAttribute('class') == 'filter')
		{
			currentFilters[n.innerHTML][currentArity] = false;
			break;
		}
	}
}

function addFilter(pred, arity)
{
	
	var f = document.getElementById('filters');
	var wholefilter = document.createElement("span");
	wholefilter.appendChild(document.createTextNode(":- "));
	var dir = createSpan('directive', 'filter');
	setAttr(dir, 'onclick', 'return dropdownmenu(this, event, true, ' + arity + ')');
	setAttr(dir, 'onmouseout', 'delayhidemenu()');
	wholefilter.appendChild(dir);
	wholefilter.appendChild(document.createTextNode("\n    "));
	wholefilter.appendChild(createSpan('filter', pred));
	
	if(arity > 0)
	{
		wholefilter.appendChild(document.createTextNode("("));
		wholefilter.appendChild(createNewTypeNode());
		for(var i = 1; i < arity; ++i)
		{
			wholefilter.appendChild(document.createTextNode(","));
			wholefilter.appendChild(createNewTypeNode());
		}
		wholefilter.appendChild(document.createTextNode(")"));
	}
	wholefilter.appendChild(document.createTextNode(".\n"));
	f.appendChild(wholefilter);
	addFilterToHash(pred, arity);
	changed = true;
}

var currentFilters = Array();
var currentDirective;

function addFilterToHash(name, arity)
{
	if(currentFilters[name] == null)
	{
		currentFilters[name] = Array();
	}
	currentFilters[name][arity] = true;
}

function filterExists(name, arity)
{
	if(currentFilters[name] == null)
	{
		return false;
	}
	return currentFilters[name][arity];
}

function changeDisableOptions(val)
{
	document.getElementsByName('watch_builtins')[0].disabled = val;
	document.getElementsByName('watch_connectives')[0].disabled = val;
	document.getElementsByName('watch_infunfold')[0].disabled = val;
	document.getElementsByName('watch_infmemo')[0].disabled = val;
	document.getElementsByName('watch_backprop')[0].disabled = val;
}

<? js_toggleShow(false); ?>

// ]]>
</script>
<script type="text/javascript" src="getText.js"></script>

<? tooltip_js(); ?>

