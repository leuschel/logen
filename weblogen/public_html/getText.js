// This function takes a DOM element and extracts all the text as a flat
// string while ignoring the markup
function getText(element)
{
    var str = '';
    var l = element.childNodes.length
    for(var i = 0; i < l; ++i)
    {
        e = element.childNodes[i];
        if(e.nodeType == 3)
        {
            str = str + e.nodeValue;
        }
        else if(e.nodeType == 1)
        {
            str = str + getText(e);
        }
    }
    return str;
}
