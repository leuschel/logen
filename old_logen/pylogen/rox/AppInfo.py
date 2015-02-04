"""Parse the AppInfo files.
Written by Christopher Arndt and Stephen Watson."""

import sys
from xml.dom import Node, minidom, XML_NAMESPACE

def _getlangs(langs):
    if langs is None:
        langs = ['en', 'C', '']
        try:
            import rox #.i18n
            langs = rox.i18n.langs + langs
        except:
            pass
    elif type(langs) is type(''):
        langs = [langs]
    return langs

def _data(node):
    """Return all the text directly inside this DOM Node."""
    return ''.join([text.nodeValue for text in node.childNodes
      if text.nodeType == Node.TEXT_NODE])

class AppInfo:
    """Parsed AppInfo.xml file. Current only deals with the <About>, 
    <Summary> and <AppMenu> elements"""

    def __init__(self, source):
        """Read the file and parse the <About> element."""
        self._doc = minidom.parse(source)
        self._parseAbout()
        self._parseSummary()
        self._parseMenu()

    def _parseAbout(self):
        """Collect 'About' info in a dictionary by language."""
        self.about = {}
        for ab in self._doc.documentElement.getElementsByTagName('About'):
            lang = ab.getAttributeNS(XML_NAMESPACE, 'lang')
            self.about[lang] = {}

            for node in ab.childNodes:
                if node.nodeType != Node.ELEMENT_NODE:
                    continue
                name = node.localName or None
                label = node.getAttribute('label') or name
                self.about[lang][name] = label, _data(node)

    def _parseSummary(self):
        """Collect 'Summary' info in a dictionary by language."""
        self.summary = {}
        for sum in self._doc.documentElement.getElementsByTagName('Summary'):
            lang=sum.getAttributeNS(XML_NAMESPACE, 'lang')
            self.summary[lang] = _data(sum)

    def _parseMenu(self):
        # XXX do <AppMenu> parsing - data structure?
        self.menu = []

        for node in self._doc.getElementsByTagName('AppMenu'):
            self._parseAppMenu(self.menu, node)

    def _parseAppMenu(self, menus, node):
        """Recursivly parse the <AppMenu>s"""
        for item in node.getElementsByTagName('Item'):
            opt=item.getAttribute('option')
            labels={}
            for label in item.getElementsByTagName('Label'):
                lang=label.getAttributeNS(XML_NAMESPACE, 'lang')
                labels[lang]=_data(label)
            sub_menus=[]
            for sub in item.getElementsByTagName('AppMenu'):
                self._parseAppMenu(sub_menus, sub)
            menus.append({'option': opt, 'label':
                          labels, 'sub-menus': sub_menus})

    def getAbout(self, elname, langs=None):
        """Return an entry from the <About> section.

        elname is the name of the element to return text from
        langs is a list of acceptable languages, or None to use
        rox.i18n.langs
        """
        langs = _getlangs(langs)

        for lang in langs:
            if self.about.has_key(lang):
                if self.about[lang].has_key(elname):
                    return self.about[lang][elname]
        else:
            return None

    def getAuthors(self, langs=None):
        """Return the contents of the <Authors> element in the
        <About> section (also tries <Author> if needed."""

        auth=self.getAbout('Authors', langs)
        if auth is None:
            auth=self.getAbout('Author', langs)
        if auth is None:
            return ''
        return auth[1]

    def setAbout(self, elname, value, lang):
        """Set the value of an element in the <About> section.

        If no element 'elname' is present in the <About> section with the
        matching 'lang' attribute, append a new one. If no such section
        exists yet, create it first.
        value must be a unicode string.
        """

        if not self.about.has_key(lang):
            ab = self._doc.createElement('About')
            ab.setAttributeNS(XML_NAMESPACE, 'xml:lang', lang)
            self._doc.documentElement.appendChild(ab)

        for ab in self._doc.documentElement.getElementsByTagName('About'):
            if ab.getAttributeNS(XML_NAMESPACE, 'lang') == lang:
                for node in ab.childNodes:
                    if node.nodeType == Node.ELEMENT_NODE and \
                      node.localName == elname:
                        ab.removeChild(node)
                        el = self._doc.createElement(elname)
                        text = self._doc.createTextNode(value)
                        el.appendChild(text)
                        ab.appendChild(el)
        self._parseAbout()

    def getSummary(self, langs=None):
        """Return the content of the <Summary> element.

        langs is a list of acceptable languages, or None to use
        rox.i18n.langs
        """
        langs = _getlangs(langs)

        for lang in langs:
            if self.summary.has_key(lang):
                return self.summary[lang]
        else:
            return None

    def setSummary(self, value, lang):
        """Set content of the Summary element with matching 'lang' attribute.

        If no such element is present, append a new one to the DOM object.
        value must be a unicode string.
        """

        if self.summary.has_key(lang):
            for sum in self._doc.documentElement.getElementsByTagName(
              'Summary'):
                if sum.getAttributeNS(XML_NAMESPACE, 'lang') == lang:
                    sum.parentNode.removeChild(sum)
        sum = self._doc.createElement('Summary')
        sum.setAttributeNS(XML_NAMESPACE, 'xml:lang', lang)
        text = self._doc.createTextNode(value)
        sum.appendChild(text)
        self._doc.documentElement.appendChild(sum)
        self._parseSummary()

    def findElements(self, elname, ns=None):
        """Return all instances of the named element with an optional name
        space.  They are returned as DOM objects."""
        if ns is None:
            return self._doc.getElementsByTagName(elname)
        return self._doc.getElementsByTagNameNS(ns, elname)

    def _getMenu(self, item, langs):
        ritem={'option': item['option']}

        labels=item['label']

        for lang in langs:
            #print labels, lang, labels.has_key(lang)
            if labels.has_key(lang):
                ritem['label']=labels[lang]
                break
        else:
            try:
                ritem['label']=labels['']
            except:
                ritem['label']=''

        subs=[]
        for s in item['sub-menus']:
            subs.append(self._getMenu(s, langs))
        ritem['sub-menus']=subs

        return ritem

    def getAppMenu(self, langs=None):
        """Return list of all menu entries.  Each item in the list is a
        dictionary with these keys:
        'option': the option to pass to the app for each item
        'label': the label of the item in an appropriate language
        'sub-menus': list of sub menu items"""
        
        langs = _getlangs(langs)

        menus=[]
        for item in self.menu:
            menus.append(self._getMenu(item, langs))

        return menus

    def _getTypeList(self, element):
        types=[]
        for node in self._doc.getElementsByTagName(element):
            for t in node.getElementsByTagNameNS(None, 'MimeType'):
                types.append(t.getAttribute('type'))
        return types

    def getCanRun(self):
        """Return list of named MIME types that this application declares
        it can handle."""
        return self._getTypeList('CanRun')

    def getCanThumbnail(self):
        """Return list of named MIME types that this application declares
        it can generate thumbnails for."""
        return self._getTypeList('CanThumbnail')

    def __str__(self):
        return self._doc.toprettyxml('', '', encoding='utf-8')

    def writeToFile(self, fname):
        fp = open(fname, 'wb')
        fp.write(str(self))
        fp.close()

# Some test code
if __name__=='__main__':
    print _getlangs(None)
    ai=AppInfo(sys.argv[1])
    #print ai
    print 'Summary: %s' % ai.getSummary()
    print ai.getAbout('Authors')
    print 'Authors: %s' % ai.getAuthors()
    #print ai.findElements('AppMenu')
    print ai.menu
    for menu in ai.getAppMenu():
        print '%s (%s) -> %d sub' % (menu['label'], menu['option'],
                                     len(menu['sub-menus']))
    #print ai.findElements('CanRun')
    print ai.getCanRun()
    print ai.getCanThumbnail()
