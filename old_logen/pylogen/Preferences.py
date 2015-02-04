import os
import xml

#from xml.dom.ext.reader import Sax2
#from xml.dom import minidom

from xml.dom import xmlbuilder, minidom

class Preferences:
    def set_defaults(self):
        self.table['examples_path'] = os.path.abspath('../examples')
        
    def __init__(self, filename):
        self.filename = filename
        self.table = {}
        self.listeners = {}
        self.set_defaults()
        if os.path.isfile(filename):
            self.reload()

    def reload(self, reset=False):
        if reset:
            self.table = {}
        file = open(self.filename)
        input = xmlbuilder.DOMInputSource()
        input.byteStream = file
        doc = xmlbuilder.DOMBuilder().parse(input)
        file.close()

        populate_table(doc.documentElement, self.table)

    def save(self):
        doc = xml.dom.minidom.Document()
        save_table(doc, doc, self.table, 'preferences', '')
        file = open(self.filename, "w")
        file.write(doc.toprettyxml())
        file.close()

    def set_preference(self, pref_id, value):
        if value is None:
            print "Setting %s to None!" % pref_id
        else:
            if isinstance(value, dict):
                self.table[pref_id] = value
            else:
                self.table[pref_id] = str(value)

        ls = self.listeners.get(pref_id)
        if ls is not None and len(ls) > 0:
            for l in ls:
                l(self.table[pref_id])

    def get_preference(self, pref_id):
        # returns None if the preference does not exist
        return self.table.get(pref_id)

    def register_listener(self, pref, listener):
        ls = self.listeners.get(pref)
        if ls is None:
            self.listeners[pref] = [listener]
        else:
            ls.append(listener)

    def deregister_listener(self, pref, listener):
        # assumes you won't try and deregister when the listener hasn't been
        # added!
        self.listeners[pref].remove(listener)

# p = Preferences('pref.xml')
# print p.get_preference('font2')
# p.save()

def populate_table(root, table):
    for n in root.childNodes:
        if n.nodeType == root.ELEMENT_NODE:
            if n.tagName == 'pref':
                id = n.getAttribute('id')
                value = n.childNodes[0].nodeValue.strip()
                table[id] = value
            elif n.tagName == 'pref_group':
                name = n.getAttribute('id')
                table[name] = {}
                populate_table(n, table[name])
            # we ignore any other tags

def save_table(doc, root, table, tagname, id):
    element = doc.createElement(tagname)
    if id != "":
        element.setAttribute('id', id)
    root.appendChild(element)
    for x in table:
        n = table[x]
        if n is None:
            print "Preference %s is None" % x
        else:
            if isinstance(n, dict):
                save_table(doc, element, n, 'pref_group', x)
            else:
                pref = doc.createElement('pref')
                element.appendChild(pref)
                pref.setAttribute('id', x)
                pref.appendChild(doc.createTextNode(table[x])) 
