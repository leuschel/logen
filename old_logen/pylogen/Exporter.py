import os
from tkFileDialog import asksaveasfilename
try:
    import xml
    import xml.dom.minidom
    have_xml = True
    from Ft.Xml.Xslt.Processor import Processor
    from Ft.Xml.InputSource import DefaultFactory
    from Ft.Lib import Uri
    from Ft.Xml.Domlette import implementation, Print
    have_xml = True
except:
    have_xml = False

class Exporter:
    export_html = None
    export_xml = None
    export_latex = None
    app = None

    def __init__(self, master=None):
        self.app = master

    def export_to_xml(self, file = None):
        if self.export_xml is None:
            self.export_xml = os.path.join(".", "print.xml")
        if file is None:
            file = asksaveasfilename(title="Export to XML as...",
                            filetypes=[("XML files","*.xml"),("All files","*")],
                            initialfile=os.path.split(self.export_xml)[1],
                            initialdir=os.path.split(self.export_xml)[0])

        if file == "" or file is ():
            return 

        doc = implementation.createRootNode('file:///article.xml')

        self.app.ann_frame.build_xml(doc)
            
        outFile = open(file,'w')
        Print(doc, stream=outFile)
        outFile.close()                  
        
    def export_to_latex(self, file = None):
        if self.export_latex is None:
            self.export_latex = os.path.join(".", "print.tex")
        if file is None:
            file = asksaveasfilename(title="Export to Latex as...",
                        filetypes=[("Latex files","*.tex"),("All files","*")],
                        initialfile=os.path.split(self.export_latex)[1],
                        initialdir=os.path.split(self.export_latex)[0])

        if file == "" or file is ():
            return 

        self.build_dom_and_apply_style_sheet("pylogen/prologtolatex.xsl", file)

    def export_to_html(self, file = None):
        if self.export_html is None:
            self.export_html = os.path.join(".", "print.html")
        if file is None:
            file = asksaveasfilename(title="Export to HTML as...",
                          filetypes=[("HTML files","*.html"),("All files","*")],
                          initialfile=os.path.split(self.export_html)[1],
                          initialdir=os.path.split(self.export_html)[0])

        if file == "" or file is ():
            return 

        if have_xml:
            self.build_dom_and_apply_style_sheet("pylogen/prologtohtml.xsl", file)
        else:
            self.export_to_html_old(file)

        self.app.browser.openpage(file)

    def build_dom_and_apply_style_sheet(self, xsl, file):
        #doc = xml.dom.minidom.Document()
        doc = implementation.createRootNode('file:///article.xml')

        self.app.ann_frame.build_xml(doc)
            
        xsltproc = Processor()
        xsltproc.appendStylesheet(DefaultFactory.fromUri(Uri.OsPathToUri(xsl)))
        output = xsltproc.runNode(doc, 'file:///article.xml')

        outFile = open(file,'w')
        outFile.write(output)
        outFile.close()                  

    def export_to_html_old(self, file = None):

        body_html = self.app.ann_frame.sourceFrame.to_html()
        filter_html = self.app.ann_frame.filterFrame.to_html()

        html = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN""http://www.w3.org/TR/html4/strict.dtd">'
        html+= "<html><head><title>PyLogen - %s</title>" % self.app.ann_frame.filename
        html += '<STYLE TYPE="text/css">p.code {font-family:monospace;display:inline;}</STYLE>'
        html += "</head>"

        
        html += "<body>" + body_html +"\n" + filter_html+ "</body></html>"
        
        #html = html.replace("<tt></tt>","")
        html = html.replace("\n\n","\n")
        
        #html = html.replace("\n", '<p style="display:inline;"<br/></p>\n')
        html = html.replace("\n", '<p style="display:inline;"><br/></p>\n')
        html = html.replace('<p class="code"></p>','')

        html =html.replace('''
<p style="display:inline;"><br/></p>
<p style="display:inline;"><br/></p>
''','<p style="display:inline;"><br/></p>')
        
        
        fd = open(file,'w')
        fd.write(html)
        fd.close()                  
        self.export_file = file

        #print html
