from ScrollText import ScrollText
from Tkinter import *

from Prolog import PrologException
from FastIndex import FastIndex
import ErrDialog
import re

import AnnotationColours
import sets
#import Colour

import parser

def startCompare(x, y):
  (xLine, xCol) = x[1].split('.')
  (yLine, yCol) = y[1].split('.')
  if xLine == yLine:
    return int(xCol) - int(yCol)
  else:
    return int(xLine) - int(yLine)

class PrologFrame(ScrollText):
    menu = None
    annotation_menu = None
    ann_changed = None
    ErrMsg = None
    filter_tags = None
    filter_colour = None
    annotation_colour = None
    app = None
    
    def colours_changed(self, new_colours):
        for t in new_colours:
            self.text.tag_configure(t, foreground=new_colours[t])

    def __init__(self,master=None, text="",readonly=True,app=None):
        ScrollText.__init__(self,master=master,text=text,readonly=readonly)
        self.app = app

        app.pref.register_listener('syntax highlights', self.colours_changed)

        self.colours_changed(app.colours.syn_colour)

        self.text.tag_configure("hilite", background="grey")

        ## force the selection cursor to have higher priority than
        ## all annotations
        self.text.tag_raise("sel",aboveThis="hilite")

    def load_source(self, file, syntax=True):
        if syntax:
            syntax = self.app.logen.get_syntax(file)        

        self.load(file)

        if syntax:
            fast_idx = FastIndex(self.text.get("1.0", "end"))
            self.highlight_list(syntax, fast_idx)        
            return fast_idx


    def parse_check(self):
        self.text.tag_remove("errorTag", "1.0", "end")        
        try:
            self.app.logen.parse_file(self.filename)
            
        except PrologException, (Type,Msg):
            lineno = re.compile('.*lines::([0-9]+)-([0-9]+)\n.*')
            m = lineno.search(Msg)
            if m is not None:
                line = m.group(1)
                lineend = m.group(2)
                self.text.tag_add("errorTag", "%s.0"%line,"%s.0 lineend"%lineend)
            ErrDialog.complain_loudly(master=self,title="Parse Error:::",msg=Msg+"\nPlease fix errors before entering annotation mode")
            return False

        return True
        

    def highlight_list(self, syntax, fast_idx, tcl_index=False):
        #print syntax
        self.ErrMsg = None
        clp = True
        if self.text.search("{","1.0") == "":            
            clp = False
        
        for i in xrange(0, len(syntax), 3):                        
            tag = syntax[i + 2]
            if tcl_index:
	      	### they are already in tcl form so leave them alone...
              	(index, index1) = (syntax[i], syntax[i+1])

            else:
              start = int(syntax[i]) - 1
              end  = int(syntax[i + 1]) - 1
              (index, index1) = fast_idx.get_two_tk_indices(start, end)

            if clp:
                if self.text.get(index,index1).startswith('{'):
                    clpend = self.text.search("}",index1, fast_idx.line_end(index))
                    index1 = fast_idx.next_char(clpend) 

            self.text.tag_add(tag, index, index1)


    def set_text_and_highlight(self, text, syntax, fast_idx, tcl_index=False):
        self.set_text(text)
        if syntax is not None:
            self.highlight_list(syntax, fast_idx, tcl_index)


    def save(self, filename):
        #if not self.app.logen.check_string_syntax(self.text.get("1.0", "end")):
        #    return False

        #steve remove parse check...

        ScrollText.save(self, filename)
        return True

    def get_tag_position(self, ann, index):
        newindex = self.text.index(index + " + 1 char")
        return self.text.tag_prevrange(ann, newindex)

    #steve refactored a bit and moved these into here (instead of Ann Frame)
    def get_call_args(self, end, upto=None, tags=None):
        if upto is None:
            (uptoTest,upto, _) = self.get_next_ann(end, tags)
            if uptoTest == "":
                upto = "end"

        posargs = self.text.get(end, upto)
        return parser.get_arity(posargs)

    def get_all_heads(self, text=None):
        if (text is None):
            text = self.text
        all_heads = sets.Set()
        
        iter = self.ann_iter("head", retval="text_index")
        heads = self.text.tag_ranges('head')
        
        for i in xrange(0, len(heads), 2):
            start = heads[i]
            end = heads[i + 1]
            head = text.get(start, end)
            # we guess by doing upto the next ann (or end of file..)
            if i < len(heads) - 2:
                next_head = heads[i + 2]
            else:
                next_head = "end"
            (arity, args) = self.get_call_args(end, upto=next_head)
            all_heads.add("%s/%d" % (head, arity))

        return all_heads

    def get_prev_ann(self, cur_loc, tag_list):
        text = self.text
        next_start = text.index("1.0")
        start = ""
        for tag in tag_list:
            i = text.tag_prevrange(tag, cur_loc)
            if i != ():
                (cur_start, cur_end) = i
                if text.compare(cur_start, ">", next_start):
                    next_start = cur_start
                    start = (tag, cur_start, cur_end)

        if start == "":
            return ("", 0.0, 0.0)
        else:
            return start

    def get_next_ann(self, cur_loc, tag_list):
        text = self.text
        next_start = text.index("end")
        start = ""
        for tag in tag_list:
            i = text.tag_nextrange(tag, cur_loc)
            if i != ():
                (cur_start, cur_end) = i
                if text.compare(cur_start, "<", next_start):
                    next_start = cur_start
                    start = (tag, cur_start, cur_end)

        if start == "":
            return ("", 0.0, 0.0)
        else:
            return start


    def ann_iter(self,Annotation,Start="1.0", retval= "text"):

        text = self.text
      
        current = Start
        i = text.tag_nextrange(Annotation, current)
        while i != ():
            (start, current) = i
            if retval == "text":
                yield text.get(start,current)
            elif retval == "index": 
                yield i
            elif retval == "text_index":
                yield (text.get(start,current), start,current)
            i = text.tag_nextrange(Annotation, current)

    
    ## HTML PRINTING...
    
    def get_all_tags(self, text=None):
        if text is None:
            text = self.text

        tags = []
        for tag in text.tag_names():
            tag_list = text.tag_ranges(tag)
            for i in range(0, len(tag_list), 2):
              tags.append((tag, tag_list[i], tag_list[i + 1]))
        tags.sort(startCompare)
        return tags
                
    def get_next_tag(self, cur_loc, text=None):
        if text is None:
            text = self.text
            
        next_start = text.index("end")
        start = ""
        for tag in text.tag_names():
            i = text.tag_nextrange(tag, cur_loc)
            if i != ():
                (cur_start, cur_end) = i
                if text.compare(cur_start, "<", next_start):
                    next_start = cur_start
                    start = (tag, cur_start, cur_end)

        if start == "":
            return ("", 0.0, 0.0)
        else:
            return start

    def htmlstring(self,str):
        str = str.replace("\t", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
        str = str.replace(" ","&nbsp;")
        #str("  ","&nbsp;&nbsp;")
        
        #return "<tt>%s</tt>"%str
        return str


    def convertColour(self,colour):
        if colour == "":
            return "#000000"
        rgb = self.text.winfo_rgb(colour)
        red, green, blue = rgb[0]/256, rgb[1]/256, rgb[2]/256
        return "#%02x%02x%02x" % (red,green,blue)
        #if colour == "hotpink1":
        #    return "gray"
        #return colour

    
    def getHtmlTag(self, text, ann):
        
        #openTag = '<FONT COLOR="%s">' % (self.convertColour(text.tag_cget(ann,"foreground")))
        #closeTag = '</FONT>'

        if text.tag_cget(ann,"underline") != "":
            under = "text-decoration:underline;"
        else:
            under = ""
            
        openTag = '<p class="code" style="color:%s;%s">'%(self.convertColour(text.tag_cget(ann,"foreground")),under)
        closeTag = '</p>'
        
        return (openTag,closeTag)
    
           
    def line_to_html(self,text, line):
        html = ""
        cur_loc = line

        lineend = text.index("%s lineend" % line)

        #print "Processing line ", line, lineend
        stop = "0.0"
        start = "0.0"

        (ann, start, stop) = self.get_next_tag(cur_loc)
        while(start != "" and text.compare(start, "<=", lineend) and text.compare(start,">=", line) and text.compare(stop,"<=", lineend)):
            html += '<p class="code">'+self.htmlstring(text.get(cur_loc, start))+ "</p>"

            (openTag, closeTag) = self.getHtmlTag(text,ann)
            
            html += openTag
            html += self.htmlstring(text.get(start,stop))            
            cur_loc = stop
            
            html += closeTag
            (ann, start, stop) = self.get_next_tag(cur_loc)
                                    
        if text.compare(stop,">=", lineend) and text.compare(start, "<=", lineend):
                (openTag, closeTag) = self.getHtmlTag(text,ann)
                html += openTag
                html += self.htmlstring(text.get(cur_loc, lineend))
                html += closeTag            
        else:
            html += '<p class="code">'+self.htmlstring(text.get(cur_loc, lineend))+'</p>'               
        return html

    
    def to_html(self, text =None, html_mode=True):
        print "Converting to html"
        html = ""
        if text is None:
            text = self.text
            
        #remove non print tags
        text.tag_remove("errorTag", "1.0", "end")
        text.tag_remove("unsafe", "1.0", "end")

        #start at begining
        
        cur_loc = "1.0"
        prev_loc = "0.0"
        
        while cur_loc != prev_loc:
            #print cur_loc
            if html_mode:
                html += self.line_to_html(text,cur_loc) + "\n"
            else:
                #html += self.line_to_tex(text, cur_loc) + "\n"
                pass
                
            prev_loc = cur_loc
            cur_loc = text.index("%s +1 line linestart" % cur_loc)                                                        
        return html
