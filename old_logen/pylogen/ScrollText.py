# A small portion of this file concerned with making the text widget read-only
# is derived from tkgui.py which is part of Anygui (anygui.sourceforge.net)
# Copyright  (c) 2001,  2002  Magnus Lie  Hetland,  Thomas Heller,  Alex
# Martelli,  Greg  Ewing, Joseph  A.  Knapka,  Matthew Schinckel,  Kalle
# Svensson, Shanky Tiwari, Laura  Creighton, Dallas T. Johnston, Patrick
# K. O'Brien, Phil Cook.


from Tkinter import *

import Pmw

readonly_keys = ["Down","Up","Left","Right","Next","Prior","Home","End"]
if "darwin" in sys.platform:
    readonly_keys.extend(["<Command-C>", "<Command-X>"])
else:
    readonly_keys.extend(["<Control-C>", "<Control-X>"])

class ScrollText(LabelFrame):
    """ A text frame with label title and two scroll bars    
    """
    
    app = None
    text = None
    scrollx = None
    scrolly = None
    
    commands = None
    readonly = None
    findmode = None
    findstring = None

    xview = None
    yview = None
    insert = None
    def store_pos(self):
        (self.yview,_) = self.text.yview()
        (self.xview,_) = self.text.xview()
        self.insert = self.text.index("insert")

    def restore_pos(self):
        self.text.yview_moveto(self.yview)
        self.text.xview_moveto(self.xview)
        self.text.mark_set("insert",self.insert)
        
    def findtext(self,text,start_pos="insert"):
        if len(text) > 0:
            index = self.text.search(text,"%s"%start_pos)
        
            if index == "":
                self.app.status.message("help", "Find as you type: '%s' failing" %text)
                return False
        else:
            index = "insert"

        self.app.status.message("help", "Find as you type: '%s'" %text)
        
        self.text.tag_remove("sel","1.0","end")
        self.text.tag_add("sel",index, "%s + %d chars" %(index, len(text)))
        self.text.mark_set("insert",index)
        self.text.see(index)
        return True
        
    def tab_pressed(self):
        cursor = self.text.index("insert linestart")
        str = self.text.get(cursor, "%s lineend" % cursor)
        self.text.delete(cursor, "%s lineend" % cursor)
        indent = self.calc_indent(cursor)        
        indent_space = ""
        for i in xrange(0,indent):
            indent_space += " "            
        self.text.insert(cursor, indent_space + str.strip())
        
    def return_pressed(self):

        ##current cursor position
        cursor = self.text.index("insert")        
        indent = self.calc_indent(self.text.index("%s + 1 line linestart"%cursor))
        indent_space = "\n"        
        for i in xrange(0,indent):
            indent_space += " "            
        self.text.insert(cursor, indent_space)
        

    def calc_indent(self, line):
        """
        calculates the index for line
        """
                
        previous_line = self.text.index("%s - 1 line" % line)
        #### what are the cases?
        ## previous line starts a predicate (ie has a :-) -> indent one level
        ## previous line ends with ., in which case no indent
        ## previous line ends with a "," in which case copy indent

        indent = 0
        if (self.is_predicate_start(previous_line)):
            return 4
        elif (self.is_predicate_end(previous_line)):
            return 0
        elif (self.is_increase_indent(previous_line)):
            indent = 4 #this will be increased

        #default case is to copy indent
        
        indent += self.get_indent(previous_line)
        ## calculate the indent for line based on previous line
        return indent

    def is_increase_indent(self,line):
        return False

    def is_predicate_start(self,line):
        str = self.text.get(line, self.text.index("%s lineend"%line))
        return ":-" in str
    
    def is_predicate_end(self,line):
        str = self.text.get(line, self.text.index("%s lineend"%line))
        return "." in str
        
    def get_indent(self,line):
        """ Return indent of line
        """
        str = self.text.get(line, self.text.index("%s lineend"%line))
        indent = 0
        while indent < len(str) and str[indent] == " ":
            indent += 1
        return indent
        
    def highlight_last_bracket(self,i=None):
        self.bracket = True
        if i == None:
            i = self.text.index("insert")
        #print self.text.index("%s-1chars"%start)

        bracket = 0
        limit = self.text.index("%s-2lines"%i)
        while self.text.compare(i,">",limit):
            c= self.text.get(i, self.text.index("%s+1chars"%i))
   
            if c == "(":
                if bracket == 0:
                    self.text.tag_add("(",i,self.text.index("%s+1chars"%i))
                    break
                else:
                    bracket = bracket -1
            elif c == ")":
                bracket = bracket + 1
            
            i = self.text.index("%s-1chars"%i)

        #call back to remove bracket after 1000ms
        self.text.after(1000,self.text.tag_remove,"(", "1.0", "end")
        
    def __init__(self,master=None,text="",app=None,readonly=True):
        
        LabelFrame.__init__(self,master,text=text,)
        
        self.bracket = False
        self.findmode = False
        self.findstring = ""
        self.stext = Pmw.ScrolledText(self)
        self.text = self.stext._textbox
        self.text.tag_configure("(", background="blue")        
        self.text["bg"] = "white"

        self.text.configure(highlightcolor="red")

        self.app = app

        #readonly
        self.readonly = not readonly
        self.set_readonly(readonly)

        self.stext.pack({"expand":"yes", "fill" :"both"})

        self.install_bindings(self.text)
            
    def clear(self):
        self.text.delete(0.0, "end")

    def save(self,file):
        fd = open(file,'w')
        
        ###stop the stupid thing from adding lots of blanks lines at
        ###end of file....
        fd.write(self.text.get("1.0","end -1 char"))        
        fd.close()

    def modified(self):
        try:
            self.text.edit_modified()
            return False
        except:
            return True

    def load(self, file):
        self.filename = file
        state = self.text["state"]
        self.app.status.set(("Loading %s"%(file)) )        
        self.text["state"] = "normal"
        self.clear()
        txt = open(file).read()
        self.text.insert(0.0, txt)

        self.app.status.set("Loaded" )        
        self.text.edit_reset()
        self.text["state"] = state

    def set_text(self, text):
        if self.readonly:
            self.text.config(state='normal')
        self.clear()
        self.text.insert(0.0, text)
        if self.readonly:
            self.text.config(state='disabled')
        
    def install_bindings(self, widget):
        self.ctl = 0
        self.alt = 0
        self.shift = 0
        widget.bind("<Key>", self.keybinding)
        widget.bind("<KeyPress-Control_L>", self.ctldown)
        widget.bind("<KeyRelease-Control_L>", self.ctlup)
        widget.bind("<KeyPress-Alt_L>", self.altdown)
        widget.bind("<KeyRelease-Alt_L>", self.altup)
        widget.bind("<KeyPress-Shift_L>", self.shiftdown)
        widget.bind("<KeyRelease-Shift_L>", self.shiftup)
        widget.bind("<Key-Insert>", self.insertbinding)
        widget.bind("<Key-Up>", self.arrowbinding)
        widget.bind("<Key-Down>", self.arrowbinding)
        widget.bind("<Key-Left>", self.arrowbinding)
        widget.bind("<Key-Right>", self.arrowbinding)
        widget.bind("<Key-Home>", self.arrowbinding)
        widget.bind("<Key-End>", self.arrowbinding)
        widget.bind("<Key-Next>", self.passthrough)
        widget.bind("<Key-Prior>", self.passthrough)
        widget.bind("<Key-slash>", self.find)
        widget.bind("<Key-backslash>", self.find)
        widget.bind("<ButtonRelease>", self.insertbinding)

    # Track modifier key state.
    def ctldown(self, ev):
        self.ctl = 1
    def ctlup(self, ev):
        self.ctl = 0
    def altdown(self, ev):
        self.alt = 1
    def altup(self, ev):
        self.alt = 0
    def shiftdown(self, ev):
        self.shift = 1
    def shiftup(self, ev):
        self.shift = 0

    def keybinding(self, ev):
        """ This method binds all keys, and causes them to be
        ignored when _readonly is set. """
        if self.readonly:
            # This is truly horrid. Please add appropriate
            # code for Mac platform, someone.
            if (ev.char == "\x03") or (ev.char == "c" and self.alt):
                # DON'T ignore this key: it's a copy operation.
                return None
            if self.alt or self.ctl:
                return None
            return "break"
        else:
            return None

    def insertbinding(self, ev):
        # Overrides keybinding for the Insert key.
        if not self.readonly:
            return None
        if self.ctl:
            # Allow copy.
            return None
        return "break"

    def check_highlight(self):
        i = self.text.index("insert+1chars")
        c = self.text.get(self.text.index("%s-1chars"%i),i)
        if c == ")":
            self.text.tag_remove("(", "1.0", "end")
            self.highlight_last_bracket(self.text.index("%s-2chars"%i))

    def passthrough(self, ev):
        if not self.readonly:
            self.check_highlight()
        return None

    def arrowbinding(self, ev):
        # If not read-only then do bracket highlighting and pass the event
        # through to the main text widget. Otherwise handle events directly
        if not self.readonly:
            self.check_highlight()
            return None

        if ev.keysym == "Down":
            self.text.yview(SCROLL, 1, "units")
        elif ev.keysym == "Up":
            self.text.yview(SCROLL, -1, "units")
        elif ev.keysym == "Left":
            self.text.xview(SCROLL, -1, "units")
        elif ev.keysym == "Right":
            self.text.xview(SCROLL, 1, "units")
        elif ev.keysym == "Home":
            self.text.yview(MOVETO, "0.0")
        elif ev.keysym == "End":
            self.text.yview(MOVETO, "1.0")

        return "break"

    def focus(self, ev):
        self.text.focus()

    def set_readonly(self, r):
        if r:
            if not self.readonly:
                self.text.config(state=DISABLED)
                self.text.bind('<1>', self.focus)
                if self.findmode:
                    self.cancel_search(False)
        else:
            if self.readonly:
                self.text.bind('<Key>', self.key_pressed)
                self.text.config(state=NORMAL)
                self.text.unbind('<1>')

        self.readonly = r

    def find(self, ev):
        if not self.readonly:
            return None

        if self.findmode:
            return self.find_key(ev)
        else:
            self.findmode = True                    
            self.app.status.message("help", "Find as you type: '%s'" %self.findstring)
            self.text.bind("<Key>", self.find_key)

        return "break"

    def cancel_search(self, report):
        self.findmode = False
        self.findstring = ""
        self.text.unbind("<Key>")
        if report:
            self.app.status.message("help", "Find as you type cancelled")

    def find_key(self, ev):
        if ev.keysym == "Escape":
            self.cancel_search(True)
            return "break"
        elif ev.keysym == "BackSpace":
            self.findstring = self.findstring[:-1]
        elif ev.keysym == "F3":
            self.findtext(self.findstring, start_pos="insert+1 char")
            return "break"
        else:
            ###some sort of valid key check...
            self.findstring += ev.char

        self.findtext(self.findstring)

        return "break"

    def key_pressed(self, ev):
        if self.bracket:
            self.text.tag_remove("(", "1.0", "end")
            self.bracket = False

        if ev.keysym == "parenright":
            self.highlight_last_bracket()
        elif ev.keysym == "Tab":
            self.tab_pressed()
            return "break"
        elif ev.keysym == "Return":
            self.return_pressed()
            return "break"
        
        return ""
