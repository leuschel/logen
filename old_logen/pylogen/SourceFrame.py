from ScrollText import ScrollText
from Tkinter import *

from PrologFrame import PrologFrame
from Prolog import PrologException
from FastIndex import FastIndex
import ErrDialog
import re

import AnnotationColours
#import Colour

import parser

def startCompare(x, y):
    (xLine, xCol) = x[1].split('.')
    (yLine, yCol) = y[1].split('.')
    if xLine == yLine:
        return int(xCol) - int(yCol)
    else:
        return int(xLine) - int(yLine)

class SourceFrame(PrologFrame):
    def __init__(self, master=None, text="", readonly=True, app=None):
        PrologFrame.__init__(self, master=master, text=text,
                             readonly=readonly, app=app)
        self.app = app

        app.pref.register_listener('annotation highlights',
                                   self.annotation_colours_changed)
        self.colours_changed(app.colours.ann_colour)
        app.pref.register_listener('filter highlights', self.colours_changed)
        self.colours_changed(app.colours.fil_colour)
        for fTag in ['dynamic', 'static']:
            self.text.tag_bind(fTag, "<Motion>",
                lambda _, fTag=fTag: self.mouse_over_filter_tag(fTag))
            self.text.tag_bind(fTag, "<Leave>",
                lambda _, fTag=fTag: self.mouse_leave_filter_tag(fTag))

        self.text.tag_configure("unsafe", background="red")
        self.text.tag_configure("hide_nf", background="yellow")
        self.text.tag_configure("errorTag", background="red")

        self.last_annotation = ""

        self.annotation_tags = AnnotationColours.annotations
        self.annotation_colour = self.app.colours.ann_colour

        self.menu = {}

        self.annotation_menu = {"unfold":"call", "call":"call", "memo":"call",
                                "rescall":"call", "ucall":"call",
                                "mcall":"call", "unknown":"call",
                                "semicall":"call", "online":"call",
                                "if":"if", "resif":"if", "semif":"if",
                                "logif":"logif", "reslogif":"logif",
                                "findall":"findall", "resfindall":"findall",
                                "resnot":"not", "not":"not",
                                ";":"or", "resdisj":"or",
                                "pp_cll":"pp_cll",
                                "pp_mnf":"pp_mnf",
                                "time_out":"time_out",
                                "mnf":"mnf",
                                "when":"when", "reswhen":"when",
                                "semiwhen":"when",
                                "gxspec":"module", "gx":"module",
                                #"spec":"module",
                               }

        self.text.bind("<Alt-n>", self.keyb_next_ann)
        self.text.bind("<Alt-p>", self.keyb_prev_ann)

        self.commands = []
        if (sys.platform == "win32"):
            Menu_Key = "<Button-3>"
        else:
            Menu_Key = "<Button-1>"

        self.annotation_colours_changed(self.annotation_colour)

        for tag in self.annotation_tags:
            self.text.tag_bind(tag, "<Motion>", self.mouse_over_tag)
            self.text.tag_bind(tag, "<Leave>", self.mouse_leave_tag)
            self.text.tag_bind(tag, Menu_Key, self.mouse_click_tag)

        #self.text.tag_bind("hide_nf", Menu_Key, self.hidenf_click)

        self.text.tag_configure("unknown", background="black",
                                foreground="white")

        #make call and rescall stand out as requested by mv
        self.text.tag_configure("call", underline=True)
        self.text.tag_configure("rescall", underline=True)

        self.text.tag_configure("unsafe", background="red", foreground="white")

    def annotation_colours_changed(self, map):
        # reset menus
        for m in ['call', 'if', 'logif', 'findall', 'not', 'or', 'pp_cll',
                  'pp_mnf', 'time_out', 'mnf', 'when', 'module']:
            self.menu[m] = Menu(self.app, tearoff=0)
        self.commands = []
        menus = {}
        self.hide_nf_menu_pos = {}
        for tag in self.annotation_tags:
            if self.hide_nf_menu_pos.get(self.annotation_menu[tag]) == None:
                # start at 2 because item 0, there will be a separator
                self.hide_nf_menu_pos[self.annotation_menu[tag]] = 2
            else:
                self.hide_nf_menu_pos[self.annotation_menu[tag]] += 1
            self.commands.append(lambda tag=tag: self.change_ann(tag))
            menus[self.annotation_menu[tag]] = 1
            self.menu[self.annotation_menu[tag]].add_command(label=tag,
                                       foreground=map[tag],
                                       command=self.commands[-1],
                                       underline=0)
        # STEVE : should perhaps to be restricted to only a few menus...
        for m in menus:
            menu = self.menu[m]
            menu.add_separator()
            menu.add_command(label="Remove hide_nf",
                             command=self.remove_hidenf)

        self.colours_changed(map)

    def new_hidenf(self):
        sel = self.text.tag_ranges("sel")
        if sel != ():
            (start, stop) = sel

            #h2 = self.get_tag_position("head", stop)
            h2 = self.text.tag_prevrange("head", stop)

            if h2 == ():
                # attempting to annotate before any head tags, which means
                # nothing of use is being annotated!
                print "annotation is pointless as before all heads"
                return
            elif self.text.compare(h2[1], ">", start):
                print "annotation encompasses head"
                return

            h1 = self.get_prev_ann(stop, self.annotation_tags)
            hidenf_stop = h1[2]
            (_, s1, e1) = self.get_prev_ann(start, self.annotation_tags)
            if self.text.compare(start, ">=", s1) and \
               self.text.compare(start, "<", e1):
                hidenf_start = s1
            else:
                (_, hidenf_start, _) = self.get_next_ann(start, self.annotation_tags)

            if self.text.compare(hidenf_start, ">", hidenf_stop) or \
               hidenf_start == 0.0:
                print "no clauses selected"
                return

            #print hidenf_start, hidenf_stop
            self.text.tag_add("hide_nf", hidenf_start, hidenf_stop)
            self.text.tag_remove("sel", start, stop)
            self.text.ann_changed = True

    def remove_hidenf(self):
        # should never be called if there is no hide_nf tag or error will occur
        (_, (start, _)) = self.selectedAnn
        (start, stop) =  self.get_tag_position("hide_nf", start)
        self.text.tag_remove("hide_nf", start, stop)

    def change_ann(self, new_ann, selected=None):
        if selected is None:
            (ann, (start, stop)) = self.selectedAnn
        else:
            (ann, (start, stop)) = selected

        if ann != new_ann:
            self.text.tag_remove(ann, start, stop)
            self.text.tag_add(new_ann, start, stop)
            self.ann_changed = True

    def keyb_change_ann(self,next=True):
        (ann,(start,end)) =  self.get_annotation_at(index="insert")

        if ann in  self.annotation_tags:
            group =  self.annotation_menu[ann]
            poss = []
            for i in self.annotation_menu:
                if self.annotation_menu[i] == group:
                    poss += [i]

            if next:
                next = self.get_next_from_list(ann,poss)
            else:
                next = self.get_prev_from_list(ann,poss)
            self.change_ann(next,(ann,(start,end)))
            self.app.status.message('help', next)

    def keyb_next_ann(self, unused):
        self.keyb_change_ann(next=True)

    def keyb_prev_ann(self, unused):
        self.keyb_change_ann(next=False)

    def get_next_from_list(self, item, list):
        for i in xrange(0, len(list)):
            if list[i] == item:
                if i < len(list) - 1:
                    return list[i + 1]
                else:
                    return list[0]
        #if not found just give first
        return list[0]

    def get_prev_from_list(self, item, list):
        for i in xrange(0, len(list)):
            if list[i] == item:
                if i == 0:
                    return list[-1]
                else:
                    return list[i - 1]
        #if not found just give first
        return list[0]

    def mouse_over_tag(self, event):
        self.highlight_tag(event, False)

    def mouse_leave_tag(self, unused):
        self.text.tag_remove("hilite", "1.0", "end")
        self.last_annotation = ""
        self.app.status.clear()

    def mouse_click_tag(self, event):
        self.highlight_tag(event, True)

    def hidenf_click(self, event):
        #not used.
        # if want to remove hide_nf by clicking on just hide_nf annotated code
        # (eg no unfold) then add code here
        print "hidenf_click"
        (ann, (start, stop)) = self.get_annotation_at(event.x, event.y)
        print ann, start, stop
        return "break"

    def highlight_tag(self, event, show_menu):
        (ann, (start, stop)) = self.get_annotation_at(event.x, event.y)
        if self.last_annotation != "":
            (s,e) = self.last_annotation
            self.text.tag_remove("hilite", s, e)

        self.text.tag_add("hilite", start, stop)
        self.last_annotation = (start, stop)
        if self.ErrMsg != None and start in self.ErrMsg:
            self.app.status.set(self.ErrMsg[start] + ":::" + ann + " - " + str(start) + " -> " + str(stop))
        else:
            self.app.status.set(ann + " - " + str(start) + " -> " + str(stop))

        if show_menu:
            self.selectedAnn = (ann, (start, stop))
            menu = self.menu[self.annotation_menu[ann]]
            menu.tk_popup(event.x_root, event.y_root)
            hide_nf = self.get_tag_position("hide_nf", start)
            state = NORMAL
            if hide_nf == ():
                state = DISABLED
            else:
                (hstart, hend) = hide_nf
                if self.text.compare(hend, "<", start):
                    state = DISABLED
                
            menu.entryconfig(self.hide_nf_menu_pos[self.annotation_menu[ann]],
                             state=state)

    def get_annotation_at(self, x=None, y=None,index=None):
        if index is None:
            index = self.text.index("@" + str(x) + "," + str(y))

        curann = self.text.tag_names(index)
        for ann in curann:
            if self.annotation_tags.count(ann) > 0:
                return (ann, self.get_tag_position(ann, index))
        return ("", (0.0, 0.0))

    def get_tag_position(self, ann, index):
        newindex = self.text.index(index + " + 1 char")
        return self.text.tag_prevrange(ann, newindex)

    def mouse_over_filter_tag(self, filterTag):
        self.app.status.set("Argument annotated as %s" % filterTag)

    def mouse_leave_filter_tag(self, unused):
        self.app.status.clear()

    
    def getHtmlTag(self, text, ann):

        if text.tag_cget(ann,"underline") != "":
            under = "text-decoration:underline;"
        else:
            under = ""

        openTag = '<p class="code" style="color:%s;%s">'%(self.convertColour(text.tag_cget(ann,"foreground")),under)
        closeTag = '</p>'

        if ann in self.annotation_tags:
            openTag+= '<a onmouseover="window.status=\'%s\';" onmouseout="window.status=\'\';"  >' % ann
            closeTag = '</a>' +closeTag

        return (openTag,closeTag)

