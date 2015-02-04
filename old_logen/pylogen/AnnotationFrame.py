from Tkinter import *
from PrologFrame import PrologFrame
from SourceFrame import SourceFrame
from FilterFrame import FilterFrame
import os
from ErrDialog import ask_question, ask_NoAnnfileDialog, complain_loudly
from FastIndex import FastIndex

try:
    from Ft.Xml import EMPTY_NAMESPACE
except:
    pass

from Prolog import PrologException
import parser
import time
import sets
class AnnotationFrame(PanedWindow):
    filename = None
    goals = None
    
    def __init__(self, master=None, app=None):
        PanedWindow.__init__(self, master, {"orient":"horizontal",
                                            "showhandle":"1",
                                            "handlepad":250})
        self.app = app

        self.sourceFrame = SourceFrame(self, "Source", app=app)
        self.filterFrame = FilterFrame(self, "Declarations", app=app)

        self.add(self.sourceFrame, minsize="4i")
        #self.add(self.filterFrame, minsize="2i")
        self.showfilter = False
        self.show_filter()
        
        self.filterFrame.text.tag_bind("filter", "<Button-1>", self.clicked_filter)

    def toggle_filter(self):
        if self.showfilter:
            self.hide_filter()
        else:
            self.show_filter()
            
    def hide_filter(self):
        if self.showfilter:            
            self.forget(self.filterFrame)
            self.showfilter = False
                    
    def show_filter(self):
        if not self.showfilter:            
            self.add(self.filterFrame, minsize="2i")
            self.showfilter = True
        
    def clicked_filter(self,event=None):
        """
        user clicked on a filter,
        we want to show relevant source code
        """
        text = self.filterFrame.text

        index = text.index("@" + str(event.x) + "," + str(event.y))

        (start,end) =text.tag_prevrange("filter","%s + 1 char" %index)
        head = text.get(start,end)
        (arity,args) = self.filterFrame.get_call_args(end, upto="end")
        sig = "%s/%d" % (head,arity)

        
        call_index = self.find_call(head,arity)

        if call_index is not None:
            (start,end) = call_index
            #self.sourceFrame.text.tag_add("sel",start,end)

            self.sourceFrame.text.mark_set("insert", start)
        
            self.sourceFrame.text.see(start)
          

    def find_call(self, call, arity, text=None, start="1.0"):
        if text is None:
            text = self.sourceFrame.text

        cur_loc = start
        next =  text.tag_nextrange("head", cur_loc)
        while next != ():
            (start, end) = next
            head = text.get(start,end)                        
            if head == call:
                (head_arity, args) = text.get_call_args(end, self.sourceFrame.annotation_tags)
                
                if arity == head_arity:
                    return (start,end)
            cur_loc = text.index("%s + 1 chars" % end)
            next =  text.tag_nextrange("head", cur_loc)
            
            
        return None
        

    def set_font(self, font):
        pass
        self.sourceFrame.text["font"] = font
        self.filterFrame.text["font"] = font

    def save(self):
        return self.sourceFrame.save(self.filename)
        
    def load(self, filename):
        self.filename = filename
        self.sourceFrame.load(filename)
        self.filterFrame.clear()

    def load_saftey(self,filename, annfile):
        (safety, _, _, _) = self.app.logen.get_annotations(filename,annfile)
        for i in range(0, len(safety), 3):
            start = int(safety[i]) -1
            stop = int(safety[i + 1]) -1 
            tag = safety[i + 2]
            oldtag = self.anns[i+2]
            if tag != oldtag:
                self.sourceFrame.text.tag_add("unsafe", 
                                  "1.0 + " + str(start) + " chars",
                                  "1.0 + " + str(stop) + " chars")

    def annotate_from_str(self, anns, filename,filter=None,filsyntax=None,syntax=None,tcl_index=False):
        self.anns = anns
        
        self.sourceFrame.load(filename)        
        filter_idx = FastIndex(filter)
        source_idx = FastIndex(self.sourceFrame.text.get("1.0", "end"))
        self.sourceFrame.highlight_list(anns, source_idx, tcl_index)

        if filter is not None:
            self.filterFrame.set_text_and_highlight(filter, filsyntax, filter_idx, tcl_index)

        if syntax is not None:
            self.sourceFrame.highlight_list(syntax, source_idx, tcl_index)
        
        
    def load_source(self, filename,annfile=None,reset=False):
        if annfile is None:
            annfile = filename + ".ann"
        else:
            print "Loading different annfile %s" % annfile
            
        if not os.path.exists(annfile):
            print "Annotation file (%s) does not exist for %s" % (annfile,filename)
                      
            if reset:                
                self.app.logen.annotate_file(filename, mode='auto_bta')
            else:
                ans = ask_NoAnnfileDialog(master=self.app, Filename=filename)
                if ans == "simple":
                    print "Simple BTA"                
                    if not self.app.logen.annotate_file(filename):
                        complain_loudly(master=self.app, msg="Could not create annotation file!")
                elif ans == "reset":
                    
                    #self.app.reset_bta_file(self,filename=filename)
                    #return
                    self.app.logen.annotate_file(filename, mode='auto_bta')
                else:
                    fd = open(filename + ".ann", "w")
                    fd.write("% Empty annotation file")
                    fd.close()    
            
        starttime = time.time()
        
        (anns, syntax, filter, filsyntax) = self.app.logen.get_annotations(filename,annfile)

        print "Get annotations ", (time.time()-starttime), "seconds"
        #save anns for safety checks...        
        self.anns = anns 
        self.filename = filename
                             
        #print "filters ", (time.time()-starttime), "seconds"                
        self.app.update()
        #print "load source ", (time.time()-starttime), "seconds"        
        self.annotate_from_str(anns,filename,filter,filsyntax,syntax)
        self.app.update()
        #print "highlight anns ", (time.time()-starttime), "seconds"        

        #print "highlight syntax ", (time.time()-starttime), "seconds"                
        self.load_pref(filename +".pref")

        self.clear_modified()


        #self.app.outbook.reset_completions()
        #completions = []        
        #for c in self.sourceFrame.get_all_heads():
        #    completions.append(c)
        #print "adding completions", completions
        #self.app.outbook.add_completions(completions)
    
    def clear_modified(self,val=False):
        self.sourceFrame.text.edit_modified(val)
        self.sourceFrame.ann_changed = val
        self.filterFrame.text.edit_modified(val)

    def reset_filters(self):
        filter = ""
        for e in self.entry:
            filter = filter + e + "\n"            
        self.filterFrame.set_text(filter)
        
    def load_pref(self,filename):
        self.goals = []
        self.entry = []
        self.norm = "list"
        self.benchmark = None
        self.iter = None
        
        print "Loading %s" % filename
        if os.path.exists(filename):
            print "Loading preferences"
            fd = open(filename,'r')
            cont = fd.read().split("\n")
            for pref in cont:
                
                pref = pref.strip()
                #print "Pref:", pref
                if pref.startswith("goal="):
                    self.goals.append(pref[5:])
                if pref.startswith("entry="):
                    self.entry.append(pref[6:])
                if pref.startswith("benchmark="):
                    self.benchmark = pref[10:]
                if pref.startswith("iter="):
                    self.iter = pref[5:]
                if pref.startswith("norm="):
                    self.norm = pref[5:]                    
            if (len(self.goals) > 0):
                self.app.specBar.set_goal(self.goals[0])
        self.app.norm_var.set(self.norm)
        
        #print "Loaded Preferences"
        #print "Benchmark:", self.benchmark
        #print "Iter:", self.iter
        
        
    def save_pref(self,filename):
        entry = self.entry
        #norm = self.norm
        
        norm = self.app.norm_var.get()
        
        iter = self.iter
        benchmark = self.benchmark

        #goals = self.goals
        goals = self.app.specBar.specEntry.get(0,"end")

        

        
        fd = open(filename, 'w')

        if entry is not None:
            for e in entry:
                fd.write('entry=%s\n' %e)
        if goals is not None:
            #if hasattr(goals, 'reverse'):
            #    goals.reverse()            
            for g in goals:
                fd.write('goal=%s\n' %g)                
        if norm is not None:
            fd.write('norm=%s\n' %norm)
        if iter is not None:
            fd.write('iter=%s\n' %str(iter))
        if benchmark is not None:
            fd.write('benchmark=%s\n' %benchmark)
        fd.close()
                
                
        



    ## def get_next_ann(self, cur_loc, text=None):
##         if text is None:
##             text = self.sourceFrame.text
##         next_start = text.index("end")
##         start = ""
##         for tag in self.sourceFrame.annotation_tags:
##             i = text.tag_nextrange(tag, cur_loc)
##             if i != ():
##                 (cur_start, cur_end) = i
##                 if text.compare(cur_start, "<", next_start):
##                     next_start = cur_start
##                     start = (tag, cur_start, cur_end)

##         if start == "":
##             return ("", 0.0, 0.0)
##         else:
##             return start
    
    

    def modified(self):
        return self.sourceFrame.modified()

    def ann_modified(self):
        return self.filterFrame.modified() or self.sourceFrame.ann_changed
    

    def goto_next_unknown(self, start ="insert"):
        text = self.sourceFrame.text
        current = text.index(start)
        
        i = text.tag_nextrange("errorTag", current)
        
        if i != ():
            (start,end) = i
            text.see(start)
            text.mark_set("insert",end)

        
    def goto_prev_unknown(self,start="insert"):
        text = self.sourceFrame.text
        current = text.index(start)
        
        i = text.tag_prevrange("errorTag", current)

        if i != ():
            (start,end) = i
            text.see(start)
            text.mark_set("insert",start)

    def filter_to_string(self):
        return self.filterFrame.text.get(1.0, "end")
    
    def ann_to_string(self):
        text = self.sourceFrame.text

        ## remove annotations that are not to be saved:
        text.tag_remove("errorTag", "1.0", "end")
        text.tag_remove("unsafe", "1.0", "end")
        
        annotations = ""
        hidenf_ranges = text.tag_ranges("hide_nf")
        start_idx = 0
        end_idx = 1

        cur_loc = "1.0"
        (ann, start, stop) = self.sourceFrame.get_next_ann(cur_loc, self.sourceFrame.annotation_tags)
        while ann != "":
            cur_loc = stop
            if start_idx < len(hidenf_ranges) and \
               text.compare(cur_loc, ">=", hidenf_ranges[start_idx]):
                annotations += "start_hide_nf -1 -1 "
                start_idx += 2
                
            annotations += (ann + " " + start + " " + stop + " ")

            if end_idx < len(hidenf_ranges) and \
               text.compare(cur_loc, ">=", hidenf_ranges[end_idx]):
                annotations += "end_hide_nf -1 -1 "
                end_idx += 2

            (ann, start, stop) = self.sourceFrame.get_next_ann(cur_loc, self.sourceFrame.annotation_tags)

        if end_idx < len(hidenf_ranges) and \
           text.compare(cur_loc, ">=", hidenf_ranges[end_idx]):
            annotations += "end_hide_nf -1 -1 "
            end_idx += 2
        return annotations
        
    def save_ann(self):

        annotations = self.ann_to_string()
        filters = self.filter_to_string()
        
        try:
            self.app.logen.save_annotations(self.filename, annotations, filters)

        except PrologException,e:
            ## Error has occurred most probably parsing....
            (Title, Msg) = e
            complain_loudly(master=self.app, title=Title,msg=Msg)
            
            return False
        
        self.save_pref(self.filename+".pref")
        
        self.load_source(self.filename)

        self.clear_modified()

        return True
        
    def sanity_check(self):
        print "Checking file"

        ErrMsg = {}
        
        text = self.sourceFrame.text

        ## first we reset all errors
        text.tag_remove("errorTag", "1.0","end")

        ## First we collect all defined predicates in file
        ## these must be tagged with head (we ignore arity as we are lazy)
        all_heads = self.sourceFrame.get_all_heads()
        #all_heads = sets.Set()
        #iter = self.ann_iter("head", text, retval="text")
        #for ann in iter:            
        #    all_heads.add(ann)

        all_filters = sets.Set()
        ## next we collect all the filters
        iter = self.filterFrame.ann_iter("filter", retval="text_index")
        for ann in iter:
            (filter, start,end) = ann
            (arity, _) = self.filterFrame.get_call_args(end, upto="end")
            new_filter = "%s/%d" % (filter,arity)
            all_filters.add(new_filter)

        returnVal = True

        ## first check -- all unfolded predicates exist
        iter = self.sourceFrame.ann_iter("unfold", retval="text_index")
        for ann in iter:
            (call,start,end) = ann            
            (arity, _) = self.sourceFrame.get_call_args(end, tags=self.sourceFrame.annotation_tags)
            call = "%s/%d" % (call, arity)
            #call = text.get(start,end)
            if call not in all_heads:
                returnVal = False
                text.tag_add("errorTag", start, end)
                ErrMsg[start] = "Call to %s is unfolded but not defined in file." % call

        ## second check -- all memo predicates exist and are given valid filters
        iter = self.sourceFrame.ann_iter("memo",  retval="text_index")
        for ann in iter:
            (call, start,end) = ann
            (arity, _) = self.sourceFrame.get_call_args(end, tags=self.sourceFrame.annotation_tags)
            call = "%s/%d" % (call, arity)
            
            ### add code to check if valid filter exists
            
            if call not in all_heads:
                returnVal = False                
                text.tag_add("errorTag", start, end)
                ErrMsg[start] = "Call to %s is memoed but not defined in file." % call                
            elif call not in all_filters:
                returnVal = False                
                text.tag_add("errorTag", start, end)        
                ErrMsg[start] = "Call to %s is memoed but no filter is defined" % call

        iter = self.sourceFrame.ann_iter("unknown", retval="index")
        for ann in iter:
            returnVal = False            
            (start,end) = ann
            call = text.get(start,end)            
            text.tag_add("errorTag", start, end)
            ErrMsg[start] = "Call to %s annotation is unknown" % call

        for line in ErrMsg:
            print line, ErrMsg[line]

        # Save the messages for hover over warnings
        self.sourceFrame.ErrMsg = ErrMsg
            
        return returnVal    

    #def ann_iter(self,Annotation, text,Start="1.0", retval= "text"):
    #    current = Start
    #    i = text.tag_nextrange(Annotation, current)
    #    while i != ():
    #        (start, current) = i
    #        if retval == "text":
    #            yield text.get(start,current)
    #        elif retval == "index": 
    #            yield i
    #        elif retval == "text_index":
    #            yield (text.get(start,current), start,current)
    #        i = text.tag_nextrange(Annotation, current)
        
    def build_xml(self, doc):
        # Create root node
        root = doc.createElementNS(EMPTY_NAMESPACE, 'article')
        doc.appendChild(root)
        
        # Add source node to XML
        sourceElement = doc.createElementNS(EMPTY_NAMESPACE, 'source')
        doc.documentElement.appendChild(sourceElement)
        source = self.sourceFrame.text.get(1.0, "end")
            

        # Add annotations to XML
        lines = source.split('\n')
        tags = self.sourceFrame.get_all_tags()

        print "there are ", len(tags), " tags in the source frame"
        
        # add null tag at end as simplifies coding
        tags.append(('null', '99999999.1', '99999999.1'))

        add_to_dom(doc, sourceElement, lines, tags, 0,
                   1, 0, len(lines), len(lines[-1]))
        print 'source tree created'

        # Add filters node to XML
        filtersElement = doc.createElementNS(EMPTY_NAMESPACE, 'filters')
        doc.documentElement.appendChild(filtersElement)
        filters = self.filter_to_string()
        #filtersElement.appendChild(
            #doc.createTextNode(self.filter_to_string()))

        lines = filters.split('\n')
        tags = self.filterFrame.get_all_tags()

        print "there are ", len(tags), " tags in the filter frame"
        
        # add null tag at end as simplifies coding
        tags.append(('null', '99999999.1', '99999999.1'))

        add_to_dom(doc, filtersElement, lines, tags, 0,
                   1, 0, len(lines), len(lines[-1]))
        print 'filters tree created'

def append_text(doc, node, source, sLine, sCol, eLine, eCol):
    if sLine < eLine:
        node.appendChild(doc.createTextNode(source[sLine - 1][sCol:] + '\n'))
        for i in xrange(sLine, eLine - 1):
            node.appendChild(doc.createTextNode(source[i] + '\n'))
        node.appendChild(doc.createTextNode(source[eLine - 1][:eCol]))
    elif sLine == eLine and sCol < eCol:
        node.appendChild(doc.createTextNode(source[eLine - 1][sCol:eCol]))

def add_to_dom(doc, node, source, tags, currentTag, posLine, posCol, eLine, eCol):
    i = currentTag
    while i < len(tags):
        annotation = tags[i]
        
        (startLine, startCol) = annotation[1].split('.')
        startLine = int(startLine)
        startCol = int(startCol)
        (endLine, endCol) = annotation[2].split('.')
        endLine = int(endLine)
        endCol = int(endCol)
        
        if startLine > eLine or (startLine == eLine and startCol >= eCol):
            append_text(doc, node, source, posLine, posCol, eLine, eCol)
            return i

        append_text(doc, node, source, posLine, posCol, startLine, startCol)
        
        posCol = endCol
        posLine = endLine
          
        annotationElement = doc.createElementNS(EMPTY_NAMESPACE, annotation[0])
        node.appendChild(annotationElement)
        i = add_to_dom(doc, annotationElement, source, tags, i + 1,
                       startLine, startCol, endLine, endCol)

    return i
