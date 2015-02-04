from tkSimpleDialog import Dialog
from ScrolledText import ScrolledText
from Tkinter import *

import Pmw

def ask_question(master=None, title="Question", msg=""):
    e = OkCancelDialog(master=master, Title=title, Msg=msg)
    return e.result
    
def complain_loudly(master=None, title="Error", msg=""):
    ErrDialog(master=master, Title=title, Msg=msg)

def ask_NoAnnfileDialog(master=None, Filename=""):
    e = NoAnnDialog(master=master,Filename=Filename)
    return e.result

def ask_FilterDialog(master=None, Filters="",Heads=[]):
    e = FilterDialog(master=master,Filters=Filters,Heads=Heads)
    if e.result == "cancel":
        return None
    else:    
        return e.filters


class OkCancelDialog(Dialog):
    result = None

    def __init__(self,master=None, Title="Question", Short="",Msg=""):
        self.Title=Title
        self.Msg=Msg
        self.Short = Short
        Dialog.__init__(self,master,title=Title)
        
    def body(self,unused):
        self.text = ScrolledText(self)
        self.text["height"] = "6"
        self.text["width"] = "50"
        
        self.text.pack(side="top",expand="yes",fill="both")
        self.text.insert("0.0", self.Msg)
        self.text["state"] = "disabled"
        self.result = False

    def ok(self, unused=None):
    	self.result = True
        Dialog.ok(self)

class NoAnnDialog(Dialog):
    result = None

    def __init__(self,master=None, Title="No Annotation File Found", Filename="",
                 Msg="The file you have loaded does not have an associated annotation file"):
        self.Title=Title
        self.Msg=Msg
        self.filename = Filename
        
        #self.Short = Short
        
        Dialog.__init__(self,master,title=self.Title)
        
        
    def body(self,unused):
        self.text = ScrolledText(self)
        
        self.text["height"] = "6"
        self.text["width"] = "50"
        
        self.text.pack(side="top",expand="yes",fill="both")
        self.text.insert("1.0", self.Msg)
        self.text["state"] = "disabled"
        self.result = False

    def ok(self, unused=None):
    	self.result = True
        Dialog.ok(self)
        return True
    

    def buttonbox(self):
        box = Frame(self)

        w = Button(box, text="Simple BTA", width=10, command=lambda :self.click("simple"), default=ACTIVE)
        w1 = Button(box, text="Reset File", width=10, command=lambda :self.click("reset"), default=ACTIVE)
        w2 = Button(box, text="Mark Unknown", width=10, command=lambda :self.click("unknown"), default=ACTIVE)        

        w.pack(side=LEFT, padx=5, pady=5)
        w1.pack(side=LEFT, padx=5, pady=5)
        w2.pack(side=LEFT, padx=5, pady=5)

        #self.bind("<Return>", self.ok)


        box.pack()

    def click(self, value):
        self.result = value        
        Dialog.ok(self)

        
class ErrDialog(Dialog):
    def __init__(self,master=None, Title="Error", Short="",Msg=""):
        self.Title=Title
        self.Msg=Msg
        self.Short = Short
        Dialog.__init__(self,parent=master,title=Title)
        
        
    def body(self,unused):
        self.text = ScrolledText(self)
        self.text["height"] = "8"
        self.text["width"] = "50"
        
        self.text.pack(side="top",expand="yes",fill="both")
        self.text.insert("0.0", self.Msg)
        self.text["state"] = "disabled"
        self.result = False

    def buttonbox(self):
        box = Frame(self)

        w = Button(box, text="OK", width=10, command=self.ok, default=ACTIVE)
        w.pack(side=LEFT, padx=5, pady=5)

        self.bind("<Return>", self.ok)

        box.pack()


class FilterDialog(Dialog):
    result = None
    filters = None

    def __init__(self,master=None, Title="Add Filter", Filters="",Heads=[]):
        self.Title=Title
        self.Heads =Heads
        self.filters = Filters
        Dialog.__init__(self,master,title=self.Title)
        
        
    def body(self,unused):
        box = Frame(self)
        
        #self.listbox = Listbox(box,height=6,bg="white",width=15)
        self.slistbox = Pmw.ScrolledListBox(box,listbox_height=6)#,hull_width=150,usehullsize=1,hull_height=150)
        
        self.listbox = self.slistbox._listbox
        
        
        
        #self.slistbox = Pmw.ScrolledListBox(box)
        
        
        for item in self.Heads:
            self.listbox.insert(END, item)

        self.text = Text(box,height=8,width=40,bg="white")

        self.text.insert("end", self.filters)
        self.listbox.bind("<Double-Button-1>", self.clickList)
        
        
        box.pack(side="top")
        #self.listbox.pack(side="left")
        self.slistbox.pack(side="left")
        self.text.pack(side="left")
        self.result = False

    def default_filter(self, arity):
        arity = int(arity)
        if arity == 0: return ""

        filters = "( dynamic" 
        ### add code here to make dynamic filter of arity
        
        for i in range(1,arity):
            filters += " , dynamic"
            

        filters += " )"

        return filters
    
    def clickList(self,unused):
        i =  self.listbox.curselection()[0]       
        head = self.Heads[int(i)]
        
        if self.text.index("insert") != self.text.index("insert linestart"):
            index = self.text.index("insert + 1 line linestart")
            
        else:
            index = self.text.index("insert")

        (call, arity) = self.Heads[int(i)].split('/')

        filString = ":- filter %s%s.\n" %(call, self.default_filter(arity))
        self.text.insert(index, filString)
        place = self.text.index("%s + %d chars" % (index,len(filString)-3))
        self.text.mark_set("insert", place)        
        self.text.focus_set()

    def buttonbox(self):
        box = Frame(self)

        w = Button(box, text="Ok", width=10, command=lambda :self.click("ok"), default=ACTIVE)
        w1 = Button(box, text="Cancel", width=10, command=lambda :self.click("cancel"), default=ACTIVE)
        w.pack(side=LEFT, padx=5, pady=5)
        w1.pack(side=LEFT, padx=5, pady=5)
     
        #self.bind("<Return>", self.ok)

        box.pack(side="bottom")

    def click(self, value):
        self.result = value
        self.filters = self.text.get("1.0","end")
        Dialog.ok(self)


if __name__=="__main__":
    master = Tk()
    a = FilterDialog(master,Heads=["match","regexp"])

    print a.filters
    #print ask_NoAnnfileDialog(master, Filename="foo.pl")

