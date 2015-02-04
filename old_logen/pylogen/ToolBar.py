from Tkinter import *

class ToolBar(Frame):
    """
    This class managed the main toolbar
    """
    
    def __init__(self,master=None,app=None):
        Frame.__init__(self, master)
        self["height"] = "-1cm"
        self.images = {}
        self.app = app
    
        
    def add_button(self, text="", command=None, imagefile=None,padx=0,side="left"):
        self.images[text] = PhotoImage(file=imagefile, master=self)
        b = Button(self,text=text,image=self.images[text],  command=command, state="normal")
        b.bind(sequence="<Enter>", func=self.over)                
        b.pack(side=side, padx=padx,pady=2)
        self.app.balloon.bind(b, text)
        return b

    def add_separator(self, padx=4,side="left"):
        Label(self).pack(padx=padx,side=side)

    def over(self,sender):
        self.app.status.set("%s" % (sender.widget.cget("text")))


