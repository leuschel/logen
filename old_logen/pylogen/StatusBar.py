from Tkinter import *

import Pmw

class StatusBar(Pmw.MessageBar):
    def __init__(self, master):
        Pmw.MessageBar.__init__(self,master,entry_width=40,entry_relief='groove',
                     labelpos ='w', label_text = 'Status:')
        #Frame.__init__(self, master,height=10)
        #self.label = Label(self, bd=1, relief=SUNKEN, anchor=W)
        #self.label.pack(fill=X)
                
    def set(self, format, *args):
        #self.label.config(text=format % args)
        #self.label.update_idletasks()
        self.message('state',(format %args))
        
            
    def clear(self):
        self.message('state', "")
        #self.label.config(text="")
        #self.label.update_idletasks()
