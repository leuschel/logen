
#        LogViewer(Toplevel(master), 'prologstdout','Prolog Standard Output')
from Tkinter import *


class Splash(Toplevel):
    def __init__(self, master=None):
        Toplevel.__init__(self, master, relief=RAISED, borderwidth=5)
        self.maxsize(width=300, height=50)
        self.minsize(width=300, height=50)        
        self.resizable(width=YES, height=NO)
        
        self.main = master

        if self.main != None:
            self.main.withdraw()
        
        self.text = Text(self,height=20,cursor="watch") 
        self.text.pack(fill=BOTH,expand="yes")

        self.text.insert("1.0","Loading Prolog Engine...")
        self.centerOnScreen()




#        self.prev = get_func()
#        self.text.insert(END,self.prev)                        
#        self.after(1000, self.poll)
#        self.get_func=get_func

        self.wm_title("pyLogen 0.1")
        
    def centerOnScreen(self):
        
        #width, height = self.width, self.height = \
        #                self.image.width(), self.image.height()

        width = 200
        height = 200
        xmax = self.winfo_screenwidth()
        ymax = self.winfo_screenheight()

        x0 = self.x0 = (xmax - self.winfo_reqwidth()) / 2 - width/2
        y0 = self.y0 = (ymax - self.winfo_reqheight()) / 2 - height/2
        self.geometry("+%d+%d" % (x0, y0))
        
        self.update_idletasks()
        
        #self.createWidgets()
        
    def exit(self):
        self.withdraw()
        self.main.deiconify()        
        
    def poll(self):

        str =  self.get_func()
        if not str == self.prev:
            self.text.delete("1.0", END)
            self.text.insert(END, self.get_func())
            self.text.see(END)
            
        self.after(1000,self.poll)


if __name__ == "__main__":
    s  = Splash()
    s.mainloop()
    
    
