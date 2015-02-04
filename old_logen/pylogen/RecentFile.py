from Tkinter import *
import os

class RecentFile:
    recent = None
    def __init__(self,app):
        self.recent = []
        self.app = app
        
        #self.loadRecent()
        self.update_menu(self.app.Filemenu)
    
        
    def add(self,filename):
        self.app.logen.preferences.add_recent(filename)        
        self.update_menu(self.app.Filemenu)
        
    def clear(self):
        self.app.logen.preferences.clear_recent()
        self.update_menu(self.app.Filemenu)
        
    def update_menu(self,menu):
        """ This should modify the menu at from position 6 onwards with recent menu
        """        
        self.recent = self.app.logen.preferences.get_recent()        

        
        
        
        start = self.app.Filemenu_index["RECENT"]
        menu.delete(start,12)
        
        RecentMenu = Menu(menu,tearoff=0)
        
        for i in self.recent:        
            #menu.add_command(label="%s"%os.path.basename(i), command=lambda i=i: self.openRecent(i))
            RecentMenu.add_command(label="%s"%i, command=lambda i=i: self.openRecent(i))

        RecentMenu.add_separator()
        RecentMenu.add_command(label="Clear Recent Files", command=self.clear)
        
        menu.add_cascade(label="Recent Files", menu=RecentMenu,underline=2)
        menu.add_separator()
        menu.add_command(label="Quit", command= self.app.exit,underline=0)
        
        
        
            

    def openRecent(self,filename):
        self.app.open_file(filename=filename)
        
