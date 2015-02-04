#!/usr/bin/env python

import sys,os
#import time
from Tkinter import *
from ScrolledText import ScrolledText

class StringViewer(Frame):
    def __init__(self, parent, get_func, title=None):
        Frame.__init__(self,parent)
        self.text = ScrolledText(parent)
        self.text.pack(fill=BOTH,expand="yes")
        self.prev = get_func()
        self.text.insert(END,self.prev)                        
        self.after(1000, self.poll)
        self.get_func=get_func
        parent.wm_title(title)
        
    def poll(self):

        str =  self.get_func()
        if not str == self.prev:
            self.text.delete("1.0", END)
            self.text.insert(END, self.get_func())
            self.text.see(END)
            
        self.after(1000,self.poll)

class LogViewer(Frame):
    def __init__(self, parent, filename, title=None, update=True):
        Frame.__init__(self,parent)
        self.update = update
        self.filename = filename 
        self.file = open(filename, 'r')
        self.text = ScrolledText(parent)
        self.text.pack(fill=BOTH,expand="yes")
        data = self.file.read()
        self.size = len(data)
        self.text.insert(END, data)

        if self.update:
            self.after(100, self.poll)            
        parent.wm_title(title)

    def poll(self):
        if os.path.getsize(self.filename) > self.size:
            data = self.file.read()
            self.size = self.size + len(data)
            self.text.insert(END, data)
        self.after(100,self.poll)

if __name__ == "__main__":
    root = Tk()
    viewer = LogViewer(root, sys.argv[1])
    viewer.mainloop()

