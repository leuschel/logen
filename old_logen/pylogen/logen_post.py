


from Tkinter import *
import development
import os

def get_pylogen_cmd():
    if development.get_development():
        cmdline = "sicstus -r pylogen_main.sav --goal \"runtime_entry(start),halt.\" -a"
    else:
        cmdline = os.path.join(".","pylogen_main")
        
    return cmdline



class PostProcessor:
    inline_var = None
    deadcode_var = None
    
    def __init__(self,app):
        self.app = app
        self.inline_var = Variable(self.app)
        self.deadcode_var = Variable(self.app)
        self.saver_var = Variable(self.app)

        # these are on by default..
        self.inline_var.set('1')
        self.saver_var.set('1')        
        self.deadcode_var.set('1')        


    def run_pp(self,Spec,Memo):
        #print "Runing PP on %s %s" % (Spec, Memo)
        options = ['loader']

        #print "INLINE:",self.inline_var.get()

        if self.inline_var.get() == '1':
            options.append('inline')
        if self.deadcode_var.get() == '1':
            options.append('deadcode')
        if self.saver_var.get() == '1':
            options.append('saver')

        cmd = get_pylogen_cmd()
        command = "%s -pp '%s' '%s' '%s' " % (cmd, Spec, Memo, options)

        print command
        os.system(command)
        
        
        
