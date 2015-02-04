from Tkinter import *
from tkFileDialog import askopenfilename, asksaveasfilename, askdirectory
from tkSimpleDialog import Dialog

import ErrDialog
from PrologFrame import PrologFrame
from AnnotationFrame import AnnotationFrame
from LogViewer import LogViewer
from LogenCalls import Logen
from Prolog import PrologException
from Exporter import Exporter, have_xml
from OutputBook import OutputBook
from TerminalFrame import TerminalFrame
from Preferences import Preferences
from AnnotationColours import ColourPreferences

from BtaTests import BtaTest

import os, bta, time, sys, re

#import psyco
#psyco.full()

import splash
from ToolBar import ToolBar
from PrefDialog import PrefDialog
from SpecBar import SpecBar
from StatusBar import StatusBar
from RecentFile import RecentFile

import development, logen_benchmark, logen_post, logen_dispatch, logen_ga_dialog
#import logen_ga

import optparse

VERSION="0.8"

import Browser
import Pmw


### Experimental debugging options....
### Only for dev

roxloaded = False
try:
    if "linux2" in sys.platform:
        import rox
        roxloaded = True
    
except:
    pass
    

        
class Application(Tk):
    Filemenu = None
    
    def createMainPane(self):
        #main panel
        self.main = PanedWindow(self,{"orient":"vertical","showhandle":"1"})
        self.main.pack(expand="yes",fill="both", side="bottom")

        
        #top panel
        self.top = Frame(self.main)

        self.ann_frame = AnnotationFrame(master=self.top,app=self)

        #toolbar
        bar = self.makeToolbar(self.top,app=self)
        bar.pack(side="top", fill="x")
        
        self.ann_frame.pack(side="bottom", fill="both", expand="yes")
        
        
        #bottom panel
        self.bottom = Frame(self)
        
        self.specBar = SpecBar(self.bottom, app=self)
        self.specBar.pack(side="top", fill="x")
        
        #statusbar
        self.status = StatusBar(self.bottom)
        self.status.pack(fill="x",side="bottom")

        self.outbook = OutputBook(master=self)
        
        self.outbook.pack(side="bottom", fill="both", expand="yes")
        
        
        self.bottom.pack(fill="x",expand="yes",side="bottom")

        #add to PanedWindow
        self.main.add(self.top, minsize="3i")
        self.main.add(self.bottom,minsize="2i")

        #set default font
        #self.changeFont(10)

    def save_ann(self):
        self.save_ann_button.configure(state="disabled")
        if self.ann_frame.filename is not None:
            #Keep track of where we are in the file!
            self.ann_frame.sourceFrame.store_pos()
            if self.ann_frame.save_ann():
                #reset scroll pos
                self.ann_frame.sourceFrame.restore_pos()
        self.save_ann_button.configure(state="normal")                
        
    def save_sourcecode(self, unused=None):
        
        #print "Should be saving souce code + parsing"
        #self.set_edit(edit_mode=False)
        self.save_pl_button.configure(state="disabled")
        self.ann_frame.sourceFrame.store_pos()
        
        if self.ann_frame.filename is None:
             filename = asksaveasfilename(title="Save As",
                filetypes=[("Prolog files","*.pl"),("All files","*")],
                initialdir=".")
             if filename == "" or filename is ():
                 return
             self.ann_frame.filename = filename
        
        self.ann_frame.save()

            #only reload source etc if parse is okay
        if  self.ann_frame.sourceFrame.parse_check():            
            self.ann_frame.load_source(self.ann_frame.filename)
            self.ann_mode()
            self.ann_frame.sourceFrame.restore_pos()

        else:
            #do not leave pl mode
            self.save_pl_button.configure(state="normal")
            return False

        return True
            

    def new_file(self,filename=None):
        if filename is None:
            filename = asksaveasfilename(filetypes=[("Prolog files","*.pl"),("All files","*")]
                                       ,initialdir=".",title="New File Name")
        if filename != "" and filename is not ():            
            fs = open(filename,'w')
            fs.write('/* Created by Pylogen */')
            fs.close()
            self.open_file(filename=filename,reset=True)
        
    def reopen_file(self,savedir=True):
        
        #Keep track of where we are in the file!
        self.ann_frame.sourceFrame.store_pos()
        
        self.open_file(filename=self.ann_frame.filename,savedir=savedir)
        
        #reset scroll pos
        self.ann_frame.sourceFrame.restore_pos()

    
    def open_file(self,filename=None,reset=False,savedir=True):
        """ Show open file dialog and load into main Source window
        """
        #
        self.check_modified()
        if filename == None:
            filename = askopenfilename(filetypes=[("Prolog files","*.pl"),
                                                  ("All files","*")],
                                       initialdir=self.pref.get_preference("examples_path"))

        starttime = time.time()
        if filename != "" and filename is not ():
            #parse check first:
            
            self.ann_frame.sourceFrame.load(filename)
            if self.ann_frame.sourceFrame.parse_check():
                
                #self.logen.parse_file(filename)                
                self.ann_frame.load_source(filename,reset=reset)
                if savedir:                
                    self.pref.set_preference("examples_path",
                                             os.path.dirname(filename))
                self.after_load()
                self.ann_mode()
            else:                                            
                self.pl_mode()
            
            self.ann_frame.filename = filename
            
            self.recent.add(filename)
            self.wm_title(self.title_msg + " - " + filename)

            #self.set_edit(edit_mode=False)

            short_filename = os.path.split(filename)[1]
            #self.Filemenu.add_command(label="Reopen %s" %short_filename, command=self.reopen_file)
            self.Filemenu.entryconfig(self.Filemenu_index["reopen"],state="normal",label="Reopen %s"%short_filename)

            self.specBar.load_goals(self.ann_frame.goals)
            
        endtime = time.time()
        print "Opening file took: " ,(endtime - starttime), " seconds"
        (root, ext) = os.path.splitext(filename)
        self.outbook.reset_output(root + ".gx")

        self.update_completions()

    def zoomIn(self):
        """ Inncrease Font Size on all panes
        """                        
        self.changeFont(self.fontsize+2)

    def zoomOut(self):
        """ Decrease Font Size on all panes
        """
        self.changeFont(self.fontsize-2)        

    def changeFont(self,size):
        self.logen.preferences["font_size"] = str(size)
        self.fontsize = max(6,size)
        if "linux2" in sys.platform:
            Font = "Fixed %d" % size
        else:
            Font = "Courier %d" % size

        self.ann_frame.set_font(Font)
        self.outbook.set_font(Font)

    def edit(self, unused=None):
        #should check ann modified
        self.pl_mode()
        #self.set_edit(edit_mode=True)

    def check_modified(self):
        pass
        #print self.ann_frame.sourceFrame.text.edit_modified()
        #    print "Modified"
        #else:
        #    print "OK"
            
            
    def show_prefs_dialog(self):
        PrefDialog(app=self)



    def startup_mode(self):
        """ We have just loaded disabled all options
        """
        
        self.edit_mode_var.set("Annotation")
        self.ann_frame.sourceFrame.set_readonly(True)
        self.ann_frame.filterFrame.set_readonly(True)
        self.specBar.set_enabled("disabled")
        self.save_pl_button.configure(state="disabled")
        self.save_ann_button.configure(state="disabled")
        self.edit_button.configure(state="disabled")
        self.export_button.configure(state="disabled")
        self.insert_filter_button.configure(state="disabled")        
        #self.Filemenu.entryconfig(self.Filemenu_index["saveann"],state="disabled")
        #self.Filemenu.entryconfig(self.Filemenu_index["savepl"],state="disabled")
        self.Filemenu.entryconfig(self.Filemenu_index["save"],state="disabled")
        self.Filemenu.entryconfig(self.Filemenu_index["export_html"],state="disabled")        
        self.Filemenu.entryconfig(self.Filemenu_index["export_latex"],state="disabled")        
        self.Filemenu.entryconfig(self.Filemenu_index["export_xml"],state="disabled")        
        
        self.Editmenu.entryconfig(self.Editmenu_index["insertfilter"], state="disabled")
        self.Editmenu.entryconfig(self.Editmenu_index["annmode"], state="disabled")
        self.Editmenu.entryconfig(self.Editmenu_index["plmode"], state="disabled")

        
    def ann_mode(self):
        self.allow_save = True
        self.edit_mode_var.set("Annotation")
        self.ann_frame.sourceFrame.set_readonly(True)
        self.ann_frame.filterFrame.set_readonly(False)
        self.specBar.set_enabled("normal")
        self.save_pl_button.configure(state="disabled")
        self.save_ann_button.configure(state="normal")
        self.edit_button.configure(state="normal")
        self.export_button.configure(state="normal")
        self.insert_filter_button.configure(state="normal")
        
        #self.reset_file.configure(state="normal")        

        #self.Filemenu.entryconfig(self.Filemenu_index["saveann"],state="normal")
        #self.Filemenu.entryconfig(self.Filemenu_index["savepl"],state="disabled")
        self.Filemenu.entryconfig(self.Filemenu_index["save"],state="normal")
        self.Filemenu.entryconfig(self.Filemenu_index["export_html"],state="normal")
        if have_xml:
            self.Filemenu.entryconfig(self.Filemenu_index["export_latex"],state="normal")
            self.Filemenu.entryconfig(self.Filemenu_index["export_xml"],state="normal")
        
        self.Editmenu.entryconfig(self.Editmenu_index["insertfilter"], state="normal")
        self.Editmenu.entryconfig(self.Editmenu_index["annmode"], state="normal")
        self.Editmenu.entryconfig(self.Editmenu_index["plmode"], state="normal")        
        
    def pl_mode(self):
        self.allow_save = True
        ## should not enter pl mode if ann is not saved:
        self.check_ann_saved()
        
        self.edit_mode_var.set("Pl")
        self.mode = "Pl"
        self.ann_frame.sourceFrame.set_readonly(False)
        self.ann_frame.filterFrame.set_readonly(True)
        self.specBar.set_enabled("disabled")
        self.save_pl_button.configure(state="normal")
        self.save_ann_button.configure(state="disabled")
        self.edit_button.configure(state="disabled")
        self.export_button.configure(state="disabled")        
        
        self.insert_filter_button.configure(state="disabled")

        self.Editmenu.entryconfig(self.Editmenu_index["insertfilter"], state="disable")
        self.Filemenu.entryconfig(self.Filemenu_index["export_html"],state="disabled")
        self.Filemenu.entryconfig(self.Filemenu_index["export_latex"],state="disabled")
        self.Filemenu.entryconfig(self.Filemenu_index["export_xml"],state="disabled")
        
        ### should also disable menu items here need to get some link to them
        #self.Filemenu.entryconfig(self.Filemenu_index["saveann"],state="disabled")
        #self.Filemenu.entryconfig(self.Filemenu_index["savepl"],state="normal")
        self.Filemenu.entryconfig(self.Filemenu_index["save"],state="normal")
        
        self.Editmenu.entryconfig(self.Editmenu_index["annmode"], state="normal")
        self.Editmenu.entryconfig(self.Editmenu_index["plmode"], state="normal")        
        
        #self.reset_file.configure(state="disabled")                
        
    def makeToolbar(self,master=None,app=None):
        #Toolbar
        icondir = "icons/"
        bar = ToolBar(master,app=app)        
        bar.add_button(text="New File", imagefile=icondir+'New24.gif', command=self.new_file)
        self.open_file_button = bar.add_button(text="Open a Source File", imagefile=icondir+'Open24.gif', command=self.open_file)
        bar.add_button(text="Reopen current File", imagefile=icondir+'Redo24.gif', command=self.reopen_file)
        self.save_ann_button = bar.add_button(text="Save the Annotations", imagefile=icondir+'SaveAnn24.gif', command=self.save_ann)
        self.save_pl_button = bar.add_button(text="Save the Source Code", imagefile=icondir+'SavePL24.gif', command=self.save_sourcecode)
        self.edit_button = bar.add_button(text="Edit Source Code", imagefile=icondir+'Edit24.gif', command=self.edit)
        #bar.add_button(text="New Annotation File", imagefile=icondir+'New24.gif', command=self.ann_frame.ann_file_manager.create_new)

        self.export_button = bar.add_button(text="Export to HTML", imagefile=icondir+'Export24.gif', command=self.exporter.export_to_html)
        
        bar.add_separator()
        bar.add_button(text="Zoom In", imagefile=icondir+'ZoomIn24.gif', command=self.zoomIn)
        bar.add_button(text="Zoom Out", imagefile=icondir+'ZoomOut24.gif', command=self.zoomOut)

        bar.add_button(text="Find", imagefile=icondir+'Find24.gif', command=self.find)
        self.find_next_button = bar.add_button(text="Find Next", imagefile=icondir+'FindAgain24.gif', command=self.find_again)

        #### FIND DIALOG BOX
        self.find_dialog = Pmw.PromptDialog(self,
            title = 'Find',
            #label_text = 'Find Text:',
            #entryfield_labelpos = 'n',
            #entry_show = '*',
            defaultbutton = 0,
            buttons = ('Find','Next', 'Cancel'),
            command = self.find_dialog_callback)
        self.find_dialog.withdraw()
        
        #bar.add_button(text="View Prolog StdOut", imagefile=icondir+'Log24.gif', command=self.logen.viewStdOut)
        #bar.add_button(text="View Prolog Error Log", imagefile=icondir+'Log24.gif', command=self.logen.viewStdErr)
        #bar.add_button(text="View Convex Log", imagefile=icondir+'Log24.gif', command=self.convex_log)        

        
        
        bar.add_separator()
        bar.add_button(text="Preferences", imagefile=icondir+'Preferences24.gif', command=self.show_prefs_dialog)
        bar.add_button(text="About", imagefile=icondir+'Information24.gif', command=self.about,side="right")

        bar.add_separator()
        
        self.prev_unknown_button = bar.add_button(text="Previous Unknown", imagefile=icondir+'StepBack24.gif', command=self.prev_error)
        self.sanity_check_button = bar.add_button(text="Check Current File", imagefile=icondir+'Check24.gif', command=self.sanity_check)
        self.next_unknown_button = bar.add_button(text="Next Unknown", imagefile=icondir+'StepForward24.gif', command=self.next_error)

        bar.add_separator()
        self.insert_filter_button = bar.add_button(text="Add New Entry Point", imagefile=icondir+'RowInsertAfter24.gif', command=self.insert_filter)

        bar.add_separator()
        self.new_terminal = bar.add_button(text="Create New Terminal", imagefile=icondir+'Host24.gif', command=self.create_new_terminal)
        bar.add_separator()
        self.hidenf = bar.add_button(text="Make selection hide_nf", imagefile=icondir+'Check24.gif', command=self.ann_frame.sourceFrame.new_hidenf)

        return bar

    findstring = None    
    def find(self):
        #print "Find"
        self.find_dialog.activate()
        
    def find_dialog_callback(self,result):
        if result is None or result is 'Cancel':
            print "No search string"
            self.find_dialog.deactivate(result)
        else:
            self.findstring = self.find_dialog.get()

            ##if find button then find and close else keep window open
            self.find_again()
            if result is 'Find':
                self.find_dialog.deactivate(result)
        
    def find_again(self,start_pos="insert"):
        if self.findstring is None:
            self.find()
            return
        
        if self.findstring == "":
            return
        
        index = self.ann_frame.sourceFrame.text.search(self.findstring,"%s + 1 chars"%start_pos)
        if index == "":
            self.status.message("help","Search string %s not found" % self.findstring)
            return 

        #we have found search string...so mark cursor and see
        self.ann_frame.sourceFrame.text.tag_remove("sel","1.0","end")
        self.ann_frame.sourceFrame.text.tag_add("sel",index, "%s + %d chars" %(index, len(self.findstring)))
        self.ann_frame.sourceFrame.text.mark_set("insert",index)
        self.ann_frame.sourceFrame.text.see(index)
        
    def create_new_terminal(self):
        self.outbook.create_new_terminal()
        
    def insert_filter(self):
        if self.ann_frame.filename is None:            
            return
        #try:
        if True:
            l = list(self.ann_frame.sourceFrame.get_all_heads())        
            filters = ErrDialog.ask_FilterDialog(master=self, Heads=l)
        
            #print filters
            
            if filters is not None:
                self.ann_frame.filterFrame.text.insert("end",filters)
        #except:
        #    print "Error in filter dialog"
            
            
            
        
        
                
    def next_error(self):
        self.ann_frame.goto_next_unknown()
    def prev_error(self):
        self.ann_frame.goto_prev_unknown()

    def sanity_check(self):
        if self.ann_frame.sanity_check():
            self.status.set("No warnings found")
        else:
            self.status.set("Please check warnings")
            self.ann_frame.goto_next_unknown("1.0")
            
            

    def undo(self):
        try:        
            self.ann_frame.edit_undo()
        except:
            pass

    def redo(self):
        try:        
            self.ann_frame.edit_redo()
        except:
            pass        

    def about(self):        
        #LogViewer(Toplevel(self), './logen_readme.txt','About Logen Version %s' % (VERSION))
        self.about.show()


    def convex_log(self):
        LogViewer(Toplevel(self), 'log_convex.txt','Convex Log')
        
    def exit(self,unused=None):
        #self.recent.saveRecent()
        self.outbook.quit()        
        self.pref.save()
        self.logen.preferences.save()
        self.logen.quit()        
        self.quit()
        sys.exit()

    def clear(self):
        file = self.ann_frame.filename
        (root,ext) = os.path.splitext(file)

        gx = root+".gx"
        spec = root+".spec"
        memo = root+".memo"
        
        if os.path.exists(gx) :os.remove(gx)
        if os.path.exists(spec) :os.remove(spec)
        if os.path.exists(memo) :os.remove(memo)
        self.outbook.reset_output(gx=gx)
        
    def check_ann_saved(self):
        if self.ann_frame.ann_modified():
            if ErrDialog.ask_question(master=self,
                            title="Annotations not saved",
                            msg="Annotations not saved. Do you want to save first?"):
                self.write_to_console("Saving annotations\n")
                self.save_ann()
            else:
                self.write_to_console("Not saving annotations")
        
    def runcogen(self):
    # Not called by anything!
        self.check_ann_saved()

        if not self.logen.runcogen(self.ann_frame.filename):
            return False        
        #self.view_gx()
        self.enable_after_cogen()
        return True
        
        #procShowErrors
        
    def enable_after_cogen(self):
        #procEnableItemsAfterRunCogen        
        self.specBar.Spec["state"] = "normal"
        

    def enable_after_spec(self):
        self.specBar.Run["state"] = "normal"

    def after_load(self):
        #self.specBar.Spec["state"] = "disabled"
        self.specBar.Cogen["state"] = "normal"
        self.specBar.Run["state"] = "disabled"


    def specialise(self,goal = None,file=None,timeout=-1,catch_exception=True,clean=False):        
        self.check_ann_saved()

        if goal ==  None:
            goal = self.specBar.specEntry.get()

        if file == None:
            file = self.ann_frame.filename

        try:
            self.write_to_console("Calling specialise for %s" % goal)
            if clean:
                (root,ext) = os.path.splitext(file)                
                spec_file = root+'.spec'
                memo_file = root+'.memo'
                byte_file = root+'.ql'
                if os.path.exists(spec_file):
                    os.remove(spec_file)
                if os.path.exists(memo_file):
                    os.remove(memo_file)
                if os.path.exists(byte_file):
                    os.remove(byte_file)                    
             
		   
	    if self.development.get_cogen_version() == "cogen3":
		#HACK! call cogen3 directly :-)
		(root,ext) = os.path.splitext(file)                
		gx_file = root + '.gx'
		ann_file = root +'.pl.ann'
		spec_file = root+'.spec'
		memo_file = root+'.memo'
		
		os.chdir('../../logen/')
		os.system('echo "" > %s' % memo_file)
		os.system('echo "" > %s' % spec_file)
		os.system('echo "" > %s' % gx_file)

		if self.development.get_watched():
		    print "WATCHED MODE"
		    os.system('sicstus --goal "assert(portray_message(informational,_)),[\'cogen.pl\'],cogen:cogen_run(\'%s\',\'%s\',[watch(all)]),halt."' % (ann_file,gx_file))
		else:
		    os.system('sicstus --goal "assert(portray_message(informational,_)),[\'cogen.pl\'],cogen:cogen_run(\'%s\',\'%s\'),halt."' % (ann_file,gx_file))
		os.system('sicstus --goal "assert(portray_message(informational,_)),[\'%s\'],gx_entry(%s,_),open(\'%s\',write,S), print_clauses(S),close(S),halt."' % (gx_file,goal,spec_file))
		print "Specialising using cogen3!"

		os.chdir('../cogen2/logen_source/')

	    else:
		self.logen.specialise(goal,file,timeout=timeout)        

            self.outbook.view_spec_output(self.ann_frame.filename)
            self.enable_after_spec()

            #steve
            self.update_completions()
            return self.logen.Spec_Time
        
        except PrologException,e:
            if catch_exception:
                ErrDialog.ErrDialog(None,"Exception",e.args[0],e.args[1])
            else:
                raise e
            
            
    def run(self):
        
        goal = self.specBar.specEntry.get()

        (root,ext) = os.path.splitext(self.ann_frame.filename)
        module = os.path.basename(root) + ".spec"        
        file = root + ".spec"

        if not os.path.exists(file):
            ErrDialog.ErrDialog(None,"Error","","No Spec file found\nPlease specialise the file first")
        
        self.logen.prolog.call("tcltk_run_spec_program(%s,'%s','%s')"%(goal, file,module))
        #print "This should use a different Prolog!"
        self.logen.prolog.call("python_run_spec_program(%s,'%s','%s',Out)"%(goal, file,module))

        self.outbook.set_output(self.logen.prolog.prologvariables["Out"])
        self.outbook.view_output()

    def create_benchmark(self):
        call = self.ann_frame.benchmark
        iter = self.ann_frame.iter
        if call == None:
            self.write_to_console("Please specify a benchmark query\n")
            return
        if iter == None:
            iter = 1000
        
        filename = self.ann_frame.filename
        if not self.logen.prolog.call("get_residual_goal_name(%s,FCall)" %(call)):
            self.write_to_console("Specialise Program First!\n")
            return
                
        fcall = self.logen.prolog.prologvariables["FCall"]
        spec = os.path.splitext(filename)[0]+".spec"
        #print "create_benchmark('%s',%s,'%s',%s,%s)"%(filename, call, spec,fcall,10)
        self.logen.prolog.call("create_benchmark('%s',%s,'%s',%s,%s)"%(filename, call, spec,fcall,iter))
        
        self.write_to_console("Creating benchmark for %s, on the call %s and %s\n" % (filename,call, fcall))
        
        

    #def run_benchmark(self):
    #    filename = self.ann_frame.filename
    #    spec = os.path.splitext(filename)[0]+".spec"
    #    benchpl = filename + ".bench"
    #    benchspec = spec + ".bench"
    #
    #    if not (os.path.exists(benchpl) and os.path.exists(benchspec)):
    #        print "Please Generate Benchmarks first"
    #
    #    os.system('sicstus -l %s --goal "run,halt."' % (benchpl))
    #    os.system('sicstus -l %s --goal "run,halt."' % (benchspec))


    
    Filemenu_index = None
    def Filemenu_update(self):
        
        
        #print "File menu construction"
        Filemenu = self.Filemenu
        Filemenu.delete(0,99)

        

        
               
        
        
        
        
        #Recent Files
        


            
        
        #

        #print "File menu construction END"
        Filemenu.add_separator()
        Filemenu.add_command(label="Exit",command=self.exit)

        


    
    def makeMenu(self):
        menu = Menu(self)
        self.config(menu=menu)

        #Filemenu
        self.Filemenu_index={}
        
        self.Filemenu = Menu(menu,tearoff=0) #,postcommand=self.Filemenu_update)

        self.Filemenu.add_command(label="New File...", command=self.new_file,underline=0)                
        self.Filemenu.add_command(label="Open File...",command=self.open_file,underline=0)
        
        if self.ann_frame.filename is not None:
            filename = os.path.split(self.ann_frame.filename)[1]
            self.Filemenu.add_command(label="Reopen %s" %filename, command=self.reopen_file,underline=0)
        else:
            self.Filemenu.add_command(label="Reopen", state="disable",command=self.reopen_file,underline=0)

            

        #self.Filemenu.add_command(label="Save Ann", command=self.save_ann,underline=S)
        self.Filemenu.add_command(label="Save", command=self.keyb_shortcut_save,underline=0)        
        self.Filemenu.add_command(label="Export to HTML...", command=self.exporter.export_to_html,underline=0)        
        self.Filemenu.add_command(label="Export to Latex...", command=self.exporter.export_to_latex,underline=0)        
        self.Filemenu.add_command(label="Export to XML...", command=self.exporter.export_to_xml,underline=0)        
        #self.Filemenu.add_command(label="Save Pl")

        self.Filemenu_index["new"]     = 0
        self.Filemenu_index["open"]    = 1
        self.Filemenu_index["reopen"]  = 2
        self.Filemenu_index["save"] = 3
        self.Filemenu_index["export_html"] = 4
        self.Filemenu_index["export_latex"] = 5
        self.Filemenu_index["export_xml"] = 6
        self.Filemenu_index["RECENT"] = 7
        
        #self.Filemenu_index["saveann"] = 3 
        #self.Filemenu_index["savepl"]  = 4

        self.Filemenu.add_separator()
        

        # Only build recent file menu if its available
        if self.recent != None:
            self.recent.menu(Filemenu)
        else:
            self.Filemenu.add_command(label="Quit",command=self.exit,underline=0)
        

        
        # STEVE REMOVED TEMP self.Filemenu_update()
        #self.Filemenu_update()
        menu.add_cascade(label="File", menu=self.Filemenu,underline=0)


       
        
        #Edit Menu
        self.Editmenu = Menu(menu, tearoff=0)
        menu.add_cascade(label="Edit", menu=self.Editmenu,underline=0)
        self.Editmenu.add_command(label="Undo",command=self.undo,underline=0)
        #Editmenu.add_command(label="Redo")
        #Editmenu.add_command(label="Cut")
        #Editmenu.add_command(label="Copy")
        #Editmenu.add_command(label="Paste")
        self.Editmenu.add_separator()
        self.Editmenu.add_radiobutton(label="Annotation Mode", state="normal",variable=self.edit_mode_var,value="Annotation",command=self.changemode,underline=0)
        self.Editmenu.add_radiobutton(label="Sourcecode Mode", state="normal",variable=self.edit_mode_var,value="Pl",command=self.changemode,underline=0)
        self.Editmenu.add_separator()
        
        self.Editmenu.add_command(label="Insert Filter",command=self.insert_filter,underline=7 )

        self.Editmenu_index ={}

        self.Editmenu_index["annmode"] = 2
        self.Editmenu_index["plmode"] = 3        
        self.Editmenu_index["insertfilter"] = 5

        #BTA Menu
        self.Btamenu = Menu(menu,tearoff=0)
        menu.add_cascade(label="BTA",menu=self.Btamenu,underline=0)
        self.Btamenu.add_command(label="Memo All", state="normal", command=self.simple_bta_file,underline=0)
        self.Btamenu.add_command(label="Unfold All", state="normal", command=self.reset_bta_file,underline=0)        
        self.Btamenu.add_command(label="Auto BTA", state="normal",command=self.auto_bta,underline=0)
        self.Btamenu.add_separator()
        
        self.Btamenu.add_command(label="Propagate Filters", state="normal", command=self.filter_prop,underline=0)
        ##BTA Debug menu
        self.BtaDebug = Menu(menu,tearoff=0)
        self.Btamenu.add_cascade(label="Debug", menu = self.BtaDebug)
        self.BtaDebug.add_command(label="Show Binary Clauses", state="normal", command=self.bin_clause)
        self.BtaDebug.add_command(label="Show Binary Convex", state="normal", command=self.show_convex)
        
        self.Btamenu.add_separator()
        self.Btamenu.add_radiobutton(label="List Norm", state="normal",variable=self.norm_var,value="list",underline=0)
        self.Btamenu.add_radiobutton(label="Term Norm", state="normal",variable=self.norm_var,value="term",underline=0)
        self.Btamenu.add_radiobutton(label="Mixed Norm", state="normal",variable=self.norm_var,value="both",underline=0)
        
        #GA Menu
        self.GAMenu = Menu(menu,tearoff=0)
        menu.add_cascade(label="GA BTA", menu=self.GAMenu, underline=0)
        self.GAMenu.add_command(label="Run trial", command=self.run_trial)
        
        
        #PostProcessor Menu
        self.PPmenu = Menu(menu,tearoff=0)
        menu.add_cascade(label="Post Processor",menu=self.PPmenu,underline=0)
        self.PPmenu.add_command(label="Run Now", state="normal", command=self.run_pp,underline=0)
        self.PPmenu.add_separator()
        
        self.PPmenu.add_checkbutton(label="Inline", state="normal",variable=self.pp.inline_var ,underline=0)
        self.PPmenu.add_checkbutton(label="Deadcode", state="normal",variable=self.pp.deadcode_var,underline=0)
        self.PPmenu.add_separator()
        self.PPmenu.add_command(label="Build Dispatcher", state="normal", command=self.build_dispatcher,underline=0)
        
        #Analyse Menu
        #Analysemenu = Menu(menu, tearoff=0)
        #REMOVED menu.add_cascade(label="Analyse", menu=Analysemenu)        
        #Analysemenu.add_command(label="Create Benchmark", state="normal", command=self.create_benchmark)
        #Analysemenu.add_command(label="Run Benchmark", state="normal", command=self.run_benchmark)

                
        
        #Prolog Menu
        Prologmenu = Menu(menu,tearoff=0)
        #REMOVED menu.add_cascade(label="Prolog",menu=Prologmenu)
        Prologmenu.add_command(label="Reset Logen", command=self.restart_prolog)
        Prologmenu.add_command(label="Rebuild Logen", command=self.rebuild_logen)

        #Options Menu
        OptionsMenu = Menu(menu,tearoff=0)
        menu.add_cascade(label="Options", menu=OptionsMenu,underline=0)
        OptionsMenu.add_command(label="Preference Dialog...", command=self.show_prefs_dialog,underline=0)

        #View Menu
        Viewmenu = Menu(menu, tearoff=0)
        menu.add_cascade(label="View", menu=Viewmenu,underline=0)

        Viewmenu.add_command(label="Clear Output",command=self.outbook.reset_output)

        Logmenu = Menu(menu,tearoff=0)                
        Logmenu.add_command(label="Prolog Standard Out",command=self.viewStdOut,underline=16)
        Logmenu.add_command(label="Prolog Standard Err",command=self.viewStdErr,underline=16)
        Logmenu.add_command(label="BTA Log",command=self.convex_log,underline=0)

        
        Viewmenu.add_cascade(label="Logs", menu=Logmenu,underline=0)
        Viewmenu.add_separator()

        Viewmenu.add_command(label="Zoom In",command=self.zoomIn,underline=5)
        Viewmenu.add_command(label="Zoom Out",command=self.zoomOut,underline=5)

        Viewmenu.add_separator()

        ## Toggle view menu checkbuttons:
        self.initialise_view_variables()

        Viewmenu.add_checkbutton(label="Show Filters", variable=self.v_show_filters, command=self.update_view)

        
        Testmenu = Menu(menu, tearoff=0)
        menu.add_cascade(label="Tests", menu=Testmenu,underline=0)

        Testbta = Menu(menu, tearoff=0)
        Testmenu.add_cascade(label="AutoBTA", menu=Testbta,underline=0)
        
        Testmenu.add_command(label="Benchmark File...", underline=0,command=self.benchmark)

        ThesisBench = Menu(menu,tearoff=0)
        #Testmenu.add_command(label="Thesis Benchmarks", underline=0,command=self.thesis_benchmarks)
        Testmenu.add_cascade(label="Thesis Benchmarks", underline=0,menu=ThesisBench)
        ThesisBench.add_command(label="All", underline=0,command=self.thesis_benchmarks)
        ThesisBench.add_separator()

        tests = logen_benchmark.thesis_benchmarks.keys()
        tests.sort()
        for test in tests:
            tfile = (test,logen_benchmark.thesis_benchmarks[test])
            ThesisBench.add_command(label=test, command= lambda t=tfile: logen_benchmark.thesis_bench_one(t,self) )    


        ProfileBta = Menu(menu,tearoff=0)
        Testbta.add_cascade(label="Profile", menu=ProfileBta)
        
        ProfileBta.add_radiobutton(label="On", state="normal",variable=self.bta_profile_var,value="True")
        ProfileBta.add_radiobutton(label="Off", state="normal",variable=self.bta_profile_var,value="False")
        
        Testbta.add_command(label="All", command=self.BtaTest.test_all,underline=0)
        Testbta.add_separator()
        
        #add all test cases
        tests = self.BtaTest.testcases.keys()
        tests.sort()
        for test in tests:
            tfile = self.BtaTest.testcases[test]
            Testbta.add_command(label=test, command= lambda t=tfile: self.BtaTest.test_file(t) )    
            
        
       

        
        
        
        

        #Help Menu
        Helpmenu = Menu(menu, tearoff=0)
        menu.add_cascade(label="Help", menu=Helpmenu,underline=0)

        Helpmenu.add_command(label="Manual",state="disable")
        Helpmenu.add_command(label="FAQ",state="disable")

        global roxloaded
        if development.get_development() and roxloaded:
            Helpmenu.add_command(label="Debug", state="normal", command=self.debug)
            
        
        Helpmenu.add_separator()
        Helpmenu.add_command(label="About", command=self.about,underline=0)


    def debug(self):
        rox.bug()
        rox.g.gdk.flush()

    def initialise_view_variables(self):
        self.v_show_filters = BooleanVar()
        self.v_show_filters.set(True)


        
    def update_view(self, event=None):
        print "Update the view"

        ## Filter toggle
        if self.v_show_filters.get():
            self.ann_frame.show_filter()
        else:
            self.ann_frame.hide_filter()
            
           
        
    def thesis_benchmarks(self):
        self.write_to_console("Running thesis benchmarks\n")
        logen_benchmark.run_thesis_benchmarks(app)
        
    def run_trial(self):
        
        
        
        filename = self.ann_frame.filename
        if file is None:
            self.status.set("Please open a file first")
            return

        timeout = 100
        goal = self.ann_frame.benchmark
        iter = self.ann_frame.iter
        
        self.ga_diag = logen_ga_dialog.LogenGADialog(self,timeout=timeout, goal=goal,iterations=iter,filename=filename)
        
        self.ga_diag.showAppModal()
        
        #GA = logen_ga.logen_GA(self)
        #GA.run_alogrithm(filename)
        
    def build_dispatcher(self):
        
        file = self.ann_frame.filename
        if file is None:
            self.status.set("Please open a file first")
            return

        path = os.path.dirname(file) +"/"
        (root,ext) = os.path.splitext(file)
        
        spec_file = os.path.basename(root+".spec")
        memo_file = os.path.basename(root+".memo")
        file = os.path.basename(file)
        output = os.path.basename(root + ".dispatch")
        

        
        logen_dispatch.build_dispatcher(path,spec_file,memo_file,file,output)
        self.set_output_from_file(path + output)
        self.outbook.selectpage("Output")
        
        
    prevmode = None
    def changemode(self):
        if self.edit_mode_var.get() != self.prevmode:
            self.prevmode = self.edit_mode_var.get()
            if self.edit_mode_var.get() == "Pl":
                self.edit()
            else:
                self.save_sourcecode()
        

    def run_pp(self):
        file = self.ann_frame.filename
        if file is None:
            self.status.set("Please open a file first")
            return
        
        (root,ext) = os.path.splitext(file)
        spec_file = root+".spec"
        memo_file = root+".memo"
        self.pp.run_pp(spec_file,memo_file)

        #reopen new files:
        self.outbook.view_spec_output(self.ann_frame.filename)
        #self.view_spec()
        #self.view_memo()
        
        
        
        
        
    def benchmark(self, file=None):
        if file is None:
            file = self.ann_frame.filename
            
        if file is None:
            self.status.set("Please open a file first")
            return

        
        bench_goal = self.ann_frame.benchmark
        iter = self.ann_frame.iter

        (root,ext) = os.path.splitext(file)
        spec_file = root + ".spec"
        
        if bench_goal is None:
            bench_goal = self.specBar.specEntry.get()
        
        (goal,iter) = logen_benchmark.run_benchmark(file,spec_file,parent=self, goal=bench_goal,iterations=iter,app=self)

        self.ann_frame.benchmark = goal
        self.ann_frame.iter = iter
        

        
    
    def viewStdOut(self):
        self.logen.viewStdOut()
    def viewStdErr(self):
        self.logen.viewStdErr()
        
    def conf(self, event):
        """ Could dynamically move slider division?
        """
        pass

        
    def simple_bta_file(self,unused=None):
        if self.ann_frame.filename is None:
            self.status.set("Please open a file first")
            return
            
        self.logen.annotate_file(self.ann_frame.filename, "safe")
        self.reopen_file()
        #self.ann_frame.reset_filters()

    def reset_bta_file(self, unused=None,filename=None):        
        #todo: this should now make .filters file and use auto_bta to reset
        #bta.reset_file(self.ann_frame.filename, self.ann_frame.entry)        
        if filename is None:
            filename = self.ann_frame.filename

        if self.ann_frame.filename is None:
            self.status.set("Please open a file first")
            return
            
        self.logen.annotate_file(filename, "auto_bta")        
        self.reopen_file()
        self.ann_frame.reset_filters()
        

        

    def global_bta(self,unused=None,norm=None):
        if self.ann_frame.filename is None:
            self.status.set("Please open a file first")
            return
        
        startTime = time.time()
        filename = self.ann_frame.filename
        annfile = filename+ ".ann"
        output = bta.run_global_bta(annfile)

        
        

        txt = output[-1] + "\n\nOutput Log:\n"
        for line in output:
            txt = txt + line
        self.outbook.set_output(txt)
            
        
        
        


    def auto_bta(self,unused=None,norm=None,print_output=True):
        """ This should be the full looping version..
        """
        if self.ann_frame.filename is None:
            self.status.set("Please open a file first")
            return
        
        if norm==None:
            norm = self.norm_var.get()
            
            
        
        startTime = time.time()
        filename = self.ann_frame.filename
        annfile = filename+ ".ann"
        tmpfile = "_temp_bta.ann"
        #logenbta = './filter_prop/logen/logenbta.pl'        
        #os.system("sh ./auto_bta %s %s _temp_bta.ann %s" % (filename,annfile,norm))

        
        if self.bta_profile_var.get() == "True":
            profile = True
        else:
            profile = False

        
        

        (outstream,time1,iter1) = bta.run_bta(filename,tmpfile, annfile=annfile,norm=norm,profile=profile)
        #print "Temp output file is: ", tmpfile
        if print_output:
            for line in outstream:            
                self.write_to_console("%s\n"% line)

        

        

        
        
        self.ann_frame.load_source(self.ann_frame.filename, tmpfile)
        endTime = time.time()

        self.ann_frame.clear_modified(val=True)
        self.write_to_console("Finished auto_bta in %s\n" % ((endTime-startTime)))



        return (outstream,time1,iter1)
        
        
    def safety_check(self, unused=None,norm=None):
        if norm == None:
            norm = self.norm_var.get()
        startTime = time.time()
        annfile = self.ann_frame.filename + ".ann"        
        os.system("sh ./auto_bta_once %s out.ann %s" % (annfile, norm))        
        self.ann_frame.load_saftey(self.ann_frame.filename, "out.ann")

        endTime = time.time()
        self.write_to_console("Finished (safety) auto_bta in %s\n" % ((endTime-startTime)))
        

    def bin_clause(self,unused=None):
        filename = self.ann_frame.filename
        annfile = filename +  ".ann"
        output = filename + ".bin"        
        bta.run_binsolve(filename, annfile,output)        
        LogViewer(Toplevel(self), output,'Binary clause for %s ' % filename, update=False)

    def show_convex(self,unused=None, norm=None):
        filename = self.ann_frame.filename
        annfile = filename +  ".ann"
        output = filename + ".bin"
        if norm==None:
            norm = self.norm_var.get()
        bta.run_convex(filename, annfile,output, norm)        
        LogViewer(Toplevel(self), output,'Convex Loop Analysis for %s ' % filename,update=False)

    
        

        
        
        
    def filter_prop(self,unused=None):
        
        filename = self.ann_frame.filename
        annfile = self.ann_frame.filename + ".ann"

        bta.filter_prop(filename, annfile, 'filter_prop.ann')
        self.ann_frame.load_source(self.ann_frame.filename, 'filter_prop.ann')

        

        
             
           
        
        
    def restart_prolog(self,unused=None):
        self.logen.restart()

    def rebuild_logen(self,unused=None):
        self.logen.rebuild()


    def show_loading(self):
        pass

    def bindkey(self,key,func):
        self.bind_all("<Control-%s>"%key, func)
        ##add mac keys if needed
        if "darwin" in sys.platform:            
            self.bind_all("<Command-%s>"%key, func)
       

        
        
        
    def define_keyboard_shortcuts(self):
        ### NOTE ALL KEYS MUST ALSO BE DEFINED IN
        ### ScrollText to passkeys else text frame
        ### traps the events see ScrollText.py
        #### pass_keyb_shortcuts(<list of chars>)
        self.bindkey("s",self.keyb_shortcut_save)
        self.bindkey("e",self.keyb_shortcut_edit)
        self.bindkey("o",self.keyb_shortcut_open)
        self.bindkey("q",self.keyb_shortcut_quit)
        self.bindkey("n",self.keyb_shortcut_new)

        


    def keyb_shortcut_open(self,event):        
        self.open_file()

    def keyb_shortcut_new(self,event):        
        self.new_file()

    def keyb_shortcut_quit(self,event):
        print "Quit shortcut not implemented"
        

    allow_save = None  
    def keyb_shortcut_save(self,event=None):
        
        if not self.allow_save:            
            return
        self.allow_save = False
        if self.edit_mode_var.get() == "Annotation":
            self.save_ann()
        else:
            self.save_sourcecode()
        self.allow_save = True   
        #self.set_enable_save(True)
    
            

    def keyb_shortcut_edit(self,unused):
        self.write_to_console("edit mode\n")
        if self.edit_mode_var.get() == "Annotation":
            self.edit()
        

    def printfoo(self,event):
        print "..."


    def write_to_console(self, string):
        self.outbook.write_to_console(string)

    def update_completions(self):
        completions = []
        self.outbook.reset_completions()
        for c in self.ann_frame.sourceFrame.get_all_heads():
            #c = c[:c.rfind('/')]
            completions.append(c)

        filename = self.ann_frame.filename
        if filename is not None:
            (root, ext) = os.path.splitext(filename)
            base = os.path.split(root)[1]
            module = base + ".spec"
            
            for c in self.outbook.output_spec.get_all_heads():
                #c = c[:c.rfind('/')]
                completions.append("'%s':%s"%(module,c))
                
            #print "adding completions", completions
            self.outbook.add_completions(completions)
        
    recent =None    
    def __init__(self, master=None):
        #Frame.__init__(self, master)
        Tk.__init__(self, master)

        self.balloon = Pmw.Balloon(self)

        self.development = development

        self.logen = Logen()
        self.exporter = Exporter(master=self)
        self.pref = Preferences('preferences.xml')
        self.colours = ColourPreferences(pref=self.pref)

        #####AHHHHHH tk defines lots of stupid key bindings
        #####ctrl-o inserts newline..... remove them
        self.unbind_class("Text", "<Control-o")
        self.unbind_class("Text", "<Alt-Up>")
        self.unbind_class("Text", "<Alt-Down>")


        self.BtaTest = BtaTest(self)

        # PostProcessor Module
        self.pp = logen_post.PostProcessor(self)
        
        # Set up tk linked variables (for menu radios)                
        self.bta_profile_var = Variable(self)
        self.bta_profile_var.set("False")
        self.norm_var = Variable(self)
        self.norm_var.set("list")

        self.prevmode = "Annotation"
        self.edit_mode_var = Variable(self)
        self.edit_mode_var.set("Annotation")

        
        # Gui only bits not dependent on Prolog    
        self.protocol("WM_DELETE_WINDOW", self.exit)
        
        if development.get_development():
            self.title_msg = "PyLogen Version %s DEVELOPMENT" %(VERSION)
        else:
            self.title_msg = "PyLogen Version %s Distribution" %(VERSION)
        
        self.wm_title(self.title_msg)
        self.createMainPane()
        self.makeMenu()
        

        # display something for the user

        # THIS CAN TAKE A LONG TIME BECAUSE OF REBUILDING...             
        ## Initialise the prolog engine and rebuild if needed...
        ## so we show a splash screen...
        self.splash = splash.Splash(self)
        self.update()
        
        self.logen.start(splash=self.splash)
        self.splash.exit()  
        
                                
        ### things that are dependent on preferences                
        self.recent = RecentFile(self)

        self.startup_mode()

        self.balloon["statuscommand"] = self.status.helpmessage
        
        self.browser = Browser.Browser(self.pref)
        Pmw.aboutversion(VERSION)
        Pmw.aboutcopyright('Copyright 1996-2004\n Logen: Michael Leuschel, Jesper Joergensen, Stephen-John Craig.\nPyLogen Interface: Stephen-John Craig, Dan Elphick')
        Pmw.aboutcontact('Michael Leuschel (http://www.ecs.soton.ac.uk/~mal) \n' +
                         'Declarative Systems and Software Engineering\n' +
                         'Department of Electronics and Computer Science\n'+
                         'University of Southampton\n'+
                         'Highfield, Southampton, SO17 1BJ,  U.K.\n'+
                         'Fax: +44 01703 59 3045   Tel: +44 01703 59 3377\n'+
                         'E-mail: mal@ecs.soton.ac.uk\n')

        self.about = Pmw.AboutDialog(self,applicationname='PyLogen')
        self.about.withdraw()
        

        ### load font size
        fontsize = self.logen.preferences["font_size"]        
        try:
            fontsize = int(fontsize)
        except:            
            fontsize = 12
            
        self.changeFont(fontsize)

        self.define_keyboard_shortcuts()

        self.write_to_console("****** Pylogen Started ******\n")
        self.write_to_console("Version %s\n"%VERSION)
        if development.get_development():
            self.write_to_console("Development Mode")
        else:
            self.write_to_console("Distribution Mode")
            



     #def destroy(self):
         #self.main.update()
         #self.main.deiconify()
         #self.withdraw()


def getopts():
    parser = optparse.OptionParser()
    parser.add_option("-d", "--dev",
                  action="store_true", dest="dev", default=False,
                  help="development mode")

    parser.add_option("-c", "--c3",
                  action="store_true", dest="cogen3", default=False,
                  help="use cogen 3 mode")

    parser.add_option("-w", "--watched",
                  action="store_true", dest="watched", default=False,
                  help="use cogen 3 WATCHED mode")

    parser.add_option("-t", "--self_test",
                  action="store_true", dest="tests", default=False,
                  help="Run self tests")
    

    (options,args) = parser.parse_args()

    #we dont expect any arguments
    if args != []:
        parser.print_help()
        sys.exit()

        
    return (options,args)
    


def resize_window(app):
    width = app.winfo_width()
    height = app.winfo_height()

    if int(width) > int(app.winfo_screenwidth()):
        width = app.winfo_screenwidth() - 50
    if int(height) > int(app.winfo_screenheight()):
        height = app.winfo_screenheight() -50
    
    app.geometry("%dx%d+20+20"%(width,height))
    
if __name__  == "__main__":
    os.chdir(sys.path[0])

    
    (options,args) = getopts()


    if options.dev:
        print "Development Mode::::"
        development.set_development_mode(True)
    
    if options.cogen3:
	print "Cogen3 mode::::"
	development.set_cogen_version('cogen3')
    
    if options.watched:
	print "WATCHED mode::::"
	development.set_watched(True)

    

    os.chdir('..')
    app = Application()
    Pmw.initialise(app, 12, fontScheme = 'default')

    if options.tests:
        app.BtaTest.test_all()


    resize_window(app)
    
    
    
    #app.master
    app.mainloop()
    del app
    sys.exit()

