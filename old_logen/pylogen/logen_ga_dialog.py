
import Tkinter
from logen_annotation import Annotation
import time
import Pmw
#from logen_ga import logen_GA
from logen_ga_controller import GAControl

class LogenGADialog:
    
    def __init__(self, app,timeout,goal=None,iterations=None,filename=None,alpha=1,beta=1,gamma=1):
        # Create the dialog.
        
        if iterations is None:
            iterations = "50000"
        if goal is None:
            goal = ""
        if timeout is None:
            timeout = 1000
        
        self.filename = filename
        self.app = app                
        self.goal = goal
        self.iterations = iterations
        self.timeout = timeout
        


        
        ## Set up the GA controller
        self.ga_control = GAControl(app,self.timeout,self.goal,self.iterations,self.filename,gui=self)
        
        self.ga_control.alpha = alpha
        self.ga_control.beta = beta
        self.ga_control.gamma = gamma
        
        self.ga_control.set_initial_state(Annotation(
            self.app.ann_frame.ann_to_string(),
            self.app.ann_frame.filter_to_string(),
            True,
            self.app))
        
        self.id = 0
        
        self.dialog = Pmw.Dialog(app,
            buttons = ('Run Trial','Refresh Scores','Clear Cache','Show Cache','Use Current Goal','Reset','Cancel'),
            defaultbutton = 'Run Trial',
            title = "Logen Annotation Trial",
            command = self.execute)
        
        self.dialog.withdraw()

        self.to = Pmw.Counter(self.dialog.interior(),
                        labelpos = 'w',
                        label_text = 'Timeout:',
                        entryfield_value = str(self.timeout),
                        entryfield_validate = {'validator':'integer','min':1},
                        increment = 100
                        )
        self.to.pack(expand = 0, fill = 'x', padx = 4, pady = 4)

        
        self.it = Pmw.Counter(self.dialog.interior(),
                        labelpos = 'w',
                        label_text = 'Iterations:',
                        entryfield_value = str(self.iterations),
                        entryfield_validate = {'validator':'integer','min':1},
                        increment = 100
                        )
        self.it.pack(expand = 0, fill = 'x', padx = 4, pady = 4)
    
        
        self.goal_entry = Pmw.EntryField(self.dialog.interior(),
                labelpos = 'w',
                label_text = 'Goal:',
                value = self.goal,
                validate = None
                )
        
        self.goal_entry.pack(expand = 0, fill = 'x', padx = 4, pady = 4)

        self.start_state = Pmw.EntryField(self.dialog.interior(),
                labelpos = 'w',
                label_text = 'Initial Pattern:',
                value = self.ga_control.initial_state,                                       
                validate = None
                )
        self.start_state.component('entry').config(state="disable")
        
        
        self.start_state.pack(expand = 0, fill = 'x', padx = 4, pady = 4)

        self.current = Pmw.ScrolledListBox(self.dialog.interior(),
                items=(),
                labelpos='nw',
                label_text='Current Annotations',
                listbox_height = 5,
                selectioncommand=self.selectionCommand,
                dblclickcommand=self.defCmd,
                usehullsize = 1,
                hull_width = 200,
                hull_height = 100,
        )

        self.current.pack(expand = 0, fill = 'x' , padx = 4, pady= 4)

        self.weights_frame = Tkinter.Frame(self.dialog.interior())
        
        self.alpha_cnt = Pmw.Counter(self.weights_frame,
                        labelpos = 'w',
                        label_text = 'Time:',
                        entryfield_value = str(self.ga_control.alpha),
                        entryfield_validate = {'validator':'integer','min':0},
                        increment = 1,
                        
                                     
                        )
        
        self.alpha_cnt.pack(side='left',expand = 0, padx = 4, pady = 4)

        self.beta_cnt = Pmw.Counter(self.weights_frame,
                        labelpos = 'w',
                        label_text = 'Size:',
                        entryfield_value = str(self.ga_control.beta),
                        entryfield_validate = {'validator':'integer','min':0},
                        increment = 1
                        )
        
        self.beta_cnt.pack(side='left',expand = 0, padx = 4, pady = 4)

        self.gamma_cnt = Pmw.Counter(self.weights_frame,
                        labelpos = 'w',
                        label_text = 'Spec:',                        
                        entryfield_value = str(self.ga_control.gamma),
                        entryfield_validate = {'validator':'integer','min':0},
                        increment = 1
                        )
        
        self.gamma_cnt.pack(side='left',expand = 0, padx = 4, pady = 4)

        self.nbest_cnt = Pmw.Counter(self.weights_frame,
                        labelpos = 'w',
                        label_text = 'Select:',                        
                        entryfield_value = str(self.ga_control.gamma),
                        entryfield_validate = {'validator':'integer','min':1},
                        increment = 1
                        )
        
        self.nbest_cnt.pack(side='left',expand = 0, padx = 4, pady = 4)
        
        
        self.weights_frame.pack(expand = 0, fill = 'x')
        

        self.option_frame = Tkinter.Frame(self.dialog.interior())

        self.btavar = Tkinter.Variable()
        self.btavar.set("1")
        self.btachk = Tkinter.Checkbutton(self.option_frame,text='Run Bta',variable=self.btavar)
        self.btachk.pack(side='left',expand = 0,fill ='x')
        
        self.filtervar = Tkinter.Variable()
        self.filtervar.set("1")
        self.filterchk = Tkinter.Checkbutton(self.option_frame,text='Mutate Filters',variable=self.filtervar)
        self.filterchk.pack(side='left',expand = 0,fill ='x')

        self.loopvar = Tkinter.Variable()
        self.loopvar.set("1")
        self.loopchk = Tkinter.Checkbutton(self.option_frame,text='Loop',variable=self.loopvar)
        self.loopchk.pack(side='left',expand = 0,fill ='x')

        self.cachevar = Tkinter.Variable()
        self.cachevar.set("1")
        self.cachechk = Tkinter.Checkbutton(self.option_frame,text='Cache',variable=self.cachevar)
        self.cachechk.pack(side='left',expand = 0,fill ='x')

        self.ppvar = Tkinter.Variable()
        self.ppvar.set("1")
        self.ppchk = Tkinter.Checkbutton(self.option_frame,text='Post Processor',variable=self.ppvar)
        self.ppchk.pack(side='left',expand = 0,fill ='x')        
        self.option_frame.pack(expand = 0,fill ='x')

        #self.progress_frame = Tkinter.Frame(self.dialog.interior())
        #self.progress_frame.pack(expand = 0, fill='x')
        
        
        self.progress = ProgressBar(self.option_frame)

        self.progress.SetProgressPercent(0)

        #self.progress.pack(expand =0, fill = 'x')
        
        # Create the ScrolledText with headers.
        fixedFont = Pmw.logicalfont('Fixed')
        self.output = Pmw.ScrolledText(self.dialog.interior(),
                # borderframe = 1,
                labelpos = 'n',
                label_text='Times',
                columnheader = 1,
                rowheader = 1,
                rowcolumnheader = 1,
                usehullsize = 1,
                hull_width = 400,
                hull_height = 200,
                text_wrap='none',
                text_font = fixedFont,
                Header_font = fixedFont,
                Header_foreground = 'blue',
                rowheader_width = 3,
                rowcolumnheader_width = 4,
                text_padx = 4,
                text_pady = 4,
                Header_padx = 4,
                rowheader_pady = 4,
        )

        self.output.component('rowcolumnheader').insert('end', 'Rank')

        

        # Create the column headers
        
        funcs = ['Time', 'Size', 'Score', 'Annotation Pattern']

        headerLine = ''
        for column in range(len(funcs)):
            headerLine = headerLine + ('%-7s   ' % (funcs[column],))
        headerLine = headerLine[:-3]


        self.output.component('columnheader').insert('0.0', headerLine)

        

        
        self.output.pack(expand=1,fill='both',padx = 4, pady=4)

        self.rowheader = self.output.component('rowheader')
        self.output.component('rowheader').tag_configure('row_id')
        self.output.component('rowheader').tag_bind('row_id',"<Button-1>", self.row_id_clicked)



        self.output_orig = Pmw.ScrolledText(self.dialog.interior(),
                # borderframe = 1,
                #labelpos = 'n',
                #label_text='Times',
                columnheader = 0,
                rowheader = 1,
                rowcolumnheader = 0,
                usehullsize = 1,
                hull_width = 400,
                hull_height = 30,
                text_height=1,                          
                text_wrap='none',
                text_font = fixedFont,
                Header_font = fixedFont,
                Header_foreground = 'blue',
                rowheader_width = 4,
                #rowcolumnheader_width = 3,
                text_padx = 4,
                text_pady = 4,
                Header_padx = 4,
                rowheader_pady = 4,
        )

        #self.output_orig.component('rowcolumnheader').insert('end', 'Rank')
        self.output_orig.pack(expand=0,fill='x',padx = 4, pady=4)


        

        self.reset()
        

    
        
        self.update()
        
    id = None

    
    def selectionCommand(self):
        print "Selection"


    


        

    def defCmd(self):        
        sels = self.current.getcurselection()[0]
        if len(sels) == 0:
            print 'No selection for double click'
        else:
            #self.set_annotation(self.current_store[sels])
            self.run_algorithm([self.ga_control.current_store[sels]])
            

    def add_current(self,Annotation):
        if self.ga_control.add_current(Annotation):
            self.current.insert('end', str(Annotation))
            
                    
    def refresh(self):
        self.set_ga_state()
        self.ga_control.update_scores()
        self.update()
    
        
        
    def show_all_cache(self):
        self.ga_control.answer_ann = []
        for key in self.ga_control.cache:
            (Ann,(Time,Size, _)) = self.ga_control.cache[key]
            self.ga_control.add_answer(Time, Size, Ann)
        self.update()


    def answer_clicked(self,event=None):
        index = self.output.index("@" + str(event.x) + "," + str(event.y))
        line = index[:index.find('.')]
        print line
        
        
        
        
    def update(self):

        ### fil the current annoation box
        self.current.clear()
        for key in self.ga_control.current_store:
            self.current.insert('end', key)
            
            

        ### set the progress bar
        self.progress.SetProgressPercent(self.ga_control.percent)

        ### Fill the answer box:
        self.output.component('rowheader').delete('1.0','end')        
        self.output.delete('1.0', 'end')

        self.id = 0
        self.output.tag_configure("Annotation", background="yellow")
        self.output.tag_bind("Annotation", "<Button-1>", self.answer_clicked)
        for answer in self.ga_control.answer_ann:
            
            (Ann,(Time,Size,Score)) = answer
            self.output.insert('end', '%-7s   %-7s   %-7f   %s\n' % (str(Time),str(Size),Score,str(Ann)))
                        
            self.output.tag_add("Annotation",  "end-%dc-3c" % (len(str(Ann))),"end-2c")
            
            
            header = str(self.id) + "  \n"
            self.id += 1
            index =  self.output.component('rowheader').index('end -1 line')                        
            self.output.component('rowheader').insert('end', header)        
            self.output.component('rowheader').tag_add('row_id',index, '%s lineend'% index)
        
        self.output_orig.delete('1.0', 'end')


        orig_time = self.ga_control.normalise_time
        orig_code = self.ga_control.normalise_code      
        orig_score = self.ga_control.normalise_score
        
        self.output_orig.insert('end', '%-7s   %-7s   %-7f   %s\n' % (str(orig_time),str(orig_code),orig_score,"Original Program")) 
        
            
                
    def row_id_clicked(self, Event):
        index = self.rowheader.index("@" + str(Event.x) + "," + str(Event.y)+" linestart")
        print index
        (start,end) = self.rowheader.tag_nextrange('row_id', index)
        id = int(self.rowheader.get(start,end).strip())
        self.add_current(self.ga_control.answer_ann[id][0])
        
        
    
    def clear(self):
        self.output.delete('1.0', 'end')
        self.output.component('rowheader').delete('1.0','end')
        self.id = 0

        
    def showAppModal(self):
        self.dialog.activate(geometry = 'centerscreenalways')

    def set_annotation(self, Ann):
        self.app.ann_frame.annotate_from_str(Ann.ann,self.filename,Ann.filter,tcl_index=True)
        
        
    def reset(self):        
        self.set_annotation(self.ga_control.initial_state)
        self.app.save_ann()
        self.clear()
        
        self.ga_control.reset()
        self.update()

        
    def execute(self,result):
        if result is None:
            result = 'Cancel'
        if result in ('Use Current Goal'):
            self.goal_entry.setvalue(self.app.specBar.specEntry.get())
            return
        
        if result in ('Reset'):      
            self.reset()
            return
        if result in ('Refresh Scores'):            
            self.refresh()
            return

        if result in ('Clear Cache'):            
            self.ga_control.clear_cache()
            return

        if result in ('Show Cache'):            
            self.show_all_cache()
            return
        
        if result not in ('Run Trial'):
            self.dialog.deactivate(result)
            return
        

        
        self.run_algorithm()
        #self.run_loop_algorithm()

        self.set_annotation(self.ga_control.initial_state)
        
        
    def run_algorithm(self, base=None):
        self.clear()        
        self.set_ga_state()
        if self.goal is "":
            print "Please specify a goal"
            return
        #self.ga_control.run_algorithm(base)
        self.ga_control.run_loop_algorithm(base)

        
        
    def set_ga_state(self):
        self.timeout = int(self.to.get())
        self.goal = self.goal_entry.get()
        self.iterations= int(self.it.get())
        

        self.ga_control.alpha = int(self.alpha_cnt.get())
        self.ga_control.beta = int(self.beta_cnt.get())
        self.ga_control.gamma = int(self.gamma_cnt.get())

        self.ga_select = int(self.nbest_cnt.get())
        self.ga_control.loop = self.loopvar.get()=='1'
        #self.ga_control.loop = self.loopvar.get()=='1'
        self.ga_control.use_cache = self.cachevar.get()=='1'
        
        self.ga_control.set_ga_state(self.timeout,self.goal,self.iterations,self.btavar.get()=='1',self.filtervar.get()=='1',self.ppvar.get() =='1')





class ProgressBar:
        def __init__(self, Parent, height=20, width=200, Foregroundcolor=None, Backgroundcolor=None,Progress=0):
                self.height=height
                self.width=width
                self.BarCanvas = Tkinter.Canvas(Parent, width=width,height=height,background=Backgroundcolor,borderwidth=1,relief=Tkinter.SUNKEN)
                if (Backgroundcolor):
                        self.BarCanvas["Backgroundcolor"]=BackgroundColor
                self.BarCanvas.pack(padx=5,pady=2)
                self.RectangleID=self.BarCanvas.create_rectangle(0,0,0,height)
                if (Foregroundcolor==None):
                        ForegroundColor ="gray"
                self.BarCanvas.itemconfigure(self.RectangleID,fill='red')
                self.SetProgressPercent(Progress)
        def SetProgressPercent(self,NewLevel):
                self.Progress=NewLevel
                #self.Progress=min(118,self.Progress) for static packages of 1.0
                self.Progress=min(100,self.Progress)
                self.Progress=max(0,self.Progress)
                self.DrawProgress()
                return self.Progress
            
                
        def DrawProgress(self):
                ProgressPixel=(self.Progress/100.0)*self.width
                self.BarCanvas.coords(self.RectangleID,0,0,ProgressPixel,self.height)
                
        def GetProgressPercent(self):
                return self.Progress
