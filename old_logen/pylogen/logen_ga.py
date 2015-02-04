import logen_benchmark
import re
import os

from logen_annotation import Annotation

import Tkinter
import Prolog

import time
import ErrDialog


benchtime = re.compile("Benchmark completed in ([0-9]*)ms for ([0-9]*) runs.*")
codesize = re.compile(".* Code Size: ([0-9]*) bytes.*")

class logen_GA:
    def __init__(self,app,timeout,goal,iter,output=None,filename=None):
        #print "LOGEN GA : ",goal
        self.app = app
        
        self.timeout = timeout
        self.goal = goal
        self.iter = iter
        self.output = output
        self.filename = filename
        self.fil_mutate = False
        self.pp = False

   

    def create_mutations(self,base):
        #print "Create Mutations"
        
        #mutations = self.mutations(self.base)                                        
        #return mutations
        
        mutations = []
        for ann in base:
            
            iter = ann.mutations(mutate_filters=self.fil_mutate)            
            for a in iter:
                ##should only add if it does not exist..
                if a not in mutations:
                    mutations.append(a)
        
        
        return mutations

            
        
    def run_algorithm(self, base=None):
        if base is None:
            base = [Annotation(self.app.ann_frame.ann_to_string(),self.app.ann_frame.filter_to_string(),True,self.app)]
            
            
        mutations = self.create_mutations(base)

        

        self.output.SetProgressPercent(0)        
        self.step = int(round(100.0/len(mutations)+0.5))
        self.progress_level = 0


        number = 0
        
        

        results = {}
        for ann in mutations:                        
            (time,code,ann) = self.run_one(ann,self.goal)
            self.progress_level = self.output.SetProgressPercent(self.step+self.progress_level)
            
            results[number] = (time,code,ann)
            
            
            
            #self.output.add_row(time,code,ann)
            self.output.add_answer(time,code,ann)
            number += 1

       
            

    
            
    def benchmark(self,iter=None,goal=None,spec_file=True):
        if goal is None:
            goal = self.goal            
        if iter is None:
            iter = self.iter
            
        
        file = self.filename


        (root,ext) = os.path.splitext(file)

        if os.path.exists(root+'.ql'):
            os.remove(root+'.ql')
                
        if spec_file:
            (root,ext) = os.path.splitext(file)
            bench_file = root+".spec"
        else:
            bench_file = file

        #added slow down here to allow calc of file size without error
        # was getting errors where if iterations were too fast the file size was
        # cached???
        time.sleep(0.5)
        lines = logen_benchmark.benchmark_loop(bench_file,goal,iter)
            
        timetaken = -1
        code = -1
        for l in lines:
            print l
            match = benchtime.search(l)
            if match != None:
                timetaken = int(match.groups()[0])
                
                
            match = codesize.search(l)
            if match != None:
                code = int(match.groups()[0])                
        
        return (timetaken,code)

    
    
    def run_one(self,Ann,goal):

        ### check this is not in the cache, if it is the just use that value
        cache = self.output.check_cache(Ann)        
        if cache is not None:
            (Ann, (benchtime,code,_score)) = cache
            return (benchtime, code,Ann)

        
        
        
        self.app.ann_frame.annotate_from_str(Ann.ann,self.filename,Ann.filter,tcl_index=True)
        self.app.save_ann()
        #
        if self.bta:
            self.app.auto_bta()
        else:
            self.app.filter_prop()
        self.app.save_ann()

        ## annotations may have changed since we requested them due to filter + bta
        ## so we sould return the correct ones, not the ones we used
        Ann = Annotation(self.app.ann_frame.ann_to_string(),self.app.ann_frame.filter_to_string(),True,self.app)

        ###  Check this is not in the cache either (we may have changed as a result of BTA)
        cache = self.output.check_cache(Ann)        
        if cache is not None:
            (Ann, (benchtime,code,_score)) = cache
            return (benchtime, code,Ann)
        
        #print "specialise with timeout"
        #print "  NEED TO ADD TIMEOUT!"
        try:
            #pr  t "RUN_ONE:SPECIALISE", goal
            t1 = time.time()
            self.app.specialise(goal=goal,timeout=self.timeout,catch_exception=False,clean=True) ##could give goal here....

            ### DO NOT ADD BACK IN UNTIL YOU FIX ENTRY POINT! (it makes benchmarking fail otherwise)
            #if self.pp:
            #    self.app.run_pp()
            timetaken = (time.time() - t1) * 1000
            print "Timeout value:",timetaken
            
            
        except Prolog.PrologException,e:
            print e
            ErrDialog.complain_loudly(master=None, title="Error", msg=e)              
            return (None,None,Ann)
            
            
            
            
        


        
        (benchtime,code) = self.benchmark(goal=goal)
        
        return (benchtime,code,Ann)

        
        
    def show_output(self,str):
        
        dialog = Pmw.TextDialog(self.app, scrolledtext_labelpos = 'n',
                title = 'Trial Results',
                defaultbutton = 0,
                label_text = 'Results')
        dialog.withdraw()        
        dialog.insert('end', str)
        dialog.configure(text_state = 'disabled')
        dialog.activate()


            


        

        
        



    

    
    


