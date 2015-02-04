from logen_ga import logen_GA

import ErrDialog

class GAControl:
    def __init__(self, app,timeout,goal,iterations,filename,gui):
        self.ga  = logen_GA(app , timeout, goal,iterations,self,filename)
        self.current_store = {}
        self.initial_state = None
        self.answer_ann = []
        self.gui = gui

        self.alpha = 1
        self.beta = 1
        self.gamma = 1

        self.normalise_time = 1
        self.normalise_code = 1
        self.normalise_score =1.0 

        self.loop = False
        self.percent = 0
        
        self.cache = {}
        self.use_cache = True

        self.select = 3

        self.open_log()
        self.write_log('Log started\n')
        self.write_log('Filename: %s\n' % filename)


    def open_log(self):
        self.log = open('ga_log.txt','w')

    def close_log(self):
        if not self.log.closed:
            self.log.close()

    def write_log(self, str):
        self.log.write(str)
        self.log.flush()

        
    def add_cache(self, ann, time, size):
        key = str(ann)
        if key in self.cache:
            return False
        else:
            self.cache[key] = (ann, (time, size,0))
        

    def clear_cache(self):
        self.cache = {}
        
    def check_cache(self,ann):
        if not self.use_cache:
            return None
        
        key = str(ann)
        if key in self.cache:
            self.write_log("Reusing cache for %s\n" % ann)
            return self.cache[key]
        else:
            return None
                
    def add_answer(self,Time,Size,Annotation):        
        score = -1.0
        new_item = (Annotation,(Time,Size,score))

        if Time is None:
            print "WARNING EMPTY TIME FOR" , Annotation
        ### ineffiecient check to see if we already have this one:
        for (ann, _results) in self.answer_ann:
            if Annotation == ann:
                print "Duplicate hit"
                return False
        
        self.answer_ann += [new_item]
        self.update_scores()

        self.write_log("Ann: %s\nTime:%sms\nSize:%sbytes\n"%
                       (str(Annotation),
                        str(Time),
                        str(Size)
                        ))
                        
        self.add_cache(Annotation,Time,Size)
        
        
        #print "Added answer"

        self.gui.update()
        
        #self.rebuild_output()        
        #pass

    def sort_annotation_tuples(ann1,ann2):
        """
        Sort based on score comparison
        """
        (A1,(T1,S1,Score1)) = ann1
        (A2,(T2,S2,Score2)) = ann2

        if Score1 == Score2:
            return cmp(str(A1), str(A2))
        else:            
            return cmp(Score2,Score1)

    sort_annotation_tuples = staticmethod(sort_annotation_tuples)



    def run_loop_algorithm(self, base=None,select=None):
        ## We start the algorithm with base and continue as long as we have
        ## a different set of N best annotations

        if select is None:
            select = self.select
            
            
        self.write_log("\nRunning Looping Algorithm\n")
        
        self.run_algorithm(base)

        self.write_log("\n\nFinished Iteration\n\n")
        
        best = self.get_best(select)

        self.write_log("Best Selection:\n***\n")
        for (ann, score) in best:
            self.write_log("Ann: %s\nScore: %s" % (str(ann),str(score)))
        self.write_log("***\n")    
        
        #we have have a selection
        Changed = False
        for (ann,_Score) in best:
            if str(ann) in self.current_store:
                pass
            else:                
                Changed = True
                break
        
        if Changed and self.loop:
            self.write_log("Best changed, looping again\n")
            self.current_store = {}
            
            for (ann,_Score) in best:
                self.write_log("Adding initial point:%s\n" % ann)
                self.add_current(ann)                            
                self.gui.update()

            self.run_loop_algorithm()    

            

            
        

        

        
                
        
    def run_algorithm(self, base=None):
        self.answer_ann = []
        (self.normalise_time, self.normalise_code) = self.ga.benchmark(goal=self.ga.goal,spec_file=False)
        #self.normalise_score = 1
        
        
        self.gui.update()
        if base is None:
            base = []
            for key in self.current_store:
                base.append(self.current_store[key])
                
        if base == []:
            print "No Base annotations given"
            return False
        self.ga.run_algorithm(base)

        ### We have completed an iteration of the algorithm
        

    def set_initial_state(self,initial):
        self.initial_state = initial


    def update_scores(self):        
        for id in xrange(0,len(self.answer_ann)):
            #print "UPDATING ID",id, self.answer_ann[id]
            (Ann,(Time,Size,Score)) = self.answer_ann[id]
            
            Score = self.calc_score(Time,Size,self.alpha,self.beta)
            self.answer_ann[id] = (Ann,(Time,Size,Score))
        self.answer_ann.sort(GAControl.sort_annotation_tuples)

        
    def calc_score(self, Time,Size,alpha=None,beta=None):
        #try:
        if alpha is None:
            alpha = self.alpha
        if beta is None:
            alpha = self.beta           

        #print "Norm Time:", self.normalise_time
        #print "Time:", Time

        try:
            if float(Time) == 0:
                ErrDialog.complain_loudly(master=None, title="Error", msg="Benchmark time 0ms unable to calculate a fair score")              
            norm_Time = 1/(float(Time)/float(self.normalise_time))
            norm_Size = 1/(float(Size)/float(self.normalise_code))
            score = pow(norm_Time,alpha) * pow(norm_Size, beta)            
            score = max(0,score)
        except Exception,e:
            print e
            score = -1        
        
        return float(score)
        
        #except:
        #    return float(-1)

        
        
    def set_ga_state(self,timeout=None,goal=None,iterations=None,bta=False,filter_mutate=False,pp=False):
        self.ga.timeout = timeout
        self.ga.goal = goal
        self.ga.iter = iterations
        self.ga.bta = bta        
        self.ga.fil_mutate = filter_mutate
        self.ga.pp = pp


    def add_current(self,Annotation):
        short_form = str(Annotation)
        if short_form in self.current_store:
            return False
            
        self.current_store[short_form] = Annotation        
        #self.current.insert('end', short_form)
        return True
    

    def SetProgressPercent(self, percent):        
        percent = min(100,percent)
        percent = max(0,percent)
        
        self.percent = percent
        #print "Set progress", percent
        
        return percent
        
        


    def get_best(self,select =3):
        #self.set_ga_state()
        self.update_scores()
        self.answer_ann.sort(GAControl.sort_annotation_tuples)
        
        selection = self.answer_ann[:select]
        for s in selection:
            (ann, _Scores) = s
            #print "Selected", ann

        return selection

    def reset(self):
        self.current_store = {}
        #self.clear_cache()
        self.answer_ann = []
        
        self.current_store[str(self.initial_state)] = self.initial_state
        
        
        
