import re


re_fil = re.compile('.*?:- filter\n\s*(.*?)\(.*(\n)')

    


class Annotation:
    """
    Class to define a set of annotations for a file
    Used at the moment for only the logen ga but could
    be used in the future to store seperate annotations
    for the same file
    """
    
    def __init__(self, Ann, Filters,convert=False,app=None):
        #print "Annotations:", Ann
        #print "Annotations:", Filters

        self.app = app
        
        if convert:
            self.ann = self.convert_ann(Ann)
        else:
            self.ann = Ann
        self.filter = Filters
        self.score = None
        


    def __eq__(self, y):
        #return (self.ann == y.ann) and (self.filter == y.filter)
        return (str(self) == str(y))
    
    

    def __str__(self):        
        ann = self.ann        
        str = ""
        #print ann
        for a in ann:
            if a in ["unfold", "memo", "rescall", "call"]:
                str += a[0] + " "                

        

        fil_short = ""
        for i in xrange(0,len(self.filter)):
            if self.filter[i:].startswith("static"):
                fil_short += "s "
            if self.filter[i:].startswith("dynamic"):
                fil_short += "d "
            if self.filter[i:].startswith("list"):
                fil_short += "l:"
            
                
        
        return str + fil_short
    
    
    def convert_ann(self,ann):
        l = ann.split(' ')
        length = len(l)        
        new = []        
        for i in xrange(0, length/3):            
            new.append(l[i*3+1])
            new.append(l[i*3+2])
            new.append(l[i*3])
            
        return new

    
    def mutations(self,mutate_filters=False):
        #all mutations have themselves as first element
        yield self
    
                                    
        fil_str = self.filter
        ann_list = self.ann

        ### always mutate filters for initial goal!        
        goal =  self.app.ga_diag.goal    
        i = goal.find('(')
        if i is -1:
            i = goal.find('.')
            if i is -1:
                goal = goal.strip()
            else:
                goal = goal[:i].strip()
        else:
            goal = goal[:i].strip()
            
            
        memo_list = [goal]
        
        #mutate annotations first 
        for i in xrange(0,len(ann_list)):
            ## we keep a list of memo-ed predicates, as we should only mutate filters that match
            if ann_list[i] == 'memo':
                start = ann_list[i-2]
                end = ann_list[i-1]
                pred = self.app.ann_frame.sourceFrame.text.get(start,end)
                
                memo_list += pred
                
                
            if ann_list[i] == 'unfold':
                m = ann_list[:i] + ['memo'] + ann_list[i+1:]  
                yield Annotation(m,fil_str)

        # we should only be intereseted in mutating the fitler of memoed predicates
        if mutate_filters:            
            i = 0
            match = re_fil.search(fil_str[i:])
            while match is not None:                
                pred = match.groups()[0]
                start = match.start() + i
                end = match.end() + i

                #this doesnt take arity into account...
                if pred in memo_list:
                    #mutate filters
                    print "Mutate filters for pred", pred                    
                    for i in xrange(start,end):                
                        fil_type = 'static'
                        if fil_str[i:].startswith(fil_type):
                            new_fil = fil_str[:i] + 'dynamic' + fil_str[i+len(fil_type):]                            
                            yield Annotation(ann_list,new_fil)

                        fil_type = '(type list(dynamic))'
                        if fil_str[i:].startswith(fil_type):
                            new_fil = fil_str[:i] + 'dynamic' + fil_str[i+len(fil_type):]                            
                            yield Annotation(ann_list,new_fil)
                            
                else:
                    print "Dont mutate filters for pred", pred
                    
                i = end
                match = re_fil.search(fil_str[i:])
                
                
                

                        
                    
                
            



                        
            


                


        
        
    
