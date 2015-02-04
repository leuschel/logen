from Prolog import Prolog
from Prolog import PrologException
import os

class Logen:
    """
    Maintains the calls to Logen and starts the Prolog session
    """
    
    preferences = None
    def __init__(self):        
        self.prolog = Prolog()
        self.timeout = False
        self.Spec_Time = -1
        

    def start(self,splash=None):
        """
        Starts Prolog and gets prefs
        """        
        self.prolog.startProlog(splash=splash)
        self.get_preferences()
        
        
    def get_syntax(self, filename):
        self.prolog.call("parse_file_syntax('" + filename + "', Syntax)")
        syntax = self.prolog.prologvariables["Syntax"]
        return syntax.split()

    def get_annotations(self, filename, annfile=None):
        if not os.path.exists(filename + ".ann"):
            print "Annotation file '" + filename + ".ann' does not exist"
            return ([], [], "", [])
        if annfile is None:
            annfile = filename + ".ann"

            
        if not self.prolog.call("request_ann('" + filename + "','"+annfile+"', Format, Syntax, Filter, FilSyntax)"):
            print "Request Annotations Failed please check ann file and prolog log"
            raise Exception("Request Ann Failed")
        format = self.prolog.prologvariables["Format"].split()
        syntax = self.prolog.prologvariables["Syntax"].split()
        filter = self.prolog.prologvariables["Filter"]
        filsyntax = self.prolog.prologvariables["FilSyntax"].split()

        #print format
        
        return (format, syntax, filter, filsyntax)

        
    def save_annotations(self, filename, annotations, filters):
        filters = filters.replace("'","\\\'")
        
        self.prolog.call("save_ann('"+filename+"', '"+annotations+"', '"+filters+"')")

    def get_preferences(self):
        if self.preferences == None:
            self.preferences = Preferences(self.prolog)
        return self.preferences
        
    def runcogen(self,filename):
        return self.prolog.call("moduledriver:run_cogen_from_file('%s')" % filename)

    def specialise(self,goal,file,timeout=-1):
        self.timeout = False #reset timeout flag
        if goal.strip().endswith("."):
            goal = goal.strip()[:-1]
        print "Specialise the file %s for goal %s" % (file,goal)
        if self.prolog.call( "atom_ok_to_specialise((%s))" % goal ):
            print "Specialising %s for %s" % (file, goal)
            self.prolog.call("run_cogen_if_necessary('%s')" % file)
            #self.prolog.call("run_cogen_on_plfile('%s')" % file)
            self.prolog.call("specialise_plfile('%s', (%s),rebuild_memo,_,LOGEN_SPEC_TIME,%d,LOGEN_TIMEOUT_RESULT)" % (file, goal,timeout))
            Spec_Time = self.prolog.prologvariables["LOGEN_SPEC_TIME"]
            Timeout_result = self.prolog.prologvariables["LOGEN_TIMEOUT_RESULT"]
            
            if Timeout_result == "time_out":
                print "Specialisation timed out"
                self.timout = True
                raise PrologException("Specialisation Error", "Specialisation Timed out\nCheck annotations or increase the timeout.\nCurrent time out is %dms" % (timeout))
                
            print "Finished specialising plfile in", Spec_Time,"ms"
            self.Spec_Time = Spec_Time
            #self.prolog.call( "moduledriver:rebuild_everything")
            #self.prolog.call("moduledriver:request_pattern('%s', %s)" % (file,goal))
            #self.prolog.call("moduledriver:specialize")
            self.prolog.call("set_preference(last_spec_query,(%s))" % (goal))
            
            return True
            #self.view_spec()
            #self.enable_after_spec()                 
        else:            
            print "GOAL NOT OKAY TO SPECIALISE %s" %goal
            return False

    def specialisecogen3(self, goal, file):
	os.chdir('../../cogen3/')
	print "Specialising using cogen3!"
	os.chdir('../cogen2/logen_source/')

    def quit(self):
        self.prolog.quit()
        
    def restart(self,unused=None):
        self.prolog.quit()
        self.prolog = Prolog()
        self.prolog.startProlog()
        self.preferences = Preferences(self.prolog)

    def rebuild(self):
        os.remove('logen_main.sav')
        self.restart()
        
    def viewStdOut(self):
        self.prolog.viewStdOut()
    def viewStdErr(self):
        self.prolog.viewStdErr()

    def filter_prop(self,filename):
        annfile = filename +".ann"
        #auto_bta_dir = '/home/sjc02r/cvs_root/Asap/Systems/bta/'        
        auto_bta_dir = self.preferences["auto_bta_dir"].strip()
        print auto_bta_dir

        if auto_bta_dir == "None":
            
            print "Please set autobta dir"
            return None
        else:
            logen_bta = auto_bta_dir + '/logen/logenbta.pl'
            auto_bta = auto_bta_dir + '/auto_bta'
            outfile = 'filter.tmp'
            os.system('%s %s %s %s' % (auto_bta,annfile,logen_bta,outfile))
            return outfile
        
    def annotate_file(self, file,mode="safe"):
        print "Annotate File::",file        
        return self.prolog.call("annotateFile('"+file+"','"+file+".ann',%s)" % mode)
        
    def check_string_syntax(self, string):
        self.prolog.call("parse_string_syntax('"+escape_str(string)+"', Syn)")
        syn = self.prolog.prologvariables["Syn"].split()
        #print syn
        return not (len(syn) == 2 and syn[0] == "error")

    def parse_file(self,filename):
        self.prolog.call("parse_file('"+filename+"')")
        
        
class Preferences:
    def __init__(self,prolog):
        self.prolog = prolog        
        self.load('logen_preferences.pl')
        
    def save(self):
        self.prolog.call("logen_preferences:save_preferences('logen_preferences.pl')")

    def load(self,filename):
        self.prolog.call("logen_preferences:init_and_load_preferences('%s')" % filename)
        
    def __getitem__(self,name):
        
        if self.prolog.call("tcltk_get_preference(%s,P)"%name):
            pass
            #print "Preference %s found" % name
        else:
            print "Preference %s NOT found" % name
        #print "get %s-->%s" % (name,self.prolog.prologvariables["P"])
        return self.prolog.prologvariables["P"]

    def __setitem__(self,name, value):
        #print "set item %s %s" %(name,value)
        self.prolog.call("set_preference(%s,'%s')" % (name,value))

    def add_recent(self,filename):
        self.prolog.call("add_recent_document('%s')." % filename)

    def get_recent(self):                          
        self.prolog.call("get_recent_documents(RECENT).")
        return self.prolog.prologvariables["RECENT"].split("<LISTSEP>")[1:]

    def clear_recent(self):
        self.prolog.call("clear_recent_documents.")
    
        
        
# Returns a new string with escaped single quotes to be used in prolog calls
def escape_str(str):
    ret_str = ""
    ptr = 0
    length = len(str)
    while ptr < length:
        if str[ptr] == "'":
            ret_str += "\\'"
        else:
            ret_str += str[ptr]
        ptr += 1
    return ret_str
