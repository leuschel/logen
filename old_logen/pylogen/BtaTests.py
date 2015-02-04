#import logen
import os
import sys
import time

import ErrDialog

class BtaTest:
    def __init__(self, app):
        self.app = app
        
    
    def run_bta(self,filename):
        filename = os.path.abspath(filename)
        filename = filename.replace("\\","/")
        #print "Annotate File::",file
        self.app.open_file(filename,reset=True,savedir=False)    
        self.app.reopen_file(savedir=False)
        self.app.ann_frame.reset_filters()
        self.app.save_ann()
        (_,time,iter) = self.app.auto_bta(print_output=False)
        self.app.save_ann()

        return (time,iter)

    def test_file(self, filename,showtime=True):
        print "Running Bta test for %s" % (filename)

        #remove any existing annfile
        annfile = filename + ".ann"
        if (os.path.exists(annfile)):
            os.remove(annfile)

        
        #run autobta
        (time,iter) = self.run_bta(filename)

       
        #test if output is correct!
        diffannfile = os.path.dirname(annfile) + "/expected/" + os.path.basename(annfile)
        cmd =  "diff %s %s" % (annfile, diffannfile)

        if os.system(cmd) == 0:
            print "OK",showtime
            if showtime:
                Message = "TestCase ok: %dms, %d iterations" % (int(time),int(iter))
                ErrDialog.complain_loudly(master=self.app,
                                      msg=Message,
                                      title="Bta Tests")
            return (time,iter)
        else:
            print "Failed"
            ErrDialog.complain_loudly(master=self.app,
                                      msg="BTA Test failed for %s" % filename,
                                      title="Bta Tests-Error")
            return False
        

    def test_all(self):
        
        Res = True

        Message = ""
        tc = self.testcases.keys()
        tc.sort()

        t = time.time()

        prologtime = 0
        for test in tc: 
            t1 = time.time()
            Ans = self.test_file(self.testcases[test],showtime=False)
            
            Res &= (Ans != False)
            
            if Res:
                timetaken = time.time() - t1
                (time1,iter1) = Ans
                prologtime += int(time1)
                #Message += "\n%20s completed in %.2f seconds (%dms, %d iterations)" % (test,timetaken,int(time1),int(iter1))
                Message += "\n%20s ok: %dms, %d iterations" % (test,int(time1),int(iter1))
                
                

        if Res:
            timetaken = time.time() - t
            Message = "All BTA OK %.2f seconds (PrologTime: %dms)" % (timetaken,int(prologtime)) + Message
            ErrDialog.complain_loudly(master=self.app,
                                      msg=Message,
                                      title="Bta Tests")

        
        print Res

    
    testcases = {
        "combined":"bta_files/tests/comb.pl",
        "inter_binding":"bta_files/tests/inter_binding.pl",
        "inter_simple":"bta_files/tests/inter_simple.pl",
        "inter_medium":"bta_files/tests/inter_medium.pl",
        "match":os.path.join('bta_files','tests','match.pl'),
        "regexp":"bta_files/tests/regexp.pl",
        "transpose":"bta_files/tests/transpose.pl",
        
        ## currently there is an error with vanilla?
        #"vanilla":"bta_files/tests/vanilla_list.pl",
        
        }


    
            
    


        
        
    
