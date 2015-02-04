import re
import os

import development

totaltime = re.compile("Total Time: ([0-9]*)ms ([0-9]*) iterations.*")




def get_bta_cmd():
    if development.get_development():
        cmdline = "sicstus -r run_bta.sav --goal \"runtime_entry(start),halt.\" -a"
    else:
        cmdline = os.path.join(".","bta")

    return cmdline

    

def filter_prop(filename, annfile, output):
    cmdline = get_bta_cmd()

    command = "%s %s -o %s -i %s -gui -filter" % (cmdline,filename,output,annfile)
    os.system(command)

    

def reset_file(filename,output,filters=None):
    #write the filters file...

    #run bta..
    #if development.get_development():
    #    command = ("sicstus -r bta.sav --goal \"set_convex_norm(%s),run_bta_loop_int('%s','%s','%s'),halt.\"" %(norm,filename, annfile,output))
    #else:
    print filters
    
    pass
    
    
def run_bta(filename,output, annfile=None, norm='term',profile=False):
    print "Running Bta..."

    #either use exe or save state...
    cmdline = get_bta_cmd()
    
    command = "%s %s -a -norm %s -o %s -i %s -gui" % (cmdline,filename,norm,output,annfile)
                                                            
    #os.system(command)
    time = 0
    iterations = 0
    lines = os.popen2(command)[1].readlines()
    time = 0
    for i in lines:
        #debug
        print i,
        timetaken = totaltime.search(i)
        if timetaken != None:
            time = timetaken.groups()[0]
            iterations = timetaken.groups()[1]
            print "Time:",time

            break
        
    return (lines,time,iterations)


def run_binsolve(filename,annfile,output):
    print "Running Bin_solve"
    cmdline = get_bta_cmd()    
    command = "%s %s -b -gui -o %s -i %s -s" % (cmdline,filename, output, annfile)
    os.system(command)    
    return output

def run_convex(filename,annfile,output,norm):
    print "Running Bin_solve"
    cmdline = get_bta_cmd()    
    command = "%s %s -b -gui -o %s -i %s -c -s -norm %s" % (cmdline,filename, output, annfile, norm)
    os.system(command)    
    return output


def run_global_bta(annfile):
    print "Running Global Bta..."
    command = ("sicstus -r global_bta.sav --goal \"global_check('%s',Unsafe),halt.\"" %( annfile))
    #print command


    return os.popen(command).readlines()

    


