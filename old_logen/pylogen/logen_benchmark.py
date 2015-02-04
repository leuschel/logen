import development
import os

import Pmw
import Tkinter
import re


rebench = re.compile('.*Benchmark completed in ([0-9]+)ms for ([0-9]+) runs.*')
resize = re.compile('.*Compiled Code Size: ([0-9]+) bytes.*')


thesis_benchmarks = {
        "Vanilla":("../examples/extending_vanilla/vanilla_list_inc.pl",
                   "solve_atom(rev_app([a,b,c,d,e,f,g,h],[i,j,k,l,m,n,o,p],C))",
                   3000000
                   #300000
                   ),
        "Debug (No debug)":("../examples/extending_vanilla/vanilla_debug_inc.pl",
                         "solve_atom(rev_app([a,b,c,d,e,f,g,h],[i,j,k,l,m,n,o,p],C),[])",
                   3000000
                   #300000
                   ),
        "Debug (trace rev/2)":("../examples/extending_vanilla/vanilla_debug_inc.pl",
                         "solve_atom(rev_app([a,b,c,d,e,f,g,h],[i,j,k,l,m,n,o,p],C),[foo/2,append/1,ranch/2, barber/2, dog/1, cat/2,rev/2])",
                   30000
                   #1000
                   ),
        "Profile (no Profile)":("../examples/extending_vanilla/vanilla_profile_inc.pl",
              "solve_and_print(rev_app([a,b,c,d,e,f,g,h],[i,j,k,l,m,n,o,p],C),[])",
                   3000000
                  ),
        
         "Profile (rev/2,append/3)":("../examples/extending_vanilla/vanilla_profile_inc.pl",
              "solve_and_print(rev_app([a,b,c,d,e,f,g,h],[i,j,k,l,m,n,o,p],C),[rev/2,append/3])",
                   30000
                  ),

        "Cache (no Cache)":("../examples/extending_vanilla/vanilla_cache_inc.pl",
              "solve_file('fib.pl', fib(25,X),[])",
                   100
                  ),
        "Cache (fib/2)":("../examples/extending_vanilla/vanilla_cache_inc.pl",
              "solve_file('fib.pl', fib(25,X),[fib/2])",
                   100000
                  ),

        "Lloyd Topor":("../examples/extending_vanilla/vanilla_lloyd_topor.pl",
              "solve_atom(subset([a,c],[c,b,a]))",
                   100000
                  ),
        
        }




def get_pylogen_cmd():
    if development.get_development():
        cmdline = "sicstus -r pylogen_main.sav --goal \"runtime_entry(start),halt.\" -a"
    else:
        cmdline = os.path.join(".","pylogen_main")
        
    return cmdline

def run_benchmark(file,spec_file, parent=None,goal="",iterations="1000",app=None):
    """ We have filename we just need to ask for the goal and number of times....
    """
    print "Benchmarking %s" % file
    d = BenchmarkDialog(parent, title="Benchmarking %s" % os.path.basename(file),file=file, goal=goal,iterations=iterations,app=app)
    d.showAppModal()
    
    return (d.goal,d.iterations)



def iterations_to_list(times):
    print "Called with %d" % times
    itr = []
    while times > 100:
        times = times / 100
        itr.append(100)

    itr.append(times)
    
    print "Iterations", itr
    return "%s" % itr

    
def benchmark(file, goal, times):
    times = int(times)
    
    list_itr = iterations_to_list(times)    
    cmdline = get_pylogen_cmd()
    command = '%s -benchmark "%s" "%s" "%s"' % (cmdline,file,goal,list_itr)

    print(command)    
    lines = os.popen2(command)[1].readlines()
    #for l in lines:
    #    print l,
    return lines

def benchmark_spec(file, goal, times):
    times = int(times)    
    list_itr = iterations_to_list(times)    
    cmdline = get_pylogen_cmd()
    command = '%s -benchmarkspec "%s" "%s" "%s"' % (cmdline,file,goal,list_itr)
    lines = os.popen2(command)[1].readlines()
    return lines

def benchmark_loop(file, goal, times):    
    cmdline = get_pylogen_cmd()    
    command = '%s -benchmarkloop "%s" "%s" "%s"' % (cmdline,file,goal,times)
    print command
    lines = os.popen2(command)[1].readlines()
    return lines





def benchmark_pl(file, goal, times):
    times = int(times)    
    list_itr = iterations_to_list(times)    
    cmdline = get_pylogen_cmd()
    command = '%s -benchmarkpl "%s" "%s" "%s"' % (cmdline,file,goal,list_itr)
    lines = os.popen2(command)[1].readlines()
    return lines


def main_benchmark(file,goal,iterations):
    str = ""
    time = "0"
    iter = "0"
    size = "0"
    
    
    str += "Original File:\n"
    output = benchmark_loop(file,goal,iterations)        
    for l in output:
        match = rebench.search(l)
        if match is not None:
            time = int(match.groups()[0])
            iter = int(match.groups()[1])
        match = resize.search(l)
        if match is not None:
            size = int(match.groups()[0])
        str += "     " + l
        
    str += "\n"    
    return (time,iter,size,str)


def specialise_and_benchmark(file, goal, iterations, app):    
    #load file
    app.open_file(filename=file)
    #specialise file    
    app.specBar.set_goal(goal)        
    specialisation_time = app.specBar.specialise()
    
    #run benchmark
    #original
    (orig_time,orig_iter,orig_size,orig_output) = main_benchmark(file,goal,iterations)
    #spec
    (root,ext) = os.path.splitext(file)
    spec = root+".spec"            
    (spec_time,spec_iter,spec_size,spec_output) = main_benchmark(spec,goal,iterations)    
    #return results

    return (specialisation_time,orig_iter, orig_time, orig_size, spec_iter, spec_time, spec_size)


def thesis_bench_one(Tuple,app):
    (bench,(file, goal,iter)) = Tuple
        
    #(specialisation_time,orig_iter, orig_time, orig_size, spec_iter, spec_time, spec_size) = specialise_and_benchmark(file,goal,iter, app)
    result  =specialise_and_benchmark(file,goal,iter, app)
    print_bench_result(bench,result)
    
    #results[bench] = (specialisation_time,orig_iter, orig_time, orig_size, spec_iter, spec_time, spec_size)
    
def print_bench_result(bench,(specialisation_time,orig_iter, orig_time, orig_size, spec_iter, spec_time, spec_size)):
    print "%s %d iterations SpecTime: %s " %(bench, orig_iter,specialisation_time)
    print "Orig: %dms %dbytes  1" % (orig_time, orig_size)
    print "Spec: %dms %dbytes  %.2f " % (spec_time, spec_size,(float(spec_time)/float(orig_time)))
            
    rel_time = (float(spec_time)/float(orig_time))        
    print "%s \t& %d \t& 0$\\to$10ms \t& %dms \t& %dms \t&  %.2f\\\\" % (bench, orig_iter,orig_time, spec_time, rel_time)
    rel_size = (float(spec_size)/float(orig_size))
    print "%s \t& %d bytes \t& %d bytes \t&  %.2f\\\\" % (bench, orig_size, spec_size, rel_size)
    print ""
    
def run_thesis_benchmarks(app):
    results = {}
    
    for bench in thesis_benchmarks:
        (file,goal,iter) = thesis_benchmarks[bench]
        (specialisation_time,orig_iter, orig_time, orig_size, spec_iter, spec_time, spec_size) = specialise_and_benchmark(file,goal,iter, app)
        results[bench] = (specialisation_time,orig_iter, orig_time, orig_size, spec_iter, spec_time, spec_size)


    for bench in results:
        print_bench_result(bench, results[bench])

        
        

    
                   

class BenchmarkDialog:
    def __init__(self, parent,title="Benchmarking",file=" ",goal=None,iterations=None,spec_file=" ",app=None):
        # Create the dialog.
        self.app = app
        if iterations is None:
            iterations = "1000"
        if goal is None:
            goal = ""
            
        self.spec_file = file
        self.file = file
        self.goal = goal
        self.iterations = iterations
        self.parent = parent
        
        self.dialog = Pmw.Dialog(parent,
            buttons = ('Benchmark','Benchmark Spec Only', 'Use Current Goal','Specialise','Cancel'),
            defaultbutton = 'OK',
            title = title,
            command = self.execute)
        
        self.dialog.withdraw()

        self.it = Pmw.Counter(self.dialog.interior(),
                        labelpos = 'w',
                        label_text = 'Iterations:',
                        entryfield_value = str(self.iterations),
                        entryfield_validate = {'validator':'integer','min':1},
                        increment = 100
                        )
    
        self.it.pack(expand = 1, fill = 'both', padx = 4, pady = 4)
        self.goal_entry = Pmw.EntryField(self.dialog.interior(),
                labelpos = 'w',
                label_text = 'Benchmark Goal:',
                value = self.goal,
                validate = None
                )
        self.goal_entry.pack(expand = 1, fill = 'both', padx = 4, pady = 4)

        
    def showAppModal(self):
        self.dialog.activate(geometry = 'centerscreenalways')

    def showGlobalModal(self):
        self.dialog.activate(globalMode = 1)

    def showDialogNoGrab(self):
        self.dialog.activate(globalMode = 'nograb')
    
    def execute(self, result):
        if result is None:
            self.dialog.deactivate(result)
            return
        print 'You clicked on', result

        if result in ('Use Current Goal'):
            self.goal = self.app.specBar.specEntry.get()        
            self.goal_entry.setvalue(self.goal)
            return
        if result in ('Specialise'):
        
            self.app.specBar.set_goal(self.goal_entry.get())        
            self.app.specBar.specialise()
            return

        
        if result not in ('Benchmark','Benchmark Spec Only'):
            self.dialog.deactivate(result)
            return

        
        self.goal = self.goal_entry.get()
        self.iterations = self.it.get()
        print "Execute benchmark for %s %s %s" % (self.file, self.goal, self.iterations)

        str = ""
        orig_output = ""
        orig_time = "0"
        orig_iter = "0"
        orig_size = "0"
        
        if result in ('Benchmark'):
            str += "Original File:\n"
            (orig_time,orig_iter,orig_size,orig_output) = main_benchmark(self.file,self.goal,self.iterations)
            str += orig_output
            
        (root,ext) = os.path.splitext(self.file)
        spec = root+".spec"
        
        spec_time = "0"
        spec_iter = "0"
        spec_size = "0"
        
        str += "Specialised File:\n"
        (spec_time,spec_iter,spec_size,spec_output) = main_benchmark(spec,self.goal,self.iterations)
        str += spec_output

        output_stats = "Goal: %s\n" %self.goal
        output_stats += "Benchmark for %s iterations\n" % spec_iter
        output_stats += "Orig:  %sms %sbytes\n" % (orig_time, orig_size)
        output_stats += "Spec:  %sms %sbytes\n" % (spec_time, spec_size)
        
        
        dialog = Pmw.TextDialog(self.parent, scrolledtext_labelpos = 'n',
                title = 'Benchmark Results',
                defaultbutton = 0,
                label_text = 'Benchmark')
        dialog.withdraw()        
        #dialog.insert('end', str)
        print str,
        dialog.insert('end', output_stats)
        
        dialog.configure(text_state = 'disabled')
        dialog.activate()

        
        


        
if __name__ == '__main__':
    development.set_development_mode(True)
    lines = benchmark( '../logen_source/bta_files/tests/transpose.pl','transpose([[a,b,c],[a,b,c], [a,b,c]],R)', '[100,100,100]')
    for l in lines:
        print l,
    



