from Tkinter import *
import socket
import os
import time
from LogViewer import LogViewer
import popen2
from select import select
from ErrDialog import ErrDialog
from threading import Thread, Lock

import MakeLogen
import development

import re

def fileExists(f):
    try:
        open(f)
    except IOError:
        exists = 0
    else:
        exists = 1
    return exists



match_no_memo = re.compile("Unable to call predicate (.*):(.*)_request/([0-9]+)")

class Prolog:
    """
    This prolog defines the socket interface into sicstus prolog
    """
    
    _stderr = None
    _stdout = None
    _stdin = None
    def __init__(self):
        self.port = None
        self.prologvariables = {}
        

    def viewStdOut(self,master=None):
        LogViewer(Toplevel(master), 'prologstdout','Prolog Standard Output')
        #viewer = LogViewer.StringViewer(Toplevel(self.app), self.get_stdlog,'Prolog Standard Output')


    def viewStdErr(self,master=None):
        LogViewer(Toplevel(master), 'prologstderr','Prolog Error Stream')
        

    def reset_logs(self):
        self.errlog = open('prologstderr', 'w')
        self.stdlog = open('prologstdout', 'w')



    def write_to_splash(self,splash=None,text=""):
        if splash != None:
            splash.text.insert("end",text)
            splash.update()
        
        
    def startProlog(self,splash=None):
        """
        Start the sicstus process and connect to the provided socket
        This can take a while if needs rebuild
        """

        #if not os.path.exists('logen_main.sav'):
        #    print "Building Save State"
        #    if 'win32' in sys.platform:                
        #        os.system("make_save.bat")
        #    else:
        #        os.system("sh ./make_save.bat")
        saves = MakeLogen.Logen_Saves()
        self.write_to_splash(splash,"\nChecking Logen Save States...")
        saves.refresh(splash=splash)
        
        try:
            os.remove('prologport')
        except OSError:
            pass
        self.reset_logs()
        ##if we are not in development mode then use the exec...
        if development.get_development():
            (self._stdout,self._stdin,self._stderr) = popen2.popen3("sicstus -r pylogen_main.sav --goal prolog_socket,halt.")
        else:
            #(self._stdout,self._stdin,self._stderr) = popen2.popen3("../distribution/pylogen_main")
            cmd = os.path.join(".","pylogen_main")
            if sys.platform == "win32":
                cmd += ".exe"
            if os.path.exists(cmd):
                print "Loading PyLogen Prolog Module"
                print "Command Line:", cmd
                print "Current Working Directory", os.getcwd()
                
                (self._stdout,self._stdin,self._stderr) = popen2.popen3(cmd)
            else:
                print "Cannot find logen binaries, please compile or run with --dev option"
                sys.exit()
        
        
        #while fileExists('prologport') != 1:            
            #print "Prolog Port not Found"
            #time.sleep(0.05)
        #self.port = open('prologport').read()

        #print "Reading"

        line = self._stdout.readline()
        while not line.startswith("Port: "):
            line = self._stdout.readline()

        
        self.port = int(line[5:])
        
        print "Port", self.port
        #print "Using port:",self.port
        self.soc = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.soc.connect(("localhost",int(self.port)))
        print "Connected"
        #self.call('X=a,Y=b')
        #self.call("X=a,print('foo\n'),Y=ok")
        #self.call("ensure_loaded('sicstus.pl'), ensure_loaded('logen_main.pl').")
        #self.call("restore('logen_main.sav').")
	
        self.stdout = Listener(self._stdout.fileno(),filename="prologstdout")
        self.stderr = Listener(self._stderr.fileno(),filename="prologstderr")
        self.stdout.start()
        self.stderr.start()

    def call(self, call):        
        call = call.replace('\=','\\\\=')
        call = call.strip()
        #print "***CALL*** :", call
        if call[-1] != ".":
            call = call + "."
        call += "\n"        
        self.soc.sendall(call)        
        return self.rec()
        

    def get_stderr(self):
        return self.errlog
    
    
    def get_stdlog(self):
        return self.stdlog


    def read_all(self, fd):
        str = ""
        while True:
            r = select([fd], [], [], 0.2)[0]
            if r == []: return ""
            str += os.read(r[0].fileno(), 1000)
        return str  
        
        
    def rec(self):                
        ans = []

        #i = 0
        #while True:
        #    r = select([self.soc, self._stderr, self._stdout],[],[])[0]
        #    
        #    i += 1
        #    if self._stdout in r:
        #        
        #        self.stdlog.write(os.read(self._stdout.fileno(), 10000))
        #        continue
        #    if self._stderr in r:
        #        time.sleep(0.05) #power nap
        #        #print os.read(self._stderr.fileno(), 10000)
        #        self.errlog.write(os.read(self._stderr.fileno(), 10000))
        #        continue
        #        
        #    if self.soc in r: break
        #self.stdlog.flush()
        #self.errlog.flush()
        
            
        while True:
            r = select([self.soc], [], [], 10)[0]
            if self.soc in r:
                break
            time.sleep(1)
            
                       
        c = self.soc.recv(1)
        while c != "\001":            
            str = ""            
            while c != "\001":
                str += c
                c = self.soc.recv(1)
            ans.append(str)
            c = self.soc.recv(1)

        # call OK so get variables
        if ans[0] == "OK--":
            for map in ans[1:]:
                var,val = map.split("\002")
                self.prologvariables[var]=val

            return True
        elif ans[0] == "FAIL":
            return False
        else:
            print "Exception %s -- %s" % (ans[1], ans[2])
            if ans[1] == "EXISTENCE":
                match = match_no_memo.search(ans[2])
                if match is not None:
                    raise PrologException("Specialisation Error", "Attempting to memoise %s but unable to call memoisation request predicate.\nDoes %s/%d have a valid filter declaration?\n\nTry using the safety check button (lightbulb)" %(match.group(2),match.group(2),int(match.group(3))-2))
                
            raise PrologException(ans[1],ans[2])


        #should put exception throwing here
        return False
    
    def quit(self):
        self.soc.close()
        self.stdout.stop()
        self.stderr.stop()
        

class PrologException(Exception):
    """ Our exception with ciao style documentation
    """
    
class Listener(Thread):
    def __init__(self, fd,filename=None,debug=False):
        self.fd = fd
        Thread.__init__(self)
        self.mutex = Lock()
        self.debug = debug
        self.stream = open(filename,'w')
        

    def run(self):
        self.alive=True
        self.out = ""
        while self.alive:
            self.mutex.acquire()
            line = os.read(self.fd, 1000)
            if self.debug:
                if line != "":
                    print line
            self.out += line
            self.stream.write(line)
            self.stream.flush()
            self.mutex.release()
            time.sleep(0.05)


    def stop(self):
        self.alive = False
    

if __name__ == "__main__":
    os.chdir('..')
    print "Prolog Class Test Harness"
    print "========================="
    p = Prolog()
    p.startProlog()
    time.sleep(10)
    print p.stdout.out
    print p.stderr.out
    p.quit()
