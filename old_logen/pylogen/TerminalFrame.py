from ScrollText import ScrollText
from Tkinter import *

from Prolog import Prolog
from FastIndex import FastIndex
from Prolog import Listener
from select import select
from threading import Thread, Lock
import ErrDialog, re, Pmw, popen2, os, time, bisect, sys

import Pmw
from tkFileDialog import askopenfilename

try:
    import posix, signal
except:
    pass

PrologBuiltins= ["print","format","portray_clause","term_variables","open","close","read_term", "read", "use_module", "consult", "ensure_loaded","use_module(library", ]

class TerminalFrame(LabelFrame):
    app = None
    stdin = None
    stdout = None
    stderr = None

    running = None
    
    def __init__(self, master=None, app=None, id=None):
        LabelFrame.__init__(self, master=master)

        self.app = app
        self.id = id
        
        ##The control box on the right        
        self.control_frame = ControlFrame(self,self)                
        self.control_frame.pack({"side":"right","fill":"y"})
        
        self.stext = Pmw.ScrolledText(self)        
        self.text = self.stext._textbox
        self.stext.pack({"expand":"yes", "fill":"both"})
                        
        self.ctl = 0
        self.alt = 0
        self.shift = 0
        
        self.temphistory = [""]
        self.history = []
        self.historypos = 1

        self.bind("<FocusIn>", self.got_focus)
        
        self.text.bind("<Key>", self.keypressed)
        self.text.bind("<Key-BackSpace>", self.backspace)
        self.text.bind("<Key-Delete>", self.delete)
        self.text.bind("<Key-Return>", self.return_trap)
        self.text.bind("<Key-Tab>", self.tab)

        self.text.bind("<KeyPress-Control_L>", self.ctldown)
        self.text.bind("<KeyRelease-Control_L>", self.ctlup)
        self.text.bind("<KeyPress-Control_R>", self.ctldown)
        self.text.bind("<KeyRelease-Control_R>", self.ctlup)
        
        self.text.bind("<KeyPress-Shift_L>", self.shiftdown)
        self.text.bind("<KeyRelease-Shift_L>", self.shiftup)
        self.text.bind("<KeyPress-Shift_R>", self.shiftdown)
        self.text.bind("<KeyRelease-Shift_R>", self.shiftup)
        
        self.text.bind("<KeyPress-Alt_L>", self.altdown)
        self.text.bind("<KeyRelease-Alt_L>", self.altup)
        self.text.bind("<KeyPress-Alt_R>", self.altdown)
        self.text.bind("<KeyRelease-Alt_R>", self.altup)

        self.text.bind("<Key-Up>", self.arrow)
        self.text.bind("<Key-Down>", self.arrow)
        self.text.bind("<Key-Left>", self.arrow)
        self.text.bind("<Key-Right>", self.arrow)
        self.text.bind("<Key-End>", self.arrow)
        self.text.bind("<Key-Home>", self.arrow)
        
        self.text.bind("<Control-c>", self.send_cancel)

        #steve fix: added bindings
        #self.text.bind("<Button-1>", self.mouse_click)
        
        #self.text.bind("<Button-2>", self.mouse_click)
        #self.text.bind("<Button-3>", self.mouse_click)


        self.reset_completions()
        self.tab_pressed = False

        self.start_prolog()

    def reset_completions(self):
        #self.completions = []
        self.completions = PrologBuiltins
        self.completions.sort()
        
    def add_completions(self, c):
        self.completions.extend(c)
        self.completions.sort()
        #print "completions", self.completions

    def got_focus(self, event=None):        
        if not self.running:
            self.start_prolog()
            
        
    def load_file(self, filename):
        """
        Load a file into the Prolog session using use_module/1
        """
        self.currentline =  "use_module('%s')."%  filename
        self.show_currentline()
        self.text.insert("insert", "\n")
        self.send_line()        
        
    def reset(self):
        """
        This should halt terminal and restart it, maintaining history
        """
        #print "reset clicked"
        self.quit()
        self.start_prolog()

    def close(self):
        """
        Should close terminal.  For when we have multiple terminals
        """
        self.app.outbook.kill_terminal(self.id)
        #print "close clicked"
        

    def start_prolog(self):
        if sys.platform[:3] == "win" or sys.platform == "os2emx":
            (self._stdout, self.stdin) = popen2.popen4("sicstus -i", 0)
            self.pid = 0
        else:
            inst = popen2.Popen4('sicstus -i', -1)
            self.pid = inst.pid
            #print "child has pid:", self.pid
            self._stdout = inst.fromchild
            self.stdin = inst.tochild

        self.stdout = MemListener(self._stdout, self)
        self.stdout.start()

        #reset vars
        self.reset_state()
        self.running = True

    def reset_state(self):
        """
        Reset vars but NOT history
        """
        self.cursor_pos = 0
        self.output_modified = False
        self.input_modified = False
        self.prompt_pos = "end"
        self.currentline = ""
        self.tab_pressed = False

    def mouse_click(self, event=None):
        """
        Set the cursor to the correct place
        Steal the focus
        Don't let tk mouse and place cursor
        """
        #self.text.mark_set("insert" , "%s +%s chars"%(self.prompt_pos,self.cursor_pos))
        self.text.focus_set()
        return "break"

    def reset_cursor(self):
        self.text.mark_set("insert", "%s+%d c"%(self.prompt_pos,self.cursor_pos))
        
    def show_currentline(self, reset_cursor=True):
        self.text.delete(self.prompt_pos, "end-1c")
        self.text.insert("insert", self.currentline)
        self.text.mark_set("insert", self.prompt_pos)
        self.input_modified = True
        self.output_modified = False
        if reset_cursor:
            self.cursor_pos = len(self.currentline)
        self.reset_cursor()
        self.look_at_end()
        
    def next_history(self):
        if self.historypos > 1:
            self.temphistory[-self.historypos] = self.currentline
            self.historypos -= 1
            self.currentline = self.temphistory[-self.historypos]
            self.tab_pressed = False
        self.show_currentline()

    def prev_history(self):
        if self.historypos < len(self.temphistory):
            self.temphistory[-self.historypos] = self.currentline
            self.historypos += 1
            self.currentline = self.temphistory[-self.historypos]
            self.tab_pressed = False
        self.show_currentline()

    def cursor_forwards(self):
        if self.cursor_pos < len(self.currentline):
            self.cursor_pos += 1
            self.tab_pressed = False
        self.output_modified = False
        self.text.mark_set(INSERT, "%s+%dc" % (self.prompt_pos,self.cursor_pos))

    def cursor_backwards(self):
        if self.cursor_pos > 0:
            self.cursor_pos -= 1
            self.tab_pressed = False
        self.output_modified = False
        self.text.mark_set(INSERT, "%s+%dc" % (self.prompt_pos,self.cursor_pos))

    def cursor_end(self):
        if self.cursor_pos != len(self.currentline):
            self.cursor_pos = len(self.currentline)
            self.tab_pressed = False
        self.output_modified = False
        self.text.mark_set(INSERT, "%s+%dc" % (self.prompt_pos,self.cursor_pos))

    def cursor_home(self):
        if self.cursor_pos > 0:
            self.cursor_pos = 0
            self.tab_pressed = False
        self.output_modified = False
        self.text.mark_set(INSERT, "%s+%dc" % (self.prompt_pos,self.cursor_pos))

    def arrow(self, ev):
        if self.output_modified:
            if self.input_modified:
                self.text.insert("end-1c", "\n")
                self.prompt_pos = self.text.index("end")
                self.text.insert(self.prompt_pos, self.currentline)
            else:
                self.prompt_pos = self.text.index("end-1c")

        if ev.keysym == "Up":
            self.prev_history()
        elif ev.keysym == "Down":
            self.next_history()
        elif ev.keysym == "Left":
            self.cursor_backwards()
        elif ev.keysym == "Right":
            self.cursor_forwards()
        elif ev.keysym == "End":
            self.cursor_end()
        elif ev.keysym == "Home":
            self.cursor_home()
        
        #print self.temphistory, self.currentline, self.cursor_pos
        return "break"

    def delete_to_line_end(self):
        if self.cursor_pos < len(self.currentline):
            self.currentline = self.currentline[:self.cursor_pos]
            self.show_currentline()
            self.input_modified = True
            self.tab_pressed = False
        return "break"

    def delete(self, ev):
        #print "Delete:", self.currentline, self.cursor_pos
    
        if self.cursor_pos < len(self.currentline):
            self.currentline = self.currentline[:self.cursor_pos] + self.currentline[self.cursor_pos + 1:]
            self.show_currentline(reset_cursor=False)
            self.reset_cursor()
            self.input_modified = True
            self.tab_pressed = False
        return "break"

    def backspace(self, ev):
        #print "Backspace:", self.currentline, self.cursor_pos, self.prompt_pos
    
        if self.cursor_pos > 0:
            self.currentline = self.currentline[:self.cursor_pos - 1] + self.currentline[self.cursor_pos:]
            self.cursor_pos -= 1
            self.show_currentline(reset_cursor=False)
            self.input_modified = True
            self.tab_pressed = False
        return "break"

    def tab(self, ev):
        up_to_cursor = self.currentline[:self.cursor_pos]
        index = max(up_to_cursor.rfind(' '), up_to_cursor.rfind(','))
        text_to_complete = up_to_cursor[index + 1:]
        #if len(text_to_complete) > 0:
        (common, completions) = self.get_completions(text_to_complete)
        if self.output_modified:
            self.prompt_pos = self.text.index("end-1c")
            self.output_modified = False

        if not self.tab_pressed:
            self.append_text_to_currentline(common[len(text_to_complete):])
            self.tab_pressed = True
        elif len(completions) > 0:
            self.text.insert('end', '\n')
            width = 0
            screenwidth = int(self.text.cget('width'))
            #print "screenwidth:",screenwidth
            max_length = 0
            for c in completions:
                if len(c) > max_length:
                    max_length = len(c)
            columns = max(1, screenwidth / (max_length + 2))
            col_pos = range(4, screenwidth, max_length + 2)
            col = 0
            pos = 0
            #print "col_pos:",col_pos
            for c in completions:
                if col == columns:
                    col = 0
                    self.text.insert('end', '\n')
                    pos = 0

                spaces = ''
                #print "col:",col
                for i in xrange(col_pos[col] - pos):
                    spaces += ' '
                    pos += 1
                self.text.insert('end', spaces + c)
                pos += len(c)
                col += 1

            self.text.insert('end', '\n')
            self.prompt_pos = self.text.index("end-1c")
            self.text.insert('end', self.currentline)
            self.output_modified = False
            self.input_modified = False
            self.reset_cursor()
        self.look_at_end()
            
        return "break"

    def append_text_to_currentline(self, text):
        self.reset_cursor()            
        self.text.insert('insert', text)
        self.cursor_pos += len(text)
        self.currentline += text

    def return_trap(self, ev):
        if self.output_modified:
            self.prompt_pos = self.text.index("end-1c")
            self.output_modified = False
        self.text.mark_set(INSERT, "%s+%dc" % (self.prompt_pos,len(self.currentline)))
        self.text.insert("insert", "\n")
        self.send_line()

        return "break"

    def keypressed(self, ev):
        if ev.char == "\x03" or (ev.keysym == "c" and self.ctl):
            return None
        if self.output_modified:
            self.prompt_pos = self.text.index("end-1c")
            self.output_modified = False
        self.reset_cursor()            
        if self.alt:
            if ev.char == 'p':
                self.prev_history()
            elif ev.char == 'n':
                self.next_history()
            return "break"
        elif self.ctl:
            if ev.keysym == 'p':
                self.prev_history()
            elif ev.keysym == 'n':
                self.next_history()
            elif ev.keysym == 'k':
                self.delete_to_line_end()
            elif ev.keysym == 'd':
                if self.currentline == '':
                    self.send_eof()
                else:
                    self.delete(ev)
            elif ev.keysym == 'a':
                self.cursor_home()
            elif ev.keysym == 'e':
                self.cursor_end()
            elif ev.keysym == 'c':
                self.send_cancel()
            return "break"

        if ev.char != '\t' and ev.char != '\n' and (ev.char < ' ' or ev.char > '~'):
            return None

        self.text.insert("insert", ev.char)
        #print self.currentline[:self.cursor_pos], "*",  ev.char, "*", self.currentline[self.cursor_pos:], "::: Cursor:",self.cursor_pos
        self.currentline = self.currentline[:self.cursor_pos] + ev.char + self.currentline[self.cursor_pos:]
        self.tab_pressed = False

        self.cursor_pos += 1
        self.input_modified = True
        self.look_at_end()
        return "break"

    # Track modifier key state.
    def ctldown(self, ev):
        self.ctl = 1
    def ctlup(self, ev):
        self.ctl = 0
    def altdown(self, ev):
        self.alt = 1
    def altup(self, ev):
        self.alt = 0
    def shiftdown(self, ev):
        self.shift = 1
    def shiftup(self, ev):
        self.shift = 0

    def clear(self):
        self.text.delete(0.0, "end")


    def quit(self):
        if self.running:
            self.stdout.stop()
            self.stdin.close()
            self.running = False
            
            
    def send_eof(self):
        if not self.running:
            return 
        print "ctrl-d"
        self.stdin.write('end_of_file.\n')
        self.stdin.flush()
        self.temphistory = self.history[:]
        self.temphistory.append("")
        self.currentline = ""
        self.historypos = 1
        self.cursor_pos = 0
        self.input_modified = False
        self.output_modified = False
        self.tab_pressed = False

    def send_cancel(self, ev):
        if not self.running:
            return  "break"    
        #print "ctrl-c"
        #print ev.char, ev.keycode, ev.keysym
        posix.kill(self.pid, signal.SIGINT)
        #self.stdin.write(chr(3))
        #self.stdin.write('\n')
        #self.stdin.flush()
        self.temphistory = self.history[:]
        self.temphistory.append("")
        self.currentline = ""
        self.historypos = 1
        self.cursor_pos = 0
        self.input_modified = False
        self.output_modified = False
        self.tab_pressed = False
        
    def send_line(self):
        if not self.running:
            return        
        self.stdin.write(self.currentline+"\n")
        self.stdin.flush()
        if self.currentline != "" and (self.history == [] or self.currentline != self.history[-1]):
            self.history.append(self.currentline)
        self.temphistory = self.history[:]
        self.temphistory.append("")
        self.currentline = ""
        self.historypos = 1
        self.cursor_pos = 0
        self.input_modified = False
        self.output_modified = False
        self.tab_pressed = False

    def append_text(self, line):
        self.text.insert("end", line)
        self.output_modified = True
        self.look_at_end()

    def look_at_end(self):
        self.text.yview(MOVETO, "1.0")

    def get_completions(self, word):
        if len(self.completions) == 0 or word.find('/') != -1:
            return ('', [])
        begin = bisect.bisect_left(self.completions, word)
        end = bisect.bisect_left(self.completions, word + chr(255), begin)
        if begin == end:
            return ('', [])
        temp = self.completions[begin:end]
        first = self.completions[begin]
        last = self.completions[end - 1]
        for i in xrange(0, min(len(first), len(last))):
            if first[i] != last[i]:
                break
        else:
            i += 1
        return (first[:i].split('/')[0], temp)

class MemListener(Thread):

    def __init__(self, stream, terminal):
        self.stream = stream
        self.fd = stream.fileno()
        Thread.__init__(self)
        self.mutex = Lock()
        self.out = ""
        self.terminal = terminal


    def run(self):
        self.alive=True
        self.out = ""
        while self.alive:
            self.mutex.acquire()
            
            fds = select([self.fd], [], [self.fd] , 0.1)[0]
                           
            if fds != [] and self.alive:
                line = os.read(self.fd, 1000)
                if line == "":
                    self.terminal.append_text("\n\nProlog Finished\n\n")
                    self.terminal.quit()
                    #self.alive = False

                self.out += line
                self.terminal.append_text(line)
                
            self.mutex.release()
            #time.sleep(0.05)

    def get_pending(self):
        self.mutex.acquire()
        out = self.out
        self.out = ""
        self.mutex.release()
        return out

    def stop(self):
        self.alive = False



class ControlFrame(Frame):
    def __init__(self, master=None, terminal=None):
        Frame.__init__(self,master, width="1in")

        self.terminal = terminal
        self.app = terminal.app
        self.bframe = Frame(self)
        
        #reset button
        self.reset_button = Button(self.bframe, text="Reset", command=self.terminal.reset)
        self.reset_button.pack(side="left",fill="x",expand="yes")

        #close button
        self.close_button = Button(self.bframe, text="Close", command=self.terminal.close)
        self.close_button.pack(side="right",fill="x",expand="yes")

        self.bframe.pack(side='bottom', fill='x')
        
        #load combo
        self.file_combo = Pmw.ComboBox(self,
                label_text = 'Load:',
                labelpos = 'nw',
                selectioncommand = self.load_selected,
                scrolledlist_items = ["Original File", "Specialised File", "Other..."],
                dropdown = 1,
        )
        #self.file_combo.component('entryfield').component('entry').configure(state="disabled")
        self.file_combo.pack(side="top",fill="y")
        
        

    def load_selected(self, selected):
        """
        An item has been selected from the load control
        """
        if selected == "Specialised File" and self.app.ann_frame.filename is not None:
            (root,_) = os.path.splitext(self.app.ann_frame.filename)
            filename = root+".spec"
            
        elif selected == "Original File" and self.app.ann_frame.filename is not None :
            filename = self.app.ann_frame.filename
            
        elif selected == "Other..." or  self.app.ann_frame.filename is None :
            filename = askopenfilename(filetypes=[("Prolog files","*.pl"),("All files","*")])
        else:
            return

        if filename is not None and filename != "":
            self.terminal.load_file(filename)
