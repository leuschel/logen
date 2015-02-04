import Pmw
import os
import re
from FastIndex import FastIndex, timer
from PrologFrame import PrologFrame
from TerminalFrame import TerminalFrame

class OutputBook(Pmw.NoteBook):

    def __init__(self, master=None):
        self.app = master
        Pmw.NoteBook.__init__(self, self.app.bottom,createcommand=self.create_page)
        self.spec_page = self.add('Specialised File')
        self.memo_page = self.add('Memo Table')
        self.gx_page = self.add('Generating Extension')
        self.output_page = self.add('Output')
        self.console_page = self.add('Console')


        #self.new_terminal_page = self.add('Terminal 1')
        

        #spec file
        self.output_spec = PrologFrame(self.spec_page,"",app=self.app)
        self.output_spec.pack(side="bottom", fill="both", expand="yes")

        #memo file
        self.output_memo = PrologFrame(self.memo_page,"",app=self.app)
        self.output_memo.pack(side="bottom", fill="both", expand="yes")

        #gx file
        self.output_gx = PrologFrame(self.gx_page,"",app=self.app)
        self.output_gx.pack(side="bottom", fill="both", expand="yes")

        #output
        self.output_out = PrologFrame(self.output_page,"",app=self.app)
        self.output_out.pack(side="bottom", fill="both", expand="yes")

        #console
        self.output_console = PrologFrame(self.console_page,"", app=self.app)
        self.output_console.pack(side="bottom", fill="both", expand="yes")
        
        self.pack(side="bottom", fill="both", expand="yes")

        self.output_spec.text.tag_bind("nametag", "<Motion>", self.mouse_over)
        self.output_spec.text.tag_bind("nametag", "<Leave>", self.mouse_leave)
        
        self.terminal_pages = []
        self.terminals = []
        self.term_count = 0

        self.create_new_terminal()

    def create_page(self, pagename):
        if pagename.startswith("Terminal"):
            i = self.terminal_pages.index(pagename)
            page = self.page(self.index(pagename))            
            self.terminals[i] = TerminalFrame(page, app=self.app, id=pagename)            
            self.terminals[i].pack(side="bottom", fill="both", expand="yes")                        
            self.app.update_completions()            
            #self.selectpage(pagename)
        
    def get_console_stream(self):
        pass

    def write_to_console(self,string):
        self.output_console.text.config(state="normal")
        self.output_console.text.insert("end", string)
        self.output_console.text.config(state="disabled")
    
    def set_font(self, font):
        self.output_spec.text["font"] = font
        self.output_memo.text["font"] = font
        self.output_gx.text["font"] = font
        self.output_out.text["font"] = font

    def view_spec_output(self, filename):
        (root, ext) = os.path.splitext(filename)
        fast_idx = self.output_spec.load_source(root + ".spec")
        self.output_memo.load_source(root + ".memo")
        self.output_gx.load_source(root + ".gx")
        self.output_out.clear()
        funcs = self.get_spec_funcs()
        spec_lines = self.output_spec.text.get(1.0, 'end')
        pos = 0
        while 1:
            id = next_id(spec_lines, pos)
            if id == ():
                break
            (start, end) = id
            pos = end
            (start_idx, end_idx) = fast_idx.get_two_tk_indices_same_line(start,
                                                                         end)
            self.output_spec.text.tag_add("nametag", start_idx, end_idx)

    def reset_output(self, gx=None):
        if gx is None:
            pass
            #self.output_gx.clear()
        elif os.path.exists(gx):
            self.output_gx.load_source(gx)
        else:
            self.output_gx.clear()
        self.output_memo.clear()
        self.output_spec.clear()
        self.output_out.clear()
        self.selectpage('Specialised File')

    def set_output_from_file(self, filename):
        self.output_out.load_source(filename)

    def set_output(self, text):
        self.output_out.set_text(text)

    def view_output(self):
        self.selectpage('Output')

    def get_spec_funcs(self):
        memo_text = self.output_memo.text.get(1.0, 'end').split('\n')
        funcs = {} 
        for line in memo_text:
            if line.startswith('table'):
                (orig, i) = up_to_comma(line[6:])
                (pattern, _) = up_to_comma(line[i + 7:])
                i = pattern.find('(')
                if i > 0:
                    name = pattern[:i]
                else:
                    name = pattern
                funcs[name] = orig + " --> " + pattern

        self.funcs = funcs
    
    def get_tag_position(self, x, y):
        index = self.output_spec.text.index("@"+str(x)+","+str(y)+" + 1 char")
        return self.output_spec.text.tag_prevrange("nametag", index)

    start = None

    def mouse_over(self, event):
        (start, end) = self.get_tag_position(event.x, event.y)
        predicate = self.output_spec.text.get(start, end)
        #print "over " + start + ", " + end + " : " + predicate
        #print self.funcs[predicate]
        if self.start != start:
            self.app.balloon.configure(relmouse="both",yoffset=15)
            self.app.balloon._showBalloon(self.output_spec.text,
                                          self.funcs[predicate], False)
            self.start = start

    def mouse_leave(self, event):
        self.app.balloon.configure(relmouse="none",yoffset=1)
        self.app.balloon.withdraw()
        self.start = None

    def create_new_terminal(self):
        self.term_count += 1
        self.terminal_pages.append('Terminal ' + str(self.term_count))       
        page = self.add(self.terminal_pages[-1])
        self.terminals.append(None)

                        
    def quit(self):
        for t in self.terminals:
            if t is not None:
                t.quit()

    def kill_terminal(self, term_str):
        i = self.terminal_pages.index(term_str)
        self.terminals[i].quit()
        self.delete(term_str)
        self.terminals.pop(i)
        self.terminal_pages.pop(i)

            
    
    def reset_completions(self):
        for t in self.terminals:
            if t is not None:
                t.reset_completions()

    def add_completions(self, completions):
        for t in self.terminals:
            if t is not None:
                t.add_completions(completions)

def up_to_comma(str):
    bracket_stack = []
    i = 0
    current_char = str[i]
    in_string = False
    in_double_string = False
    ret_string = ''

    while len(bracket_stack) > 0 or current_char != ',' or in_string or in_double_string:
        if current_char == '(' or current_char == '[' or current_char == '{':
            bracket_stack.append(current_char)
        elif current_char == ')' or current_char == ']' or current_char == '}':
            bracket_stack = bracket_stack[:-1]
        elif current_char == '"':
            if in_double_string:
                in_double_string = False
            elif not in_string:
                in_double_string = True
        elif current_char == "'":
            if in_string:
                in_string = False
            elif not in_double_string:
                in_string = True

        ret_string = ret_string + current_char
        i = i + 1
        current_char = str[i]

    return (ret_string.strip(), i)

regexp = re.compile('[a-zA-z0-9_]+__[0-9]+')

def next_id(string, pos):
    match = regexp.search(string[pos:])
    if match is None:
        return ()
    else:
        return (match.start() + pos, match.end() + pos)
