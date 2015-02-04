from Tkinter import *
from tkSimpleDialog import Dialog
from tkColorChooser import askcolor
import os

syntax = ['comment', 'head', 'list']

filters = ['filter', 'dynamic', 'static']

annotations = ['unfold', 'memo', 'call', 'rescall', 'semicall', 'if', 'resif',
               'semif', 'logif', 'reslogif', 'mcall', 'ucall', 'unknown',
               'findall', 'resfindall', 'resnot', 'not', ';', 'resdisj',
               'pp_cll', 'pp_mnf', 'mnf', 'reswhen', 'when', 'semiwhen',
               'online', 'time_out', 'gxspec', 'gx']

syn_colour = {'comment':'hotpink1', 'head':'blue', 'list':'purple'}

fil_colour = {'filter':'darkblue', 'dynamic':'darkred', 'static':'darkgreen'}

ann_colour = {'unfold':'darkgreen', 'ucall':'darkgreen', 'call':'darkgreen',
              'if':'darkgreen', 'logif':'darkgreen', 'rescall':'darkred',
              'semicall':'darkblue', 'mcall':'darkred', 'memo':'darkred',
              'resif':'darkred', 'semif':'darkblue', 'reslogif':'darkred',
              'unknown':'blue', 'findall':'darkgreen', 'resfindall':'darkred',
              'resnot':'darkred', 'not':'darkgreen', ';':'darkgreen',
              'resdisj':'darkred', 'mnf':'darkblue', 'pp_cll':'darkblue',
              'pp_mnf':'darkblue', 'when':'darkgreen', 'semiwhen':'darkblue',
              'reswhen':'darkred', 'online':'darkblue', 'time_out':'darkred',
              'gxspec':'darkred', 'gx':'darkgreen'}

if os.sys.platform == 'darwin':
    using_mac = True
else:
    using_mac = False

class ColourFrame(LabelFrame):
    def __init__(self, master=None, text=None, anns=None, map=None, rows=None, cols=None):
        LabelFrame.__init__(self, master, text=text, padx=4, pady=4)
        self.anns = anns
        self.map = map
        self.buttons = []
        k = 0
        r = 0
        if rows is None:
            if cols is None:
                cols = 3
            rows = (len(anns) - 1) / cols
            
        self.buttons = []
        for i in xrange(0, len(self.anns)):
            colour = self.map[self.anns[i]]
            Label(self, text=self.anns[i]).grid(column=k,row=r, sticky=E)
            
            if using_mac:
                b = Canvas(self, width=20, height=20, background=colour)
                b.bind("<Button-1>", lambda ev, j=i : self.set_colour(ev, j))
            else:
                b = Button(self, background=colour, command=lambda j=i : self.set_colour(j))

            b.grid(column=k+1,row=r, sticky=E)
            r += 1
            if r > rows:
                r = 0
                k = k + 2
            self.buttons.append(b)

    def set_colour(self, ev, row):
        print "row = ",row
        (_, new_colour) = askcolor(initialcolor=self.map[self.anns[row]])
        self.map[self.anns[row]] = str(new_colour)
        self.buttons[row]['background'] = new_colour

    def update(self, new_map):
        self.map = new_map
        for i in xrange(0, len(self.anns)):
            colour = self.map[self.anns[i]]
            self.buttons[i]['background'] = colour

class AnnotationColours(Dialog):
    def __init__(self, master=None, colours=None):
        self.colours = colours
        Dialog.__init__(self, master)

    def body(self, master):
        self.af = ColourFrame(master,
                              text='Annotation Colours', anns=annotations,
                              map=self.colours.ann_colour.copy())
        self.af.pack({'expand':'yes', 'fill':'both'})

        self.sf = ColourFrame(master,
                              text='Syntax Colours', anns=syntax,
                              map=self.colours.syn_colour.copy())
        self.sf.pack({'expand':'yes', 'fill':'both'})

        self.ff = ColourFrame(master,
                              text='Filter Colours', anns=filters,
                              map=self.colours.fil_colour.copy())
        self.ff.pack({'expand':'yes', 'fill':'both'})

    def buttonbox(self):
        box = Frame(self)

        Button(box, text='Use Defaults', width=10,
               command=self.use_defaults).pack(side=LEFT, padx=5, pady=5)
        Button(box, text='Cancel', width=10,
               command=self.cancel).pack(side=LEFT, padx=5, pady=5)
        Button(box, text='Ok', width=10, default=ACTIVE,
               command=self.ok).pack(side=LEFT, padx=5, pady=5)

        box.pack()

    def apply(self):
        self.colours.set_new_maps(self.af.map, self.sf.map, self.ff.map)

    def use_defaults(self):
        self.af.update(ann_colour.copy())
        self.sf.update(syn_colour.copy())
        self.ff.update(fil_colour.copy())

class ColourPreferences:
    def __init__(self, pref=None):
        self.pref = pref
        self.load_colours()
        
    def set_defaults(self):
        self.syn_colour = syn_colour.copy()
        self.ann_colour = ann_colour.copy()
        self.fil_colour = fil_colour.copy()

    def load_colours(self):
        self.set_defaults()
        psyn_colour = self.pref.get_preference('syntax highlights')
        pann_colour = self.pref.get_preference('annotation highlights')
        pfil_colour = self.pref.get_preference('filter highlights')

        set_undefined_colours(self.syn_colour, psyn_colour)
        set_undefined_colours(self.ann_colour, pann_colour)
        set_undefined_colours(self.fil_colour, pfil_colour)

        self.pref.set_preference('annotation highlights', self.ann_colour)
        self.pref.set_preference('syntax highlights', self.syn_colour)
        self.pref.set_preference('filter highlights', self.fil_colour)

    def set_new_maps(self, af, sf, ff):
        self.ann_colour = af
        self.syn_colour = sf
        self.fil_colour = ff
        
        self.pref.set_preference('annotation highlights', af)
        self.pref.set_preference('syntax highlights', sf)
        self.pref.set_preference('filter highlights', ff)

def set_undefined_colours(main, pref):
    if pref is not None:
        for c in pref:
            main[c] = pref[c]
