from Tkinter import *
from tkSimpleDialog import Dialog
from AnnotationColours import AnnotationColours

class PrefDialog(Dialog):
    def __init__(self, master=None, app=None):
        self.app = app
        Dialog.__init__(self, master)

    def body(self, master):
        self.lf = LabelFrame(master, text="List of Preferences", padx=4, pady=4)
        self.lf.pack({"expand":"yes", "fill":"both"})
        
        Label(self.lf, text="Default Debug Level").grid(row=0, sticky=E)
        self.debug_entry = Entry(self.lf)
        self.debug_entry.insert(0, self.app.logen.preferences["gx_debug_mode"])
        self.debug_entry.grid(row=0,column=1)

        Label(self.lf,
              text="Default Examples Directory").grid(row=1, pady=4, sticky=E)
        self.examples_entry = Entry(self.lf)
        self.examples_entry.insert(0,
                                self.app.pref.get_preference("examples_path"))
        self.examples_entry.grid(row=1,column=1)

        Label(self.lf, text="Web browser").grid(row=2, pady=4, sticky=E)
        self.browser_entry = Entry(self.lf)
        browser = self.app.pref.get_preference("browser")
        if browser is None:
            browser = ""
        self.browser_entry.insert(0, browser)
        self.browser_entry.grid(row=2,column=1)

        Label(self.lf, text="Annotation Colours").grid(row=3, pady=4, sticky=E)
        self.colour_button = Button(master=self.lf,
                                    text="Change Colours",
                                    command=self.change_colours)
        self.colour_button.grid(row=3,column=1)
        
        return self.debug_entry

    def change_colours(self):
        #app.ann_frame.sourceFrame.choose_colours()
        AnnotationColours(colours=self.app.colours)
        
    def apply(self):
        self.app.pref.set_preference("examples_path", self.examples_entry.get())
        self.app.pref.set_preference("browser", self.browser_entry.get())
        self.app.logen.preferences["gx_debug_mode"] = self.debug_entry.get()
