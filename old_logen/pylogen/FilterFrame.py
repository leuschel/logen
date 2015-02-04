from PrologFrame import PrologFrame
import os

if (os.sys.platform == "win32"):
    Menu_Key = "<Button-3>"
else:
    Menu_Key = "<Button-1>"

class FilterFrame(PrologFrame):
    def __init__(self,master=None, readonly=True,app=None):
        PrologFrame.__init__(self, master=master, text="", app=app,
                             readonly=readonly)
        self.app = app

        app.pref.register_listener('filter highlights', self.colours_changed)
        self.colours_changed(app.colours.fil_colour)

        for fTag in ['dynamic', 'static']:
            self.text.tag_bind(fTag, "<Motion>", 
                    lambda _, fTag=fTag: self.mouse_over_filter_tag(fTag))
            self.text.tag_bind(fTag, "<Leave>",
                    lambda _, fTag=fTag: self.mouse_leave_filter_tag(fTag))
            # remove because it's knackered
            #self.text.tag_bind(fTag, Menu_Key,
            #        lambda e, ftag=fTag: self.mouse_click_filter_tag(e, fTag))

    def mouse_over_filter_tag(self, filterTag):
        self.app.status.set("Argument annotated as %s" % filterTag)

    def mouse_leave_filter_tag(self, unused):
        self.app.status.clear()

    def mouse_click_filter_tag(self, event=None, tag=None):
        print "Menu showing"
        index = self.text.index("@" + str(event.x) + "," + str(event.y))
        fil = self.text.tag_names(index)[0]
        (start, end) = self.get_tag_position(fil, index)
        # we can now cycle through annotations?

        if fil == "dynamic":
            newfil = "static"
        else:
            newfil = "dynamic"

        print fil, start, end, newfil
        self.text.delete(start, end)
        self.text.insert(start, newfil)
        self.text.tag_add(newfil, start,"%s+%dc" % (start, len(newfil)))

