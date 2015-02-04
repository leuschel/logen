from Tkinter import *

import Pmw
                    
class SpecBar(Frame):
    def __init__(self, master=None, app=None):
        Frame.__init__(self, master, height="1cm")
        self.app = app
        
        self.specLabel = Button(self,text="Goal:",relief="flat")
        self.specLabel.bind("<Button-1>",self.goal)
                        
        #### Use a combo box to save history as well
        self.specEntry = Pmw.ComboBox(self)
        self.goalList = self.specEntry._list.component('listbox')
        self.goalEntry = self.specEntry.component('entry')
        self.goalButton = self.specEntry.component('arrowbutton')

        self.goalEntry.bind('<Return>', self.specialise)
        
        self.Spec = Button(self, text="Specialise", command=self.specialise)
        self.app.balloon.bind(self.Spec, 'Specialise the program')
        
        self.Cogen = Button(self, text="Run Cogen", command=self.app.runcogen)
        self.Run = Button(self, text="Run", command=self.app.run)
        self.Clear = Button(self, text="Clear", command=self.app.clear)
        self.FilterProp = Button(self,text="Filter Prop", command=self.app.filter_prop)
        self.AutoBta = Button(self,text="Safety", command=self.app.safety_check)
        self.AutoBtaLoop = Button(self,text="Auto Bta", command=self.app.auto_bta)
        self.GlobalBta = Button(self,text="Global", command=self.app.global_bta)                
        #self.specEntry.insert(0,app.logen.preferences["last_spec_query"])
        
        #put them all in the bar
        self.specLabel.pack(side="left", padx=2, pady=5)
        self.specEntry.pack(side="left", fill="x", expand="yes")
        self.Run.pack(side="left", padx=2, pady=5)

        #self.FilterProp.pack(side="left",padx=2,pady=5)
        #self.AutoBta.pack(side="left",padx=2,pady=5)
        
        #self.AutoBtaLoop.pack(side="left",padx=2,pady=5)
        
        #self.GlobalBta.pack(side="left", padx=2, pady=5)
        self.Spec.pack(side="left", padx=2, pady=5)
        #self.Cogen.pack(side="left", padx=2, pady=5)
        #self.Clear.pack(side="left", padx=2, pady=5)
        self.menu = Menu(self.app, tearoff=0)

    def specialise(self,unused=None):
        """ Save the goal and call specialise
        """
        self.specEntry._addHistory()
        goal =  self.goalEntry.get()        
        return self.app.specialise(goal=goal)
        
        
        
        
    def set_enabled(self,state):
        self.goalEntry["state"] = state
        self.goalList["state"] = state
        self.goalButton["state"] = state
        
        self.Spec["state"] = state
        self.Cogen["state"] = state
        self.Clear["state"] = state
        self.Run["state"] = state
        self.FilterProp["state"] = state
        self.AutoBta["state"] = state
        self.AutoBtaLoop["state"] = state
        self.GlobalBta["state"] = state


    def set_goal(self,text):
        #self.specEntry.delete(0,"end")
        #self.specEntry.insert(0,text)
        self.goalEntry.delete(0,"end")
        self.goalEntry.insert(0,text)        
        

    def load_goals(self,goals):
        self.specEntry.clear()
        if goals is not None:
            for goal in goals:
                self.goalList.insert(0,goal)
            if len(goals) > 0:                
                self.set_goal(goals[0])
                

        
    def goal(self,event=None):
        """
        The Goal Button has been clicked show the goals by
        displaying the drop down combo
        """
        self.specEntry.invoke()
        
