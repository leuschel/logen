

#sicstus --goal "set_prolog_flag(single_var_warnings,off), ensure_loaded('sicstus.pl'),ensure_loaded('socket.pl'),ensure_loaded('logen_main.pl'),save_program('logen_main.sav'),halt." 2>&1 | grep -v "^%" | grep -v "from clpq is private"

import os
import development

class PrologSav:
    def __init__(self,name,  depends, buildcmd):
        self.name = name
        self.depends = depends
        self.buildcmd = buildcmd

    def uptodate(self):
        if not os.path.exists(self.name):
            return False
        save_state = os.stat(self.name).st_mtime
        
        for file in self.depends:
            time_file = os.stat(file).st_mtime
            if time_file > save_state:
                return False
        return True

    def write_to_splash(self,splash=None,text=""):
        if splash != None:
            splash.text.insert("end",text)
            splash.update()
        else:
            print text

    def refresh(self,splash=None):
        self.write_to_splash(splash,"\n\nChecking %s..." % self.name)
        if not self.uptodate():
            self.write_to_splash(splash,"Out of date!\nRebuilding save state:"            )
            self.build() 
            self.write_to_splash(splash,"\n%s Rebuilt" % self.name)           
        else:
            self.write_to_splash(splash,"OK")                       
            print self.name, "is is uptodate"
            

    def build(self):
        os.system("%s %s" %(self.buildcmd,"2>&1 | grep -v \"^%\" | grep -v \"from clpq is private\" | grep -v \"^SICStus 3\" | grep -v \"$Licencensed to \"") )
        #print self.buildcmd
        #os.system(self.buildcmd)
        print "Rebuilt %s" % (self.name)
        


class Logen_Saves:
    def __init__(self):
        logen_depends =  ['ann_db.pl',
                 'annfile.pl',
                 'bta.pl',
                 'builtin_db.pl',
                 'cogen-tools.pl',
                 'cogen.pl',
                 'flags.pl',
                 'gensym.pl',
                 'logen_annotation.pl',
                 'logen_attributes.pl',
                 'logen_benchmark.pl',
                          'logen_benchmark_loop.pl',
                 'logen_codesize.pl',        
                 'logen_filter.pl',
                 'logen_main.pl',
                 'logen_messages.pl',
                 'logen_post.pl',
                          'pp/inline.pl',
                          'pp/saver.pl',
                          'pp/deadcode.pl',
                          'pp/loader.pl',                          
                 'memoizer.pl',
                 'moduledriver.pl',
                 'pylogen_main.pl',
                 'pp.pl',
                 'run_gx.pl',
                 'sicstus.pl',
                 'sicstus_term.pl',
                 'socket.pl',
                 'annotation/match_ann.pl',
                 'annotation/match_unknowns.pl',
                 'annotation/save_ann.pl',
                 'annotation/tokens.pl',
                 'annotation/parser.pl',
                 'annotation/read.pl',
                 'prob/logen_preferences.pl']
        
        self.logen=PrologSav('pylogen_main.sav',
                  logen_depends,
                  "sicstus --goal \"set_prolog_flag(single_var_warnings,off), ensure_loaded('pylogen_main.pl'),save_program('pylogen_main.sav'),halt.\" ")
		# spld --static --output pylogen_main --resources=logen_main.sav=/pylogen_main.sav


        bta_depends = [
            'auto_bta.pl',
            'bta_driver.pl',
            'convex_analyser.pl',
            'convex_hull.pl',
            'convex_norm.pl',
            'builtin_norms.pl',
            'filter_prop/auto_bta.pl',
            'filter_prop/filters.pl',
            'filter_prop/logen/filter.pl',
            'filter_prop/abstractBuiltins.pl',
            'filter_prop/abstractCallSucc.pl',
            'filter_prop/logen/logen_create_types.pl',
            'filter_prop/readprog.pl',
            'run_binsolve.pl',
            'run_bta.pl',
            'bta_files/bin_loader.pl',
            'bta_files/bta_pp.pl',
            'bta_files/pp_annfile.pl',
            'bta_files/pp_iftransform.pl',

            
            
            ]
        
        bta_depends.extend(logen_depends)
        
        self.localbta=PrologSav('run_bta.sav',
                                bta_depends,                                
                                #"sicstus --goal \"set_prolog_flag(single_var_warnings,off),prolog_flag(compiling,_,profiledcode),ensure_loaded('run_bta.pl'),save,halt.\"  2>&1| grep -v \"^%\" | grep -v \"from clpq is private\""
                                "sicstus --goal \"set_prolog_flag(single_var_warnings,off),ensure_loaded('run_bta.pl'),save,halt.\"  2>&1| grep -v \"^%\" | grep -v \"from clpq is private\""
                                )

        #self.localbta_profile =PrologSav('bta.sav',
        #                      bta_depends,
        #                        "sicstus --goal \"set_prolog_flag(single_var_warnings,off),prolog_flag(compiling,_,profiledcode),ensure_loaded('auto_bta.pl'),compile_bta,halt.\"  2>&1| grep -v \"^%\" | grep -v \"from clpq is private\""
       #                         )
                              
    def refresh(self,splash=None):
        if development.get_development():
            self.logen.refresh(splash=splash)
            self.localbta.refresh(splash=splash)
        else:
            print "Using precompiled binaries"
                

if __name__=="__main__":
    print "Manual Make and Check to build sav states"
    development.set_development_mode(True)
    l = Logen_Saves()
    l.refresh()
                
                 
        
        
