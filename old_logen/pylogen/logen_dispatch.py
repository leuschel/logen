import os
import development


def get_pylogen_cmd():
    if development.get_development():
        cmdline = "sicstus -r pylogen_main.sav --goal \"runtime_entry(start),halt.\" -a"
    else:
        cmdline = os.path.join(".","pylogen")
        
    return cmdline





def build_dispatcher(path,Spec,Memo,File,Output):
    cmd = get_pylogen_cmd()
    command = "%s -dispatch '%s' '%s' '%s' '%s' '%s'" % (cmd, path,Spec,Memo,File,Output)
    print command

    os.system(command)

    
