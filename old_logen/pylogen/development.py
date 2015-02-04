


development = False
cogen_version = 'cogen2'
cogen3_watched = False

def set_development_mode(val):
    global development
    development = val

def get_development():
    global development
    return development

def set_cogen_version(val):
    global cogen_version
    cogen_version = val

def get_cogen_version():
    global cogen_version
    return cogen_version

def set_watched(val):
    global cogen3_watched
    cogen3_watched = val

def get_watched():
    global cogen3_watched
    return cogen3_watched
