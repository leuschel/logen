import os
import sys

class Browser:
    """ Runs the users browser and displays the page
        for html previewing
    """
    
    def __init__(self, pref):
        self.pref = pref

        browser_cmd = pref.get_preference('browser')
        if browser_cmd is None or browser_cmd == '':
            if sys.platform == "win32":
                pref.set_preference('browser', 'explorer')

    def openpage(self, page):
        browser_cmd = self.pref.get_preference('browser')
        if browser_cmd is None or browser_cmd == '':
            print "Please specify a browser for preview to autoopen exported html"
            return False

        if sys.platform == "win32":
            page = page.replace("/", "\\")
            
        cmd = ('%s "%s"' % (browser_cmd , page))
        os.system(cmd)
        return True
        


if __name__ == "__main__":
    print "Testing Browser:"
    b = Browser();
    b.openpage("C:\Documents and Settings\Steve\Desktop\print.html")
