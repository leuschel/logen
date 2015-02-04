"""Provide a standard window for displaying application information."""

import os

import rox
from rox import g
import webbrowser

class InfoWin(g.Dialog):
    """Window to show app info"""
    def __init__(self, program, purpose, version, author, website):
        g.Dialog.__init__(self)
        self.website=website

        def close(iw, event=None, data=None):
            iw.hide()

        self.connect("delete_event", close)

        hbox=g.HBox()
        self.vbox.pack_start(hbox)
        hbox.show()

        try:
            path=os.path.join(rox.app_dir, '.DirIcon')
            pixbuf=g.gdk.pixbuf_new_from_file(path)
            icon=g.Image()
            icon.set_from_pixbuf(pixbuf)
            hbox.pack_start(icon)
            icon.show()
        except:
            #rox.report_exception()
            pass

        table=g.Table(5, 2)
        hbox.pack_start(table)

        label=g.Label("Program")
        table.attach(label, 0, 1, 0, 1)

        frame=g.Frame()
        frame.set_shadow_type(g.SHADOW_IN)
        table.attach(frame, 1, 2, 0, 1)

        label=g.Label(program or '')
        frame.add(label)

        label=g.Label("Purpose")
        table.attach(label, 0, 1, 1, 2)

        frame=g.Frame()
        frame.set_shadow_type(g.SHADOW_IN)
        table.attach(frame, 1, 2, 1, 2)

        label=g.Label(purpose or '')
        frame.add(label)

        label=g.Label("Version")
        table.attach(label, 0, 1, 2, 3)

        frame=g.Frame()
        frame.set_shadow_type(g.SHADOW_IN)
        table.attach(frame, 1, 2, 2, 3)

        label=g.Label(version or '')
        frame.add(label)

        label=g.Label("Authors")
        table.attach(label, 0, 1, 3, 4)

        frame=g.Frame()
        frame.set_shadow_type(g.SHADOW_IN)
        table.attach(frame, 1, 2, 3, 4)

        label=g.Label(author or '')
        frame.add(label)

        label=g.Label("Web site")
        table.attach(label, 0, 1, 5, 6)

        if website:
            button=g.Button(website)
            table.attach(button, 1, 2, 5, 6)

            def goto_website(widget, iw):
                webbrowser.open(iw.website)

            button.connect("clicked", goto_website, self)
            
        else:
            frame=g.Frame()
            frame.set_shadow_type(g.SHADOW_IN)
            table.attach(frame, 1, 2, 5, 6)

        hbox=self.action_area

        button=g.Button(stock=g.STOCK_CLOSE)
        hbox.pack_start(button)

        def dismiss(widget, iw):
            iw.hide()

        button.connect("clicked", dismiss, self)
        button.show()

        self.vbox.show_all()

import rox.AppInfo

def infowin(pname, info=None):
    """Open info window for this program.  info is a source of the
    AppInfo.xml file, if None then $APP_DIR/AppInfo.xml is loaded instead"""

    if info is None:
        info=os.path.join(rox.app_dir, 'AppInfo.xml')

    try:
        app_info=rox.AppInfo.AppInfo(info)
    except:
        rox.report_exception()
        return

    try:
        iw=InfoWin(pname, app_info.getAbout('Purpose')[1],
                   app_info.getAbout('Version')[1],
                   app_info.getAuthors(),
                   app_info.getAbout('Homepage')[1])
        iw.show()
        return iw
    except:
        rox.report_exception()
