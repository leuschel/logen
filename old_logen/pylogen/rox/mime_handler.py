"""This module allows applications to set themselves as the default handler for
a particular MIME type. This is generally not a good thing to do, because it
annoys users if programs fight over the defaults."""

import os

import rox
import rox.choices
from rox import _, mime

_TNAME = 0
_COMMENT = 1
_CURRENT = 2
_INSTALL = 3
_ICON = 4

class InstallList(rox.Dialog):
    """Dialog to select installation of MIME type handlers"""
    def __init__(self, application, itype, dir, types, info=None, check=True):
        """Create the install list dialog.
	application - path to application to install
	itype - string describing the type of action to install
	dir - directory in Choices to store links in
	types - list of MIME types
	info - optional message to display below list
	check - if true (the default), check for existing entries"""
        rox.Dialog.__init__(self, title='Install %s' % itype,
                            buttons=(rox.g.STOCK_CANCEL, rox.g.RESPONSE_CLOSE,
                                     rox.g.STOCK_OK, rox.g.RESPONSE_ACCEPT))

        self.itype=itype
        self.dir=dir
        self.types=types
	self.app=application
	self.aname=os.path.basename(application)
	self.check=check

        vbox=self.vbox

        swin = rox.g.ScrolledWindow()
        swin.set_size_request(-1, 160)
        swin.set_border_width(4)
        swin.set_policy(rox.g.POLICY_NEVER, rox.g.POLICY_ALWAYS)
        swin.set_shadow_type(rox.g.SHADOW_IN)
        vbox.pack_start(swin, True, True, 0)

        self.model = rox.g.ListStore(str, str, str, int, rox.g.gdk.Pixbuf)
        view = rox.g.TreeView(self.model)
        self.view = view
        swin.add(view)
        view.set_search_column(1)

        cell = rox.g.CellRendererPixbuf()
        column = rox.g.TreeViewColumn('', cell, pixbuf = _ICON)
        view.append_column(column)
        
        cell = rox.g.CellRendererText()
        column = rox.g.TreeViewColumn('Type', cell, text = _TNAME)
        view.append_column(column)
        column.set_sort_column_id(_TNAME)
        
        cell = rox.g.CellRendererText()
        column = rox.g.TreeViewColumn('Name', cell, text = _COMMENT)
        view.append_column(column)
        column.set_sort_column_id(_COMMENT)

        if check:
            cell = rox.g.CellRendererText()
            column = rox.g.TreeViewColumn('Current', cell, text = _CURRENT)
            view.append_column(column)
            column.set_sort_column_id(_CURRENT)

        cell = rox.g.CellRendererToggle()
        cell.set_property('activatable', True)
        cell.connect('toggled', self.toggled, self.model)
        column = rox.g.TreeViewColumn('Install?', cell, active = _INSTALL)
        view.append_column(column)
        column.set_sort_column_id(_INSTALL)

        view.get_selection().set_mode(rox.g.SELECTION_NONE)

	if info:
		hbox=rox.g.HBox(spacing=4)
		img=rox.g.image_new_from_stock(rox.g.STOCK_DIALOG_INFO,
					       rox.g.ICON_SIZE_DIALOG)
		hbox.pack_start(img)

		lbl=rox.g.Label(info)
		lbl.set_line_wrap(True)
		hbox.pack_start(lbl)

		vbox.pack_start(hbox)

        vbox.show_all()
        
        self.load_types()

    def toggled(self, cell, path, model):
	"""Handle the CellRedererToggle stuff"""    
	if type(path) == str:
		# Does this vary by pygtk version?
		iter=model.iter_nth_child(None, int(path))
	else:
		iter=model.get_iter(path)
        model.set_value(iter, _INSTALL, not cell.get_active())

    def load_types(self):
	"""Load list of types into window"""    
        self.model.clear()

        for tname in self.types:
            type=rox.mime.lookup(tname)
	    if self.check:
		    old=rox.choices.load(self.dir, '%s_%s' %
					 (type.media, type.subtype))
		    if old and os.path.islink(old):
			    old=os.readlink(old)
			    oname=os.path.basename(old)
		    elif old:
			    oname='script'
		    else:
			    oname=''

		    if old==self.app:
			    dinstall=False
		    else:
			    dinstall=True
	    else:
		    dinstall=True
		    oname=''
		    
	    icon=type.get_icon(rox.mime.ICON_SIZE_SMALL)

            iter=self.model.append()
            self.model.set(iter, _TNAME, tname, _COMMENT, type.get_comment(),
			   _INSTALL, dinstall)
	    if self.check:
		    self.model.set(iter, _CURRENT, oname)
	    if icon:
		    self.model.set(iter, _ICON, icon)


    def get_active(self):
	"""Return list of selected types"""    
        iter=self.model.get_iter_first()
        active=[]
        while iter:
            if self.model.get_value(iter, _INSTALL):
                active.append(self.model.get_value(iter, _TNAME))
            iter=self.model.iter_next(iter)

        return active

def _install_type_handler(types, dir, desc, application=None, overwrite=True,
                          info=None):
	if len(types)<1:
		return
	
	if not application:
		application=rox.app_dir
	if application[0]!='/':
		application=os.path.abspath(application)
		
	win=InstallList(application, desc, dir, types, info)

	if win.run()!=rox.g.RESPONSE_ACCEPT:
		win.destroy()
		return
	
	types=win.get_active()

	for tname in types:
		type = mime.lookup(tname)

		sname=rox.choices.save(dir,
					  '%s_%s' % (type.media, type.subtype))
		os.symlink(application, sname+'.tmp')
		os.rename(sname+'.tmp', sname)

	win.destroy()

run_action_msg=_("""Run actions can be changed by selecting a file of the appropriate type in the Filer and selecting the menu option 'Set Run Action...'""")
def install_run_action(types, application=None, overwrite=True):
	"""Install application as the run action for 1 or more types.
	application should be the full path to the AppDir.
	If application is None then it is the running program which will
	be installed.  If overwrite is False then existing run actions will
	not be changed.  The user is asked to confirm the setting for each
	type."""
	_install_type_handler(types, "MIME-types", _("run action"),
			     application, overwrite, run_action_msg)

def install_thumbnailer(types, application=None, overwrite=True):
	"""Install application as the thumbnail handler for 1 or more types.
	application should be the full path to the AppDir.
	If application is None then it is the running program which will
	be installed.  If overwrite is False then existing thumbnailerss will
	not be changed.  The user is asked to confirm the setting for each
	type."""
	_install_type_handler(types, "MIME-thumb", _("thumbnail handler"),
			     application, overwrite, _("""Thumbnail handlers provide support for creating thumbnail images of types of file.  The filer can generate thumbnails for most types of image (JPEG, PNG, etc.) but relies on helper applications for the others."""))

def install_send_to_types(types, application=None):
	"""Install application in the SendTo menu for 1 or more types.
	application should be the full path to the AppDir.
	If application is None then it is the running program which will
	be installed.  The user is asked to confirm the setting for each
	type."""
	if len(types)<1:
		return
	
	if not application:
		application=rox.app_dir
	if application[0]!='/':
		application=os.path.abspath(application)
		
	win=InstallList(application, _('type handler'), 'SendTo', types,
			_("""The application can handle files of these types.  Click on OK to add it to the SendTo menu for the type of file, and also the customized File menu."""),
			check=False)

	if win.run()!=rox.g.RESPONSE_ACCEPT:
		win.destroy()
		return
	
	types=win.get_active()

	for tname in types:
		type=lookup(tname)
		
		sname=rox.choices.save('SendTo/.%s_%s' %  (type.media,
							    type.subtype),
					  win.aname)
		os.symlink(application, sname+'.tmp')
		os.rename(sname+'.tmp', sname)
	
	win.destroy()
	
def install_from_appinfo(appdir = rox.app_dir):
	"""Read the AppInfo file from the AppDir and perform the installations
	indicated. The elements to use are <CanThumbnail> and <CanRun>, each containing
	a number of <MimeType type='...'/> elements.
	appdir - Path to application (defaults to current app)
	"""
	import rox.AppInfo

	app_info_path = os.path.join(appdir, 'AppInfo.xml')
	ainfo = rox.AppInfo.AppInfo(app_info_path)

	can_run = ainfo.getCanRun()
	can_thumbnail = ainfo.getCanThumbnail()
	if can_run or can_thumbnail:
		install_run_action(can_run, appdir)
		install_thumbnailer(can_thumbnail, appdir)
                install_send_to_types(can_run, appdir)
	else:
		raise Exception('Internal error: No actions found in %s. '
				'Check your namespaces!' % app_info_path)
