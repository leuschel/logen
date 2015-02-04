"""Utilities to perform various operations on paths, with support for
opening the ROX filer to examine locations if things go wrong.

Typical usage:

	import rox.fileutils
	rox.fileutils.makedirs('/path/for/new/directory')
"""

import os
from rox import _

def report_patherror(message, path):
	"""Display a <Cancel>/<Retry>/<Examine> dialog.
	This will raise an OSError exception if the user selects Cancel, or
	will return successfully if the user chooses to retry."""
	from rox import g, filer, toplevel_ref, toplevel_unref, ButtonMixed
	toplevel_ref()
	box = g.MessageDialog(None, 0, g.MESSAGE_QUESTION,
				g.BUTTONS_CANCEL, message)

	button = ButtonMixed(g.STOCK_REDO, _('Retry'))
	button.set_flags(g.CAN_DEFAULT)
	button.show()
	box.add_action_widget(button, g.RESPONSE_OK)

	button = ButtonMixed(g.STOCK_JUMP_TO, _('Examine'))
	button.set_flags(g.CAN_DEFAULT)
	button.show()
	box.add_action_widget(button, g.RESPONSE_APPLY)

	box.set_position(g.WIN_POS_CENTER)
	box.set_title(_('Error:'))
	box.set_default_response(g.RESPONSE_APPLY)
	while 1:
		resp = box.run()
		if resp != g.RESPONSE_APPLY: break
		filerpath = os.path.normpath(path)
		filer.show_file(filerpath)
	box.destroy()
	toplevel_unref()
	if resp != g.RESPONSE_OK:
		raise OSError, message

def _makedirs_recursive(path, mode):
	"""Recursive part of makedirs. Calls itself to ensure head
	of path exists, and then makes a new directory at path. Returns
	an Exception if it can't make a directory at path."""
	if os.path.isdir(path): return
	head, tail = os.path.split(path)
	if not tail:
		head, tail = os.path.split(head)
	if head and tail:
		_makedirs_recursive(head, mode)

	while True:
		if os.path.exists(path):
			report_patherror( \
				_("Could not create directory `%s' because a file already exists at that path.\n") % path, path)
			continue
		try:
			os.mkdir(path, mode)
			return
		except OSError, msg:
			report_patherror(("%s.\n" + _("Could not create directory `%s'.\n")) \
				% (msg[1], path), path)

def makedirs(path, mode=0777):
	"""Make a directory at the specified path, creating intermediate
	directories if necessary. No error if 'path' is already a directory.

	On error, a dialog which allows the user to open the filer to fix
	things and then retry will be opened.

	Returns successfully if all directories get created (or already exist),
	Raises an OSError if there is a problem, in which case the application
	should not open a dialog box to inform the user, since one will already
	have been displayed.
	"""
	# Get rid of any ..'s in the path
	path = os.path.normpath(path)
	_makedirs_recursive(path, mode)

