"""This module provides features to help with debugging ROX applications."""

import sys, os
import traceback
import gobject
import linecache

from rox import g, ButtonMixed, toplevel_ref, toplevel_unref, _
from rox import info, alert

savebox = None

def show_exception(type, value, tb, auto_details = False):
	"""Display this exception in an error box. The user has the options
	of ignoring the error, quitting the application and examining the
	exception in more detail. See also rox.report_exception()."""

	QUIT = 1
	DETAILS = 2
	SAVE = 3
	
	brief = ''.join(traceback.format_exception_only(type, value))

	toplevel_ref()
	box = g.MessageDialog(None, 0, g.MESSAGE_ERROR, g.BUTTONS_NONE, brief)
	
	if not auto_details:
		button = ButtonMixed(g.STOCK_ZOOM_IN, _('_Details'))
		button.set_flags(g.CAN_DEFAULT)
		button.show()
		box.add_action_widget(button, DETAILS)

	box.add_button(g.STOCK_OK, g.RESPONSE_OK)
	box.set_default_response(g.RESPONSE_OK)

	box.set_position(g.WIN_POS_CENTER)
	box.set_title(_('Error'))
	reply = []
	def response(box, resp):
		reply.append(resp)
		g.mainquit()
	box.connect('response', response)
	box.show()

	bug_report = 'Traceback (most recent call last):\n' + \
		     ''.join(traceback.format_stack(tb.tb_frame.f_back) +
			     traceback.format_tb(tb) +
			     traceback.format_exception_only(type, value))

	while 1:
		if auto_details:
			resp = DETAILS
			auto_details = False
		else:
			g.mainloop()
			resp = reply.pop()
		if resp == g.RESPONSE_OK or resp == g.RESPONSE_DELETE_EVENT:
			break
		if resp == SAVE:
			global savebox
			if savebox:
				savebox.destroy()
			def destroy(box):
				savebox = None
			from saving import StringSaver
			savebox = StringSaver(bug_report, 'BugReport')
			savebox.connect('destroy', destroy)
			savebox.show()
			continue
		if resp == QUIT:
			sys.exit(1)
		assert resp == DETAILS
		box.set_response_sensitive(DETAILS, False)
		box.set_has_separator(False)

		button = ButtonMixed(g.STOCK_SAVE, _('_Bug Report'))
		button.set_flags(g.CAN_DEFAULT)
		button.show()
		box.add_action_widget(button, SAVE)
		box.action_area.set_child_secondary(button, True)

		button = ButtonMixed(g.STOCK_QUIT, _('Forced Quit'))
		button.set_flags(g.CAN_DEFAULT)
		button.show()
		box.add_action_widget(button, QUIT)
		box.action_area.set_child_secondary(button, True)

		ee = ExceptionExplorer(tb)
		box.vbox.pack_start(ee)
		ee.show()
	box.destroy()
	toplevel_unref()

class ExceptionExplorer(g.Frame):
	"""Displays details from a traceback object."""
	FILE = 0
	LINE = 1
	FUNC = 2
	CODE = 3
	def __init__(self, tb):
		g.Frame.__init__(self, _('Stack trace (innermost last)'))

		vbox = g.VBox(False, 0)
		self.add(vbox)

		inner = g.Frame()
		inner.set_shadow_type(g.SHADOW_IN)
		vbox.pack_start(inner, False, True, 0)

		self.savebox = None

		self.tb = tb
		
		self.model = g.ListStore(gobject.TYPE_STRING, gobject.TYPE_INT,
					 gobject.TYPE_STRING, gobject.TYPE_STRING)
		tree = g.TreeView(self.model)
		inner.add(tree)

		cell = g.CellRendererText()

		column = g.TreeViewColumn('File', cell, text = ExceptionExplorer.FILE)
		cell.set_property('xalign', 1)
		tree.append_column(column)

		cell = g.CellRendererText()
		column = g.TreeViewColumn('Line', cell, text = ExceptionExplorer.LINE)
		tree.append_column(column)
		column = g.TreeViewColumn('Func', cell, text = ExceptionExplorer.FUNC)
		tree.append_column(column)
		column = g.TreeViewColumn('Code', cell, text = ExceptionExplorer.CODE)
		tree.append_column(column)

		inner.set_border_width(5)

		frames = []
		while tb is not None:
			frames.insert(0, (tb.tb_frame, traceback.tb_lineno(tb)))
			tb = tb.tb_next
		f = self.tb.tb_frame
		if f:
			f = f.f_back	# Skip the reporting frame
		while f is not None:
			frames.append((f, f.f_lineno))
			f = f.f_back

		frames.reverse()

		new = None
		for f, lineno in frames:
			co = f.f_code
			filename = co.co_filename
			name = co.co_name
			line = linecache.getline(filename, lineno).strip()

			filename = os.path.basename(filename)
			
			new = self.model.append()
			self.model.set(new, ExceptionExplorer.FILE, filename,
					    ExceptionExplorer.LINE, lineno,
					    ExceptionExplorer.FUNC, name,
					    ExceptionExplorer.CODE, line)

		def selected_frame():
			selected = sel.get_selected()
			assert selected
			model, iter = selected
			frame, = model.get_path(iter)
			return frames[frame][0]

		vars = g.ListStore(str, str)
		sel = tree.get_selection()
		sel.set_mode(g.SELECTION_BROWSE)
		def select_frame(tree):
			vars.clear()
			for n, v in selected_frame().f_locals.iteritems():
				value = `v`
				if len(value) > 500:
					value = value[:500] + ' ...'
				new = vars.append()
				vars.set(new, 0, str(n), 1, value)
		sel.connect('changed', select_frame)

		# Area to show the local variables
		tree = g.TreeView(vars)

		vbox.pack_start(g.Label(_('Local variables in selected frame:')),
				False, True, 0)

		cell = g.CellRendererText()
		column = g.TreeViewColumn('Name', cell, text = 0)
		cell.set_property('xalign', 1)
		tree.append_column(column)
		cell = g.CellRendererText()
		column = g.TreeViewColumn('Value', cell, text = 1)
		tree.append_column(column)

		inner = g.ScrolledWindow()
		inner.set_size_request(-1, 200)
		inner.set_policy(g.POLICY_AUTOMATIC, g.POLICY_ALWAYS)
		inner.set_shadow_type(g.SHADOW_IN)
		inner.add(tree)
		inner.set_border_width(5)
		vbox.pack_start(inner, True, True, 0)

		if new:
			sel.select_iter(new)

		hbox = g.HBox(False, 4)
		hbox.set_border_width(5)
		vbox.pack_start(hbox, False, True, 0)
		hbox.pack_start(g.Label('>>>'), False, True, 0)

		expr = g.Entry()
		hbox.pack_start(expr, True, True, 0)
		def activate(entry):
			expr = entry.get_text()
			frame = selected_frame()
			try:
				info(`eval(expr, frame.f_locals, frame.f_globals)`)
			except:
				type, value = sys.exc_info()[:2]
				brief = ''.join(traceback.format_exception_only(type, value))
				alert(brief)
			entry.grab_focus()
		expr.connect('activate', activate)

		vbox.show_all()
