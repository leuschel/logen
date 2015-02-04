"""ROX applications should provide good drag-and-drop support. Use this module
to allow drops onto widgets in your application."""

import rox
from rox import g, alert, get_local_path, _

gdk = g.gdk

TARGET_URILIST = 0
TARGET_RAW = 1

def extract_uris(data):
	"""Convert a text/uri-list to a python list of (still escaped) URIs"""
	lines = data.split('\r\n')
	out = []
	for l in lines:
		if l == chr(0):
			continue	# (gmc adds a '\0' line)
		if l and l[0] != '#':
			out.append(l)
	return out

def provides(context, type): return type in map(str, context.targets)

class RemoteFiles(Exception):
	"Internal use"
	def __init__(self):
		Exception.__init__(self, _('Cannot load files from a remote machine '
			   '(multiple files, or target application/octet-stream not provided)'))

class XDSLoader:
	"""A mix-in class for widgets that can have files/data dropped on
	them. If object is also a GtkWidget, xds_proxy_for(self) is called
	automatically."""

	def __init__(self, types):
		"""Call this after initialising the widget.
		Types is a list of MIME-types, or None to only accept files."""

		targets = [('text/uri-list', 0, TARGET_URILIST)]
		if types:
			for type in types + ['application/octet-stream']:
				targets.append((type, 0, TARGET_RAW))
		
		self.targets = targets
		if isinstance(self, g.Widget):
			self.xds_proxy_for(self)
	
	def xds_proxy_for(self, widget):
		"Handle drops on this widget as if they were to 'self'."
		# (Konqueror requires ACTION_MOVE)
		widget.drag_dest_set(g.DEST_DEFAULT_MOTION | g.DEST_DEFAULT_HIGHLIGHT,
				self.targets,
				gdk.ACTION_COPY | gdk.ACTION_MOVE | gdk.ACTION_PRIVATE)
		
		widget.connect('drag-data-received', self.xds_data_received)
		widget.connect('drag-drop', self.xds_drag_drop)
	
	def xds_drag_drop(self, widget, context, data, info, time):
		"""Called when something is dropped on us. Decide which of the
		offered targets to request and ask for it. xds_data_received will
		be called when it finally arrives."""
		target = widget.drag_dest_find_target(context, self.targets)
		context.rox_leafname = None
		if target is None:
			# Error?
			context.drop_finish(False, time)
		else:
			if provides(context, 'XdndDirectSave0'):
				import saving
				context.rox_leafname = saving._read_xds_property(context, False)
			widget.drag_get_data(context, target, time)
		return True

	def xds_data_received(self, widget, context, x, y, selection, info, time):
		"Called when we get some data. Internal."
		if selection.data is None:
			# Timeout?
			context.drop_finish(False, time)
			return

		if info == TARGET_RAW:
			try:
				self.xds_load_from_selection(selection, context.rox_leafname)
			except:
				context.drop_finish(False, time)
				raise
			context.drop_finish(True, time)
			return 1
		if info != TARGET_URILIST:
			return 0

		uris = extract_uris(selection.data)
		if not uris:
			alert("Nothing to load!")
			context.drop_finish(False, time)
			return 1

		try:
			try:
				self.xds_load_uris(uris)
			except RemoteFiles:
				if len(uris) != 1 or not provides(context, 'application/octet-stream'):
					raise
				widget.drag_get_data(context, 'application/octet-stream', time)
				return 1	# Don't do drag_finish
		except:
			context.drop_finish(False, time)
			rox.report_exception()
		else:
			context.drop_finish(True, time)

		return 1
	
	def xds_load_uris(self, uris):
		"""Try to load each URI in the list. Override this if you can handle URIs
		directly. The default method passes each local path to xds_load_from_file()
		and displays an error for anything else.
		The uris are escaped, so a space will appear as '%20'"""
		paths = []
		for uri in uris:
			path = get_local_path(uri)
			if path:
				paths.append(path)
		if len(paths) < len(uris):
			raise RemoteFiles
		for path in paths:
			self.xds_load_from_file(path)
	
	def xds_load_from_file(self, path):
		"""Try to load this local file. Override this if you have a better way
		to load files. The default method opens the file and calls xds_load_from_stream()."""
		try:
			self.xds_load_from_stream(path, None, open(path, 'rb'))
		except:
			rox.report_exception()
	
	def xds_load_from_selection(self, selection, leafname = None):
		"""Try to load this selection (data from another application). The default
		puts the data in a cStringIO and calls xds_load_from_stream()."""
		if selection.data is None:
			g.gdk.beep()	# Load aborted
			return
		from cStringIO import StringIO
		type = str(selection.type)
		self.xds_load_from_stream(leafname, type, StringIO(selection.data))
	
	def xds_load_from_stream(self, name, type, stream):
		"""Called when we get any data sent via drag-and-drop in any way (local
		file or remote application transfer). You should override this and do
		something with the data. 'name' may be None (if the data is unnamed),
		a leafname, or a full path or URI. 'type' is the MIME type, or None if
		unknown."""
		alert('Got some data, but missing code to handle it!\n\n(name="%s";type="%s")'
			% (name, type))
