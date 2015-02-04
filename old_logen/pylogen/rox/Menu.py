"""The Menu widget provides an easy way to create menus that allow the user to
define keyboard shortcuts, and saves the shortcuts automatically. You only define
each Menu once, and attach it to windows as required.

Example:

from rox.Menu import Menu, set_save_name

set_save_name('Edit')

menu = Menu('main', [
	('/File',		'',		'<Branch>'),
	('/File/Save',		'save',		''),
	('/File/Open Parent',	'up',		''),
	('/File/Close',		'close',	''),
	('/File/',		'',		'<Separator>'),
	('/File/New',		'new',		''),
	('/Edit',		'',		'<Branch>'),
	('/Edit/Undo',		'undo',		''),
	('/Edit/Redo',		'redo',		''),
	('/Edit/',		'',		'<Separator>'),
	('/Edit/Search...',	'search',	''),
	('/Edit/Goto line...',	'goto',		''),
	('/Edit/',		'',		'<Separator>'),
	('/Edit/Process...',	'process',	''),
	('/Options',		'show_options', ''),
	('/Help',		'help',		'<StockItem>',	'F1', g.STOCK_HELP),
	])

There is also a new syntax, supported from 1.9.13, where you pass instances of MenuItem
instead of tuples to the Menu constructor. Be sure to require version 1.9.13 if
using this feature.
"""

from __future__ import generators

import rox
from rox import g
import choices

_save_name = None
def set_save_name(prog, leaf = 'menus'):
	"""Set the directory/leafname (see choices) used to save the menu keys.
	Call this before creating any menus."""
	global _save_name
	_save_name = (prog, leaf)

class MenuItem:
	"""Base class for menu items. You should normally use one of the subclasses..."""
	def __init__(self, label, callback_name, type = '', key = None, stock = None):
		if label and label[0] == '/':
			self.label = label[1:]
		else:
			self.label = label
		self.fn = callback_name
		self.type = type
		self.key = key
		self.stock = stock

	def activate(self, caller):
		getattr(caller, self.fn)()

class Action(MenuItem):
	"""A leaf menu item, possibly with a stock icon, which calls a method when clicked."""
	def __init__(self, label, callback_name, key = None, stock = None, values = ()):
		"""object.callback(*values) is called when the item is activated."""
		if stock:
			MenuItem.__init__(self, label, callback_name, '<StockItem>', key, stock)
		else:
			MenuItem.__init__(self, label, callback_name, '', key)
		self.values = values

	def activate(self, caller):
		getattr(caller, self.fn)(*self.values)

class ToggleItem(MenuItem):
	"""A menu item that has a check icon and toggles state each time it is activated."""
	def __init__(self, label, property_name):
		"""property_name is a boolean property on the caller object. You can use
		the built-in Python function property() if you want to perform calculations when
		getting or setting the value."""
		MenuItem.__init__(self, label, property_name, '<ToggleItem>')
		self.widget = None
		self.updating = False
	
	def update(self, menu, widget):
		"""Called when then menu is opened."""
		self.updating = True
		state = getattr(menu.caller, self.fn)
		widget.set_active(state)
		self.updating = False
	
	def activate(self, caller):
		if not self.updating:
			setattr(caller, self.fn, not getattr(caller, self.fn))

class SubMenu(MenuItem):
	"""A branch menu item leading to a submenu."""
	def __init__(self, label, submenu):
		MenuItem.__init__(self, label, None, '<Branch>')
		self.submenu = submenu

class Separator(MenuItem):
	"""A line dividing two parts of the menu."""
	def __init__(self):
		MenuItem.__init__(self, '', None, '<Separator>')

def _walk(items):
	for x in items:
		yield "/" + x.label, x
		if isinstance(x, SubMenu):
			for l, y in _walk(x.submenu):
				yield "/" + x.label + l, y

class Menu:
	def __init__(self, name, items):
		"""names should be unique (eg, 'popup', 'main', etc).
		items is a list of menu items:
		[(name, callback_name, type, key), ...].
		'name' is the item's path.
		'callback_name' is the NAME of a method to call.
		'type' is as for g.ItemFactory.
		'key' is only used if no bindings are in Choices."""
		if not _save_name:
			raise Exception('Call rox.Menu.set_save_name() first!')

		ag = g.AccelGroup()
		self.accel_group = ag
		factory = g.ItemFactory(g.Menu, '<%s>' % name, ag)

		program, save_leaf = _save_name
		accel_path = choices.load(program, save_leaf)

		out = []
		self.fns = []

		# Convert old-style list of tuples to new classes
		if items and not isinstance(items[0], MenuItem):
			items = [MenuItem(*t) for t in items]
		
		items_with_update = []
		for path, item in _walk(items):
			if item.fn:
				self.fns.append(item)
				cb = self._activate
			else:
				cb = None
			if item.stock:
				out.append((path, item.key, cb, len(self.fns) - 1, item.type, item.stock))
			else:
				out.append((path, item.key, cb, len(self.fns) - 1, item.type))
			if hasattr(item, 'update'):
				items_with_update.append((path, item))

		factory.create_items(out)
		self.factory = factory

		self.update_callbacks = []
		for path, item in items_with_update:
			widget = factory.get_widget(path)
			fn = item.update
			self.update_callbacks.append(lambda: fn(self, widget))

		if accel_path:
			g.accel_map_load(accel_path)
		
		self.caller = None	# Caller of currently open menu
		self.menu = factory.get_widget('<%s>' % name)

		def keys_changed(*unused):
			program, name = _save_name
			path = choices.save(program, name)
			if path:
				try:
					g.accel_map_save(path)
				except AttributeError:
					print "Error saving keybindings to", path
		# GtkAccelGroup has its own (unrelated) connect method,
		# so the obvious approach doesn't work.
		#ag.connect('accel_changed', keys_changed)
		import gobject
		gobject.GObject.connect(ag, 'accel_changed', keys_changed)
	
	def attach(self, window, object):
		"""Keypresses on this window will be treated as menu shortcuts
		for this object, calling 'object.<callback_name>' when used."""
		def kev(w, k):
			self.caller = object
			return 0
		window.connect('key-press-event', kev)
		window.add_accel_group(self.accel_group)
	
	def _position(self, menu):
		x, y, mods = g.gdk.get_default_root_window().get_pointer()
		width, height = menu.size_request()
		return (x - width * 3 / 4, y - 16, True)
	
	def popup(self, caller, event, position_fn = None):
		"""Display the menu. Call 'caller.<callback_name>' when an item is chosen.
		For applets, position_fn should be my_applet.position_menu)."""
		self.caller = caller
		map(apply, self.update_callbacks) # Update toggles, etc
		if event:
			self.menu.popup(None, None, position_fn or self._position, event.button, event.time)
		else:
			self.menu.popup(None, None, position_fn or self._position, 0, 0)
	
	def _activate(self, action, widget):
		if self.caller:
			try:
				self.fns[action].activate(self.caller)
			except:
				rox.report_exception()
		else:
			raise Exception("No caller for menu!")
