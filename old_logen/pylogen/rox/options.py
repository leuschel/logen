"""
To use the Options system:

1. Create an OptionGroup:
	options = OptionGroup('MyProg', 'Options')
You can also use the handy rox.setup_app_options() in most applications.

2. Create the options:
	colour = Option('colour', 'red', options)
	size = Option('size', 3, options)

3. Register any callbacks (notification of options changing):
	def my_callback():
		if colour.has_changed:
			print "The colour is now", colour.value
	options.add_notify(my_callback)

4. Notify any changes from defaults:
	options.notify()

See OptionsBox for editing options. Do not change the value of options
yourself.
"""

from __future__ import generators

import choices
import rox

from xml.dom import Node, minidom

def data(node):
	"""Return all the text directly inside this DOM Node."""
	return ''.join([text.nodeValue for text in node.childNodes
			if text.nodeType == Node.TEXT_NODE])

class Option:
	"""An Option stores a single value. Every option is part of exactly one OptionGroup.

	The read-only attributes value and int_value can be used to get the current setting
	for the Option. int_value will be -1 if the value is not a valid integer.

	The has_changed attribute is used during notify() calls to indicate whether this
	Option's value has changed since the last notify (or option creation).
	You may set has_changed = 1 right after creating an option if you want to force
	notification the first time even if the default is used.
	"""
	def __init__(self, name, value, group = None):
		"""Create a new option with this name and default value.
		Add to 'group', or to rox.app_options if no group is given.
		The value cannot be used until the first notify() call to
		the group."""
		if not group:
			assert rox.app_options
			group = rox.app_options
		self.name = name
		self.has_changed = 0	# ... since last notify/default
		self.default_value = str(value)
		self.group = group
		self.value = None
		self.int_value = None

		self.group._register(self)
	
	def _set(self, value):
		if self.value != value:
			self.value = str(value)
			self.has_changed = 1
			try:
				if self.value == 'True':
					self.int_value = 1
				elif self.value == 'False':
					self.int_value = 0
				else:
					self.int_value = int(float(self.value))
			except:
				self.int_value = -1
	
	def _to_xml(self, parent):
		doc = parent.ownerDocument
		node = doc.createElement('Option')
		node.setAttribute('name', self.name)
		node.appendChild(doc.createTextNode(self.value))
		parent.appendChild(node)
	
	def __str__(self):
		return "<Option %s=%s>" % (self.name, self.value)

class OptionGroup:
	def __init__(self, program, leaf):
		"program/leaf is a Choices pair for the saved options."
		self.program = program
		self.leaf = leaf
		self.pending = {}	# Loaded, but not registered
		self.options = {}	# Name -> Option
		self.callbacks = []
		self.too_late_for_registrations = 0
		
		path = choices.load(program, leaf)
		if not path:
			return

		try:
			doc = minidom.parse(path)
			
			root = doc.documentElement
			assert root.localName == 'Options'
			for o in root.childNodes:
				if o.nodeType != Node.ELEMENT_NODE:
					continue
				if o.localName != 'Option':
					print "Warning: Non Option element", o
					continue
				name = o.getAttribute('name')
				self.pending[name] = data(o)
		except:
			rox.report_exception()
	
	def _register(self, option):
		"""Called by Option.__init__."""
		assert option.name not in self.options
		assert not self.too_late_for_registrations

		name = option.name

		self.options[name] = option
		
		if name in self.pending:
			option._set(self.pending[name])
			del self.pending[name]
	
	def save(self):
		"""Save all option values. Usually called by OptionsBox()."""
		assert self.too_late_for_registrations

		path = choices.save(self.program, self.leaf)
		if not path:
			return	# Saving is disabled

		from xml.dom.minidom import Document
		doc = Document()
		root = doc.createElement('Options')
		doc.appendChild(root)

		for option in self:
			option._to_xml(root)

		stream = open(path, 'w')
		doc.writexml(stream)
		stream.close()
	
	def add_notify(self, callback):
		"Call callback() after one or more options have changed value."
		assert callback not in self.callbacks

		self.callbacks.append(callback)
	
	def remove_notify(self, callback):
		"""Remove a callback added with add_notify()."""
		self.callbacks.remove(callback)
	
	def notify(self):
		"""Call this after creating any new options or changing their values."""
		if not self.too_late_for_registrations:
			self.too_late_for_registrations = 1
			if self.pending:
				print "Warning: Some options loaded but unused:"
				for (key, value) in self.pending.iteritems():
					print "%s=%s" % (key, value)
			for o in self:
				if o.value is None:
					o._set(o.default_value)
		map(apply, self.callbacks)
		for option in self:
			option.has_changed = 0
	
	def __iter__(self):
		return self.options.itervalues()
