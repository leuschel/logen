"""This module implements the Choices system for user preferences.
The environment variable CHOICESPATH gives a list of directories to search
for choices. Changed choices are saved back to the first directory in the
list."""

import os
from os.path import exists

try:
	path = os.environ['CHOICESPATH']
	paths = path.split(':')
except KeyError:
	paths = [ os.environ['HOME'] + '/Choices',
		  '/usr/local/share/Choices',
		  '/usr/share/Choices' ]
	
def load(dir, leaf):
	"""When you want to load user choices, use this function. 'dir' is
	the subdirectory within Choices where the choices are saved (usually
	this will be the name of your program). 'leaf' is the file within it.
	If serveral files are present, the most important one is returned. If
	no files are there, returns None.
	Eg ('Edit', 'Options') - > '/usr/local/share/Choices/Edit/Options'"""

	for path in paths:
		if path:
			full = path + '/' + dir + '/' + leaf
			if exists(full):
				return full

	return None

def save(dir, leaf, create = 1):
	"""Returns a path to save to, or None if saving is disabled.
	If 'create' is FALSE then no directories are created. 'dir' and
	'leaf' are as for load()."""

	p = paths[0]
	if not p:
		return None

	if create and not os.path.exists(p):
		os.mkdir(p, 0x1ff)
	p = p + '/' + dir
	if create and not os.path.exists(p):
		os.mkdir(p, 0x1ff)
		
	return p + '/' + leaf
