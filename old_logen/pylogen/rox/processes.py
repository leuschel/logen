"""This module makes it easier to use other programs to process data.

The Process class provides the low-level interface, which you can extend
by subclassing. Processes run in the background, so users can still work
with your application while they are running. If you don't care about that,
you might like to look at Python's builtin popen2 module.

The PipeThroughCommand class extends Process to provide an easy way to
run other commands. It also, optionally, allows a stream of data to be fed
in to the process's standard input, and can collect the output to another
stream. Typical usage:

rox.processes.PipeThroughCommand(('echo', 'hello'), None, file('output', 'w')).wait()

This creates a new process, and execs 'echo hello' in it with output sent
to the file 'output' (any file-like object can be used). The wait() runs a
recursive mainloop, so that your application can still be used while the
command runs, but the wait() itself doesn't return until the command
completes.

Instead of using a tuple for the command, a string may be passed (eg, "echo
hello"). In this case, the shell is used to interpret the command, allowing
pipes, wildcards and so on. Be very careful of escaping in this case (think
about filenames containing spaces, quotes, apostrophes, etc).
"""

from rox import g, saving

import os, sys, fcntl
import signal

def _keep_on_exec(fd): fcntl.fcntl(fd, fcntl.F_SETFD, 0)

class ChildError(Exception):
	"Raised when the child process reports an error."
	def __init__(self, message):
		Exception.__init__(self, message)

class ChildKilled(ChildError):
	"Raised when child died due to a call to the kill method."
	def __init__(self):
		ChildError.__init__(self, "Operation aborted at user's request")

class Process:
	"""This represents another process. You should subclass this
	and override the various methods. Use this when you want to
	run another process in the background, but still be able to
	communicate with it."""
	def __init__(self):
		self.child = None
	
	def start(self):
		"""Create the subprocess. Calls pre_fork() and forks.
		The parent then calls parent_post_fork() and returns,
		while the child calls child_post_fork() and then
		child_run()."""
		
		assert self.child is None

		stderr_r = stderr_w = None

		try:
			self.pre_fork()
			stderr_r, stderr_w = os.pipe()
			child = os.fork()
		except:
			if stderr_r: os.close(stderr_r)
			if stderr_w: os.close(stderr_w)
			self.start_error()
			raise

		if child == 0:
			# This is the child process
			try:
				try:
					os.setpgid(0, 0)  # Start a new process group
					os.close(stderr_r)

					if stderr_w != 2:
						os.dup2(stderr_w, 2)
						os.close(stderr_w)

					self.child_post_fork()
					self.child_run()
					raise Exception('child_run() returned!')
				except:
					import traceback
					traceback.print_exc()
			finally:
				os._exit(1)
			assert 0

		self.child = child

		# This is the parent process
		os.close(stderr_w)
		self.err_from_child = stderr_r

		import gobject
		if not hasattr(gobject, 'io_add_watch'):
			self.tag = g.input_add_full(self.err_from_child,
					g.gdk.INPUT_READ, self._got_errors)
		else:
			self.tag = gobject.io_add_watch(self.err_from_child,
					gobject.IO_IN | gobject.IO_HUP | gobject.IO_ERR,
					self._got_errors)

		self.parent_post_fork()
	
	def pre_fork(self):
		"""This is called in 'start' just before forking into
		two processes. If you want to share a resource between
		both processes (eg, a pipe), create it here.
		Default method does nothing."""
		
	def parent_post_fork(self):
		"""This is called in the parent after forking. Free the
		child part of any resources allocated in pre_fork().
		Also called if the fork or pre_fork() fails.
		Default method does nothing."""
	
	def child_post_fork(self):
		"""Called in the child after forking. Release the parent
		part of any resources allocated in pre_fork().
		Also called (in the parent) if the fork or pre_fork()
		fails. Default method does nothing."""
	
	def start_error(self):
		"""An error occurred before or during the fork (possibly
		in pre_fork(). Clean up. Default method calls
		parent_post_fork() and child_post_fork(). On returning,
		the original exception will be raised."""
		self.parent_post_fork()
		self.child_post_fork()
	
	def child_run(self):
		"""Called in the child process (after child_post_fork()).
		Do whatever processing is required (perhaps exec another
		process). If you don't exec, call os._exit(n) when done.
		DO NOT make gtk calls in the child process, as it shares its
		parent's connection to the X server until you exec()."""
		os._exit(0)
	
	def kill(self, sig = signal.SIGTERM):
		"""Send a signal to all processes in the child's process
		group. The default, SIGTERM, requests all the processes
		terminate. SIGKILL is more forceful."""
		assert self.child is not None
		os.kill(-self.child, sig)
	
	def got_error_output(self, data):
		"""Read some characters from the child's stderr stream.
		The default method copies to our stderr. Note that 'data'
		isn't necessarily a complete line; it could be a single
		character, or several lines, etc."""
		sys.stderr.write(data)
	
	def _got_errors(self, source, cond):
		got = os.read(self.err_from_child, 100)
		if got:
			self.got_error_output(got)
			return 1

		os.close(self.err_from_child)
		g.input_remove(self.tag)
		del self.tag

		pid, status = os.waitpid(self.child, 0)
		self.child = None
		self.child_died(status)
		
	def child_died(self, status):
		"""Called when the child died (actually, when the child
		closes its end of the stderr pipe). The child process has
		already been reaped at this point; 'status' is the status
		returned by os.waitpid."""
	
class PipeThroughCommand(Process):
	def __init__(self, command, src, dst):
		"""Execute 'command' with src as stdin and writing to stream
		dst. If either stream is not a fileno() stream, temporary files
		will be used as required.
		Either stream may be None if input or output is not required.
		Call the wait() method to wait for the command to finish.
		'command' may be a string (passed to os.system) or a list (os.execvp).
		"""

		if src is not None and not hasattr(src, 'fileno'):
			import shutil
			new = _Tmp()
			src.seek(0)
			shutil.copyfileobj(src, new)
			src = new

		Process.__init__(self)

		self.command = command
		self.dst = dst
		self.src = src
		self.tmp_stream = None

		self.callback = None
		self.killed = 0
		self.errors = ""

		self.done = False	# bool or exception
		self.waiting = False

	def pre_fork(self):
		# Output to 'dst' directly if it's a fileno stream. Otherwise,
		# send output to a temporary file.
		assert self.tmp_stream is None

		if self.dst:
			if hasattr(self.dst, 'fileno'):
				self.dst.flush()
				self.tmp_stream = self.dst
			else:
				self.tmp_stream = _Tmp()

	def start_error(self):
		self.tmp_stream = None

	def child_run(self):
		"""Assigns file descriptors and calls child_run_with_streams."""
		src = self.src or file('/dev/null', 'r')

		os.dup2(src.fileno(), 0)
		_keep_on_exec(0)
		try:
			os.lseek(0, 0, 0)	# OpenBSD needs this, dunno why
		except:
			pass

		if self.dst:
			os.dup2(self.tmp_stream.fileno(), 1)
			_keep_on_exec(1)
	
		self.child_run_with_streams()
		os._exit(1)
	
	def child_run_with_streams(self):
		"""This is run by the child process. stdin and stdout have already been set up.
		Should call exec() or os._exit() to finish. Default method execs self.command."""
		# (basestr is python2.3 only)
		if isinstance(self.command, str):
			if os.system(self.command) == 0:
				os._exit(0)	# No error code or signal
		else:
			os.execvp(self.command[0], self.command)
	
	def parent_post_fork(self):
		if self.dst and self.tmp_stream is self.dst:
			self.tmp_stream = None
	
	def got_error_output(self, data):
		self.errors += data
	
	def check_errors(self, errors, status):
		"""Raise an exception here if errors (the string the child process wrote to stderr) or
		status (the status from waitpid) seems to warrent it. It will be returned by wait()."""
		if errors:
			raise ChildError("Errors from command '%s':\n%s" % (str(self.command), errors))
		raise ChildError("Command '%s' returned an error code (%d)!" % (str(self.command), status))
	
	def child_died(self, status):
		errors = self.errors.strip()

		if self.killed:
			self.done = ChildKilled()
		elif errors or status:
			try:
				self.check_errors(errors, status)
				self.done = True
			except Exception, e:
				self.done = e
		else:
			self.done = True

		assert self.done is True or isinstance(self.done, Exception)

		if self.done is True:
			# Success
			# If dst wasn't a fileno stream, copy from the temp file to it
			if self.tmp_stream:
				self.tmp_stream.seek(0)
				self.dst.write(self.tmp_stream.read())

		self.tmp_stream = None

		if self.waiting:
			assert self.done
			self.waiting = False
			g.mainquit()
	
	def wait(self):
		"""Run a recursive mainloop until the command terminates.
		Raises an exception on error."""
		if self.child is None:
			self.start()
		self.waiting = True
		while not self.done:
			g.mainloop()
		if self.done is not True:
			raise self.done
	
	def kill(self, sig = signal.SIGTERM):
		self.killed = 1
		Process.kill(self, sig)

def _Tmp(mode = 'w+b', suffix = '-tmp'):
	"Create a seekable, randomly named temp file (deleted automatically after use)."
	import tempfile
	try:
		return tempfile.NamedTemporaryFile(mode, suffix = suffix)
	except:
		# python2.2 doesn't have NamedTemporaryFile...
		pass

	import random
	name = tempfile.mktemp(`random.randint(1, 1000000)` + suffix)

	fd = os.open(name, os.O_RDWR|os.O_CREAT|os.O_EXCL, 0700)
	tmp = tempfile.TemporaryFileWrapper(os.fdopen(fd, mode), name)
	tmp.name = name
	return tmp

	
def _test():
	"Check that this module works."

	def show():
		error = sys.exc_info()[1]
		print "(error reported was '%s')" % error
	
	def pipe_through_command(command, src, dst): PipeThroughCommand(command, src, dst).wait()

	print "Test _Tmp()..."
	
	file = _Tmp()
	file.write('Hello')
	print >>file, ' ',
	file.flush()
	os.write(file.fileno(), 'World')

	file.seek(0)
	assert file.read() == 'Hello World'

	print "Test pipe_through_command():"

	print "Try an invalid command..."
	try:
		pipe_through_command('bad_command_1234', None, None)
		assert 0
	except ChildError:
		show()
	else:
		assert 0

	print "Try a valid command..."
	pipe_through_command('exit 0', None, None)
	
	print "Writing to a non-fileno stream..."
	from cStringIO import StringIO
	a = StringIO()
	pipe_through_command('echo Hello', None, a)
	assert a.getvalue() == 'Hello\n'

	print "Try with args..."
	a = StringIO()
	pipe_through_command(('echo', 'Hello'), None, a)
	assert a.getvalue() == 'Hello\n'

	print "Reading from a stream to a StringIO..."
	file.seek(1)			# (ignored)
	pipe_through_command('cat', file, a)
	assert a.getvalue() == 'Hello\nHello World'

	print "Writing to a fileno stream..."
	file.seek(0)
	file.truncate(0)
	pipe_through_command('echo Foo', None, file)
	file.seek(0)
	assert file.read() == 'Foo\n'

	print "Read and write fileno streams..."
	src = _Tmp()
	src.write('123')
	src.seek(0)
	file.seek(0)
	file.truncate(0)
	pipe_through_command('cat', src, file)
	file.seek(0)
	assert file.read() == '123'

	print "Detect non-zero exit value..."
	try:
		pipe_through_command('exit 1', None, None)
	except ChildError:
		show()
	else:
		assert 0
	
	print "Detect writes to stderr..."
	try:
		pipe_through_command('echo one >&2; sleep 2; echo two >&2', None, None)
	except ChildError:
		show()
	else:
		assert 0

	print "Check tmp file is deleted..."
	name = file.name
	assert os.path.exists(name)
	file = None
	assert not os.path.exists(name)

	print "Check we can kill a runaway proces..."
	ptc = PipeThroughCommand('sleep 100; exit 1', None, None)
	def stop():
		ptc.kill()
	g.timeout_add(2000, stop)
	try:
		ptc.wait()
		assert 0
	except ChildKilled:
		pass
	
	print "All tests passed!"

if __name__ == '__main__':
	_test()
