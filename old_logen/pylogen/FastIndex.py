import bisect
import time

class FastIndex:

    newlines = None

    def __init__(self, string):
        self.newlines = [-1]
        for i in xrange(0, len(string)):
            if string[i] == '\n':
                self.newlines.append(i)

    def get_tk_index(self, char_pos):
        line = bisect.bisect_left(self.newlines, char_pos)
        return str(line) + "." + str(char_pos - self.newlines[line - 1] - 1)

    def get_two_tk_indices_same_line(self, char_pos1, char_pos2):
        line = bisect.bisect_left(self.newlines, char_pos1)
        col = char_pos1 - self.newlines[line - 1] - 1
        return (str(line) + "." + str(col),
                str(line) + "." + str(col + char_pos2 - char_pos1))

    def get_two_tk_indices(self, char_pos1, char_pos2):
        line = bisect.bisect_left(self.newlines, char_pos1)
        line2 = bisect.bisect_left(self.newlines, char_pos2, line)
        return (str(line) + "." + str(char_pos1 - self.newlines[line - 1] - 1),
                str(line2) + "." + str(char_pos2 - self.newlines[line2 - 1] - 1))

    def line_end(self, index):
        (line, _) = index.split('.')
        lineno = int(line)
        return line + '.' + str(self.newlines[lineno] - self.newlines[lineno - 1])

    def next_char(self, index):
        (line, col) = index.split('.')
        lineno = int(line)
        colno = int(col)
        if self.newlines[lineno] - self.newlines[lineno - 1] == colno + 1:
            return str(lineno + 1) + '.0'
        else:
            return line + '.' + str(colno + 1)

class Timer:
    clock = None

    def tic(self):
        self.clock = time.time()

    def toc(self):
        print "Elapsed time is %.2f" % (time.time() - self.clock);

    def toc_and_tic(self):
        self.toc()
        self.tic()

timer = Timer()

