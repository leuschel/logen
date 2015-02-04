### Simple conversion script by steve
### convert ciao to sics ----well kind of

import sys
import glob
import os
def convert_file(File,prolog="SICS"):
	print "Converting %-20s" % File,
	input = open(File, 'r')
	output = []
	changes = 0
	for line in input:
		#print line,
		newline = convert_line(line,prolog)
		
		output += [newline]
		if newline != line:
			changes += 1

	output = "".join(output)

	out = open(File,'w')
	out.write(output)
	print "\t\tOK Changes Made %d" % changes

	

def convert_line(line, prolog="SICS"):

	if prolog=="SICS":
		prolog_on = "%%%SICS"
		prolog_off = "%%%CIAO"
	else:
		prolog_off = "%%%SICS"
		prolog_on = "%%%CIAO"
		
	prolog = "%%%CIAO"
	
	nl = line
	nl = nl.replace("%s/*"%prolog_off, "/*%s"%prolog_off)
	nl = nl.replace("%s*/"%prolog_off, "*/%s"%prolog_off)
	
	nl = nl.replace("/*%s"%prolog_on, "%s/*"%prolog_on)
	nl = nl.replace("*/%s"%prolog_on, "%s*/"%prolog_on)

	
	return nl
	
		


def clean_dir(dir,pattern):
	olddir = os.getcwd()
	os.chdir(dir)
	files = glob.glob(pattern)
	for f in files:
		os.remove(f)
	os.chdir(olddir)
	

if __name__ == "__main__":
	if sys.argv[1] == "CIAO":
		print "Converting to CIAO"
	elif sys.argv[1] == "SICS":
		print "Converting to SICS"
	else:
		print "ERROR"
		print "\tUSAGE: To Convert to CIAO MODE"
		print "\tpython convert.py CIAO"
		print "\tOR: To Convert to SICStus Mode"
		print "\tpython convert.py SICS"
		sys.exit(1)
	print "Running Conversion to %s" % sys.argv[1]
	
	files = glob.glob("*.pl")
	for f in files:		
		convert_file(f,sys.argv[1])		
	

	clean_dir(".","*.itf")
	clean_dir("./logen_examples","*.itf")
	clean_dir("./logen","*.itf")
	clean_dir("./Tests","*.itf")
	clean_dir("./Types","*.itf")
	clean_dir("./TypesBTA","*.itf")
	clean_dir(".","*.po")
	clean_dir("./logen_examples","*.po")
	clean_dir("./logen","*.po")
	clean_dir("./Tests","*.po")
	clean_dir("./Types","*.po")
	clean_dir("./TypesBTA","*.po")

	
