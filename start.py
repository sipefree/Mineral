#!/usr/bin/env python
import os
from sys import argv

mnesiadir = os.path.abspath("./db")
cmd = "erl -K true -pa ./ebin -boot start_sasl -config elog -name mineral@local"

def myrun(it):
	print it
	os.system(it)

if len(argv) > 1:
	if argv[1] == "nostart":
		myrun(cmd)
		if len(argv) > 2:
			myrun(cmd + " " + argv[2])
	else:
		myrun(cmd + " -eval \"application:start(mineral).\"" + " " + argv[1])
else:
	myrun(cmd + " -eval \"application:start(mineral).\"")