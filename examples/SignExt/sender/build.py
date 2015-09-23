#!/usr/bin/python

import os
import sys

try:
	from fabricate import *
except ImportError, e:
	print "Couldn't find the fabricate module."
	sys.exit(1)

sources = ['sender.c']
target = 'sender'
includes = [] 

cflags = ['-ggdb', '-O2', '-fPIC', 
		  '-std=gnu99', '-Wall', '-Werror'] + includes

def build():
	compile()
	link()

def compile():
	for source in sources:
		run('gcc', cflags, '-c', source, '-o', source.replace('.c', '.o'))

def link():
	objects = [s.replace('.c', '.o') for s in sources]
	run('gcc', objects, '-o', target)

main()
