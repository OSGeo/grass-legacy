#! /usr/bin/python
## vim:ts=4:et:sts=4:sw=4:ai:
""" Construct simple wx.Python GUI from a GRASS command interface description.

# Copyright (C) 2000 by the GRASS Development Team
# Author: Jan-Oliver Wagner <jan@intevation.de>
# improved by: Bernhard Reiter	 <bernhard@intevation.de>
# Improved by: Michael Barton, Arizona State University
#
# This program is free software under the GPL (>=v2)
# Read the file COPYING coming with GRASS for details.

# This program is just a coarse approach to
# automatically build a GUI from a xml-based
# GRASS user interface description.
#
# You need to have Python 2.4, wx.Python 2.6 and python-xml.
#
# The XML stream is read from stdin, thus you
# may call it for instance this way:
# r.basins.fill --interface-description | python grassgui.py
# or
# r.basins.fill --interface-description | ./grassgui.py
#
# Or you set an alias or wrap the call up in a nice
# shell script, GUI environment ... please contribute your idea.
#
# Updated to wxPython 2.6 syntax. Methods added to make it callable by gui.
# Method added to automatically re-run with pythonw on a Mac.
"""
__version__ ="$Date: 2006/08/06 21:21:01 $"

import wx
import sys
import string

# Do the python 2.0 standard xml thing and map it on the old names
import xml.sax
import xml.sax.handler
HandlerBase=xml.sax.handler.ContentHandler
from xml.sax import make_parser

import os
from os import system
import subprocess
import re

import render
import mapdisp

def reexec_with_pythonw():
	if sys.platform == 'darwin' and\
		not sys.executable.endswith('MacOS/Python'):
		print >>sys.stderr,'re-executing using pythonw'
		os.execvp('pythonw',['pythonw',__file__] + sys.argv[1:])

reexec_with_pythonw()

ID_RUN	  = 10
ID_CANCEL = 11
ID_PARAM_START = 800
ID_FLAG_START  = 900

ID_ABOUT = 101
ID_ABOUT_COMMAND = 102
ID_EXIT	 = 103

VSPACE = 4
HSPACE = 4
MENU_HEIGHT = 30
STATUSBAR_HEIGHT = 30
ENTRY_HEIGHT = 20
STRING_ENTRY_WIDTH = 300
BUTTON_HEIGHT = 44
BUTTON_WIDTH = 100

grass_task = { 'name' : 'unknown',
	'description' : 'No description available.',
	'lines' : 0, 'params' : [], 'flags' : [] }

def normalize_whitespace(text):
	"Remove redundant whitespace from a string"
	return string.join( string.split(text), ' ')

def escape_ampersand(text):
	"Escapes ampersands with additional ampersand for GUI"
	return string.replace(text, "&", "&&")

class testSAXContentHandler(HandlerBase):
# SAX compliant
	def characters(self, ch, start, length):
		pass

def test_for_broken_SAX():
	ch=testSAXContentHandler()
	try:
		xml.sax.parseString("""<?xml version="1.0"?>
			<child1 name="paul">Text goes here</child1>
			""",ch)
	except TypeError:
		return 1
	return 0

class processTask(HandlerBase):
	def __init__(self):
		self.inDescriptionContent = 0
		self.inDefaultContent = 0
		self.inValueContent = 0
		self.inParameter = 0
		self.inFlag = 0

	def startElement(self, name, attrs):
		if name == 'task':
			grass_task['name'] = attrs.get('name', None)

		if name == 'parameter':
			self.inParameter = 1;
			self.param_description = ''
			self.param_default = ''
			self.param_values = []
			# Look for the parameter name, type, requiredness
			self.param_name = attrs.get('name', None)
			self.param_type = attrs.get('type', None)
			if type == 'flag':
				grass_task['lines'] = grass_task['lines'] + 1
			else:
				grass_task['lines'] = grass_task['lines'] + 2
			self.param_required = attrs.get('required', None)
			self.param_multiple = attrs.get('multiple', None)

		if name == 'flag':
			self.inFlag = 1;
			self.flag_description = ''
			self.flag_default = ''
			self.flag_values = []
			# Look for the flag name
			self.flag_name = attrs.get('name', None)
			grass_task['lines'] = grass_task['lines'] + 1

		if name == 'description':
			self.inDescriptionContent = 1
			self.description = ''
		if name == 'default':
			self.inDefaultContent = 1
			self.param_default = ''
		if name == 'value':
			self.inValueContent = 1
			self.value_tmp = ''

	# works with python 2.0, but is not SAX compliant
	def characters(self, ch):
		self.my_characters(ch)

	def my_characters(self, ch):
		if self.inDescriptionContent:
			self.description = self.description + ch
		if self.inDefaultContent:
			self.param_default = self.param_default + ch
		if self.inValueContent:
			self.value_tmp = self.value_tmp + ch

	def endElement(self, name):
		# If it's not a parameter element, ignore it
		if name == 'parameter':
			self.inParameter = 0;
			grass_task['params'].append({
				"name" : self.param_name,
				"type" : self.param_type,
				"required" : self.param_required,
				"multiple" : self.param_multiple,
				"description" : self.param_description,
				"default" : self.param_default,
				"values" : self.param_values,
				"value" : '' })

		if name == 'flag':
			self.inFlag = 0;
			grass_task['flags'].append({
				"name" : self.param_name,
				"description" : self.flag_description } )

		if name == 'description':
			if self.inParameter:
				self.param_description = normalize_whitespace(self.description)
			elif self.inFlag:
				self.flag_description = normalize_whitespace(self.description)
			else:
				grass_task['description'] = normalize_whitespace(self.description)
			self.inDescriptionContent = 0

		if name == 'default':
			self.param_default = normalize_whitespace(self.param_default)
			self.inDefaultContent = 0
		if name == 'value':
			v = normalize_whitespace(self.value_tmp)
			self.param_values = self.param_values + [ normalize_whitespace(self.value_tmp) ]
			self.inValueContent = 0


class mainFrame(wx.Frame):
	def __init__(self, parent, ID, w, h):
		wx.Frame.__init__(self, parent, ID, grass_task['name'],
			wx.DefaultPosition, style=wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)

		self.CreateStatusBar()
		self.SetStatusText("Enter parameters for " + grass_task['name'])

		menu = wx.Menu()
		menu.Append(ID_ABOUT, "&About GrassGUI",
			"Information about GrassGUI")
		menu.Append(ID_ABOUT_COMMAND, "&About " + grass_task['name'],
			"Short descripton of GRASS command " + grass_task['name'])
		menu.AppendSeparator()
		menu.Append(ID_EXIT, "E&xit", "Terminate the program")

		menuBar = wx.MenuBar()
		menuBar.Append(menu, "&File");

		self.SetMenuBar(menuBar)

		self.guisizer = wx.BoxSizer(wx.VERTICAL)

		self.panel = wx.ScrolledWindow(self, -1, style=wx.TAB_TRAVERSAL)
		self.panel.SetScrollRate(10,10)

		p_count = 0
		l_count = 0
		while p_count < len(grass_task['params']):
			title = escape_ampersand(grass_task['params'][p_count]['description'])
			if grass_task['params'][p_count]['required'] == 'no':
				title = "[optional] " + title
			if grass_task['params'][p_count]['multiple'] == 'yes':
				title = "[multiple] " + title
			grass_task['params'][p_count]['value'] = grass_task['params'][p_count]['default']
			if (len(grass_task['params'][p_count]['values']) > 0):
				txt1 = wx.StaticText(self.panel, -1, title + ':', wx.Point(-1, -1), wx.Size(-1, -1))
				self.guisizer.Add(txt1, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)
				l_count = l_count + 1

				valuelist=map(str,grass_task['params'][p_count]['values'])
				cb = wx.ComboBox(self.panel, ID_PARAM_START + p_count, grass_task['params'][p_count]['default'],
					wx.Point(-1, -1), wx.Size(STRING_ENTRY_WIDTH, -1), valuelist, wx.CB_DROPDOWN)
				self.guisizer.Add(cb, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)
				wx.EVT_COMBOBOX(self, ID_PARAM_START + p_count, self.EvtComboBox)

			if ((grass_task['params'][p_count]['type'] == 'string' or
				grass_task['params'][p_count]['type'] == 'integer' or
				grass_task['params'][p_count]['type'] == 'float') and
				len(grass_task['params'][p_count]['values']) == 0):
				txt2 = wx.StaticText(self.panel, -1, title + ':',
					wx.Point(-1, -1), wx.Size(-1, -1))
				self.guisizer.Add(txt2, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)
				l_count = l_count + 1
				txt3 = wx.TextCtrl(self.panel, ID_PARAM_START + p_count,
					grass_task['params'][p_count]['default'], wx.Point(-1, -1),
					wx.Size(STRING_ENTRY_WIDTH, ENTRY_HEIGHT))
				self.guisizer.Add(txt3, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)
				wx.EVT_TEXT(self, ID_PARAM_START + p_count, self.EvtText)

			p_count = p_count + 1
			l_count = l_count + 1

		f_count = 0
		while f_count < len(grass_task['flags']):
			title = escape_ampersand(grass_task['flags'][f_count]['description'])
			chk = wx.CheckBox(self.panel,ID_FLAG_START + f_count, title,
				wx.Point(-1, -1), wx.Size(-1, -1), wx.NO_BORDER)
			self.guisizer.Add(chk, 0, wx.ALL, 5)
			wx.EVT_CHECKBOX(self, ID_FLAG_START + f_count, self.EvtCheckBox)

			f_count = f_count + 1
			l_count = l_count + 1

		p_count = p_count + f_count
		btnsizer = wx.BoxSizer(wx.HORIZONTAL)
		btn1 = wx.Button(self.panel, ID_CANCEL, "Cancel")
		btnsizer.Add(btn1, 0, wx.ALL| wx.ALIGN_CENTER, 10)
		btn2 = wx.Button(self.panel, ID_RUN, "Run")
		btnsizer.Add(btn2, 0, wx.ALL| wx.ALIGN_CENTER, 10)
		btn2.SetDefault()
		self.guisizer.Add(btnsizer, 0, wx.EXPAND)
		wx.EVT_MENU(self, ID_ABOUT, self.OnAbout)
		wx.EVT_MENU(self, ID_ABOUT_COMMAND, self.OnAboutCommand)
		wx.EVT_MENU(self, ID_EXIT,	self.OnCancel)
		wx.EVT_BUTTON(self.panel, ID_CANCEL, self.OnCancel)
		wx.EVT_BUTTON(self.panel, ID_RUN, self.OnRun)
		self.Bind(wx.EVT_CLOSE, self.onCloseWindow)

		self.panel.SetSizer(self.guisizer)
		self.guisizer.Fit(self.panel)

	def EvtText(self, event):
		grass_task['params'][event.GetId()-ID_PARAM_START]['value'] = event.GetString()

	def EvtCheckBox(self, event):
		if (event.Checked()):
			grass_task['flags'][event.GetId()-ID_FLAG_START]['value'] = 'checked'
		else:
			grass_task['flags'][event.GetId()-ID_FLAG_START]['value'] = 'not checked'

	def EvtComboBox(self, event):
		grass_task['params'][event.GetId()-ID_PARAM_START]['value'] = event.GetString()


	def OnRun(self, event):
		cmd = grass_task['name']
		p_count = 0
		errors = 0
		errStr = ""
		while p_count < len(grass_task['params']):
			if (grass_task['params'][p_count]['type'] != 'flag' and grass_task['params'][p_count]['value'] == '' and grass_task['params'][p_count]['required'] != 'no'):
				errStr = errStr + "Parameter " + grass_task['params'][p_count]['name'] + "(" +grass_task['params'][p_count]['description']	+ ") is missing\n"
				errors = errors + 1

			if (grass_task['params'][p_count]['type'] == 'flag'):
				if (grass_task['params'][p_count]['value'] == 'checked'):
					cmd = cmd + ' -' + grass_task['params'][p_count]['name']
			if (grass_task['params'][p_count]['type'] != 'flag' and grass_task['params'][p_count]['value'] != ''):
				cmd = cmd + ' ' + grass_task['params'][p_count]['name'] + '=' + grass_task['params'][p_count]['value']
			p_count = p_count + 1

		if errors:
			self.OnError(errStr)
		else:
##			if cmd[0:2] == "d.":
##				# Send GRASS display command(s)with arguments
##				# to the display processor.
##				# Display with focus receives display command(s).
####				  self.console_output.write(cmd+"\n----------\n") #need to echo this back to gism.py console
##				currmap = render.Track().getMD()
##				currmap.setDcommandList(cmd)
##
##
##			else:
				# Send any other command to the shell.
			try:
				retcode = subprocess.call(cmd, shell=True)
				if retcode < 0:
					print >>sys.stderr, "Child was terminated by signal", -retcode
				elif retcode > 0:
					print >>sys.stderr, "Child returned", retcode
			except OSError, e:
				print >>sys.stderr, "Execution failed:", e

	##			  self.console_output.write(cmd+"\n----------\n") #need to echo this back to gism.py console
	##				  self.out = subprocess.Popen(cmd, shell=True, stdout=Pipe).stdout
	##			  self.out = os.popen(cmd, "r").read() #need to echo this back to gism.py console
	##			  self.console_output.write(self.out+"\n") #need to echo this back to gism.py console


	def OnError(self, errMsg):
		dlg = wx.MessageDialog(self, errMsg, "Error", wx.OK | wx.ICON_ERROR)
		dlg.ShowModal()
		dlg.Destroy()

	def OnCancel(self, event):
		self.Close(True)

	def onCloseWindow(self, event):
		self.Destroy()

	def OnAbout(self, event):
		dlg = wx.MessageDialog(self, "This is a sample program for\n"
			"GRASS command interface parsing\n"
			"and automatic GUI building. \n%s" %(__version__),
			"About GrassGUI", wx.OK | wx.ICON_INFORMATION)
		dlg.ShowModal()
		dlg.Destroy()

	def OnAboutCommand(self, event):
		dlg = wx.MessageDialog(self,
			grass_task['name']+": "+grass_task['description'],
			"About " + grass_task['name'],
			wx.OK | wx.ICON_INFORMATION)
		dlg.ShowModal()
		dlg.Destroy()



class GrassGUIApp(wx.App):
	def OnInit(self):
		self.w = HSPACE + STRING_ENTRY_WIDTH + HSPACE
		self.h = MENU_HEIGHT + VSPACE + grass_task['lines'] * ENTRY_HEIGHT + VSPACE	 + BUTTON_HEIGHT + VSPACE + STATUSBAR_HEIGHT
		frame = mainFrame(None, -1, self.w, self.h)
		frame.Show(True)
		self.SetTopWindow(frame)
		return True

class GUI:
	def __init__(self):
		'''Parses GRASS commands when module is imported and used
		from gism.py'''
		self.w = HSPACE + STRING_ENTRY_WIDTH + HSPACE
		self.h = MENU_HEIGHT + VSPACE + grass_task['lines'] * ENTRY_HEIGHT + VSPACE	 + BUTTON_HEIGHT + VSPACE + STATUSBAR_HEIGHT

	def parseCommand(self, cmd, gmpath):
		cmdlst = []
		cmdlst = cmd.split(' ')

		if len(cmdlst) > 1:
			print "usage: <grass command> --task-description | " + cmdlst[0]
		else:
			# parse the interface decription
			cmd = cmd + r' --interface-description'
			cmdout = os.popen(cmd, "r").read()
			p = re.compile( '(grass-interface.dtd)')
			cmdout2 = p.sub( gmpath+r'/grass-interface.dtd', cmdout)
			handler = processTask()
			xml.sax.parseString(cmdout2, handler)

		frame = mainFrame(None, -1, self.w, self.h)
		frame.Show(True)

if __name__ == "__main__":
	# Create the application

	# Parsing if run from command line: find out the command to run
	if len(sys.argv) > 1:
		print "usage: <grass command> --task-description | " + sys.argv[0]
	else:
		# parse the interface decription
		handler = processTask()

		xml.sax.parse(sys.stdin,handler)

	app = GrassGUIApp(0)
	app.MainLoop()

