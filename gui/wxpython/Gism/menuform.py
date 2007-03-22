#! /usr/bin/python
""" Construct simple wx.Python GUI from a GRASS command interface description.

# Copyright (C) 2000-2007 by the GRASS Development Team
# Author: Jan-Oliver Wagner <jan@intevation.de>
# improved by: Bernhard Reiter   <bernhard@intevation.de>
# Improved by: Michael Barton, Arizona State University
# Improved by: Daniel Calvelo <dca@users.sf.net>
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
import select
import wx.lib.flatnotebook as FN
import  wx.lib.colourselect as csel

# Do the python 2.0 standard xml thing and map it on the old names
import xml.sax
import xml.sax.handler
HandlerBase=xml.sax.handler.ContentHandler
from xml.sax import make_parser

import os
from os import system
try:
    import subprocess
except:
    from compat import subprocess
import re


def reexec_with_pythonw():
    if sys.platform == 'darwin' and\
        not sys.executable.endswith('MacOS/Python'):
        print >>sys.stderr,'re-executing using pythonw'
        os.execvp('pythonw',['pythonw',__file__] + sys.argv[1:])

reexec_with_pythonw()

ID_RUN    = 10
ID_CANCEL = 11
ID_PARAM_START = 800
ID_FLAG_START  = 900
ID_MULTI_START = 1000

ID_ABOUT = 101
ID_ABOUT_COMMAND = 102
ID_EXIT  = 103

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
        self.inGispromptContent = 0
        self.inGuisection = 0

    def startElement(self, name, attrs):
        global grass_task

        if name == 'task':
            grass_task['name'] = attrs.get('name', None)

        if name == 'parameter':
            self.inParameter = 1;
            self.param_description = ''
            self.param_default = ''
            self.param_values = []
            self.param_gisprompt = False
            self.param_age = ''
            self.param_element = ''
            self.param_prompt = ''
            self.param_guisection = ''
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

        if name == 'gisprompt':
            self.param_gisprompt = True
            self.param_age = attrs.get('age', None)
            self.param_element = attrs.get('element', None)
            self.param_prompt = attrs.get('prompt', None)

        if name == 'guisection':
            self.inGuisection = 1
            self.param_guisection = ''

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
        if self.inGuisection:
            self.param_guisection = self.param_guisection + ch

    def endElement(self, name):
        global grass_task
        # If it's not a parameter element, ignore it
        if name == 'parameter':
            self.inParameter = 0;
            grass_task['params'].append({
                "name" : self.param_name,
                "type" : self.param_type,
                "required" : self.param_required,
                "multiple" : self.param_multiple,
                "description" : self.param_description,
                'gisprompt' : self.param_gisprompt,
                'age' : self.param_age,
                'element' :self.param_element,
                'prompt' : self.param_prompt,
                "guisection" : self.param_guisection,
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

        if name == 'guisection':
            self.param_guisection = normalize_whitespace(self.param_guisection)
            self.inGuisection = 0

class mainFrame(wx.Frame):
    """This is the Frame containing the dialog for options input."""
    def __init__(self, parent, ID, w, h, get_dcmd=None, layer=None):
        global grass_task
        wx.Frame.__init__(self, parent, ID, grass_task['name'],
            wx.DefaultPosition, style=wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)

        self.CreateStatusBar()
        self.SetStatusText("Enter parameters for " + grass_task['name'])
        self.parent = parent
        self.selection = '' #selection from GIS element selector
        self.paramdict = {} # dictionary of controls and their parameter values
        self.get_dcmd = get_dcmd
        self.layer = layer

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

        is_section = {}
        for task in grass_task['params']+grass_task['flags']:
            if task.has_key('guisection') and task['guisection'] != '':
                is_section[task['guisection']] = 1
        sections = is_section.keys()
        sections.sort()

        nbStyle=FN.FNB_NO_X_BUTTON|FN.FNB_NO_NAV_BUTTONS
        self.notebook = FN.FlatNotebook(self, id=wx.ID_ANY, style=nbStyle|wx.EXPAND)
        self.tab = {}
        self.tabsizer = {}
        for section in ['Main']+sections:
            self.tab[section] = wx.ScrolledWindow(self.notebook, id = wx.ID_ANY )
            self.tabsizer[section] = wx.BoxSizer(wx.VERTICAL)
            self.notebook.AddPage( self.tab[section], text = section )

        self.guisizer.Add( self.notebook, flag=wx.EXPAND )

        for p_count in range(0,len(grass_task['params'])):
            p = grass_task['params'][p_count]
            if p.has_key('guisection') and p['guisection'] != '':
                which_sizer = self.tabsizer[ p['guisection'] ]
                which_panel = self.tab[ p['guisection'] ]
            else:
                which_sizer = self.tabsizer[ 'Main' ]
                which_panel = self.tab[ 'Main' ]
            title = escape_ampersand(p['description'])
            if p['required'] == 'no':
                title = "[optional] " + title
            if p['multiple'] == 'yes' and len( p['values'] ) == 0:
                title = "[multiple] " + title
            p['value'] = p['default']
            if (len(p['values']) > 0):

                valuelist=map(str,p['values'])
                if p['multiple'] == 'yes':
                    hSizer=wx.StaticBoxSizer( wx.StaticBox(which_panel,0,title+":"),
                                              wx.HORIZONTAL )
                    v_count = 0
                    isDefault = {}
                    for defval in p['value'].split(','):
                        isDefault[ defval ] = 'yes'
                    for val in valuelist:
                        idForWX =  ID_MULTI_START + p_count*20 + v_count
                        chkbox = wx.CheckBox( which_panel, idForWX, val+" " )
                        if isDefault.has_key(val): chkbox.SetValue( True )
                        hSizer.Add( chkbox,0,wx.ADJUST_MINSIZE,0 )
                        wx.EVT_CHECKBOX(self, idForWX, self.EvtCheckBoxMulti)
                        v_count += 1
                    which_sizer.Add( hSizer, 0, wx.ADJUST_MINSIZE |wx.EXPAND, 5)
                else:
                    txt1 = wx.StaticText(which_panel, -1, title + ':', wx.Point(-1, -1), wx.Size(-1, -1))
                    which_sizer.Add(txt1, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)
                    self.cb = wx.ComboBox(which_panel, -1, p['default'],
                                     wx.Point(-1, -1), wx.Size(STRING_ENTRY_WIDTH, -1),
                                     valuelist, wx.CB_DROPDOWN)
                    which_sizer.Add(self.cb, 0, wx.ADJUST_MINSIZE, 5)
                    self.paramdict[self.cb] = ID_PARAM_START + p_count
                    self.cb.Bind( wx.EVT_COMBOBOX, self.EvtComboBox)

            if (p['type'] in ('string','integer','float') and
                len(p['values']) == 0 and
                (p['gisprompt'] == False and p['prompt'] != 'color')):
#                (p['gisprompt'] == False or p['prompt'] == 'color')):

                txt2 = wx.StaticText(which_panel, -1, title + ':',
                    wx.Point(-1, -1), wx.Size(-1, -1))
                which_sizer.Add(txt2, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)

                self.txt3 = wx.TextCtrl(which_panel, -1,
                    p['default'], wx.Point(-1, -1),
                    wx.Size(STRING_ENTRY_WIDTH, ENTRY_HEIGHT))
                which_sizer.Add(self.txt3, 0, wx.ADJUST_MINSIZE| wx.ALL, 5)
                self.paramdict[self.txt3] = ID_PARAM_START + p_count
                self.txt3.Bind(wx.EVT_TEXT, self.EvtText)

            if p['type'] == 'string' and p['gisprompt'] == True:
                txt4 = wx.StaticText(which_panel, -1, title + ':',
                    wx.Point(-1, -1), wx.Size(-1, -1))
                which_sizer.Add(txt4, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)

                if p['prompt'] != 'color':
                    self.selection = select.Select(which_panel, id=wx.ID_ANY, size=(250,-1),
                                                   type=p['element'])
                    which_sizer.Add(self.selection, 0, wx.ADJUST_MINSIZE, 5)
                    self.paramdict[self.selection] = ID_PARAM_START + p_count
                    self.selection.Bind(wx.EVT_TEXT, self.EvtText)
                elif p['prompt'] == 'color':
                    # TODO: color selector
                    pass
#                    btn_colour = csel.ColourSelect(which_panel, -1, "Select color", (200, 200, 200), size = (-1,-1))
#                    which_sizer.Add(btn_colour, 0, wx.ADJUST_MINSIZE, 5)
#                    self.paramdict[btn_colour] = ID_PARAM_START + p_count
#                    self.Bind(csel.EVT_COLOURSELECT, self.onColorButton, btn_colour)

        for f_count in range(0, len(grass_task['flags'])):
            f = grass_task['flags'][f_count]
            if f.has_key('guisection') and f['guisection'] != '':
                which_sizer = self.tabsizer[ f['guisection'] ]
                which_panel = self.tab[ f['guisection'] ]
            else:
                which_sizer = self.tabsizer[ 'Main' ]
                which_panel = self.tab[ 'Main' ]
            title = escape_ampersand(f['description'])
            self.chk = wx.CheckBox(which_panel,-1, title,
                wx.Point(-1, -1), wx.Size(-1, -1), wx.NO_BORDER)
            which_sizer.Add(self.chk, 0, wx.EXPAND, 5)
            self.paramdict[self.chk] = ID_FLAG_START + f_count
            self.chk.Bind(wx.EVT_CHECKBOX, self.EvtCheckBox)

        btnsizer = wx.BoxSizer(wx.HORIZONTAL)
        self.btn_cancel = wx.Button(self, ID_CANCEL, "Cancel")
        btnsizer.Add(self.btn_cancel, 0, wx.ALL| wx.ALIGN_CENTER, 10)
        if self.get_dcmd is not None: # A callback has been set up
            self.btn_apply = wx.Button(self, ID_RUN, "Apply")
            btnsizer.Add(self.btn_apply, 0, wx.ALL| wx.ALIGN_CENTER, 10)
            self.btn_ok = wx.Button(self, ID_RUN, "OK")
            btnsizer.Add(self.btn_ok, 0, wx.ALL| wx.ALIGN_CENTER, 10)
            self.btn_ok.SetDefault()
            self.btn_apply.Bind(wx.EVT_BUTTON, self.OnApply)
            self.btn_ok.Bind(wx.EVT_BUTTON, self.OnOK)
        else: # We're standalone
            self.btn_run = wx.Button(self, ID_RUN, "Run")
            btnsizer.Add(self.btn_run, 0, wx.ALL| wx.ALIGN_CENTER, 10)
            self.btn_run.SetDefault()
            self.btn_run.Bind(wx.EVT_BUTTON, self.OnRun)
        self.guisizer.Add(btnsizer, 0, wx.EXPAND)
        wx.EVT_MENU(self, ID_ABOUT, self.OnAbout)
        wx.EVT_MENU(self, ID_ABOUT_COMMAND, self.OnAboutCommand)
        wx.EVT_MENU(self, ID_EXIT,  self.OnCancel)
        self.btn_cancel.Bind(wx.EVT_BUTTON, self.OnCancel)
        self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)

        for section in ['Main']+sections:
            self.tabsizer[section].SetSizeHints( self.tab[section] )
            self.tabsizer[section].Fit( self.tab[section] )
            self.tab[section].SetAutoLayout(True)
            self.tab[section].SetSizer( self.tabsizer[section] )

        self.guisizer.SetSizeHints(self)
        self.SetAutoLayout(True)
        self.SetSizer(self.guisizer)
        self.guisizer.Fit(self)
#        self.guisizer.SetSizeHints( self )
#        self.Layout()

    def onColorButton(self, event):
        data = event.GetValue()
        color = str(data[0])+':'+str(data[1])+':'+str(data[2])
        print 'color = ',color
        self.getValues()


    def getValues(self):
        for item in self.paramdict.items():
            param_num = item[1]

            if 'CheckBox' in str(item[0]):
                tasktype = 'flags'
                num = param_num-ID_FLAG_START
                param_val = item[0].GetValue()
            elif 'ColourSelect' in str(item[0]):
                data = item[0].GetValue()
                param_val = str(data[0])+':'+str(data[1])+':'+str(data[2])
            else:
                tasktype = 'params'
                num = param_num-ID_PARAM_START
                param_val = item[0].GetValue()
            grass_task[tasktype][num]['value'] = param_val

    def EvtText(self, event):
        self.getValues()

    def EvtCheckBox(self, event):
        self.getValues()

    def EvtComboBox(self, event):
        self.getValues()

    def EvtCheckBoxMulti(self, event):
        """Fill the values ,-separated string according to current status of the checkboxes."""
        theParamId = (event.GetId()-ID_MULTI_START ) / 20
        theCheckedId = (event.GetId()-ID_MULTI_START ) % 20
        # Unpack current value list
        currentValues={}
        for isThere in grass_task['params'][theParamId]['value'].split(','):
            currentValues[isThere] = 1
        theValue = grass_task['params'][theParamId]['values'][theCheckedId]
        if event.Checked():
            currentValues[ theValue ] = 1
        else:
            del currentValues[ theValue ]
        # Pack it back
        grass_task['params'][theParamId]['value'] = ','.join( currentValues.keys() )

    def createCmd(self):
        """Produce a command line string for feeding into GRASS."""
        cmd = grass_task['name']
        errors = 0
        errStr = ""

        for p_count in range(0, len(grass_task['params'])):
            p = grass_task['params'][p_count]
            if (p['type'] != 'flag' and p['value'] == '' and p['required'] != 'no'):
                errStr += "Parameter " + p['name'] + "(" + p['description'] + ") is missing\n"
                errors += 1
            if (p['type'] == 'flag'):
                if (grass_task['params'][p_count]['value'] == 'checked'):
                    cmd += ' -' + grass_task['params'][p_count]['name']
            if (p['type'] != 'flag' and p['value'] != ''):
                cmd += ' ' + p['name'] + '=' + p['value']
        if errors:
            self.OnError(errStr)
            return None

        return cmd

    def OnOK(self, event):
        cmd = self.OnApply(event)
        if cmd is not None and self.get_dcmd is not None:
            self.OnCancel(event)

    def OnApply(self, event):
        cmd = self.createCmd()

        if cmd is not None and self.get_dcmd is not None:
            # return d.* command to layer tree for rendering
            self.get_dcmd(cmd, self.layer)
            # echo d.* command to output console
            self.parent.writeDCommand(cmd)
        return cmd

    def OnRun(self, event):
        cmd = self.createCmd()

        if cmd != None and cmd[0:2] != "d.":
             # Send any non-display command to parent window (probably gism.py)
            if self.parent > -1:
                # put to parents
                try:
                    self.parent.goutput.runCmd(cmd)
                except AttributeError,e:
                    print >>sys.stderr, "%s: Propably not running in gism.py session?" % (e)
                    print >>sys.stderr, "parent window is: %s" % (str(self.parent))
            # Send any other command to the shell.
            else:
                try:
                    retcode = subprocess.call(cmd, shell=True)
                    if retcode < 0:
                        print >>sys.stderr, "Child was terminated by signal", -retcode
                    elif retcode > 0:
                        print >>sys.stderr, "Child returned", retcode
                except OSError, e:
                    print >>sys.stderr, "Execution failed:", e

    def OnError(self, errMsg):
        dlg = wx.MessageDialog(self, errMsg, "Error", wx.OK | wx.ICON_ERROR)
        dlg.ShowModal()
        dlg.Destroy()

    def OnCancel(self, event):
        self.Close(True)

    def OnCloseWindow(self, event):
        global grass_task
        grass_task = { 'name' : 'unknown',
            'description' : 'No description available.',
            'lines' : 0, 'params' : [], 'flags' : [] }
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
        global grass_task
        grass_task = { 'name' : 'unknown',
            'description' : 'No description available.',
            'lines' : 0, 'params' : [], 'flags' : [] }
        self.w = HSPACE + STRING_ENTRY_WIDTH + HSPACE
        self.h = MENU_HEIGHT + VSPACE + grass_task['lines'] * ENTRY_HEIGHT + VSPACE + BUTTON_HEIGHT + VSPACE + STATUSBAR_HEIGHT
        self.frame = mainFrame(None, -1, self.w, self.h)
        self.frame.Show(True)
        self.SetTopWindow(self.frame)
        return True

class GUI:
    def __init__(self,parent=-1):
        '''Parses GRASS commands when module is imported and used
        from gism.py'''
        global grass_task
        grass_task = { 'name' : 'unknown',
            'description' : 'No description available.',
            'lines' : 0, 'params' : [], 'flags' : [] }
        self.w = HSPACE + STRING_ENTRY_WIDTH + HSPACE
        self.h = MENU_HEIGHT + VSPACE + grass_task['lines'] * ENTRY_HEIGHT + VSPACE + BUTTON_HEIGHT + VSPACE + STATUSBAR_HEIGHT
        self.parent = parent

    def parseCommand(self, cmd, gmpath, completed=None, parentframe=-1 ):
        if completed == None:
            self.get_dcmd = None
            layer = None
        else:
            self.get_dcmd = completed[0]
            layer = completed[1]
        cmdlst = []
        cmdlst = cmd.split(' ')

        if parentframe > -1:
            self.parent = parentframe

        if len(cmdlst) > 1:
            print "usage: %s <grass command> " % cmdlst[0]
        else:
            # parse the interface decription
            cmd = cmd + r' --interface-description'
            cmdout = os.popen(cmd, "r").read()
            p = re.compile( '(grass-interface.dtd)')
            cmdout2 = p.sub( gmpath+r'/grass-interface.dtd', cmdout)
            handler = processTask()
            xml.sax.parseString(cmdout2, handler)

        mf = mainFrame(self.parent ,-1, self.w, self.h, self.get_dcmd, layer)
#        mf = mainFrame(None, self.parent , self.w, self.h, self.get_dcmd, layer)
        mf.Show(True)

if __name__ == "__main__":
    # Just for testing purposes

    # Create the application
    if len(sys.argv) != 2:
        print "Usage: %s <grass command>" % sys.argv[0]
        sys.exit()
    app = GrassGUIApp(0)
    # Parsing if run from command line: find out the command to run
    gui = GUI(app.frame)
    gui.parseCommand( sys.argv[1], os.getenv("GISBASE") + "/etc/wx/Gism" )
    app.MainLoop()

