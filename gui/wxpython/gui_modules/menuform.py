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
import wx.lib.colourselect as csel

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

ID_PARAM_START = 800
ID_FLAG_START  = 900
ID_MULTI_START = 1000

ID_ABOUT_COMMAND = 102

VSPACE = 4
HSPACE = 4
MENU_HEIGHT = 30
STATUSBAR_HEIGHT = 30
ENTRY_HEIGHT = 20
STRING_ENTRY_WIDTH = 300
BUTTON_HEIGHT = 44
BUTTON_WIDTH = 100

t_colors = "red,orange,yellow,green,blue,indigo,violet,white,black,gray,brown,magenta,aqua,grey,cyan,purple"
t_rgb = ( # From lib/gis/col_str.c
  (255,  0,  0),
  (255,128,  0),
  (255,255,  0),
  (  0,255,  0),
  (  0,  0,255),
  (  0,128,255),
  (128,  0,255),
  (255,255,255),
  (  0,  0,  0),
  (128,128,128),
  (180, 77, 25),
  (255,  0,255),
  (100,128,255),
  (128,128,128),
  (  0,255,255),
  (128,  0,128)
)
t_color = t_colors.split(',')
color_str2rgb = {}
color_rgb2str = {}
for c in range(0,len(t_rgb)):
    color_str2rgb[ t_color[c] ] = t_rgb[ c ]
    color_rgb2str[ t_rgb[ c ] ] = t_color[ c ]


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

class grass_task:
    pass

def grass_task_init():
    grass_task.name = 'unknown'
    grass_task.params = []
    grass_task.description = ''
    grass_task.flags = []
grass_task_init()

class processTask(HandlerBase):
    def __init__(self):
        self.inDescriptionContent = 0
        self.inDefaultContent = 0
        self.inValueContent = 0
        self.inParameter = 0
        self.inFlag = 0
        self.inGispromptContent = 0
        self.inGuisection = 0
	grass_task_init()

    def startElement(self, name, attrs):

        if name == 'task':
            grass_task.name = attrs.get('name', None)

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
            self.param_required = attrs.get('required', None)
            self.param_multiple = attrs.get('multiple', None)

        if name == 'flag':
            self.inFlag = 1;
            self.flag_description = ''
            self.flag_default = ''
            self.flag_values = []
            # Look for the flag name
            self.flag_name = attrs.get('name', None)

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
        # If it's not a parameter element, ignore it
        if name == 'parameter':
            self.inParameter = 0;
            grass_task.params.append({
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
            grass_task.flags.append({
                "name" : self.flag_name,
                "description" : self.flag_description } )

        if name == 'description':
            if self.inParameter:
                self.param_description = normalize_whitespace(self.description)
            elif self.inFlag:
                self.flag_description = normalize_whitespace(self.description)
            else:
                grass_task.description = normalize_whitespace(self.description)
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
    """This is the Frame containing the dialog for options input.

    The dialog is organized in a notebook according to the guisections
    defined by each GRASS command."""
    def __init__(self, parent, ID, get_dcmd=None, layer=None, dcmd_params=None):
        wx.Frame.__init__(self, parent, ID, grass_task.name,
            wx.DefaultPosition, style=wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)

        self.CreateStatusBar()
        self.SetStatusText("Enter parameters for " + grass_task.name + " (those in Main are required)")
        self.parent = parent
        self.selection = '' #selection from GIS element selector
        self.paramdict = {} # dictionary of controls and their parameter values
        self.get_dcmd = get_dcmd
        self.dcmd_params = dcmd_params #this should be passed from the layer tree eventually
        self.layer = layer

        menu = wx.Menu()
        menu.Append(wx.ID_ABOUT, "&About GrassGUI",
            "Information about GrassGUI")
        menu.Append(ID_ABOUT_COMMAND, "&About " + grass_task.name,
            "Short descripton of GRASS command " + grass_task.name)
        menu.AppendSeparator()
        menu.Append(wx.ID_EXIT, "E&xit", "Terminate the program")

        menuBar = wx.MenuBar()
        menuBar.Append(menu, "&File");

        self.SetMenuBar(menuBar)
        self.guisizer = wx.BoxSizer(wx.VERTICAL)

        sections = ['Main']
        is_section = {}
        for task in grass_task.params + grass_task.flags:
            if not task.has_key('guisection') or task['guisection']=='':
                task['guisection'] = 'Options'
            if not is_section.has_key(task['guisection']):
                is_section[task['guisection']] = 1
                sections.append( task['guisection'] )
        there_is_main = False
        for i in grass_task.params+grass_task.flags:
            if i.has_key('required') and i['required'] == 'yes':
                i['guisection'] = 'Main'
                there_is_main = True
        if not there_is_main:
            sections = sections[1:]


        self.notebookpanel = wx.ScrolledWindow( self, id=wx.ID_ANY )
        self.notebookpanel.SetScrollRate(10,10)
        self.panelsizer = wx.BoxSizer(wx.VERTICAL)

        nbStyle=FN.FNB_NO_X_BUTTON|FN.FNB_NO_NAV_BUTTONS|FN.FNB_VC8|FN.FNB_BACKGROUND_GRADIENT
        self.notebook = FN.FlatNotebook(self.notebookpanel, id=wx.ID_ANY, style=nbStyle)
        self.notebook.SetTabAreaColour(wx.Colour(125,200,175))
        self.notebook.Bind( FN.EVT_FLATNOTEBOOK_PAGE_CHANGED, self.OnPageChange )
        self.tab = {}
        self.tabsizer = {}
        is_first = True
        for section in sections:
            self.tab[section] = wx.Panel(self.notebook, id = wx.ID_ANY )
            self.tabsizer[section] = wx.BoxSizer(wx.VERTICAL)
            self.notebook.AddPage( self.tab[section], text = section, select = is_first )
            is_first = False

        self.panelsizer.Add( self.notebook, flag=wx.EXPAND )
        self.guisizer.Add( self.notebookpanel, flag = wx.EXPAND )

        p_count = -1
        for p in grass_task.params:
            p_count += 1 # Needed for checkboxes hack
            which_sizer = self.tabsizer[ p['guisection'] ]
            which_panel = self.tab[ p['guisection'] ]
            title = escape_ampersand(p['description'])
            text_style = wx.FONTWEIGHT_BOLD
            txt = None
            if p['required'] == 'no':
                text_style = wx.FONTWEIGHT_NORMAL
            if p['multiple'] == 'yes' and len( p['values'] ) == 0:
                title = "[multiple] " + title
            p['value'] = p['default']
            # inserting existing values from d.* command in layer tree
            if self.dcmd_params != None:
                for dparam in self.dcmd_params:
                    if p == dparam:
                        p['value'] = self.dcmd_params[dparam]

            if (len(p['values']) > 0):

                valuelist=map(str,p['values'])
                if p['multiple'] == 'yes':
                    txt = wx.StaticBox(which_panel,0,title+":")
                    hSizer=wx.StaticBoxSizer( txt, wx.HORIZONTAL )
                    v_count = 0
                    isDefault = {}
                    for defval in p['value'].split(','):
                        isDefault[ defval ] = 'yes'
                    for val in valuelist:
                        # This is the checkboxes hack
                        idForWX =  ID_MULTI_START + p_count*20 + v_count
                        chkbox = wx.CheckBox( which_panel, idForWX, val+" " )
                        if isDefault.has_key(val): chkbox.SetValue( True )
                        hSizer.Add( chkbox,0,wx.ADJUST_MINSIZE,5 )
                        self.Bind(wx.EVT_CHECKBOX, self.EvtCheckBoxMulti)
                        v_count += 1
                    which_sizer.Add( hSizer, 0, wx.ADJUST_MINSIZE, 5)
                else:
                    txt = wx.StaticText(which_panel, label = title + ':' )
                    which_sizer.Add(txt, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)
                    self.cb = wx.ComboBox(which_panel, -1, p['default'],
                                     wx.Point(-1, -1), wx.Size(STRING_ENTRY_WIDTH, -1),
                                     valuelist, wx.CB_DROPDOWN)
                    which_sizer.Add(self.cb, 0, wx.ADJUST_MINSIZE, 5)
                    self.paramdict[self.cb] = ID_PARAM_START + p_count
                    self.cb.Bind( wx.EVT_COMBOBOX, self.EvtComboBox)

            if (p['type'] in ('string','integer','float')
                and len(p['values']) == 0
                and p['gisprompt'] == False
                and p['prompt'] != 'color'):

                txt = wx.StaticText(which_panel, label = title + ':' )
                which_sizer.Add(txt, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)

                self.txt3 = wx.TextCtrl(which_panel, value = p['default'],
                    size = (STRING_ENTRY_WIDTH, ENTRY_HEIGHT))
                which_sizer.Add(self.txt3, 0, wx.ADJUST_MINSIZE| wx.ALL, 5)
                self.paramdict[self.txt3] = ID_PARAM_START + p_count
                self.txt3.Bind(wx.EVT_TEXT, self.EvtText)

            if p['type'] == 'string' and p['gisprompt'] == True:
                txt = wx.StaticText(which_panel, label = title + ':')
                which_sizer.Add(txt, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)
                if p['prompt'] != 'color':
                    self.selection = select.Select(which_panel, id=wx.ID_ANY, size=(250,-1),
                                                   type=p['element'])
                    which_sizer.Add(self.selection, 0, wx.ADJUST_MINSIZE| wx.ALL, 5)
                    self.paramdict[self.selection] = ID_PARAM_START + p_count
                    self.selection.Bind(wx.EVT_TEXT, self.EvtText)
                elif p['prompt'] == 'color':
                    if p['default'] != '':
                        if p['default'][0] in "0123456789":
                            default_color = tuple(map(int,p['default'].split( ':' )))
                            label_color = p['default']
                        else:
                            # Convert color names to RGB
                            try:
                                default_color = color_str2rgb[ p['default'] ]
                                label_color = p['default']
                            except KeyError:
                                default_color = (200,200,200)
                                label_color = 'Select Color'
                    else:
                        default_color = (200,200,200)
                        label_color = 'Select Color'
                    btn_colour = csel.ColourSelect(which_panel, -1, label_color, default_color, wx.DefaultPosition, (150,-1) )
                    which_sizer.Add(btn_colour, 0, wx.ADJUST_MINSIZE| wx.ALL, 5)
                    self.paramdict[btn_colour] = ID_PARAM_START + p_count
                    self.Bind(csel.EVT_COLOURSELECT, self.OnColorButton, btn_colour)
	    if txt is not None:
                txt.SetFont( wx.Font( 12, wx.FONTFAMILY_DEFAULT, wx.NORMAL, text_style, 0, ''))

        f_count = -1
        for f in grass_task.flags:
            f_count += 1
            which_sizer = self.tabsizer[ f['guisection'] ]
            which_panel = self.tab[ f['guisection'] ]
            title = escape_ampersand(f['description'])
            self.chk = wx.CheckBox(which_panel,-1, label = title, style = wx.NO_BORDER)
            self.chk.SetFont( wx.Font( 12, wx.FONTFAMILY_DEFAULT, wx.NORMAL, text_style, 0, ''))
            which_sizer.Add(self.chk, 0, wx.EXPAND| wx.ALL, 5)
            self.paramdict[self.chk] = ID_FLAG_START + f_count
            self.chk.Bind(wx.EVT_CHECKBOX, self.EvtCheckBox)


        btnsizer = wx.BoxSizer(wx.HORIZONTAL)
        self.btn_cancel = wx.Button(self, wx.ID_CANCEL, "Cancel")
        btnsizer.Add(self.btn_cancel, 0, wx.ALL| wx.ALIGN_CENTER, 10)
        if self.get_dcmd is not None: # A callback has been set up
            self.btn_apply = wx.Button(self, wx.ID_APPLY, "Apply")
            btnsizer.Add(self.btn_apply, 0, wx.ALL| wx.ALIGN_CENTER, 10)
            self.btn_ok = wx.Button(self, wx.ID_OK, "OK")
            btnsizer.Add(self.btn_ok, 0, wx.ALL| wx.ALIGN_CENTER, 10)
            self.btn_ok.SetDefault()
            self.btn_apply.Bind(wx.EVT_BUTTON, self.OnApply)
            self.btn_ok.Bind(wx.EVT_BUTTON, self.OnOK)
        else: # We're standalone
            self.btn_run = wx.Button(self, wx.ID_OK, "Run")
            btnsizer.Add(self.btn_run, 0, wx.ALL| wx.ALIGN_CENTER, 10)
            self.btn_run.SetDefault()
            self.btn_run.Bind(wx.EVT_BUTTON, self.OnRun)
        self.guisizer.Add(btnsizer, 0, wx.ALIGN_BOTTOM)
        wx.EVT_MENU(self, wx.ID_ABOUT, self.OnAbout)
        wx.EVT_MENU(self, ID_ABOUT_COMMAND, self.OnAboutCommand)
        wx.EVT_MENU(self, wx.ID_EXIT,  self.OnCancel)
        self.btn_cancel.Bind(wx.EVT_BUTTON, self.OnCancel)
        self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)

        maxsizes = (0,0)
        for section in sections:
            self.tabsizer[section].SetSizeHints( self.tab[section] )
            self.tabsizer[section].Fit( self.tab[section] )
            self.tab[section].SetAutoLayout(True)
            self.tab[section].SetSizer( self.tabsizer[section] )
            self.tab[section].Layout()
            minsecsizes = self.tabsizer[section].GetMinSize()
            maxsizes = map( lambda x: max( maxsizes[x], minsecsizes[x] ), (0,1) )

        self.notebookpanel.SetSize( (min(600, maxsizes[0]), min(600, maxsizes[1]+60) ) ) # 60 takes the tabbar into account
        self.notebookpanel.SetSizer(self.panelsizer)
 
        self.guisizer.SetSizeHints(self)
        self.SetAutoLayout(True)
        self.SetSizer(self.guisizer)
        self.Layout()


    def OnPageChange(self, event):
        self.Layout()

    def OnColorButton(self, event):
        colorchooser = wx.FindWindowById( event.GetId() )
        new_color = colorchooser.GetValue()
        # This is weird: new_color is a 4-tuple and new_color[:] is a 3-tuple
        # under wx2.8.1
        new_label = color_rgb2str.get( new_color[:], ':'.join(map(str,new_color)) )
        colorchooser.SetLabel( new_label )
        colorchooser.SetColour( new_color )
        colorchooser.Refresh()
        self.getValues()

    def updateStatusLine(self):
        self.SetStatusText( self.createCmd(ignoreErrors = True) )

    def getValues(self):
        for item in self.paramdict.items():
            param_num = item[1]
            if 'CheckBox' in str(item[0]):
                tasktype = grass_task.flags
                num = param_num-ID_FLAG_START
                param_val = item[0].GetValue()
            else:
                tasktype = grass_task.params
                num = param_num-ID_PARAM_START
                if 'ColourSelect' in str(item[0]):
                    data = item[0].GetValue()
                    param_val = str(data[0])+':'+str(data[1])+':'+str(data[2])
                else:
                    param_val = item[0].GetValue()
            tasktype[num]['value'] = param_val
        self.updateStatusLine()
        
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
        for isThere in grass_task.params[theParamId]['value'].split(','):
            currentValues[isThere] = 1
        theValue = grass_task.params[theParamId]['values'][theCheckedId]
        if event.Checked():
            currentValues[ theValue ] = 1
        else:
            del currentValues[ theValue ]
        # Pack it back
        grass_task.params[theParamId]['value'] = ','.join( currentValues.keys() )
        self.updateStatusLine()

    def createCmd(self, ignoreErrors = False):
        """Produce a command line string for feeding into GRASS."""
        cmd = grass_task.name
        errors = 0
        errStr = ""
        for flag in grass_task.flags:
            if 'value' in flag and flag['value']:
                cmd += ' -' + flag['name']
        for p in grass_task.params:
            if p['value'] == '' and p['required'] != 'no':
                cmd += ' ' + p['name'] + '=' + '<required>'
                errStr += "Parameter " + p['name'] + "(" + p['description'] + ") is missing\n"
                errors += 1
            if p['value'] != '' and p['value'] != p['default'] :
                cmd += ' ' + p['name'] + '=' + p['value']
        if errors and not ignoreErrors:
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
             # Send any non-display command to parent window (probably wxgui.py)
            if self.parent > -1:
                # put to parents
                try:
                    self.parent.goutput.runCmd(cmd)
                except AttributeError,e:
                    print >>sys.stderr, "%s: Propably not running in wxgui.py session?" % (e)
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
        for t in self.tab.values(): t = None
        for t in self.tabsizer.values(): t.Clear(True)
        self.notebook.Destroy()
        self.guisizer.Clear(True)
        self.Destroy()

    def OnCloseWindow(self, event):
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
            grass_task.name+": "+grass_task.description,
            "About " + grass_task.name,
            wx.OK | wx.ICON_INFORMATION)
        dlg.ShowModal()
        dlg.Destroy()


class GrassGUIApp(wx.App):
    def OnInit(self):
        self.frame = mainFrame(None, -1)
        self.frame.Show(True)
        self.SetTopWindow(self.frame)
        return True

class GUI:
    def __init__(self, parent=-1):
        '''Parses GRASS commands when module is imported and used
        from wxgui.py'''
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

        mf = mainFrame(self.parent ,-1, self.get_dcmd, layer)
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
    gui.parseCommand( sys.argv[1], os.getenv("GISBASE") + "/etc/wx/gui_modules")
    app.MainLoop()

