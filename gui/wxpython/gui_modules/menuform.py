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
# The XML stream is read from executing the command given in the
# command line, thus you may call it for instance this way:
#
# python <this file.py> r.basins.fill
#
# Or you set an alias or wrap the call up in a nice
# shell script, GUI environment ... please contribute your idea.
#
# Updated to wxPython 2.6 syntax. Methods added to make it callable by gui.
# Method added to automatically re-run with pythonw on a Mac.
#
# TODO:
#
# - verify option value types
# - add tooltips
"""
__version__ ="$Date: 2006/08/06 21:21:01 $"

import wx
import sys
import string
import textwrap
import select
import wx.lib.flatnotebook as FN
import wx.lib.colourselect as csel
import wx.html

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
    sys.path.append(os.path.join(os.getenv("GISBASE"),"etc","wx"))
    from compat import subprocess
import re

imagepath = os.getenv("GISBASE") + "/etc/wx/images/"
#imagepath = os.sep.join( os.getcwd().split(os.sep) [:-1] + ['images'] )
sys.path.append(imagepath)


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

def text_beautify( someString ):
    "Make really long texts shorter"
    # TODO: remove magic number (calculate a correct value from
    # pixelSize of text and the magic number for maximum size
    return escape_ampersand( "\n".join( textwrap.wrap( normalize_whitespace(someString), 72 ) ) )

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

class grassTask:
    """This class holds the structures needed for both filling by the parser and
    use by the interface constructor."""
    def __init__(self):
        self.name = 'unknown'
        self.params = []
        self.description = ''
        self.flags = []

class processTask(HandlerBase):
    """A SAX handler for the --interface-description output, as
    defined in grass-interface.dtd. Extend or modify this and the
    DTD if the XML output of GRASS' parser is extended or modified."""
    def __init__(self, task_description):
        self.inDescriptionContent = 0
        self.inDefaultContent = 0
        self.inValueContent = 0
        self.inParameter = 0
        self.inFlag = 0
        self.inGispromptContent = 0
        self.inGuisection = 0
        self.task = task_description

    def startElement(self, name, attrs):

        if name == 'task':
            self.task.name = attrs.get('name', None)

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
            self.task.params.append({
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
            self.task.flags.append({
                "name" : self.flag_name,
                "description" : self.flag_description } )

        if name == 'description':
            if self.inParameter:
                self.param_description = normalize_whitespace(self.description)
            elif self.inFlag:
                self.flag_description = normalize_whitespace(self.description)
            else:
                self.task.description = normalize_whitespace(self.description)
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


class helpPanel(wx.html.HtmlWindow):
    """This panel holds the text from GRASS docs.

    GISBASE must be set in the environment to find the html docs dir.
    The SYNOPSIS section is skipped, since this Panel is supposed to
    be integrated into the cmdPanel."""
    def __init__(self, parent, id, grass_command = "index"):
        wx.html.HtmlWindow.__init__(self, parent, id)
        self.fspath = os.getenv( "GISBASE" ) + "/docs/html/"
        self.SetStandardFonts( size = 11 )
        self.fillContentsFromFile( self.fspath + grass_command + ".html" )

    def fillContentsFromFile( self, htmlFile ):
        aLink = re.compile( r'(<a href="?)(.+\.html?["\s]*>)', re.IGNORECASE )
        try:
            contents = [ '<head><base href="%s"></head>' % self.fspath ]
            dont_skip = True
            for l in file( htmlFile, "rb" ).readlines():
                if "DESCRIPTION" in l: dont_skip = True
                if dont_skip:
                    if "SYNOPSIS" in l: dont_skip = False # do skip the options description
                    else:
                        findLink = aLink.search( l )
                        if findLink is not None:
                            contents.append( aLink.sub(findLink.group(1)+self.fspath+findLink.group(2),l) )
                        else:
                            contents.append( l )
            self.SetPage( "".join( contents ) )
            self.Ok = True
        except:
            raise
            self.Ok = False


class mainFrame(wx.Frame):
    """This is the Frame containing the dialog for options input.

    The dialog is organized in a notebook according to the guisections
    defined by each GRASS command.

    If run with a parent, it may Apply, Ok or Cancel; the latter two close the dialog.
    The former two trigger a callback.

    If run standalone, it will allow execution of the command.

    The command is checked and sent to the clipboard when clicking "Copy". """
    def __init__(self, parent, ID, task_description, get_dcmd=None, layer=None, dcmd_params=None):

        self.get_dcmd = get_dcmd
        self.dcmd_params = dcmd_params #this should be passed from the layer tree eventually
        self.layer = layer
        self.task = task_description
        # inserting existing values from d.* command in layer tree
        for p in self.task.params:
            if self.dcmd_params != None:
                for dparam in self.dcmd_params:
                    if p == dparam:
                        p['value'] = self.dcmd_params[dparam]

        wx.Frame.__init__(self, parent, ID, self.task.name,
            wx.DefaultPosition, style=wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)

        self.CreateStatusBar()
        self.parent = parent
        self.SetIcon(wx.Icon(os.path.join(imagepath,'grass.form.gif'), wx.BITMAP_TYPE_ANY))

        menu = wx.Menu()
        menu.Append(wx.ID_ABOUT, "&About GrassGUI",
            "Information about GrassGUI")
        menu.Append(ID_ABOUT_COMMAND, "&About " + self.task.name,
            "Short descripton of GRASS command " + self.task.name)
        menu.AppendSeparator()
        menu.Append(wx.ID_EXIT, "E&xit", "Terminate the program")

        menuBar = wx.MenuBar()
        menuBar.Append(menu, "&File");

        self.SetMenuBar(menuBar)
        self.guisizer = wx.BoxSizer(wx.VERTICAL)
        
        self.notebookpanel = cmdPanel( self, self.task )
        self.guisizer.Add( self.notebookpanel, 1, flag = wx.EXPAND )

        status_text = "Enter parameters for " + self.task.name
        if self.notebookpanel.tab.has_key('Main'):
            # We have to wait for the notebookpanel to be filled in order
            # to know if there actually is a Main tab
            status_text += " (those of Main in bold typeface are required)"
        self.SetStatusText( status_text )

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
            self.btn_clipboard = wx.Button(self, wx.ID_OK, "Copy")
            btnsizer.Add(self.btn_clipboard, 0, wx.ALL| wx.ALIGN_CENTER, 10)
            self.btn_clipboard.Bind(wx.EVT_BUTTON, self.OnCopy)
        self.guisizer.Add(btnsizer, 0, wx.ALIGN_BOTTOM)
        wx.EVT_MENU(self, wx.ID_ABOUT, self.OnAbout)
        wx.EVT_MENU(self, ID_ABOUT_COMMAND, self.OnAboutCommand)
        wx.EVT_MENU(self, wx.ID_EXIT,  self.OnCancel)
        self.btn_cancel.Bind(wx.EVT_BUTTON, self.OnCancel)
        self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)

        constrained_size = self.notebookpanel.GetSize()
        self.notebookpanel.SetSize( (constrained_size[0],constrained_size[1]+80) ) # 80 takes the tabbar into account
        self.notebookpanel.SetSizer( self.notebookpanel.panelsizer )
        self.notebookpanel.Layout()

        self.guisizer.SetSizeHints(self)
        self.SetAutoLayout(True)
        self.SetSizer(self.guisizer)
        self.Layout()


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

    def OnCopy(self, event):
        cmddata = wx.TextDataObject()
        cmddata.SetText(self.createCmd(ignoreErrors=True))
        if wx.TheClipboard.Open():
            wx.TheClipboard.UsePrimarySelection(True)
            wx.TheClipboard.SetData(cmddata)
            wx.TheClipboard.Close()
            self.SetStatusText("'%s' copied to clipboard" %\
                            (self.createCmd(ignoreErrors=True)))

    def OnError(self, errMsg):
        dlg = wx.MessageDialog(self, errMsg, "Error", wx.OK | wx.ICON_ERROR)
        dlg.ShowModal()
        dlg.Destroy()

    def OnCancel(self, event):
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
            self.task.name+": "+self.task.description,
            "About " + self.task.name,
            wx.OK | wx.ICON_INFORMATION)
        dlg.ShowModal()
        dlg.Destroy()

    def createCmd(self, ignoreErrors = False):
        return self.notebookpanel.createCmd(ignoreErrors=ignoreErrors)


class cmdPanel(wx.Panel):
    """A panel containing a notebook dividing in tabs the different guisections of the GRASS cmd."""
    def __init__( self, parent, task, *args, **kwargs ):
        wx.Panel.__init__( self, parent, *args, **kwargs )

        self.task = task
        self.selection = '' #selection from GIS element selector
        self.paramdict = {} # dictionary of controls and their parameter values

        sections = ['Main']
        is_section = {}
        for task in self.task.params + self.task.flags:
            if not task.has_key('guisection') or task['guisection']=='':
                task['guisection'] = 'Options'
            if not is_section.has_key(task['guisection']):
                is_section[task['guisection']] = 1
                sections.append( task['guisection'] )
        there_is_main = False
        for i in self.task.params+self.task.flags:
            if i.has_key('required') and i['required'] == 'yes':
                i['guisection'] = 'Main'
                there_is_main = True
        if not there_is_main:
            sections = sections[1:]

        self.panelsizer = wx.BoxSizer(wx.VERTICAL)

        nbStyle=FN.FNB_NO_X_BUTTON|FN.FNB_NO_NAV_BUTTONS|FN.FNB_VC8|FN.FNB_BACKGROUND_GRADIENT
        self.notebook = FN.FlatNotebook( self, id=wx.ID_ANY, style=nbStyle)
        self.notebook.SetTabAreaColour(wx.Colour(125,200,175))
        self.notebook.Bind( FN.EVT_FLATNOTEBOOK_PAGE_CHANGED, self.OnPageChange )
        self.tab = {}
        self.tabsizer = {}
        is_first = True
        for section in sections:
            self.tab[section] = wx.ScrolledWindow(self.notebook, id = wx.ID_ANY )
            self.tab[section].SetScrollRate(10,10)
            self.tabsizer[section] = wx.BoxSizer(wx.VERTICAL)
            self.notebook.AddPage( self.tab[section], text = section, select = is_first )
            is_first = False

        manual_tab =  helpPanel( self.notebook, id = wx.ID_ANY, grass_command = self.task.name)
        if manual_tab.Ok:
            manual_tabsizer = wx.BoxSizer(wx.VERTICAL)
            self.notebook.AddPage( manual_tab, text = "Manual" , select = False )

        self.panelsizer.Add( self.notebook, 1, flag=wx.EXPAND )

        p_count = -1
        for p in self.task.params:
            p_count += 1 # Needed for checkboxes hack
            which_sizer = self.tabsizer[ p['guisection'] ]
            which_panel = self.tab[ p['guisection'] ]
            title = text_beautify(p['description'])
            text_style = wx.FONTWEIGHT_BOLD
            txt = None
            if p['required'] == 'no':
                text_style = wx.FONTWEIGHT_NORMAL
            if p['multiple'] == 'yes' and len( p['values'] ) == 0:
                title = "[multiple] " + title
            if p[ 'value'] ==  '' :
                p['value'] = p['default']
            if (len(p['values']) > 0):

                valuelist=map(str,p['values'])
                if p['multiple'] == 'yes':
                    txt = wx.StaticBox(which_panel,0,title+":")
                    hSizer=wx.StaticBoxSizer( txt, wx.VERTICAL )
                    v_count = 0
                    isDefault = {}
                    for defval in p['value'].split(','):
                        isDefault[ defval ] = 'yes'
                    for val in valuelist:
                        # This is the checkboxes hack
                        idForWX =  ID_MULTI_START + p_count*20 + v_count
                        chkbox = wx.CheckBox( which_panel, idForWX, text_beautify(val) )
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
        for f in self.task.flags:
            f_count += 1
            which_sizer = self.tabsizer[ f['guisection'] ]
            which_panel = self.tab[ f['guisection'] ]
            title = text_beautify(f['description'])
            self.chk = wx.CheckBox(which_panel,-1, label = title, style = wx.NO_BORDER)
            self.chk.SetFont( wx.Font( 12, wx.FONTFAMILY_DEFAULT, wx.NORMAL, text_style, 0, ''))
            which_sizer.Add(self.chk, 0, wx.EXPAND| wx.ALL, 5)
            self.paramdict[self.chk] = ID_FLAG_START + f_count
            self.chk.Bind(wx.EVT_CHECKBOX, self.EvtCheckBox)

        maxsizes = (0,0)
        for section in sections:
            self.tabsizer[section].SetSizeHints( self.tab[section] )
            self.tabsizer[section].Fit( self.tab[section] )
            self.tab[section].SetAutoLayout(True)
            self.tab[section].SetSizer( self.tabsizer[section] )
            self.tab[section].Layout()
            minsecsizes = self.tabsizer[section].GetMinSize()
            maxsizes = map( lambda x: max( maxsizes[x], minsecsizes[x] ), (0,1) )

        # TODO: be less arbitrary with these 600
        constrained_size = (min(600, maxsizes[0]), min(600, maxsizes[1]) )
        for section in sections:
            self.tab[section].SetMinSize( constrained_size )
        if manual_tab.Ok:
            manual_tab.SetMinSize( constrained_size )


    def OnPageChange(self, event):
        self.Layout()

    def OnColorButton(self, event):
        colorchooser = wx.FindWindowById( event.GetId() )
        new_color = colorchooser.GetValue()[:]
        # This is weird: new_color is a 4-tuple and new_color[:] is a 3-tuple
        # under wx2.8.1
        new_label = color_rgb2str.get( new_color, ':'.join(map(str,new_color)) )
        colorchooser.SetLabel( new_label )
        colorchooser.SetColour( new_color )
        colorchooser.Refresh()
        self.getValues()

    def updateStatusLine(self):
        """If we were part of a richer interface, report back the current command being built."""
        # TODO: don't tie this to a StatusLine
        try:
            self.GetParent().SetStatusText( self.createCmd(ignoreErrors = True) )
        except:
            pass

    def getValues(self):
        for (gui_object,param_num) in self.paramdict.items():
            if 'CheckBox' in str( gui_object ):
                tasktype = self.task.flags
                num = param_num-ID_FLAG_START
                param_val = gui_object.GetValue()
            else:
                tasktype = self.task.params
                num = param_num-ID_PARAM_START
                if 'ColourSelect' in str( gui_object ):
                    if 'Select' in gui_object.GetLabel():
                        param_val = ''
                    else:
                        data = gui_object.GetValue()[:]
                        param_val = color_rgb2str.get( data , ':'.join( map(str, data) ) )
                else:
                    param_val = gui_object.GetValue()
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
        for isThere in self.task.params[theParamId]['value'].split(','):
            currentValues[isThere] = 1
        theValue = self.task.params[theParamId]['values'][theCheckedId]
        if event.Checked():
            currentValues[ theValue ] = 1
        else:
            del currentValues[ theValue ]
        currentValueList=[] # Keep the original order, so that some defaults may be recovered
        for v in self.task.params[theParamId]['values']:
            if currentValues.has_key(v):
                currentValueList.append( v )
        # Pack it back
        self.task.params[theParamId]['value'] = ','.join( currentValueList )
        self.updateStatusLine()

    def createCmd(self, ignoreErrors = False):
        """Produce a command line string for feeding into GRASS.

        If ignoreErrors==True then it will return whatever has been
        built so far, even though it would not be a correct command
        for GRASS."""
        cmd = self.task.name
        errors = 0
        errStr = ""
        for flag in self.task.flags:
            if 'value' in flag and flag['value']:
                cmd += ' -' + flag['name']
        for p in self.task.params:
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

def getInterfaceDescription( cmd ):
    """Returns the XML description for the GRASS cmd.

    The DTD must be located in $GISBASE/etx/wx/gui_modules/grass-interface.dtd,
    otherwise the parser will not succeed."""
    gmpath =  os.getenv("GISBASE") + "/etc/wx/gui_modules"
    cmd = cmd + r' --interface-description'
    cmdout = os.popen(cmd, "r").read()
    p = re.compile( '(grass-interface.dtd)')
    p.search( cmdout )
    cmdout = p.sub( gmpath+r'/grass-interface.dtd', cmdout)
    return cmdout

class GrassGUIApp(wx.App):
    """Stand-alone GRASS command GUI"""
    def __init__(self, cmd):
        self.grass_task = grassTask()
        handler = processTask(self.grass_task)
        xml.sax.parseString( getInterfaceDescription( cmd ) , handler )
        wx.App.__init__(self)
        
    def OnInit(self):
        self.mf = mainFrame(None ,-1, self.grass_task )
        self.mf.Show(True)
        self.SetTopWindow(self.mf)
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
        cmdlst = cmd.split(' ')

        if parentframe != -1:
            self.parent = parentframe

        if len(cmdlst) > 1:
            print "usage: %s <grass command> " % cmdlst[0]
        else:
            # parse the interface decription
            self.grass_task = grassTask()
            handler = processTask(self.grass_task)
            xml.sax.parseString( getInterfaceDescription( cmd ) , handler )

            self.mf = mainFrame(self.parent ,-1, self.grass_task, self.get_dcmd, layer)
            self.mf.Show(True)

if __name__ == "__main__":

    if len(sys.argv) == 1:
        print "Usage: %s <grass command>" % sys.argv[0]
        sys.exit()
    app = GrassGUIApp(sys.argv[1])
    app.MainLoop()

