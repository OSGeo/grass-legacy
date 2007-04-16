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
import re
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
import gettext
gettext.install("wxgrass")

sys.path.append(os.path.join(os.getenv("GISBASE"),"etc","wx"))
imagepath = os.path.join(os.getenv("GISBASE"),"etc","wx","images")
sys.path.append(imagepath)

try:
    import subprocess
except:
    from compat import subprocess


def reexec_with_pythonw():
    if sys.platform == 'darwin' and\
        not sys.executable.endswith('MacOS/Python'):
        print >>sys.stderr,'re-executing using pythonw'
        os.execvp('pythonw',['pythonw',__file__] + sys.argv[1:])

reexec_with_pythonw()

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
str2rgb = {}
rgb2str = {}
for c in range(0,len(t_rgb)):
    str2rgb[ t_color[c] ] = t_rgb[ c ]
    rgb2str[ t_rgb[ c ] ] = t_color[ c ]
del t_colors
del t_color
del t_rgb

def color_resolve(color):
    if len(color)>0 and color[0] in "0123456789":
        rgb = tuple(map(int,color.split( ':' )))
        label = color
    else:
        # Convert color names to RGB
        try:
            rgb = str2rgb[ color ]
            label = color
        except KeyError:
            rgb = (200,200,200)
            label = _('Select Color')
    return (rgb, label)


def normalize_whitespace(text):
    "Remove redundant whitespace from a string"
    return string.join( string.split(text), ' ')

def text_beautify( someString ):
    "Make really long texts shorter"
    # TODO: remove magic number (calculate a correct value from
    # pixelSize of text and the magic number for maximum size
    return escape_ampersand( os.linesep.join( textwrap.wrap( normalize_whitespace(someString), 70 ) ) )

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
        self.name = _('unknown')
        self.params = []
        self.description = ''
        self.flags = []

    def buildCmd(self): # TODO: It should be this class' responsibility to build the command, not the gui's.
        pass


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
        if self.inValueContent and not self.inDescriptionContent:
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
    def __init__(self, grass_command = "index", *args, **kwargs):
        wx.html.HtmlWindow.__init__(self, *args, **kwargs)
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
        except: # The Manual file was not found
            self.Ok = False


class mainFrame(wx.Frame):
    """This is the Frame containing the dialog for options input.

    The dialog is organized in a notebook according to the guisections
    defined by each GRASS command.

    If run with a parent, it may Apply, Ok or Cancel; the latter two close the dialog.
    The former two trigger a callback.

    If run standalone, it will allow execution of the command.

    The command is checked and sent to the clipboard when clicking "Copy". """
    def __init__(self, parent, ID, task_description, get_dcmd=None, layer=None):

        self.get_dcmd = get_dcmd
        self.layer = layer
        self.task = task_description

        wx.Frame.__init__(self, parent, ID, self.task.name,
            wx.DefaultPosition, style=wx.DEFAULT_FRAME_STYLE | wx.TAB_TRAVERSAL)

        self.CreateStatusBar()
        self.parent = parent
        self.SetIcon(wx.Icon(os.path.join(imagepath,'grass.form.gif'), wx.BITMAP_TYPE_ANY))

        menu = wx.Menu()
        menu.Append(wx.ID_ABOUT, _("&About GrassGUI"),
            _("Information about GrassGUI") )
        menu.Append(ID_ABOUT_COMMAND, _("&About %s") % self.task.name,
            _("Short descripton of GRASS command %s") % self.task.name)
        menu.AppendSeparator()
        menu.Append(wx.ID_EXIT, _("E&xit"), _("Terminate the program") )

        menuBar = wx.MenuBar()
        menuBar.Append(menu, "&File");

        self.SetMenuBar(menuBar)
        guisizer = wx.BoxSizer(wx.VERTICAL)

        # set apropriate output window
#        if self.parent:
#            standalone=False
#            self.goutput = self.parent.goutput
#        else:
        standalone=True
        self.notebookpanel = cmdPanel( parent=self, task=self.task, standalone=standalone )
        if standalone:
            self.goutput = self.notebookpanel.goutput
        self.notebookpanel.OnUpdateValues = self.updateValuesHook

        guisizer.Add( self.notebookpanel, 1, flag = wx.EXPAND )

        status_text = _("Enter parameters for ") + self.task.name
        if self.notebookpanel.hasMain:
            # We have to wait for the notebookpanel to be filled in order
            # to know if there actually is a Main tab
            status_text += _(" (those of Main in bold typeface are required)")
        self.SetStatusText( status_text )

        btnsizer = wx.BoxSizer(wx.HORIZONTAL)
        btn_cancel = wx.Button(self, wx.ID_CANCEL, _("Cancel") )
        btnsizer.Add( btn_cancel, 0, wx.ALL| wx.ALIGN_CENTER, 10)
        if self.get_dcmd is not None: # A callback has been set up
            btn_apply = wx.Button(self, wx.ID_APPLY, _("Apply") )
            btnsizer.Add( btn_apply, 0, wx.ALL| wx.ALIGN_CENTER, 10)
            btn_ok = wx.Button(self, wx.ID_OK, _("OK") )
            btnsizer.Add( btn_ok, 0, wx.ALL| wx.ALIGN_CENTER, 10)
            btn_ok.SetDefault()
            btn_apply.Bind(wx.EVT_BUTTON, self.OnApply)
            btn_ok.Bind(wx.EVT_BUTTON, self.OnOK)
        else: # We're standalone
            btn_run = wx.Button(self, wx.ID_OK, _("Run") )
            btnsizer.Add( btn_run, 0, wx.ALL| wx.ALIGN_CENTER, 10)
            btn_run.SetDefault()
            btn_run.Bind(wx.EVT_BUTTON, self.OnRun)
            btn_clipboard = wx.Button(self, wx.ID_OK, _("Copy") )
            btnsizer.Add(btn_clipboard, 0, wx.ALL| wx.ALIGN_CENTER, 10)
            btn_clipboard.Bind(wx.EVT_BUTTON, self.OnCopy)
        guisizer.Add(btnsizer, 0, wx.ALIGN_BOTTOM)
        wx.EVT_MENU(self, wx.ID_ABOUT, self.OnAbout)
        wx.EVT_MENU(self, ID_ABOUT_COMMAND, self.OnAboutCommand)
        wx.EVT_MENU(self, wx.ID_EXIT,  self.OnCancel)
        btn_cancel.Bind(wx.EVT_BUTTON, self.OnCancel)
        self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)

        constrained_size = self.notebookpanel.GetSize()
        self.notebookpanel.SetSize( (constrained_size[0],constrained_size[1]+80) ) # 80 takes the tabbar into account
        self.notebookpanel.Layout()

        guisizer.SetSizeHints(self)
        self.SetAutoLayout(True)
        self.SetSizer(guisizer)
        self.Layout()

    def updateValuesHook(self):
        self.SetStatusText( self.notebookpanel.createCmd(ignoreErrors = True) )

    def OnOK(self, event):
        cmd = self.OnApply(event)
        if cmd is not None and self.get_dcmd is not None:
            self.OnCancel(event)

    def OnApply(self, event):
        cmd = self.createCmd()

        if cmd is not None and self.get_dcmd is not None:
            # return d.* command to layer tree for rendering
            self.get_dcmd(cmd, self.layer, {"params":self.task.params,"flags":self.task.flags} )
            # echo d.* command to output console
#            self.parent.writeDCommand(cmd)
        return cmd

    def OnRun(self, event):
        cmd = self.createCmd()

        if cmd != None and cmd[0:2] != "d.":
            # Send any non-display command to parent window (probably wxgui.py)
            # put to parents
            try:
                self.goutput.runCmd(cmd)
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
            self.SetStatusText( _("'%s' copied to clipboard") %\
                            (self.createCmd(ignoreErrors=True)))

    def OnCancel(self, event):
        self.Destroy()

    def OnCloseWindow(self, event):
        self.Destroy()

    def OnAbout(self, event):
        dlg = wx.MessageDialog(self, _("This is a sample program for\n"
            "GRASS command interface parsing\n"
            "and automatic GUI building. \n%s") %(__version__),
            _("About GrassGUI"), wx.OK | wx.ICON_INFORMATION)
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
    def __init__( self, parent, task, standalone, *args, **kwargs ):
        wx.Panel.__init__( self, parent, *args, **kwargs )

        self.task = task

        # Determine tab layout
        sections = [ _('Main') ]
        is_section = {}
        for task in self.task.params + self.task.flags:
            if not task.has_key('guisection') or task['guisection']=='':
                task['guisection'] = 'Options'
            if not is_section.has_key(task['guisection']):
                is_section[task['guisection']] = 1
                if task['guisection'] != _('Main'): # check for pre-existing parameters passed from layer tree
                    sections.append( task['guisection'] )
        there_is_main = False
        for i in self.task.params+self.task.flags:
            if i.has_key('required') and i['required'] == 'yes':
                i['guisection'] = _('Main')
                there_is_main = True
        if not there_is_main:
            sections = sections[1:]

        panelsizer = wx.BoxSizer(wx.VERTICAL)
        # Build notebook
        nbStyle=FN.FNB_NO_X_BUTTON|FN.FNB_VC8|FN.FNB_BACKGROUND_GRADIENT
        notebook = FN.FlatNotebook( self, id=wx.ID_ANY, style=nbStyle)
        notebook.SetTabAreaColour(wx.Colour(125,200,175))
        notebook.Bind( FN.EVT_FLATNOTEBOOK_PAGE_CHANGED, self.OnPageChange )
        tab = {}
        tabsizer = {}
        for section in sections:
            tab[section] = wx.ScrolledWindow( notebook )
            tab[section].SetScrollRate(10,10)
            tabsizer[section] = wx.BoxSizer(wx.VERTICAL)
            notebook.AddPage( tab[section], text = section )

        # are we running from command line?
        if standalone:
            from gui_modules import wxgui_utils
            self.goutput = wxgui_utils.GMConsole(self)
            self.outpage = notebook.AddPage(self.goutput, text=_("Command output") )

        manual_tab =  helpPanel( parent = notebook, grass_command = self.task.name)
        if manual_tab.Ok:
            manual_tabsizer = wx.BoxSizer(wx.VERTICAL)
            notebook.AddPage( manual_tab, text = _("Manual") )

        notebook.SetSelection(0)
        panelsizer.Add( notebook, 1, flag=wx.EXPAND )

        visible_params = [ p for p in self.task.params if not p.get( 'hidden', 'no' ) == 'yes' ]
        for p in visible_params:
            which_sizer = tabsizer[ p['guisection'] ]
            which_panel = tab[ p['guisection'] ]
            title = text_beautify( p['description'] )
            text_style = wx.FONTWEIGHT_BOLD
            txt = None
            if p.get('required','no') == 'no':
                text_style = wx.FONTWEIGHT_NORMAL
            if p.get('multiple','no') == 'yes' and len( p.get('values','') ) == 0:
                title = _("[multiple]") + " " + title
            if p.get('value','') ==  '' :
                p['value'] = p.get('default','')
            if ( len(p.get('values',[]) ) > 0):

                valuelist=map(str,p.get('values',[]))
                if p.get('multiple','no') == 'yes':
                    txt = wx.StaticBox(which_panel,0,title+":")
                    hSizer=wx.StaticBoxSizer( txt, wx.VERTICAL )
                    isDefault = {}
                    for defval in p['value'].split(','):
                        isDefault[ defval ] = 'yes'
                    # for multi checkboxes, this is an array of all wx IDs
                    # for each individual checkbox
                    p[ 'wxId' ]=[]
                    for val in valuelist:
                        chkbox = wx.CheckBox( which_panel, label = text_beautify(val) )
                        p[ 'wxId' ].append( chkbox.GetId() )
                        if isDefault.has_key(val): chkbox.SetValue( True )
                        hSizer.Add( chkbox,0,wx.ADJUST_MINSIZE,5 )
                        chkbox.Bind(wx.EVT_CHECKBOX, self.OnCheckBoxMulti)
                    which_sizer.Add( hSizer, 0, wx.ADJUST_MINSIZE, 5)
                elif len(valuelist) == 1:
                    txt = wx.StaticText(which_panel,
                                label = _('%s. Valid range=%s') % (title, str(valuelist).strip("[]'") + ':' ) )
                    which_sizer.Add(txt, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)
                    txt2 = wx.TextCtrl(which_panel, value = p.get('default',''),
                                            size = (STRING_ENTRY_WIDTH, ENTRY_HEIGHT))
                    if p.get('value','') != '': txt2.SetValue(p['value']) # parameter previously set

                    which_sizer.Add( txt2, 0, wx.ADJUST_MINSIZE, 5)
                    p['wxId'] = txt2.GetId()
                    txt2.Bind(wx.EVT_TEXT, self.OnSetValue)
                else:
                    txt = wx.StaticText(which_panel, label = title + ':' )
                    which_sizer.Add(txt, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)
                    cb = wx.ComboBox(which_panel, -1, p.get('default',''),
                                     wx.Point(-1, -1), wx.Size(STRING_ENTRY_WIDTH, -1),
                                     valuelist, wx.CB_DROPDOWN)
                    if p.get('value','') != '': cb.SetValue(p['value']) # parameter previously set
                    which_sizer.Add( cb, 0, wx.ADJUST_MINSIZE, 5)
                    p['wxId'] = cb.GetId()
                    cb.Bind( wx.EVT_COMBOBOX, self.OnSetValue)

            # text entry
            if (p.get('type','string') in ('string','integer','float')
                and len(p.get('values',[])) == 0
                and p.get('gisprompt',False) == False
                and p.get('prompt','') != 'color'):

                txt = wx.StaticText(which_panel, label = title + ':' )
                which_sizer.Add(txt, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)

                txt3 = wx.TextCtrl(which_panel, value = p.get('default',''),
                    size = (STRING_ENTRY_WIDTH, ENTRY_HEIGHT))
                if p.get('value','') != '': txt3.SetValue(p['value']) # parameter previously set
                which_sizer.Add( txt3, 0, wx.ADJUST_MINSIZE| wx.ALL, 5)
                p['wxId'] = txt3.GetId()
                txt3.Bind(wx.EVT_TEXT, self.OnSetValue)

            if p.get('type','string') == 'string' and p.get('gisprompt',False) == True:
                txt = wx.StaticText(which_panel, label = title + ':')
                which_sizer.Add(txt, 0, wx.ADJUST_MINSIZE | wx.ALL, 5)
                # element selection tree combobox (maps, icons, regions, etc.)
                if p.get('prompt','') != 'color':
                    selection = select.Select(which_panel, id=wx.ID_ANY, size=(300,-1),
                                                   type=p.get('element','') )
                    if p.get('value','') != '': selection.SetValue(p['value']) # parameter previously set
                    which_sizer.Add( selection, 0, wx.ADJUST_MINSIZE| wx.ALL, 5)
                    # A select.Select is a combobox with two children: a textctl and a popupwindow;
                    # we target the textctl here
                    p['wxId'] = selection.GetChildren()[0].GetId()
                    selection.Bind(wx.EVT_TEXT, self.OnSetValue)
                # color entry
                elif p.get('prompt','') == 'color':
                    default_color = (200,200,200)
                    label_color = _("Select Color")
                    if p.get('default','') != '':
                        default_color, label_color = color_resolve( p['default'] )
                    if p.get('value','') != '': # parameter previously set
                        default_color, label_color = color_resolve( p['value'] )
                    if "none" in title:
                        this_sizer = wx.BoxSizer( wx.HORIZONTAL )
                    else:
                        this_sizer = which_sizer
                    btn_colour = csel.ColourSelect(which_panel, wx.ID_ANY, label_color, default_color, wx.DefaultPosition, (150,-1) )
                    this_sizer.Add(btn_colour, 0, wx.ADJUST_MINSIZE| wx.ALL, 5)
                    # For color selectors, this is a two-member array, holding the IDs of
                    # the selector proper and either a "transparent" button or None
                    p['wxId'] = [btn_colour.GetId(),]
                    btn_colour.Bind(csel.EVT_COLOURSELECT,  self.OnColorChange )
                    if "none" in title:
                        none_check = wx.CheckBox(which_panel, wx.ID_ANY, _("Transparent") )
                        if p.get('value','') != '' and p.get('value',[''])[0] == "none":
                            none_check.SetValue(True)
                        else:
                            none_check.SetValue(False)
                        none_check.SetFont( wx.Font( 12, wx.FONTFAMILY_DEFAULT, wx.NORMAL, text_style, 0, ''))
                        this_sizer.Add(none_check, 0, wx.ADJUST_MINSIZE| wx.ALL, 5)
                        which_sizer.Add( this_sizer )
                        none_check.Bind(wx.EVT_CHECKBOX, self.OnColorChange)
                        p['wxId'].append( none_check.GetId() )
                    else:
                        p['wxId'].append(None)
	    if txt is not None:
                txt.SetFont( wx.Font( 12, wx.FONTFAMILY_DEFAULT, wx.NORMAL, text_style, 0, ''))

        visible_flags = [ f for f in self.task.flags if not f.get( 'hidden', 'no' ) == 'yes' ]
        for f in visible_flags:
            which_sizer = tabsizer[ f['guisection'] ]
            which_panel = tab[ f['guisection'] ]
            title = text_beautify( f['description'] )
            chk = wx.CheckBox(which_panel,-1, label = title, style = wx.NO_BORDER)
            if 'value' in f: chk.SetValue( f['value'] )
            chk.SetFont( wx.Font( 12, wx.FONTFAMILY_DEFAULT, wx.NORMAL, text_style, 0, ''))
            which_sizer.Add( chk, 0, wx.EXPAND| wx.ALL, 5)
            f['wxId'] = chk.GetId()
            chk.Bind(wx.EVT_CHECKBOX, self.OnSetValue)

        maxsizes = (0,0)
        for section in sections:
            tabsizer[section].SetSizeHints( tab[section] )
            tabsizer[section].Fit( tab[section] )
            tab[section].SetAutoLayout(True)
            tab[section].SetSizer( tabsizer[section] )
            tab[section].Layout()
            minsecsizes = tabsizer[section].GetMinSize()
            maxsizes = map( lambda x: max( maxsizes[x], minsecsizes[x] ), (0,1) )

        # TODO: be less arbitrary with these 600
        constrained_size = (min(600, maxsizes[0]), min(600, maxsizes[1]) )
        for section in sections:
            tab[section].SetMinSize( constrained_size )
        if manual_tab.Ok:
            manual_tab.SetMinSize( constrained_size )

        self.SetSizer( panelsizer )
        self.hasMain = tab.has_key( _('Main') ) # publish, to enclosing Frame for instance


    def OnPageChange(self, event):
        self.Layout()

    def OnColorChange( self, event ):
        myId = event.GetId()
        for p in self.task.params:
            if 'wxId' in p and type( p['wxId'] ) == type( [] ) and myId in p['wxId']:
                has_button = p['wxId'][1] is not None
                if has_button and wx.FindWindowById( p['wxId'][1] ).GetValue() == True:
                    p[ 'value' ] = 'none'
                else:
                    colorchooser = wx.FindWindowById( p['wxId'][0] )
                    new_color = colorchooser.GetValue()[:]
                    # This is weird: new_color is a 4-tuple and new_color[:] is a 3-tuple
                    # under wx2.8.1
                    new_label = rgb2str.get( new_color, ':'.join(map(str,new_color)) )
                    colorchooser.SetLabel( new_label )
                    colorchooser.SetColour( new_color )
                    colorchooser.Refresh()
                    p[ 'value' ] = colorchooser.GetLabel()
        self.OnUpdateValues()

    def OnUpdateValues(self):
        """If we were part of a richer interface, report back the current command being built.

        This method should be set by the parent of this panel if needed. It's a hook, actually.
        Beware of what is "self" in the method def, though. It will be called with no arguments."""
        pass

    def OnCheckBoxMulti(self, event):
        """Fill the values ,-separated string according to current status of the checkboxes."""
        me = event.GetId()
        theParam = None
        for p in self.task.params:
            if 'wxId' in p and type( p['wxId'] ) == type( [] ) and me in p['wxId']:
                theParam = p
                myIndex = p['wxId'].index( me )
        # Unpack current value list
        currentValues={}
        for isThere in theParam['value'].split(','):
            currentValues[isThere] = 1
        theValue = theParam['values'][myIndex]
        if event.Checked():
            currentValues[ theValue ] = 1
        else:
            del currentValues[ theValue ]
        currentValueList=[] # Keep the original order, so that some defaults may be recovered
        for v in theParam['values']:
            if currentValues.has_key(v):
                currentValueList.append( v )
        # Pack it back
        theParam['value'] = ','.join( currentValueList )
        self.OnUpdateValues()

    def OnSetValue(self, event):
        myId = event.GetId()
        me = wx.FindWindowById( myId )
        for porf in self.task.params + self.task.flags:
            if 'wxId' in porf and type( porf[ 'wxId' ] ) == type( 1 ) and porf['wxId'] == myId:
                porf[ 'value' ] = me.GetValue()
        self.OnUpdateValues()

    def createCmd(self, ignoreErrors = False):
        """Produce a command line string for feeding into GRASS.

        If ignoreErrors==True then it will return whatever has been
        built so far, even though it would not be a correct command
        for GRASS."""
        cmd = self.task.name
        errors = 0
        errStr = ""
        dcmd_params = {}

        for flag in self.task.flags:
            if 'value' in flag and flag['value']:
                cmd += ' -' + flag['name']
        for p in self.task.params:
            if p.get('value','') == '' and p.get('required','no') != 'no':
                cmd += ' ' + p['name'] + '=' + _('<required>')
                errStr += _("Parameter %s (%s) is missing\n") % ( p['name'], p['description'] )
                errors += 1
            if p.get('value','') != '' and p['value'] != p.get('default','') :
                cmd += ' ' + p['name'] + '=' + p['value']
        if errors and not ignoreErrors:
            self.OnError(errStr)
            return None

        return cmd

    def OnError(self, errMsg):
        dlg = wx.MessageDialog(self, errMsg, _("Error"), wx.OK | wx.ICON_ERROR)
        dlg.ShowModal()
        dlg.Destroy()


def getInterfaceDescription( cmd ):
    """Returns the XML description for the GRASS cmd.

    The DTD must be located in $GISBASE/etx/wx/gui_modules/grass-interface.dtd,
    otherwise the parser will not succeed."""
    gmpath = os.getenv("GISBASE") + "/etc/wx/gui_modules"
    cmd = cmd + r' --interface-description'
    cmdout = os.popen(cmd, "r").read()
    p = re.compile( '(grass-interface.dtd)')
    p.search( cmdout )
    cmdout = p.sub( gmpath+r'/grass-interface.dtd', cmdout)
    return cmdout

class GrassGUIApp(wx.App):
    """Stand-alone GRASS command GUI"""
    def __init__(self, grass_task):
        self.grass_task = grass_task
#XXX        from pprint import pprint
#        pprint( self.grass_task.params )

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

        dcmd_params = {}
        if completed == None:
            get_dcmd = None
            layer = None
            dcmd_params = None
        else:
            get_dcmd = completed[0]
            layer = completed[1]
            if completed[2]: dcmd_params.update(completed[2])
        cmdlst = cmd.split(' ')

        if parentframe != -1:
            self.parent = parentframe

        if len(cmdlst) > 1:
            raise ValueError, _("usage: %s <grass command> ") % cmdlst[0]
        else:
            # parse the interface decription
            self.grass_task = grassTask()
            handler = processTask(self.grass_task)
            xml.sax.parseString( getInterfaceDescription( cmd ) , handler )

            # if layer parameters previously set, re-insert them into dialog
            if completed is not None:
                if 'params' in dcmd_params: self.grass_task.params = dcmd_params['params']
                if 'flags' in dcmd_params: self.grass_task.flags = dcmd_params['flags']

            self.mf = mainFrame(self.parent ,-1, self.grass_task, get_dcmd, layer)
            self.mf.Show(True)


if __name__ == "__main__":

    if len(sys.argv) == 1:
        print _("usage: %s <grass command>") % sys.argv[0]
        sys.exit()
    if sys.argv[1] != 'test':
        grass_task = grassTask()
        handler = processTask(grass_task)
        xml.sax.parseString( getInterfaceDescription( sys.argv[1] ) , handler )
        app = GrassGUIApp( grass_task )
        app.MainLoop()
    else: #Test
        task = grassTask()
        task.name = "TestTask"
        task.description = "This is a artificial grassTask() object intended for testing purposes"
        task.params = [
            {
            "name" : "text",
            "description" : "Enter some text"
            },{
            "name" : "hidden_text",
            "description" : "This text should not appear in the form",
            "hidden" : "yes"
            },{
            "name" : "text_default",
            "description" : "Enter text to override the default",
            "default" : "default text"
            },{
            "name" : "text_prefilled",
            "description" : "You should see a friendly welcome message here",
            "value" : "hello, world"
            },{
            "name" : "plain_color",
            "description" : "This is a plain color, and it is a compulsory parameter",
            "required" : "yes",
            "gisprompt" : True,
            "prompt" : "color"
            },{
            "name" : "transparent_color",
            "description" : "This color becomes transparent when set to none",
            "guisection" : "tab",
            "prompt" : "color"
            },{
            "name" : "multi",
            "description" : "A multiple selection",
            'default': u'red,green,blue',
            'gisprompt': False,
            'guisection': 'tab',
            'multiple': u'yes',
            'type': u'string',
            'value': '',
            'values': ['red', 'green', u'yellow', u'blue', u'purple', u'other']
            },{
            "name" : "single",
            "description" : "A single multiple-choice selection",
            'values': ['red', 'green', u'yellow', u'blue', u'purple', u'other'],
            "guisection" : "tab"
            }
            ]
        task.flags = [
            {
            "name" : "a",
            "description" : "Some flag",
            "required" : "yes"
            },{
            "name" : "b",
            "description" : "pre-filled flag",
            "value" : True
            },{
            "name" : "h",
            "description" : "hidden flag",
            "hidden" : "yes"
            }
            ]
        GrassGUIApp( task ).MainLoop()
