#! /usr/bin/python
## vim:ts=4:et:sts=4:sw=4:ai:
""" Construct simple wxPython GUI from a GRASS command interface description.

# Copyright (C) 2000 by the GRASS Development Team
# Author: Jan-Oliver Wagner <jan@intevation.de>
# improved by: Bernhard Reiter   <bernhard@intevation.de>
#
# This program is free software under the GPL (>=v2)
# Read the file COPYING coming with GRASS for details.

# This program is just a coarse approach to
# automatically build a GUI from a xml-based
# GRASS user interface description.
#
# You need to have Python, wxPython and python-xml.
#
# The XML stream is read from stdin, thus you
# may call it for instance this way:
# r.basins.fill --interface-description | python grassgui.py
# or
# r.basins.fill --interface-description | grassgui.py
# 
# Or you set an alias or wrap the call up in a nice
# shell script, GUI environment ... please contribute your idea.
"""
__version__ ="$RCSFile$ $Revision$"

from wxPython.wx import *
import sys
oldPython=0

if not sys.__dict__.has_key("version_info"):
    # We can assume to have python 1.5.2 or lower here now
    oldPython=1

if oldPython:
    try:
        from xml.sax.saxexts import make_parser
        from xml.sax.saxlib import HandlerBase
        from xml.sax import saxutils
    except ImportError:
        sys.stdout.write("You need to have Python-XML installed or" +
                   " a modern Python!\n"+
                   "Check www.python.org/sigs/xml-sig/\n\n")
        raise
    
else:
    # Do the python 2.0 standard xml thing and map it on the old names
    import xml.sax
    import xml.sax.handler
    HandlerBase=xml.sax.handler.ContentHandler
    from xml.sax import make_parser



from os import system

ID_RUN    = 10
ID_CANCEL= 11
ID_PARAM_START = 900

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

grass_task = { 'name' : 'unknown', 'description' : 'No description available.',
    'lines' : 0, 'params' : [] }

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
        if name == 'description':
            self.inDescriptionContent = 1
            self.description = ''
        if name == 'default':
            self.inDefaultContent = 1
            self.param_default = ''
        if name == 'value':
            self.inValueContent = 1
            self.value_tmp = ''

    if not oldPython and test_for_broken_SAX():
        # works with python 2.0, but is not SAX compliant
        def characters(self, ch):
            self.my_characters(ch)
    else:
        # SAX compliant
        def characters(self, ch, start, length):
            self.my_characters(ch[start:start+length])

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
            grass_task['params'].append({ "name" : self.param_name,
                "type" : self.param_type, "required" : self.param_required,
                "multiple" : self.param_multiple, "description" : self.param_description,
                "default" : self.param_default, "values" : self.param_values, "value" : '' })

        if name == 'description':
            if self.inParameter:
                self.param_description = normalize_whitespace(self.description)
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


class mainFrame(wxFrame):
    def __init__(self, parent, ID, w, h):
        wxFrame.__init__(self, parent, ID, str(grass_task['name']),
            wxDefaultPosition, wxSize(w, h))

        self.CreateStatusBar()
        self.SetStatusText("Enter parameters for " + str(grass_task['name']))

        menu = wxMenu()
        menu.Append(ID_ABOUT, "&About GrassGUI",
                    "Information about GrassGUI")
        menu.Append(ID_ABOUT_COMMAND, "&About " + str(grass_task['name']),
                    "Short descripton of GRASS command " + str(grass_task['name']))
        menu.AppendSeparator()
        menu.Append(ID_EXIT, "E&xit", "Terminate the program")

        menuBar = wxMenuBar()
        menuBar.Append(menu, "&File");

        self.SetMenuBar(menuBar)

        self.panel = wxPanel(self, -1)

        p_count = 0
        l_count = 0
        while p_count < len(grass_task['params']):
            title = str(escape_ampersand(grass_task['params'][p_count]['description']))
            if grass_task['params'][p_count]['required'] == 'no':
                title = "[optional] " + title
            if grass_task['params'][p_count]['multiple'] == 'yes':
                title = "[mutiple] " + title
            grass_task['params'][p_count]['value'] = grass_task['params'][p_count]['default'] 
            if (len(grass_task['params'][p_count]['values']) > 0):
                wxStaticText(self.panel, -1, title + ':',
                    wxPoint(HSPACE, l_count * ENTRY_HEIGHT + VSPACE + p_count * VSPACE), wxSize(-1, -1))
                l_count = l_count + 1

                valuelist=map(str,grass_task['params'][p_count]['values'])
                wxComboBox(self.panel, ID_PARAM_START + p_count, grass_task['params'][p_count]['default'],
                    wxPoint(HSPACE, l_count * ENTRY_HEIGHT + VSPACE + p_count * VSPACE),
                    wxSize(STRING_ENTRY_WIDTH, -1),
                    valuelist, wxCB_DROPDOWN)
                EVT_COMBOBOX(self, ID_PARAM_START + p_count, self.EvtComboBox)

            if ((grass_task['params'][p_count]['type'] == 'string' or
                grass_task['params'][p_count]['type'] == 'integer' or
                grass_task['params'][p_count]['type'] == 'float') and
                len(grass_task['params'][p_count]['values']) == 0):
                wxStaticText(self.panel, -1, title + ':',
                    wxPoint(HSPACE, l_count * ENTRY_HEIGHT + VSPACE + p_count * VSPACE), wxSize(-1, -1))
                l_count = l_count + 1
                wxTextCtrl(self.panel, ID_PARAM_START + p_count,
                    grass_task['params'][p_count]['default'], wxPoint(HSPACE, l_count * ENTRY_HEIGHT + VSPACE + p_count * VSPACE),
                    wxSize(STRING_ENTRY_WIDTH, ENTRY_HEIGHT))
                EVT_TEXT(self, ID_PARAM_START + p_count, self.EvtText)

            if (grass_task['params'][p_count]['type'] == 'flag'):
                wxCheckBox(self.panel,ID_PARAM_START + p_count, title,
                    wxPoint(HSPACE, l_count * ENTRY_HEIGHT + VSPACE + p_count * VSPACE),
                    wxSize(-1, -1), wxNO_BORDER)
                EVT_CHECKBOX(self, ID_PARAM_START + p_count, self.EvtCheckBox)

            p_count = p_count + 1
            l_count = l_count + 1

        wxButton(self.panel, ID_CANCEL, "Cancel",
            wxPoint(2 * HSPACE, VSPACE + l_count * ENTRY_HEIGHT + p_count * VSPACE + 3 * VSPACE))
        wxButton(self.panel, ID_RUN, "Run",
            wxPoint(2 * HSPACE + BUTTON_WIDTH + HSPACE, VSPACE + l_count * ENTRY_HEIGHT + p_count * VSPACE + 3 * VSPACE)).SetDefault()

        EVT_MENU(self, ID_ABOUT, self.OnAbout)
        EVT_MENU(self, ID_ABOUT_COMMAND, self.OnAboutCommand)
        EVT_MENU(self, ID_EXIT,  self.OnCancel)
        EVT_BUTTON(self.panel, ID_CANCEL, self.OnCancel)
        EVT_BUTTON(self.panel, ID_RUN, self.OnRun)

    def EvtText(self, event):
        grass_task['params'][event.GetId()-ID_PARAM_START]['value'] = event.GetString()

    def EvtCheckBox(self, event):
        if (event.Checked()):
            grass_task['params'][event.GetId()-ID_PARAM_START]['value'] = 'checked'
        else:
            grass_task['params'][event.GetId()-ID_PARAM_START]['value'] = 'not checked'

    def EvtComboBox(self, event):
        grass_task['params'][event.GetId()-ID_PARAM_START]['value'] = event.GetString()


    def OnRun(self, event):
        cmd = grass_task['name']
        p_count = 0
        errors = 0
        errStr = ""
        while p_count < len(grass_task['params']):
            if (grass_task['params'][p_count]['type'] != 'flag' and grass_task['params'][p_count]['value'] == '' and grass_task['params'][p_count]['required'] != 'no'):
                errStr = errStr + "Parameter " + grass_task['params'][p_count]['name'] + "(" +grass_task['params'][p_count]['description']  + ") is missing\n"
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
            system(cmd)

    def OnError(self, errMsg):
        dlg = wxMessageDialog(self, errMsg, "Error", wxOK | wxICON_ERROR)
        dlg.ShowModal()
        dlg.Destroy()

    def OnCancel(self, event):
        self.Close(false)

    def OnAbout(self, event):
        dlg = wxMessageDialog(self, "This is a sample program for\n"
                              "GRASS command interface parsing\n"
                              "and automatic GUI building. \n%s" %(__version__),
                              "About GrassGUI", wxOK | wxICON_INFORMATION)
        dlg.ShowModal()
        dlg.Destroy()

    def OnAboutCommand(self, event):
        dlg = wxMessageDialog(self, 
                            grass_task['name']+": "+grass_task['description'],
                            "About " + grass_task['name'], 
                            wxOK | wxICON_INFORMATION)
        dlg.ShowModal()
        dlg.Destroy()


class GrassGUIApp(wxApp):
    def OnInit(self):
        self.w = HSPACE + STRING_ENTRY_WIDTH + HSPACE
        self.h = MENU_HEIGHT + VSPACE + grass_task['lines'] * ENTRY_HEIGHT + VSPACE  + BUTTON_HEIGHT + VSPACE + STATUSBAR_HEIGHT
        frame = mainFrame(NULL, -1, self.w, self.h)
        frame.Show(true)
        self.SetTopWindow(frame)
        return true

# find out the command to run
if len(sys.argv) > 1:
    print "usage: <grass command> --task-description | " + sys.argv[0]
else:
    # parse the interface decription
    handler = processTask()

    if oldPython:
        parser = make_parser()
        parser.setDocumentHandler(handler)
        parser.setErrorHandler(saxutils.ErrorPrinter())
        parser.parseFile(sys.stdin)
        parser.close()
    else:
        xml.sax.parse(sys.stdin,handler)

    # Create the application
    app = GrassGUIApp(0)
    app.MainLoop()
