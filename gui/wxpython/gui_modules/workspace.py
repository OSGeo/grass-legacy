"""
@package workspace

@brief Open/save workspace definition file

Classes:
 - ProcessWorkspaceFile

(C) 2007-2008 by the GRASS Development Team
This program is free software under the GNU General Public
License (>=v2). Read the file COPYING that comes with GRASS
for details.

@author Martin Landa <landa.martin gmail.com>

@date 2007-2008
"""

### for gxw (workspace file) parsering
# xmlproc not available on Mac OS
# from xml.parsers.xmlproc import xmlproc
# from xml.parsers.xmlproc import xmlval
# from xml.parsers.xmlproc import xmldtd
import xml.sax
import xml.sax.handler
HandlerBase=xml.sax.handler.ContentHandler
from xml.sax import make_parser

class ProcessWorkspaceFile(HandlerBase):
    """
    A SAX handler for the GXW XML file, as
    defined in grass-gxw.dtd.
    """
    def __init__(self):
        self.inGxw       = False
        self.inLayer     = False
        self.inTask      = False
        self.inParameter = False
        self.inFlag      = False
        self.inValue     = False
        self.inGroup     = False
        self.inDisplay   = False

        # list of layers
        self.layers = []
        self.cmd    = []
        self.displayIndex = -1 # first display has index '0'

    def startElement(self, name, attrs):
        if name == 'gxw':
            self.inGxw = True

        elif name == 'display':
            self.inDisplay = True
            self.displayIndex += 1

        elif name == 'group':
            self.groupName    = attrs.get('name', None)
            self.groupChecked = attrs.get('checked', None)
            self.layers.append({
                    "type"    : 'group',
                    "name"    : self.groupName,
                    "checked" : int(self.groupChecked),
                    "opacity" : None,
                    "cmd"     : None,
                    "group"   : self.inGroup,
                    "display" : self.displayIndex})
            self.inGroup = True

        elif name == 'layer':
            self.inLayer = True
            self.layerType    = attrs.get('type', None)
            self.layerName    = attrs.get('name', None)
            self.layerChecked = attrs.get('checked', None)
            self.layerOpacity = attrs.get('opacity', None)
            self.cmd = []

        elif name == 'task':
            self.inTask = True;
            name = attrs.get('name', None)
            self.cmd.append(name)

        elif name == 'parameter':
            self.inParameter = True;
            self.parameterName = attrs.get('name', None)

        elif name == 'value':
            self.inValue = True
            self.value = ''

        elif name == 'flag':
            self.inFlag = True;
            name = attrs.get('name', None)
            self.cmd.append('-' + name)

    def endElement(self, name):
        if name == 'gxw':
            self.inGxw = False

        elif name == 'display':
            self.inDisplay = False

        elif name == 'group':
            self.inGroup = False
            self.groupName = self.groupChecked = None

        elif name == 'layer':
            self.inLayer = False
            self.layers.append({
                    "type"    : self.layerType,
                    "name"    : self.layerName,
                    "checked" : int(self.layerChecked),
                    "opacity" : None,
                    "cmd"     : None,
                    "group"   : self.inGroup,
                    "display" : self.displayIndex})

            if self.layerOpacity:
                self.layers[-1]["opacity"] = float(self.layerOpacity)
            if self.cmd:
                self.layers[-1]["cmd"] = self.cmd

            self.layerType = self.layerName = self.Checked = \
                self.Opacity = self.cmd = None

        elif name == 'task':
            self.inTask = False

        elif name == 'parameter':
            self.inParameter = False
            self.cmd.append('%s=%s' % (self.parameterName, self.value))
            self.parameterName = self.value = None

        elif name == 'value':
            self.inValue = False

        elif name == 'flag':
            self.inFlag = False

    def characters(self, ch):
        self.my_characters(ch)

    def my_characters(self, ch):
        if self.inValue:
            self.value += ch

