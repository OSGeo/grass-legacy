"""
MODULE: digit

CLASSES:
 * VEdit
 * VDigit
 * SettingsDialog
 
PURPOSE: Digitization tool wxPython GUI prototype

         Note: Initial version under development

         Progress:
          (1) v.edit called on the background (class VEdit)
          (2) Reimplentation of v.digit (VDigit)

AUTHORS: The GRASS Development Team
         Martin Landa <landa.martin gmail.com>

COPYRIGHT: (C) 2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import wx
import wx.lib.colourselect as csel

import cmd
import dbm
from debug import Debug as Debug

class Digit:
    """
    Abstract digitization class
    """
    def __init__(self):
        pass

class VEdit(Digit):
    """
    Prototype of digitization class based on v.edit command

    Note: This should be replaced by VDigit class.
    """
    def AddPoint (self, map, type, x, y, z=None):
        """
        Add point/centroid to the vector map layer
        """
        if type == "centroid":
            key = "C"
        else:
            key = "P"
        
        addstring="""%s 1 1
                    %f %f\n1 1""" % (key, x, y)

        Debug.msg (3, "VEdit.AddPoint(): map=%s, type=%s, x=%f, y=%f" % \
                   (map, type, x, y))
        
        self._AddFeature (map=map, input=addstring)

    def AddLine (self, map, type, coords):
        """
        Add line/boundary to the vector map layer
        """
        if len(coords) < 2:
            return

        if type == "boundary":
            key = "B"
        else:
            key = "L"
            
        addstring="""%s %d 1\n""" % (key, len(coords))
        for point in coords:
            addstring += """%f %f\n""" % \
                (float(point[0]), float(point [1]))

        addstring += "1 1"

        Debug.msg (3, "VEdit.AddPoint(): map=%s, type=%s" % \
                   (map, type))

        self._AddFeature (map=map, input=addstring)

    def _AddFeature (self, map, input):
        """
        General method which adds feature to the vector map
        """
        command = ["v.edit", "-n", "map=%s" % map, "tool=add"]

        # run the command
        vedit = cmd.Command(cmd=command, stdin=input)

class VDigit(Digit):
    """
    Prototype of digitization class based on v.digit reimplementation

    Under development (wxWidgets C/C++ background)
    """
    pass

class DigitSettingsDialog(wx.Dialog):
    """
    Standard settings dialog for digitization purposes
    """
    def __init__(self, parent, title, style):
        wx.Dialog.__init__(self, parent=parent, id=wx.ID_ANY, title=title, style=style)

        # notebook
        notebook = wx.Notebook(parent=self, id=wx.ID_ANY, style=wx.BK_DEFAULT)
        self.__CreateSymbologyPage(notebook)
        self.__CreateSettingsPage(notebook)
        
        # buttons
        btnApply = wx.Button(self, wx.ID_APPLY, _("Apply") )
        btnCancel = wx.Button(self, wx.ID_CANCEL)
        btnOk = wx.Button(self, wx.ID_OK, _("OK") )
        btnOk.SetDefault()

        # bindigs
        #btnApply.Bind(wx.EVT_BUTTON, self.OnApply)
        #btn_ok.Bind(wx.EVT_BUTTON, self.OnOK)

        # sizers
        btnSizer = wx.StdDialogButtonSizer()
        btnSizer.AddButton(btnCancel)
        btnSizer.AddButton(btnApply)
        btnSizer.AddButton(btnOk)
        btnSizer.Realize()
        
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(item=notebook, proportion=1, flag=wx.EXPAND | wx.ALL, border=5)
        mainSizer.Add(item=btnSizer, proportion=0, flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)

        self.SetSizer(mainSizer)
        mainSizer.Fit(self)

    def __CreateSymbologyPage(self, notebook):
        """Create notebook page concerning with symbology settings"""

        panel = wx.Panel(parent=notebook, id=wx.ID_ANY)
        notebook.AddPage(page=panel, text=_("Symbology"))

        sizer = wx.BoxSizer(wx.VERTICAL)
        
        flexSizer = wx.FlexGridSizer (cols=3, hgap=5, vgap=5)
        flexSizer.AddGrowableCol(0)
        
        for label, isCheckbox, color in self.__SymbologyData():
            textLabel = wx.StaticText(panel, wx.ID_ANY, label)
            if isCheckbox:
                enabled = wx.CheckBox(panel, id=wx.ID_ANY, label="")
            else:
                enabled = (1, 1)
            color = csel.ColourSelect(panel, id=wx.ID_ANY, colour=color, size=(25, 25))
            
            flexSizer.Add(textLabel, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL | wx.EXPAND)
            flexSizer.Add(enabled, proportion=0, flag=wx.ALIGN_CENTER | wx.FIXED_MINSIZE)
            flexSizer.Add(color, proportion=0, flag=wx.ALIGN_RIGHT | wx.FIXED_MINSIZE)

        sizer.Add(item=flexSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=10)
        
        panel.SetSizer(sizer)
        
        return panel

    def __CreateSettingsPage(self, notebook):
        """Create notebook page concerning with symbology settings"""

        panel = wx.Panel(parent=notebook, id=wx.ID_ANY)
        notebook.AddPage(page=panel, text=_("Settings"))

        border = wx.BoxSizer(wx.VERTICAL)
        
        box   = wx.StaticBox (parent=panel, id=wx.ID_ANY, label=" %s " % _("Display"))
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)

        #
        # display section
        #
        flexSizer = wx.FlexGridSizer (cols=3, hgap=5, vgap=5)
        flexSizer.AddGrowableCol(0)
        for label, defaultValue in self.__SettingsData():
            textLabel = wx.StaticText(parent=panel, id=wx.ID_ANY, label=label)
            value = wx.SpinCtrl(parent=panel, id=wx.ID_ANY, value=defaultValue, min=1, max=1e6)
            units = wx.Choice(parent=panel, id=wx.ID_ANY, choices=["screen pixels", "map units"])
            
            flexSizer.Add(textLabel, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL | wx.EXPAND | wx.ALIGN_CENTER_HORIZONTAL)
            flexSizer.Add(value, proportion=0, flag=wx.ALIGN_CENTER | wx.FIXED_MINSIZE)
            flexSizer.Add(units, proportion=0, flag=wx.ALIGN_RIGHT | wx.FIXED_MINSIZE)
        sizer.Add(item=flexSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=5)
        border.Add(item=sizer, proportion=0, flag=wx.ALL | wx.EXPAND, border=10)

        #
        # attributes
        #
        box   = wx.StaticBox (parent=panel, id=wx.ID_ANY, label=" %s " % _("Digitize new feature"))
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        # checkbox
        addRecord = wx.CheckBox(parent=panel, id=wx.ID_ANY, label=_("Add new record into table"))
        sizer.Add(item=addRecord, proportion=0, flag=wx.ALL, border=5)
        # settings
        flexSizer = wx.FlexGridSizer(cols=2, hgap=3, vgap=3)
        settings = ((_("Layer"), 1), (_("Category"), 1), (_("Mode"), _("Next to use")))
        for label, value in settings:
            text = wx.StaticText(parent=panel, id=wx.ID_ANY, label=label)
            if label is not "Mode":
                value = wx.TextCtrl(parent=panel, id=wx.ID_ANY, value=str(value)) # TODO: validator
            else:
                value = wx.Choice(parent=panel, id=wx.ID_ANY, choices=[_("Next to use"), _("Manual entry"), _("No category")])
            flexSizer.Add(item=text, proportion=0, flag=wx.EXPAND | wx.ALIGN_CENTER_VERTICAL)
            flexSizer.Add(item=value, proportion=0, flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
            
        sizer.Add(item=flexSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=1)
        border.Add(item=sizer, proportion=0, flag=wx.LEFT | wx.RIGHT | wx.BOTTOM | wx.EXPAND, border=10)

        panel.SetSizer(border)
        
        return panel

    def __SymbologyData(self):
        """
        Data for __CreateSymbologyPage()

        label | checkbox | color
        """
        
        return (
            ("Background", False, "white"),
            ("Highlight", False, "yellow"),
            ("Point", True, "black"),
            ("Line", True, "black"),
            ("Boundary (no area)", True, "grey"),
            ("Boundary (one area)", True, "orange"),
            ("Boundary (two areas)", True, "green"),
            ("Centroid (in area)", True, "blue"),
            ("Centroid (outside area)", True, "brown"),
            ("Centroid (duplicate in area)", True, "violet"),
            ("Node (one line)", True, "red"),
            ("Node (two lines)", True, "dark green"))

    def __SettingsData(self):
        """
        Data for __CreateSettingsPage()

        label | checkbox | default value
        """

        return (
            (_("Snapping threshold"), "10"),
            (_("Line width"), "2")
            )
    


##############################
# digitization class instance
##############################

Digit = VEdit()
