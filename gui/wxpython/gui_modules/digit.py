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

class AbstractDigit:
    """
    Abstract digitization class
    """
    def __init__(self, settings=None):
        self.map = None
        if not settings:
            self.settings = {}
            # symbology
            self.settings["symbolBackground"] = (None, "white") # enabled, color
            self.settings["symbolHighlight"] = (None, "yellow")
            self.settings["symbolPoint"] = (True, "black")
            self.settings["symbolLine"] = (True, "black")
            self.settings["symbolBoundaryNo"] = (True, "grey")
            self.settings["symbolBoundaryOne"] = (True, "orange")
            self.settings["symbolBoundaryTwo"] = (True, "green")
            self.settings["symbolCentroidIn"] = (True, "blue")
            self.settings["symbolCentroidOut"] = (True, "brown")
            self.settings["symbolCentroidDup"] = (True, "violet")
            self.settings["symbolNodeOne"] = (True, "red")
            self.settings["symbolNodeTwo"] = (True, "dark green")
            
            # display
            self.settings["snapping"] = (10, "screen pixels") # value, unit
            self.settings["lineWidth"] = (2, "screen pixels")
            # digitize new record
            self.settings["addRecord"] = True
            self.settings["layer"] = 1
            self.settings["category"] = 1
            self.settings["categoryMode"] = "Next to use"
        else:
            self.settings = settings

    def SetCategoryNextToUse(self):
        """Find maximum category number in the map layer
        and update Digit.settings['category']

        Returns 'True' on success, 'False' on failure
        """

        if self.map:
            categoryCmd = cmd.Command(cmd=["v.category", "-g", "--q",
                                           "input=%s" % self.map, 
                                           "option=report",
                                           "layer=%d" % self.settings["layer"]])

            if categoryCmd.returncode != 0:
                return
        
            for line in categoryCmd.ReadStdOutput():
                if "all" in line:
                    try:
                        maxCat = int(line.split(' ')[-1]) + 1
                        self.settings['category'] = maxCat
                    except:
                        return False
                    return True
        else:
            self.settings["category"] = 1

    def SetCategory(self):
        """Return category number to use (according Settings)"""
        if self.settings["categoryMode"] == "No category":
            self.settings["category"] = "None"
        elif self.settings["categoryMode"] == "Next to use":
            self.SetCategoryNextToUse()
        else:
            if self.settings["category"] == "None":
                self.SetCategoryNextToUse()

        return self.settings["category"]

    def ReInitialize(self, map):
        """Re-initialize settings according selected map layer"""
        self.map = map
        self.SetCategory()
    
class VEdit(AbstractDigit):
    """
    Prototype of digitization class based on v.edit command

    Note: This should be replaced by VDigit class.
    """
    def __init__(self, settings=None):
        AbstractDigit.__init__(self, settings)

    def AddPoint (self, map, type, x, y, z=None):
        """
        Add point/centroid to the vector map layer
        """
        if type == "centroid":
            key = "C"
        else:
            key = "P"

        layer = self.settings["layer"]
        cat   = self.SetCategory()
        
        addstring="""%s 1 1
                    %f %f\n%d %d""" % (key, x, y, layer, cat)

        Debug.msg (3, "VEdit.AddPoint(): map=%s, type=%s, layer=%d, cat=%d, x=%f, y=%f" % \
                   (map, type, layer, cat, x, y))
        
        self._AddFeature (map=map, input=addstring)

    def AddLine (self, map, type, coords):
        """
        Add line/boundary to the vector map layer
        """
        if len(coords) < 2:
            return

        layer = self.settings["layer"]
        cat   = self.SetCategory()
        
        if type == "boundary":
            key = "B"
        else:
            key = "L"
            
        addstring="""%s %d 1\n""" % (key, len(coords))
        for point in coords:
            addstring += """%f %f\n""" % \
                (float(point[0]), float(point [1]))

        addstring += "%d %d" % (layer, cat)

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

class VDigit(AbstractDigit):
    """
    Prototype of digitization class based on v.digit reimplementation

    Under development (wxWidgets C/C++ background)
    """
    pass

class DigitSettingsDialog(wx.Dialog):
    """
    Standard settings dialog for digitization purposes
    """
    def __init__(self, parent, title, style=wx.DEFAULT_DIALOG_STYLE):
        wx.Dialog.__init__(self, parent=parent, id=wx.ID_ANY, title=title, style=style)

        self.parent = parent # mapdisplay.BufferedWindow class instance

        # notebook
        notebook = wx.Notebook(parent=self, id=wx.ID_ANY, style=wx.BK_DEFAULT)
        self.__CreateSymbologyPage(notebook)
        Digit.SetCategory() # update category number (next to use)
        self.__CreateSettingsPage(notebook)
        
        # buttons
        btnApply = wx.Button(self, wx.ID_APPLY, _("Apply") )
        btnCancel = wx.Button(self, wx.ID_CANCEL)
        btnOk = wx.Button(self, wx.ID_OK, _("OK") )
        btnOk.SetDefault()

        # bindigs
        btnApply.Bind(wx.EVT_BUTTON, self.OnApply)
        btnOk.Bind(wx.EVT_BUTTON, self.OnOK)

        # sizers
        btnSizer = wx.StdDialogButtonSizer()
        btnSizer.AddButton(btnCancel)
        btnSizer.AddButton(btnApply)
        btnSizer.AddButton(btnOk)
        btnSizer.Realize()
        
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(item=notebook, proportion=1, flag=wx.EXPAND | wx.ALL, border=5)
        mainSizer.Add(item=btnSizer, proportion=0,
                      flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)

        self.SetSizer(mainSizer)
        mainSizer.Fit(self)

    def __CreateSymbologyPage(self, notebook):
        """Create notebook page concerning with symbology settings"""

        panel = wx.Panel(parent=notebook, id=wx.ID_ANY)
        notebook.AddPage(page=panel, text=_("Symbology"))

        sizer = wx.BoxSizer(wx.VERTICAL)
        
        flexSizer = wx.FlexGridSizer (cols=3, hgap=5, vgap=5)
        flexSizer.AddGrowableCol(0)

        self.symbology = {}
        for label, key in self.__SymbologyData():
            textLabel = wx.StaticText(panel, wx.ID_ANY, label)
            color = csel.ColourSelect(panel, id=wx.ID_ANY,
                                      colour=Digit.settings[key][1], size=(25, 25))
            isEnabled = Digit.settings[key][0]
            if isEnabled is not None:
                enabled = wx.CheckBox(panel, id=wx.ID_ANY, label="")
                enabled.SetValue(isEnabled)
                self.symbology[key] = (enabled, color)
            else:
                enabled = (1, 1)
                self.symbology[key] = (None, color)
            
            flexSizer.Add(textLabel, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL)
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
        
        #
        # display section
        #
        box   = wx.StaticBox (parent=panel, id=wx.ID_ANY, label=" %s " % _("Display"))
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        flexSizer = wx.FlexGridSizer (cols=3, hgap=5, vgap=5)
        flexSizer.AddGrowableCol(0)
        # snapping
        text = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Snapping threshold"))
        self.snappingValue = wx.SpinCtrl(parent=panel, id=wx.ID_ANY, size=(50, -1),
                                         value=str(Digit.settings["snapping"][0]), min=1, max=1e6)
        self.snappingUnit = wx.ComboBox(parent=panel, id=wx.ID_ANY, size=(125, -1),
                                         choices=["screen pixels", "map units"])
        self.snappingUnit.SetValue(Digit.settings["snapping"][1])
        flexSizer.Add(text, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(self.snappingValue, proportion=0, flag=wx.ALIGN_CENTER | wx.FIXED_MINSIZE)
        flexSizer.Add(self.snappingUnit, proportion=0, flag=wx.ALIGN_RIGHT | wx.FIXED_MINSIZE)
        # line width
        text = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Line width"))
        self.lineWidthValue = wx.SpinCtrl(parent=panel, id=wx.ID_ANY, size=(50, -1),
                                          value=str(Digit.settings["lineWidth"][0]),
                                          min=1, max=1e6)
        self.lineWidthUnit = wx.ComboBox(parent=panel, id=wx.ID_ANY, size=(125, -1),
                                         choices=["screen pixels", "map units"])
        self.lineWidthUnit.SetValue(Digit.settings["lineWidth"][1])
        flexSizer.Add(text, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(self.lineWidthValue, proportion=0, flag=wx.ALIGN_CENTER | wx.FIXED_MINSIZE)
        flexSizer.Add(self.lineWidthUnit, proportion=0, flag=wx.ALIGN_RIGHT | wx.FIXED_MINSIZE)

        sizer.Add(item=flexSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=1)
        border.Add(item=sizer, proportion=0, flag=wx.ALL | wx.EXPAND, border=5)

        #
        # attributes
        #
        box   = wx.StaticBox (parent=panel, id=wx.ID_ANY, label=" %s " % _("Digitize new feature"))
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        # checkbox
        self.addRecord = wx.CheckBox(parent=panel, id=wx.ID_ANY,
                                     label=_("Add new record into table"))
        self.addRecord.SetValue(Digit.settings["addRecord"])
        sizer.Add(item=self.addRecord, proportion=0, flag=wx.ALL | wx.EXPAND, border=1)
        # settings
        flexSizer = wx.FlexGridSizer(cols=2, hgap=3, vgap=3)
        flexSizer.AddGrowableCol(0)
        settings = ((_("Layer"), 1), (_("Category"), 1), (_("Mode"), _("Next to use")))
        # layer
        text = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Layer"))
        self.layer = wx.TextCtrl(parent=panel, id=wx.ID_ANY, size=(125, -1),
                                 value=str(Digit.settings["layer"])) # TODO: validator
        flexSizer.Add(item=text, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(item=self.layer, proportion=0,
                      flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
        # category number
        text = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Category number"))
        self.category = wx.TextCtrl(parent=panel, id=wx.ID_ANY, size=(125, -1),
                                    value=str(Digit.settings["category"])) # TODO: validator
        if Digit.settings["categoryMode"] != "Manual entry":
            self.category.SetEditable(False)
            self.category.Enable(False)
        flexSizer.Add(item=text, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(item=self.category, proportion=0,
                      flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
        # category mode
        text = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Category mode"))
        self.categoryMode = wx.ComboBox(parent=panel, id=wx.ID_ANY,
                                        style=wx.CB_SIMPLE | wx.CB_READONLY, size=(125, -1),
                                        choices=[_("Next to use"), _("Manual entry"), _("No category")])
        self.categoryMode.SetValue(Digit.settings["categoryMode"])
        flexSizer.Add(item=text, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(item=self.categoryMode, proportion=0,
                      flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)

        sizer.Add(item=flexSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=1)
        border.Add(item=sizer, proportion=0,
                   flag=wx.LEFT | wx.RIGHT | wx.BOTTOM | wx.EXPAND, border=5)

        # bindings
        self.Bind(wx.EVT_CHECKBOX, self.OnChangeAddRecord, self.addRecord)
        self.Bind(wx.EVT_COMBOBOX, self.OnChangeCategoryMode, self.categoryMode)
        
        panel.SetSizer(border)
        
        return panel

    def __SymbologyData(self):
        """
        Data for __CreateSymbologyPage()

        label | checkbox | color
        """

        return (
            ("Background", "symbolBackground"),
            ("Highlight", "symbolHighlight"),
            ("Point", "symbolPoint"),
            ("Line", "symbolLine"),
            ("Boundary (no area)", "symbolBoundaryNo"),
            ("Boundary (one area)", "symbolBoundaryOne"),
            ("Boundary (two areas)", "symbolBoundaryTwo"),
            ("Centroid (in area)", "symbolCentroidIn"),
            ("Centroid (outside area)", "symbolCentroidOut"),
            ("Centroid (duplicate in area)", "symbolCentroidDup"),
            ("Node (one line)", "symbolNodeOne"),
            ("Node (two lines)", "symbolNodeTwo"))

    def OnChangeCategoryMode(self, event):
        """Change category mode"""

        mode = event.GetString()
        Digit.settings["categoryMode"] = mode
        if mode == "Manual entry": # enable
            self.category.Enable(True)
            self.category.SetEditable(True)
        elif self.category.IsEnabled(): # disable
            self.category.SetEditable(False)
            self.category.Enable(False)

        if mode == "No category" and self.addRecord.IsChecked():
            self.addRecord.SetValue(False)
        Digit.SetCategory()
        self.category.SetValue(str(Digit.settings['category']))

    def OnChangeAddRecord(self, event):
        """Checkbox 'Add new record' status changed"""
        self.category.SetValue(str(Digit.SetCategory()))
            
    def OnOK(self, event):
        """Button 'OK' clicked"""
        self.UpdateSettings()
        self.Close()

    def OnApply(self, event):
        """Button 'Apply' clicked"""
        self.UpdateSettings()

    def UpdateSettings(self):
        """Update Digit.settings"""
        try:
            # symbology
            for key, (enabled, color) in self.symbology.iteritems():
                if enabled:
                    Digit.settings[key] = (enabled.IsChecked(), color.GetColour())
                else:
                    Digit.settings[key] = (None, color.GetColour())
            # display
            Digit.settings["snapping"] = (int(self.snappingValue.GetValue()), # value
                                          self.snappingUnit.GetValue()) # unit
            Digit.settings["lineWidth"] = (int(self.lineWidthValue.GetValue()),
                                           self.lineWidthUnit.GetValue())
            # digitize new feature
            Digit.settings["addRecord"] = self.addRecord.IsChecked()
            Digit.settings["layer"] = int(self.layer.GetValue())
            if Digit.settings["categoryMode"] == "No category":
                Digit.settings["category"] = None
            else:
                Digit.settings["category"] = int(self.category.GetValue())
            Digit.settings["categoryMode"] = self.categoryMode.GetValue()
        except:
            pass
    
##############################
# digitization class instance
##############################

Digit = VEdit()
