"""
MODULE: digit

CLASSES:
 * AbstractDigit 
 * VEdit
 * VDigit
 * AbstractDisplayDriver
 * CDisplayDriver
 * DigitSettingsDialog
 * DigitCategoryDialog

PURPOSE: Digitization tool wxPython GUI prototype

         Note: Initial version under development

         Progress:
          (1) v.edit called on the background (class VEdit)
          (2) Reimplentation of v.digit (VDigit)

         Import:
          from digit import Digit as Digit
          
AUTHORS: The GRASS Development Team
         Martin Landa <landa.martin gmail.com>

COPYRIGHT: (C) 2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import os
import sys
import string
import copy

import wx
import wx.lib.colourselect as csel
import wx.lib.mixins.listctrl as listmix

import gcmd
import dbm
from debug import Debug as Debug
import select 
try:
    driverPath = os.path.join( os.getenv("GISBASE"), "etc","wx", "display_driver")
    sys.path.append(driverPath)
    from grass6_wxdriver import DisplayDriver
except:
    print >> sys.stderr, "Digitization tool is disabled.\n" \
        "Under development...\n"

USEVEDIT = True
    
class AbstractDigit:
    """
    Abstract digitization class
    """
    def __init__(self, mapwindow, settings=None):
        self.map       = None

        Debug.msg (3, "AbstractDigit.__init__(): map=%s" % \
                   self.map)

        #self.SetCategory()

        # is unique for map window instance
        if not settings:
            self.settings = {}
            # symbology
#            self.settings["symbolBackground"] = (None, (255,255,255, 255)) # white
            self.settings["symbolHighlight"] = (None, (255, 255, 0, 255)) #yellow
            self.settings["symbolPoint"] = (True, (0, 0, 0, 255)) # black
            self.settings["symbolLine"] = (True, (0, 0, 0, 255)) # black
            self.settings["symbolBoundaryNo"] = (True, (126, 126, 126, 255)) # grey
            self.settings["symbolBoundaryOne"] = (True, (0, 255, 0, 255)) # green
            self.settings["symbolBoundaryTwo"] = (True, (255, 135, 0, 255)) # orange
            self.settings["symbolCentroidIn"] = (True, (0, 0, 255, 255)) # blue
            self.settings["symbolCentroidOut"] = (True, (165, 42, 42, 255)) # brown
            self.settings["symbolCentroidDup"] = (True, (156, 62, 206, 255)) # violet
            self.settings["symbolNodeOne"] = (True, (255, 0, 0, 255)) # red
            self.settings["symbolNodeTwo"] = (True, (0, 86, 45, 255)) # dark green
            self.settings["symbolVertex"] = (False, (255, 20, 147, 255)) # deep pink
            
            # display
            self.settings["lineWidth"] = (2, "screen pixels")

            # snapping
            self.settings["snapping"] = (10, "screen pixels") # value, unit
            self.settings["snapToVertex"] = False
            self.settings["backgroundMap"] = 'a1@martin'

            # digitize new record
            self.settings["addRecord"] = True
            self.settings["layer"] = 1
            self.settings["category"] = 1
            self.settings["categoryMode"] = "Next to use"
        else:
            self.settings = settings

        self.driver = CDisplayDriver(self, mapwindow)

    def SetCategoryNextToUse(self):
        """Find maximum category number in the map layer
        and update Digit.settings['category']

        Returns 'True' on success, 'False' on failure
        """
        # vector map layer without categories, reset to '1'
        self.settings['category'] = 1

        if self.map:
            categoryCmd = gcmd.Command(cmd=["v.category", "-g", "--q",
                                           "input=%s" % self.map, 
                                           "option=report",
                                           "layer=%d" % self.settings["layer"]])

            if categoryCmd.returncode != 0:
                return False
        
            for line in categoryCmd.ReadStdOutput():
                if "all" in line:
                    try:
                        maxCat = int(line.split(' ')[-1]) + 1
                        self.settings['category'] = maxCat
                    except:
                        return False
                    return True
    
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

    def SetMapName(self, map):
        """Set map name"""
        Debug.msg (3, "AbstractDigit.SetMapName map=%s" % map)
        self.map = map

        self.driver.Reset(self.map)
        
class VEdit(AbstractDigit):
    """
    Prototype of digitization class based on v.edit command

    Note: This should be replaced by VDigit class.
    """
    def __init__(self, mapwindow, settings=None):
        AbstractDigit.__init__(self, mapwindow, settings)

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
                    %f %f""" % (key, x, y)

        if layer > 0 and cat != "None":
            addstring += "\n%d %d" % (layer, cat)
            Debug.msg (3, "VEdit.AddPoint(): map=%s, type=%s, layer=%d, cat=%d, x=%f, y=%f" % \
                           (map, type, layer, cat, x, y))
        else:
            Debug.msg (3, "VEdit.AddPoint(): map=%s, type=%s, x=%f, y=%f" % \
                           (map, type, x, y))

        Debug.msg (4, "Vline.AddPoint(): input=%s" % addstring)
                
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
            flags = ['-c'] # close boundaries
        else:
            key = "L"
            flags = []
            
        addstring="""%s %d 1\n""" % (key, len(coords))
        for point in coords:
            addstring += """%f %f\n""" % \
                (float(point[0]), float(point [1]))

        if layer > 0 and cat != "None":
            addstring += "\n%d %d" % (layer, cat)
            Debug.msg (3, "Vline.AddLine(): type=%s, layer=%d, cat=%d coords=%s" % \
                           (key, layer, cat, coords))
        else:
            Debug.msg (3, "Vline.AddLine(): type=%s, layer=%d, cat=%d coords=%s" % \
                           (key, coords))

        Debug.msg (4, "VEdit.AddLine(): input=%s" % addstring)

        self._AddFeature (map=map, input=addstring, flags=flags)

    def _AddFeature (self, map, input, flags=[]):
        """
        General method which adds feature to the vector map
        """
                        
        if self.settings['snapping'][0] <= 0:
            snap = "no"
        else:
            if self.settings['snapToVertex']:
                snap = "vertex"
            else:
                snap = "node"

        command = ["v.edit", "-n", "--q", 
                   "map=%s" % map,
                   "tool=add",
                   "thresh=%f" % self.driver.GetThreshold(),
                   "snap=%s" % snap]

        if self.settings['backgroundMap'] != '':
            command.append("bgmap=%s" % self.settings['backgroundMap'])

        # additional flags
        for flag in flags:
            command.append(flag)

        # run the command
        Debug.msg(4, "VEdit._AddFeature(): input=%s" % input)
        vedit = gcmd.Command(cmd=command, stdin=input)

        # reload map (needed for v.edit)
        self.driver.ReloadMap()
        
    def DeleteSelectedLines(self):
        """Delete selected vector features from the vector map"""

        selected = self.driver.GetSelected() # grassId

        if len(selected) <= 0:
            return False

        ids = ",".join(["%d" % v for v in selected])

        Debug.msg(4, "Digit.DeleteSelectedLines(): ids=%s" % \
                      ids)

        command = [ "v.edit",
                    "map=%s" % self.map,
                    "tool=delete",
                    "ids=%s" % ids]

        # run the command
        vedit = gcmd.Command(cmd=command)

        # reload map (needed for v.edit)
        self.driver.ReloadMap()

        return True

    def MoveSelectedLines(self, move):
        """Move selected vector features"""
        return self.__MoveFeature("move", None, move)

    def MoveSelectedVertex(self, coords, move):
        """Move selected vertex of the line"""
        return self.__MoveFeature("vertexmove", coords, move)

    def __MoveFeature(self, tool, coords, move):
        """Move selected vector feature or vertex"""

        selected = self.driver.GetSelected()

        if len(selected) <= 0:
            return False

        ids = ",".join(["%d" % v for v in selected])

        Debug.msg(4, "Digit.MoveSelectedLines(): ids=%s, move=%s" % \
                      (ids, move))

        if self.settings['snapping'][0] <= 0:
            snap = "no"
        else:
            if self.settings['snapToVertex']:
                snap = "vertex"
            else:
                snap = "node"


        command = ["v.edit", "--q", 
                   "map=%s" % self.map,
                   "tool=%s" % tool,
                   "ids=%s" % ids,
                   "move=%f,%f" % (float(move[0]),float(move[1])),
                   "thresh=%f" % self.driver.GetThreshold(),
                   "snap=%s" % snap]

        if tool == "vertexmove":
            command.append("coords=%f,%f" % (float(coords[0]), float(coords[1])))
            command.append("-1") # modify only first selected
                                             
        # run the command
        vedit = gcmd.Command(cmd=command)

        # reload map (needed for v.edit)
        self.driver.ReloadMap()

        return True

    def SplitLine(self, coords):
        """Split selected line on position 'coords'"""

        try:
            line = self.driver.GetSelected()[0]
        except:
            return False

        command = ["v.edit", "--q",
                   "map=%s" % self.map,
                   "tool=break",
                   "ids=%s" % line,
                   "coords=%f,%f" % (float(coords[0]),float(coords[1])),
                   "thresh=%f" % self.driver.GetThreshold()]

        # run the command
        vedit = gcmd.Command(cmd=command)

        # redraw map
        self.driver.ReloadMap()
        
        return True

    def AddVertex(self, coords):
        """Add new vertex to the selected line on position 'coords'"""
        return self.__ModifyVertex(coords, "vertexadd")

    def RemoveVertex(self, coords):
        """Remove vertex from the selected line on position 'coords'"""
        return self.__ModifyVertex(coords, "vertexdel")
    
    def __ModifyVertex(self, coords, action):
        
        try:
            line = self.driver.GetSelected()[0]
        except:
            return False

        command = ["v.edit", "--q",
                   "map=%s" % self.map,
                   "tool=%s" % action,
                   "ids=%s" % line,
                   "coords=%f,%f" % (float(coords[0]),float(coords[1])),
                   "thresh=%f" % self.driver.GetThreshold()]

        # run the command
        vedit = gcmd.Command(cmd=command)

        # reload map (needed for v.edit)
        self.driver.ReloadMap()

        return True

    def CopyCats(self, cats, ids):
        """Copy given categories to objects with id listed in ids"""
        if len(cats) == 0 or len(ids) == 0:
            return False

        # collect cats
        gcmd.Command(['v.edit',
                     '--q',
                     'map=%s' % self.map,
                     'tool=catadd',
                     'cats=%s' % ",".join(["%d" % v for v in cats]),
                     'ids=%s' % ",".join(["%d" % v for v in ids])])
        
        return True

    def EditLine(self, line, coords):
        """Edit existing line"""
        # remove line
        vEditDelete = gcmd.Command(['v.edit',
                                   '--q',
                                   'map=%s' % self.map,
                                   'tool=delete',
                                   'ids=%s' % line])

        # add line
        if len(coords) > 0:
            self.AddLine(self.map, "line", coords)

        # reload map (needed for v.edit)
        self.driver.ReloadMap()

    def __ModifyLines(self, tool):
        """General method to modify selected lines"""

        ids = self.driver.GetSelected()

        if len(ids) <= 0:
            return False

        vEdit = ['v.edit',
                 '--q',
                 'map=%s' % self.map,
                 'tool=%s' % tool,
                 'ids=%s' % ",".join(["%d" % v for v in ids])]

        if tool in ['snap', 'connect']:
            vEdit.append("thresh=%f" % self.driver.GetThreshold())

        runCmd = gcmd.Command(vEdit)

        # reload map (needed for v.edit)
        self.driver.ReloadMap()
                        
        return True

    def FlipLine(self):
        """Flip selected lines"""

        return self.__ModifyLines('flip')

    def MergeLine(self):
        """Merge selected lines"""

        return self.__ModifyLines('merge')

    def SnapLine(self):
        """Snap selected lines"""

        return self.__ModifyLines('snap')

    def ConnectLine(self):
        """Connect selected lines"""

        return self.__ModifyLines('connect')

    def CopyLine(self, ids=None):
        """Copy features from (background) vector map"""
        
        if not ids:
            ids = self.driver.GetSelected()

        if len(ids) <= 0:
            return False

        vEdit = ['v.edit',
                 '--q',
                 'map=%s' % self.map,
                 'tool=copy',
                 'ids=%s' % ",".join(["%d" % v for v in ids])]

        if self.settings['backgroundMap'] != '':
            vEdit.append('bgmap=%s' % self.settings['backgroundMap'])

        runCmd = gcmd.Command(vEdit)

        # reload map (needed for v.edit)
        self.driver.ReloadMap()
                        
        return True

    def SelectLinesFromBackgroundMap(self, pos1, pos2):
        """Select features from background map

        pos1, pos2: bounding box defifinition
        """

        if self.settings['backgroundMap'] == '':
            Debug.msg(4, "VEdit.SelectLinesFromBackgroundMap(): []")
            return []

        print "#", pos1, pos2
        x1, y1 = pos1
        x2, y2 = pos2

        vEditCmd = gcmd.Command(['v.edit',
                                 '--q',
                                 'map=%s' % self.map,
                                 'tool=select',
                                 # 'bbox=%f,%f,%f,%f' % (pos1[0], pos1[1], pos2[0], pos2[1])])
                                 'polygon=%f,%f,%f,%f,%f,%f,%f,%f,%f,%f' % \
                                     (x1, y1, x2, y1, x2, y2, x1, y2, x1, y1)])
                                     
        try:
            output = vEditCmd.ReadStdOutput()[0][0]
            print "#", output
            ids = output.split(',') #first line & item in list
            ids = map(int, ids) # str -> int
        except:
            return []

        Debug.msg(4, "VEdit.SelectLinesFromBackgroundMap(): %s" % \
                      ",".join(["%d" % v for v in ids]))
        
        return ids

class VDigit(AbstractDigit):
    """
    Prototype of digitization class based on v.digit reimplementation

    Under development (wxWidgets C/C++ background)
    """
    def __init__(self, mapwindow, settings=None):
        AbstractDigit.__init__(self, mapwindow, settings)

        try:
            from grass6_wxdriver import DigitClass
            self.digit = DigitClass()
        except:
            self.digit = None

    def DeleteSelectedLines(self):
        """Delete selected vector features from the vector map"""

        selected = self.driver.GetSelected() # grassId

        if len(selected) <= 0:
            return False

        ids = ",".join(["%d" % v for v in selected])

        Debug.msg(4, "Digit.DeleteSelectedLines(): ids=%s" % \
                      ids)

        self.digit.DeleteSelectedLines()
        #self.driver.DrawUpdatedLines()

        return True

if USEVEDIT:
    class Digit(VEdit):
        """Default digit class"""
        def __init__(self, mapwindow):
            VEdit.__init__(self, mapwindow)
else:
    class Digit(VDigit):
        """Default digit class"""
        def __init__(self, mapwindow):
            VDigit.__init__(self, mapwindow)

class AbstractDisplayDriver:
    """Abstract classs for display driver"""
    def __init__(self, parent, mapwindow):
        self.parent      = parent
        self.mapwindow   = mapwindow
        
        self.ids         = {}   # dict[g6id] = [pdcId]
        self.selected    = []   # list of selected objects (grassId!)

    def GetThreshold(self, value=None, units=None):
        """Return threshold in map units"""
        if not value:
            value = self.parent.settings["snapping"][0]

        if not units:
            units = self.parent.settings["snapping"][1]

        if units == "screen pixels":
            # pixel -> cell
            reg = self.mapwindow.Map.region
            if reg['nsres'] > reg['ewres']:
                res = reg['nsres']
            else:
                res = reg['ewres']
                
            threshold = value * res
        else:
            threshold = value

        Debug.msg(4, "AbstractDisplayDriver.GetThreshold(): thresh=%f" % threshold)
        
        return threshold

class CDisplayDriver(AbstractDisplayDriver):
    """
    Display driver using grass6_wxdriver module
    """
    def __init__(self, parent, mapwindow):
        AbstractDisplayDriver.__init__(self, parent, mapwindow)

        self.mapWindow = mapwindow

        # initialize wx display driver
        try:
            self.__display = DisplayDriver(mapwindow.pdcVector)
        except:
            self.__display = None
            
        settings = self.parent.settings
        self.UpdateSettings()

    def SetDevice(self, pdc):
        """Set device for driver"""
        self.__display.SetDevice(pdc)
            
    def Reset(self, map):
        """Close map and open new one"""
        if map:
            name, mapset = map.split('@')
            self.__display.OpenMap(str(name), str(mapset))
        else:
            self.__display.CloseMap()
    
    def ReloadMap(self):
        """Reload map (close and re-open).

        Needed for v.edit, TODO: get rid of that..."""
        
        Debug.msg(4, "CDisplayDriver.ReloadMap():")
        self.__display.ReloadMap()

    def DrawMap(self):
        """Draw vector map layer content

        Return wx.Image 
        """
        import time
        start = time.clock()
        nlines = self.__display.DrawMap(True) # force
        stop = time.clock()
        Debug.msg(3, "CDisplayDriver.DrawMap(): nlines=%d, sec=%f" % \
                      (nlines, stop-start))

        return nlines

    def SelectLinesByBox(self, begin, end, type=None):
        """Select vector features by given bounding box.

        If type is given, only vector features of given type are selected.
        """

        x1, y1 = begin
        x2, y2 = end

        nselected = self.__display.SelectLinesByBox(x1, y1, x2, y2)
        Debug.msg(4, "CDisplayDriver.SelectLinesByBox(): selected=%d" % \
                      nselected)

        return nselected

    def SelectLineByPoint(self, point, type=None):
        """Select vector feature by coordinates of click point (in given threshold).

        If type is given, only vector features of given type are selected.
        """

        ftype = -1 # all types
        if type:
            if type == "point":
                ftype = 0
            else:
                ftype = 1 # line
            
        pointOnLine = self.__display.SelectLineByPoint(point[0], point[1],
                                                       self.GetThreshold(),
                                                       ftype); 

        if len(pointOnLine) > 0:
            Debug.msg(4, "CDisplayDriver.SelectLineByPoint(): pointOnLine=%f,%f" % \
                          (pointOnLine[0], pointOnLine[1]))
            return pointOnLine
        else:
            Debug.msg(4, "CDisplayDriver.SelectLineByPoint(): no line found")
            return None
        
    def GetSelected(self, grassId=True):
        """Return ids of selected vector features
        
        If grassId is True returns GRASS ids, otherwise
        internal ids of objects drawn in PseudoDC"""
        
        if grassId:
            selected = self.__display.GetSelected(True)
        else:
            selected = self.__display.GetSelected(False)
            
        Debug.msg(4, "CDisplayDriver.GetSelected(): grassId=%d, ids=%s" % \
                      (grassId, (",".join(["%d" % v for v in selected]))))
            
        return selected

    def GetSelectedVertex(self, coords):
        """Get PseudoDC id(s) of vertex (of selected line)
        on position 'coords'"""

        x, y = coords

        id = self.__display.GetSelectedVertex(x, y)

        Debug.msg(4, "CDisplayDriver.GetSelectedVertex(): id=%s" % \
                      (",".join(["%d" % v for v in id])))

        return id 

    def Unselect(self):
        """Delesect selected vector features"""

        self.__display.Unselect()

    def SetSelected(self, id):
        """Set selected vector features"""

        Debug.msg(4, "CDisplayDriver.SetSelected(): id=%s" % id)

        self.__display.SetSelected(id)

    def UpdateRegion(self):
        """Set geographical region
        
        Needed for 'cell2pixel' conversion"""
        
        map = self.mapwindow.Map
        reg = map.region
        
        self.__display.SetRegion(reg['n'],
                                 reg['s'],
                                 reg['e'],
                                 reg['w'],
                                 reg['nsres'],
                                 reg['ewres'],
                                 reg['center_easting'],
                                 reg['center_northing'],
                                 map.width, map.height)
    
    def UpdateSettings(self):
        """Update display driver settings"""
        settings = self.parent.settings
        # TODO map units

        if not self.__display:
            return
        
        self.__display.SetSettings (wx.Color(settings['symbolHighlight'][1][0],
                                           settings['symbolHighlight'][1][1],
                                           settings['symbolHighlight'][1][2],
                                           255).GetRGB(),
                                  settings['symbolPoint'][0],
                                  wx.Color(settings['symbolPoint'][1][0],
                                           settings['symbolPoint'][1][1],
                                           settings['symbolPoint'][1][2],
                                           255).GetRGB(),
                                  settings['symbolLine'][0],
                                  wx.Color(settings['symbolLine'][1][0],
                                           settings['symbolLine'][1][1],
                                           settings['symbolLine'][1][2],
                                           255).GetRGB(),
                                  settings['symbolBoundaryNo'][0],
                                  wx.Color(settings['symbolBoundaryNo'][1][0],
                                           settings['symbolBoundaryNo'][1][1],
                                           settings['symbolBoundaryNo'][1][2],
                                           255).GetRGB(),
                                  settings['symbolBoundaryOne'][0],
                                  wx.Color(settings['symbolBoundaryOne'][1][0],
                                           settings['symbolBoundaryOne'][1][1],
                                           settings['symbolBoundaryOne'][1][2],
                                           255).GetRGB(),
                                  settings['symbolBoundaryTwo'][0],
                                  wx.Color(settings['symbolBoundaryTwo'][1][0],
                                           settings['symbolBoundaryTwo'][1][1],
                                           settings['symbolBoundaryTwo'][1][2],
                                           255).GetRGB(),
                                  settings['symbolCentroidIn'][0],
                                  wx.Color(settings['symbolCentroidIn'][1][0],
                                           settings['symbolCentroidIn'][1][1],
                                           settings['symbolCentroidIn'][1][2],
                                           255).GetRGB(),
                                  settings['symbolCentroidOut'][0],
                                  wx.Color(settings['symbolCentroidOut'][1][0],
                                           settings['symbolCentroidOut'][1][1],
                                           settings['symbolCentroidOut'][1][2],
                                           255).GetRGB(),
                                  settings['symbolCentroidDup'][0],
                                  wx.Color(settings['symbolCentroidDup'][1][0],
                                           settings['symbolCentroidDup'][1][1],
                                           settings['symbolCentroidDup'][1][2],
                                           255).GetRGB(),
                                  settings['symbolNodeOne'][0],
                                  wx.Color(settings['symbolNodeOne'][1][0],
                                           settings['symbolNodeOne'][1][1],
                                           settings['symbolNodeOne'][1][2],
                                           255).GetRGB(),
                                  settings['symbolNodeTwo'][0],
                                  wx.Color(settings['symbolNodeTwo'][1][0],
                                           settings['symbolNodeTwo'][1][1],
                                           settings['symbolNodeTwo'][1][2],
                                           255).GetRGB(),
                                  settings['symbolVertex'][0],
                                  wx.Color(settings['symbolVertex'][1][0],
                                           settings['symbolVertex'][1][1],
                                           settings['symbolVertex'][1][2],
                                           255).GetRGB(),
                                  settings['lineWidth'][0])

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
        parent.digit.SetCategory() # update category number (next to use)
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
                                      colour=self.parent.digit.settings[key][1], size=(25, 25))
            isEnabled = self.parent.digit.settings[key][0]
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
        # line width
        text = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Line width"))
        self.lineWidthValue = wx.SpinCtrl(parent=panel, id=wx.ID_ANY, size=(50, -1),
                                          initial=self.parent.digit.settings["lineWidth"][0],
                                          min=1, max=1e6)
        self.lineWidthUnit = wx.ComboBox(parent=panel, id=wx.ID_ANY, size=(125, -1),
                                         choices=["screen pixels", "map units"])
        self.lineWidthUnit.SetValue(self.parent.digit.settings["lineWidth"][1])
        flexSizer.Add(text, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(self.lineWidthValue, proportion=0, flag=wx.ALIGN_CENTER | wx.FIXED_MINSIZE)
        flexSizer.Add(self.lineWidthUnit, proportion=0, flag=wx.ALIGN_RIGHT | wx.FIXED_MINSIZE)

        sizer.Add(item=flexSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=1)
        border.Add(item=sizer, proportion=0, flag=wx.ALL | wx.EXPAND, border=5)

        #
        # snapping section
        #
        box   = wx.StaticBox (parent=panel, id=wx.ID_ANY, label=" %s " % _("Snapping"))
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        flexSizer1 = wx.FlexGridSizer (cols=3, hgap=5, vgap=5)
        flexSizer1.AddGrowableCol(0)
        flexSizer2 = wx.FlexGridSizer (cols=2, hgap=5, vgap=5)
        flexSizer2.AddGrowableCol(0)
        # snapping
        text = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Snapping threshold"))
        self.snappingValue = wx.SpinCtrl(parent=panel, id=wx.ID_ANY, size=(50, -1),
                                         initial=self.parent.digit.settings["snapping"][0],
                                         min=0, max=1e6)
        self.snappingValue.Bind(wx.EVT_SPINCTRL, self.OnChangeSnappingValue)
        self.snappingUnit = wx.ComboBox(parent=panel, id=wx.ID_ANY, size=(125, -1),
                                         choices=["screen pixels", "map units"])
        self.snappingUnit.SetValue(self.parent.digit.settings["snapping"][1])
        self.snappingUnit.Bind(wx.EVT_COMBOBOX, self.OnChangeSnappingUnits)
        flexSizer1.Add(text, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer1.Add(self.snappingValue, proportion=0, flag=wx.ALIGN_CENTER | wx.FIXED_MINSIZE)
        flexSizer1.Add(self.snappingUnit, proportion=0, flag=wx.ALIGN_RIGHT | wx.FIXED_MINSIZE)
        # background map
        text = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Backgroud vector map"))
        self.backgroundMap = select.Select(parent=panel, id=wx.ID_ANY, size=(200,-1),
                                           type="vector")
        self.backgroundMap.SetValue(self.parent.digit.settings["backgroundMap"])
        self.backgroundMap.Bind(wx.EVT_TEXT, self.OnChangeBackgroundMap)
        flexSizer2.Add(text, proportion=1, flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer2.Add(self.backgroundMap, proportion=1, flag=wx.ALIGN_CENTER | wx.FIXED_MINSIZE)
        #flexSizer.Add(self.snappingUnit, proportion=0, flag=wx.ALIGN_RIGHT | wx.FIXED_MINSIZE)

        vertexSizer = wx.BoxSizer(wx.VERTICAL)
        self.snapVertex = wx.CheckBox(parent=panel, id=wx.ID_ANY,
                                      label=_("Snap also to vertex"))
        self.snapVertex.SetValue(self.parent.digit.settings["snapToVertex"])
        vertexSizer.Add(item=self.snapVertex, proportion=0, flag=wx.EXPAND)
        self.mapUnits = self.parent.MapWindow.Map.ProjInfo()['units']
        self.snappingInfo = wx.StaticText(parent=panel, id=wx.ID_ANY,
                                          label=_("Snapping threshold is currently %.1f %s") % \
                                              (self.parent.digit.driver.GetThreshold(),
                                               self.mapUnits))
        vertexSizer.Add(item=self.snappingInfo, proportion=0,
                        flag=wx.ALL | wx.EXPAND, border=1)

        sizer.Add(item=flexSizer1, proportion=1, flag=wx.TOP | wx.LEFT | wx.EXPAND, border=1)
        sizer.Add(item=flexSizer2, proportion=1, flag=wx.TOP | wx.LEFT | wx.EXPAND, border=1)
        sizer.Add(item=vertexSizer, proportion=1, flag=wx.BOTTOM | wx.LEFT | wx.EXPAND, border=1)
        border.Add(item=sizer, proportion=0, flag=wx.ALL | wx.EXPAND, border=5)

        #
        # attributes
        #
        box   = wx.StaticBox (parent=panel, id=wx.ID_ANY, label=" %s " % _("Digitize new feature"))
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        # checkbox
        self.addRecord = wx.CheckBox(parent=panel, id=wx.ID_ANY,
                                     label=_("Add new record into table"))
        self.addRecord.SetValue(self.parent.digit.settings["addRecord"])
        sizer.Add(item=self.addRecord, proportion=0, flag=wx.ALL | wx.EXPAND, border=1)
        # settings
        flexSizer = wx.FlexGridSizer(cols=2, hgap=3, vgap=3)
        flexSizer.AddGrowableCol(0)
        settings = ((_("Layer"), 1), (_("Category"), 1), (_("Mode"), _("Next to use")))
        # layer
        text = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Layer"))
        self.layer = wx.TextCtrl(parent=panel, id=wx.ID_ANY, size=(125, -1),
                                 value=str(self.parent.digit.settings["layer"])) # TODO: validator
        flexSizer.Add(item=text, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(item=self.layer, proportion=0,
                      flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
        # category number
        text = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Category number"))
        self.category = wx.TextCtrl(parent=panel, id=wx.ID_ANY, size=(125, -1),
                                    value=str(self.parent.digit.settings["category"])) # TODO: validator
        if self.parent.digit.settings["categoryMode"] != "Manual entry":
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
        self.categoryMode.SetValue(self.parent.digit.settings["categoryMode"])
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
            #            ("Background", "symbolBackground"),
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
            ("Node (two lines)", "symbolNodeTwo"),
            ("Vertex", "symbolVertex"))

    def OnChangeCategoryMode(self, event):
        """Change category mode"""

        mode = event.GetString()
        self.parent.digit.settings["categoryMode"] = mode
        if mode == "Manual entry": # enable
            self.category.Enable(True)
            self.category.SetEditable(True)
        elif self.category.IsEnabled(): # disable
            self.category.SetEditable(False)
            self.category.Enable(False)

        if mode == "No category" and self.addRecord.IsChecked():
            self.addRecord.SetValue(False)
        self.parent.digit.SetCategory()
        self.category.SetValue(str(self.parent.digit.settings['category']))

    def OnChangeAddRecord(self, event):
        """Checkbox 'Add new record' status changed"""
        self.category.SetValue(str(self.parent.digit.SetCategory()))
            
    def OnChangeSnappingValue(self, event):
        """Change snapping value - update static text"""
        value = self.snappingValue.GetValue()
        threshold = self.parent.digit.driver.GetThreshold(value)
        self.snappingInfo.SetLabel(_("Snapping threshold is currently %.1f %s") % \
                                       (threshold,
                                        self.mapUnits))

        event.Skip()

    def OnChangeSnappingUnits(self, event):
        """Snapping units change -> update static text"""
        value = self.snappingValue.GetValue()
        units = self.snappingUnit.GetValue()
        threshold = self.parent.digit.driver.GetThreshold(value, units)

        if units == "map units":
            self.snappingInfo.SetLabel(_("Snapping threshold is %.1f %s") % \
                                           (value,
                                            self.mapUnits))
        else:
            self.snappingInfo.SetLabel(_("Snapping threshold is %.1f %s") % \
                                           (threshold,
                                            self.mapUnits))
            
        event.Skip()

    def OnChangeBackgroundMap(self, event):
        """Change background map"""
        map = self.backgroundMap.GetValue()
        
        self.parent.digit.settings['backgroundMap'] = map
        
    def OnOK(self, event):
        """Button 'OK' clicked"""
        self.UpdateSettings()
        self.Close()

    def OnApply(self, event):
        """Button 'Apply' clicked"""
        self.UpdateSettings()

    def UpdateSettings(self):
        """Update self.parent.digit.settings"""
        # symbology
        for key, (enabled, color) in self.symbology.iteritems():
            if enabled:
                self.parent.digit.settings[key] = (enabled.IsChecked(), color.GetColour())
            else:
                self.parent.digit.settings[key] = (None, color.GetColour())
        # display
        self.parent.digit.settings["lineWidth"] = (int(self.lineWidthValue.GetValue()),
                                                   self.lineWidthUnit.GetValue())

        # snapping
        self.parent.digit.settings["snapping"] = (int(self.snappingValue.GetValue()), # value
                                      self.snappingUnit.GetValue()) # unit
        self.parent.digit.settings["snapToVertex"] = self.snapVertex.IsChecked()
        
        # digitize new feature
        self.parent.digit.settings["addRecord"] = self.addRecord.IsChecked()
        self.parent.digit.settings["layer"] = int(self.layer.GetValue())
        if self.parent.digit.settings["categoryMode"] == "No category":
            self.parent.digit.settings["category"] = None
        else:
            self.parent.digit.settings["category"] = int(self.category.GetValue())
        self.parent.digit.settings["categoryMode"] = self.categoryMode.GetValue()

        # threshold
        try:
            self.parent.digit.threshold = self.parent.digit.driver.GetThreshold()
        except:
            pass

        # update driver settings
        self.parent.digit.driver.UpdateSettings()

        # redraw map if auto-rendering is enabled
        if self.parent.autoRender.GetValue(): 
            self.parent.ReRender(None)

class DigitCategoryDialog(wx.Dialog, listmix.ColumnSorterMixin):
    """
    Dialog used to display/modify categories of vector objects
    """
    def __init__(self, parent, title,
                 map, queryCoords, qdist,
                 pos=wx.DefaultPosition,
                 style=wx.DEFAULT_DIALOG_STYLE | wx.RESIZE_BORDER):

        # parent
        self.parent = parent # mapdisplay.BufferedWindow class instance

        # map name
        self.map = map

        # line id (if not found remains 'None')
        self.line = None

        # {layer: [categories]}
        self.cats = {}
        # do not display dialog if no line is found (-> self.cats)
        if self.__GetCategories(queryCoords, qdist) == 0 or not self.line:
            Debug.msg(3, "DigitCategoryDialog(): nothing found!")
            return
        
        # make copy of cats (used for 'reload')
        self.cats_orig = copy.deepcopy(self.cats)

        Debug.msg(3, "DigitCategoryDialog(): line=%d, cats=%s" % \
                      (self.line, self.cats))

        wx.Dialog.__init__(self, parent=self.parent, id=wx.ID_ANY, title=title, style=style)

        # list of categories
        box = wx.StaticBox(parent=self, id=wx.ID_ANY,
                           label=" %s " % _("List of categories"))
        listSizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        self.list = CategoryListCtrl(parent=self, id=wx.ID_ANY,
                                     style=wx.LC_REPORT |
                                     wx.BORDER_NONE |
                                     wx.LC_SORT_ASCENDING |
                                     wx.LC_HRULES |
                                     wx.LC_VRULES)
        # sorter
        self.itemDataMap = self.list.Populate()
        listmix.ColumnSorterMixin.__init__(self, 2)

        listSizer.Add(item=self.list, proportion=1, flag=wx.EXPAND)

        # add new category
        box = wx.StaticBox(parent=self, id=wx.ID_ANY,
                           label=" %s " % _("Add new category"))
        addSizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        flexSizer = wx.FlexGridSizer (cols=5, hgap=5, vgap=5)
        flexSizer.AddGrowableCol(3)

        layerNewTxt = wx.StaticText(parent=self, id=wx.ID_ANY,
                                 label="%s:" % _("Layer"))
        self.layerNew = wx.TextCtrl(parent=self, id=wx.ID_ANY, size=(50, -1),
                                    value="1")
        catNewTxt = wx.StaticText(parent=self, id=wx.ID_ANY,
                               label="%s:" % _("Category"))
        try:
            newCat = max(self.cats[1]) + 1
        except:
            newCat = 1
        self.catNew = wx.TextCtrl(parent=self, id=wx.ID_ANY, size=(50, -1),
                             value=str(newCat))
        btnAddCat = wx.Button(self, wx.ID_ADD)
        flexSizer.Add(item=layerNewTxt, proportion=0,
                      flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(item=self.layerNew, proportion=0,
                      flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(item=catNewTxt, proportion=0,
                      flag=wx.ALIGN_CENTER_VERTICAL | wx.ALIGN_RIGHT | wx.LEFT,
                      border=10)
        flexSizer.Add(item=self.catNew, proportion=0,
                      flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(item=btnAddCat, proportion=0,
                      flag=wx.EXPAND | wx.ALIGN_RIGHT | wx.FIXED_MINSIZE)
        addSizer.Add(item=flexSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=5)

        # buttons
        btnApply = wx.Button(self, wx.ID_APPLY)
        btnCancel = wx.Button(self, wx.ID_CANCEL)
        btnReload = wx.Button(self, wx.ID_UNDO, _("Reload"))
        btnOk = wx.Button(self, wx.ID_OK)
        btnOk.SetDefault()

        # sizers
        btnSizer = wx.StdDialogButtonSizer()
        btnSizer.AddButton(btnCancel)
        btnSizer.AddButton(btnReload)
        btnSizer.SetNegativeButton(btnReload)
        btnSizer.AddButton(btnApply)
        btnSizer.AddButton(btnOk)
        btnSizer.Realize()
        
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(item=listSizer, proportion=1,
                      flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)
        mainSizer.Add(item=addSizer, proportion=0,
                      flag=wx.EXPAND | wx.ALIGN_CENTER |
                      wx.LEFT | wx.RIGHT | wx.BOTTOM, border=5)
        mainSizer.Add(item=btnSizer, proportion=0,
                      flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)

        self.SetSizer(mainSizer)
        mainSizer.Fit(self)
        self.SetAutoLayout(True)

        # set min size for dialog
        self.SetMinSize(self.GetBestSize())

        # bindings
        # buttons
        btnReload.Bind(wx.EVT_BUTTON, self.OnReload)
        btnApply.Bind(wx.EVT_BUTTON, self.OnApply)
        btnOk.Bind(wx.EVT_BUTTON, self.OnOK)
        btnAddCat.Bind(wx.EVT_BUTTON, self.OnAddCat)

        # list
        # self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected, self.list)
        # self.list.Bind(wx.EVT_RIGHT_DOWN, self.OnRightDown)
        self.list.Bind(wx.EVT_COMMAND_RIGHT_CLICK, self.OnRightUp) #wxMSW
        self.list.Bind(wx.EVT_RIGHT_UP, self.OnRightUp) #wxGTK
        self.Bind(wx.EVT_LIST_BEGIN_LABEL_EDIT, self.OnBeginEdit, self.list)
        self.Bind(wx.EVT_LIST_END_LABEL_EDIT, self.OnEndEdit, self.list)
        self.Bind(wx.EVT_LIST_COL_CLICK, self.OnColClick, self.list)

    def GetListCtrl(self):
        """Used by ColumnSorterMixin"""
        return self.list

    def OnColClick(self, event):
        """Click on column header (order by)"""
        event.Skip()
        
    def OnBeginEdit(self, event):
        """Editing of item started"""
        event.Allow()

    def OnEndEdit(self, event):
        """Finish editing of item"""
        itemIndex = event.GetIndex()
        layerOld = int (self.list.GetItem(itemIndex, 0).GetText())
        catOld = int (self.list.GetItem(itemIndex, 1).GetText())

        if event.GetColumn() == 0:
            layerNew = int(event.GetLabel())
            catNew = catOld
        else:
            layerNew = layerOld
            catNew = int(event.GetLabel())

        try:
            if layerNew not in self.cats.keys():
                self.cats[layerNew] = []
            self.cats[layerNew].append(catNew)
            self.cats[layerOld].remove(catOld)
        except:
            event.Veto()
            self.list.SetStringItem(itemIndex, 0, str(layerNew))
            self.list.SetStringItem(itemIndex, 1, str(catNew))
            dlg = wx.MessageDialog(self, _("Unable to add new layer/category <%s/%s>.\n"
                                           "Layer and category number must be integer.\n"
                                           "Layer number must be greater then zero.") %
                                   (str(self.layerNew.GetValue()), str(self.catNew.GetValue())),
                                   _("Error"), wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return False
    def OnRightDown(self, event):
        """Mouse right button down"""
        x = event.GetX()
        y = event.GetY()
        item, flags = self.list.HitTest((x, y))

        if item !=  wx.NOT_FOUND and \
                flags & wx.LIST_HITTEST_ONITEM:
            self.list.Select(item)

        event.Skip()

    def OnRightUp(self, event):
        """Mouse right button up"""
        if not hasattr(self, "popupID1"):
            self.popupID1 = wx.NewId()
            self.popupID2 = wx.NewId()
            self.Bind(wx.EVT_MENU, self.OnItemDelete, id=self.popupID1)
            self.Bind(wx.EVT_MENU, self.OnItemDeleteAll, id=self.popupID2)

        # generate popup-menu
        menu = wx.Menu()
        menu.Append(self.popupID1, _("Delete selected"))
        menu.Append(self.popupID2, _("Delete all"))

        self.PopupMenu(menu)
        menu.Destroy()

    def OnItemSelected(self, event):
        """Item selected"""
        event.Skip()

    def OnItemDelete(self, event):
        """Delete selected item(s) from the list (layer/category pair)"""
        item = self.list.GetFirstSelected()
        while item != -1:
            layer = int (self.list.GetItem(item, 0).GetText())
            cat = int (self.list.GetItem(item, 1).GetText())
            self.list.DeleteItem(item)
            self.cats[layer].remove(cat)

            item = self.list.GetFirstSelected()
            
        event.Skip()
        
    def OnItemDeleteAll(self, event):
        """Delete all items from the list"""
        self.list.DeleteAllItems()
        self.cats = {}

        event.Skip()

    def __GetCategories(self, coords, qdist):
        """Get layer/category pairs for all available
        layers

        Return True line found or False if not found"""
        
        cmdWhat = gcmd.Command(cmd=['v.what',
                                   '--q',
                                   'map=%s' % self.map,
                                   'east_north=%f,%f' % \
                                       (float(coords[0]), float(coords[1])),
                                   'distance=%f' % qdist])

        if cmdWhat.returncode != 0:
            return False

        for item in cmdWhat.ReadStdOutput():
            litem = item.lower()
            if "line:" in litem: # get line id
                self.line = int(item.split(':')[1].strip())
            elif "layer:" in litem: # add layer
                layer = int(item.split(':')[1].strip())
                if layer not in self.cats.keys():
                    self.cats[layer] = []
            elif "category:" in litem: # add category
                self.cats[layer].append(int(item.split(':')[1].strip()))

        return True

    def OnReload(self, event):
        """Reload button clicked"""
        # restore original list
        self.cats = copy.deepcopy(self.cats_orig)

        # polulate list
        self.itemDataMap = self.list.Populate(update=True)

        event.Skip()

    def OnApply(self, event):
        """Apply button clicked"""

        # action : (catsFrom, catsTo)
        check = {'catadd': (self.cats,      self.cats_orig),
                 'catdel': (self.cats_orig, self.cats)}

        # add/delete new category
        for action, cats in check.iteritems():
            for layer in cats[0].keys():
                catList = ""
                for cat in cats[0][layer]:
                    if layer not in cats[1].keys() or \
                            cat not in cats[1][layer]:
                        catList += "%s," % cat
                if catList != "":
                    catList = catList[:-1] # remove last comma
                    vEditCmd = ['v.edit', '--q',
                                'map=%s' % self.map,
                                'layer=%d' % layer,
                                'tool=%s' % action,
                                'cats=%s' % catList,
                                'id=%d' % self.line]
            
                    gcmd.Command(vEditCmd)
        
        self.cats_orig = copy.deepcopy(self.cats)

        event.Skip()

    def OnOK(self, event):
        """OK button clicked"""
        self.OnApply(event)
        self.Close()

    def OnAddCat(self, event):
        """Button 'Add' new category clicked"""
        try:
            layer = int(self.layerNew.GetValue())
            cat   = int(self.catNew.GetValue())
            if layer <= 0:
                raise ValueError
        except ValueError:
            dlg = wx.MessageDialog(self, _("Unable to add new layer/category <%s/%s>.\n"
                                           "Layer and category number must be integer.\n"
                                           "Layer number must be greater then zero.") %
                                   (str(self.layerNew.GetValue()), str(self.catNew.GetValue())),
                                   _("Error"), wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return False

        if layer not in self.cats.keys():
            self.cats[layer] = []

        self.cats[layer].append(cat)

        # reload list
        self.itemDataMap = self.list.Populate(update=True)

        # update category number for add
        self.catNew.SetValue(str(cat + 1))

        event.Skip()

        return True

    def GetLine(self):
        """Get id of selected line of 'None' if no line is selected"""
        return self.line

class CategoryListCtrl(wx.ListCtrl,
                       listmix.ListCtrlAutoWidthMixin,
                       listmix.TextEditMixin):
    """List of layers/categories"""

    def __init__(self, parent, id, pos=wx.DefaultPosition,
                 size=wx.DefaultSize, style=0):
        
        self.parent = parent
        
        wx.ListCtrl.__init__(self, parent, id, pos, size, style)

        listmix.ListCtrlAutoWidthMixin.__init__(self)
        listmix.TextEditMixin.__init__(self)

    def Populate(self, update=False):
        """Populate the list"""

        itemData = {} # requested by sorter

        if not update:
            self.InsertColumn(0, _("Layer"))
            self.InsertColumn(1, _("Category"))
        else:
            self.DeleteAllItems()

        i = 1
        for layer in self.parent.cats.keys():
            catsList = self.parent.cats[layer]
            for cat in catsList:
                index = self.InsertStringItem(sys.maxint, str(catsList[0]))
                self.SetStringItem(index, 0, str(layer))
                self.SetStringItem(index, 1, str(cat))
                self.SetItemData(index, i)
                itemData[i] = (str(layer), str(cat))
                i = i + 1

        if not update:
            self.SetColumnWidth(0, 100)
            self.SetColumnWidth(1, wx.LIST_AUTOSIZE)

        self.currentItem = 0

        return itemData
