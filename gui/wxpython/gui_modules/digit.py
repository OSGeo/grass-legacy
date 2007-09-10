"""
MODULE: digit

CLASSES:
 * AbstractDigit 
 * VEdit
 * VDigit
 * AbstractDisplayDriver
 * PyDisplayDriver
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

import wx
import wx.lib.colourselect as csel
import wx.lib.mixins.listctrl as listmix

import gcmd as cmd
import dbm
from debug import Debug as Debug

usePyDisplayDriver = False

if usePyDisplayDriver:
    try:
        # replace with the path to the GRASS SWIG-Python interface
        g6libPath = "/hardmnt/schiele0/ssi/landa/src/grass6/swig/python"
        # g6libPath = "/usr/src/gis/grass6/swig/python"
        sys.path.append(g6libPath)
        import python_grass6 as g6lib
    except:
        print >> sys.stderr, "For running digitization tool you need to enable GRASS SWIG-Python interface.\n" \
              "This only TEMPORARY solution (display driver based on SWIG-Python interface is EXTREMELY SLOW!\n" \
              "Will be replaced by C/C++ display driver."
else:
    try:
        driverPath = os.path.join( os.getenv("GISBASE"), "etc","wx", "display_driver")
        sys.path.append(driverPath)
        from grass6_wxdriver import DisplayDriver
    except:
        print >> sys.stderr, "Digitization tool is disabled.\n" \
              "Under development..."
    
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
            self.settings["symbolBackground"] = (None, (255,255,255, 255)) # white
            self.settings["symbolHighlight"] = (None, (255, 255, 0, 255)) #yellow
            self.settings["symbolPoint"] = (True, (0, 0, 0, 255)) # black
            self.settings["symbolLine"] = (True, (0, 0, 0, 255)) # black
            self.settings["symbolBoundaryNo"] = (True, (126, 126, 126, 255)) # grey
            self.settings["symbolBoundaryOne"] = (True, (255, 135, 0, 255)) # orange
            self.settings["symbolBoundaryTwo"] = (True, (0, 255, 0, 255)) # green
            self.settings["symbolCentroidIn"] = (True, (0, 0, 255, 255)) # blue
            self.settings["symbolCentroidOut"] = (True, (165, 42, 42, 255)) # brown
            self.settings["symbolCentroidDup"] = (True, (156, 62, 206, 255)) # violet
            self.settings["symbolNodeOne"] = (True, (255, 0, 0, 255)) # red
            self.settings["symbolNodeTwo"] = (True, (0, 86, 45, 255)) # dark green
            self.settings["symbolVertex"] = (False, (255, 20, 147, 255)) # deep pink
            
            # display
            self.settings["lineWidth"] = (2, "screen pixels")

            # snapping
            self.settings["snapping"] = (20, "screen pixels") # value, unit
            self.settings["snapToVertex"] = False

            # digitize new record
            self.settings["addRecord"] = True
            self.settings["layer"] = 1
            self.settings["category"] = 1
            self.settings["categoryMode"] = "Next to use"
        else:
            self.settings = settings

        if usePyDisplayDriver:
            self.driver = PyDisplayDriver(self, mapwindow)
        else:
            self.driver = CDisplayDriver(self, mapwindow)

        self.threshold = self.driver.GetThreshold()

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
                return False
        
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
                
        self._AddFeature (map=map, input=addstring, flags=['-s'])

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
            flags = ['-c', '-s'] # close boundaries
        else:
            key = "L"
            flags = ['-s']
            
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

        Debug.msg (4, "Vline.AddLine(): input=%s" % addstring)

        self._AddFeature (map=map, input=addstring, flags=flags)

    def _AddFeature (self, map, input, flags):
        """
        General method which adds feature to the vector map
        """
                                                     
        command = ["v.edit", "-n", "--q", 
                   "map=%s" % map,
                   "tool=add",
                   "thresh=%f" % self.threshold]

        # additional flags
        for flag in flags:
            command.append(flag)

        # run the command
        vedit = cmd.Command(cmd=command, stdin=input)

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

        command = [ "v.edit", "--q",
                    "map=%s" % self.map,
                    "tool=delete",
                    "ids=%s" % ids]

        # run the command
        vedit = cmd.Command(cmd=command)

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

        command = ["v.edit", "--q", "-s", # snap
                   "map=%s" % self.map,
                   "tool=%s" % tool,
                   "ids=%s" % ids,
                   "move=%f,%f" % (float(move[0]),float(move[1])),
                   "thresh=%f" % self.threshold]

        if tool == "vertexmove":
            command.append("coords=%f,%f" % (float(coords[0]), float(coords[1])))
                                             
        # run the command
        vedit = cmd.Command(cmd=command)

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
                   "thresh=%f" % self.threshold]

        # run the command
        vedit = cmd.Command(cmd=command)

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
                   "thresh=%f" % self.threshold]

        # run the command
        vedit = cmd.Command(cmd=command)

        # reload map (needed for v.edit)
        self.driver.ReloadMap()

        return True
    
class VDigit(AbstractDigit):
    """
    Prototype of digitization class based on v.digit reimplementation

    Under development (wxWidgets C/C++ background)
    """
    pass

class Digit(VEdit):
    """Default digit class"""
    def __init__(self, mapwindow):
        VEdit.__init__(self, mapwindow)

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
        """Reload map (close and re-open). Needed for v.edit."""
        
        self.__display.ReloadMap()

    def DrawMap(self):
        """Draw vector map layer content

        Return wx.Image 
        """
        import time
        start = time.clock()
        nlines = self.__display.DrawMap()
        stop = time.clock()
        Debug.msg(3, "CDisplayDriver.DrawMap(): nlines=%d, sec=%f" % \
                      (nlines, stop-start))

        return nlines
    def SelectLinesByBox(self, begin, end, onlyType=None):
        """Select vector features by given bounding box.

        Number of selected features can be decreased by 'onlyType'
        ('None' for no types)
        """

        x1, y1 = begin
        x2, y2 = end

        nselected = self.__display.SelectLinesByBox(x1, y1, x2, y2)
        Debug.msg(4, "CDisplayDriver.SelectLinesByBox(): selected=%d" % \
                      nselected)

        return nselected

    def SelectLinesByPoint(self, point, onlyType=None):
        """Select vector features by coordinates of click point.
        Number of selected features can be decreased by 'onlyType'
        ('None' for all types)"""

        nselected = self.__display.SelectLinesByPoint(point[0], point[1],
                                                    float(self.parent.threshold),
                                                    -1); 

        Debug.msg(4, "CDisplayDriver.SelectLinesByPoint(): selected=%d" % \
                      nselected)

        return nselected

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


class PyDisplayDriver(AbstractDisplayDriver):
    """
    Experimental display driver implemented in Python using
    GRASS-Python SWIG interface

    Note: This will be in the future rewritten in C/C++
    """
    def __init__(self, parent, mapwindow):
        AbstractDisplayDriver.__init__(self, parent, mapwindow)
        self.mapInfo     = None

    def __del__(self):
        if self.points:
            g6lib.Vect_destroy_line_struct(self.points)
        if self.cats:
            g6lib.Vect_destroy_cats_struct(self.cats)

    def Reset(self, map):
        g6lib.G_gisinit('')

        if map == None:
            if self.mapInfo:
                g6lib.Vect_close(self.mapInfo)

            self.mapInfo = None
            return
        
        name, mapset = map.split('@')

        Debug.msg(4, "DisplayDriver.__init__(): name=%s, mapset=%s" % \
                      (name, mapset))
        
        # define map structure
        self.mapInfo = g6lib.Map_info()
        # define open level (level 2: topology)
        g6lib.Vect_set_open_level(2)

        # open existing map
        # python 2.5 (unicode) -> str()
        g6lib.Vect_open_old(self.mapInfo, str(name), str(mapset))

        # auxilary structures
        self.points = g6lib.Vect_new_line_struct();
        self.cats = g6lib.Vect_new_cats_struct();

    def GetSelectedVertex(self, coords):
        """Return PseudoDC id(s) of vertex (of selected line)
        on position 'coords'"""

        selectedId = []

        try:
            line = self.selected[0]
        except:
            return selectedId

        idx = 0

        type = g6lib.Vect_read_line (self.mapInfo, self.points, self.cats, line)

        npoints = self.points.n_points
        
        for idx in range(npoints):
            x = g6lib.doubleArray_getitem(self.points.x, idx)
            y = g6lib.doubleArray_getitem(self.points.y, idx)
            z = g6lib.doubleArray_getitem(self.points.z, idx)

            dist = g6lib.Vect_points_distance(coords[0], coords[1], 0,
                                              x, y, z, 0)

            if idx == 0:
                minDist = dist
                minIdx  = idx
            else:
                if minDist > dist:
                    minDist = dist
                    minIdx = idx

        # [line, vertex1, ..., node1, node2]
        if minIdx == 0:
            selectedId.append(self.ids[line][-2])
        elif minIdx == npoints - 1:
            selectedId.append(self.ids[line][-1])
        else:
            selectedId.append(self.ids[line][minIdx])

        return selectedId
        
    def SelectLinesByBox(self, rect, onlyType=None):
        """Select vector features by given bounding box.

        rect = ((x1, y1), (x2, y2))
        Number of selected features can be decreased by 'onlyType'
        ('None' for no types)
        """

        type = 0
        if not onlyType:
            type = -1 # all types (see include/vect/dig_defines.h)
        elif onlyType == "line":
            type = g6lib.GV_LINES
        elif onlyType == "point":
            type = g6lib.GV_POINTS
                    
        list = g6lib.Vect_new_list()
        bbox = g6lib.Vect_new_line_struct()

        x1, y1 = rect[0]
        x2, y2 = rect[1]
        dx = abs(x2 - x1)
        dy = abs(y2 - y1)
        
        g6lib.Vect_append_point(bbox, x1, y1, 0)
        g6lib.Vect_append_point(bbox, x2, y1, 0)
        g6lib.Vect_append_point(bbox, x2, y2, 0)
        g6lib.Vect_append_point(bbox, x1, y2, 0)
        g6lib.Vect_append_point(bbox, x1, y1, 0)
        
        g6lib.Vect_select_lines_by_polygon(self.mapInfo, bbox,
                                           0, None,
                                           type, list)

        for idx in range(list.n_values):
            line = g6lib.intArray_getitem(list.value, idx)
            if line not in self.selected:
                self.selected.append(line)

        Debug.msg(4, "DisplayDriver.SelectLinesByBox(%s): n=%d, ids=%s" % \
                  (rect, list.n_values, self.selected))
        
        g6lib.Vect_destroy_line_struct(bbox)
        g6lib.Vect_destroy_list(list)

        return self.selected

    def SelectLinesByPoint(self, point, onlyType=""):
        """Select vector features by the box given by its center and thresh (size/2)
        Number of selected features can be decreased by 'onlyType'
        ('None' for all types)"""
                           
        type = 0
        if not onlyType:
            type = -1 # all types (see include/vect/dig_defines.h)
        elif onlyType == "line":
            type = g6lib.GV_LINES
        elif onlyType == "point":
            type = g6lib.GV_POINTS


        line = g6lib.Vect_find_line(self.mapInfo, point[0], point[1], 0,
                                    type, self.parent.threshold, 0, 0)

        self.selected = []
        if line > 0:
            self.selected.append(line)

        return self.selected

    def ReDrawMap(self, map):
        """Reopen map and draw its content in PseudoDC"""
        # close map
        if self.mapInfo:
            g6lib.Vect_close(self.mapInfo)
        
        # re-open map
        name, mapset = map.split('@')
        g6lib.Vect_open_old(self.mapInfo, str(name), str(mapset))

        self.DrawMap()
        
    def DrawMap(self):
        """Display content of the map in PseudoDC"""
        if not self.mapInfo:
            return
        nlines = g6lib.Vect_get_num_lines(self.mapInfo)
        Debug.msg(4, "PyDisplayDriver.DrawMap(): nlines=%d" % nlines)

        for line in range(1, nlines + 1):
            self.DisplayLine(line)

        return nlines
    
    def DisplayLine(self, line):
        """Display line (defined by id) in PseudoDC"""
        Debug.msg(4, "DisplayDriver.DisplayLine(): line=%d" % line)
        
        if not g6lib.Vect_line_alive (self.mapInfo, line):
            return
        
        type = g6lib.Vect_read_line (self.mapInfo, self.points, self.cats, line)

        Debug.msg (4, "DisplayDriver.DisplayLine(): line=%d type=%d" % \
                   (line, type))

        # add id
        self.ids[line] = []
        self.DisplayPoints(line, type, self.points)

    def DisplayNodes(self, line):
        """Display nodes of the given line"""
        Debug.msg(4, "DisplayDriver.DisplayNodes(): line=%d" % line)

        n1 = g6lib.new_intp()
        n2 = g6lib.new_intp()
        x = g6lib.new_doublep()
        y = g6lib.new_doublep()
        z = g6lib.new_doublep()

        nodes = [n1, n2]
        g6lib.Vect_get_line_nodes(self.mapInfo, line, n1, n2)
        
        for nodep in nodes:
            node = g6lib.intp_value(nodep)
            g6lib.Vect_get_node_coor(self.mapInfo, node,
                                     x, y, z)
            coords = (self.mapwindow.Cell2Pixel(g6lib.doublep_value(x),
                                                g6lib.doublep_value(y)))

            if g6lib.Vect_get_node_n_lines(self.mapInfo, node) == 1:
                self.SetPen(line, "symbolNodeOne") # one line
            else:
                self.SetPen(line, "symbolNodeTwo") # two lines

            self.ids[line].append(self.mapwindow.DrawCross(coords, size=5))

        g6lib.delete_doublep(x)
        g6lib.delete_doublep(y)
        g6lib.delete_doublep(z)
        g6lib.delete_intp(n1)
        g6lib.delete_intp(n2)

    def DisplayPoints(self, line, type, points):
        """Draw points in PseudoDC"""

        coords = []
        npoints = points.n_points
        Debug.msg(4, "DisplayDriver.DisplayPoints() npoints=%d" % npoints);

        for idx in range(npoints):
            x = g6lib.doubleArray_getitem(self.points.x, idx)
            y = g6lib.doubleArray_getitem(self.points.y, idx)
            z = g6lib.doubleArray_getitem(self.points.z, idx)
            coords.append(self.mapwindow.Cell2Pixel(x, y))

        if len(coords) > 1: # -> line
            # draw line
            if type == g6lib.GV_BOUNDARY:
                leftp = g6lib.new_intp()
                rightp = g6lib.new_intp()
                g6lib.Vect_get_line_areas(self.mapInfo, line,
                                          leftp, rightp)
                left, right = g6lib.intp_value(leftp), g6lib.intp_value(rightp)
                if left == 0 and right == 0:
                    self.SetPen(line, "symbolBoundaryNo")
                elif left > 0 and right > 0:
                    self.SetPen(line, "symbolBoundaryTwo")
                else:
                    self.SetPen(line, "symbolBoundaryOne")
                g6lib.delete_intp(leftp)
                g6lib.delete_intp(rightp)
            else: # GV_LINE
                self.SetPen(line, "symbolLine")
            self.ids[line].append(self.mapwindow.DrawLines(coords))
            # draw verteces
            self.SetPen(line, "symbolVertex")
            for idx in range(1, npoints-1):
                self.ids[line].append(self.mapwindow.DrawCross(coords[idx], size=4))
            # draw nodes
            self.DisplayNodes(line)
        else:
            if type == g6lib.GV_CENTROID:
                cret = g6lib.Vect_get_centroid_area(self.mapInfo, line)
                if cret > 0: # -> area
                    self.SetPen(line, "symbolCentroidIn")
                elif cret == 0:
                    self.SetPen(line, "symbolCentroidOut")
                else:
                    self.SetPen(line, "symbolCentroidDup")
            else:
                self.SetPen(line, "symbolPoint")
            self.ids[line].append(self.mapwindow.DrawCross(coords[0], size=5))

    def SetPen(self, line, symbol):
        """Set Pen for PseudoDC according vector feature status"""
        
        if line in self.selected:
            symbol = "symbolHighlight"
        
        width = self.parent.settings["lineWidth"][0]
        
        if self.parent.settings[symbol][0] in [True, None]:
            self.mapwindow.pen = self.mapwindow.polypen = wx.Pen(colour=self.parent.settings[symbol][1],
                                                                 width=width, style=wx.SOLID)
        else:
            self.mapwindow.pen = self.mapwindow.polypen = None

    def SetSelected(self, pdcId):
        """Set selected objects (their ids) in PseudoDC

        For deseleids=[]
        """
        # reset
        self.selected = []
        
        for line in pdcId:
            self.selected.append(line)

        Debug.msg(4, "DisplayDriver.SetSelected(): pdcId=%s, grassId=%s" % \
                      (pdcId, self.selected))

    def GetSelected(self, grassId=True):
        """Get ids of selected objects in PseudoDC

        If grassId=True return GRASS line id otherwise PseudoDC id"""
        if grassId:
            return self.selected
        else:
            pdcId = []
            for line in self.selected:
                for id in self.ids[line]:
                    pdcId.append(id)
            return pdcId

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
                                          value=str(self.parent.digit.settings["lineWidth"][0]),
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
        flexSizer = wx.FlexGridSizer (cols=3, hgap=5, vgap=5)
        flexSizer.AddGrowableCol(0)
        # snapping
        text = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Snapping threshold"))
        self.snappingValue = wx.SpinCtrl(parent=panel, id=wx.ID_ANY, size=(50, -1),
                                         value=str(self.parent.digit.settings["snapping"][0]), min=1, max=1e6)
        self.snappingValue.Bind(wx.EVT_SPINCTRL, self.OnChangeSnappingValue)
        self.snappingUnit = wx.ComboBox(parent=panel, id=wx.ID_ANY, size=(125, -1),
                                         choices=["screen pixels", "map units"])
        self.snappingUnit.SetValue(self.parent.digit.settings["snapping"][1])
        self.snappingUnit.Bind(wx.EVT_COMBOBOX, self.OnChangeSnappingUnits)
        flexSizer.Add(text, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(self.snappingValue, proportion=0, flag=wx.ALIGN_CENTER | wx.FIXED_MINSIZE)
        flexSizer.Add(self.snappingUnit, proportion=0, flag=wx.ALIGN_RIGHT | wx.FIXED_MINSIZE)
        vertexSizer = wx.BoxSizer(wx.VERTICAL)
        self.snapVertex = wx.CheckBox(parent=panel, id=wx.ID_ANY,
                                      label=_("Snap also to vertex"))
        self.snapVertex.SetValue(self.parent.digit.settings["snapToVertex"])
        vertexSizer.Add(item=self.snapVertex, proportion=0, flag=wx.ALL | wx.EXPAND, border=1)
        self.mapUnits = self.parent.MapWindow.Map.ProjInfo()['units']
        self.snappingInfo = wx.StaticText(parent=panel, id=wx.ID_ANY,
                                          label=_("Snapping threshold is %.1f %s") % \
                                              (self.parent.digit.threshold,
                                               self.mapUnits))
        vertexSizer.Add(item=self.snappingInfo, proportion=0, flag=wx.ALL | wx.EXPAND, border=1)

        sizer.Add(item=flexSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=1)
        sizer.Add(item=vertexSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=1)
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
        self.snappingInfo.SetLabel(_("Snapping threshold is %.1f %s") % \
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
        if not usePyDisplayDriver:
            self.parent.digit.driver.UpdateSettings()

        # redraw map if auto-rendering is enabled
        if self.parent.autoRender.GetValue(): 
            self.parent.ReRender(None)

class DigitCategoryDialog(wx.Dialog):
    """
    Dialog used to display/modify categories of vector objects
    """
    def __init__(self, parent, title,
                 map, queryCoords, qdist,
                 pos=wx.DefaultPosition,
                 style=wx.DEFAULT_DIALOG_STYLE):
        # map name
        self.map = map

        # line id (if not found remains 'None')
        self.line = None

        # {layer: [categories]}
        self.cats = {}

        # do not display dialog if no line is found
        if self.__GetCategories(queryCoords, qdist) == 0 or not self.line:
            Debug.msg(3, "DigitCategoryDialog(): nothing found!")
            return

        Debug.msg(3, "DigitCategoryDialog(): line=%d, cats=%s" % \
                      (self.line, self.cats))

        wx.Dialog.__init__(self, parent=parent, id=wx.ID_ANY, title=title, style=style)

        self.parent = parent # mapdisplay.BufferedWindow class instance

        # list
        listSizer = wx.BoxSizer(wx.VERTICAL)
        self.list = CategoryListCtrl(parent=self, id=wx.ID_ANY,
                                     style=wx.LC_REPORT |
                                     wx.BORDER_NONE |
                                     wx.LC_SORT_ASCENDING)
        listSizer.Add(item=self.list, proportion=1, flag=wx.EXPAND)

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
        mainSizer.Add(item=listSizer, proportion=0,
                      flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)
        mainSizer.Add(item=btnSizer, proportion=0,
                      flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)

        self.SetSizer(mainSizer)
        mainSizer.Fit(self)

    def __GetCategories(self, coords, qdist):
        """Get layer/category pairs for all available
        layers

        Return True line found or False if not found"""
        
        cmdWhat = cmd.Command(cmd=['v.what',
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

    def OnApply(self, event):
        """Apply button clicked"""
        self.Close()

    def OnOK(self, event):
        """OK button clicked"""
        self.Close()

    def GetLine(self):
        """Get id of selected line of 'None' if no line is selected"""
        return self.line

class CategoryListCtrl(wx.ListCtrl,
                       listmix.ListCtrlAutoWidthMixin,
                       listmix.TextEditMixin):
    """List of layers/categories"""

    def __init__(self, parent, id, pos=wx.DefaultPosition,
                 size=wx.DefaultSize, style=0):

        wx.ListCtrl.__init__(self, parent, id, pos, size, style)

        listmix.ListCtrlAutoWidthMixin.__init__(self)
        self.Populate()
        listmix.TextEditMixin.__init__(self)

    def Populate(self):
        """Populate the list"""
        self.InsertColumn(0, _("Layer"))
        self.InsertColumn(1, _("Category"))

        self.SetColumnWidth(0, 100)
        self.SetColumnWidth(1, wx.LIST_AUTOSIZE)

        self.currentItem = 0
