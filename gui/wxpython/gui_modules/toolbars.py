"""
MODULE: toolbar

CLASSES:
    * AbstractToolbar
    * MapToolbar
    * DigitToolbar

PURPOSE: Toolbars for Map Display window

AUTHORS: The GRASS Development Team
         Michael Barton, Martin Landa, Jachym Cepicky

COPYRIGHT: (C) 2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import wx
import os, sys

gmpath = os.path.join( os.getenv("GISBASE"),"etc","wx","icons")
sys.path.append(gmpath)

import gcmd as cmd
import grassenv
from digit import Digit as Digit
from digit import DigitSettingsDialog as DigitSettingsDialog
from debug import Debug as Debug
from icon import Icons as Icons

class AbstractToolbar:
    """Abstract toolbar class"""
    def __init__():
        pass

    def InitToolbar(self, parent, toolbar, toolData):
        """Initialize toolbar, i.e. add tools to the toolbar"""

        for tool in toolData:
            self.CreateTool(parent, toolbar, *tool)

    def ToolbarData(self):
        """Toolbar data"""

        return None

    def CreateTool(self, parent, toolbar, tool, label, bitmap, kind,
                   shortHelp, longHelp, handler):
        """Add tool to the toolbar"""

        bmpDisabled=wx.NullBitmap

        if label:
            tool = toolbar.AddLabelTool(wx.ID_ANY, label, bitmap,
                                        bmpDisabled, kind,
                                        shortHelp, longHelp)
            parent.Bind(wx.EVT_TOOL, handler, tool)
        else: # add separator
            toolbar.AddSeparator()

class MapToolbar(AbstractToolbar):
    """
    Main Map Display toolbar
    """

    def __init__(self, mapdisplay, map):
        self.mapcontent = map
        self.mapdisplay = mapdisplay

        self.toolbar = wx.ToolBar(parent=self.mapdisplay, id=wx.ID_ANY)

        # self.SetToolBar(self.toolbar)
        tsize = (24, 24)
        self.toolbar.SetToolBitmapSize(tsize)

        self.InitToolbar(self.mapdisplay, self.toolbar, self.ToolbarData())

        # optional tools
        self.combo = wx.ComboBox(parent=self.toolbar, id=wx.ID_ANY, value='Tools',
                                 choices=['Digitize'], style=wx.CB_READONLY, size=(90, -1))

        self.comboid = self.toolbar.AddControl(self.combo)
        self.mapdisplay.Bind(wx.EVT_COMBOBOX, self.OnSelect, self.comboid)

        # realize the toolbar
        self.toolbar.Realize()

    def ToolbarData(self):
        """Toolbar data"""

        self.displaymap = self.rendermap = self.erase = \
        self.pointer = self.query = self.pan = self.zoomin = self.zoomout = \
        self.zoomback = self.zoommenu = self.analyze = self.dec = self.savefile = self.printmap =None

        # tool, label, bitmap, kind, shortHelp, longHelp, handler
        return (
            (self.displaymap, "displaymap", Icons["displaymap"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["displaymap"].GetLabel(), Icons["displaymap"].GetDesc(),
             self.mapdisplay.ReDraw),
            (self.rendermap, "rendermap", Icons["rendermap"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["rendermap"].GetLabel(), Icons["rendermap"].GetDesc(),
             self.mapdisplay.ReRender),
            (self.erase, "erase", Icons["erase"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["erase"].GetLabel(), Icons["erase"].GetDesc(),
             self.mapdisplay.OnErase),
            ("", "", "", "", "", "", ""),
            (self.pointer, "pointer", Icons["pointer"].GetBitmap(),
             wx.ITEM_RADIO, Icons["pointer"].GetLabel(), Icons["pointer"].GetDesc(),
             self.mapdisplay.OnPointer),
            (self.query, "query", Icons["query"].GetBitmap(),
             wx.ITEM_RADIO, Icons["query"].GetLabel(), Icons["query"].GetDesc(),
             self.mapdisplay.OnQuery),
            (self.pan, "pan", Icons["pan"].GetBitmap(),
             wx.ITEM_RADIO, Icons["pan"].GetLabel(), Icons["pan"].GetDesc(),
             self.mapdisplay.OnPan),
            (self.zoomin, "zoom_in", Icons["zoom_in"].GetBitmap(),
             wx.ITEM_RADIO, Icons["zoom_in"].GetLabel(), Icons["zoom_in"].GetDesc(),
             self.mapdisplay.OnZoomIn),
            (self.zoomout, "zoom_out", Icons["zoom_out"].GetBitmap(),
             wx.ITEM_RADIO, Icons["zoom_out"].GetLabel(), Icons["zoom_out"].GetDesc(),
             self.mapdisplay.OnZoomOut),
            (self.zoomback, "zoom_back", Icons["zoom_back"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["zoom_back"].GetLabel(), Icons["zoom_back"].GetDesc(),
             self.mapdisplay.OnZoomBack),
            (self.zoommenu, "zoommenu", Icons["zoommenu"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["zoommenu"].GetLabel(), Icons["zoommenu"].GetDesc(),
             self.mapdisplay.OnZoomMenu),
            ("", "", "", "", "", "", ""),
            (self.analyze, "analyze", Icons["analyze"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["analyze"].GetLabel(), Icons["analyze"].GetDesc(),
             self.mapdisplay.OnAnalyze),
            ("", "", "", "", "", "", ""),
            (self.dec, "overlay", Icons["overlay"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["overlay"].GetLabel(), Icons["overlay"].GetDesc(),
             self.mapdisplay.OnDecoration),
            ("", "", "", "", "", "", ""),
            (self.savefile, "savefile", Icons["savefile"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["savefile"].GetLabel(), Icons["savefile"].GetDesc(),
             self.mapdisplay.SaveToFile),
            (self.printmap, "printmap", Icons["printmap"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["printmap"].GetLabel(), Icons["printmap"].GetDesc(),
             self.mapdisplay.PrintMenu),
            ("", "", "", "", "", "", "")
            )

    def OnSelect(self, event):
        """
        Select / enable tool available in tools list
        """
        tool =  event.GetString()

        if tool == "Digitize" and not self.mapdisplay.digittoolbar:
            self.mapdisplay.AddToolbar("digit")

class GRToolbar(AbstractToolbar):
    """
    Georectify Display toolbar
    """

    def __init__(self, mapdisplay, map):
        self.mapcontent = map
        self.mapdisplay = mapdisplay

        self.toolbar = wx.ToolBar(parent=self.mapdisplay, id=wx.ID_ANY)

        # self.SetToolBar(self.toolbar)
        tsize = (24, 24)
        self.toolbar.SetToolBitmapSize(tsize)

        self.InitToolbar(self.mapdisplay, self.toolbar, self.ToolbarData())

        # realize the toolbar
        self.toolbar.Realize()

    def ToolbarData(self):
        """Toolbar data"""

        self.displaymap = self.rendermap = self.erase = \
        self.gcpset = self.query = self.pan = self.zoomin = self.zoomout = \
        self.zoomback = self.zoommenu = self.analyze = self.dec = self.savefile = self.printmap =None

        # tool, label, bitmap, kind, shortHelp, longHelp, handler
        return (
            (self.displaymap, "displaymap", Icons["displaymap"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["displaymap"].GetLabel(), Icons["displaymap"].GetDesc(),
             self.mapdisplay.ReDraw),
            (self.rendermap, "rendermap", Icons["rendermap"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["rendermap"].GetLabel(), Icons["rendermap"].GetDesc(),
             self.mapdisplay.ReRender),
            (self.erase, "erase", Icons["erase"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["erase"].GetLabel(), Icons["erase"].GetDesc(),
             self.mapdisplay.OnErase),
            ("", "", "", "", "", "", ""),
            (self.gcpset, "gcpset", Icons["gcpset"].GetBitmap(),
             wx.ITEM_RADIO, Icons["gcpset"].GetLabel(), Icons["gcpset"].GetDesc(),
             self.mapdisplay.OnPointer),
            (self.pan, "pan", Icons["pan"].GetBitmap(),
             wx.ITEM_RADIO, Icons["pan"].GetLabel(), Icons["pan"].GetDesc(),
             self.mapdisplay.OnPan),
            (self.zoomin, "zoom_in", Icons["zoom_in"].GetBitmap(),
             wx.ITEM_RADIO, Icons["zoom_in"].GetLabel(), Icons["zoom_in"].GetDesc(),
             self.mapdisplay.OnZoomIn),
            (self.zoomout, "zoom_out", Icons["zoom_out"].GetBitmap(),
             wx.ITEM_RADIO, Icons["zoom_out"].GetLabel(), Icons["zoom_out"].GetDesc(),
             self.mapdisplay.OnZoomOut),
            (self.zoomback, "zoom_back", Icons["zoom_back"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["zoom_back"].GetLabel(), Icons["zoom_back"].GetDesc(),
             self.mapdisplay.OnZoomBack),
            (self.zoommenu, "zoommenu", Icons["zoommenu"].GetBitmap(),
             wx.ITEM_NORMAL, Icons["zoommenu"].GetLabel(), Icons["zoommenu"].GetDesc(),
             self.mapdisplay.OnZoomMenu),
            ("", "", "", "", "", "", "")
            )

    def OnSelect(self, event):
        """
        Select / enable tool available in tools list
        """
        tool =  event.GetString()

        if tool == "Digitize" and not self.mapdisplay.digittoolbar:
            self.mapdisplay.AddToolbar("digit")

class DigitToolbar(AbstractToolbar):
    """
    Toolbar for digitization
    """

    def __init__(self, parent, map):
        self.mapcontent    = map    # Map class instance
        self.parent        = parent # MapFrame

        # selected map to digitize
        self.layerSelectedID  = None
        self.layers     = []

        # default action (digitize new point, line, etc.)
        self.action     = "addLine"
        self.type       = "point"
        self.addString  = ""

        self.comboid    = None

        # only one dialog settings can be one
        self.settingsDialog = None

        # create toolbars (two rows)
        self.toolbar = []
        numOfRows = 2
        for row in range(0, numOfRows):
            self.toolbar.append(wx.ToolBar(parent=self.parent, id=wx.ID_ANY))
            self.toolbar[row].SetToolBitmapSize(wx.Size(24,24))

            # create toolbar
            self.InitToolbar(self.parent, self.toolbar[row], self.ToolbarData(row))

        # list of available vector maps
        self.UpdateListOfLayers(updateTool=True)

        # realize toolbar
        for row in range(0, numOfRows):
            self.toolbar[row].Realize()

    def ToolbarData(self, row):
        """
        Toolbar data
        """

        if row == 0:
            self.displayCats = self.displayAttr = self.copyCats = None
            self.settings = self.exit = None

            return (("", "", "", "", "", "", ""),
                    (self.settings, "digSettings", Icons["digSettings"].GetBitmap(),
                     wx.ITEM_NORMAL, Icons["digSettings"].GetLabel(), Icons["digSettings"].GetDesc(),
                     self.OnSettings),
                    (self.exit, "digExit", Icons["digExit"].GetBitmap(),
                     wx.ITEM_NORMAL, Icons["digExit"].GetLabel(), Icons["digExit"].GetDesc(),
                     self.OnExit))
        else:
            self.addPoint = self.addLine = self.addBoundary = self.addCentroid = None
            self.moveVertex = self.addVertex = self.removeVertex = None
            self.splitLine = self.editLine = self.moveLine = self.deleteLine = None
            self.additioanlTools = None

            return ((self.addPoint, "digAddPoint", Icons["digAddPoint"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digAddPoint"].GetLabel(), Icons["digAddPoint"].GetDesc(),
                     self.OnAddPoint),
                    (self.addLine, "digAddLine", Icons["digAddLine"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digAddLine"].GetLabel(), Icons["digAddLine"].GetDesc(),
                     self.OnAddLine),
                    (self.addBoundary, "digAddBoundary", Icons["digAddBoundary"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digAddBoundary"].GetLabel(), Icons["digAddBoundary"].GetDesc(),
                     self.OnAddBoundary),
                    (self.addCentroid, "digAddCentroid", Icons["digAddCentroid"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digAddCentroid"].GetLabel(), Icons["digAddCentroid"].GetDesc(),
                     self.OnAddCentroid),
                    (self.moveVertex, "digMoveVertex", Icons["digMoveVertex"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digMoveVertex"].GetLabel(), Icons["digMoveVertex"].GetDesc(),
                     self.OnMoveVertex),
                    (self.addVertex, "digAddVertex", Icons["digAddVertex"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digAddVertex"].GetLabel(), Icons["digAddVertex"].GetDesc(),
                     self.OnAddVertex),
                    (self.removeVertex, "digRemoveVertex", Icons["digRemoveVertex"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digRemoveVertex"].GetLabel(), Icons["digRemoveVertex"].GetDesc(),
                     self.OnRemoveVertex),
                    (self.splitLine, "digSplitLine", Icons["digSplitLine"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digSplitLine"].GetLabel(), Icons["digSplitLine"].GetDesc(),
                     self.OnSplitLine),
                    (self.editLine, "digEditLine", Icons["digEditLine"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digEditLine"].GetLabel(), Icons["digEditLine"].GetDesc(),
                     self.OnEditLine),
                    (self.moveLine, "digMoveLine", Icons["digMoveLine"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digMoveLine"].GetLabel(), Icons["digMoveLine"].GetDesc(),
                     self.OnMoveLine),
                    (self.deleteLine, "digDeleteLine", Icons["digDeleteLine"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digDeleteLine"].GetLabel(), Icons["digDeleteLine"].GetDesc(),
                     self.OnDeleteLine),
                    (self.displayCats, "digDispCats", Icons["digDispCats"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digDispCats"].GetLabel(), Icons["digDispCats"].GetDesc(),
                     self.OnDisplayCats),
                    (self.copyCats, "digCopyCats", Icons["digCopyCats"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digCopyCats"].GetLabel(), Icons["digCopyCats"].GetDesc(),
                     self.OnCopyCats),
                    (self.displayAttr, "digDispAttr", Icons["digDispAttr"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digDispAttr"].GetLabel(), Icons["digDispAttr"].GetDesc(),
                     self.OnDisplayAttr),
                    (self.additioanlTools, "digAdditionalTools", Icons["digAdditionalTools"].GetBitmap(),
                     wx.ITEM_RADIO, Icons["digAdditionalTools"].GetLabel(),
                     Icons["digAdditionalTools"].GetDesc(),
                     self.OnAdditionalToolMenu))

    def OnAddPoint(self, event):
        """Add point to the vector map Laier"""
        Debug.msg (2, "DigitToolbar.OnAddPoint()")
        self.action = "addLine"
        self.type   = "point"
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnAddLine(self, event):
        """Add line to the vector map layer"""
        Debug.msg (2, "DigitToolbar.OnAddLine()")
        self.action = "addLine"
        self.type   = "line"
        self.parent.MapWindow.mouse['box'] = 'line'
        self.parent.MapWindow.polycoords = [] # reset temp line

    def OnAddBoundary(self, event):
        """Add boundary to the vector map layer"""
        Debug.msg (2, "DigitToolbar.OnAddBoundary()")
        self.action = "addLine"
        self.type   = "boundary"
        self.parent.MapWindow.mouse['box'] = 'line'
        self.parent.MapWindow.polycoords = [] # reset temp line

    def OnAddCentroid(self, event):
        """Add centroid to the vector map layer"""
        Debug.msg (2, "DigitToolbar.OnAddCentroid()")
        self.action = "addLine"
        self.type   = "centroid"
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnExit (self, event):
        """Quit digitization tool"""
        # stop editing of the currently selected map layer
        try:
            self.StopEditing(self.layers[self.layerSelectedID])
        except:
            pass

        # close settings dialog if still open
        if self.settingsDialog:
            self.settingsDialog.OnCancel(None)

        # disable the toolbar
        self.parent.RemoveToolbar ("digit")

    def OnMoveVertex(self, event):
        """Move line vertex"""
        Debug.msg(2, "Digittoolbar.OnMoveVertex():")
        self.action = "moveVertex"
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnAddVertex(self, event):
        """Add line vertex"""
        Debug.msg(2, "Digittoolbar.OnAddVertex():")
        self.action = "addVertex"
        self.parent.MapWindow.mouse['box'] = 'point'


    def OnRemoveVertex(self, event):
        """Remove line vertex"""
        Debug.msg(2, "Digittoolbar.OnRemoveVertex():")
        self.action = "removeVertex"
        self.parent.MapWindow.mouse['box'] = 'point'


    def OnSplitLine(self, event):
        """Split line"""
        Debug.msg(2, "Digittoolbar.OnSplitLine():")
        self.action = "splitLine"
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnEditLine(self, event):
        """Edit line"""
        Debug.msg(2, "Digittoolbar.OnEditLine():")
        self.action="editLine"
        self.parent.MapWindow.mouse['box'] = 'line'

    def OnMoveLine(self, event):
        """Move line"""
        Debug.msg(2, "Digittoolbar.OnMoveLine():")
        self.action = "moveLine"
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnDeleteLine(self, event):
        """Delete line"""
        Debug.msg(2, "Digittoolbar.OnDeleteLine():")
        self.action = "deleteLine"
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnDisplayCats(self, event):
        """Display/update categories"""
        Debug.msg(2, "Digittoolbar.OnDisplayCats():")
        self.action="displayCats"
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnDisplayAttr(self, event):
        """Display/update attributes"""
        Debug.msg(2, "Digittoolbar.OnDisplayAttr():")
        self.action="displayAttrs"
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnCopyCats(self, event):
        """Copy categories"""
        Debug.msg(2, "Digittoolbar.OnCopyCats():")
        self.action="copyCats"
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnSettings(self, event):
        """Show settings dialog"""

        if not self.settingsDialog:
            self.settingsDialog = DigitSettingsDialog(parent=self.parent, title=_("Digitization settings"),
                                                      style=wx.DEFAULT_DIALOG_STYLE)
            self.settingsDialog.Show()

    def OnAdditionalToolMenu(self, event):
        """Menu for additional tools"""
        point = wx.GetMousePosition()
        toolMenu = wx.Menu()
        # Add items to the menu
        copy = wx.MenuItem(toolMenu, wx.ID_ANY, 'Copy features from (background) vector map')
        toolMenu.AppendItem(copy)
        self.parent.MapWindow.Bind(wx.EVT_MENU, self.OnCopy, copy)

        flip = wx.MenuItem(toolMenu, wx.ID_ANY, 'Flip selected lines')
        toolMenu.AppendItem(flip)
        self.parent.MapWindow.Bind(wx.EVT_MENU, self.OnFlip, flip)

        merge = wx.MenuItem(toolMenu, wx.ID_ANY, 'Merge selected lines')
        toolMenu.AppendItem(merge)
        self.parent.MapWindow.Bind(wx.EVT_MENU, self.OnMerge, merge)

        breakL = wx.MenuItem(toolMenu, wx.ID_ANY, 'Break selected lines at intersection')
        toolMenu.AppendItem(breakL)
        self.parent.MapWindow.Bind(wx.EVT_MENU, self.OnBreak, breakL)

        snap = wx.MenuItem(toolMenu, wx.ID_ANY, 'Snap selected lines (only to nodes)')
        toolMenu.AppendItem(snap)
        self.parent.MapWindow.Bind(wx.EVT_MENU, self.OnSnap, snap)

        connect = wx.MenuItem(toolMenu, wx.ID_ANY, 'Connect two selected lines')
        toolMenu.AppendItem(connect)
        self.parent.MapWindow.Bind(wx.EVT_MENU, self.OnConnect, connect)

        query = wx.MenuItem(toolMenu, wx.ID_ANY, 'Query tool')
        toolMenu.AppendItem(query)
        self.parent.MapWindow.Bind(wx.EVT_MENU, self.OnQuery, query)

        zbulk = wx.MenuItem(toolMenu, wx.ID_ANY, 'Z bulk-labeling of 3D lines')
        toolMenu.AppendItem(zbulk)
        self.parent.MapWindow.Bind(wx.EVT_MENU, self.OnZBulk, zbulk)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.parent.MapWindow.PopupMenu(toolMenu)
        toolMenu.Destroy()

    def OnCopy(self, event):
        """Copy selected features from (background) vector map"""
        Debug.msg(2, "Digittoolbar.OnCopy():")
        self.action="copyLine"
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnFlip(self, event):
        """Flip selected lines/boundaries"""
        Debug.msg(2, "Digittoolbar.OnFlip():")
        self.action="flipLine"
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnMerge(self, event):
        """Merge selected lines/boundaries"""
        Debug.msg(2, "Digittoolbar.OnMerge():")
        self.action="mergeLine"
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnBreak(self, event):
        """Break selected lines/boundaries"""
        Debug.msg(2, "Digittoolbar.OnBreak():")
        self.action="breakLine"
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnSnap(self, event):
        """Snap selected features"""
        Debug.msg(2, "Digittoolbar.OnSnap():")
        self.action="snapLine"
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnConnect(self, event):
        """Connect selected lines/boundaries"""
        Debug.msg(2, "Digittoolbar.OnConnect():")
        self.action="connectLine"
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnQuery(self, event):
        """Query selected lines/boundaries"""
        Debug.msg(2, "Digittoolbar.OnQuery(): %s" % self.parent.digit.settings["query"][0])
        self.action="queryLine"
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnZBulk(self, event):
        """Z bulk-labeling selected lines/boundaries"""
        Debug.msg(2, "Digittoolbar.OnZBulk():")
        self.action="zbulkLine"
        self.parent.MapWindow.mouse['box'] = 'line'

    def OnSelectMap (self, event):
        """
        Select vector map layer for editing

        If there is a vector map layer already edited, this action is
        firstly terminated. The map layer is closed. After this the
        selected map layer activated for editing.
        """

        if self.layerSelectedID == event.GetSelection():
            return False

        if self.layerSelectedID != None: # deactive map layer for editing
            self.StopEditing(self.layers[self.layerSelectedID])

        # select the given map layer for editing
        self.layerSelectedID = event.GetSelection()
        self.StartEditing(self.layers[self.layerSelectedID])

        event.Skip()

        return True
    def StartEditing (self, layerSelected):
        """
        Start editing of selected vector map layer.

        Return True on success or False if layer cannot be edited
        """
        try:
            self.layerSelectedID = self.layers.index(layerSelected)
            mapLayer = self.layers[self.layerSelectedID]
        except:
            return False

        # update toolbar
        self.combo.SetValue (layerSelected.name)
        self.parent.maptoolbar.combo.SetValue ('Digitize')
        # set initial category number for new features (layer=1), etc.

        Debug.msg (4, "DigitToolbar.StartEditing(): layerSelectedID=%d layer=%s" % \
                   (self.layerSelectedID, mapLayer.name))

        self.parent.digit.SetMapName(mapLayer.name)

        # deactive layer
        self.mapcontent.ChangeLayerActive(mapLayer, False)

        # change cursor
        if self.parent.MapWindow.mouse['use'] == 'pointer':
            self.parent.MapWindow.SetCursor(self.parent.cursors["cross"])

        # create pseudoDC for drawing the map
        self.parent.MapWindow.pdcVector = wx.PseudoDC()
        self.parent.digit.driver.SetDevice(self.parent.MapWindow.pdcVector)
        # self.parent.MapWindow.UpdateMap()
        if not self.parent.MapWindow.resize:
            self.parent.MapWindow.UpdateMap(render=True)


        return True

    def StopEditing (self, layerSelected):
        """
        Stop editing of selected vector map layer.
        """

        if self.layers[self.layerSelectedID] == layerSelected:
            self.layerSelectedID = None
            Debug.msg (4, "DigitToolbar.StopEditing(): layer=%s" % \
                       (layerSelected.name))
            self.combo.SetValue ('Select vector map')

            # re-active layer 
            item = self.parent.tree.FindItemByData('maplayer', layerSelected)
            if item and self.parent.tree.IsItemChecked(item):
                self.mapcontent.ChangeLayerActive(layerSelected, True)

            self.parent.digit.SetMapName(None)

            # change cursor
            self.parent.MapWindow.SetCursor(self.parent.cursors["default"])

            # disable pseudodc for vector map layer
            self.parent.MapWindow.pdcVector = None
            self.parent.digit.driver.SetDevice(None)

            return True

        return False

    def UpdateListOfLayers (self, updateTool=False):
        """
        Update list of available vector map layers.
        This list consists only editable layers (in the current mapset)

        Optionaly also update toolbar
        """

        Debug.msg (4, "DigitToolbar.UpdateListOfLayers(): updateTool=%d" % \
                   updateTool)

        layerNameSelected = None
        if self.layerSelectedID != None: # name of currently selected layer
            layerNameSelected = self.layers[self.layerSelectedID].name

        # select vector map layer in the current mapset
        layerNameList = []
        self.layers = self.mapcontent.GetListOfLayers(l_type="vector",
                                                      l_mapset=grassenv.GetGRASSVariable('MAPSET'))
        for layer in self.layers:
            if not layer.name in layerNameList: # do not duplicate layer
                layerNameList.append (layer.name)

        if updateTool: # update toolbar
            if self.layerSelectedID == None:
                value = 'Select vector map'
            else:
                value = layerNameSelected

            # ugly ...
            if self.comboid:
                self.toolbar[0].DeleteToolByPos(0)

            self.combo = wx.ComboBox(self.toolbar[0], id=wx.ID_ANY, value=value,
                                     choices=layerNameList, size=(150, -1), style=wx.CB_READONLY)
            
            # update layer index
            try:
                self.layerSelectedID = layerNameList.index(value)
            except ValueError:
                self.layerSelectedID = None

            self.comboid = self.toolbar[0].InsertControl(0, self.combo)

            # additional bindings
            self.parent.Bind(wx.EVT_COMBOBOX, self.OnSelectMap, self.comboid)

            self.toolbar[0].Realize()

        return layerNameList
