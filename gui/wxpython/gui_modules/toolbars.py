"""
toolbars package

class:
* MapToolbar
* DigitToolbar
"""

import wx
import os, sys
#import wxgui_utils

gmpath = os.getenv("GISBASE") + "/etc/wx/icons/"
sys.path.append(gmpath)

import cmd, grassenv
from debug import Debug as Debug
from icon import Icons as Icons

class MapToolbar:
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

        #
        # Draw
        #
        self.displaymap = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="displaymap",
                                                    bitmap=Icons["displaymap"].GetBitmap(),
                                                    bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                    shortHelp=Icons["displaymap"].GetLabel(), longHelp=Icons["displaymap"].GetDesc())

        self.rendermap = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="rendermap",
                                                   bitmap=Icons["rendermap"].GetBitmap(),
                                                   bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                   shortHelp=Icons["rendermap"].GetLabel(), longHelp=Icons["rendermap"].GetDesc())

        self.erase = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="erase",
                                               bitmap=Icons["erase"].GetBitmap(),
                                               bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                               shortHelp=Icons["erase"].GetLabel(), longHelp=Icons["erase"].GetDesc())
        self.toolbar.AddSeparator()

        #
        # Zooming, etc.
        #
        self.pointer = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="pointer",
                                                 bitmap=Icons["pointer"].GetBitmap(),
                                                 kind=wx.ITEM_RADIO,
                                                 shortHelp=Icons["pointer"].GetLabel(), longHelp=Icons["pointer"].GetDesc())
        self.query   = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="query",
                                                 bitmap=Icons["query"].GetBitmap(),
                                                 bmpDisabled=wx.NullBitmap,
                                                 kind=wx.ITEM_RADIO,
                                                 shortHelp=Icons["query"].GetLabel(), longHelp=Icons["query"].GetDesc())
        self.pan     = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="pan",
                                                 bitmap=Icons["pan"].GetBitmap(),
                                                 bmpDisabled=wx.NullBitmap,
                                                 kind=wx.ITEM_RADIO,
                                                 shortHelp=Icons["pan"].GetLabel(), longHelp=Icons["pan"].GetDesc())
        self.zoomin  = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoom_in",
                                                 bitmap=Icons["zoom_in"].GetBitmap(),
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_RADIO,
                                                 shortHelp=Icons["zoom_in"].GetLabel(), longHelp=Icons["zoom_in"].GetDesc())
        self.zoomout = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoom_out",
                                                 bitmap=Icons["zoom_out"].GetBitmap(),
                                                 bmpDisabled=wx.NullBitmap,
                                                 kind=wx.ITEM_RADIO,
                                                 shortHelp=Icons["zoom_out"].GetLabel(), longHelp=Icons["zoom_out"].GetDesc())
        self.zoomback = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoom_back",
                                                 bitmap=Icons["zoom_back"].GetBitmap(),
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                 shortHelp=Icons["zoom_back"].GetLabel(), longHelp=Icons["zoom_back"].GetDesc())
        self.zoommenu = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoommenu",
                                                  bitmap=Icons["zoommenu"].GetBitmap(),
                                                  bmpDisabled=wx.NullBitmap,
                                                  shortHelp=Icons["zoommenu"].GetLabel(), longHelp=Icons["zoommenu"].GetDesc())

        self.toolbar.AddSeparator()

        self.analyze = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="analyze",
                                             bitmap=Icons["analyze"].GetBitmap(),
                                             bmpDisabled=wx.NullBitmap,
                                             shortHelp=Icons["analyze"].GetLabel(),
                                             longHelp=Icons["analyze"].GetDesc())
        self.toolbar.AddSeparator()


        self.dec = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="overlay",
                                             bitmap=Icons["overlay"].GetBitmap(),
                                             bmpDisabled=wx.NullBitmap,
                                             shortHelp=Icons["overlay"].GetLabel(),
                                             longHelp=Icons["overlay"].GetDesc())

        self.toolbar.AddSeparator()

        #
        # Misc
        #
        self.savefile = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="savefile",
                                                  bitmap=Icons["savefile"].GetBitmap(),
                                                  bmpDisabled=wx.NullBitmap,
                                                  kind=wx.ITEM_NORMAL,
                                                  shortHelp=Icons["savefile"].GetLabel(),
                                                  longHelp=Icons["savefile"].GetDesc())

        self.printmap = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="printmap",
                                                  bitmap=Icons["printmap"].GetBitmap(),
                                                  bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                  shortHelp=Icons["printmap"].GetLabel(),
                                                  longHelp=Icons["printmap"].GetDesc())

        self.toolbar.AddSeparator()

        #
        # Optional toolbars
        #
        self.combo = wx.ComboBox(parent=self.toolbar, id=wx.ID_ANY, value='Tools',
                                 choices=['Digitize'], style=wx.CB_READONLY, size=(90, -1))

        self.comboid = self.toolbar.AddControl(self.combo)

        self.toolbar.Realize()

        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.ReDraw,       self.displaymap)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.ReRender,     self.rendermap)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.Pointer,      self.pointer)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnZoomIn,     self.zoomin)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnZoomOut,    self.zoomout)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnPan,        self.pan)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnZoomBack,   self.zoomback)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnDecoration, self.dec)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnZoomMenu,   self.zoommenu)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnQuery,      self.query)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnAnalyze,    self.analyze)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnErase,      self.erase)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.SaveToFile,   self.savefile)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.PrintMenu,    self.printmap)
        self.mapdisplay.Bind(wx.EVT_COMBOBOX, self.OnSelect,                self.comboid)

    def OnSelect(self, event):
        """
        Select / enable tool available in tools list
        """
        tool =  event.GetString()

        if tool == "Digitize" and not self.mapdisplay.digittoolbar:
            self.mapdisplay.AddToolbar("digit")

class DigitToolbar:
    """
    Toolbar for digitization
    """

    def __init__(self, parent, map):
        self.mapcontent = map
        self.parent     = parent

        # selected map to digitize
        self.layerID    = None
        self.layers     = []
        # action (digitize new point, line, etc.
        self.action     = "add"
        self.type       = "point"
        self.addString  = ""

        self.comboid    = None

        # create toolbar
        self.toolbar = wx.ToolBar(parent=self.parent, id=wx.ID_ANY)
        self.toolbar.SetToolBitmapSize(wx.Size(24,24))

        # create toolbar
        self.initToolbar()

        # list of available vector maps
        self.UpdateListOfLayers(updateTool=True)

        self.toolbar.Realize()

    def initToolbar(self):
        """
        Init digitization toolbar
        """
        self.toolbar.AddSeparator()

        self.point = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="digaddpoint",
                                               bitmap=Icons["digaddpoint"].GetBitmap(),
                                               bmpDisabled=wx.NullBitmap,
                                               kind=wx.ITEM_RADIO,
                                               shortHelp=Icons["digaddpoint"].GetLabel(),
                                               longHelp=Icons["digaddpoint"].GetDesc())

        self.line = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="digaddline",
                                              bitmap=Icons["digaddline"].GetBitmap(),
                                              bmpDisabled=wx.NullBitmap,
                                              kind=wx.ITEM_RADIO,
                                              shortHelp=Icons["digaddline"].GetLabel(),
                                              longHelp=Icons["digaddline"].GetDesc())

        self.boundary = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="digaddbound",
                                                  bitmap=Icons["digaddbound"].GetBitmap(),
                                                  bmpDisabled=wx.NullBitmap,
                                                  kind=wx.ITEM_RADIO,
                                                  shortHelp=Icons["digaddbound"].GetLabel(),
                                                  longHelp=Icons["digaddbound"].GetDesc())

        self.centroid = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="digaddcentr",
                                                  bitmap=Icons["digaddcentr"].GetBitmap(),
                                                  bmpDisabled=wx.NullBitmap,
                                                  kind=wx.ITEM_RADIO,
                                                  shortHelp=Icons["digaddcentr"].GetLabel(),
                                                  longHelp=Icons["digaddcentr"].GetDesc())

        self.toolbar.AddSeparator()

        self.exit = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="digexit",
                                              bitmap=Icons["digexit"].GetBitmap(),
                                              bmpDisabled=wx.NullBitmap,
                                              kind=wx.ITEM_NORMAL,
                                              shortHelp=Icons["digexit"].GetLabel(),
                                              longHelp=Icons["digexit"].GetDesc())

        # Bindings
        self.parent.Bind(wx.EVT_TOOL,     self.OnAddPoint,    self.point)
        self.parent.Bind(wx.EVT_TOOL,     self.OnAddLine,     self.line)
        self.parent.Bind(wx.EVT_TOOL,     self.OnAddBoundary, self.boundary)
        self.parent.Bind(wx.EVT_TOOL,     self.OnAddCentroid, self.centroid)
        self.parent.Bind(wx.EVT_TOOL,     self.OnExit,      self.exit)
        self.parent.Bind(wx.EVT_COMBOBOX, self.OnSelectMap, self.comboid)

    def OnAddPoint(self, event):
        """Add point to the vector map layer"""
        Debug.msg (3, "DigitToolbar.OnAddPoint()")
        self.action = "add"
        self.type   = "point"

    def OnAddLine(self, event):
        """Add line to the vector map layer"""
        Debug.msg (3, "DigitToolbar.OnAddLine()")
        self.action = "add"
        self.type   = "line"

    def OnAddBoundary(self, event):
        """Add boundary to the vector map layer"""
        Debug.msg (3, "DigitToolbar.OnAddBoundary()")
        self.action = "add"
        self.type   = "boundary"

    def OnAddCentroid(self, event):
        """Add centroid to the vector map layer"""
        Debug.msg (3, "DigitToolbar.OnAddCentroid()")
        self.action = "add"
        self.type   = "centroid"


    def OnExit (self, event):
        """
        Quit digitization tool
        """
        Debug.msg (3, "DigitToolbar.OnExit(): layer=%s" % \
                   self.layers[self.layerID].name)

        # deactive the toolbar
        self.parent.RemoveToolbar ("digit")

    def OnSelectMap (self, event):
        """
        Select vector map layer for editing

        If there is a vector map layer already edited, this action is
        firstly terminated. The map layer is closed. After this the
        selected map layer activated for editing.
        """
        if self.layerID: # deactive map layer for editing
            # TODO
            pass

        # select the given map layer for editing
        self.layerID = self.combo.GetCurrentSelection()

        Debug.msg (3, "DigitToolbar.OnSelectMap(): layerID=%d layer=%s" % \
                   (self.layerID, self.layers[self.layerID].name))

    def StartEditing (self, layerSelected):
        """
        Mark map layer enabled for digitization

        Return True on success or False if layer cannot be edited
        """
        try:
            self.layerID = self.layers.index(layerSelected)
            self.combo.SetValue (layerSelected.name)
            self.parent.maptoolbar.combo.SetValue ('Digitize')
            return True
        except:
            return False

    def UpdateListOfLayers (self, updateTool=False):
        """
        Update list of available vector map layers.
        This list consists only editable layers (in the current mapset)

        Optionaly also update toolbar
        """

        layerNameSelected = None
        if self.layerID != None: # name of currently selected layer
            layerNameSelected = self.layers[self.layerID].name

        # select vector map layer in the current mapset
        layerNameList = []
        self.layers = self.mapcontent.GetListOfLayers(l_type="vector", l_mapset=grassenv.env["MAPSET"])
        for layer in self.layers:
            if not layer.name in layerNameList: # do not duplicate layer
                layerNameList.append (layer.name)

        if updateTool: # update toolbar
            if self.layerID == None:
                value = 'Select vector map'
            else:
                value = layerNameSelected

            # ugly ...
            if self.comboid:
                self.toolbar.DeleteToolByPos(0)
                #self.combo.Destroy()

            self.combo = wx.ComboBox(self.toolbar, id=wx.ID_ANY, value=value,
                                     choices=layerNameList, size=(150, -1))

            self.comboid = self.toolbar.InsertControl(0, self.combo)
            self.toolbar.Realize()

        return layerNameList
