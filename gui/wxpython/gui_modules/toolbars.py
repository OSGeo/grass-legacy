"""
toolbars package

class:
* MapToolbar
* DigitToolbar
"""

import wx
import os
#import wxgui_utils

import cmd
from debug import Debug as Debug

#icons= os.path.split(icons)[0]
#icons= os.path.split(icons)[0]
#icons= os.path.split(icons)[0]
#print icons

if not os.getenv("GRASS_ICONPATH"):
    icons = os.getenv("GISBASE") + "/etc/gui/icons/"
else:
    icons = os.environ["GRASS_ICONPATH"]


class MapToolbar:
    """
    Main Map Display toolbar
    """

    def __init__(self, mapdisplay, map):

        global icons

        self.mapcontent = map
        self.mapdisplay = mapdisplay

    	self.toolbar = wx.ToolBar(parent=self.mapdisplay, id=wx.ID_ANY)

    	#self.SetToolBar(self.toolbar)
        tsize = (24, 24)
        self.toolbar.SetToolBitmapSize(tsize)

        #
        # Draw
        #

    	self.displaymap = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="displaymap",
                                                    bitmap=wx.Bitmap(name=os.path.join(icons,"gui-display.gif"),
                                                    type=wx.BITMAP_TYPE_ANY),
                                                    bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                    shortHelp="Display map", longHelp="")

        self.rendermap = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="rendermap",
                                                    bitmap=wx.Bitmap(name=os.path.join(icons,"gui-redraw.gif"),
                                                    type=wx.BITMAP_TYPE_ANY),
                                                    bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                    shortHelp="Re-render map", longHelp="Force re-rendering of all layers")

    	self.erase = self.toolbar.AddLabelTool(wx.ID_ANY, "erase",
                                               wx.Bitmap(os.path.join(icons,"gui-erase.gif"),
                                               wx.BITMAP_TYPE_ANY),
                                               wx.NullBitmap, wx.ITEM_NORMAL, "Erase display", "")
    	self.toolbar.AddSeparator()

        #
        # Zooming, etc.
        #
    	self.pointer = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="pointer",
                                                 bitmap=wx.Bitmap(os.path.join(icons,"gui-pointer.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap,
                                                 kind=wx.ITEM_RADIO,
                                                 shortHelp="Pointer", longHelp="")
        self.zoomin  = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoom_in",
                                                 bitmap=wx.Bitmap(os.path.join(icons,"gui-zoom_in.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_RADIO,
                                                 shortHelp="Zoom in", longHelp="Drag or click mouse to zoom")
    	self.zoomout = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoom_out",
                                                 bitmap=wx.Bitmap(os.path.join(icons,"gui-zoom_out.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap,
                                                 kind=wx.ITEM_RADIO,
                                                 shortHelp="Zoom out", longHelp="Drag or click mouse to unzoom")
    	self.pan     = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="pan",
                                                 bitmap=wx.Bitmap(os.path.join(icons,"gui-pan.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap,
                                                 kind=wx.ITEM_RADIO,
                                                 shortHelp="Pan", longHelp="Drag with mouse to pan")
    	self.query     = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="query",
                                                 bitmap=wx.Bitmap(os.path.join(icons,"gui-query.gif"),
                                                 wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap,
                                                 kind=wx.ITEM_RADIO,
                                                 shortHelp="Query", longHelp="Query selected map")
        self.toolbar.AddSeparator()


        self.zoomback = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoom_back",
                                                 bitmap=wx.Bitmap(os.path.join(icons,"gui-zoom_back.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                 shortHelp="Zoom options", longHelp="Display zoom management")
        self.zoommenu     = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoommenu",
                                                 bitmap=wx.Bitmap(os.path.join(icons,"gui-mapzoom.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap,
                                                 shortHelp="Decoration", longHelp="Add graphic overlays to map")
        self.toolbar.AddSeparator()


        self.dec     = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="dec",
                                                 bitmap=wx.Bitmap(os.path.join(icons,"module-d.barscale.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap,
                                                 shortHelp="Decoration",
                                                 longHelp="Add graphic overlays to map")

        self.toolbar.AddSeparator()

        #
        # Misc
        #
    	self.savefile = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="savefile",
                                                  #bitmap=wx.Bitmap(os.path.join(icons,"file-save.gif"),
                                                  #wx.BITMAP_TYPE_ANY),
                                                  # just testing wx.ArtProvider
                                                  bitmap=wx.ArtProvider.GetBitmap(id=wx.ART_FILE_SAVE, client=wx.ART_BUTTON, size=tsize),
                                                  bmpDisabled=wx.NullBitmap,
                                                  kind=wx.ITEM_NORMAL,
                                                  shortHelp="Save display to PNG file",
                                                  longHelp="")

        self.printmap = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="printmap",
                                                  #bitmap=wx.Bitmap(os.path.join(icons,"file-save.gif"),
                                                  #wx.BITMAP_TYPE_ANY),
                                                  # just testing wx.ArtProvider
                                                  bitmap=wx.ArtProvider.GetBitmap(id=wx.ART_PRINT, client=wx.ART_BUTTON),
                                                  bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                  shortHelp="Print display", longHelp="")

        self.toolbar.AddSeparator()

        #
        # Optional toolbars
        #
        self.combo = wx.ComboBox(parent=self.toolbar, id=wx.ID_ANY, value='Tools',
                choices=['Digitize'], style=wx.CB_READONLY, size=(110, -1))

        self.comboid = self.toolbar.AddControl(self.combo)

        self.toolbar.Realize()

    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.ReDraw,       self.displaymap)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.ReRender,       self.rendermap)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.Pointer,      self.pointer)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnZoomIn,     self.zoomin)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnZoomOut,    self.zoomout)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnPan,        self.pan)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnZoomBack,   self.zoomback)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.onDecoration, self.dec)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.onZoomMenu,   self.zoommenu)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnQuery,      self.query)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnErase,      self.erase)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.SaveToFile,   self.savefile)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.PrintMap,     self.printmap)
        self.mapdisplay.Bind(wx.EVT_COMBOBOX, self.OnSelect,                self.comboid)

    def OnSelect(self,event):
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
        self.icons      = os.path.join (os.getenv("GISBASE"), "etc/v.digit")

        # selected map to digitize
        self.layerID    = -1
        # action (digitize new point, line, etc.
        self.action     = None
        # list of available vector maps
        self.layers     = self._getListOfLayers()

        self.addString  = ""

        # create toolbar
    	self.toolbar = wx.ToolBar(parent=self.parent, id=wx.ID_ANY)
        self.toolbar.SetToolBitmapSize(wx.Size(24,24))

        self.initToolbar()

    def initToolbar(self):
        self.combo = wx.ComboBox(self.toolbar, id=wx.ID_ANY, value='Select vector map',
                                 choices=self.layers, size=(150, -1))

        self.comboid = self.toolbar.AddControl(self.combo)

        self.toolbar.AddSeparator()

        self.point = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="point",
                                               bitmap=wx.Bitmap(os.path.join(self.icons,"new.point.gif"),
                                                                wx.BITMAP_TYPE_ANY),
                                               bmpDisabled=wx.NullBitmap,
                                               kind=wx.ITEM_RADIO,
                                               shortHelp="Digitize new point",
                                               longHelp="")

    	self.line = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="line",
                                              bitmap=wx.Bitmap(os.path.join(self.icons,"new.line.gif"),
                                                               wx.BITMAP_TYPE_ANY),
                                              bmpDisabled=wx.NullBitmap,
                                              kind=wx.ITEM_RADIO,
                                              shortHelp="Digitize new line",
                                              longHelp="")

    	self.boundary = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="boundary",
                                                  bitmap=wx.Bitmap(os.path.join(self.icons,"new.boundary.gif"),
                                                                   wx.BITMAP_TYPE_ANY),
                                                  bmpDisabled=wx.NullBitmap,
                                                  kind=wx.ITEM_RADIO,
                                                  shortHelp="Digitize new boundary",
                                                  longHelp="")

    	self.centroid = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="centroid",
                                                  bitmap=wx.Bitmap(os.path.join(self.icons,"new.centroid.gif"),
                                                                   wx.BITMAP_TYPE_ANY),
                                                  bmpDisabled=wx.NullBitmap,
                                                  kind=wx.ITEM_RADIO,
                                                  shortHelp="Digitize new centroid",
                                                  longHelp="")

        self.toolbar.AddSeparator()

    	self.exit = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="exit",
                                              bitmap=wx.Bitmap(os.path.join(self.icons,"exit.gif"),
                                                               wx.BITMAP_TYPE_ANY),
                                              bmpDisabled=wx.NullBitmap,
                                              kind=wx.ITEM_NORMAL,
                                              shortHelp="Quit digitization tool",
                                              longHelp="")

        # Bindings
    	self.parent.Bind(wx.EVT_TOOL,     self.OnAddPoint,  self.point)
        self.parent.Bind(wx.EVT_TOOL,     self.OnExit,      self.exit)
        self.parent.Bind(wx.EVT_COMBOBOX, self.OnSelectMap, self.comboid)

    def OnAddPoint(self,event):
        Debug.msg (3, "DigitToolbar.OnAddPoint()")
        self.action="addpoint"
        #self.parent.MapWindow.mouse['box'] = "point"

    def OnExit (self, event):
        """
        Quit digitization tool
        """

        self.parent.RemoveToolbar ("digit")

    def OnSelectMap (self, event):
        """
        Select vector map to digitize

        If any vector map is activated for digitization this action
        is firstly terminated
        """

        self.layerID = self.combo.GetCurrentSelection()

        # digitize (self.layers[self.layerID], mapset)

    def _getListOfLayers(self):
        layers = []

        for layer in self.mapcontent.GetListOfLayers(l_type="vector"):
            layers.append (layer.name)

        return layers
