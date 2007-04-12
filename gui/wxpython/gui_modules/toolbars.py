"""
toolbars package

class:
* MapToolbar
"""
import wx
import os
import wxgui_utils

import cmd

icons= os.path.split(wxgui_utils.icons)[0]
icons= os.path.split(icons)[0]
icons= os.path.split(icons)[0]
#print icons

class MapToolbar:
    """
    Main Map Display toolbar
    """
    def __init__(self, mapdisplay, map):
        global icons
        self.mapcontent = map
        self.mapdisplay = mapdisplay

    	self.toolbar = wx.ToolBar(parent=self.mapdisplay, id=wx.ID_ANY, size=(5,100))
    	#self.SetToolBar(self.toolbar)
        self.toolbar.SetToolBitmapSize(wx.Size(24,24))
        #
        # Draw
        #
    	self.displaymap = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="displaymap",
                                                    bitmap=wx.Bitmap(name=os.path.join(wxgui_utils.icons,"gui-display.gif"),
                                                    type=wx.BITMAP_TYPE_ANY),
                                                    bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                    shortHelp="Display map", longHelp="")
    	self.erase = self.toolbar.AddLabelTool(wx.ID_ANY, "erase",
                                               wx.Bitmap(os.path.join(wxgui_utils.icons,"gui-erase.gif"),
                                               wx.BITMAP_TYPE_ANY),
                                               wx.NullBitmap, wx.ITEM_NORMAL, "Erase display", "")
    	self.toolbar.AddSeparator()

        #
        # Zooming, etc.
        #
    	self.pointer = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="pointer",
                                                 bitmap=wx.Bitmap(os.path.join(wxgui_utils.icons,"gui-pointer.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_RADIO,
                                                 shortHelp="Pointer", longHelp="")
        self.zoomin  = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoom_in",
                                                 bitmap=wx.Bitmap(os.path.join(wxgui_utils.icons,"gui-zoom_in.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_RADIO,
                                                 shortHelp="Zoom in", longHelp="Drag or click mouse to zoom")
    	self.zoomout = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoom_out",
                                                 bitmap=wx.Bitmap(os.path.join(wxgui_utils.icons,"gui-zoom_out.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_RADIO,
                                                 shortHelp="Zoom out", longHelp="Drag or click mouse to unzoom")
    	self.pan     = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="pan",
                                                 bitmap=wx.Bitmap(os.path.join(wxgui_utils.icons,"gui-pan.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_RADIO,
                                                 shortHelp="Pan", longHelp="Drag with mouse to pan")
    	self.query     = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="query",
                                                 bitmap=wx.Bitmap(os.path.join(wxgui_utils.icons,"gui-query.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_RADIO,
                                                 shortHelp="Query", longHelp="Query selected map")
        self.toolbar.AddSeparator()


        self.zoomback = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoom_back",
                                                 bitmap=wx.Bitmap(os.path.join(wxgui_utils.icons,"gui-zoom_back.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                 shortHelp="Zoom options", longHelp="Display zoom management")
        self.zoommenu     = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoommenu",
                                                 bitmap=wx.Bitmap(os.path.join(wxgui_utils.icons,"gui-mapzoom.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap,
                                                 shortHelp="Decoration", longHelp="Add graphic overlays to map")
        self.toolbar.AddSeparator()


        self.dec     = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="dec",
                                                 bitmap=wx.Bitmap(os.path.join(wxgui_utils.icons,"module-d.barscale.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap,
                                                 shortHelp="Decoration", longHelp="Add graphic overlays to map")

        self.toolbar.AddSeparator()

        #
        # Misc
        #
    	self.savefile = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="savefile",
                                                  #bitmap=wx.Bitmap(os.path.join(wxgui_utils.icons,"file-save.gif"),
                                                  #wx.BITMAP_TYPE_ANY),
                                                  # just testing wx.ArtProvider
                                                  bitmap=wx.ArtProvider.GetBitmap(id=wx.ART_FILE_SAVE, client=wx.ART_BUTTON),
                                                  bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                  shortHelp="Save display to PNG file", longHelp="")

        self.printmap = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="printmap",
                                                  #bitmap=wx.Bitmap(os.path.join(wxgui_utils.icons,"file-save.gif"),
                                                  #wx.BITMAP_TYPE_ANY),
                                                  # just testing wx.ArtProvider
                                                  bitmap=wx.ArtProvider.GetBitmap(id=wx.ART_PRINT, client=wx.ART_BUTTON),
                                                  bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                  shortHelp="Print display", longHelp="")

        self.toolbar.AddSeparator()

        #
        # Optional toolbars
        #
        cb = wx.ComboBox(self.toolbar, id=wx.ID_ANY, value='',
                choices=['Digitize'], size=(-1, -1), style=wx.CB_READONLY , name='Tools')
        self.combo = self.toolbar.AddControl(cb)

        self.toolbar.Realize()

    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.ReDraw,     self.displaymap)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.Pointer,    self.pointer)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnZoomIn,   self.zoomin)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnZoomOut,  self.zoomout)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnPan,      self.pan)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnZoomBack,  self.zoomback)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.onDecoration, self.dec)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.onZoomMenu, self.zoommenu)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnQuery, self.query)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnErase,    self.erase)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.SaveToFile, self.savefile)
        self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.PrintMap, self.printmap)
        self.mapdisplay.Bind(wx.EVT_COMBOBOX, self.OnSelect,              self.combo)

    def OnSelect(self,event):
        tool =  event.GetString()

        if tool == "Digitize" and not self.mapdisplay.digittoolbar:
            self.mapdisplay.AddToolbar("digit")

class DigitToolbar:
    def __init__(self,parent,map):

        global icons

        self.mapcontent = map
        self.digitize=None
        self.parent=parent
    	self.toolbar = wx.ToolBar(self.parent, wx.ID_ANY)
        icons = os.path.join(icons,"v.digit")

        self.addString = ""

        self.layers = self._getListOfLayers()

        self.initToolbar()

    def initToolbar(self):
        self.combo = wx.ComboBox(self.toolbar, 4, 'Select vector map',
                choices=self.layers, size=(120, 10))

        self.comboid = self.toolbar.AddControl(self.combo)

        self.toolbar.AddSeparator()

        self.point = self.toolbar.AddLabelTool(wx.ID_ANY, "point",
                            wx.Bitmap(os.path.join(icons,"new.point.gif"),
                                      wx.BITMAP_TYPE_ANY),
                            wx.NullBitmap, wx.ITEM_RADIO, "Digitize new point", "")
    	self.line = self.toolbar.AddLabelTool(wx.ID_ANY, "line",
                        wx.Bitmap(os.path.join(icons,"new.line.gif"),
                                  wx.BITMAP_TYPE_ANY),
                       wx.NullBitmap, wx.ITEM_RADIO, "Digitize new line",
                       "")
    	self.boundary = self.toolbar.AddLabelTool(wx.ID_ANY, "boundary",
                        wx.Bitmap(os.path.join(icons,"new.boundary.gif"),
                                  wx.BITMAP_TYPE_ANY),
                        wx.NullBitmap, wx.ITEM_RADIO, "Digitize new boundary", "")
    	self.centroid = self.toolbar.AddLabelTool(wx.ID_ANY, "centroid",
                        wx.Bitmap(os.path.join(icons,"new.centroid.gif"),
                                  wx.BITMAP_TYPE_ANY),
                        wx.NullBitmap, wx.ITEM_RADIO, "Digitize new centroid", "")

    	self.parent.Bind(wx.EVT_TOOL, self.OnPoint, self.point)

    def OnPoint(self,event):

        self.digitize="point"
        #self.parent.MapWindow.mouse['box'] = "point"

    def AddPoint(self,x,y):
        #east,north = self.parent.Pixel2Cell(x,y)
        selectedmap=self.combo.GetCurrentSelection()
        if selectedmap < 1:
            print "you have to select map"
            return

        addstring="""P 1
                    %f %f
                    """ % (x,y)
        command = """v.edit -n  map="%s" tool=add""" % (self.combo.GetValue())
        #print addstring, command
        vedit=cmd.Command(command,stdin=addstring)
        #try:
        #    kye,val = vedit.RunV()
        #    while 1:
        #        print key,val
        #        key,val = vedit.RunV()
        #except:
        #    pass


    def _getListOfLayers(self):
        layers =[""]
        for layer in self.mapcontent.GetListOfLayers(l_type="vector"):
            layers.append( layer.name)
        return layers

#class pokus:
#    def __init__(self):
#        global icons
#        print icons
#
#if __name__=="__main__":
#    p = pokus()


