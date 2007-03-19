"""
toolbars package

class:
* MapToolbar
"""
import wx
import os
import gismutils

import cmd

icons= os.path.split(gismutils.icons)[0]
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

        #
        # Draw
        #
    	self.displaymap = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="displaymap", 
                                                    bitmap=wx.Bitmap(name=os.path.join(gismutils.icons,"gui-display.gif"),
                                                    type=wx.BITMAP_TYPE_ANY),
                                                    bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                    shortHelp="Display map", longHelp="")
    	self.erase = self.toolbar.AddLabelTool(wx.ID_ANY, "erase", 
                                               wx.Bitmap(os.path.join(gismutils.icons,"gui-erase.gif"),
                                               wx.BITMAP_TYPE_ANY), 
                                               wx.NullBitmap, wx.ITEM_NORMAL, "Erase display", "")
    	self.toolbar.AddSeparator()

        #
        # Zooming, etc.
        #
    	self.pointer = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="pointer", 
                                                 bitmap=wx.Bitmap(os.path.join(gismutils.icons,"gui-pointer.gif"),
                                                                  wx.BITMAP_TYPE_ANY), 
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_RADIO,
                                                 shortHelp="Pointer", longHelp="")
        self.zoomin  = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoom_in", 
                                                 bitmap=wx.Bitmap(os.path.join(gismutils.icons,"gui-zoom_in.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_RADIO,
                                                 shortHelp="Zoom in", longHelp="Drag or click mouse to zoom")
    	self.zoomout = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="zoom_out", 
                                                 bitmap=wx.Bitmap(os.path.join(gismutils.icons,"gui-zoom_out.gif"),
                                                                  wx.BITMAP_TYPE_ANY), 
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_RADIO,
                                                 shortHelp="Zoom out", longHelp="Drag or click mouse to unzoom")
    	self.pan     = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="pan", 
                                                 bitmap=wx.Bitmap(os.path.join(gismutils.icons,"gui-pan.gif"),
                                                                  wx.BITMAP_TYPE_ANY),
                                                 bmpDisabled=wx.NullBitmap, kind=wx.ITEM_RADIO,
                                                 shortHelp="Pan", longHelp="Drag with mouse to pan")
        self.toolbar.AddSeparator()

        #
        # Misc
        #
    	self.savefile = self.toolbar.AddLabelTool(id=wx.ID_ANY, label="savefile", 
                                                  #bitmap=wx.Bitmap(os.path.join(gismutils.icons,"file-save.gif"),
                                                  #wx.BITMAP_TYPE_ANY),
                                                  # just testing wx.ArtProvider
                                                  bitmap=wx.ArtProvider.GetBitmap(id=wx.ART_FILE_SAVE, client=wx.ART_BUTTON),
                                                  bmpDisabled=wx.NullBitmap, kind=wx.ITEM_NORMAL,
                                                  shortHelp="Save display to PNG file", longHelp="")

        self.toolbar.AddSeparator()

        #
        # Optional toolbars
        #
        combo = wx.ComboBox(self.toolbar, 4, 'Tools',
                choices=["Digitize"], size=(120, -1))
        self.combo = self.toolbar.AddControl(combo)

    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.ReDraw,     self.displaymap)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.Pointer,    self.pointer)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnZoomIn,   self.zoomin)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnZoomOut,  self.zoomout)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnPan,      self.pan)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.OnErase,    self.erase)
    	self.mapdisplay.Bind(wx.EVT_TOOL,     self.mapdisplay.SaveToFile, self.savefile)
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
    

