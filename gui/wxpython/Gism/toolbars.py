import wx
import os
import gismutils

import cmd


icons= os.path.split(gismutils.icons)[0]
icons= os.path.split(icons)[0]
icons= os.path.split(icons)[0]
#print icons

class DigitToolbar:
    def __init__(self,parent,map):

        global icons
        
        self.mapcontent = map
        self.digitize=None
        self.parent=parent
    	self.toolbar = wx.ToolBar(self.parent, -1)
        icons = os.path.join(icons,"v.digit")

        self.addString = ""

        self.layers = self._getListOfLayers()
        
        self.initToolbar()

    def initToolbar(self):
        self.combo = wx.ComboBox(self.toolbar, 4, 'Select vector map',
                choices=self.layers, size=(120, 10))

        self.comboid = self.toolbar.AddControl(self.combo)

        self.toolbar.AddSeparator()

        self.point = self.toolbar.AddLabelTool(-1, "point", 
                            wx.Bitmap(os.path.join(icons,"new.point.gif"), wx.BITMAP_TYPE_ANY), 
                            wx.NullBitmap, wx.ITEM_RADIO, "Digitize new point", "")
    	self.line = self.toolbar.AddLabelTool(-1, "line", 
                        wx.Bitmap(os.path.join(icons,"new.line.gif"), wx.BITMAP_TYPE_ANY), 
                       wx.NullBitmap, wx.ITEM_RADIO, "Digitize new line",
                       "")
    	self.boundary = self.toolbar.AddLabelTool(-1, "boundary", 
                        wx.Bitmap(os.path.join(icons,"new.boundary.gif"), wx.BITMAP_TYPE_ANY), 
                        wx.NullBitmap, wx.ITEM_RADIO, "Digitize new boundary", "")
    	self.centroid = self.toolbar.AddLabelTool(-1, "centroid", 
                        wx.Bitmap(os.path.join(icons,"new.centroid.gif"), wx.BITMAP_TYPE_ANY), 
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


class pokus:
    def __init__(self):
        global icons
        print icons


if __name__=="__main__":
    p = pokus()
    

