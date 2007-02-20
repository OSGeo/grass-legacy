import wx
import os
import gismutils


icons= os.path.split(gismutils.icons)[0]
icons= os.path.split(icons)[0]
icons= os.path.split(icons)[0]
print icons

class DigitToolbar:
    def __init__(self,parent,map):

        global icons
        
        self.mapcontent = map
    	self.toolbar = wx.ToolBar(parent, -1)

        icons = os.path.join(icons,"v.digit")

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

        layers = self._getListOfLayers()
        combo = wx.ComboBox(self.toolbar, 4, 'Select vector map',
                choices=layers, size=(120, 10))

        self.combo = self.toolbar.AddControl(combo)

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
    

