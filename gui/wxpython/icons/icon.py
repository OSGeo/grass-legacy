"""
MODULE: icon

CLASSES:
 * MetaIcon

PURPOSE: Icon themes

         from icons import Icons as Icons

AUTHORS: The GRASS Development Team
         Martin Landa <landa.martin gmail.com>

COPYRIGHT: (C) 2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import os
import wx

iconpath_default = os.getenv("GISBASE") + "/etc/gui/icons/"
iconpath = os.getenv("GRASS_ICONPATH")

icons_default = {
    # map display
    "displaymap" : 'gui-display.gif',
    "rendermap"  : 'gui-redraw.gif',
    "erase"      : 'gui-erase.gif',
    "pointer"    : 'gui-pointer.gif', 
    "zoom_in"    : 'gui-zoom_in.gif', 
    "zoom_out"   : 'gui-zoom_out.gif', 
    "pan"        : 'gui-pan.gif', 
    "query"      : 'gui-query.gif', 
    "zoom_back"  : 'gui-zoom_back.gif', 
    "zoommenu"   : 'gui-mapzoom.gif', 
    "dec"        : 'module-d.barscale.gif', 
    "savefile"   : wx.ART_FILE_SAVE, 
    "printmap"   : wx.ART_PRINT,
    # digit
    "addvpoint"  : wx.ART_ERROR,
    "addvline"   : wx.ART_ERROR,
    "addvbound"  : wx.ART_ERROR,
    "addvcentr"  : wx.ART_ERROR,
    "exit"       : wx.ART_ERROR,
    # gis manager
    "newdisplay" : 'gui-startmon.gif', 
    "addrast"    : 'element-cell.gif', 
    "addvect"    : 'element-vector.gif', 
    "addcmd"     : 'gui-cmd.gif', 
    "addgrp"     : wx.ART_ERROR, 
    "addovl"     : 'module-d.grid.gif', 
    "delcmd"     : wx.ART_ERROR, 
    "attrtable"  : wx.ART_ERROR,
    "addrgb"     : 'module-d.rgb.gif',
    "addhis"     : 'channel-his.gif',
    "addlegend"  : 'module-d.legend.gif',
    "elvect"     : 'element-vector.gif',
    "addthematic": 'module-d.vect.thematic.gif',
    "addchart"   : 'module-d.vect.chart.gif',
    "addgrid"    : 'module-d.grid.gif',
    "addlabels"  : 'module-d.labels.gif'
    }

# merge icons dictionaries, join paths
if iconpath and iconpath.find('silk') > -1: # silk icon theme
    from silk import IconsSilk as icons_img
    # use default icons if needed
    for key, img in icons_default.iteritems():
        if not icons_img.has_key(key): # add key 
            icons_img[key] = img
            iconpath_tmp = iconpath_default
        else:
            iconpath_tmp = iconpath

        if icons_img[key]: # join paths
            if type (icons_img[key]) == type(''):
                icons_img[key] = os.path.join(iconpath_tmp, icons_img[key])
else: # default icons
    icons_img = icons_default
    for key, img in icons_img.iteritems():
        if img and type (icons_img[key]) == type(''):
            icons_img[key] = os.path.join(iconpath_default, img)

class MetaIcon:
    """
    Handle icon metadata (image path, tooltip, ...)
    """
    def __init__(self, img, label, desc=None):
        self.imagepath = img
        if not self.imagepath:
            self.type = 'unknown'
        else:
            if self.imagepath.find ('wxART_') > -1:
                self.type = 'wx'
            else:
                self.type = 'img'

        self.label = label

        if desc:
            self.description = desc
        else:
            self.description = ''

    def __str__(self):
        """Debugging"""
        return "label=%s, img=%s, type=%s" % (self.label, self.imagepath, self.type)

    def GetBitmap (self, size=None):
        """Get bitmap"""
        bmp = None
        
        if self.type == 'wx':
            bmp = wx.ArtProvider.GetBitmap(id=self.imagepath, client=wx.ART_TOOLBAR, size=size)
        elif self.type == 'img':
            if os.path.isfile(self.imagepath) and os.path.getsize(self.imagepath):
                if size and len(size) == 2:
                    image = wx.Image (name=self.imagepath)
                    image.Rescale (size[0], size[1])
                    bmp = image.ConvertToBitmap()
                elif self.imagepath:
                    bmp = wx.Bitmap (name=self.imagepath)

        return bmp

    def GetLabel (self):
        return self.label

    def GetDesc (self):
        return self.description

#
# create list of icon instances
#
Icons = {
    # map display
    "displaymap" : MetaIcon (img=icons_img["displaymap"], label="Display map"),
    "rendermap"  : MetaIcon (img=icons_img["rendermap"], label="Re-render map", desc="Force re-rendering of all layers"),
    "erase"      : MetaIcon (img=icons_img["erase"], label="Erase display"),
    "pointer"    : MetaIcon (img=icons_img["pointer"], label="Pointer"),
    "zoom_in"    : MetaIcon (img=icons_img["zoom_in"], label="Zoom in", desc="Drag or click mouse to zoom"),
    "zoom_out"   : MetaIcon (img=icons_img["zoom_out"], label="Zoom out", desc="Drag or click mouse to unzoom"),
    "pan"        : MetaIcon (img=icons_img["pan"], label="Pan", desc="Drag with mouse to pan"),
    "query"      : MetaIcon (img=icons_img["query"], label="Query", desc="Query selected map"),
    "zoom_back"  : MetaIcon (img=icons_img["zoom_back"], label="Return to previous zoom"),
    "zoommenu"   : MetaIcon (img=icons_img["zoommenu"], label="Zoom options", desc="Display zoom management"),
    "dec"        : MetaIcon (img=icons_img["dec"], label="Decoration", desc="Add graphic overlays to map"),
    "savefile"   : MetaIcon (img=icons_img["savefile"], label="Save display to PNG file"),
    "printmap"   : MetaIcon (img=icons_img["printmap"], label="Print display"),
    # gis manager
    "newdisplay" : MetaIcon (img=icons_img["newdisplay"], label="Start new display"),
    "addrast"    : MetaIcon (img=icons_img["addrast"], label="Add raster map layer"),
    "addvect"    : MetaIcon (img=icons_img["addvect"], label="Add vector map layer"),
    "addcmd"     : MetaIcon (img=icons_img["addcmd"], label="Add command layer"),
    "addgrp"     : MetaIcon (img=icons_img["addgrp"], label="Add layer group"),
    "addovl"     : MetaIcon (img=icons_img["addovl"], label="Add grid or vector labels overlay"),
    "delcmd"     : MetaIcon (img=icons_img["delcmd"], label="Delete selected layer"),
    "attrtable"  : MetaIcon (img=icons_img["attrtable"], label="Show attribute table"),
    "addrgb"     : MetaIcon (img=icons_img["addrgb"], label="Add RGB layer"),
    "addhis"     : MetaIcon (img=icons_img["addhis"], label="Add HIS layer"),
    "addlegend"  : MetaIcon (img=icons_img["addlegend"], label="Add legend"),
    "elvect"     : MetaIcon (img=icons_img["elvect"], label=""),
    "addthematic": MetaIcon (img=icons_img["addthematic"], label="Add thematic layer"),
    "addchart"   : MetaIcon (img=icons_img["addchart"], label="Add thematic chart layer"),
    "addgrid"    : MetaIcon (img=icons_img["addgrid"], label="Add grid layer"),
    "addlabels"  : MetaIcon (img=icons_img["addlabels"], label="Add labels"),
    # digit
    "addvpoint"  : MetaIcon (img=icons_img["addvpoint"], label="Digitize new point"),
    "addvline"   : MetaIcon (img=icons_img["addvline"], label="Digitize new line"),
    "addvbound"  : MetaIcon (img=icons_img["addvbound"], label="Digitize new boundary"),
    "addvcentr"  : MetaIcon (img=icons_img["addvcentr"], label="Digitize new centroid"),
    "exit"       : MetaIcon (img=icons_img["exit"], label="Quit digitization tool")}

# testing ...
if __name__ == "__main__":
    for k,v in Icons.iteritems():
        print k, "/", v

    print icons_default["savefile"]
