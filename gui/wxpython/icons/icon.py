"""!
@package icons.icon

@brief Icon metadata

Classes:
 - MetaIcon

(C) 2007-2008, 2010-2011 by the GRASS Development Team

This program is free software under the GNU General Public License
(>=v2). Read the file COPYING that comes with GRASS for details.

@author Martin Landa <landa.martin gmail.com>
@author Anna Kratochvilova <kratochanna gmail.com>
"""

import os
import sys
import types
import copy

import wx

from core.settings import UserSettings

import grass2_icons # default icon set
iconPathDefault = grass2_icons.iconPath
iconSetDefault  = grass2_icons.iconSet

iconTheme = UserSettings.Get(group = 'appearance', key = 'iconTheme', subkey = 'type')
if iconTheme == 'silk':
    import silk_icons
    iconPath = silk_icons.iconPath
    iconSet  = silk_icons.iconSet
elif iconTheme == 'grass':
    import grass_icons
    iconPath = grass_icons.iconPath
    iconPathVDigit = grass_icons.iconPathVDigit
    iconSet  = grass_icons.iconSet
else:
    iconPath = iconPathDefault
    iconSet  = iconSetDefault

# merge icons dictionaries, join paths
try:
    if iconPath and not os.path.exists(iconPath):
        raise OSError
    
    if iconTheme != 'grass':
        # use default icons if no icon is available
        for key, img in iconSet.iteritems():
            if key not in iconSet or \
                    iconSet[key] is None: # add key
                iconSet[key] = img
            
            iconSet[key] = os.path.join(iconPath, iconSet[key])
    else:
        for key, img in iconSet.iteritems():
            if img and type(iconSet[key]) == types.StringType:
                if key in ("point-create",
                           "line-create",
                           "boundary-create",
                           "centroid-create",
                           "polygon-create",
                           "vertex-create",
                           "vertex-move",
                           "vertex-delete",
                           "line-split",
                           "line-edit",
                           "line-move",
                           "line-delete",
                           "cats-copy",
                           "cats-display",
                           "attributes-display",
                           "undo",
                           "tools"):
                    iconSet[key] = os.path.join(iconPathVDigit, img)
                else:
                    iconSet[key] = os.path.join(iconPath, img)

except StandardError, e:
    sys.exit(_("Unable to load icon theme. Reason: %s") % e)

class MetaIcon:
    """!Handle icon metadata (image path, tooltip, ...)
    """
    def __init__(self, img, label = None, desc = None):
        self.imagepath = iconSet.get(img, wx.ART_MISSING_IMAGE)
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
        return "label=%s, img=%s, type=%s" % (self.label, self.imagepath, self.type)

    def GetBitmap(self, size = None):
        bmp = None
        
        if self.type == 'wx':
            bmp = wx.ArtProvider.GetBitmap(id = self.imagepath, client = wx.ART_TOOLBAR, size = size)
        elif self.type == 'img':
            if os.path.isfile(self.imagepath) and os.path.getsize(self.imagepath):
                if size and len(size) == 2:
                    image = wx.Image(name = self.imagepath)
                    image.Rescale(size[0], size[1])
                    bmp = image.ConvertToBitmap()
                elif self.imagepath:
                    bmp = wx.Bitmap(name = self.imagepath)
        
        return bmp
    
    def GetLabel(self):
        return self.label

    def GetDesc(self):
        return self.description
    
    def GetImageName(self):
        return os.path.basename(self.imagepath)

    def SetLabel(self, label = None, desc = None):
        """!Set label/description for icon
        @param label icon label (None for no change)
        @param desc icon description (None for no change)
        
        @return copy of original object
        """
        cobj = copy.copy(self)
        if label:
            cobj.label = label
        if desc:
            cobj.description = desc
        
        return cobj
