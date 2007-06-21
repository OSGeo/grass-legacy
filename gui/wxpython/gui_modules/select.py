"""
MODULE: select

CLASSES:
 * Select
 * TreeCrtlComboPopup

PURPOSE: Custon control that selects GRASS GIS elements

AUTHORS: The GRASS Development Team. Michael Barton & Martin Landa

COPYRIGHT: (C) 2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import os
import sys

import wx
import wx.combo

GuiModulePath = os.path.join(os.getenv("GISBASE"), "etc", "wx", "gui_modules")
sys.path.append(GuiModulePath)

import cmd

class Select(wx.combo.ComboCtrl):
    def __init__(self, parent, id, size, type):
        """
        Custom control to create a ComboBox with a tree control
        to display GIS elements within acessible mapsets.
        Elements can be selected with mouse.
        """
        wx.combo.ComboCtrl.__init__(self, parent=parent, id=id, size=size)
        tcp = TreeCtrlComboPopup()
        self.SetPopupControl(tcp)
        self.SetPopupExtents(0,100)
        tcp.GetElementList(type)

class TreeCtrlComboPopup(wx.combo.ComboPopup):
    """
    Create a tree ComboBox for selecting maps and other GIS elements
    in accessible mapsets within the current location
    """

    # overridden ComboPopup methods

    def Init(self):
        self.value = None
        self.curitem = None


    def Create(self, parent):
        self.seltree = wx.TreeCtrl(parent, style=wx.TR_HIDE_ROOT
                                   |wx.TR_HAS_BUTTONS
                                   |wx.TR_SINGLE
                                   |wx.TR_LINES_AT_ROOT
                                   |wx.SIMPLE_BORDER
                                   |wx.TR_FULL_ROW_HIGHLIGHT)
        self.seltree.Bind(wx.EVT_MOTION, self.OnMotion)
        self.seltree.Bind(wx.EVT_LEFT_DOWN, self.OnLeftDown)
        self.seltree.Bind(wx.EVT_TREE_ITEM_EXPANDING, self.mapsetExpanded)
        self.seltree.Bind(wx.EVT_TREE_ITEM_COLLAPSED, self.mapsetCollapsed)
        self.seltree.Bind(wx.EVT_TREE_ITEM_ACTIVATED, self.mapsetActivated)
        self.seltree.Bind(wx.EVT_TREE_SEL_CHANGED, self.mapsetSelected)

    # the following dummy handler are needed to keep tree events from propagating up to
    # the parent GIS Manager layer tree
    def mapsetExpanded(self, event):
        pass

    def mapsetCollapsed(self, event):
        pass

    def mapsetActivated(self, event):
        pass

    def mapsetSelected(self, event):
        pass
    # end of dummy events

    def GetControl(self):
        return self.seltree

    def GetStringValue(self):
        if self.value:
            return self.seltree.GetItemText(self.value)
        return ""

    def OnPopup(self):
        if self.value:
            self.seltree.EnsureVisible(self.value)
            self.seltree.SelectItem(self.value)

    def SetStringValue(self, value):
        # this assumes that item strings are unique...
        root = self.seltree.GetRootItem()
        if not root:
            return
        found = self.FindItem(root, value)
        if found:
            self.value = found
            self.seltree.SelectItem(found)

    def GetAdjustedSize(self, minWidth, prefHeight, maxHeight):
        return wx.Size(minWidth, min(200, maxHeight))

    def GetElementList(self, element):
        """
        Get list of GIS elements in accessible mapsets and display as tree
        with all relevant elements displayed beneath each mapset branch
        """
        #set environmental variables
        cmdlist = ['g.gisenv', 'get=GISDBASE']
        gisdbase = cmd.Command(cmdlist).module_stdout.read().strip()

        cmdlist = ['g.gisenv', 'get=LOCATION_NAME']
        location = cmd.Command(cmdlist).module_stdout.read().strip()

        cmdlist = ['g.gisenv', 'get=MAPSET']
        curr_mapset = cmd.Command(cmdlist).module_stdout.read().strip()

        location_path = os.path.join (gisdbase,location)
        windfile = os.path.join (location_path,'PERMANENT','WIND')
        symbol_path = os.path.join (os.environ['GISBASE'],'etc','symbol')

        #valid location test if needed
        if windfile != None:
            pass

        #mapsets in current location
        cmdlist = ['g.mapsets', '-p']
        mapsets = cmd.Command(cmdlist).module_stdout.read().strip().split(' ')

        elementlist = ['cell',
                       'grid3d',
                       'vector',
                       'dig',
                       'dig_ascii',
                       'icons',
                       'paint/labels',
                       'site_lists',
                       'windows',
                       'windows3d',
                       'group',
                       '3d.view']

        if element not in elementlist:
            self.AddItem('Not selectable element')
            return

        #Get directory tree nodes
        for dir in mapsets:
            if dir == curr_mapset:
                dir_node = self.AddItem('Mapset: '+dir)
                self.seltree.SetItemTextColour(dir_node, wx.Colour(50,50,200))
                try:
                    elem_list = os.listdir(os.path.join (location_path, dir, element))
                    elem_list.sort()
                    for elem in elem_list:
                        self.AddItem(elem+'@'+dir, parent=dir_node)
                except:
                    continue
            else:
                dir_node = self.AddItem('Mapset: '+dir)
                self.seltree.SetItemTextColour(dir_node,wx.Colour(50,50,200))
                try:
                    elem_list = os.listdir(os.path.join (location_path, dir, element))
                    elem_list.sort()
                    for elem in elem_list:
                        self.AddItem(elem+'@'+dir, parent=dir_node)
                except:
                    continue

            if self.seltree.ItemHasChildren(dir_node):
                self.seltree.Expand(dir_node)

    # helpers
    def FindItem(self, parentItem, text):
        item, cookie = self.seltree.GetFirstChild(parentItem)
        while item:
            if self.seltree.GetItemText(item) == text:
                return item
            if self.seltree.ItemHasChildren(item):
                item = self.FindItem(item, text)
            item, cookie = self.seltree.GetNextChild(parentItem, cookie)
        return wx.TreeItemId();


    def AddItem(self, value, parent=None):
        if not parent:
            root = self.seltree.GetRootItem()
            if not root:
                root = self.seltree.AddRoot("<hidden root>")
            parent = root

        item = self.seltree.AppendItem(parent, text=value)
        return item

    def OnMotion(self, evt):
        # have the selection follow the mouse, like in a real combobox
        item, flags = self.seltree.HitTest(evt.GetPosition())
        if item and flags & wx.TREE_HITTEST_ONITEMLABEL:
            self.seltree.SelectItem(item)
            self.curitem = item
        evt.Skip()

    def OnLeftDown(self, evt):
        # do the combobox selection
        item, flags = self.seltree.HitTest(evt.GetPosition())
        if item and flags & wx.TREE_HITTEST_ONITEMLABEL:
            self.curitem = item

            if self.seltree.GetRootItem() == self.seltree.GetItemParent(item):
                self.value = None # cannot select mapset item
            else:
                self.value = item

            self.Dismiss()

        evt.Skip()



