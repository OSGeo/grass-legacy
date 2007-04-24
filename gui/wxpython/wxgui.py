#!/usr/bin/env python
"""
Classes:
* GRasterDialog
* GMFrame
* SetVal
* GMApp
"""
import sys
import os
import wx
import wx.combo
import wx.lib.customtreectrl as CT
import wx.lib.flatnotebook as FN
import wx.stc
import wx.richtext

import sys, os, time, traceback, types

import wx                  # This module uses the new wx namespace
import wx.html


# try:
#    import subprocess
#except:
#    from compat import subprocess

import gui_modules
gmpath = gui_modules.__path__[0]
sys.path.append(gmpath)

import images
imagepath = images.__path__[0]
sys.path.append(imagepath)

import icons
gmpath = icons.__path__[0]
sys.path.append(gmpath)


import gui_modules.track as track
import gui_modules.wxgui_utils as wxgui_utils
import gui_modules.mapdisp as mapdisp
import gui_modules.render as render
import gui_modules.menudata as menudata
import gui_modules.menuform as menuform
import gui_modules.grassenv as grassenv
from icons.icon import Icons as Icons

"""Main Python app to set up GIS Manager window and trap commands
Only command console is working currently, but windows for
panels and layer tree done and demo tree items appear"""

##########################################################################
#
# wxgui.py - wxPython prototype GUI for GRASS 6+
#
# Authors: Michael Barton (Arizona State University) &
#	Jachym Cepicky (Mendel University of Agriculture)
#
# August 2006
#
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################

menucmd = {}

class GRasterDialog(wx.Frame):
    def __init__(self,parent,id=-1,title="Set raster layer"):
        wx.Frame.__init__(self, parent, id , title, size=(50,600))

        # sizers
        sizer = wx.BoxSizer(wx.VERTICAL)
        buttsizer = wx.BoxSizer(wx.HORIZONTAL)

        # labels
        lmap = wx.StaticText(self,-1,"Map name")
        lvalues = wx.StaticText(self,-1,"List of values to be displayed")
        lopaque = wx.StaticText(self,-1,"Transparency")

        # checkboxes
        cboverlay = wx.CheckBox(self, -1, "Overlay (non-null values)")
        cboverlay.SetValue(True)

        # text entries
        tmapname = wx.TextCtrl(self,-1,size=(-1,-1))
        tvalues = wx.TextCtrl(self,-1,size=(-1,-1))

        # buttons
        bsize=(75,-1)
        bok = wx.Button(self,-1, "OK",size=bsize)
        bapply = wx.Button(self,-1, "Apply", size=bsize)
        bcancel = wx.Button(self,-1, "Cancel", size=bsize)

        buttsizer.Add(bok, 0, wx.ADJUST_MINSIZE, 1)
        buttsizer.Add(bapply, 0, wx.ADJUST_MINSIZE, 1)
        buttsizer.Add(bcancel, 0, wx.ADJUST_MINSIZE, 1)
        sizer.Add(lopaque,1, wx.EXPAND,  1)
        sizer.Add(lmap,0, wx.EXPAND,  1)
        sizer.Add(tmapname,0, wx.EXPAND,  1)
        sizer.Add(lvalues,0, wx.EXPAND,  1)
        sizer.Add(tvalues,0, wx.EXPAND,  1)
        sizer.Add(cboverlay,1, wx.EXPAND,  1)
        sizer.Add(buttsizer,0, wx.ADJUST_MINSIZE, 1)
        self.SetSizer(sizer)
        sizer.Fit(self)
        self.Layout()


class GMFrame(wx.Frame):
    '''GIS Manager frame with notebook widget for controlling
    GRASS GIS. Includes command console page for typing GRASS
    (and other) commands, tree widget page for managing GIS map layers.'''
    def __init__(self, parent, id, title):
        self.parent = parent
        self.iconsize = (16, 16)
        wx.Frame.__init__(self, parent=parent, id=-1, title=title, style=wx.DEFAULT_FRAME_STYLE)

        # creating widgets
        self.notebook = self.__createNoteBook()
        self.cmdinput = self.__createCommandInput()
        self.menubar = self.__createMenuBar()
        toolbar = self.__createToolBar()
        #self.panel = wx.Panel(self,-1, style= wx.EXPAND)
        self.sizer= wx.BoxSizer(wx.VERTICAL)
        self.cmdsizer = wx.BoxSizer(wx.HORIZONTAL)
        self.track = track

        # do layout
        self.SetTitle(_("GRASS GIS Manager - wxPython Prototype"))
        self.SetMinSize((450, 450))
        self.SetIcon(wx.Icon(os.path.join(imagepath,'grass.smlogo.gif'), wx.BITMAP_TYPE_ANY))
        # self.nb_panel = wx.Panel(self)

        # initialize variables
        self.mapdisplays = {} #dictionary to index open map displays
        self.disp_idx = 0 #index value for map displays and layer trees
        self.maptree = {} #dictionary to index a layer tree to accompanying a map display
        self.mapfocus = 0 #track which display currently has focus
        self.curr_page   = '' # currently selected page for layer tree notebook
        self.curr_pagenum = '' # currently selected page number for layer tree notebook

        self.Bind(wx.EVT_CLOSE, self.onCloseWindow)
        self.Bind(wx.EVT_LEFT_DOWN, self.addRaster)

        # item, proportion, flag, border, userData
        self.sizer.Add(self.notebook, proportion=1, flag=wx.EXPAND, border=1)
        self.sizer.Add(self.cmdinput, proportion=0, flag=wx.EXPAND, border=1)
        self.SetSizer(self.sizer)
        self.sizer.Fit(self)
        self.Layout()
        wx.CallAfter(self.notebook.SetSelection, 0)

        # start default initial display
        self.newDisplay()

    def __createCommandInput(self):
        """Creates command input area"""
        #l = wx.StaticText(self, -1, "GRASS> ")

        self.cmdinput = wx.TextCtrl(self, id=wx.ID_ANY, value="", style=wx.HSCROLL|wx.TE_LINEWRAP|
                                    wx.TE_PROCESS_ENTER)

        self.cmdinput.SetFont(wx.Font(10, wx.FONTFAMILY_MODERN, wx.NORMAL, wx.NORMAL, 0, ''))
        wx.CallAfter(self.cmdinput.SetInsertionPoint, 0)

        self.Bind(wx.EVT_TEXT_ENTER, self.runCmd, self.cmdinput)

        return self.cmdinput

    def __createMenuBar(self):
        """Creates menubar"""

        self.menubar = wx.MenuBar()
        menud = menudata.Data()
        for eachMenuData in menud.GetMenu():
            for eachHeading in eachMenuData:
                menuLabel = eachHeading[0]
                menuItems = eachHeading[1]
                self.menubar.Append(self.__createMenu(menuItems), menuLabel)
        self.SetMenuBar(self.menubar)

        return self.menubar

    def __createMenu(self, menuData):
        """Cretes menu"""

        menu = wx.Menu()
        for eachItem in menuData:
            if len(eachItem) == 2:
                label = eachItem[0]
                subMenu = self.__createMenu(eachItem[1])
                menu.AppendMenu(wx.NewId(), label, subMenu)
            else:
                self.__createMenuItem(menu, *eachItem)
        return menu

    def __createMenuItem(self, menu, label, help, handler, gcmd, kind=wx.ITEM_NORMAL):
        """Creates menu items"""

        if not label:
            menu.AppendSeparator()
            return
        menuItem = menu.Append(-1, label, help, kind)
        if label:
            menucmd[label] = gcmd
        rhandler = eval(handler)
        self.Bind(wx.EVT_MENU, rhandler, menuItem)

    def __createNoteBook(self):
        """Creates notebook widgets"""

        # create main notebook widget
        #bookStyle=FN.FNB_DEFAULT_STYLE #| FN.FNB_FANCY_TABS
        #bookStyle=FN.FNB_DEFAULT_STYLE|FN.FNB_BOTTOM|FN.FNB_NO_X_BUTTON|FN.FNB_NO_NAV_BUTTONS
        nbStyle=FN.FNB_FANCY_TABS|FN.FNB_BOTTOM|FN.FNB_NO_X_BUTTON|FN.FNB_NO_NAV_BUTTONS
        self.notebook = FN.FlatNotebook(self, id=wx.ID_ANY, style=nbStyle)

        # create displays notebook widget and add it to main notebook page
        cbStyle=FN.FNB_VC8|FN.FNB_BACKGROUND_GRADIENT|FN.FNB_X_ON_TAB|FN.FNB_TABS_BORDER_SIMPLE
        #self.cb_panel = wx.Panel(self,-1, style = wx.EXPAND)
        self.gm_cb = FN.FlatNotebook(self, id=wx.ID_ANY, style=cbStyle)
        self.gm_cb.SetTabAreaColour(wx.Colour(125,200,175))
        self.notebook.AddPage(self.gm_cb, text="Map layers for each display")

        # create command output text area and add it to main notebook page
        #self.outpanel = wx.Panel(self,-1, style = wx.EXPAND)
        self.goutput = wxgui_utils.GMConsole(self)
        #self.goutput =  wx.richtext.RichTextCtrl(self,style=wx.VSCROLL|wx.HSCROLL|wx.NO_BORDER)
        self.outpage = self.notebook.AddPage(self.goutput, text="Command output")

        self.Bind(FN.EVT_FLATNOTEBOOK_PAGE_CHANGED, self.onCBPageChanged, self.gm_cb)
        self.Bind(FN.EVT_FLATNOTEBOOK_PAGE_CLOSING, self.onCBPageClosed,  self.gm_cb)

        self.out_sizer = wx.BoxSizer(wx.VERTICAL)
        self.out_sizer.Add(self.goutput, proportion=1, flag=wx.EXPAND, border=1)
        self.SetSizer(self.out_sizer)
        #self.out_sizer.Fit(self.outpage)
        #self.outpage.Layout()

        self.Centre()
        return self.notebook


    # choicebook methods
    def onCBPageChanged(self, event):
        """Page in notebook changed"""

        old_pgnum = event.GetOldSelection()
        new_pgnum = event.GetSelection()
        self.curr_page   = self.gm_cb.GetCurrentPage()
        self.curr_pagenum = self.gm_cb.GetSelection()
        try:
            self.curr_page.maptree.mapdisplay.SetFocus()
            self.curr_page.maptree.mapdisplay.Raise()
        except:
            pass

        event.Skip()

    def onCBPageClosed(self, event):
        """
        Page of notebook closed
        Also close associated map display
        """

        self.gm_cb.GetPage(event.GetSelection()).maptree.Map.Clean()
        self.gm_cb.GetPage(event.GetSelection()).maptree.Close(True)
        event.Skip()

    def runCmd(self,event):
        """Run command"""

        #global gmpath
        cmd = self.cmdinput.GetValue()

        self.goutput.runCmd(cmd)
        #menuform.GUI().parseCommand(cmd, gmpath)

    def runMenuCmd(self, event):
        """Run menu command"""

        menuitem = self.menubar.FindItemById(event.GetId())
        itemtext = menuitem.GetText()
        cmd = menucmd[itemtext]
        global gmpath
        menuform.GUI().parseCommand(cmd,gmpath, parentframe=self)

    def __createToolBar(self):
        """Creates toolbar"""

        toolbar = self.CreateToolBar()
        for each in self.toolbarData():
            self.addToolbarButton(toolbar, *each)
        toolbar.Realize()

    def addToolbarButton(self, toolbar, label, icon, help, handler):
        """Adds button to the given toolbar"""

        if not label:
            toolbar.AddSeparator()
            return
        tool = toolbar.AddLabelTool(id=wx.ID_ANY, label=label, bitmap=icon, shortHelp=help)
        self.Bind(wx.EVT_TOOL, handler, tool)

    def toolbarData(self):

        return   (
                 ('newdisplay', Icons["newdisplay"].GetBitmap(), Icons["newdisplay"].GetLabel(), self.newDisplay),
                 ('', '', '', ''),
                 ('addrast', Icons["addrast"].GetBitmap(), Icons["addrast"].GetLabel(), self.onRaster),
                 ('addvect', Icons["addvect"].GetBitmap(), Icons["addvect"].GetLabel(), self.onVector),
                 ('addcmd',  Icons["addcmd"].GetBitmap(),  Icons["addcmd"].GetLabel(),  self.addCommand),
                 ('addgrp',  wx.ArtProvider.GetBitmap(wx.ART_FOLDER, wx.ART_TOOLBAR, (16,16)),  Icons["addgrp"].GetLabel(), self.addGroup),
                 ('addovl',  Icons["addovl"].GetBitmap(),  Icons["addovl"].GetLabel(), self.onOverlay),
                 ('delcmd',  wx.ArtProvider.GetBitmap(wx.ART_DELETE, wx.ART_TOOLBAR, (16,16)), 'Delete selected layer', self.deleteLayer),
                 ('attrtable', Icons["attrtable"].GetBitmap(), Icons['attrtable'].GetLabel(), self.ShowAttributeTable),
                 )

    def ShowAttributeTable(self,event):
        if self.curr_page.maptree.GetSelection() not in self.curr_page.maptree.layertype: return
        maptype = self.curr_page.maptree.layertype[self.curr_page.maptree.GetSelection()]
        if maptype != 'vector':
            print 'Attribute management only available for vector files'
            return

        if not self.curr_page.maptree.GetPyData(self.curr_page.maptree.GetSelection()): return
        dcmd = self.curr_page.maptree.GetPyData(self.curr_page.maptree.GetSelection())[0]
        if not dcmd: return
        mapname = map = mapset = size = icon = None
        for item in dcmd.split(' '):
            if 'map=' in item:
                mapname = item.split('=')[1]
            elif 'size=' in item:
                size = item.split('=')[1]
            elif 'icon=' in item:
                icon = item.split('=')[1]

        pointdata = (icon,size)

        from gui_modules import dbm
        self.dbmanager = gui_modules.dbm.AttributeManager(self,
                    -1,"GRASS Attribute Table Manager: %s" % mapname,
                    size=wx.Size(500,300),vectmap=mapname,
                    pointdata=pointdata)


    def newDisplay(self, event=None):
        """Create new map display frame"""

        # make a new page in the bookcontrol for the layer tree (on page 0 of the notebook)
        self.pg_panel = wx.Panel(self.gm_cb, id=wx.ID_ANY, style= wx.EXPAND)
        self.gm_cb.AddPage(self.pg_panel, text="Display "+ str(self.disp_idx), select = True)
        self.curr_page = self.gm_cb.GetCurrentPage()

        # create layer tree (tree control for managing GIS layers)  and put on new notebook page
        self.curr_page.maptree = wxgui_utils.LayerTree(self.curr_page, id=wx.ID_ANY, pos=wx.DefaultPosition,
                                           size=wx.DefaultSize, style=wx.TR_HAS_BUTTONS
                                           |wx.TR_LINES_AT_ROOT|wx.TR_EDIT_LABELS|wx.TR_HIDE_ROOT
                                           |wx.TR_DEFAULT_STYLE|wx.NO_BORDER|wx.FULL_REPAINT_ON_RESIZE,
                                           idx=self.disp_idx, gismgr=self, notebook=self.gm_cb)

        # layout for controls
        cb_boxsizer = wx.BoxSizer(wx.VERTICAL)
        cb_boxsizer.Add(self.curr_page.maptree, proportion=1, flag=wx.EXPAND, border=1)
        self.curr_page.SetSizer(cb_boxsizer)
        cb_boxsizer.Fit(self.curr_page.maptree)
        self.curr_page.Layout()
        self.curr_page.maptree.Layout()

        self.disp_idx += 1

    # toolBar button handlers
    def onRaster(self, event):
        """Add raster menu"""
        point = wx.GetMousePosition()
        rastmenu = wx.Menu()
        # Add items to the menu
        addrast = wx.MenuItem(rastmenu, -1, Icons["addrast"].GetLabel())
        addrast.SetBitmap(Icons["addrast"].GetBitmap(self.iconsize))
        rastmenu.AppendItem(addrast)
        self.Bind(wx.EVT_MENU, self.addRaster, addrast)

        addrgb = wx.MenuItem(rastmenu, -1, Icons["addrgb"].GetLabel())
        addrgb.SetBitmap(Icons["addrgb"].GetBitmap(self.iconsize))
        rastmenu.AppendItem(addrgb)
        self.Bind(wx.EVT_MENU, self.addRGB, addrgb)

        addhis = wx.MenuItem(rastmenu, -1, Icons ["addhis"].GetLabel())
        addhis.SetBitmap(Icons["addhis"].GetBitmap (self.iconsize))
        rastmenu.AppendItem(addhis)
        self.Bind(wx.EVT_MENU, self.addHIS, addhis)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(rastmenu)
        rastmenu.Destroy()

    def onVector(self, event):
        """Add vector menu"""
        point = wx.GetMousePosition()
        vectmenu = wx.Menu()

        addvect = wx.MenuItem(vectmenu, -1, Icons["addvect"].GetLabel())
        addvect.SetBitmap(Icons["addvect"].GetBitmap(self.iconsize))
        vectmenu.AppendItem(addvect)
        self.Bind(wx.EVT_MENU, self.addVector, addvect)

        addtheme = wx.MenuItem(vectmenu, -1, Icons["addthematic"].GetLabel())
        addtheme.SetBitmap(Icons["addthematic"].GetBitmap(self.iconsize))
        vectmenu.AppendItem(addtheme)
        self.Bind(wx.EVT_MENU, self.addThemeMap, addtheme)

        addchart = wx.MenuItem(vectmenu, -1, Icons["addchart"].GetLabel())
        addchart.SetBitmap(Icons["addchart"].GetBitmap(self.iconsize))
        vectmenu.AppendItem(addchart)
        self.Bind(wx.EVT_MENU, self.addThemeChart, addchart)
        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(vectmenu)
        vectmenu.Destroy()

    def onOverlay(self, event):
        """Add overlay menu"""
        point = wx.GetMousePosition()
        ovlmenu = wx.Menu()

        addgrid = wx.MenuItem(ovlmenu, -1, Icons["addgrid"].GetLabel())
        addgrid.SetBitmap(Icons["addgrid"].GetBitmap(self.iconsize))
        ovlmenu.AppendItem(addgrid)
        self.Bind(wx.EVT_MENU, self.addGrid, addgrid)

        addlbl = wx.MenuItem(ovlmenu, -1, Icons["addlabels"].GetLabel())
        addlbl.SetBitmap(Icons["addlabels"].GetBitmap(self.iconsize))
        ovlmenu.AppendItem(addlbl)
        self.Bind(wx.EVT_MENU, self.addLabels, addlbl)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(ovlmenu)
        ovlmenu.Destroy()


    def addRaster(self, event):
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('raster')

    def addRGB(self, event):
        """Add RGB layer"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('rgb')

    def addHIS(self, event):
        """Add HIS layer"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('his')

    def addRastLeg(self, event):
        """Add raster legend"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('rastleg')

    def addVector(self, event):
        """Add vector layer"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('vector')

    def addThemeMap(self, event):
        """Add thematic map layer"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('thememap')

    def addThemeChart(self, event):
        """Add thematic chart layer"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('themechart')

    def addCommand(self, event):
        """Add command line layer"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('command')

    def addGroup(self, event):
        """Add layer group"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('group')

    def addGrid(self, event):
        """Add layer grid"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('grid')

    def addLabels(self, event):
        """Add layer vector labels"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('labels')

    def GetSelectedDisplay(self):
        return self.notebook.GetSelection()

    def deleteLayer(self, event):
        """
        Delete selected map display layer in GIS Manager tree widget
        """
        for layer in self.curr_page.maptree.GetSelections():
            if self.curr_page.maptree.layertype[layer] == 'group':
                self.curr_page.maptree.DeleteChildren(layer)
            self.curr_page.maptree.Delete(layer)

    #Misc methods
    def onCloseWindow(self, event):
        '''Cleanup when wxgui.py is quit'''
        try:
            for page in range(self.gm_cb.GetPageCount()):
                self.gm_cb.GetPage(page).maptree.Map.Clean()
            self.DeleteAllPages()
        except:
            self.DestroyChildren()
        self.Destroy()

    def Nomethod(self, event):
        '''Stub for testing'''
        pass
        event.Skip()

class GMApp(wx.App):
    """
    GMApp class
    """
    def OnInit(self):
##	  reexec_with_pythonw()
        # initialize all available image handlers
        wx.InitAllImageHandlers()
        # create and show main frame
        mainframe = GMFrame(None, -1, "" )
        self.SetTopWindow(mainframe)
        mainframe.Show()
        return 1

def reexec_with_pythonw():
	if sys.platform == 'darwin' and\
		not sys.executable.endswith('MacOS/Python'):
		print >>sys.stderr,'re-executing using pythonw'
		os.execvp('pythonw',['pythonw',__file__] + sys.argv[1:])


if __name__ == "__main__":

	reexec_with_pythonw()

	import gettext
	gettext.install("GMApp") # replace with the appropriate catalog name
	app = GMApp(0)
	app.MainLoop()
