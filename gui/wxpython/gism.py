#!/usr/bin/env python
import sys
import os
import wx
import wx.lib.customtreectrl as CT
import re
import tempfile

try:
    import subprocess
except:
    from compat import subprocess

import Gism
gmpath = Gism.__path__[0]
sys.path.append(gmpath)

if not os.getenv("GISBASE"):
    sys.stderr.write("GISBASE not set, you have to be in running GRASS session!\n")
    sys.exit(1)

gipath = ""
if not os.getenv("GRASS_ICONPATH"):
    gipath = os.getenv("GISBASE")+"/etc/gui/icons/"
else:
    gipath = os.environ["GRASS_ICONPATH"]

os.environ["GRASS_ICONPATH"] = gipath

import Gism.gismutils as gmutils
import Gism.mapdisp as mapdisp
import Gism.render as render
import Gism.menudata as menudata
import Gism.menuform as menuform


"""Main Python app to set up GIS Manager window and trap commands
Only command console is working currently, but windows for
panels and layer tree done and demo tree items appear"""

##########################################################################
#
# gism.py - wxPython prototype GUI for GRASS 6+
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

class GMFrame(wx.Frame):
    '''GIS Manager frame with notebook widget for controlling
    GRASS GIS. Includes command console page for typing GRASS
    (and other) commands, tree widget page for managing GIS map layers,'''
    def __init__(self, *args, **kwds):
        kwds["style"] = wx.DEFAULT_FRAME_STYLE
        wx.Frame.__init__(self, *args, **kwds)
        self.createToolBar()
        self.createMenuBar()
        self.SetTitle(_("GRASS GIS Manager - wxPython Prototype"))
        self.SetSize((450, 450))
        self.SetMinSize((100, 100))
        self.cb_panel = wx.Panel(self)

        # create choicebook for GIS controls - one page for each display
        self.gm_cb = GMChoicebook(self.cb_panel, -1, wx.DefaultPosition, wx.DefaultSize, wx.CHB_TOP)
        boxsizer = wx.BoxSizer()
        boxsizer.Add(self.gm_cb, 1, wx.EXPAND)
        self.cb_panel.SetSizer(boxsizer)
        self.cb_panel.SetAutoLayout(True)
        self.Centre()
        render.Track().SetChbk(self.gm_cb)

        # initialize variables
        self.MapDisplay = {} #dictionary to index open map displays
        self.disp_idx = 0 #index value for map displays and layer trees
        self.maptree = {} #dictionary to index a layer tree to accompanying a map display
        self.mapfocus = 0 #track which display currently has focus

        self.Bind(wx.EVT_CLOSE, self.onCloseWindow)

        #start default initial display
        self.NewDisplay()

    #---Menubar creation---#000000#FFFFFF-------------------------------------------

    def createMenuBar(self):
        menuBar = wx.MenuBar()
        # data object with descriptions for each menu item is in separate module menudata.py
        self.Data = menudata.Data()
        for eachMenuData in self.Data.GetMenu():
            for eachHeading in eachMenuData:
                menuLabel = eachHeading[0]
                menuItems = eachHeading[1]
                menuBar.Append(self.createMenu(menuItems), menuLabel)
        self.SetMenuBar(menuBar)

    def createMenu(self, menuData):
        menu = wx.Menu()
        for eachItem in menuData:
            if len(eachItem) == 2:
                label = eachItem[0]
                subMenu = self.createMenu(eachItem[1])
                menu.AppendMenu(wx.NewId(), label, subMenu)
            else:
                self.createMenuItem(menu, *eachItem)
        return menu

    def createMenuItem(self, menu, label, help, handler, gcmd, kind=wx.ITEM_NORMAL):
        if not label:
            menu.AppendSeparator()
            return
        menuItem = menu.Append(-1, label, help, kind)
        if label:
            menucmd[label] = gcmd
        rhandler = eval(handler)
        self.Bind(wx.EVT_MENU, rhandler, menuItem)

    def runMenuCmd(self, event):
        '''Run menu command'''
        menuitem = self.GetMenuBar().FindItemById(event.GetId())
        itemtext = menuitem.GetText()
        cmd = menucmd[itemtext]
        global gmpath
        menuform.GUI().parseCommand(cmd, gmpath)

    #---Toolbar creation---#000000#FFFFFF-------------------------------------------
    def createToolBar(self):
        toolbar = self.CreateToolBar()
        for each in self.toolbarData():
            self.addToolbarButton(toolbar, *each)
        toolbar.Realize()

    def addToolbarButton(self, toolbar, label, iconfile, help, handler):
        if not label:
            toolbar.AddSeparator()
            return
        icon = wx.Bitmap(iconfile, wx.BITMAP_TYPE_ANY)
        tool = toolbar.AddSimpleTool(-1, icon, label, help)
        self.Bind(wx.EVT_TOOL, handler, tool)

    def toolbarData(self):

        global gipath
        return (
            ('newdisplay', gipath+r'/gui-startmon.gif', 'Start new display', self.NewDisplay),
            ('', '', '', ''),
            ('addraster', gipath+r'/element-cell.gif', 'Add raster layer', self.AddRaster),
            ('addvect', gipath+r'/element-vector.gif', 'Add vector layer', self.AddVector),
            ('addcmd', gipath+r'/gui-cmd.gif', 'Add command layer', self.AddCommand)
            )

    #---Start display---#000000#FFFFFF----------------------------------------------
    def NewDisplay(self, event=None):
        '''Create new map display widget'''
        #update display index
        #start a new display, indexed by disp_idx
        #mID = wx.NewId()
        self.MapDisplay[self.disp_idx] = mapdisp.MapFrame(self, -1, 'Map Display', wx.DefaultPosition, wx.DefaultSize,
            style=wx.DEFAULT_FRAME_STYLE, cb=self.gm_cb, idx=self.disp_idx)
        self.MapDisplay[self.disp_idx].SetTitle(_("Map Display-"+str(self.disp_idx)))
        #self.maptree[self.disp_idx] = self.MapDisplay[self.disp_idx].getTree()
        self.MapDisplay[self.disp_idx].Show()
        self.disp_idx += 1

    #---ToolBar button handlers---#000000#FFFFFF------------------------------------
    def AddRaster(self, event):
        self.SetTree('raster')
        event.Skip()

    def AddVector(self, event):
        self.SetTree('vector')
        event.Skip()

    def AddCommand(self, event):
        self.SetTree('command')
        event.Skip()

    def SetTree(self, layertype):
         #get ID of active display
        disp_idx = render.Track().GetDisp_idx()
        #get layer tree for active display
        layertree = render.Track().GetTree(disp_idx)
        #add new layer to tree
        layertree.AddLayer(disp_idx, layertype)
        return

    #---Misc methods---#000000#FFFFFF-----------------------------------------------
    def onCloseWindow(self, event):
        '''Cleanup when gism.py is quit'''
        mdlist = range(0, self.disp_idx+1)
        try:
            for md in mdlist:
                if self.MapDisplay.has_key(md):
                    if self.MapDisplay[md].Close(False):
                        self.MapDisplay[md].Close(True)
        except:
            self.DestroyChildren()
        self.Destroy()

    def Nomethod(self, event):
        '''Stub for testing'''
        pass
        event.Skip()

    def printmd(self):
        print 'self.disp_idx is now', self.disp_idx

class GMChoicebook(wx.Choicebook):
    '''This class creates a choicebook widget for the GIS Manager
    control panel. The choice book allows and controls an independent
    ayer tree, command console, and layer options for each display
    opened.'''
    def __init__(self, parent, id, pos, size, style):
        wx.Choicebook.__init__(self, parent, id, pos, size, style)

	# choicebook methods
	def OnCBPageChanged(self, event):
		old = event.GetOldSelection()
		new = event.GetSelection()
		sel = self.GetSelection()
		event.Skip()

	def OnCBPageChanging(self, event):
		old = event.GetOldSelection()
		new = event.GetSelection()
		sel = self.GetSelection()
		event.Skip()

	# notebook methods
	def changePage(self, pg, content, name):
		self.DeletePage(pg)
		self.InsertPage(pg, content, name)

	def getPage():
		pass


class SetVal:
	'''Class to store and set values needed by map, gism,
	and other modules. This should work but doesn't for some reason.'''

	def setMdFocus(self, mdnum=-1):
		#get the id number of map display that has the focus
		#and use it to set md
		global mdfocus
		if mdnum > -1:
			mdfocus = mdnum
		else:
			return mdfocus

	def getMdFocus(self):
		global mdfocus
		return mdfocus


class GMApp(wx.App):
	def OnInit(self):
##	  reexec_with_pythonw()
		wx.InitAllImageHandlers()
		mainframe = GMFrame(None, -1, "")
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








