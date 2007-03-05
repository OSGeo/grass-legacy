#!/usr/bin/env python
import sys
import os
import wx
import wx.lib.customtreectrl as CT
import wx.lib.flatnotebook as FN
import wx.stc
import wx.richtext

#try:
#    import subprocess
#except:
#    from compat import subprocess

import Gism
gmpath = Gism.__path__[0]
sys.path.append(gmpath)

import Gism.gismutils as gismutils
import Gism.mapdisp2 as mapdisp
import Gism.render as render
import Gism.menudata as menudata
import Gism.menuform as menuform
import Gism.grassenv as grassevn 


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
    (and other) commands, tree widget page for managing GIS map layers,'''
    def __init__(self, parent, id, title):
        self.parent = parent
        wx.Frame.__init__(self, parent=parent, id=-1, title=title, style=wx.DEFAULT_FRAME_STYLE)

        # creating widgets
        self.menubar = self.__createMenuBar()
        self.toolbar = self.__createToolBar()
        #self.panel = wx.Panel(self,-1, style= wx.EXPAND)
        self.sizer= wx.BoxSizer(wx.VERTICAL)
        #self.cmdsizer = wx.BoxSizer(wx.HORIZONTAL)
        self.displayNotebook = None # will be notebook for map displays
        self.notebook = self.__createNoteBook()
        self.cmdinput = self.__createCommandInput()
        
        # do layout
        self.SetTitle(_("GRASS GIS Manager - wxPython Prototype"))
        self.SetSize((450, 450))
        self.SetMinSize((100, 100))
        
        # create choicebook for GIS controls - one page for each display
        self.Centre()

        # initialize variables
        self.mapdisplays = {} #dictionary to index open map displays
        self.layertrees = {}
        self.disp_idx = 0 #index value for map displays and layer trees
        self.mapfocus = 0 #track which display currently has focus

        self.Bind(wx.EVT_CLOSE, self.onCloseWindow)
        self.Bind(wx.EVT_TEXT_ENTER, self.RunCmd, self.cmdinput)

        #item, proportion, flag, border, userData)
        self.sizer.Add(self.notebook,1, wx.EXPAND,  1)
        self.sizer.Add(self.cmdinput,0, wx.EXPAND, 1)
        self.SetSizer(self.sizer)
        #self.sizer.Fit(self)
        self.Layout()
        wx.CallAfter(self.notebook.SetSelection, 0)

        #start default initial display
        self.NewDisplay()

    def __createCommandInput(self):
        #l = wx.StaticText(self, -1, "GRASS> ")

        self.cmdinput = wx.TextCtrl(self, -1, "", )
        self.cmdinput.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.NORMAL, 0, "Monospace"))
        wx.CallAfter(self.cmdinput.SetInsertionPoint, 0)

        #self.cmdsizer.Add(l,0,wx.ADJUST_MINSIZE | wx.ALIGN_CENTER_VERTICAL, 1)
        #self.cmdsizer.Add(self.cmdinput, 0, wx.EXPAND, 0)

        return self.cmdinput

    def __createMenuBar(self):
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
        if not label:
            menu.AppendSeparator()
            return
        menuItem = menu.Append(-1, label, help, kind)
        if label:
            menucmd[label] = gcmd
        rhandler = eval(handler)
        self.Bind(wx.EVT_MENU, rhandler, menuItem)

    def RunCmd(self,event):
        #global gmpath
        print self.cmdinput.GetValue()
        #menuform.GUI().parseCommand(cmd, gmpath)

    def runMenuCmd(self, event):
        '''Run menu command'''
        menuitem = self.menubar.FindItemById(event.GetId())
        itemtext = menuitem.GetText()
        cmd = menucmd[itemtext]
        global gmpath
        menuform.GUI().parseCommand(cmd, gmpath)


    def __createNoteBook(self):

        bookStyle=FN.FNB_DEFAULT_STYLE #| FN.FNB_FANCY_TABS
        self.notebook = FN.FlatNotebook(self, wx.ID_ANY, style=bookStyle)
        bookStyle=FN.FNB_DEFAULT_STYLE 
        self.displayNotebook = FN.FlatNotebook(self, -1, style=bookStyle)
        self.notebook.AddPage(self.displayNotebook, "Displays")

        self.goutput =  wx.richtext.RichTextCtrl(self,style=wx.VSCROLL|wx.HSCROLL|wx.NO_BORDER)
        self.notebook.AddPage(self.goutput, "Output")
        #s = wx.TreeCtrl(self.notebook, -1, style=wx.SUNKEN_BORDER|wx.EXPAND)   
        #self.notebook.AddPage(page, "Layers")

        #self.choiceb = GMChoicebook(page,style=wx.CHB_TOP)

        #boxsizer = wx.BoxSizer()
        #boxsizer.Add(self.choiceb, 1, wx.EXPAND)
        #page.SetSizer(boxsizer)
        #page.SetAutoLayout(True)
        

        #for num in range(0, 5):
        #    page = wx.TextCtrl(self.notebook, -1, "This is page %d" % num ,
        #                       style=wx.TE_MULTILINE)
        #    self.notebook.AddPage(page, "Tab Number %d" % num)
        #    
        return self.notebook

    def __createLayerTree(self,parent=None):
        ctstyle = CT.TR_HIDE_ROOT
        ctrltree = CT.CustomTreeCtrl(parent, -1,style=ctstyle)
        ctrltree.AddRoot("Map Layers")

        return ctrltree

    def __createToolBar(self):
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

        return (
            ('newdisplay', os.path.join(gismutils.icons,'gui-startmon.gif'), 'Start new display', self.NewDisplay),
            ('', '', '', ''),
            ('addraster', os.path.join(gismutils.icons,'element-cell.gif'), 'Add raster layer', self.AddRaster),
            ('addvect', os.path.join(gismutils.icons,'element-vector.gif'), 'Add vector layer', self.AddVector),
            ('addcmd', os.path.join(gismutils.icons,'gui-cmd.gif'), 'Add command layer', self.AddCommand)
            )



    #---Start display---#000000#FFFFFF----------------------------------------------
    def NewDisplay(self, event=None):
        '''Create new map display widget'''
        #update display index
        #start a new display, indexed by disp_idx
        #mID = wx.NewId()
        self.mapdisplays[self.disp_idx] = mapdisp.MapFrame(self, 
                -1, pos=wx.DefaultPosition, size=wx.DefaultSize,
                style=wx.DEFAULT_FRAME_STYLE)
        self.mapdisplays[self.disp_idx].SetTitle(_("Map Display-"+str(self.disp_idx)))
        self.mapdisplays[self.disp_idx].Show()
        self.mapdisplays[self.disp_idx].Refresh()
        self.mapdisplays[self.disp_idx].Update()

        # add layer tree to display notebook
        self.layertrees[self.disp_idx] =  self.__createLayerTree(self.displayNotebook)

        self.displayNotebook.AddPage(self.layertrees[self.disp_idx], "Display %d" % self.disp_idx)

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

    def GetSelectedDisplay(self):
        return self.notebook.GetSelection()

    def SetTree(self, layertype):
        display = self.mapdisplays[self.GetSelectedDisplay()]
        layertree = self.layertrees[self.GetSelectedDisplay()]
        #add new layer to tree
        #layertree.AddLayer(disp_idx, layertype)
        return 

    #---Misc methods---#000000#FFFFFF-----------------------------------------------
    def onCloseWindow(self, event):
        '''Cleanup when gism.py is quit'''
        mdlist = range(0, self.disp_idx+1)
        try:
            for md in mdlist:
                if self.mapdisplays.has_key(md):
                    if self.mapdisplays[md].Close(False):
                        self.mapdisplays[md].Close(True)
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
        new = ""
        old = ""
        sel = ""

        self.Bind(wx.EVT_CHOICEBOOK_PAGE_CHANGED, self.OnCBPageChanged)


	# choicebook methods
    def OnCBPageChanged(self, event):
        old_pgnum = event.GetOldSelection()
        new = event.GetSelection()
        curr_pg = self.GetCurrentPage()
        sel_pgnum = self.GetSelection()
        #get ID of active display
        disp_idx = render.Track().GetCB_idx(str(curr_pg))
        #get associated display and make it active
        newdisp = render.Track().GetDisp(disp_idx)
        newdisp.SetFocus()
        newdisp.Raise()
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








