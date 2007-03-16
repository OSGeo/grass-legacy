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
import wx.lib.customtreectrl as CT
import wx.lib.flatnotebook as FN
import wx.stc
import wx.richtext

# try:
#    import subprocess
#except:
#    from compat import subprocess

import Gism
gmpath = Gism.__path__[0]
sys.path.append(gmpath)

import Gism.track as track
import Gism.gismutils as gismutils
import Gism.mapdisp as mapdisp
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
    (and other) commands, tree widget page for managing GIS map layers.'''
    def __init__(self, parent, id, title):
        self.parent = parent
        wx.Frame.__init__(self, parent=parent, id=-1, title=title, style=wx.DEFAULT_FRAME_STYLE)

        # creating widgets
        self.notebook = self.__createNoteBook()
        self.cmdinput = self.__createCommandInput()
        self.menubar = self.__createMenuBar()
        self.toolbar = self.__createToolBar()
        #self.panel = wx.Panel(self,-1, style= wx.EXPAND)
        self.sizer= wx.BoxSizer(wx.VERTICAL)
        self.cmdsizer = wx.BoxSizer(wx.HORIZONTAL)

        # do layout
        self.SetTitle(_("GRASS GIS Manager - wxPython Prototype"))
        self.SetMinSize((450, 450))
        # self.nb_panel = wx.Panel(self)

        # initialize variables
        self.mapdisplays = {} #dictionary to index open map displays
        self.disp_idx = 0 #index value for map displays and layer trees
        self.maptree = {} #dictionary to index a layer tree to accompanying a map display
        self.mapfocus = 0 #track which display currently has focus

        self.Bind(wx.EVT_CLOSE, self.onCloseWindow)

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
        
        #self.cmdinput.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.NORMAL, 0, "Monospace"))
        wx.CallAfter(self.cmdinput.SetInsertionPoint, 0)

        #self.cmdsizer.Add(l,0,wx.ADJUST_MINSIZE | wx.ALIGN_CENTER_VERTICAL, 1)
        #self.cmdsizer.Add(self.cmdinput, 0, wx.EXPAND, 0)

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
        self.goutput = gismutils.GMConsole(self)
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
        curr_pg   = self.gm_cb.GetCurrentPage()
        sel_pgnum = self.gm_cb.GetSelection()

        # get ID of associated display if more than one
        disp_idx = track.Track().GetDisp_idx(curr_pg)
        if disp_idx != None:
            #get associated display and make it active
            newdisp = track.Track().GetCtrls(disp_idx, 0)
            newdisp.SetFocus()
            newdisp.Raise()
        event.Skip()

    def onCBPageClosed(self, event):
        """Page of notebook closed"""
        
        curr_pg = self.gm_cb.GetCurrentPage()
        disp_idx = track.Track().GetDisp_idx(curr_pg)
        if disp_idx != None:
            #get associated display and make it active
            disp = track.Track().GetCtrls(disp_idx, 0)
            try:
                if self.mapdisplays.has_key(disp_idx):
                    if self.mapdisplays[disp_idx].Close(False):
                        self.mapdisplays[disp_idx].Close(True)
            except:
                pass

    def runCmd(self,event):
        """Run command"""
        
        #global gmpath
        print 'the command = ',self.cmdinput.GetValue()
        cmd = self.cmdinput.GetLineText(0)

        self.goutput.runCmd(cmd)
        #menuform.GUI().parseCommand(cmd, gmpath)

    def runMenuCmd(self, event):
        """Run menu command"""
        
        menuitem = self.menubar.FindItemById(event.GetId())
        itemtext = menuitem.GetText()
        cmd = menucmd[itemtext]
        global gmpath
        menuform.GUI().parseCommand(cmd, gmpath)

#    def __createLayerTree(self,parent=None):
#        ctstyle = CT.TR_HIDE_ROOT
#        ctrltree = CT.CustomTreeCtrl(parent, -1,style=ctstyle)
#        ctrltree.AddRoot("Map Layers")
#
#        return ctrltree

    def __createToolBar(self):
        """Creates toolbar"""
        
        toolbar = self.CreateToolBar()
        for each in self.toolbarData():
            self.addToolbarButton(toolbar, *each)
        toolbar.Realize()

    def addToolbarButton(self, toolbar, label, iconfile, help, handler):
        """Adds button to the given toolbar"""
        
        if not label:
            toolbar.AddSeparator()
            return
        icon = wx.Bitmap(iconfile, wx.BITMAP_TYPE_ANY)
        tool = toolbar.AddSimpleTool(-1, icon, label, help)
        self.Bind(wx.EVT_TOOL, handler, tool)

    def toolbarData(self):
        
        return (
            ('New display', os.path.join(gismutils.icons,'gui-startmon.gif'), 'Start new display', self.newDisplay),
            ('', '', '', ''),
            ('Add raster layer', os.path.join(gismutils.icons,'element-cell.gif'), 'Add raster layer', self.addRaster),
            ('Add vector layer', os.path.join(gismutils.icons,'element-vector.gif'), 'Add vector layer', self.addVector),
            ('Add command layer', os.path.join(gismutils.icons,'gui-cmd.gif'), 'Add command layer', self.addCommand)
            )

    def newDisplay(self, event=None):
        """Create new map display frame"""

        newdisp = self.mapdisplays[self.disp_idx] = mapdisp.MapFrame(self,
                                                                     id=wx.ID_ANY, pos=wx.DefaultPosition, size=wx.DefaultSize,
                                                                     style=wx.DEFAULT_FRAME_STYLE,
                                                                     cb=self.gm_cb, idx=self.disp_idx)
        # title
        newdisp.SetTitle(_("Map Display " + str(self.disp_idx)))
        #self.maptree[self.disp_idx] = self.mapdisplays[self.disp_idx].getTree()

        #add notebook page to GIS Manager

        # make a new page in the bookcontrol for the layer tree (on page 0 of the notebook)
        self.pg_panel = wx.Panel(self.gm_cb, id=wx.ID_ANY, style= wx.EXPAND)
        self.gm_cb.AddPage(self.pg_panel, text="Display "+ str(self.disp_idx), select = True)
        self.cb_page = self.gm_cb.GetCurrentPage()

        # create layer tree (tree control for managing GIS layers)  and put on new notebook page
        self.maptree = gismutils.LayerTree(self.cb_page, id=wx.ID_ANY, pos=wx.DefaultPosition,
                                           size=wx.DefaultSize, ctstyle=wx.TR_HAS_BUTTONS
                                           |wx.TR_LINES_AT_ROOT|wx.TR_EDIT_LABELS|wx.TR_HIDE_ROOT
                                           |wx.TR_DEFAULT_STYLE|wx.NO_BORDER|wx.FULL_REPAINT_ON_RESIZE)

        # layout for controls
        cb_boxsizer = wx.BoxSizer(wx.VERTICAL)
        cb_boxsizer.Add(self.maptree, proportion=1, flag=wx.EXPAND, border=1)
        self.cb_page.SetSizer(cb_boxsizer)
        cb_boxsizer.Fit(self.cb_page)
        self.cb_page.Layout()
        #self.cb_page.SetAutoLayout(True)
        #self.Centre()

        # store information about display and associated controls in a dictionary in track.py
        track.Track().SetDisp(self.disp_idx,self)
        track.Track().SetCtrlDict(self.disp_idx, newdisp, self.cb_page, self.maptree)

        #show new display
        self.mapdisplays[self.disp_idx].Show()
        self.mapdisplays[self.disp_idx].Refresh()
        self.mapdisplays[self.disp_idx].Update()

        self.disp_idx += 1

    # toolBar button handlers
    def addRaster(self, event):
        """Add raster layer"""
        self.SetTree('rast')
        event.Skip()

    def addVector(self, event):
        """Add vector layer"""
        self.SetTree('vect')
        event.Skip()

    def addCommand(self, event):
        """Add command line layer"""
        self.SetTree('cmd')
        event.Skip()

    def GetSelectedDisplay(self):
        return self.notebook.GetSelection()

    def SetTree(self, layertype):
        #get ID of active display
        curr_pg = self.gm_cb.GetCurrentPage()
        disp_idx = track.Track().GetDisp_idx(curr_pg)
        if disp_idx != None:
            #get layer tree for active display
            layertree = track.Track().GetCtrls(disp_idx, 2)
        layertree.AddLayer(disp_idx, layertype)

    #Misc methods
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

class SetVal:
    """
    Class to store and set values needed by map, gism,
    and other modules. This should work but doesn't for some reason.
    """

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
    """
    GMApp class
    """
    def OnInit(self):
##	  reexec_with_pythonw()
        # initialize all available image handlers
        wx.InitAllImageHandlers()
        # create and show main frame
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
