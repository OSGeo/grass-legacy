#!/usr/bin python

"""
MODULE:     wxgui.py

CLASSES:
    * GRasterDialog
    * GMFrame
    * GMApp

PURPOSE:    Main Python app for GRASS wxPython GUI. Main menu, layer management
            toolbar, notebook control for display management and access to
            command console.

AUTHORS:    The GRASS Development Team
            Michael Barton (Arizona State University) &
            Jachym Cepicky (Mendel University of Agriculture)
            Martin Landa

COPYRIGHT:  (C) 2006-2007 by the GRASS Development Team
            This program is free software under the GNU General Public
            License (>=v2). Read the file COPYING that comes with GRASS
            for details.

"""

import sys
import os
import time
import traceback
import types

import wx
import wx.aui
import wx.combo
import wx.html
import wx.stc
import wx.lib.customtreectrl as CT
import wx.lib.flatnotebook as FN
try:
    import subprocess
except:
    import compat.subprocess as subprocess

import gui_modules
gmpath = gui_modules.__path__[0]
sys.path.append(gmpath)

import images
imagepath = images.__path__[0]
sys.path.append(imagepath)

import icons
gmpath = icons.__path__[0]
sys.path.append(gmpath)

import gui_modules.wxgui_utils as wxgui_utils
import gui_modules.mapdisp as mapdisp
import gui_modules.menudata as menudata
import gui_modules.menuform as menuform
import gui_modules.grassenv as grassenv
import gui_modules.defaultfont as defaultfont
import gui_modules.histogram as histogram
import gui_modules.profile as profile
import gui_modules.rules as rules
import gui_modules.utils as utils
import gui_modules.cmd as cmd
import gui_modules.georect as georect

from   icons.icon import Icons as Icons
from   gui_modules.debug import Debug as Debug

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
    """
    GIS Manager frame with notebook widget for controlling
    GRASS GIS. Includes command console page for typing GRASS
    (and other) commands, tree widget page for managing GIS map layers.
    """
    def __init__(self, parent, id, title):
        self.parent = parent
        self.iconsize = (16, 16)
        wx.Frame.__init__(self, parent=parent, id=-1, title=title, style=wx.DEFAULT_FRAME_STYLE)

        self.CreateStatusBar()

        self._auimgr = wx.aui.AuiManager(self)

        # creating widgets
        self.notebook = self.__createNoteBook()

        self.cmdinput = self.__createCommandInput()
        self.cmdprompt = self.__createCommandPrompt()
        self.menubar = self.__createMenuBar()
        toolbar = self.__createToolBar()
        #self.panel = wx.Panel(self,-1, style= wx.EXPAND)
        self.sizer= wx.BoxSizer(wx.VERTICAL)
#        self.cmdsizer = wx.BoxSizer(wx.HORIZONTAL)

        # do layout
        self.SetTitle(_("GRASS Layer Manager"))
        self.SetMinSize((500, 400))
        self.SetIcon(wx.Icon(os.path.join(imagepath,'grass.smlogo.gif'), wx.BITMAP_TYPE_ANY))

        # set environmental variables
        os.environ["GRASS_RENDER_IMMEDIATE"] = "TRUE"

        # initialize variables
        self.mapdisplays = {} #dictionary to index open map displays
        self.disp_idx = 0 #index value for map displays and layer trees
        self.maptree = {} #dictionary to index a layer tree to accompanying a map display
        self.mapfocus = 0 #track which display currently has focus
        self.curr_page   = '' # currently selected page for layer tree notebook
        self.curr_pagenum = '' # currently selected page number for layer tree notebook
        self.encoding = 'ISO-8859-1' # default encoding for display fonts

        self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)
        self.Bind(wx.EVT_LEFT_DOWN, self.AddRaster)

        self._auimgr.AddPane(toolbar, wx.aui.AuiPaneInfo().ToolbarPane().
                              Top().Dockable(False).CloseButton(False).
                              DestroyOnClose(True).Row(0).Layer(0))
        self._auimgr.AddPane(self.notebook, wx.aui.AuiPaneInfo().
                              Left().CentrePane().BestSize((-1,-1)).Dockable(False).
                              CloseButton(False).DestroyOnClose(True).Row(1).Layer(0))
        self._auimgr.AddPane(self.cmdprompt, wx.aui.AuiPaneInfo().
                              Bottom().BestSize((-1,25)).Dockable(False).
                              CloseButton(False).DestroyOnClose(True).
                              PaneBorder(False).Row(2).Layer(0).Position(0).
                              Fixed().CaptionVisible(False).PinButton(False))
        self._auimgr.AddPane(self.cmdinput, wx.aui.AuiPaneInfo().
                              Bottom().BestSize((-1,25)).Dockable(False).
                              CloseButton(False).DestroyOnClose(True).
                              PaneBorder(False).Row(2).Layer(0).Position(1).
                              CaptionVisible(False))
        self._auimgr.Update()

        # item, proportion, flag, border, userData
#        self.sizer.Add(self.notebook, proportion=1, flag=wx.EXPAND, border=1)
#        self.sizer.Add(self.cmdinput, proportion=0, flag=wx.EXPAND, border=1)
#        self.SetSizer(self.sizer)
        self.sizer.Fit(self)
        self.Layout()
        wx.CallAfter(self.notebook.SetSelection, 0)

        # start default initial display
        self.NewDisplay()

    def __createCommandPrompt(self):
        """Creates command prompt"""
        self.cmdprompt = wx.StaticText(self, -1, "GRASS>")
        return self.cmdprompt

    def __createCommandInput(self):
        """Creates command input area"""
        self.cmdinput = wx.TextCtrl(self, id=wx.ID_ANY, value="", style=wx.HSCROLL | wx.TE_LINEWRAP |
                                    wx.TE_PROCESS_ENTER)

        self.cmdinput.SetFont(wx.Font(10, wx.FONTFAMILY_MODERN, wx.NORMAL, wx.NORMAL, 0, ''))
        wx.CallAfter(self.cmdinput.SetInsertionPoint, 0)

        self.Bind(wx.EVT_TEXT_ENTER, self.OnRunCmd, self.cmdinput)

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
        self.Bind(wx.EVT_MENU_HIGHLIGHT_ALL, self.OnMenuHighlight)
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
        nbStyle=FN.FNB_FANCY_TABS|FN.FNB_BOTTOM|FN.FNB_NO_X_BUTTON|FN.FNB_NO_NAV_BUTTONS
        self.notebook = FN.FlatNotebook(self, id=wx.ID_ANY, style=nbStyle)

        # create displays notebook widget and add it to main notebook page
        cbStyle=FN.FNB_VC8|FN.FNB_BACKGROUND_GRADIENT|FN.FNB_X_ON_TAB|FN.FNB_TABS_BORDER_SIMPLE
        self.gm_cb = FN.FlatNotebook(self, id=wx.ID_ANY, style=cbStyle)
        self.gm_cb.SetTabAreaColour(wx.Colour(125,200,175))
        self.notebook.AddPage(self.gm_cb, text="Map layers for each display")

        # create command output text area and add it to main notebook page
        self.goutput = wxgui_utils.GMConsole(self)
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

    def OnMenuHighlight(self, event):
        """
        Default menu help handler
        """
         # Show how to get menu item info from this event handler
        id = event.GetMenuId()
        item = self.GetMenuBar().FindItemById(id)
        if item:
            text = item.GetText()
            help = item.GetHelp()

        # but in this case just call Skip so the default is done
        event.Skip()

    def OnGeorectify(self, event):
        """
        Launch georectifier module
        """
        print 'launch georect'
        georect.GeorectWizard(self)

    def OnMapsets(self, event):
        """
        Launch mapset access dialog
        """
        dlg = MapsetAccess(self, wx.ID_ANY)

        dlg.CenterOnScreen()

        # if OK is pressed...
        if dlg.ShowModal() == wx.ID_OK:
            # create string of accessible mapsets
            ms_string = 'PERMANENT'
            if dlg.curr_mapset == 'PERMANENT':
                ms_string = 'PERMANENT'
            else:
                ms_string = 'PERMANENT,%s' % dlg.curr_mapset
            for mset in dlg.all_mapsets:
                    index = dlg.all_mapsets.index(mset)
                    if dlg.mapsetlb.IsChecked(index):
                        ms_string += ',%s' % mset

            # run g.mapsets with string of accessible mapsets
            cmdlist = ['g.mapsets', 'mapset=%s' % ms_string]
            cmd.Command(cmdlist)

    def OnRDigit(self, event):
        """
        Launch raster digitizing module
        """
        pass

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

    def OnRunCmd(self,event):
        """Run command"""

        #global gmpath
        cmd = self.cmdinput.GetValue()

        self.goutput.RunCmd(cmd)

    def GetMenuCmd(self, event):
        menuitem = self.menubar.FindItemById(event.GetId())
        itemtext = menuitem.GetText()
        cmd = menucmd[itemtext]
        return cmd

    def RunMenuCmd(self, event):
        cmd = self.GetMenuCmd(event)
        self.goutput.RunCmd(cmd)

    def OnMenuCmd(self, event):
        """Run menu command"""
        cmd = self.GetMenuCmd(event)
        global gmpath
        menuform.GUI().ParseCommand(cmd, parentframe=self)

    def RulesCmd(self, event):
        """
        Launches dialog for commands that need rules
        input and processes rules
        """
        command = self.GetMenuCmd(event)
        dlg = rules.RulesText(self, cmd=command)
        if dlg.ShowModal() == wx.ID_OK:
            gtemp = utils.GetTempfile()
            output = open(gtemp,"w")
            output.write(dlg.rules)
            output.close()

            if command == 'r.colors':
                cmdlist = [command,'map=%s' % dlg.inmap,'rules=%s' % gtemp,'--verbose']
            else:
                cmdlist = [command,'input=%s' % dlg.inmap,'output=%s' % dlg.outmap,'rules=%s' % gtemp]

            if dlg.overwrite == True:
                cmdlist.append('--o')

            cmdlist.append('--verbose')

            cmd.Command(cmdlist)

        dlg.Destroy()

    def OnXTerm(self, event):
        """
        Run commands that need interactive xmon
        """
        command = self.GetMenuCmd(event)

        # unset display mode
        del os.environ['GRASS_RENDER_IMMEDIATE']

        # open next available xmon
        xmonlist = []
        gisbase = os.environ['GISBASE']

        # make list of xmons that are not running
        cmdlist = ['d.mon', '-L']
        p = cmd.Command(cmdlist)
        output = p.module_stdout.read().split('\n')
        for outline in output:
            outline = outline.strip()
            if outline.startswith('x') and 'not running' in outline:

                xmonlist.append(outline[0:2])

        # open available xmon
        xmon = xmonlist[0]
        cmdlist = ['d.mon','start=%s' % xmon]
        p = subprocess.Popen(cmdlist)

        # run the command
        if 'OS' in os.environ and os.environ['OS'] == "Windows_NT":
            cmdlist = ['cmd.exe', '/c', 'start', os.path.join(gisbase,'etc','grass-run.bat'), command]
        else:
            cmdlist = [os.path.join(gisbase,'etc','grass-xterm-wrapper'), '-name', 'xterm-grass', '-e', os.path.join(gisbase,'etc','grass-run.sh'), command]
        cmd.Command(cmdlist)

        # reset display mode
        os.environ['GRASS_RENDER_IMMEDIATE'] = 'TRUE'

    def DefaultFont(self, event):
        """Set default font for GRASS displays"""

        dlg = defaultfont.SetDefaultFont(self, wx.ID_ANY, 'Select default display font',
                                   pos=wx.DefaultPosition, size=wx.DefaultSize,
                                   style=wx.DEFAULT_DIALOG_STYLE,
                                   encoding=self.encoding)
        if dlg.ShowModal() == wx.ID_CANCEL:
            dlg.Destroy()
            return

        # set default font type, font, and encoding to whatever selected in dialog

        if dlg.font != None:
            self.font = dlg.font
        if dlg.encoding != None:
            self.encoding = dlg.encoding

        dlg.Destroy()

        # set default font and encoding environmental variables
        os.environ["GRASS_FONT"] = self.font
        if self.encoding != None and self.encoding != "ISO-8859-1":
            os.environ["GRASS_ENCODING"] = self.encoding

    def DispHistogram(self, event):
        """
        Init histogram display canvas and tools
        """
        self.histogram = histogram.HistFrame(self,
                                           id=wx.ID_ANY, pos=wx.DefaultPosition, size=(400,300),
                                           style=wx.DEFAULT_FRAME_STYLE)

        #show new display
        self.histogram.Show()
        self.histogram.Refresh()
        self.histogram.Update()

    def DispProfile(self, event):
        """
        Init profile canvas and tools
        """
        self.profile = profile.ProfileFrame(self,
                                           id=wx.ID_ANY, pos=wx.DefaultPosition, size=(400,300),
                                           style=wx.DEFAULT_FRAME_STYLE)
        self.profile.Show()
        self.profile.Refresh()
        self.profile.Update()


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
                 ('newdisplay', Icons["newdisplay"].GetBitmap(), Icons["newdisplay"].GetLabel(), self.NewDisplay),
                 ('', '', '', ''),
                 ('addrast', Icons["addrast"].GetBitmap(), Icons["addrast"].GetLabel(), self.OnRaster),
                 ('addvect', Icons["addvect"].GetBitmap(), Icons["addvect"].GetLabel(), self.OnVector),
                 ('addcmd',  Icons["addcmd"].GetBitmap(),  Icons["addcmd"].GetLabel(),  self.AddCommand),
                 ('addgrp',  wx.ArtProvider.GetBitmap(wx.ART_FOLDER, wx.ART_TOOLBAR, (16,16)),  Icons["addgrp"].GetLabel(), self.AddGroup),
                 ('addovl',  Icons["addovl"].GetBitmap(),  Icons["addovl"].GetLabel(), self.OnOverlay),
                 ('addlabels',  Icons["addlabels"].GetBitmap(),  Icons["addlabels"].GetLabel(), self.AddLabels),
                 ('delcmd',  wx.ArtProvider.GetBitmap(wx.ART_DELETE, wx.ART_TOOLBAR, (16,16)), 'Delete selected layer', self.DeleteLayer),
                 ('', '', '', ''),
                 ('attrtable', Icons["attrtable"].GetBitmap(), Icons["attrtable"].GetLabel(), self.ShowAttributeTable)
                  )

    def ShowAttributeTable(self, event):
        """
        Show attribute table of the given vector map layer
        """
        layer = self.curr_page.maptree.layer_selected
        # no map layer selected
        if not layer:
            self.MsgNoLayerSelected()
            return

        # available only for vector map layers
        try:
            maptype = self.curr_page.maptree.layers[layer].maplayer.type
        except:
            maptype = None
        if not maptype or maptype != 'vector':
            dlg = wx.MessageDialog(self, _("Attribute management is available only for vector map layers"), _("Error"), wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return

        if not self.curr_page.maptree.GetPyData(layer):
            return
        dcmd = self.curr_page.maptree.GetPyData(layer)[0]
        if not dcmd:
            return

        mapname = map = mapset = size = icon = None

        for option in dcmd:
            if option.find('map') > -1:
                mapname = option.split('=')[1]
            elif option.find('size') > -1:
                size = option.split('=')[1]
            elif option.find('icon') > -1:
                icon = option.split('=')[1]

        pointdata = (icon, size)

        from gui_modules import dbm
        self.dbmanager = dbm.AttributeManager(parent=self, id=wx.ID_ANY, title="GRASS Attribute Table Manager: %s" % mapname,
                                              size=wx.Size(500,300), vectmap=mapname,
                                              pointdata=pointdata)

    def NewDisplay(self, event=None):
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
                                                       idx=self.disp_idx, gismgr=self, notebook=self.gm_cb,
                                                       auimgr=self._auimgr)

        # layout for controls
        cb_boxsizer = wx.BoxSizer(wx.VERTICAL)
        cb_boxsizer.Add(self.curr_page.maptree, proportion=1, flag=wx.EXPAND, border=1)
        self.curr_page.SetSizer(cb_boxsizer)
        cb_boxsizer.Fit(self.curr_page.maptree)
        self.curr_page.Layout()
        self.curr_page.maptree.Layout()

        self.disp_idx += 1

#        self._auimgr.SetManagedWindow(self.curr_page.maptree.testframe)
#
#        self._auimgr.AddPane(self.curr_page.maptree.testframe,
#                             wx.aui.AuiPaneInfo().Right().
#                             BestSize((-1,-1)).
#                             CloseButton(True).MinimizeButton(True).
#                             DestroyOnClose(True).Layer(2))
#
#        self._auimgr.Update()

    # toolBar button handlers
    def OnRaster(self, event):
        """Add raster menu"""
        point = wx.GetMousePosition()
        rastmenu = wx.Menu()
        # Add items to the menu
        addrast = wx.MenuItem(rastmenu, -1, Icons["addrast"].GetLabel())
        addrast.SetBitmap(Icons["addrast"].GetBitmap(self.iconsize))
        rastmenu.AppendItem(addrast)
        self.Bind(wx.EVT_MENU, self.AddRaster, addrast)

        addshaded = wx.MenuItem(rastmenu, -1, Icons ["addshaded"].GetLabel())
        addshaded.SetBitmap(Icons["addshaded"].GetBitmap (self.iconsize))
        rastmenu.AppendItem(addshaded)
        self.Bind(wx.EVT_MENU, self.AddShaded, addshaded)

        addrgb = wx.MenuItem(rastmenu, -1, Icons["addrgb"].GetLabel())
        addrgb.SetBitmap(Icons["addrgb"].GetBitmap(self.iconsize))
        rastmenu.AppendItem(addrgb)
        self.Bind(wx.EVT_MENU, self.AddRGB, addrgb)

        addhis = wx.MenuItem(rastmenu, -1, Icons ["addhis"].GetLabel())
        addhis.SetBitmap(Icons["addhis"].GetBitmap (self.iconsize))
        rastmenu.AppendItem(addhis)
        self.Bind(wx.EVT_MENU, self.AddHIS, addhis)

        addrastarrow = wx.MenuItem(rastmenu, -1, Icons ["addrarrow"].GetLabel())
        addrastarrow.SetBitmap(Icons["addrarrow"].GetBitmap (self.iconsize))
        rastmenu.AppendItem(addrastarrow)
        self.Bind(wx.EVT_MENU, self.AddRastarrow, addrastarrow)

        #        addrastnums = wx.MenuItem(rastmenu, -1, Icons ["addrnum"].GetLabel())
        #        addrastnums.SetBitmap(Icons["addrnum"].GetBitmap (self.iconsize))
        #        rastmenu.AppendItem(addrastnums)
        #        self.Bind(wx.EVT_MENU, self.AddRastnum, addrastnums)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(rastmenu)
        rastmenu.Destroy()

    def OnVector(self, event):
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

    def OnOverlay(self, event):
        """Add overlay menu"""
        point = wx.GetMousePosition()
        ovlmenu = wx.Menu()

        addgrid = wx.MenuItem(ovlmenu, -1, Icons["addgrid"].GetLabel())
        addgrid.SetBitmap(Icons["addgrid"].GetBitmap(self.iconsize))
        ovlmenu.AppendItem(addgrid)
        self.Bind(wx.EVT_MENU, self.AddGrid, addgrid)

        addgeodesic = wx.MenuItem(ovlmenu, -1, Icons["addgeodesic"].GetLabel())
        addgeodesic.SetBitmap(Icons["addgeodesic"].GetBitmap(self.iconsize))
        ovlmenu.AppendItem(addgeodesic)
        self.Bind(wx.EVT_MENU, self.AddGeodesic, addgeodesic)

        addrhumb = wx.MenuItem(ovlmenu, -1, Icons["addrhumb"].GetLabel())
        addrhumb.SetBitmap(Icons["addrhumb"].GetBitmap(self.iconsize))
        ovlmenu.AppendItem(addrhumb)
        self.Bind(wx.EVT_MENU, self.AddRhumb, addrhumb)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(ovlmenu)
        ovlmenu.Destroy()

    def AddRaster(self, event):
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('raster')

    def AddRGB(self, event):
        """Add RGB layer"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('rgb')

    def AddHIS(self, event):
        """Add HIS layer"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('his')

    def AddShaded(self, event):
        """Add shaded relief map layer"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('shaded')

    def AddRastarrow(self, event):
        """Add raster flow arrows map"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('rastarrow')

    def AddRastnum(self, event):
        """Add raster map with cell numbers"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('rastnum')

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

    def AddCommand(self, event):
        """Add command line layer"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('command')

    def AddGroup(self, event):
        """Add layer group"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('group')

    def AddGrid(self, event):
        """Add layer grid"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('grid')

    def AddGeodesic(self, event):
        """Add layer geodesic"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('geodesic')

    def AddRhumb(self, event):
        """Add layer rhumb"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('rhumb')

    def AddLabels(self, event):
        """Add layer vector labels"""
        self.notebook.SetSelection(0)
        self.curr_page.maptree.AddLayer('labels')

    def GetSelectedDisplay(self):
        return self.notebook.GetSelection()

    def DeleteLayer(self, event):
        """
        Delete selected map display layer in GIS Manager tree widget
        """

        if not self.curr_page.maptree.GetSelections():
            self.MsgNoLayerSelected()
            return

        layerName = str(self.curr_page.maptree.GetItemText(self.curr_page.maptree.layer_selected))
        if layerName:
            message = _("Are you sure you want delete layer <" + layerName + ">?")
        else:
            message = _("Are you sure you want delete this layer?")

        dlg = wx.MessageDialog (parent=self, message=message,
                                caption=_("Delete layer"),
                                style=wx.YES_NO | wx.NO_DEFAULT | wx.CANCEL | wx.ICON_QUESTION)

        if dlg.ShowModal() in [wx.ID_NO, wx.ID_CANCEL]:
            dlg.Destroy()
            return

        dlg.Destroy()

        for layer in self.curr_page.maptree.GetSelections():
            if self.curr_page.maptree.layers[layer].type == 'group':
                self.curr_page.maptree.DeleteChildren(layer)
            self.curr_page.maptree.Delete(layer)

    #Misc methods
    def OnCloseWindow(self, event):
        """Cleanup when wxgui.py is quit"""
        try:
            for page in range(self.gm_cb.GetPageCount()):
                self.gm_cb.GetPage(page).maptree.Map.Clean()
            self.DeleteAllPages()
        except:
            pass
#            self.DestroyChildren()
        self._auimgr.UnInit()
        self.Destroy()

    def Nomethod(self, event):
        '''Stub for testing'''
        pass
        event.Skip()

    def MsgNoLayerSelected(self):
        """Show dialog message 'No layer selected'"""
        dlg = wx.MessageDialog(self, _("No layer selected"), _("Error"), wx.OK | wx.ICON_ERROR)
        dlg.ShowModal()
        dlg.Destroy()

class MapsetAccess(wx.Dialog):
    """
    Controls setting options and displaying/hiding map overlay decorations
    """
    def __init__(self, parent, id, title=_('Set/unset access to mapsets in current location'),
                           pos=wx.DefaultPosition, size=(-1,-1),
                           style=wx.DEFAULT_DIALOG_STYLE|wx.RESIZE_BORDER):
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)

        cmdlist = ['g.mapsets', '-l']
        self.all_mapsets = cmd.Command(cmdlist).module_stdout.read().strip().split(' ')
        for mset in self.all_mapsets:
            indx = self.all_mapsets.index(mset)
            self.all_mapsets[indx] = mset.strip('\n')

        cmdlist = ['g.mapsets', '-p']
        self.accessible_mapsets = cmd.Command(cmdlist).module_stdout.read().strip().split(' ')
        for mset in self.accessible_mapsets:
            indx = self.accessible_mapsets.index(mset)
            self.accessible_mapsets[indx] = mset.strip('\n')

        cmdlist = ['g.gisenv', 'get=MAPSET']
        self.curr_mapset = cmd.Command(cmdlist).module_stdout.read().strip()


        # remove PERMANENT and current mapset from list because they are always accessible
        self.PERMANENT_ndx = self.all_mapsets.index("PERMANENT")
        self.all_mapsets.pop(self.PERMANENT_ndx)
        if self.curr_mapset != "PERMANENT":
            self.curr_ndx = self.all_mapsets.index(self.curr_mapset)
            self.all_mapsets.pop(self.curr_ndx)

        # make a checklistbox from available mapsets and check those that are active
        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        if len(self.all_mapsets) == 0:
            label = wx.StaticText(self, -1, "No other accessible mapsets besides \
                                \nPERMANENT and current mapset.",
                                style=wx.ALIGN_CENTRE)
        else:
            label = wx.StaticText(self, -1, "Check mapset to make it accessible, uncheck it to hide it.\
                                \nPERMANENT and current mapset are always accessible.",
                                style=wx.ALIGN_CENTRE)
        box.Add(label, 0, wx.ALIGN_CENTRE)
        sizer.Add(box, 0, wx.ALIGN_CENTRE|wx.TOP|wx.BOTTOM, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        self.mapsetlb = wx.CheckListBox(self, -1, pos=wx.DefaultPosition,
                                        size=(350,200), choices=self.all_mapsets)
        box.Add(self.mapsetlb, 0, wx.ALIGN_CENTRE)
        sizer.Add(box, 0, wx.ALIGN_CENTRE|wx.TOP|wx.BOTTOM, 5)

        # check all accessible mapsets
        for mset in self.accessible_mapsets:
            if mset != 'PERMANENT' and mset != self.curr_mapset:
                self.mapsetlb.Check(self.all_mapsets.index(mset),True)

        # dialog buttons
        line = wx.StaticLine(self, -1, size=(-1,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.EXPAND|wx.ALIGN_CENTRE|wx.TOP|wx.BOTTOM, 5)

        btnsizer = wx.StdDialogButtonSizer()

        okbtn = wx.Button(self, wx.ID_OK)
        okbtn.SetDefault()
        btnsizer.AddButton(okbtn)

        cancelbtn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(cancelbtn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.EXPAND|wx.ALIGN_RIGHT|wx.ALL, 5)

        self.Layout()
        self.SetSizer(sizer)

        sizer.Fit(self)


class GMApp(wx.App):
    """
    GMApp class
    """
    def OnInit(self):
        # reexec_with_pythonw()

        # initialize all available image handlers
        wx.InitAllImageHandlers()

        # create splash screen
        introImagePath = os.path.join(os.getenv("GISBASE"), "etc", "gm", "intro.gif")
        introImage     = wx.Image(introImagePath, wx.BITMAP_TYPE_GIF)
        introBmp       = introImage.ConvertToBitmap()
        wx.SplashScreen (bitmap=introBmp, splashStyle=wx.SPLASH_CENTRE_ON_SCREEN | wx.SPLASH_TIMEOUT,
                         milliseconds=1500, parent=None, id=wx.ID_ANY)
        wx.Yield()

        # create and show main frame
        mainframe = GMFrame(parent=None, id=wx.ID_ANY, title="")

        mainframe.Show()
        self.SetTopWindow(mainframe)

        return True

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
