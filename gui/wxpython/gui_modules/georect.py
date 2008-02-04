"""
MODULE:    georect.py

CLASSES:
    * Georectify
    * GCP
    * GRMap

PURPOSE:   Georectification module for GRASS GIS. Includes ground control
            point management and interactive point and click GCP creation

AUTHORS:   The GRASS Development Team
           Michael Barton

COPYRIGHT: (C) 2006-2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

# recheck once completed to see how many of these are still needed
import os
import sys
import time
import glob
import math
import tempfile
import shutil

try:
    import subprocess # Not needed if GRASS commands could actually be quiet
except:
    CompatPath = os.path.join( os.getenv("GISBASE"),"etc","wx")
    sys.path.append(CompatPath)
    from compat import subprocess

import wx
import wx.aui
import wx.lib.filebrowsebutton as filebrowse
from wx.lib.mixins.listctrl import CheckListCtrlMixin, ListCtrlAutoWidthMixin
import wx.wizard as wiz
import wx.grid as gridlib

from threading import Thread

try:
    import subprocess
except:
    CompatPath = os.path.join( os.getenv("GISBASE"),"etc","wx")
    sys.path.append(CompatPath)
    from compat import subprocess

gmpath = os.path.join( os.getenv("GISBASE"),"etc","wx","gui_modules" )
sys.path.append(gmpath)
gmpath = os.path.join( os.getenv("GISBASE"),"etc","wx","icons" )
sys.path.append(gmpath)

import mapdisp
import render
import toolbars
import menuform
import gselect
import disp_print
import gcmd
import utils
from debug import Debug as Debug
from icon import Icons as Icons

import images
imagepath = images.__path__[0]
sys.path.append(imagepath)

# global variables
global grassdatabase
global xy_group
global xy_map

global maptype

xy_group = ''
xy_map = ''
maptype = 'cell'


class TitledPage(wiz.WizardPageSimple):
    """
    Class to make wizard pages. Generic methods to make
    labels, text entries, and buttons.
    """
    def __init__(self, parent, title):
        wiz.WizardPageSimple.__init__(self, parent)

        self.title = wx.StaticText(self,-1,title)
        self.title.SetFont(wx.Font(13, wx.SWISS, wx.NORMAL, wx.BOLD))
        self.sizer = wx.BoxSizer(wx.VERTICAL)

        tmpsizer = wx.BoxSizer(wx.VERTICAL)

        tmpsizer.Add(self.title, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        tmpsizer.AddSpacer(10)
        tmpsizer.Add(wx.StaticLine(self, -1), 0, wx.EXPAND|wx.ALL, 0)
        tmpsizer.Add(self.sizer, wx.EXPAND)

        self.SetSizer(tmpsizer)
        self.SetAutoLayout(True)
        tmpsizer.Fit(self)

class GeorectWizard(object):
    """
    Start wizard here and finish wizard here
    """

    def __init__(self, parent):
        wizbmp = wx.Image(os.path.join(os.getenv("GISBASE"),"etc","wx","images","wizard.png"), wx.BITMAP_TYPE_PNG)
        wizbmp.Rescale(250,600)
        wizbmp = wizbmp.ConvertToBitmap()

        self.parent = parent

        #set environmental variables
        cmdlist = ['g.gisenv', 'get=GISDBASE']
        #global grassdatabase
        p = gcmd.Command(cmdlist)
        self.grassdatabase = p.ReadStdOutput()[0]
        
        # read original environment settings
        #self.orig_env = os.environ.copy()
        self.orig_gisrc = os.environ['GISRC']
        f = open(self.orig_gisrc)
        self.gisrc_dict = {}
        try:
            for line in f:
                line = line.strip('/n')
                self.gisrc_dict[line.split(':')[0]] = line.split(':')[1].strip()
        finally:
            f.close()
        self.new_gisrc = '' #GISRC file for source location/mapset of map(s) to georectify
        
        # define wizard pages
        self.wizard = wiz.Wizard(parent, -1, "Setup for georectification")
        self.startpage = LocationPage(self.wizard, self)
        self.grouppage = GroupPage(self.wizard, self)
        self.mappage = DispMapPage(self.wizard, self)

        # Set the initial order of the pages
        self.startpage.SetNext(self.grouppage)

        self.grouppage.SetPrev(self.startpage)
        self.grouppage.SetNext(self.mappage)

        self.mappage.SetPrev(self.grouppage)

        self.wizard.FitToPage(self.startpage)

#        self.Bind(wx.EVT_CLOSE,    self.OnCloseWindow)

        success = False

        if self.wizard.RunWizard(self.startpage):
            success = self.onWizFinished()
            if success == True:
                pass
            else:
                wx.MessageBox("Georectifying setup canceled.")
                self.Cleanup()
        else:
            wx.MessageBox("Georectifying setup canceled.")
            self.Cleanup()

        # start display showing xymap - need to put an if statement here
        if success != False:
            self.Map = render.Map()    # instance of render.Map to be associated with display
    
            global maptype
            global xy_map
    
            if maptype == 'cell':
                rendertype = 'raster'
                cmdlist = ['d.rast', 'map=%s' % xy_map]
            elif maptype == 'vector':
                rendertype = 'vector'
                cmdlist = ['d.vect', 'map=%s' % xy_map]
    
            self.Map.AddLayer(type=rendertype, command=cmdlist,l_active=True,
                              l_hidden=False, l_opacity=1, l_render=False)
                
            self.xy_mapdisp = mapdisp.MapFrame(self.parent, title="Set ground control points (GCPs)",
                 pos=wx.DefaultPosition, size=(640,480),
                 style=wx.DEFAULT_FRAME_STYLE, toolbars=["georect"],
                 Map=self.Map, gismgr=self.parent, georect=True)
            
            self.mapwin = self.xy_mapdisp.MapWindow
            
            # set mouse characteristics
            self.mapwin.mouse['box'] = 'point'
            self.mapwin.mouse["use"] == "pointer"
            self.mapwin.zoomtype = 0
            self.mapwin.pen = wx.Pen(colour='black', width=2, style=wx.SOLID)
            self.mapwin.SetCursor(self.xy_mapdisp.cursors["cross"])
            
            # draw selected xy map
            self.xy_mapdisp.MapWindow.UpdateMap()

            #show new display
            self.xy_mapdisp.Show()
            self.xy_mapdisp.Refresh()
            self.xy_mapdisp.Update()
    
            # start GCP form
            self.gcpmgr = GCP(self.parent)
            self.gcpmgr.Show()
            self.gcpmgr.Refresh()
            self.gcpmgr.Update()
        else:
            self.Cleanup()
                    
    def PrintCoord(self, coordtype, coord):
        print coord,coordtype
        self.gcpmgr.SetGCPData(coordtype, coord)
        
    def SetSrcEnv(self, location, mapset):
        """Create environment to use for location and mapset
        that are the source of the file(s) to georectify"""
        
        self.gisrc_dict['LOCATION_NAME'] = location
        self.gisrc_dict['MAPSET'] = mapset
        self.new_gisrc = utils.GetTempfile()     
        f = open(self.new_gisrc, mode='w')        
        for line in self.gisrc_dict.items():
            f.write(line[0]+": "+line[1]+"\n")
        f.close()

    def SwitchEnv(self, grc):
        """Switches between original working location/mapset and
        location/mapset that is source of file(s) to georectify"""
        print 'switch = ',grc
        if grc == 'original':
            os.environ["GISRC"] = str(self.orig_gisrc)
        elif grc == 'new':
            os.environ["GISRC"] = str(self.new_gisrc)
        

    def onWizFinished(self):
        global grassdatabase
        global xy_group
        global xy_map
        global maptype

        #print 'Current global variables'
        #print 'curr_location=',curr_location
        #print 'curr_mapset=',curr_mapset
        #print 'xy_location=',xy_location
        #print 'xy_mapset=',xy_mapset
        #print 'xy_group=',xy_group
        #print 'xy_map=',xy_map
        #print 'maptype=',maptype

        return True
        self.Cleanup()

    def Cleanup(self):
        # return to current location and mapset
        self.SwitchEnv('original')
        self.parent.georectifying = False
        self.xy_mapdisp.Destroy()
        self.wizard.Destroy()

class LocationPage(TitledPage):
    """
    Set map type (raster or vector) to georectify and
    select location/mapset of map(s) to georectify.
    """
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, "Select map type and location/mapset")

        self.parent = parent
        self.grassdatabase = self.parent.grassdatabase
        
        self.xylocation = ''
        self.xymapset = ''

        tmplist = os.listdir(self.grassdatabase)

        self.locList = []

        # Create a list of valid locations
        for item in tmplist:
            if os.path.isdir(os.path.join(self.grassdatabase,item)) and \
                os.path.exists(os.path.join(self.grassdatabase,item,'PERMANENT')):
                self.locList.append(item)

        self.mapsetList = []

        box = wx.BoxSizer(wx.HORIZONTAL)
        self.rb_maptype = wx.RadioBox(self, -1, "Map type to georectify",
                                   wx.DefaultPosition, wx.DefaultSize,
                                   ['raster','vector'], 2, wx.RA_SPECIFY_COLS)
        box.Add(self.rb_maptype, 0, wx.ALIGN_CENTER|wx.ALL, 5)
        self.sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, 'select location:',
                style=wx.ALIGN_RIGHT)
        box.Add(label, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        self.cb_location = wx.ComboBox(self, wx.ID_ANY, "",
                                     wx.DefaultPosition,
                                     wx.DefaultSize,
                                     choices = self.locList,
                                     style=wx.CB_DROPDOWN|wx.CB_READONLY)
        box.Add(self.cb_location, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        self.sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, 'select mapset:',
                style=wx.ALIGN_RIGHT)
        box.Add(label, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        self.cb_mapset = wx.ComboBox(self, wx.ID_ANY, "",
                                     wx.DefaultPosition,
                                     wx.DefaultSize,
                                     choices = self.mapsetList,
                                     style=wx.CB_DROPDOWN|wx.CB_READONLY)
        box.Add(self.cb_mapset, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        self.sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.Bind(wx.EVT_RADIOBOX, self.OnMaptype, self.rb_maptype)
        self.Bind(wx.EVT_COMBOBOX, self.OnLocation, self.cb_location)
        self.Bind(wx.EVT_COMBOBOX, self.OnMapset, self.cb_mapset)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.onPageChanging)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED, self.OnPageChanged)
        self.Bind(wx.EVT_CLOSE, self.parent.Cleanup)

    def OnMaptype(self,event):
        global maptype

        if event.GetInt() == 0:
            maptype = 'cell'
        elif event.GetInt() == 1:
            maptype = 'vector'

    def OnLocation(self, event):
        """Sets source location for map(s) to georectify"""

        self.xylocation = event.GetString()
        
        #create a list of valid mapsets
        tmplist = os.listdir(os.path.join(self.grassdatabase,self.xylocation))
        self.mapsetList = []
        for item in tmplist:
            if os.path.isdir(os.path.join(self.grassdatabase,self.xylocation,item)) and \
                os.path.exists(os.path.join(self.grassdatabase,self.xylocation,item,'WIND')):
                self.mapsetList.append(item)

        self.cb_mapset.SetItems(self.mapsetList)

    def OnMapset(self, event):
        """Sets source mapset for map(s) to georectify"""

        if self.xylocation == '':
            wx.MessageBox('You must select a valid location before selecting a mapset')
            return

        self.xymapset = event.GetString()

    def onPageChanging(self,event=None):

        if event.GetDirection() and (self.xylocation == '' or self.xymapset == ''):
            wx.MessageBox('You must select a valid location and mapset in order to continue')
            event.Veto()
            return
        else:
            self.parent.SetSrcEnv(self.xylocation, self.xymapset)

    def OnPageChanged(self,event=None):
        pass

class GroupPage(TitledPage):
    """
    Set group to georectify. Create group if desired.
    """
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, "Select image/map group to georectify")

        self.parent = parent
        self.groupList = []
        self.grassdatabase = self.parent.grassdatabase
        self.xylocation = ''
        self.xymapset = ''

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, 'select group:',
                style=wx.ALIGN_RIGHT)
        box.Add(label, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        self.cb_group = wx.ComboBox(self, wx.ID_ANY, "",
                                     wx.DefaultPosition,
                                     wx.DefaultSize,
                                     choices = self.groupList,
                                     style=wx.CB_DROPDOWN|wx.CB_READONLY)
        box.Add(self.cb_group, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        self.sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, 'Create group if none exists', style=wx.ALIGN_LEFT)
        box.Add(label, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        self.btn_mkgroup = wx.Button(self, wx.ID_ANY, "Make group ...")
        box.Add(self.btn_mkgroup, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        self.sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.Bind(wx.EVT_COMBOBOX, self.OnGroup, self.cb_group)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.onPageChanging)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED, self.OnPageChanged)
        self.Bind(wx.EVT_CLOSE, self.parent.Cleanup)

    def OnGroup(self, event):
        global xy_group
        
        xy_group = event.GetString()

    def onPageChanging(self,event=None):
        global xy_group

        if event.GetDirection() and xy_group == '':
            wx.MessageBox('You must select a valid image/map group in order to continue')
            event.Veto()
            return

    def OnPageChanged(self,event=None):

        self.groupList = []
        tmplist = []
        self.xylocation = self.parent.gisrc_dict['LOCATION_NAME']
        self.xymapset = self.parent.gisrc_dict['MAPSET']

        # create a list of groups in selected mapset
        tmplist = os.listdir(os.path.join(self.grassdatabase,self.xylocation,self.xymapset,'group'))

        if event.GetDirection() and xy_group == '':
            if tmplist == []:
                wx.MessageBox('No map/imagery groups exist to georectify. You will need to create one')
            else:
                for item in tmplist:
                    if os.path.isdir(os.path.join(self.grassdatabase,self.xylocation,self.xymapset,'group',item)):
                        self.groupList.append(item)
    
                self.cb_group.SetItems(self.groupList)
                
class DispMapPage(TitledPage):
    """
    Select ungeoreferenced map to display for interactively
    setting ground control points (GCPs).
    """
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, "Select image/map to display for ground control point (GCP) creation")

        self.parent = parent
        global maptype

        self.parent = parent

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, 'Select display image/map:', style=wx.ALIGN_LEFT)
        box.Add(label, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        self.selection = gselect.Select(self, id=wx.ID_ANY, size=(300,-1),
                                              type=maptype )
        box.Add(self.selection, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        self.sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.selection.Bind(wx.EVT_TEXT, self.OnSelection)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.onPageChanging)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED, self.OnPageChanged)
        self.Bind(wx.EVT_CLOSE, self.parent.Cleanup)

    def OnSelection(self,event):
        global xy_map

        xy_map = event.GetString()

    def onPageChanging(self,event=None):
        global xy_map

        if event.GetDirection() and xy_map == '':
            wx.MessageBox('You must select a valid image/map in order to continue')
            event.Veto()
            return

    def OnPageChanged(self,event=None):
        global maptype

        if event.GetDirection():
            # switch to xy location if coming into the page from preceding
            self.parent.SwitchEnv('new')
            self.selection.SetElementList(maptype)
        else:
            # switch back to current location if leaving the page
            self.parent.SwitchEnv('original')

class GCP(wx.Frame):
    """
    Manages ground control points for georectifying. Calculates RMS statics.
    Calls i.rectify or v.transform to georectify map.
    """

    def __init__(self,parent,id=-1,title="Create & manage ground control points"):
        wx.Frame.__init__(self, parent, id , title, size=(500,400))

        toolbar = self.__createToolBar()
        self.selected = 0 #gcp list item selected
        self.mapcoordlist = [(0000000.00,0000000.00,'')] #list map coords and ID of map display they came from

        p = wx.Panel(self, -1, style=0)

        self.sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        self.rb_grmethod = wx.RadioBox(p, -1, "Select rectification method for rasters ",
                                   wx.DefaultPosition, wx.DefaultSize,
                                   ['1st order','2nd order', '3rd order'], 3, wx.RA_SPECIFY_COLS)
        box.Add(self.rb_grmethod, 0, wx.ALIGN_CENTER|wx.ALL, 5)
        self.sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        # initialize list control for GCP management
        self.list = CheckListCtrl(p)
        self.sizer.Add(self.list, 1, wx.GROW|wx.ALL, 5)
        self.list.InsertColumn(0, 'use| X coord', width=120)
        self.list.InsertColumn(1, 'Y coord')
        self.list.InsertColumn(2, 'N coord')
        self.list.InsertColumn(3, 'E coord')
        self.list.InsertColumn(4, 'Forward error')
        self.list.InsertColumn(5, 'Backward error')

        i = ('0000000.00','0000000.00','0000000.00','0000000.00','0000.00','0000.00')

        index = self.list.InsertStringItem(sys.maxint, i[0])
        self.list.SetStringItem(index, 1, i[1])
        self.list.SetStringItem(index, 2, i[2])
        self.list.SetStringItem(index, 3, i[3])
        self.list.SetStringItem(index, 4, i[4])
        self.list.SetStringItem(index, 5, i[5])

        p.SetSizer(self.sizer)

        self.Bind(wx.EVT_RADIOBOX, self.OnGRMethod, self.rb_grmethod)
        self.list.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected)

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
                 ('savegcp', Icons["savefile"].GetBitmap(), Icons["savefile"].GetLabel(), self.SaveGCPs),
                 ('addgcp',  wx.ArtProvider.GetBitmap(wx.ART_NEW, wx.ART_TOOLBAR, (16,16)), 'Add new GCP', self.AddGCP),
                 ('deletegcp',  wx.ArtProvider.GetBitmap(wx.ART_DELETE, wx.ART_TOOLBAR, (16,16)), 'Delete selected GCP', self.DeleteGCP),
                 ('cleargcp', Icons["cleargcp"].GetBitmap(), Icons["cleargcp"].GetLabel(), self.ClearGCP),
                 ('refreshgcp', Icons["refreshgcp"].GetBitmap(), Icons["refreshgcp"].GetLabel(), self.RefreshGCPMarks),
                 ('rms', Icons["rms"].GetBitmap(), Icons["rms"].GetLabel(), self.OnRMS),
                 ('georect',  Icons["georect"].GetBitmap(),  Icons["georect"].GetLabel(),  self.OnGeorect),
                 ('quit',  wx.ArtProvider.GetBitmap(wx.ART_QUIT, wx.ART_TOOLBAR, (16,16)), 'Quit georectification module', self.OnQuit)
                  )

    def SaveGCPs(self, event):
        pass

    def DeleteGCP(self, event):
        self.list.DeleteItem(self.selected)
        del self.mapcoordlist[self.selected]

    def AddGCP(self, event):
        self.list.Append(['0000000.00','0000000.00','0000000.00','0000000.00','0000.00','0000.00'])
        self.mapcoordlist.append((0000000.00,0000000.00,''))
        
    def SetGCPData(self, coordtype, coord, mapdisp):
        index = self.selected
        coord0 = str(coord[0])
        coord1 = str(coord[1])

        if coordtype == 'gcpcoord':
            self.list.SetStringItem(index, 0, coord0)
            self.list.SetStringItem(index, 1, coord1)
        if coordtype == 'mapcoord':
            self.list.SetStringItem(index, 2, coord0)
            self.list.SetStringItem(index, 3, coord1)
            self.mapcoordlist[index] = [(coord[0], coord[1], mapdisp)]
        if coordtype == 'rms':
            self.list.SetStringItem(index, 4, coord0)
            self.list.SetStringItem(index, 5, coord1) 

    def ClearGCP(self, event):
        index = self.selected
        for i in range(6):
            self.list.SetStringItem(index, i, '0000000.00')
        self.mapcoordlist[index] = [(0000000.00,0000000.00,'')]

    def OnRMS(self, event):
        pass

    def OnGeorect(self, event):
        pass

    def OnQuit(self, event):
        self.Destroy()
        self.parent.Cleanup()

    def OnGRMethod(self, event):
        pass

    def OnItemSelected(self, event):
        self.selected = event.GetIndex()
        print 'item = ',self.selected
        
    def RefreshGCPMarks(self, event):
        for index in range(self.list.GetItemCount()):
            if self.list.IsChecked(index):
                coord0 = float(self.list.GetItem(index, 0).GetText())
                coord1 = float(self.list.GetItem(index, 1).GetText())
                coord2 = float(self.list.GetItem(index, 2).GetText())
                coord3 = float(self.list.GetItem(index, 3).GetText())
                print coord0,coord1,coord2,coord3,coord4
                self.mapcoordlist[2].MapWindow.DrawCross(pdc=self.pdcTmp, coords=(coord2,coord3),
                                       size=5)
                self.mapcoordlist[2].MapWindow.UpdateMap()
                self.parent.mapwin.DrawCross(pdc=self.pdcTmp, coords=(coord0,coord1),
                                       size=5)
                self.parent.mapwin.UpdateMap()

class CheckListCtrl(wx.ListCtrl, CheckListCtrlMixin, ListCtrlAutoWidthMixin):
    def __init__(self, parent):
        wx.ListCtrl.__init__(self, parent, -1, style=wx.LC_REPORT | wx.SUNKEN_BORDER)
        CheckListCtrlMixin.__init__(self)
        ListCtrlAutoWidthMixin.__init__(self)
        
        self.CheckList = [] # tracks whether list items are checked or not
        self.Bind(wx.EVT_LIST_ITEM_ACTIVATED, self.OnItemActivated)

    def OnItemActivated(self, event):
        self.ToggleItem(event.m_itemIndex)

    # this is called by the base class when an item is checked/unchecked
    def OnCheckItem(self, index, flag):
        pass

# These 2 grid classes are ones I tried to use instead of a list. Might want
# to rethink this later
class GCPGrid(gridlib.Grid):
    def __init__(self, parent):
        gridlib.Grid.__init__(self, parent, -1)
        table = GCPDateTable()
        # The second parameter means that the grid is to take ownership of the
        # table and will destroy it when done.  Otherwise you would need to keep
        # a reference to it and call it's Destroy method later.
        self.SetTable(table, True)
        self.SetRowLabelSize(10)
        self.SetMargins(0,0)
        self.AutoSizeColumns(True)
        gridlib.EVT_GRID_CELL_LEFT_DCLICK(self, self.OnLeftDClick)
    # I do this because I don't like the default behaviour of not starting the
    # cell editor on double clicks, but only a second click.
    def OnLeftDClick(self, evt):
        if self.CanEnableCellControl():
            self.EnableCellEditControl()

class GCPDateTable(gridlib.PyGridTableBase):
    def __init__(self):
        gridlib.PyGridTableBase.__init__(self)
        self.colLabels = ['Use', 'X Coord', 'Y Coord', 'East', 'North',
                          'Forward Error', 'Backward Error']
        self.dataTypes = [gridlib.GRID_VALUE_BOOL,
                          gridlib.GRID_VALUE_FLOAT + ':7,2',
                          gridlib.GRID_VALUE_FLOAT + ':7,2',
                          gridlib.GRID_VALUE_FLOAT + ':7,2',
                          gridlib.GRID_VALUE_FLOAT + ':7,2',
                          gridlib.GRID_VALUE_FLOAT + ':7,2',
                          gridlib.GRID_VALUE_FLOAT + ':7,2',
                          ]
        self.data = [
            [1, '0000000.00', '0000000.00', '0000000.00', '0000000.00', '0000000.00', '0000000.00'],
            [1, '0000000.00', '0000000.00', '0000000.00', '0000000.00', '0000000.00', '0000000.00'],
            [1, '0000000.00', '0000000.00', '0000000.00', '0000000.00', '0000000.00', '0000000.00']
            ]
    #--------------------------------------------------
    # required methods for the wxPyGridTableBase interface
    def GetNumberRows(self):
        return len(self.data) + 1
    def GetNumberCols(self):
        return len(self.data[0])
    def IsEmptyCell(self, row, col):
        try:
            return not self.data[row][col]
        except IndexError:
            return True
    # Get/Set values in the table.  The Python version of these
    # methods can handle any data-type, (as long as the Editor and
    # Renderer understands the type too,) not just strings as in the
    # C++ version.
    def GetValue(self, row, col):
        try:
            return self.data[row][col]
        except IndexError:
            return ''
    def SetValue(self, row, col, value):
        try:
            self.data[row][col] = value
        except IndexError:
            # add a new row
            self.data.append([''] * self.GetNumberCols())
            self.SetValue(row, col, value)
            # tell the grid we've added a row
            msg = gridlib.GridTableMessage(self,            # The table
                    gridlib.GRIDTABLE_NOTIFY_ROWS_APPENDED, # what we did to it
                    1                                       # how many
                    )
            self.GetView().ProcessTableMessage(msg)
    #--------------------------------------------------
    # Some optional methods
    # Called when the grid needs to display labels
    def GetColLabelValue(self, col):
        return self.colLabels[col]
    # Called to determine the kind of editor/renderer to use by
    # default, doesn't necessarily have to be the same type used
    # natively by the editor/renderer if they know how to convert.
    def GetTypeName(self, row, col):
        return self.dataTypes[col]
    # Called to determine how the data can be fetched and stored by the
    # editor and renderer.  This allows you to enforce some type-safety
    # in the grid.
    def CanGetValueAs(self, row, col, typeName):
        colType = self.dataTypes[col].split(':')[0]
        if typeName == colType:
            return True
        else:
            return False
    def CanSetValueAs(self, row, col, typeName):
        return self.CanGetValueAs(row, col, typeName)


