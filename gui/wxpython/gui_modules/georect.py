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
import wx.wizard as wiz
import wx.lib.rcsizer  as rcs
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
import grassenv
import menuform
import select
import disp_print
import cmd
from debug import Debug as Debug
from icon import Icons as Icons

import images
imagepath = images.__path__[0]
sys.path.append(imagepath)

# global variables
global grassdatabase
global curr_location
global curr_mapset

global xy_location
global xy_mapset
global xy_group
global xy_map

global maptype

xy_location = ''
xy_mapset = ''
xy_group = ''
xy_map = ''
maptype = 'cell'

class Georectify(object):
    """
    Init class for georectifying. Launches startup dialog
    for setting georectifying parameters.
    """
    def __init__(self,parent):

        self.parent = parent

        # launch the startup dialog
        dlg = GeorectStart(self.parent)

        dlg.CenterOnScreen()
        dlg.ShowModal()

        # If OK button pressed in decoration control dialog
#        if dlg.ShowModal() == wx.ID_OK:
#            # go on to GCP management
#            pass




        # starup dialog calls GCP which

    def StartUp(self):
        pass

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
        global grassdatabase
        grassdatabase = cmd.Command(cmdlist).module_stdout.read().strip()

        cmdlist = ['g.gisenv', 'get=LOCATION_NAME']
        global curr_location
        curr_location = cmd.Command(cmdlist).module_stdout.read().strip()

        cmdlist = ['g.gisenv', 'get=MAPSET']
        global curr_mapset
        curr_mapset = cmd.Command(cmdlist).module_stdout.read().strip()

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
        else:
            wx.MessageBox("Georectifying setup canceled.")

        # start display showing xymap
        self.Map = render.Map()    # instance of render.Map to be associated with display

        self.xy_mapdisp = mapdisp.MapFrame(self.parent, title="Set ground control points (GCPs)",
                 pos=wx.DefaultPosition, size=wx.DefaultSize,
                 style=wx.DEFAULT_FRAME_STYLE, toolbars=["georect"],
                 Map=self.Map)

        #show new display
        self.xy_mapdisp.Show()
        self.xy_mapdisp.Refresh()
        self.xy_mapdisp.Update()


        # start GCP form

        self.Cleanup()

    def SwitchLocMapset(self, location, mapset):

        cmdlist = ['g.gisenv', 'set=LOCATION_NAME=%s' % location]
        cmd.Command(cmdlist)

        cmdlist = ['g.gisenv', 'set=MAPSET=%s' % mapset]
        cmd.Command(cmdlist)

    def onWizFinished(self):
        global grassdatabase
        global curr_location
        global curr_mapset
        global xy_location
        global xy_mapset
        global xy_group
        global xy_map
        global maptype

        print 'Current global variables'
        print 'curr_location=',curr_location
        print 'curr_mapset=',curr_mapset
        print 'xy_location=',xy_location
        print 'xy_mapset=',xy_mapset
        print 'xy_group=',xy_group
        print 'xy_map=',xy_map
        print 'maptype=',maptype

        return True

    def Cleanup(self):
        # return to current location and mapset
        global curr_location
        self.SwitchLocMapset(curr_location, curr_mapset)
        self.wizard.Destroy()

class LocationPage(TitledPage):
    """
    Set map type (raster or vector) to georectify and
    select location/mapset of map(s) to georectify.
    """
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, "Select map type and location/mapset")

        self.parent = parent
        global grassdatabase
        global xy_location
        global xy_mapset

        tmplist = os.listdir(grassdatabase)

        self.locList = []

        # Create a list of valid locations
        for item in tmplist:
            if os.path.isdir(os.path.join(grassdatabase,item)) and \
                os.path.exists(os.path.join(grassdatabase,item,'PERMANENT')):
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
        global grassdatabase
        global xy_location

        xy_location = event.GetString()
        tmplist = os.listdir(os.path.join(grassdatabase,xy_location))
        self.mapsetList = []
        for item in tmplist:
            if os.path.isdir(os.path.join(grassdatabase,xy_location,item)):
                self.mapsetList.append(item)

        self.cb_mapset.SetItems(self.mapsetList)

    def OnMapset(self, event):
        global xy_mapset
        global xy_location

        if xy_location == '':
            wx.MessageBox('You must select a valid location before selecting a mapset')
            return

        xy_mapset = event.GetString()

    def onPageChanging(self,event=None):
        global xy_location
        global xy_mapset

        if event.GetDirection() and (xy_location == '' or xy_mapset == ''):
            wx.MessageBox('You must select a valid location and mapset in order to continue')
            event.Veto()
            return

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
        global xy_location
        global xy_mapset
        global xy_group

        if xy_location == '' or xy_mapset == '':
            wx.MessageBox('You must select a valid location and mapset before selecting a group')
            return

        xy_group = event.GetString()

    def onPageChanging(self,event=None):
        global xy_group

        if event.GetDirection() and xy_group == '':
            wx.MessageBox('You must select a valid image/map group in order to continue')
            event.Veto()
            return

    def OnPageChanged(self,event=None):
        global grassdatabase
        global xy_location
        global xy_mapset

        self.groupList = []
        tmplist = []
        tmplist = os.listdir(os.path.join(grassdatabase,xy_location,xy_mapset,'group'))\

        if tmplist == []:
            wx.MessageBox('No map/imagery groups exist to georectify. You will need to create one')
        else:
            for item in tmplist:
                if os.path.isdir(os.path.join(grassdatabase,xy_location,xy_mapset,'group',item)):
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
        self.selection = select.Select(self, id=wx.ID_ANY, size=(300,-1),
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
        global curr_location
        global curr_mapset
        global xy_location
        global xy_mapset
        global maptype

        if event.GetDirection():
            # switch to xy location if coming into the page from preceding
            self.parent.SwitchLocMapset(xy_location, xy_mapset)
            self.selection.SetElementList(maptype)
        else:
            # switch back to current location if leaving the page
            self.parent.SwitchLocMapset(curr_location, curr_mapset)

class GCP(wx.Frame):
    """
    Manages ground control points for georectifying. Calculates RMS statics.
    Calls i.rectify or v.transform to georectify map.
    """
    def __init__(self,parent,id=-1,title="Create & manage ground control points"):
        wx.Frame.__init__(self, parent, id , title, size=(50,600))

    # Button for adding new GCP

# maybe we can use mapdisp classes to create the display???