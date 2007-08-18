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

import wx
import wx.aui
import wx.lib.filebrowsebutton as filebrowse
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

class Georectify(object):
    """
    Init class for georectifying. Launches startup dialog
    for setting georectifying parameters.
    """
    def __init__(self,parent):
        print 'in georectify start'

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

class GeorectStart(wx.Dialog):
    def __init__(self, parent, id=wx.ID_ANY, title=_('Set georectification parameters'),
                           pos=wx.DefaultPosition, size=(-1,-1),
                           style=wx.DEFAULT_DIALOG_STYLE|wx.RESIZE_BORDER):
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)

        #set environmental variables
        cmdlist = ['g.gisenv', 'get=GISDBASE']
        self.grassdatabase = cmd.Command(cmdlist).module_stdout.read().strip()

        cmdlist = ['g.gisenv', 'get=LOCATION_NAME']
        self.curr_location = cmd.Command(cmdlist).module_stdout.read().strip()

        cmdlist = ['g.gisenv', 'get=MAPSET']
        self.curr_mapset = cmd.Command(cmdlist).module_stdout.read().strip()

        self.StartDir = os.path.join(self.grassdatabase,self.curr_location)
        self.xy_mapset = ''
        self.xy_mapsetDir = ''
        self.xy_group = ''

        #select display map (entry)
        #start and cancel (buttons)

        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        self.rb_maptype = wx.RadioBox(self, -1, "Map type to georectify",
                                   wx.DefaultPosition, wx.DefaultSize,
                                   ['raster','vector'], 2, wx.RA_SPECIFY_COLS)
        box.Add(self.rb_maptype, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        self.dbb_mapset = filebrowse.DirBrowseButton(
            self, wx.ID_ANY, size=(450, -1),
            labelText='Select mapset: ',
            buttonText='Browse ...',
            toolTip='Select mapset of maps to georeference',
            dialogTitle='Mapsets',
            startDirectory=self.StartDir,
            changeCallback = self.On_dbb_mapset)
        box.Add(self.dbb_mapset, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        self.btn_mkgroup = wx.Button(self, wx.ID_ANY, "Make group ...")
        box.Add(self.btn_mkgroup, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        self.dbb_group = filebrowse.DirBrowseButton(
            self, wx.ID_ANY, size=(450, -1),
            labelText='Select group: ',
            buttonText='Browse ...',
            toolTip='Select group of maps to georeference',
            dialogTitle='Groups',
            startDirectory=os.path.join(self.xy_mapset,"group"),
            changeCallback = self.On_dbb_group)
        # Need to deactivate until xy_mapsetDir is set
        box.Add(self.dbb_group, 0, wx.ALIGN_LEFT|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        #Select the display map here...????
        #In TclTk, first switch to xy mapset; then select map
        box = wx.BoxSizer(wx.HORIZONTAL)
        self.btn_selmap = wx.Button(self, wx.ID_ANY, "Select map ...")

        box.Add(self.btn_selmap, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

        btnsizer = wx.StdDialogButtonSizer()

        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(btnsizer, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

        self.Bind(wx.EVT_BUTTON, self.OnMkgroup, self.btn_mkgroup)
        self.Bind(wx.EVT_BUTTON, self.OnSelectMap, self.btn_selmap)

    def On_dbb_mapset(self,event):
        self.xy_mapsetDir = event.GetString()
        # Disable group select if no mapset selected?
        # need to split mapsetDir into xy_location and xy_mapset
#        self.xy_mapsetDir = os.path.join(self.grassdatabase,self.location,self.mapset)

        print 'mapset =',self.mapset

    def On_dbb_group(self,event):
        self.xy_group = event.GetString()

        print 'group =',self.xy_group

    def OnSelectMap(self,event):
        dlg = select.SelectDialog(self,title="Select map",type='cell')

        if dlg.ShowModal() == wx.ID_OK:
            self.xy_map = dlg.selection.GetValue()

        dlg.Destroy()
        print 'selection =',self.xy_map


    def OnMkgroup(self,event):
        pass

    def SwitchToXY(self):
        pass

    def SwitchBack(self):
        pass

class GCP(wx.Frame):
    """
    Manages ground control points for georectifying. Calculates RMS statics.
    Calls i.rectify or v.transform to georectify map.
    """
    def __init__(self,parent,id=-1,title="Create & manage ground control points"):
        wx.Frame.__init__(self, parent, id , title, size=(50,600))

    # Button for adding new GCP

# maybe we can use mapdisp classes to create the display???