#!/usr/bin/env python

"""
MODULE:    gis_set.py

CLASSES:
    * GRASSStartup
    * StartUp

PURPOSE:   Initialization module for wxPython GUI for GRASS GIS. Includes
            location/mapset selection, location/mapset creation,
            location/mapset management.

           Usage:
            python "$GISBASE/etc/wx/wxgui.py" -name wxgui_py

AUTHORS:   The GRASS Development Team
           Michael Barton
           Jachym Cepicky

COPYRIGHT: (C) 2006-2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""


import wx
import os
import glob
import shutil
import wx.lib.rcsizer  as rcs
import gui_modules.gcmd as gcmd
import shutil

def read_grassrc():
    """
    Read variables from $HOME/.grassrc6 file
    """

    grassrc = {}

    if os.path.isfile(os.getenv("GISRC")):
        rc = open(os.getenv("GISRC"), "r")
        for line in rc.readlines():
            key,val = line.split(":")
            grassrc[key.strip()] = val.strip()
        rc.close()

    return grassrc

class GRASSStartup(wx.Frame):
    def __init__(self, *args, **kwds):
        kwds["style"] = wx.DEFAULT_FRAME_STYLE
        wx.Frame.__init__(self, *args, **kwds)

        #
        # variables
        #
        self.grassrc = read_grassrc()
        self.gisbase=os.getenv("GISBASE")
        self.gisdbase=self._getRCValue("GISDBASE")
        self.listOfLocations = []
        self.listOfMapsets = []

        #
        # graphical elements
        #
        try:
            self.hbitmap = wx.StaticBitmap(self, -1,
                    wx.Bitmap(os.path.join(self.gisbase,"etc","gintro.gif"), wx.BITMAP_TYPE_ANY))
        except:
            self.hbitmap = wx.StaticBitmap(self,  -1, wx.EmptyBitmap(530,150))

        # labels
        self.lwelcome = wx.StaticText(self, -1,
                "Welcome to GRASS GIS Version 6.3.cvs\n"+\
                "The world's leading open source GIS",
                style=wx.ALIGN_CENTRE)
        self.ltitle = wx.StaticText(self, -1,
                "Select an existing project location and mapset\n"+\
                "or define a new location",
                style=wx.ALIGN_CENTRE)
        self.ldbase = wx.StaticText(self, -1, "GIS Data Directory:")
        self.llocation = wx.StaticText(self, -1, "Project Location\n(projection/coordinate system)", style=wx.ALIGN_CENTRE)
        self.lmapset = wx.StaticText(self, -1, "Accessible Mapsets\n(directories of GIS files)", style=wx.ALIGN_CENTRE)
        self.lmanage = wx.StaticText(self, -1, "Manage Locations\nand Mapsets", style=wx.ALIGN_CENTRE)
        self.lcreate = wx.StaticText(self, -1, "Create new mapset\nin selected location", style=wx.ALIGN_CENTRE)
        self.ldefine = wx.StaticText(self, -1, "Define new location", style=wx.ALIGN_CENTRE)
        self.lmanageloc = wx.StaticText(self, -1, "Rename/delete selected\nmapset or location", style=wx.ALIGN_CENTRE)

        # buttons
        buttonsize1 = (150,-1)
        buttonsize2 = (150, -1)

        self.bstart = wx.Button(self, -1, "Start GRASS", size=buttonsize2)
        self.bstart.SetDefault()
        self.bexit = wx.Button(self, -1, "Exit", size=buttonsize2)
        self.bhelp = wx.Button(self, -1, "Help", size=buttonsize2)
        self.bbrowse = wx.Button(self, -1, "Browse ...", size=(-1,-1))
        self.bmapset = wx.Button(self, -1, "Create mapset", size=buttonsize1)
        self.bwizard = wx.Button(self, -1, "Location wizard", size=buttonsize1)
        choicelist = ['Rename mapset','Rename location', 'Delete mapset', 'Delete location']
        self.manageloc = wx.Choice(self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, choices=choicelist)

        # textinputs
        self.tgisdbase = wx.TextCtrl(self, -1, "", size=(300, 20),
                style=wx.TE_LEFT)
        self.tnewmapset = wx.TextCtrl(self,-1, "", size=(150,20))

        # Locations
        self.lpanel = wx.Panel(self,-1)
        self.lblocations = wx.ListBox(self.lpanel,
                id=26, pos=wx.DefaultPosition, size=(150, 200),
                choices=self.listOfLocations, style=wx.LB_SINGLE)

        # Mapsets
        self.mpanel = wx.Panel(self,-1)
        self.lbmapsets = wx.ListBox(self.mpanel,
                id=26, pos=wx.DefaultPosition, size=(150, 200),
                choices=self.listOfMapsets, style=wx.LB_SINGLE)

        # layout & properties
        self.__set_properties()
        self.__do_layout()

        # events
        self.bbrowse.Bind(wx.EVT_BUTTON, self.OnBrowse)
        self.bstart.Bind(wx.EVT_BUTTON, self.OnStart)
        self.bexit.Bind(wx.EVT_BUTTON, self.OnExit)
        self.bhelp.Bind(wx.EVT_BUTTON, self.OnHelp)
        self.bmapset.Bind(wx.EVT_BUTTON, self.OnCreateMapset)
        self.bwizard.Bind(wx.EVT_BUTTON, self.OnWizard)
        self.manageloc.Bind(wx.EVT_CHOICE, self.OnManageLoc)
        self.lblocations.Bind(wx.EVT_LISTBOX, self.OnSelectLocation)
        self.lbmapsets.Bind(wx.EVT_LISTBOX, self.OnSelectMapset)
        self.Bind(wx.EVT_KEY_DOWN, self.OnKeyPressedInDbase, self.tgisdbase)
        self.Bind(wx.EVT_KEY_DOWN, self.OnKeyPressedInMapset, self.tnewmapset)
        self.Bind(wx.EVT_CLOSE, self.onCloseWindow)

    def __set_properties(self):
        self.SetTitle("Welcome to GRASS GIS")
        self.SetIcon(wx.Icon(os.path.join(self.gisbase,"etc","dm","grass.gif"),
            wx.BITMAP_TYPE_GIF))
        self.lwelcome.SetForegroundColour(wx.Colour(35, 142, 35))
        self.lwelcome.SetFont(wx.Font(14, wx.DEFAULT, wx.NORMAL, wx.BOLD, 0, ""))
        self.bstart.SetForegroundColour(wx.Colour(35, 142, 35))
        self.bstart.SetToolTipString("Enter GRASS session")
        #self.bstart.Enable(False)
        #self.bmapset.Enable(False)

        # set database
        if not self.gisdbase:
            # sets an initial path for gisdbase if nothing in GISRC
            if os.path.isdir(os.getenv("HOME")):
                self.gisdbase = os.getenv("HOME")
            else:
                self.gisdbase = os.getcwd()
        self.tgisdbase.SetValue(self.gisdbase)

        self.OnSetDatabase(None)
        location = self._getRCValue("LOCATION_NAME")
        if location == "<UNKNOWN>":
            location = None
        if location:
            # list of locations
            self.UpdateLocations(self.gisdbase)
            self.lblocations.SetSelection(self.listOfLocations.index(location))

            # list of mapsets
            self.UpdateMapsets(os.path.join(self.gisdbase,location))
            mapset =self._getRCValue("MAPSET")
            if  mapset:
                self.lbmapsets.SetSelection(self.listOfMapsets.index(mapset))
                #self.bstart.Enable(True)

    def __do_layout(self):
        label_style = wx.ADJUST_MINSIZE | wx.ALIGN_CENTER_HORIZONTAL
        sizer = wx.BoxSizer(wx.VERTICAL)
        dbase_sizer=wx.BoxSizer(wx.HORIZONTAL)
        grid_sizer = wx.FlexGridSizer(4, 3, 4, 4)
        mapset_sizer = wx.BoxSizer(wx.VERTICAL)

        dbase_sizer.Add(self.ldbase, 0, wx.ALIGN_CENTER_VERTICAL|
                wx.ALIGN_CENTER_HORIZONTAL|wx.ALL, 5)
        dbase_sizer.Add(self.tgisdbase, 0,  wx.ALIGN_CENTER_VERTICAL
                |wx.ALIGN_CENTER_HORIZONTAL|wx.ALL, 5)
        dbase_sizer.Add(self.bbrowse, 0, wx.ALIGN_CENTER_VERTICAL |
                wx.ALIGN_CENTER_HORIZONTAL|wx.ALL, 5)

        mapset_sizer.Add(self.ldefine, 0, label_style|wx.RIGHT|wx.LEFT|wx.BOTTOM, 5)
        mapset_sizer.Add(self.bwizard, 0, label_style|wx.BOTTOM, 8)
        mapset_sizer.Add(self.lcreate, 0, label_style|wx.TOP|wx.RIGHT|wx.LEFT, 5)
        mapset_sizer.Add(self.tnewmapset, 0, label_style|wx.ALL, 5)
        mapset_sizer.Add(self.bmapset, 0, label_style|wx.BOTTOM, 8)
        mapset_sizer.Add(self.lmanageloc, 0, label_style|wx.TOP|wx.RIGHT|wx.LEFT, 5)
        mapset_sizer.Add(self.manageloc, 0, label_style|wx.ALL, 5)
        mapset_sizer.Add((5,0))

        grid_sizer.Add(self.llocation, 0,label_style|wx.ALL, 5)
        grid_sizer.Add(self.lmapset, 0,label_style|wx.ALL, 5)
        grid_sizer.Add(self.lmanage, 0,label_style|wx.ALL, 5)

        grid_sizer.Add(self.lpanel, 0, wx.ADJUST_MINSIZE|
                       wx.ALIGN_CENTER_VERTICAL|
                       wx.ALIGN_CENTER_HORIZONTAL, 0)
        grid_sizer.Add(self.mpanel, 0, wx.ADJUST_MINSIZE|
                       wx.ALIGN_CENTER_VERTICAL|
                       wx.ALIGN_CENTER_HORIZONTAL, 0)
        grid_sizer.Add(mapset_sizer, 0, wx.ADJUST_MINSIZE|
                       wx.ALIGN_CENTER_VERTICAL|
                       wx.ALIGN_CENTER_HORIZONTAL, 0)

        grid_sizer.Add(self.bstart, 0, wx.ADJUST_MINSIZE|
                       wx.ALIGN_TOP|
                       wx.ALIGN_CENTER_HORIZONTAL|
                       wx.BOTTOM, 10)
        grid_sizer.Add(self.bexit, 0, wx.ADJUST_MINSIZE|
                       wx.ALIGN_CENTER_VERTICAL|
                       wx.ALIGN_CENTER_HORIZONTAL|
                       wx.BOTTOM, 10)
        grid_sizer.Add(self.bhelp, 0, wx.ADJUST_MINSIZE|
                       wx.ALIGN_CENTER_VERTICAL|
                       wx.ALIGN_CENTER_HORIZONTAL|
                       wx.BOTTOM, 10)

        # adding to main VERTICAL sizer
        sizer.Add(self.hbitmap, 0, wx.ADJUST_MINSIZE |
                wx.ALIGN_CENTER_VERTICAL |
                wx.ALIGN_CENTER_HORIZONTAL |
                wx.BOTTOM, 5) # image
        sizer.Add(self.lwelcome, # welcome message
                0,wx.ADJUST_MINSIZE |
                wx.ALIGN_CENTER_VERTICAL |
                wx.ALIGN_CENTER_HORIZONTAL |
                wx.EXPAND |
                wx.BOTTOM, 10)
        sizer.Add(self.ltitle, # controls title
                0,wx.ADJUST_MINSIZE |
                wx.ALIGN_CENTER_VERTICAL |
                wx.ALIGN_CENTER_HORIZONTAL |
                wx.EXPAND |
                wx.BOTTOM, 5)
        sizer.Add(dbase_sizer,0,wx.ADJUST_MINSIZE |
                wx.ALIGN_CENTER_VERTICAL |
                wx.ALIGN_CENTER_HORIZONTAL |
                wx.RIGHT | wx.LEFT, 5) # GISDBASE setting
        sizer.Add(grid_sizer, 1, wx.ADJUST_MINSIZE |
                wx.ALIGN_CENTER_VERTICAL |
                wx.ALIGN_CENTER_HORIZONTAL |
                wx.RIGHT | wx.LEFT, 5)
        self.SetAutoLayout(True)
        self.SetSizer(sizer)
        sizer.Fit(self)
        sizer.SetSizeHints(self)
        self.Layout()
        # end wxGlade

    def _getRCValue(self,value):

        if self.grassrc.has_key(value):
            return self.grassrc[value]
        else:
            return None

    def OnWizard(self,event):
        import location_wizard
        reload(location_wizard)
        gWizard = location_wizard.GWizard(self, self.tgisdbase.GetValue())
        if gWizard.location != None:
            self.OnSetDatabase(event)
            self.UpdateMapsets(os.path.join(
                    self.gisdbase,gWizard.location))
            self.lblocations.SetSelection(self.listOfLocations.index(gWizard.location))
            self.lbmapsets.SetSelection(0)

    def OnManageLoc(self,event):
        """
        Location management choice control handler
        """

        if event.GetString() == 'Rename mapset':
            self.RenameMapset()
        elif event.GetString() == 'Rename location':
            self.RenameLocation()
        elif event.GetString() == 'Delete mapset':
            self.DeleteMapset()
        elif event.GetString() == 'Delete location':
            self.DeleteLocation()

    def RenameMapset(self):
        """
        Renames selected mapset
        """

        location = location=self.listOfLocations[self.lblocations.GetSelection()]
        mapset = self.listOfMapsets[self.lbmapsets.GetSelection()]
        dlg = wx.TextEntryDialog(
                self, 'Current name: %s\nEnter new name:' % mapset,
                'Rename selected mapset')

        if dlg.ShowModal() == wx.ID_OK:
            newmapset = dlg.GetValue()
            try:
                os.rename(os.path.join(self.gisdbase,location,mapset),\
                            os.path.join(self.gisdbase,location,newmapset))
                self.OnSelectLocation(None)
                self.lbmapsets.SetSelection(self.listOfMapsets.index(newmapset))
            except:
                wx.MessageBox('Mapset could not be renamed')

        dlg.Destroy()

    def RenameLocation(self):
        """
        Renames selected location
        """

        location = location=self.listOfLocations[self.lblocations.GetSelection()]
        dlg = wx.TextEntryDialog(
                self, 'Current name: %s\nEnter new name:' % location,
                'Rename selected location')

        if dlg.ShowModal() == wx.ID_OK:
            newlocation = dlg.GetValue()
            mapset = self.listOfMapsets[self.lbmapsets.GetSelection()]
            try:
                os.rename(os.path.join(self.gisdbase,location),\
                            os.path.join(self.gisdbase,newlocation))
                self.UpdateLocations(self.gisdbase)
                self.lblocations.SetSelection(self.listOfLocations.index(newlocation))

            except:
                wx.MessageBox('Location could not be renamed')

        dlg.Destroy()

    def DeleteMapset(self):
        """
        Deletes selected mapset
        """

        location = location=self.listOfLocations[self.lblocations.GetSelection()]
        mapset = self.listOfMapsets[self.lbmapsets.GetSelection()]
        dlg = wx.MessageDialog(self, "Do you want to continue with deleting the mapset?",
                               "WARNING! Mapset '%s', and ALL MAPS it contains will be PERMANENTLY DELETED!"
                               % mapset,wx.YES_NO|wx.NO_DEFAULT|wx.ICON_EXCLAMATION)
        if dlg.ShowModal() == wx.ID_YES:
            try:
                shutil.rmtree(os.path.join(self.gisdbase,location,mapset))
                self.OnSelectLocation(None)
                self.lbmapsets.SetSelection(0)
            except:
                wx.MessageBox('Mapset could not be deleted')

        dlg.Destroy()

    def DeleteLocation(self):
        """
        Deletes selected location
        """

        location = location=self.listOfLocations[self.lblocations.GetSelection()]
        dlg = wx.MessageDialog(self, "Do you want to continue with deleting the location?",
                               "WARNING! Location '%s', and ALL MAPSETS and MAPS it contains will be PERMANENTLY DELETED!"
                               % location,wx.YES_NO|wx.NO_DEFAULT|wx.ICON_EXCLAMATION)
        if dlg.ShowModal() == wx.ID_YES:
            try:
                shutil.rmtree(os.path.join(self.gisdbase,location))
                self.UpdateLocations(self.gisdbase)
            except:
                wx.MessageBox('Location could not be deleted')

        dlg.Destroy()

    def UpdateLocations(self,dbase):

        self.listOfLocations = []
        for location in glob.glob(os.path.join(dbase,"*")):
            try:
                if os.path.join(location,"PERMANENT") in glob.glob(os.path.join(location,"*")):
                    self.listOfLocations.append(os.path.basename(location))
            except:
                pass
        self.lblocations.Clear()
        self.lblocations.InsertItems(self.listOfLocations,0)
        return self.listOfLocations

    def UpdateMapsets(self,location):

        self.listOfMapsets = []
        for mapset in glob.glob(os.path.join(location,"*")):
            if os.path.isdir(mapset):
                self.listOfMapsets.append(os.path.basename(mapset))
        self.lbmapsets.Clear()
        self.lbmapsets.InsertItems(self.listOfMapsets,0)
        return self.listOfMapsets

    def OnSelectLocation(self,event):
        if self.lblocations.GetSelection() > -1:
            self.UpdateMapsets(os.path.join(
                    self.gisdbase,self.listOfLocations[self.lblocations.GetSelection()]))
        else:
            self.listOfMapsets = []
        self.lbmapsets.Clear()
        self.lbmapsets.InsertItems(self.listOfMapsets,0)

    def OnSelectMapset(self,event):
        #self.bstart.Enable(True)
        pass

    def OnSetDatabase(self,event):
        self.gisdbase = self.tgisdbase.GetValue()
        self.UpdateLocations(self.gisdbase)
        self.lblocations.Clear()
        self.lblocations.InsertItems(self.listOfLocations,0)
        if self.listOfLocations != []: self.lblocations.SetSelection(0)
        self.OnSelectLocation(event)

    def OnBrowse(self, event):

        grassdata = None

        dlg = wx.DirDialog(self, "Choose a GRASS directory:",
                style=wx.DD_DEFAULT_STYLE | wx.DD_NEW_DIR_BUTTON)
        if dlg.ShowModal() == wx.ID_OK:
            self.gisdbase = dlg.GetPath()
            self.tgisdbase.SetValue(self.gisdbase)
        dlg.Destroy()

        self.OnSetDatabase(event)

    def OnKeyPressedInDbase(self,event):
        if wx.WXK_RETURN == event.KeyCode:
            self.OnSetDatabase(event)
        else:
            event.Skip()

    def OnCreateMapset(self,event):
        self.gisdbase = self.tgisdbase.GetValue()
        location = self.listOfLocations[self.lblocations.GetSelection()]

        try:
            mapset = self.tnewmapset.GetValue()
            os.mkdir(os.path.join(self.gisdbase,location,mapset))
            # copy WIND file and its permissions from PERMANENT and set permissions to u+rw,go+r
            shutil.copy(os.path.join(self.gisdbase,location,'PERMANENT','WIND'),
                        os.path.join(self.gisdbase,location,mapset))
#            os.chmod(os.path.join(database,location,mapset,'WIND'), 0644)
            self.OnSelectLocation(None)
            self.lbmapsets.SetSelection(self.listOfMapsets.index(mapset))
        except StandardError, e:
            dlg = wx.MessageDialog(self, "Could not create new mapset: %s"
                    % e,"Can not create mapset",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()

    def OnKeyPressedInMapset(self,event):
        if wx.WXK_RETURN == event.KeyCode:
            self.OnCreateMapset(None)
        else:
            #self.bmapset.Enable(True)
            event.Skip()

    def OnStart(self, event):
        print "g.gisenv set=GISDBASE='%s';" % self.tgisdbase.GetValue()
        print "g.gisenv set=LOCATION_NAME='%s';" % self.listOfLocations[self.lblocations.GetSelection()]
        print "g.gisenv set=MAPSET='%s';" % self.listOfMapsets[self.lbmapsets.GetSelection()]
        self.Destroy()

    def OnExit(self, event):
        print "exit"
        self.Destroy()

    def OnHelp(self, event):
        wx.MessageBox("Help not yet implemented")
        event.Skip()

    def onCloseWindow(self, event):
        print "exit"
        event.Skip()

class StartUp(wx.App):
    def OnInit(self):
        wx.InitAllImageHandlers()
        StartUp = GRASSStartup(None, -1, "")
        self.SetTopWindow(StartUp)
        StartUp.Show()
        return 1

# end of class StartUp

if __name__ == "__main__":

    GRASSStartUp = StartUp(0)
    GRASSStartUp.MainLoop()
