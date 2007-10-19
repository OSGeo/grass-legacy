#!/usr/bin/env python

"""
MODULE:    gis_set.py

CLASSES:
    * GRASSStartup
    * StartUp

PURPOSE:   Initialization module for wxPython GRASS GUI.
           Location/mapset management (selection, creation, etc.).

AUTHORS:   The GRASS Development Team
           Michael Barton
           Jachym Cepicky
           Martin Landa <landa.martin gmail.com>

COPYRIGHT: (C) 2006-2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import os
import sys
import glob
import shutil

import wx
import wx.lib.rcsizer as rcs
import wx.lib.filebrowsebutton as filebrowse

class GRASSStartup(wx.Frame):
    """GRASS start-up screen"""
    def __init__(self, parent=None, id=wx.ID_ANY, style=wx.DEFAULT_FRAME_STYLE):

        #
        # GRASS variables
        #
        self.gisbase  = os.getenv("GISBASE")
        self.grassrc  = self._read_grassrc()
        self.gisdbase = self._getRCValue("GISDBASE")

        #
        # list of locations/mapsets
        #
        self.listOfLocations = []
        self.listOfMapsets = []

        wx.Frame.__init__(self, parent=parent, id=id, style=style)

        #
        # graphical elements
        #
        # image
        try:
            name = os.path.join(self.gisbase, "etc", "gintro.gif")
            self.hbitmap = wx.StaticBitmap(self, wx.ID_ANY,
                                           wx.Bitmap(name=name,
                                                     type=wx.BITMAP_TYPE_GIF))
        except:
            self.hbitmap = wx.StaticBitmap(self, wx.ID_ANY, wx.EmptyBitmap(530,150))

        # labels
        versionCmd = gcmd.Command(['g.version'])
        grassVersion = versionCmd.ReadStdOutput()[0].replace('GRASS', '').strip()

        self.lwelcome = wx.StaticText(parent=self, id=wx.ID_ANY,
                                      label=_("Welcome to GRASS GIS %s\n"
                                              "The world's leading open source GIS") % grassVersion,
                                      style=wx.ALIGN_CENTRE)
        self.ltitle = wx.StaticText(parent=self, id=wx.ID_ANY,
                                    label=_("Select an existing project location and mapset\n"
                                            "or define a new location"),
                                    style=wx.ALIGN_CENTRE)
        self.ldbase = wx.StaticText(parent=self, id=wx.ID_ANY,
                                    label=_("GIS Data Directory:"))
        self.llocation = wx.StaticText(parent=self, id=wx.ID_ANY,
                                       label=_("Project location\n(projection/coordinate system)"),
                                       style=wx.ALIGN_CENTRE)
        self.lmapset = wx.StaticText(parent=self, id=wx.ID_ANY,
                                     label=_("Accessible mapsets\n(directories of GIS files)"),
                                     style=wx.ALIGN_CENTRE)
        self.lcreate = wx.StaticText(parent=self, id=wx.ID_ANY,
                                     label=_("Create new mapset\nin selected location"),
                                     style=wx.ALIGN_CENTRE)
        self.ldefine = wx.StaticText(parent=self, id=wx.ID_ANY,
                                     label=_("Define new location"),
                                     style=wx.ALIGN_CENTRE)
        self.lmanageloc = wx.StaticText(parent=self, id=wx.ID_ANY,
                                        label=_("Rename/delete selected\nmapset or location"),
                                        style=wx.ALIGN_CENTRE)

        # buttons
        self.bstart = wx.Button(parent=self, id=wx.ID_ANY,
                                label=_("Start GRASS"), size=(200, -1))
        self.bstart.SetDefault()
        self.bexit = wx.Button(parent=self, id=wx.ID_EXIT)
        self.bhelp = wx.Button(parent=self, id=wx.ID_HELP)
        self.bbrowse = wx.Button(parent=self, id=wx.ID_ANY,
                                 label=_("Browse"))
        self.bmapset = wx.Button(parent=self, id=wx.ID_ANY,
                                 label=_("Create mapset"))
        self.bwizard = wx.Button(parent=self, id=wx.ID_ANY,
                                 label=_("Location wizard"))
        self.manageloc = wx.Choice(parent=self, id=wx.ID_ANY,
                                   choices=['Rename mapset','Rename location',
                                            'Delete mapset', 'Delete location'])

        # textinputs
        self.tgisdbase = wx.TextCtrl(parent=self, id=wx.ID_ANY, value="", size=(300, -1),
                                     style=wx.TE_LEFT)

        # Locations
        self.lpanel = wx.Panel(parent=self,id=wx.ID_ANY)
        self.lblocations = wx.ListBox(parent=self.lpanel,
                                      id=wx.ID_ANY, size=(150, 200),
                                      choices=self.listOfLocations,
                                      style=wx.LB_SINGLE)

        # Mapsets
        self.mpanel = wx.Panel(parent=self,id=wx.ID_ANY)
        self.lbmapsets = wx.ListBox(parent=self.mpanel,
                                    id=wx.ID_ANY, size=(150, 200),
                                    choices=self.listOfMapsets,
                                    style=wx.LB_SINGLE)

        # layout & properties
        self._set_properties()
        self._do_layout()

        # events
        self.bbrowse.Bind(wx.EVT_BUTTON,      self.OnBrowse)
        self.bstart.Bind(wx.EVT_BUTTON,       self.OnStart)
        self.bexit.Bind(wx.EVT_BUTTON,        self.OnExit)
        self.bhelp.Bind(wx.EVT_BUTTON,        self.OnHelp)
        self.bmapset.Bind(wx.EVT_BUTTON,      self.OnCreateMapset)
        self.bwizard.Bind(wx.EVT_BUTTON,      self.OnWizard)
        self.manageloc.Bind(wx.EVT_CHOICE,    self.OnManageLoc)
        self.lblocations.Bind(wx.EVT_LISTBOX, self.OnSelectLocation)
        self.lbmapsets.Bind(wx.EVT_LISTBOX,   self.OnSelectMapset)
        self.Bind(wx.EVT_KEY_DOWN,            self.OnKeyPressedInDbase, self.tgisdbase)
        self.Bind(wx.EVT_CLOSE,               self.OnCloseWindow)

    def _set_properties(self):
        """Set frame properties"""
        self.SetTitle(_("Welcome to GRASS GIS"))
        self.SetIcon(wx.Icon(os.path.join(self.gisbase, "etc", "dm", "grass.gif"),
                             wx.BITMAP_TYPE_GIF))

        self.lwelcome.SetForegroundColour(wx.Colour(35, 142, 35))
        self.lwelcome.SetFont(wx.Font(14, wx.DEFAULT, wx.NORMAL, wx.BOLD, 0, ""))

        self.bstart.SetForegroundColour(wx.Colour(35, 142, 35))
        self.bstart.SetToolTipString(_("Enter GRASS session"))
        # self.bstart.Enable(False)
        # self.bmapset.Enable(False)

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
                # self.bstart.Enable(True)

    def _do_layout(self):
        label_style = wx.ADJUST_MINSIZE | wx.ALIGN_CENTER_HORIZONTAL

        sizer           = wx.BoxSizer(wx.VERTICAL)
        dbase_sizer     = wx.BoxSizer(wx.HORIZONTAL)
        location_sizer  = wx.FlexGridSizer(rows=1, cols=2, vgap=4, hgap=4)
        select_box      = wx.StaticBox (parent=self, id=wx.ID_ANY,
                                        label=" %s " % _("Choose location and mapset"))
        select_boxsizer = wx.StaticBoxSizer(select_box, wx.VERTICAL)
        select_sizer    = wx.FlexGridSizer(rows=2, cols=2, vgap=4, hgap=4)
        manage_box      = wx.StaticBox (parent=self, id=wx.ID_ANY,
                                        label=" %s " % _("Manage"))
        manage_boxsizer = wx.StaticBoxSizer(manage_box, wx.VERTICAL)
        manage_sizer    = wx.BoxSizer(wx.VERTICAL)
        btns_sizer    = wx.BoxSizer(wx.HORIZONTAL)

        # gis data directory
        dbase_sizer.Add(item=self.ldbase, proportion=0,
                        flag=wx.ALIGN_CENTER_VERTICAL |
                        wx.ALIGN_CENTER_HORIZONTAL | wx.ALL,
                        border=5)
        dbase_sizer.Add(item=self.tgisdbase, proportion=0,
                        flag=wx.ALIGN_CENTER_VERTICAL |
                        wx.ALIGN_CENTER_HORIZONTAL | wx.ALL,
                        border=5)
        dbase_sizer.Add(item=self.bbrowse, proportion=0,
                        flag=wx.ALIGN_CENTER_VERTICAL |
                        wx.ALIGN_CENTER_HORIZONTAL | wx.ALL,
                        border=5)

        # select sizer
        select_sizer.Add(item=self.llocation, proportion=0,
                         flag=label_style | wx.ALL,
                         border=5)
        select_sizer.Add(item=self.lmapset, proportion=0,
                         flag=label_style | wx.ALL,
                         border=5)
        select_sizer.Add(item=self.lpanel, proportion=0,
                         flag=wx.ADJUST_MINSIZE |
                         wx.ALIGN_CENTER_VERTICAL |
                         wx.ALIGN_CENTER_HORIZONTAL)
        select_sizer.Add(item=self.mpanel, proportion=0,
                         flag=wx.ADJUST_MINSIZE |
                         wx.ALIGN_CENTER_VERTICAL |
                         wx.ALIGN_CENTER_HORIZONTAL)

        select_boxsizer.Add(item=select_sizer, proportion=0)

        # define new location and mapset
        manage_sizer.Add(item=self.ldefine, proportion=0,
                         flag=label_style | wx.ALL,
                         border=5) 
        manage_sizer.Add(item=self.bwizard, proportion=0,
                         flag=label_style | wx.BOTTOM,
                         border=8)
        manage_sizer.Add(item=self.lcreate, proportion=0,
                         flag=label_style | wx.ALL,
                         border=5)
        manage_sizer.Add(item=self.bmapset, proportion=0,
                         flag=label_style | wx.BOTTOM,
                         border=8)
        manage_sizer.Add(item=self.lmanageloc, proportion=0,
                         flag=label_style | wx.ALL,
                         border=5)
        manage_sizer.Add(item=self.manageloc, proportion=0,
                         flag=label_style | wx.BOTTOM,
                         border=8)
       
        manage_boxsizer.Add(item=manage_sizer, proportion=0)

        # location sizer
        location_sizer.Add(item=select_boxsizer, proportion=0,
                           flag=wx.ADJUST_MINSIZE |
                           wx.ALIGN_CENTER_VERTICAL |
                           wx.ALIGN_CENTER_HORIZONTAL |
                           wx.RIGHT | wx.LEFT,
                           border=5) # GISDBASE setting
        location_sizer.Add(item=manage_boxsizer, proportion=0,
                           flag=wx.ADJUST_MINSIZE |
                           wx.ALIGN_TOP |
                           wx.ALIGN_CENTER_HORIZONTAL |
                           wx.RIGHT,
                           border=5)

        # buttons
        btns_sizer.Add(item=self.bstart, proportion=0,
                       flag=wx.ALIGN_CENTER_HORIZONTAL |
                       wx.ALL,
                       border=10)
        btns_sizer.Add(item=self.bexit, proportion=0,
                       flag=wx.ALIGN_CENTER_HORIZONTAL |
                       wx.ALL,
                       border=10)
        btns_sizer.Add(item=self.bhelp, proportion=0,
                       flag=wx.ALIGN_CENTER_HORIZONTAL |
                       wx.ALL,
                       border=10)

        # main sizer
        sizer.Add(item=self.hbitmap, proportion=0,
                  flag=wx.ADJUST_MINSIZE |
                  wx.ALIGN_CENTER_VERTICAL |
                  wx.ALIGN_CENTER_HORIZONTAL |
                  wx.BOTTOM | wx.TOP,
                  border=5) # image
        sizer.Add(item=self.lwelcome, # welcome message
                  proportion=0,
                  flag= wx.ADJUST_MINSIZE |
                  wx.ALIGN_CENTER_VERTICAL |
                  wx.ALIGN_CENTER_HORIZONTAL |
                  wx.EXPAND |
                  wx.BOTTOM,
                  border=10)
        sizer.Add(item=self.ltitle, # title
                  proportion=0,
                  flag=wx.ADJUST_MINSIZE |
                  wx.ALIGN_CENTER_VERTICAL |
                  wx.ALIGN_CENTER_HORIZONTAL |
                  wx.EXPAND |
                  wx.BOTTOM,
                  border=5)
        sizer.Add(item=dbase_sizer, proportion=0,
                  flag=wx.ADJUST_MINSIZE |
                  wx.ALIGN_CENTER_VERTICAL |
                  wx.ALIGN_CENTER_HORIZONTAL |
                  wx.RIGHT | wx.LEFT,
                  border=5) # GISDBASE setting
        sizer.Add(item=location_sizer, proportion=1,
                  flag=wx.ADJUST_MINSIZE |
                  wx.ALIGN_CENTER_VERTICAL |
                  wx.ALIGN_CENTER_HORIZONTAL |
                  wx.RIGHT | wx.LEFT,
                  border=5)
        sizer.Add(item=btns_sizer, proportion=0,
                  flag=wx.ALIGN_CENTER_VERTICAL |
                  wx.ALIGN_CENTER_HORIZONTAL |
                  wx.RIGHT | wx.LEFT,
                  border=5)

        self.SetAutoLayout(True)
        self.SetSizer(sizer)
        sizer.Fit(self)
        sizer.SetSizeHints(self)
        self.Layout()

    def _read_grassrc(self):
        """
        Read variables from $HOME/.grassrc6 file
        """

        grassrc = {}
        
        gisrc = os.getenv("GISRC")

        if gisrc and os.path.isfile(gisrc):
            try:
                rc = open(gisrc, "r")
                for line in rc.readlines():
                    key, val = line.split(":")
                    grassrc[key.strip()] = val.strip()
            finally:
                rc.close()

        return grassrc

    def _getRCValue(self, value):
        "Return GRASS variable (read from GISRC)"""

        if self.grassrc.has_key(value):
            return self.grassrc[value]
        else:
            return None

    def OnWizard(self,event):
        """Location wizard started"""
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
        Rename selected mapset
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
        Rename selected location
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
        Delete selected mapset
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
        Delete selected location
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
        """Update list of locations"""
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
        """Update list of mapsets"""
        self.listOfMapsets = []
        for mapset in glob.glob(os.path.join(location,"*")):
            if os.path.isdir(mapset):
                self.listOfMapsets.append(os.path.basename(mapset))
        self.lbmapsets.Clear()
        self.lbmapsets.InsertItems(self.listOfMapsets,0)
        return self.listOfMapsets

    def OnSelectLocation(self,event):
        """Location selected"""
        if self.lblocations.GetSelection() > -1:
            self.UpdateMapsets(os.path.join(
                    self.gisdbase,self.listOfLocations[self.lblocations.GetSelection()]))
        else:
            self.listOfMapsets = []
        self.lbmapsets.Clear()
        self.lbmapsets.InsertItems(self.listOfMapsets,0)

    def OnSelectMapset(self,event):
        """Mapset selected"""
        #self.bstart.Enable(True)
        pass

    def OnSetDatabase(self,event):
        """Database set"""
        self.gisdbase = self.tgisdbase.GetValue()
        self.UpdateLocations(self.gisdbase)
        self.lblocations.Clear()
        self.lblocations.InsertItems(self.listOfLocations,0)
        if self.listOfLocations != []: self.lblocations.SetSelection(0)
        self.OnSelectLocation(event)

    def OnBrowse(self, event):
        """'Browse' button clicked"""
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
        """Create new mapset"""
        self.gisdbase = self.tgisdbase.GetValue()
        location = self.listOfLocations[self.lblocations.GetSelection()]

        dlg = wx.TextEntryDialog(parent=self,
                                 message=_('Enter name for new mapset:'),
                                 caption='Rename selected mapset')

        if dlg.ShowModal() == wx.ID_OK:
            mapset = dlg.GetValue()
            try:
                os.mkdir(os.path.join(self.gisdbase, location, mapset))
                # copy WIND file and its permissions from PERMANENT and set permissions to u+rw,go+r
                shutil.copy(os.path.join(self.gisdbase, location, 'PERMANENT', 'WIND'),
                            os.path.join(self.gisdbase, location, mapset))
                # os.chmod(os.path.join(database,location,mapset,'WIND'), 0644)
                self.OnSelectLocation(None)
                self.lbmapsets.SetSelection(self.listOfMapsets.index(mapset))
            except StandardError, e:
                dlg = wx.MessageDialog(parent=self, message=_("Unable to create new mapset: %s") % e,
                                       caption=_("Error"), style=wx.OK | wx.ICON_ERROR)
                dlg.ShowModal()
                dlg.Destroy()
                return False

        return True

    def OnStart(self, event):
        print "g.gisenv set=GISDBASE='%s';" % self.tgisdbase.GetValue()
        print "g.gisenv set=LOCATION_NAME='%s';" % self.listOfLocations[self.lblocations.GetSelection()]
        print "g.gisenv set=MAPSET='%s';" % self.listOfMapsets[self.lbmapsets.GetSelection()]
        self.Destroy()

    def OnExit(self, event):
        """'Exit' button clicked"""
        self.Destroy()

    def OnHelp(self, event):
        """'Help' button clicked"""
        wx.MessageBox("Help not yet implemented")
        event.Skip()

    def OnCloseWindow(self, event):
        """Close window event"""
        event.Skip()

class StartUp(wx.App):
    """Start-up application"""

    def OnInit(self):
        wx.InitAllImageHandlers()
        StartUp = GRASSStartup()
        self.SetTopWindow(StartUp)
        StartUp.Show()
        return 1

if __name__ == "__main__":
    
    if os.getenv("GISBASE") is None:
        print >> sys.stderr, "Failed to start GUI, GRASS GIS is not running."
    else:
        import gettext
        gettext.install("GRASSStartUp") # replace with the appropriate catalog name
        
        import gui_modules.gcmd as gcmd

        GRASSStartUp = StartUp(0)
        GRASSStartUp.MainLoop()
