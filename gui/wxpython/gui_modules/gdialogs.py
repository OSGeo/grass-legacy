"""
@package gdialogs.py

@brief Common dialog used in wxGUI.

List of classes:
 - NewVectorDialog

(C) 2008 by the GRASS Development Team

This program is free software under the GNU General Public
License (>=v2). Read the file COPYING that comes with GRASS
for details.

@author Martin Landa <landa.martin gmail.com>
"""

import wx

import gcmd
import grassenv
import globalvar
import gselect
from preferences import globalSettings as UserSettings

class NewVectorDialog(wx.Dialog):
    """Create new vector map layer"""
    def __init__(self, parent, id, title, 
                style=wx.DEFAULT_DIALOG_STYLE | wx.RESIZE_BORDER):

        wx.Dialog.__init__(self, parent, id, title, style=style)

        self.panel = wx.Panel(parent=self, id=wx.ID_ANY)

        self.btnCancel = wx.Button(self.panel, wx.ID_CANCEL)
        self.btnOK = wx.Button(self.panel, wx.ID_OK)
        self.btnOK.SetDefault()
        self.btnOK.Enable(False)

        self.label = wx.StaticText(parent=self.panel, id=wx.ID_ANY,
                                   label=_("Name for new vector map:"))
        self.mapName = gselect.Select(parent=self.panel, id=wx.ID_ANY, size=globalvar.DIALOG_GSELECT_SIZE,
                                      type='vector', mapsets=[grassenv.GetGRASSVariable('MAPSET'),])

        self.mapName.Bind(wx.EVT_TEXT, self.OnMapName)

        # TODO remove (see Preferences dialog)
        self.overwrite = wx.CheckBox(parent=self.panel, id=wx.ID_ANY,
                                     label=_("Allow output files to overwrite existing files"))
        self.overwrite.SetValue(UserSettings.Get(group='cmd', key='overwrite', subkey='enabled'))

        self.__Layout()

        self.SetMinSize(self.GetSize())

    def OnMapName(self, event):
        """Name for vector map layer given"""
        if len(event.GetString()) > 0:
            self.btnOK.Enable(True)
        else:
            self.btnOK.Enable(False)

    def __Layout(self):
        """Do layout"""
        sizer = wx.BoxSizer(wx.VERTICAL)

        dataSizer = wx.BoxSizer(wx.VERTICAL)
        dataSizer.Add(self.label, proportion=0,
                      flag=wx.ALL, border=1)
        dataSizer.Add(self.mapName, proportion=0,
                      flag=wx.EXPAND | wx.ALL, border=1)
        dataSizer.Add(self.overwrite, proportion=0,
                      flag=wx.ALL, border=1)

        # buttons
        btnSizer = wx.StdDialogButtonSizer()
        btnSizer.AddButton(self.btnCancel)
        btnSizer.AddButton(self.btnOK)
        btnSizer.Realize()

        sizer.Add(item=dataSizer, proportion=1,
                  flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)

        sizer.Add(item=btnSizer, proportion=0,
                  flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)
       
        self.panel.SetSizer(sizer)
        sizer.Fit(self)

    def GetName(self):
        """Return (mapName, overwrite)"""
        return (self.mapName.GetValue() + '@' + grassenv.GetGRASSVariable('MAPSET'),
                self.overwrite.IsChecked())


def CreateNewVector(parent, title=_('Create new vector map')):
    """Create new vector map layer

    @return name of create vector map
    @return None of failure
    """
    dlg = NewVectorDialog(parent=parent, id=wx.ID_ANY, title=title)
    if dlg.ShowModal() == wx.ID_OK:
        outmap, overwrite = dlg.GetName()
        
        if outmap == '': # should not happen
            return False
        
        cmd = ["v.edit",
               "map=%s" % outmap,
               "tool=create"]
                
        if overwrite is True:
            cmd.append('--overwrite')
            
        p = gcmd.Command(cmd)
            
        if p.returncode == 0:
            return outmap

    return None
