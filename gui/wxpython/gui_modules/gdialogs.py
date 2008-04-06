"""
@package gdialogs.py

@brief Common dialog used in wxGUI.

List of classes:
 - NewVectorDialog
 - SavedRegion
 - DecorationDialog
 - TextLayerDialog 

(C) 2008 by the GRASS Development Team

This program is free software under the GNU General Public
License (>=v2). Read the file COPYING that comes with GRASS
for details.

@author Martin Landa <landa.martin gmail.com>
"""

import sys

import wx

import gcmd
import grassenv
import globalvar
import gselect
import menuform
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
        mapName = self.mapName.GetValue().split('@', 1)[0]

        return (mapName,
                self.overwrite.IsChecked())

def CreateNewVector(parent, title=_('Create new vector map'),
                    exceptMap=None):
    """Create new vector map layer

    @return name of create vector map
    @return None of failure
    """
    dlg = NewVectorDialog(parent=parent, id=wx.ID_ANY, title=title)
    if dlg.ShowModal() == wx.ID_OK:
        outmap, overwrite = dlg.GetName()
        if outmap == exceptMap:
            wx.MessageBox(parent=parent,
                          message=_("Unable to create vector map <%s>.") % outmap,
                          caption=_("Error"),
                          style=wx.ID_OK | wx.ICON_ERROR | wx.CENTRE)
            return False

        if outmap == '': # should not happen
            return False
        
        cmd = ["v.edit",
               "map=%s" % outmap,
               "tool=create"]
                
        if overwrite is True:
            cmd.append('--overwrite')
            
        try:
            p = gcmd.Command(cmd, stderr=None)
        except gcmd.CmdError, e:
            print >> sys.stderr, e
            return None

        if p.returncode == 0:
            # return fully qualified map name
            return outmap + '@' + grassenv.GetGRASSVariable('MAPSET')

    return None

class SavedRegion(wx.Dialog):
    def __init__(self, parent, id, title="", pos=wx.DefaultPosition, size=wx.DefaultSize,
                 style=wx.DEFAULT_DIALOG_STYLE,
                 loadsave='load'):
        """
        Loading and saving of display extents to saved region file
        """
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)

        self.loadsave = loadsave
        self.wind = ''

        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        if loadsave == 'load':
            label = wx.StaticText(parent=self, id=wx.ID_ANY, label=_("Load region:"))
            box.Add(item=label, proportion=0, flag=wx.ALIGN_CENTRE | wx.ALL, border=5)
            self.selection = gselect.Select(parent=self, id=wx.ID_ANY, size=globalvar.DIALOG_GSELECT_SIZE,
                                            type='windows')
            box.Add(item=self.selection, proportion=0, flag=wx.ALIGN_CENTRE | wx.ALL, border=5)
            self.selection.Bind(wx.EVT_TEXT, self.OnSelection)

        elif loadsave == 'save':
            label = wx.StaticText(parent=self, id=wx.ID_ANY, label=_("Save region:"))
            box.Add(item=label, proportion=0, flag=wx.ALIGN_CENTRE | wx.ALL, border=5)
            self.textentry = wx.TextCtrl(parent=self, id=wx.ID_ANY, value="",
                                         size=globalvar.DIALOG_TEXTCTRL_SIZE)
            box.Add(item=self.textentry, proportion=0, flag=wx.ALIGN_CENTRE | wx.ALL, border=5)
            self.textentry.Bind(wx.EVT_TEXT, self.OnText)

        sizer.Add(item=box, proportion=0, flag=wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL,
                  border=5)

        line = wx.StaticLine(parent=self, id=wx.ID_ANY, size=(20, -1), style=wx.LI_HORIZONTAL)
        sizer.Add(item=line, proportion=0,
                  flag=wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.LEFT|wx.RIGHT, border=5)

        btnsizer = wx.StdDialogButtonSizer()

        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(item=btnsizer, proportion=0, flag=wx.ALIGN_RIGHT | wx.ALL, border=5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def OnSelection(self, event):
        self.wind = event.GetString()

    def OnText(self, event):
        self.wind = event.GetString()

class DecorationDialog(wx.Dialog):
    """
    Controls setting options and displaying/hiding map overlay decorations
    """
    def __init__(self, parent, ovlId, title, cmd, name=None,
                 pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.DEFAULT_DIALOG_STYLE,
                 checktxt='', ctrltxt=''):

        wx.Dialog.__init__(self, parent, wx.ID_ANY, title, pos, size, style)

        self.ovlId   = ovlId   # PseudoDC id
        self.cmd     = cmd
        self.name    = name    # overlay name
        self.parent  = parent  # MapFrame

        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        self.chkbox = wx.CheckBox(parent=self, id=wx.ID_ANY, label=checktxt)
        if self.parent.Map.GetOverlay(self.ovlId) is None:
            self.chkbox.SetValue(True)
        else:
            self.chkbox.SetValue(self.parent.MapWindow.overlays[self.ovlId]['layer'].IsActive())
        box.Add(item=self.chkbox, proportion=0,
                flag=wx.ALIGN_CENTRE|wx.ALL, border=5)
        sizer.Add(item=box, proportion=0,
                  flag=wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, border=5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        optnbtn = wx.Button(parent=self, id=wx.ID_ANY, label=_("Set options"))
        box.Add(item=optnbtn, proportion=0, flag=wx.ALIGN_CENTRE|wx.ALL, border=5)
        sizer.Add(item=box, proportion=0,
                  flag=wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, border=5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(parent=self, id=wx.ID_ANY,
                              label=_("Drag %s with mouse in pointer mode to position.\n"
                                      "Double-click to change options." % ctrltxt))
        box.Add(item=label, proportion=0,
                flag=wx.ALIGN_CENTRE|wx.ALL, border=5)
        sizer.Add(item=box, proportion=0,
                  flag=wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, border=5)

        line = wx.StaticLine(parent=self, id=wx.ID_ANY, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(item=line, proportion=0,
                  flag=wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, border=5)

        # buttons
        btnsizer = wx.StdDialogButtonSizer()

        btnOK = wx.Button(parent=self, id=wx.ID_OK)
        btnOK.SetDefault()
        btnsizer.AddButton(btnOK)

        btnCancel = wx.Button(parent=self, id=wx.ID_CANCEL)
        btnsizer.AddButton(btnCancel)
        btnsizer.Realize()

        sizer.Add(item=btnsizer, proportion=0,
                  flag=wx.EXPAND | wx.ALIGN_CENTER_VERTICAL | wx.ALL, border=5)

        #
        # bindings
        #
        self.Bind(wx.EVT_BUTTON,   self.OnOptions, optnbtn)
        self.Bind(wx.EVT_BUTTON,   self.OnCancel,  btnCancel)
        self.Bind(wx.EVT_BUTTON,   self.OnOK,      btnOK)

        self.SetSizer(sizer)
        sizer.Fit(self)

        # create overlay if doesn't exist
        self._CreateOverlay()

    def _CreateOverlay(self):
        if not self.parent.Map.GetOverlay(self.ovlId):
            overlay = self.parent.Map.AddOverlay(id=self.ovlId, type=self.name,
                                                 command=[self.cmd],
                                                 l_active=False, l_render=False, l_hidden=True)

            self.parent.MapWindow.overlays[self.ovlId] = {}
            self.parent.MapWindow.overlays[self.ovlId] = { 'layer' : overlay,
                                                           'params' : '',
                                                           'propwin' : None,
                                                           'cmd' : [self.cmd],
                                                           'coords': (10, 10),
                                                           'pdcType': 'image' }

    def OnOptions(self, event):
        """        self.SetSizer(sizer)
        sizer.Fit(self)

        Sets option for decoration map overlays
        """
        if not self.parent.MapWindow.overlays.has_key(self.ovlId) or \
                self.parent.MapWindow.overlays[self.ovlId]['propwin'] is None:
            # display properties dialog
            menuform.GUI().ParseCommand(cmd=[self.cmd],
                                        completed=(self.GetOptData, self.name, ''),
                                        parentframe=self.parent)
        else:
            if self.parent.MapWindow.overlays[self.ovlId]['propwin'].IsShown():
                self.parent.MapWindow.overlays[self.ovlId]['propwin'].SetFocus()
            else:
                self.parent.MapWindow.overlays[self.ovlId]['propwin'].Show()
        
    def OnCancel(self, event):
        """Cancel dialog"""
        self.parent.dialogs['barscale'] = None

        self.Destroy()

    def OnOK(self, event):
        """Button 'OK' pressed"""
        # enable or disable overlay
        self.parent.Map.GetOverlay(self.ovlId).SetActive(self.chkbox.IsChecked())

        # update map
        self.parent.MapWindow.UpdateMap()

        # close dialog
        self.OnCancel(None)

    def GetOptData(self, dcmd, layer, params, propwin):
        """Process decoration layer data"""
        # update layer data
        if params:
            self.parent.MapWindow.overlays[self.ovlId]['params'] = params
        if dcmd:
            self.parent.MapWindow.overlays[self.ovlId]['cmd'] = dcmd
        self.parent.MapWindow.overlays[self.ovlId]['propwin'] = propwin

        # change parameters for item in layers list in render.Map
        self.parent.Map.ChangeOverlay(id=self.ovlId, type=self.name,
                                      command=self.parent.MapWindow.overlays[self.ovlId]['cmd'],
                                      l_active=self.parent.MapWindow.overlays[self.ovlId]['layer'].IsActive(),
                                      l_render=False, l_hidden=True)

class TextLayerDialog(wx.Dialog):
    """
    Controls setting options and displaying/hiding map overlay decorations
    """

    def __init__(self, parent, ovlId, title, name='text',
                 pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.DEFAULT_DIALOG_STYLE):

        wx.Dialog.__init__(self, parent, wx.ID_ANY, title, pos, size, style)

        self.ovlId = ovlId
        self.parent = parent

        if self.ovlId in self.parent.MapWindow.textdict:
            self.currText, self.currFont, self.currClr, self.currRot = self.parent.MapWindow.textdict[drawid]
        else:
            self.currClr = wx.BLACK
            self.currText = ''
            self.currFont = self.GetFont()
            self.currRot = 0.0

        sizer = wx.BoxSizer(wx.VERTICAL)
        box = wx.GridBagSizer(vgap=5, hgap=5)

        # text entry
        label = wx.StaticText(parent=self, id=wx.ID_ANY, label=_("Enter text:"))
        box.Add(item=label,
                flag=wx.ALIGN_CENTER_VERTICAL,
                pos=(0, 0))

        self.textentry = wx.TextCtrl(parent=self, id=wx.ID_ANY, value="", size=(300,-1))
        self.textentry.SetFont(self.currFont)
        self.textentry.SetForegroundColour(self.currClr)
        self.textentry.SetValue(self.currText)
        box.Add(item=self.textentry,
                pos=(0, 1))

        # rotation
        label = wx.StaticText(parent=self, id=wx.ID_ANY, label=_("Rotation:"))
        box.Add(item=label,
                flag=wx.ALIGN_CENTER_VERTICAL,
                pos=(1, 0))
        self.rotation = wx.SpinCtrl(parent=self, id=wx.ID_ANY, value="", pos=(30, 50),
                                    size=(75,-1), style=wx.SP_ARROW_KEYS)
        self.rotation.SetRange(-360, 360)
        self.rotation.SetValue(int(self.currRot))
        box.Add(item=self.rotation,
                flag=wx.ALIGN_RIGHT,
                pos=(1, 1))

        # font
        fontbtn = wx.Button(parent=self, id=wx.ID_ANY, label=_("Set font"))
        box.Add(item=fontbtn,
                flag=wx.ALIGN_RIGHT,
                pos=(2, 1))

        sizer.Add(item=box, proportion=1,
                  flag=wx.ALL, border=10)

        # note
        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(parent=self, id=wx.ID_ANY,
                              label=_("Drag text with mouse in pointer mode "
                                      "to position.\nDouble-click to change options"))
        box.Add(item=label, proportion=0,
                flag=wx.ALIGN_CENTRE | wx.ALL, border=5)
        sizer.Add(item=box, proportion=0,
                  flag=wx.EXPAND | wx.ALIGN_CENTER_VERTICAL | wx.ALIGN_CENTER | wx.ALL, border=5)

        line = wx.StaticLine(parent=self, id=wx.ID_ANY,
                             size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(item=line, proportion=0,
                  flag=wx.EXPAND | wx.ALIGN_CENTRE | wx.ALL, border=5)

        btnsizer = wx.StdDialogButtonSizer()

        btn = wx.Button(parent=self, id=wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(parent=self, id=wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(item=btnsizer, proportion=0,
                  flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)

        self.SetSizer(sizer)
        sizer.Fit(self)

        # bindings
        self.Bind(wx.EVT_BUTTON,     self.OnSelectFont, fontbtn)
        self.Bind(wx.EVT_TEXT,       self.OnText,       self.textentry)
        self.Bind(wx.EVT_SPINCTRL,   self.OnRotation,   self.rotation)

    def OnText(self, event):
        """Change text string"""
        self.currText = event.GetString()

    def OnRotation(self, event):
        """Change rotation"""
        self.currRot = event.GetInt()

        event.Skip()

    def OnSelectFont(self, event):
        """Change font"""
        data = wx.FontData()
        data.EnableEffects(True)
        data.SetColour(self.currClr)         # set colour
        data.SetInitialFont(self.currFont)

        dlg = wx.FontDialog(self, data)

        if dlg.ShowModal() == wx.ID_OK:
            data = dlg.GetFontData()
            self.currFont = data.GetChosenFont()
            self.currClr = data.GetColour()

            self.textentry.SetFont(self.currFont)
            self.textentry.SetForegroundColour(self.currClr)

            self.Layout()

        dlg.Destroy()

    def GetValues(self):
        """Get text properties"""
        return (self.currText, self.currFont,
                self.currClr, self.currRot)
