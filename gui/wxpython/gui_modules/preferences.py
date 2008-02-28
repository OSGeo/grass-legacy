"""
@package preferences

@brief User preferences dialog

Sets default display font, etc.

Classes:
 - PreferencesDialog
 - SetDefaultFont
 - MapsetAccess

(C) 2007-2008 by the GRASS Development Team
This program is free software under the GNU General Public
License (>=v2). Read the file COPYING that comes with GRASS
for details.

@author Michael Barton (Arizona State University)
Martin Landa <landa.martin gmail.com>
"""

import os
import sys
import copy

import wx
import wx.lib.filebrowsebutton as filebrowse
from wx.lib.wordwrap import wordwrap

import gcmd
import grassenv
import utils

class Settings:
    """Generic class where to store settings"""
    def __init__(self):
        # filename for settings
        self.fileName = ".grasswx"

        # default settings
        self.defaultSettings = {
            #
            # general
            #
            'general': {
            'displayFont' : { 'value' : '' },
            'mapsetPath'  : { 'value' : 'p' }, # current mapset search path
            },
            #
            # advanced
            #
            'advanced' : {
            'settingsFile'   : { 'value' : 'gisdbase' }, # gisdbase, location, mapset
            'digitInterface' : { 'value' : 'vdigit' }, # vedit, vdigit
            'iconTheme'      : { 'value' : 'silk' }, # grass, silk
            },
            #
            # vdigit
            #
            'vdigit' : {
            # symbology
            'symbolHighlight'   : { 'enabled' : None,  'color' : (255, 255, 0, 255) }, #yellow
            'symbolPoint'       : { 'enabled' : True,  'color' : (0, 0, 0, 255) }, # black
            'symbolLine'        : { 'enabled' : True,  'color' : (0, 0, 0, 255) }, # black
            'symbolBoundaryNo'  : { 'enabled' : True,  'color' : (126, 126, 126, 255) }, # grey
            'symbolBoundaryOne' : { 'enabled' : True,  'color' : (0, 255, 0, 255) }, # green
            'symbolBoundaryTwo' : { 'enabled' : True,  'color' : (255, 135, 0, 255) }, # orange
            'symbolCentroidIn'  : { 'enabled' : True,  'color' : (0, 0, 255, 255) }, # blue
            'symbolCentroidOut' : { 'enabled' : True,  'color' : (165, 42, 42, 255) }, # brown
            'symbolCentroidDup' : { 'enabled' : True,  'color' : (156, 62, 206, 255) }, # violet
            'symbolNodeOne'     : { 'enabled' : True,  'color' : (255, 0, 0, 255) }, # red
            'symbolNodeTwo'     : { 'enabled' : True,  'color' : (0, 86, 45, 255) }, # dark green
            'symbolVertex'      : { 'enabled' : False, 'color' : (255, 20, 147, 255) }, # deep pink
            # display
            'lineWidth' : { 'value' : 2, 'units' : 'screen pixels' },
            # snapping
            'snapping' : { 'value' : 10, 'units' : 'screen pixels' },
            'snapToVertex' : { 'enabled' : False },
            'backgroundMap' : {'value' : ''},
            # digitize new record
            'addRecord' : { 'enabled' : True },
            'layer' : {'value' : 1 },
            'category' : {'value' : 1 },
            'categoryMode' : {'value' : 'Next to use' },
            # delete existing feature(s)
            'delRecord' : { 'enabled' : True },
            # query tool
            'query'       : { 'type' : 'length', 'box' : True },
            'queryLength' : { 'than' : 'shorter than', 'thresh' : 0 },
            'queryDangle' : { 'than' : 'shorter than', 'thresh' : 0 },
            # select feature (point, line, centroid, boundary)
            'selectFeaturePoint'    : { 'enabled' : True },
            'selectFeatureLine'     : { 'enabled' : True },
            'selectFeatureCentroid' : { 'enabled' : True },
            'selectFeatureBoundary' : { 'enabled' : True },
            'selectThresh'          : { 'value' : 10, 'units' : 'screen pixels'},
            }
        }
        
        # user settings
        self.userSettings = copy.deepcopy(self.defaultSettings)
        try:
            self.ReadSettingsFile()
        except IOError, e:
            raise gcmd.SettingsError(e)
        except:
            gcmd.SettingsError('Reading settings failed.')

        # internal settings (based on user settings)
        self.internalSettings = {}
        self.internalSettings['general'] = {}
        self.internalSettings['general']["mapsetPath"] = {}
        self.internalSettings['general']["mapsetPath"]['value'] = self.GetMapsetPath()

    def GetMapsetPath(self):
        """Store mapset search path"""
        all, access = utils.ListOfMapsets()

        if self.Get(group='general', key='mapsetPath', subkey='value') == 'p':
            return access
        else:
            return all
    
    def ReadSettingsFile(self, settings=None):
        """Reads settings file (mapset, location, gisdbase)"""
        if settings is None:
            settings = self.userSettings

        # look for settings file
        # -> mapser
        #  -> location
        #   -> gisdbase
        gisdbase = grassenv.GetGRASSVariable("GISDBASE")
        location_name = grassenv.GetGRASSVariable("LOCATION_NAME")
        mapset_name = grassenv.GetGRASSVariable("MAPSET")

        mapset_file = os.path.join(gisdbase, location_name, mapset_name, self.fileName)
        location_file = os.path.join(gisdbase, location_name, self.fileName)
        gisdbase_file = os.path.join(gisdbase, self.fileName)

        if os.path.isfile(mapset_file):
            self.__ReadFile(mapset_file)
        elif os.path.isfile(location_file):
            self.__ReadFile(location_file)
        elif os.path.isfile(gisdbase_file):
            self.__ReadFile(gisdbase_file)

    def __ReadFile(self, filename, settings=None):
        """Read settings from file to dict"""
        if settings is None:
            settings = self.userSettings

        try:
            # print '#', filename
            file = open(filename, "r")
            for line in file.readlines():
                line = line.rstrip('%s' % os.linesep)
                group, key = line.split(':')[0:2]
                kv = line.split(':')[2:]
                idx = 0
                while idx < len(kv):
                    # print group, key, kv[idx], kv[idx+1]
                    # settings.Set(grou=group, key=key, subkey=kv[idx], value=kv[idx+1])
                    idx += 2
        except IOError, e:
            raise gcmd.SettingsError(e)
        except:
            raise gcmd.SettingsError(_('Reading settings from file <%s> failed.') % filename)

        file.close()

    def SaveToFile(self, settings=None):
        """Save settings to the file"""
        if settings is None:
            settings = self.userSettings
        
        loc = self.Get(group='advanced', key='settingsFile', subkey='value')
        gisdbase = grassenv.GetGRASSVariable("GISDBASE")
        location_name = grassenv.GetGRASSVariable("LOCATION_NAME")
        mapset_name = grassenv.GetGRASSVariable("MAPSET")
        filePath = None
        if loc == 'gisdbase':
            filePath = os.path.join(gisdbase, self.fileName)
        elif loc == 'location':
            filePath = os.path.join(gisdbase, location_name, self.fileName)
        elif loc == 'mapset':
            filePath = os.path.join(gisdbase, location_name, mapset_name, self.fileName)
        
        if filePath is None:
            raise gcmd.SettingsError('Uknown file location.')

        try:
            file = open(filePath, "w")
            for group in settings.keys():
                for item in settings[group].keys():
                    file.write('%s:%s:' % (group, item))
                    items = settings[group][item].keys()
                    for idx in range(len(items)):
                        file.write('%s:%s' % (items[idx], settings[group][item][items[idx]]))
                        if idx < len(items) - 1:
                            file.write(':')
                    file.write('%s' % os.linesep)
        except IOError, e:
            raise gcmd.SettingsError(e)
        except:
            raise gcmd.SettingsError('Writing settings to file <%s> failed.' % filePath)

        file.close()

        return filePath

    def Get(self, group, key, subkey=None, internal=False):
        """Get value by key/subkey

        Raise KeyError if key is not found
        
        @param group settings group
        @param key
        @param subkey if not given return dict of key
        
        @return value

        """
        if internal is True:
            settings = self.internalSettings
        else:
            settings = self.userSettings
            
        if settings.has_key(group) and settings[group].has_key(key):
            if subkey is None:
                return settings[group][key]
            else:
                if settings[group][key].has_key(subkey):
                    return settings[group][key][subkey]
                else:
                    raise KeyError
        else:
            raise KeyError
        
        return None
    
    def Set(self, group, key, subkey, value, internal=False):
        """Set value by key/subkey

        Raise KeyError if key is not found
        
        @param group settings group
        @param key
        @param subkey
        @param value 
        """
        if internal is True:
            settings = self.internalSettings
        else:
            settings = self.userSettings
            
        if settings.has_key(group) and settings[group].has_key(key) and \
               settings[group][key].has_key(subkey):
            settings[group][key][subkey] = value
        else:
            raise KeyError

globalSettings = Settings()

class PreferencesDialog(wx.Dialog):
    """User preferences dialog"""
    def __init__(self, parent, title,
                 settings=globalSettings,
                 style=wx.DEFAULT_DIALOG_STYLE):
        self.parent = parent # GMFrame
        self.title = title
        wx.Dialog.__init__(self, parent=parent, id=wx.ID_ANY, title=title,
                           style=style)

        self.settings = settings
        # notebook
        notebook = wx.Notebook(parent=self, id=wx.ID_ANY, style=wx.BK_DEFAULT)

        # create notebook pages
        self.__CreateGeneralPage(notebook)
        self.__CreateAdvancedPage(notebook)

        # buttons
        btnSave = wx.Button(self, wx.ID_SAVE)
        btnApply = wx.Button(self, wx.ID_APPLY)
        btnCancel = wx.Button(self, wx.ID_CANCEL)
        # btnOk = wx.Button(self, wx.ID_OK)
        btnSave.SetDefault()

        # bindigs
        btnApply.Bind(wx.EVT_BUTTON, self.OnApply)
        btnSave.Bind(wx.EVT_BUTTON, self.OnSave)
        btnCancel.Bind(wx.EVT_BUTTON, self.OnCancel)

        # sizers
        btnSizer = wx.StdDialogButtonSizer()
        btnSizer.AddButton(btnCancel)
        btnSizer.AddButton(btnSave)
        btnSizer.AddButton(btnApply)
        # btnSizer.AddButton(btnOk)
        btnSizer.Realize()
        
        mainSizer = wx.BoxSizer(wx.VERTICAL)
        mainSizer.Add(item=notebook, proportion=1, flag=wx.EXPAND | wx.ALL, border=5)
        mainSizer.Add(item=btnSizer, proportion=0,
                      flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)

        self.SetSizer(mainSizer)
        mainSizer.Fit(self)

    def __CreateGeneralPage(self, notebook):
        """Create notebook page concerning with symbology settings"""
        panel = wx.Panel(parent=notebook, id=wx.ID_ANY)
        notebook.AddPage(page=panel, text=_("General"))

        border = wx.BoxSizer(wx.VERTICAL)
        box   = wx.StaticBox (parent=panel, id=wx.ID_ANY, label=" %s " % _("General settings"))
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)

        gridSizer = wx.GridBagSizer (hgap=3, vgap=3)
        gridSizer.AddGrowableCol(0)

        #
        # display font
        #
        gridSizer.Add(item=wx.StaticText(parent=panel, id=wx.ID_ANY,
                                         label=_("Default font for GRASS displays:")),
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL,
                       pos=(0, 0))
        fontButton = wx.Button(parent=panel, id=wx.ID_ANY,
                               label=_("Set font"), size=(100, -1))
        gridSizer.Add(item=fontButton,
                      flag=wx.ALIGN_RIGHT |
                       wx.ALIGN_CENTER_VERTICAL,
                       pos=(0, 1))
        gridSizer.Add(item=wx.StaticText(parent=panel, id=wx.ID_ANY,
                                         label=_("Currently selected font:")),
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL,
                       pos=(1, 0), span=(1, 2))

        #
        # mapsets path
        # 
        gridSizer.Add(item=wx.StaticText(parent=panel, id=wx.ID_ANY,
                                         label=_("Mapsets path:")),
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL,
                       pos=(2, 0))
        self.mapsetPath = wx.Choice(parent=panel, id=wx.ID_ANY, size=(200, -1),
                                      choices=['mapset search path', 'all available mapsets'])
        if self.settings.Get(group='general', key='mapsetPath', subkey='value') == 'p':
            self.mapsetPath.SetSelection(0)
        else:
            self.mapsetPath.SetSelection(1)
        gridSizer.Add(item=self.mapsetPath,
                      flag=wx.ALIGN_RIGHT |
                      wx.ALIGN_CENTER_VERTICAL,
                      pos=(2, 1))
        
        sizer.Add(item=gridSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=5)
        border.Add(item=sizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=3)

        panel.SetSizer(border)
        
        # bindings
        fontButton.Bind(wx.EVT_BUTTON, self.OnSetFont)
        self.mapsetPath.Bind(wx.EVT_CHOICE, self.OnChangeMapsetPath)
        
        return panel

    def __CreateAdvancedPage(self, notebook):
        """Create notebook page concerning with symbology settings"""
        panel = wx.Panel(parent=notebook, id=wx.ID_ANY)
        notebook.AddPage(page=panel, text=_("Advanced"))

        border = wx.BoxSizer(wx.VERTICAL)
        box   = wx.StaticBox (parent=panel, id=wx.ID_ANY, label=" %s " % _("Advanced settings"))
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)

        gridSizer = wx.GridBagSizer (hgap=3, vgap=3)
        gridSizer.AddGrowableCol(0)

        row = 0

        #
        # place where to store settings
        #
        gridSizer.Add(item=wx.StaticText(parent=panel, id=wx.ID_ANY,
                                         label=_("Place where to store settings:")),
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL,
                       pos=(row, 0))
        self.settingsFile = wx.Choice(parent=panel, id=wx.ID_ANY, size=(125, -1),
                                      choices=['gisdbase', 'location', 'mapset'])
        self.settingsFile.SetStringSelection(self.settings.Get(group='advanced', key='settingsFile', subkey='value'))
        gridSizer.Add(item=self.settingsFile,
                      flag=wx.ALIGN_RIGHT |
                      wx.ALIGN_CENTER_VERTICAL,
                      pos=(row, 1))
        row += 1

        #
        # icon theme
        #
        gridSizer.Add(item=wx.StaticText(parent=panel, id=wx.ID_ANY,
                                         label=_("Icon theme:")),
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL,
                       pos=(row, 0))
        self.iconTheme = wx.Choice(parent=panel, id=wx.ID_ANY, size=(125, -1),
                                   choices=['grass', 'silk'])
        self.iconTheme.SetStringSelection(self.settings.Get(group='advanced', key='iconTheme', subkey='value'))
        gridSizer.Add(item=self.iconTheme,
                      flag=wx.ALIGN_RIGHT |
                      wx.ALIGN_CENTER_VERTICAL,
                      pos=(row, 1))
        
        row += 1
        iconNote = wordwrap(_("Note: Requires GUI restart."),
                            self.GetSize()[0]-50, wx.ClientDC(self))

        gridSizer.Add(item=wx.StaticText(parent=panel, id=wx.ID_ANY,
                                         label=iconNote),
                      flag=wx.ALIGN_LEFT |
                      wx.ALIGN_CENTER_VERTICAL,
                      pos=(row, 0), span=(1, 2))
        row += 1
        
        #
        # digitization interface
        #
        gridSizer.Add(item=wx.StaticText(parent=panel, id=wx.ID_ANY,
                                         label=_("Digitization interface:")),
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL,
                       pos=(row, 0))
        self.digitInterface = wx.Choice(parent=panel, id=wx.ID_ANY, size=(125, -1),
                                        choices=['vdigit', 'vedit'])
        self.digitInterface.SetStringSelection(self.settings.Get(group='advanced', key='digitInterface', subkey='value'))
        gridSizer.Add(item=self.digitInterface,
                      flag=wx.ALIGN_RIGHT |
                      wx.ALIGN_CENTER_VERTICAL,
                      pos=(row, 1))
        row += 1

        digitNote = wordwrap(_("Note: User can choose from two interfaces for digitization. "
                               "The simple one uses v.edit command on the background. "
                               "Map topology is rebuild on each operation which can "
                               "significantly slow-down response. The vdigit is a native "
                               "interface which uses v.edit functionality, but doesn't "
                               "call the command itself."),
                             self.GetSize()[0]-50, wx.ClientDC(self))

        gridSizer.Add(item=wx.StaticText(parent=panel, id=wx.ID_ANY,
                                         label=digitNote),
                      flag=wx.ALIGN_LEFT |
                      wx.ALIGN_CENTER_VERTICAL,
                      pos=(row, 0), span=(1, 2))

        sizer.Add(item=gridSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=5)
        border.Add(item=sizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=3)

        panel.SetSizer(border)
        
        return panel

    def OnSetFont(self, event):
        """'Set font' button pressed"""
        dlg = SetDefaultFont(parent=self, id=wx.ID_ANY,
                             title=_('Select default display font'),
                             pos=wx.DefaultPosition, size=wx.DefaultSize,
                             style=wx.DEFAULT_DIALOG_STYLE,
                             encoding=self.parent.encoding)
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

        event.Skip()

    def OnChangeMapsetPath(self, event):
        """Mapset path changed"""
        if event.GetSelection() == 0:
            self.settings.Set(group='general', key='mapsetPath', subkey='value', value='p')
        else:
            self.settings.Set(group='general', key='mapsetPath', subkey='value', value='l')

        # update internal settings
        self.settings.Set(group='general', key="mapsetPath", subkey='value', value=self.settings.GetMapsetPath(), internal=True)
        
    def OnSave(self, event):
        """Button 'Save' clicked"""
        self.__UpdateSettings()
        file = self.settings.SaveToFile()
        self.parent.goutput.cmd_stdout.write('Settings saved to file <%s>.' % file)
        self.Close()

    def OnApply(self, event):
        """Button 'Apply' clicked"""
        self.__UpdateSettings()
        
    def OnCancel(self, event):
        """Button 'Cancel' clicked"""
        self.Close()

    def __UpdateSettings(self):
        """Update user settings"""
        # font
        # TODO

        # location
        self.settings.Set(group='advanced', key='settingsFile', subkey='value', value=self.settingsFile.GetStringSelection())

        # icon theme
        self.settings.Set(group='advanced', key='iconTheme', subkey='value', value=self.iconTheme.GetStringSelection())
        
        # digitization interface
        self.settings.Set(group='advanced', key='digitInterface', subkey='value', value=self.digitInterface.GetStringSelection())

class SetDefaultFont(wx.Dialog):
    """
    Opens a file selection dialog to select default font
    to use in all GRASS displays
    """
    def __init__(self, parent, id, title, pos=wx.DefaultPosition, size=wx.DefaultSize,
            style=wx.DEFAULT_DIALOG_STYLE, encoding='ISO-8859-1'):
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)

        if "GRASS_FONT" in os.environ:
            self.font = os.environ["GRASS_FONT"]
        else:
            self.font = None

        self.fontlist = self.GetFonts()

        self.encoding = encoding

        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Select Font:", (15, 50))
        box.Add(label, 0, wx.EXPAND|wx.GROW|wx.ALIGN_TOP|wx.RIGHT, 5)
        self.fontlb = wx.ListBox(self, wx.ID_ANY, pos=wx.DefaultPosition,
                                 size=(280,150), choices=self.fontlist,
                                 style=wx.LB_SINGLE|wx.LB_SORT)
        self.Bind(wx.EVT_LISTBOX, self.EvtListBox, self.fontlb)
        self.Bind(wx.EVT_LISTBOX_DCLICK, self.EvtListBoxDClick, self.fontlb)
        if self.font:
            self.fontlb.SetStringSelection(self.font, True)
        box.Add(self.fontlb, 0, wx.EXPAND|wx.GROW|wx.ALIGN_RIGHT)
        sizer.Add(box, 0, wx.EXPAND|wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 8)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Character encoding:")
        box.Add(label, 0, wx.ALIGN_RIGHT|wx.RIGHT, 5)
        self.textentry = wx.TextCtrl(self, -1, "", size=(200,-1))
        self.textentry.SetValue(self.encoding)
        box.Add(self.textentry, 0, wx.ALIGN_LEFT)
        self.textentry.Bind(wx.EVT_TEXT, self.OnEncoding)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 8)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 10)

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

    def EvtRadioBox(self, event):
        if event.GetInt() == 0:
            self.fonttype = 'grassfont'
        elif event.GetInt() == 1:
            self.fonttype = 'truetype'

        self.fontlist = self.GetFonts(self.fonttype)
        self.fontlb.SetItems(self.fontlist)

    def OnEncoding(self, event):
        self.encoding = event.GetString()

    def EvtListBox(self, event):
        self.font = event.GetString()
        event.Skip()

    def EvtListBoxDClick(self, event):
        self.font = event.GetString()
        event.Skip()

    def GetFonts(self):
        """
        parses fonts directory or fretypecap file to get a list of fonts for the listbox
        """
        fontlist = []

        cmd = ["d.font", "-l"]

        p = gcmd.Command(cmd, stderr=None)

        dfonts = p.ReadStdOutput()
        dfonts.sort(lambda x,y: cmp(x.lower(), y.lower()))
        for item in range(len(dfonts)):
           # ignore duplicate fonts and those starting with #
           if not dfonts[item].startswith('#') and \
                  dfonts[item] != dfonts[item-1]:
              fontlist.append(dfonts[item])

        return fontlist

class MapsetAccess(wx.Dialog):
    """
    Controls setting options and displaying/hiding map overlay decorations
    """
    def __init__(self, parent, id, title=_('Set/unset access to mapsets in current location'),
                 pos=wx.DefaultPosition, size=(-1, -1),
                 style=wx.DEFAULT_DIALOG_STYLE|wx.RESIZE_BORDER):
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)

        self.all_mapsets, self.accessible_mapsets = utils.ListOfMapsets()
        self.curr_mapset = grassenv.GetGRASSVariable('MAPSET')

        # make a checklistbox from available mapsets and check those that are active
        sizer = wx.BoxSizer(wx.VERTICAL)

        label = wx.StaticText(parent=self, id=wx.ID_ANY,
                              label=_("Check mapset to make it accessible, uncheck it to hide it.%s"
                                      "Note: PERMANENT and current mapset are always accessible.") % os.linesep)
        sizer.Add(item=label, proportion=0,
                  flag=wx.ALL, border=5)

        self.mapsetlb = wx.CheckListBox(parent=self, id=wx.ID_ANY, pos=wx.DefaultPosition,
                                        size=(350,200), choices=self.all_mapsets)
        self.mapsetlb.Bind(wx.EVT_CHECKLISTBOX, self.OnCheckMapset)
        
        sizer.Add(item=self.mapsetlb, proportion=1,
                  flag=wx.ALL | wx.EXPAND, border=5)

        # check all accessible mapsets
        if globalSettings.Get(group='general', key='mapsetPath', subkey='value') == 'l':
            for mset in self.all_mapsets:
                self.mapsetlb.Check(self.all_mapsets.index(mset), True)
        else:
            for mset in self.accessible_mapsets:
                self.mapsetlb.Check(self.all_mapsets.index(mset), True)

        # dialog buttons
        line = wx.StaticLine(parent=self, id=wx.ID_ANY,
                             style=wx.LI_HORIZONTAL)
        sizer.Add(item=line, proportion=0,
                  flag=wx.EXPAND | wx.ALIGN_CENTRE | wx.ALL, border=5)

        btnsizer = wx.StdDialogButtonSizer()
        okbtn = wx.Button(self, wx.ID_OK)
        okbtn.SetDefault()
        btnsizer.AddButton(okbtn)

        cancelbtn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(cancelbtn)
        btnsizer.Realize()

        sizer.Add(item=btnsizer, proportion=0,
                  flag=wx.EXPAND | wx.ALIGN_RIGHT | wx.ALL, border=5)

        # do layout
        self.Layout()
        self.SetSizer(sizer)
        sizer.Fit(self)

        self.SetMinSize(self.GetBestSize())
        
    def OnCheckMapset(self, event):
        """Mapset checked/unchecked"""
        mapset = self.mapsetlb.GetString(event.GetSelection())
        if mapset == 'PERMANENT' or mapset == self.curr_mapset:
            self.mapsetlb.Check(event.GetSelection(), True)
        
    def GetMapsets(self):
        """Get list of checked mapsets"""
        ms = []
        i = 0
        for mset in self.all_mapsets:
            if self.mapsetlb.IsChecked(i):
                ms.append(mset)
            i += 1

        return ms
