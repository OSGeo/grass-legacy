"""
MODULE:    location_wizard.py

CLASSES:
    * BaseClass
    * TitledPage
    * DatabasePage
    * CoordinateSystemPage
    * ProjectionsPage
    * ItemList
    * ProjTypePage
    * DatumPage
    * EllipsePage
    * GeoreferencedFilePage
    * EPSGPage
    * CustomPage
    * SummaryPage
    * RegionDef
    * LocationWizard

PURPOSE:   Create a new GRASS Location. User can choose from multiple methods.

AUTHORS:   The GRASS Development Team
           Michael Barton
           Jachym Cepicky
           Various updates: Martin Landa <landa.martin gmail.com>
           
COPYRIGHT: (C) 2006-2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""
import os
import shutil
import re
import string
import sys

import wx
import wx.lib.mixins.listctrl as listmix
import wx.wizard as wiz

import gcmd
try:
    import subprocess
except:
    from compat import subprocess

global coordsys
global north
global south
global east
global west
global resolution
global wizerror

coordsys = ''
north = ''
south = ''
east = ''
west = ''
resolution = ''

class BaseClass(wx.Object):
    """Base class providing basic methods"""
    def __init__(self):
        pass

    def MakeLabel(self, text="", style=wx.ALIGN_RIGHT):
        """Make aligned label"""
        return wx.StaticText(parent=self, id=wx.ID_ANY, label=text,
                             style=style)

    def MakeTextCtrl(self, text='', size=(100,-1), style=0):
        """Generic text control"""
        return wx.TextCtrl(parent=self, id=wx.ID_ANY, value=text,
                           size=size, style=style)

    def MakeButton(self, text, size=(-1,-1)):
        """Generic button"""
        return wx.Button(parent=self, id=wx.ID_ANY, label=text,
                         size=size)

class TitledPage(BaseClass, wiz.WizardPageSimple):
    """
    Class to make wizard pages. Generic methods to make
    labels, text entries, and buttons.
    """
    def __init__(self, parent, title):
        wiz.WizardPageSimple.__init__(self, parent)

        # page title
        self.title = wx.StaticText(parent=self, id=wx.ID_ANY, label=title)
        self.title.SetFont(wx.Font(13, wx.SWISS, wx.NORMAL, wx.BOLD))

        # main sizer
        self.sizer = wx.GridBagSizer(vgap=0, hgap=0)

    def DoLayout(self):
        """Do page layout"""
        tmpsizer = wx.BoxSizer(wx.VERTICAL)

        tmpsizer.Add(item=self.title, proportion=0,
                     flag=wx.ALIGN_CENTRE | wx.ALL,
                     border=5)
        tmpsizer.Add(item=wx.StaticLine(self, -1), proportion=0,
                     flag=wx.EXPAND | wx.ALL,
                     border=0)
        tmpsizer.Add(item=self.sizer, proportion=1,
                     flag=wx.EXPAND | wx.ALL,
                     border=5)

        self.SetAutoLayout(True)
        self.SetSizer(tmpsizer)
        self.Layout()

class DatabasePage(TitledPage):
    """
    Wizard page for setting GIS data directory and location name
    """
    def __init__(self, wizard, parent, grassdatabase):
        TitledPage.__init__(self, wizard, _("Define GRASS Database and Location Name"))

        self.grassdatabase = grassdatabase
        self.location = ''

        # buttons
        self.bbrowse = self.MakeButton(_("Browse"))

        # text controls
        self.tgisdbase = self.MakeTextCtrl(grassdatabase, size=(300, -1))
        self.tlocation = self.MakeTextCtrl("newLocation", size=(300, -1))
        
        # layout
        self.sizer.Add(item=self.MakeLabel(_("GIS Data Directory:")),
                       flag=wx.ALIGN_RIGHT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5,
                       pos=(1, 1))
        self.sizer.Add(item=self.tgisdbase,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5,
                       pos=(1, 2))
        self.sizer.Add(item=self.bbrowse,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5,
                       pos=(1, 3))
        #
        self.sizer.Add(item=self.MakeLabel("%s:" % _("Project Location")),
                       flag=wx.ALIGN_RIGHT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5,
                       pos=(2, 1))
        self.sizer.Add(item=self.tlocation,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5,
                       pos=(2, 2))
        # self.sizer.Add(item=self.MakeLabel(_("(projection/coordinate system)")),
        #                flag=wx.ALIGN_LEFT |
        #                wx.ALIGN_CENTER_VERTICAL |
        #                wx.ALL, border=5,
        #                pos=(2, 4))

        # bindings
        self.Bind(wx.EVT_BUTTON,                self.OnBrowse, self.bbrowse)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.OnPageChanging)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED,  self.OnPageChanged)
        self.tgisdbase.Bind(wx.EVT_TEXT,        self.OnChangeName)
        self.tlocation.Bind(wx.EVT_TEXT,        self.OnChangeName)

        # do page layout
        # self.DoLayout()

    def OnChangeName(self, event):
        """Name for new location was changed"""
        nextButton = wx.FindWindowById(wx.ID_FORWARD)
        if len(event.GetString()) > 0:
            if not nextButton.IsEnabled():
                nextButton.Enable()
        else:
            nextButton.Disable()

        event.Skip()

    def OnBrowse(self, event):
        dlg = wx.DirDialog(self, _("Choose GRASS data directory:"),
                           os.getcwd(), wx.DD_DEFAULT_STYLE)
        if dlg.ShowModal() == wx.ID_OK:
            self.grassdatabase = dlg.GetPath()
            self.tgisdbase.SetValue(self.grassdatabase)
            
        dlg.Destroy()

    def OnPageChanging(self,event=None):
        error = ''
        if os.path.isdir(os.path.join(self.tgisdbase.GetValue(), self.tlocation.GetValue())):
            error = _("Location already exists in GRASS Database.")

        if error != '':
            dlg = wx.MessageDialog(parent=self, message=_("Unable to create location <%s>.%s"
                                                          "%s" % \
                                                              (str(self.tlocation.GetValue()),
                                                               os.linesep,
                                                               error)),
                                   caption=_("Error"),  style=wx.OK | wx.ICON_ERROR)
            
            dlg.ShowModal()
            dlg.Destroy()
            event.Veto()
            return

        #         if not self.tlocation.GetValue():
        #             dlg = wx.MessageDialog(parent=self, message=_("Unable to create new location: location not set "
        #                     ,"Could not create location",  wx.OK|wx.ICON_INFORMATION)
        #                                    dlg.ShowModal()
        #             dlg.Destroy()
        #             event.Veto()
        #             return

        self.location = self.tlocation.GetValue()
        self.grassdatabase = self.tgisdbase.GetValue()

    def OnPageChanged(self, event=None):
        """Wizard page changed"""
        self.grassdatabase = self.tgisdbase.GetValue()
        self.location = self.tlocation.GetValue()

class CoordinateSystemPage(TitledPage):
    """
    Wizard page for choosing method for location creation
    """
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, _("Choose method for creating a new location"))

        self.parent = parent
        global coordsys

        # toggles
        self.radio1 = wx.RadioButton(parent=self, id=wx.ID_ANY,
                                     label=_("Select coordinate system"),
                                     style = wx.RB_GROUP)
        self.radio2 = wx.RadioButton(parent=self, id=wx.ID_ANY,
                                     label=_("Select EPSG code for coordinate system"))
        self.radio3 = wx.RadioButton(parent=self, id=wx.ID_ANY,
                                     label=_("Use coordinate system of selected "
                                             "georeferenced file"))
        self.radio4 = wx.RadioButton(parent=self, id=wx.ID_ANY,
                                     label=_("Create custom PROJ.4 parameters "
                                             "string for coordinate system"))
        self.radio5 = wx.RadioButton(parent=self, id=wx.ID_ANY,
                                     label=_("Create arbitrary non-earth "
                                             "coordinate system (XY)"))
        # layout
        self.sizer.Add(item=self.radio1,
                       flag=wx.ALIGN_LEFT, pos=(1, 1))
        self.sizer.Add(item=self.radio2,
                       flag=wx.ALIGN_LEFT, pos=(2, 1))
        self.sizer.Add(item=self.radio3,
                       flag=wx.ALIGN_LEFT, pos=(3, 1))
        self.sizer.Add(item=self.radio4,
                       flag=wx.ALIGN_LEFT, pos=(4, 1))
        self.sizer.Add(item=self.radio5,
                       flag=wx.ALIGN_LEFT, pos=(5, 1))

        # bindings
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio1.GetId())
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio2.GetId())
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio3.GetId())
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio4.GetId())
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio5.GetId())
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED,  self.OnPageChanged)

        # do page layout
        # self.DoLayout()

    def OnPageChanged(self, event):
        global coordsys
        if event.GetDirection() and not coordsys:
            coordsys = "proj"
            self.SetNext(self.parent.projpage)
            self.parent.sumpage.SetPrev(self.parent.datumpage)
            
        if not wx.FindWindowById(wx.ID_FORWARD).IsEnabled():
            wx.FindWindowById(wx.ID_FORWARD).Enable()
    
    def SetVal(self, event):
        """Choose method"""
        global coordsys
        if event.GetId() == self.radio1.GetId():
            coordsys = "proj"
            self.SetNext(self.parent.projpage)
            self.parent.sumpage.SetPrev(self.parent.datumpage)
        elif event.GetId() == self.radio2.GetId():
            coordsys = "epsg"
            self.SetNext(self.parent.epsgpage)
            self.parent.sumpage.SetPrev(self.parent.epsgpage)
        elif event.GetId() == self.radio3.GetId():
            coordsys = "file"
            self.SetNext(self.parent.filepage)
            self.parent.sumpage.SetPrev(self.parent.filepage)
        elif event.GetId() == self.radio4.GetId():
            coordsys = "custom"
            self.SetNext(self.parent.custompage)
            self.parent.sumpage.SetPrev(self.parent.custompage)
        elif event.GetId() == self.radio5.GetId():
            coordsys = "xy"
            self.SetNext(self.parent.sumpage)
            self.parent.sumpage.SetPrev(self.parent.csystemspage)

class ProjectionsPage(TitledPage):
    """
    Wizard page for selecting projection (select coordinate system option)
    """
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, _("Choose projection"))

        self.parent = parent
        self.proj = ''
        self.projdesc = ''

        # text input
        self.tproj = self.MakeTextCtrl("", size=(200,-1))
        
        # search box
        self.searchb = wx.SearchCtrl(self, size=(200,-1),
                                     style=wx.TE_PROCESS_ENTER)

        # projection list
        self.projlist = ItemList(self, data=self.parent.projections.items(),
                                 columns=[_('Code'), _('Description')])
        # layout
        self.sizer.Add(item=self.MakeLabel(_("Projection code:")),
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(1, 1))
        self.sizer.Add(item=self.tproj,
                       flag=wx.ALIGN_RIGHT | wx.EXPAND | wx.ALL,
                       border=5, pos=(1, 2))

        self.sizer.Add(item=self.MakeLabel(_("Search in projection description:")),
                       flag=wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL | wx.ALL,
                       border=5, pos=(2, 1))
        self.sizer.Add(item=self.searchb,
                       flag=wx.ALIGN_RIGHT | wx.EXPAND | wx.ALL,
                       border=5, pos=(2, 2))

        self.sizer.Add(item=self.projlist,
                       flag=wx.EXPAND |
                       wx.ALIGN_LEFT |
                       wx.ALL, border=5, pos=(3, 1), span=(1, 3))

        # events
        self.tproj.Bind(wx.EVT_TEXT, self.OnText)
        self.tproj.Bind(wx.EVT_TEXT_ENTER, self.OnText)
        self.searchb.Bind(wx.EVT_TEXT_ENTER, self.OnSearch)
        self.projlist.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.OnPageChanging)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED,  self.OnEnterPage)

        # do layout
        # self.Layout()

    def OnPageChanging(self,event):
        if event.GetDirection() and self.proj not in self.parent.projections:
            event.Veto()
        if self.proj == 'utm':
            self.parent.projtypepage.text_utm.SetEditable(True)
            self.parent.projtypepage.hemischoices = ['north','south']
        else:
            self.parent.projtypepage.text_utm.SetValue('')
            self.parent.projtypepage.text_utm.SetEditable(False)
            self.parent.projtypepage.hemischoices = []

    def OnText(self, event):
        """Projection name changed"""
        self.proj = event.GetString()
        nextButton = wx.FindWindowById(wx.ID_FORWARD)
        if len(self.proj) == 0 and nextButton.IsEnabled():
            nextButton.Enable(False)
        
        if self.proj in self.parent.projections:
            self.projdesc = self.parent.projections[self.proj]
            if not nextButton.IsEnabled():
                nextButton.Enable()

    def OnEnterPage(self, event):
        if len(self.proj) == 0:
            # disable 'next' button by default
            wx.FindWindowById(wx.ID_FORWARD).Enable(False)
        else:
            wx.FindWindowById(wx.ID_FORWARD).Enable(True)

        event.Skip()
    
    def OnSearch(self, event):
        """Search projection by desc"""
        str = event.GetString()
        try:
            self.proj, self.projdesc = self.projlist.Search(index=1, str=event.GetString())
        except:
            self.proj = self.projdesc = ''

        self.tproj.SetValue(self.proj)

        event.Skip()

    def OnItemSelected(self, event):
        """Projection selected"""
        index = event.m_itemIndex

        # set values
        self.proj = self.projlist.GetItem(index, 0).GetText()
        self.projdesc = self.projlist.GetItem(index, 0).GetText()
        self.tproj.SetValue(self.proj)

class ItemList(wx.ListCtrl,
               listmix.ListCtrlAutoWidthMixin):
    """Generic list (for projections, ellipsoids, etc.)"""

    def __init__(self, parent, data, columns):
        wx.ListCtrl.__init__(self, parent=parent, id=wx.ID_ANY,
                             style=wx.LC_REPORT |
                             wx.LC_HRULES |
                             wx.LC_VRULES |
                             wx.LC_SINGLE_SEL, size=(400, 100))

        # dict to list
        self.sourceData = data

        listmix.ListCtrlAutoWidthMixin.__init__(self)

        i = 0
        for column in columns:
            self.InsertColumn(i, column)
            i += 1

        if self.sourceData:
            self.Populate()
        else:
            for i in range(self.GetColumnCount()):
                self.SetColumnWidth(i, wx.LIST_AUTOSIZE_USEHEADER)

    def Populate(self, data=None):
        """Populate list"""
        if data is None:
            data = self.sourceData
        else:
            self.sourceData = data

        try:
            data.sort()
            self.DeleteAllItems()
            for value in data:
                entry = self.GetItemCount()
                index = self.InsertStringItem(entry, value[0])
                for i in range(1, len(value)):
                    try:
                        self.SetStringItem(index, i, value[i])
                    except:
                        self.SetStringItem(index, i, unicode(value[i], 'latin1'))

            # set column width
            for i in range(self.GetColumnCount()):
                self.SetColumnWidth(i, wx.LIST_AUTOSIZE)
            for i in range(self.GetColumnCount()):
                if self.GetColumnWidth(i) < 80:
                    self.SetColumnWidth(i, 80)

            self.SendSizeEvent()

        except StandardError, e:
            dlg = wx.MessageDialog(parent=self,
                                   message=_("Unable to read list: %s ") % e,
                                   caption=_("Error"),  style=wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()

    def Search (self, index, str):
        """Search projection by name"""
        if str == '':
            self.Populate(self.sourceData)
            return None

        data = []
        for i in range(len(self.sourceData)):
            value = self.sourceData[i][index]
            if str in value.lower():
                data.append(self.sourceData[i])

        self.Populate(data)

        if len(data) > 0:
            return data[0]
        else:
            return None

class ProjTypePage(TitledPage):
    """
    Wizard page for selecting method of setting coordinate system parameters
    (select coordinate system option)
    """

    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, _("Choose method of specifying georeferencing parameters"))
        global coordsys

        self.utmzone = ''
        self.utmhemisphere = ''
        self.hemischoices = ["north","south"]
        self.parent = parent

        self.radio1 = wx.RadioButton(parent=self, id=wx.ID_ANY, label=_("Select datum with associated ellipsoid"),
                                     style = wx.RB_GROUP)
        self.radio2 = wx.RadioButton(parent=self, id=wx.ID_ANY,
                                     label=_("Select ellipsoid"))
        self.title_utm = self.MakeLabel(_("Set zone for UTM projection:"))
        self.text_utm = self.MakeTextCtrl(size=(100,-1))
        self.label_utm = self.MakeLabel(_("Zone:"))
        self.hemisphere = wx.Choice(parent=self, id=wx.ID_ANY, size=(100, -1),
                                    choices=self.hemischoices)
        self.label_hemisphere = self.MakeLabel(_("Hemisphere for zone:"))

        # layout
        self.sizer.Add(item=self.radio1,
                       flag=wx.ALIGN_LEFT, pos=(1, 1), span=(1, 2))
        self.sizer.Add(item=self.radio2,
                       flag=wx.ALIGN_LEFT, pos=(2, 1), span=(1, 2))
        self.sizer.Add(item=self.title_utm,
                       flag=wx.ALIGN_LEFT | wx.ALL,
                       border=5, pos=(4, 1), span=(1, 2))
        self.sizer.Add(item=self.label_utm,
                       flag=wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL | wx.ALL,
                       border=5, pos=(5, 1))
        self.sizer.Add(item=self.text_utm,
                       flag=wx.ALIGN_LEFT | wx.ALL, border=5,
                       pos=(5, 2))
        self.sizer.Add(item=self.label_hemisphere,
                       flag=wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL | wx.ALL,
                       border=5, pos=(6, 1))
        self.sizer.Add(item=self.hemisphere,
                       flag=wx.ALIGN_LEFT|wx.ALL,
                       border=5, pos=(6, 2))

        self.title_utm.Hide()
        self.text_utm.Hide()
        self.label_utm.Hide()
        self.hemisphere.Hide()
        self.label_hemisphere.Hide()

        # bindings
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio1.GetId())
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio2.GetId())
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.OnPageChange)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED, self.OnEnterPage)

        # do page layout
        # self.DoLayout()

    def OnPageChange(self,event=None):
        if event.GetDirection() and self.parent.projpage.proj == 'utm' and self.utmzone == '':
            wx.MessageBox('You must set a zone for a UTM projection')
            event.Veto()
        self.title_utm.Hide()
        self.text_utm.Hide()
        self.label_utm.Hide()
        self.hemisphere.Hide()
        self.label_hemisphere.Hide()

    def OnEnterPage(self,event):
        if self.parent.projpage.proj == 'utm':
            self.title_utm.Show()
            self.text_utm.Show()
            self.label_utm.Show()
            self.hemisphere.Show()
            self.label_hemisphere.Show()
            
            self.Bind(wx.EVT_CHOICE, self.OnHemisphere, self.hemisphere)
            self.Bind(wx.EVT_TEXT, self.GetUTM, self.text_utm)

    def SetVal(self, event):
        global coordsys
        if event.GetId() == self.radio1.GetId():
            self.SetNext(self.parent.datumpage)
            self.parent.sumpage.SetPrev(self.parent.datumpage)
        elif event.GetId() == self.radio2.GetId():
            self.SetNext(self.parent.ellipsepage)
            self.parent.sumpage.SetPrev(self.parent.ellipsepage)

    def GetUTM(self, event):
        self.utmzone = event.GetString()

    def OnHemisphere(self, event):
        self.utmhemisphere = event.GetString()


class DatumPage(TitledPage):
    """
    Wizard page for selecting datum (with associated ellipsoid)
    and datum transformation parameters (select coordinate system option)
    """

    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, _("Specify geodetic datum"))

        self.parent = parent
        self.datum = ''
        self.datumdesc = ''
        self.ellipsoid = ''
        self.datumparams = ''
        self.transform = ''
        self.transregion = ''
        self.transparams = ''
        self.hastransform = False
        self.proj4params = ''

        # text input
        self.tdatum = self.MakeTextCtrl("", size=(200,-1))
        self.ttrans = self.MakeTextCtrl("", size=(200,-1))

        # search box
        self.searchb = wx.SearchCtrl(self, size=(200,-1),
                                     style=wx.TE_PROCESS_ENTER)

        # create list control for datum/elipsoid list
        data = []
        for key in self.parent.datums.keys():
            data.append([key, self.parent.datums[key][0], self.parent.datums[key][1]])
        self.datumlist = ItemList(self,
                                  data=data,
                                  columns=[_('Code'), _('Description'), _('Ellipsoid')])

        # create list control for datum transformation parameters list
        data = []
        for key in self.parent.transforms.keys():
            data.append([key, self.parent.transforms[key][0], self.parent.transforms[key][1]])
        self.transformlist = ItemList(self,
                                      data=data,
                                      columns=[_('Code'), _('Datum'), _('Description')])
        # layout
        self.sizer.Add(item=self.MakeLabel(_("Datum code:")),
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(1, 1))
        self.sizer.Add(item=self.tdatum,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(1, 2))

        self.sizer.Add(item=self.MakeLabel(_("Search in description:")),
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(2, 1))
        self.sizer.Add(item=self.searchb,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(2, 2))

        self.sizer.Add(item=self.datumlist,
                       flag=wx.EXPAND |
                       wx.ALIGN_LEFT |
                       wx.ALL, border=5, pos=(3, 1), span=(1, 4))

        self.sizer.Add(item=self.MakeLabel(_("Transformation parameters:")),
                       flag=wx.ALIGN_RIGHT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(5, 1))
        self.sizer.Add(item=self.ttrans,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(5, 2))

        self.sizer.Add(item=self.transformlist,
                       flag=wx.EXPAND |
                       wx.ALIGN_LEFT |
                       wx.ALL, border=5, pos=(6, 1), span=(1, 4))

        # events
        self.datumlist.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnDatumSelected)
        self.transformlist.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnTransformSelected)
        self.searchb.Bind(wx.EVT_TEXT_ENTER, self.OnDSearch)
        self.tdatum.Bind(wx.EVT_TEXT, self.OnDText)
        self.tdatum.Bind(wx.EVT_TEXT_ENTER, self.OnDText)
        self.ttrans.Bind(wx.EVT_TEXT, self.OnTText)
        self.ttrans.Bind(wx.EVT_TEXT_ENTER, self.OnTText)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.OnPageChanging)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED, self.OnEnterPage)

        # do page layout
        # self.DoLayout()

    def OnPageChanging(self, event):
        self.proj4params = ''
        if event.GetDirection() and self.datum not in self.parent.datums:
            event.Veto()
        if self.hastransform == True and self.transform == '':
            event.Veto()
        self.GetNext().SetPrev(self)
        self.parent.ellipsepage.ellipseparams = self.parent.ellipsoids[self.ellipsoid][1]
        self.GetNext().SetPrev(self)

    def OnEnterPage(self,event):
        if len(self.datum) == 0 or \
                (self.hastransform == True and self.transform == ''):
            # disable 'next' button by default
            wx.FindWindowById(wx.ID_FORWARD).Enable(False)
        else:
            wx.FindWindowById(wx.ID_FORWARD).Enable(True)

        event.Skip()

    def OnDText(self, event):
        self.datum = event.GetString()
        nextButton = wx.FindWindowById(wx.ID_FORWARD)
        if len(self.datum) == 0 and nextButton.IsEnabled():
            nextButton.Enable(False)
        elif self.datum in self.parent.datums:
            self.datumdesc = self.parent.datums[self.datum][0]
            self.ellipsoid = self.parent.datums[self.datum][1]
            self.datumparams = self.parent.datums[self.datum][2]
            if self.hastransform == False or \
                    (self.hastransform == True and self.transform == ''):
                if not nextButton.IsEnabled():
                    nextButton.Enable()

        event.Skip()

    def OnTText(self, event):
        if self.hastransform == False:
            event.Skip()
            return

        self.transform = event.GetString()
        nextButton = wx.FindWindowById(wx.ID_FORWARD)

        if len(self.transform) == 0 and nextButton.IsEnabled():
            nextButton.Enable(False)
        elif self.transform in self.parent.transforms:
            self.transdatum = self.parent.transforms[self.transform][0]
            self.transregion = self.parent.transforms[self.transform][1]
            self.transparams = self.parent.transforms[self.transform][2]
            if not nextButton.IsEnabled():
                nextButton.Enable()

    def OnDSearch(self, event):
        str =  self.searchb.GetValue()
        listItem  = self.datumlist.GetColumn(1)

        for i in range(self.datumlist.GetItemCount()):
            listItem = self.datumlist.GetItem(i,1)
            if listItem.GetText().find(str) > -1:
                datum = self.datumlist.GetItem(i, 0)
                self.tdatum.SetValue(datum.GetText())
                break

        self._onBrowseDatums(None,str)

    def OnTransformSelected(self,event):
        index = event.m_itemIndex
        item = event.GetItem()

        self.transform = item.GetText()
        self.transdatum = self.parent.transforms[self.transform][0]
        self.transregion = self.parent.transforms[self.transform][1]
        self.transparams = self.parent.transforms[self.transform][2]

        self.ttrans.SetValue(str(self.transform))

    def OnDatumSelected(self,event):
        index = event.m_itemIndex
        item = event.GetItem()

        self.datum = item.GetText()
        self.datumdesc = self.parent.datums[self.datum][0]
        self.ellipsoid = self.parent.datums[self.datum][1]
        self.datumparams = self.parent.datums[self.datum][2]

        self.tdatum.SetValue(self.datum)
        event.Skip()

class EllipsePage(TitledPage):
    """
    Wizard page for selecting ellipsoid (select coordinate system option)
    """

    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, _("Specify ellipsoid"))

        self.parent = parent
        
        self.ellipse = ''
        self.ellipsedesc = ''
        self.ellipseparams = ''
        self.proj4params = ''

        # text input
        self.tellipse = self.MakeTextCtrl("", size=(200,-1))

        # search box
        self.searchb = wx.SearchCtrl(self, size=(200,-1),
                                     style=wx.TE_PROCESS_ENTER)

        # create list control for ellipse list
        data = []
        # extract code, desc
        for key in self.parent.ellipsoids.keys():
            data.append([key, self.parent.ellipsoids[key][0]])

        self.ellipselist = ItemList(self, data=data,
                                    columns=[_('Code'), _('Description')])

        # layout
        self.sizer.Add(item=self.MakeLabel(_("Ellipsoid code:")),
                       flag=wx.ALIGN_RIGHT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(1, 1))
        self.sizer.Add(item=self.tellipse,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(1, 2))
        self.sizer.Add(item=self.MakeLabel(_("Search in description:")),
                       flag=wx.ALIGN_RIGHT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(2, 1))
        self.sizer.Add(item=self.searchb,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(2, 2))
                                    
        self.sizer.Add(item=self.ellipselist,
                       flag=wx.EXPAND |
                       wx.ALIGN_LEFT |
                       wx.ALL, border=5, pos=(3, 1), span=(1, 3))

        # events
        self.ellipselist.Bind(wx.EVT_LIST_ITEM_SELECTED,    self.OnItemSelected)
        self.tellipse.Bind(wx.EVT_TEXT, self.OnText)
        self.tellipse.Bind(wx.EVT_TEXT_ENTER, self.OnText)
        self.searchb.Bind(wx.EVT_TEXT_ENTER,    self.OnSearch)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED, self.OnEnterPage)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.OnPageChanging)

        # do page layout
        # self.DoLayout()

    def OnEnterPage(self,event):
        if len(self.ellipse) == 0:
            # disable 'next' button by default
            wx.FindWindowById(wx.ID_FORWARD).Enable(False)
        else:
            wx.FindWindowById(wx.ID_FORWARD).Enable(True)

        event.Skip()

    def OnPageChanging(self, event):
        if event.GetDirection() and self.ellipse not in self.parent.ellipsoids:
            event.Veto()

        self.proj4params = ''
        self.GetNext().SetPrev(self)
        self.parent.datumpage.datumparams = ''
        self.parent.datumpage.transparams = ''
        # self.GetNext().SetPrev(self) (???)

    def OnText(self, event):
        """Ellipspoid code changed"""
        self.ellipse = event.GetString()
        nextButton = wx.FindWindowById(wx.ID_FORWARD)
        if len(self.ellipse) == 0 and nextButton.IsEnabled():
            nextButton.Enable(False)
            self.ellipsedesc = ''
            self.ellipseparams = ''
            self.proj4params = ''
        elif self.ellipse in self.parent.ellipsoids:
            self.ellipsedesc = self.parent.ellipsoids[self.ellipse][0]
            self.ellipseparams = self.parent.ellipsoids[self.ellipse][1]
            if not nextButton.IsEnabled():
                nextButton.Enable()

    def OnSearch(self, event):
        """Search ellipsoid by desc"""
        str =  event.GetString()
        try:
            self.ellipse, self.ellipsedesc = \
                self.ellipselist.Search(index=1, str=event.GetString())
            self.ellipseparams = self.parent.ellipsoids[self.ellipse][1]
            self.proj4params = self.parent.ellipsoids[self.ellipse][2]
        except:
            self.ellipse = self.ellipsedesc = ''
            self.ellipseparams = self.proj4params = ''

        self.tellipse.SetValue(self.ellipse)

        event.Skip()

    def OnItemSelected(self,event):
        index = event.m_itemIndex
        item = event.GetItem()

        self.ellipse = item.GetText()
        self.ellipsedesc = self.parent.ellipsoids[self.ellipse][0]
        self.ellipseparams = self.parent.ellipsoids[self.ellipse][1]

        self.tellipse.SetValue(self.ellipse)

class GeoreferencedFilePage(TitledPage):
    """
    Wizard page for selecting georeferenced file to use
    for setting coordinate system parameters
    """

    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, _("Select georeferenced file"))

        self.georeffile = ''

        # create controls
        self.lfile= self.MakeLabel(_("Georeferenced file:"))
        self.tfile = self.MakeTextCtrl(size=(300,-1))
        self.bbrowse = self.MakeButton(_("Browse"))

        # do layout
        self.sizer.Add(item=self.lfile, flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTRE_VERTICAL |
                       wx.ALL, border=5, pos=(1, 1))
        self.sizer.Add(item=self.tfile, flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTRE_VERTICAL |
                       wx.ALL, border=5, pos=(1, 2))
        self.sizer.Add(item=self.bbrowse, flag=wx.ALIGN_LEFT |
                       wx.ALL, border=5, pos=(1, 3))

        self.bbrowse.Bind(wx.EVT_BUTTON, self.OnBrowse)
        self.tfile.Bind(wx.EVT_TEXT, self.OnText)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.OnPageChanging)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED, self.OnEnterPage)

        # do page layout
        # self.DoLayout()

    def OnEnterPage(self, event):
        if len(self.georeffile) == 0:
            # disable 'next' button by default
            wx.FindWindowById(wx.ID_FORWARD).Enable(False)
        else:
            wx.FindWindowById(wx.ID_FORWARD).Enable(True)

        event.Skip()

    def OnPageChanging(self, event):
        if event.GetDirection() and self.georeffile == '':
            event.Veto()
        self.GetNext().SetPrev(self)

        event.Skip()

    def OnText(self, event):
        self.georeffile = event.GetString()
        nextButton = wx.FindWindowById(wx.ID_FORWARD)
        if len(self.georeffile) == 0:
            if nextButton.IsEnabled():
                nextButton.Enable(False)
        else:
            if not nextButton.IsEnabled():
                nextButton.Enable(True)

        event.Skip()

    def OnBrowse(self, event):
        """Choose file"""
        dlg = wx.FileDialog(self,
                            _("Choose a georeferenced file"),
                            os.getcwd(), "", "*.*", wx.OPEN)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            self.tfile.SetValue(path)
        dlg.Destroy()

        event.Skip()

    def OnCreate(self, event):
        pass

class EPSGPage(TitledPage):
    """
    Wizard page for selecting EPSG code for
    setting coordinate system parameters
    """

    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, _("Choose EPSG Code"))
        self.parent = parent
        self.epsgCodeDict = {}
        self.epsgcode = ''
        self.epsgdesc = ''
        self.epsgparams = ''

        # labels
        self.lfile= self.MakeLabel(_("Path to the EPSG-codes file:"),
                                    style=wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.lcode= self.MakeLabel(_("EPSG code:"),
                                    style=wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)
        self.lsearch = self.MakeLabel(_("Search in code description:"),
                                       style=wx.ALIGN_LEFT | wx.ALIGN_CENTER_VERTICAL)

        # text input
        epsgdir = os.path.join(os.environ["GRASS_PROJSHARE"], 'epsg')
        self.tfile = self.MakeTextCtrl(text=epsgdir, size=(200,-1))
        self.tcode = self.MakeTextCtrl(size=(200,-1))

        # buttons
        self.bbrowse = self.MakeButton(_("Browse"))
        self.bbcodes = self.MakeButton(_("Browse EPSG Codes"))

        # search box
        self.searchb = wx.SearchCtrl(self, size=(200,-1),
                                     style=wx.TE_PROCESS_ENTER)

        self.epsglist = ItemList(self, data=None,
                                 columns=[_('Code'), _('Description'), _('Parameters')])

        # layout
        self.sizer.Add(item=self.lfile,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(1, 1))
        self.sizer.Add(item=self.tfile,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(1, 2))
        self.sizer.Add(item=self.bbrowse,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(1, 3))

        self.sizer.Add(item=self.lcode,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(2, 1))
        self.sizer.Add(item=self.tcode,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(2, 2))

        self.sizer.Add(item=self.lsearch,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(3, 1))
        self.sizer.Add(item=self.searchb,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(3, 2))
        self.sizer.Add(item=self.bbcodes,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(3, 3))

        self.sizer.Add(item=self.epsglist,
                       flag=wx.ALIGN_LEFT | wx.EXPAND, pos=(4, 1),
                       span=(2, 4))

        # events
        self.bbrowse.Bind(wx.EVT_BUTTON, self.OnBrowse)
        self.bbcodes.Bind(wx.EVT_BUTTON, self.OnBrowseCodes)
        self.tcode.Bind(wx.EVT_TEXT, self.OnText)
        self.tcode.Bind(wx.EVT_TEXT_ENTER, self.OnText)
        self.epsglist.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected)
        self.searchb.Bind(wx.EVT_TEXT_ENTER, self.OnSearch)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.OnPageChanging)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED, self.OnEnterPage)

        # do page layout
        # self.DoLayout()

    def OnEnterPage(self, event):
        if len(self.epsgcode) == 0:
            # disable 'next' button by default
            wx.FindWindowById(wx.ID_FORWARD).Enable(False)
        else:
            wx.FindWindowById(wx.ID_FORWARD).Enable(True)

        event.Skip()

    def OnPageChanging(self, event):
        if event.GetDirection() and not self.epsgcode:
            event.Veto()
        self.GetNext().SetPrev(self)

    def OnText(self, event):
        self.epsgcode = event.GetString()
        nextButton = wx.FindWindowById(wx.ID_FORWARD)

        if len(self.epsgcode) > 0 and self.epsgcode in self.epsgCodeDict.keys():
            self.epsgdesc = self.epsgCodeDict[self.epsgcode][0]
            self.epsgparams = self.epsgCodeDict[self.epsgcode][1]
            if not nextButton.IsEnabled():
                nextButton.Enable(True)
        else:
            if nextButton.IsEnabled():
                nextButton.Enable(False)
            self.epsgdesc = self.epsgparams = ''

    def OnSearch(self, event):
        str =  self.searchb.GetValue()
        try:
            self.epsgcode = self.epsglist.Search(index=1, str=event.GetString())[0]
        except:
            self.epsgcode = ''

        self.tcode.SetValue(self.epsgcode)

        event.Skip()
        
    def OnBrowse(self, event):
        """Define path for EPSG code file"""
        dlg = wx.FileDialog(self, _("Choose EPSG codes file:"),
                            "/", "", "*.*", wx.OPEN)
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            self.tfile.SetValue(path)
        dlg.Destroy()

        event.Skip()

    def OnItemSelected(self,event):
        index = event.m_itemIndex
        item = event.GetItem()

        self.epsgcode = item.GetText()
        self.epsgdesc = self.epsglist.GetItem(index, 1).GetText()
        self.tcode.SetValue(str(self.epsgcode))

    def OnBrowseCodes(self, event, search=None):
        """Browse EPSG codes"""
        try:
            data = []
            self.epsgCodeDict = {}
            f = open(self.tfile.GetValue(),"r")
            descr = None
            code = None
            params = None
            i = 0
            for line in f.readlines():
                line = line.strip()
                if line.find("#") == 0:
                    descr = line[1:].strip()
                elif line.find("<") == 0:
                    code = line.split(" ")[0]
                    for par in line.split(" ")[1:]:
                        params += par + " "
                    code = code[1:-1]
                if code == None:
                    code = 'no code'
                if descr == None:
                    descr = 'no description'
                if params == None:
                    params = 'no parameters'
                
                if i % 2 == 0:
                    data.append((code, descr, params))
                    self.epsgCodeDict[code] = (descr, params)
                    descr = None; code = None; params = ""
                i += 1
            f.close()

            self.epsglist.Populate(data)
        except StandardError, e:
            dlg = wx.MessageDialog(parent=self,
                                   message=_("Unable to read EPGS codes: %s " % e,
                                             _("Error"),  wx.OK | wx.ICON_ERROR))
            dlg.ShowModal()
            dlg.Destroy()

class CustomPage(TitledPage):
    """
    Wizard page for entering custom PROJ.4 string
    for setting coordinate system parameters
    """

    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard,
                            _("Choose method of specifying georeferencing parameters"))
        global coordsys
        self.customstring = ''
        self.parent = parent

        # widgets
        self.text_proj4string = self.MakeTextCtrl(size=(400, 200),
                                                  style=wx.TE_MULTILINE)
        self.label_proj4string = self.MakeLabel(_("Enter PROJ.4 parameters string:"))

        # layout
        self.sizer.Add(self.label_proj4string,
                       flag=wx.ALIGN_LEFT | wx.ALL,
                       border=5, pos=(1, 1))
        self.sizer.Add(self.text_proj4string,
                       flag=wx.ALIGN_LEFT | wx.ALL | wx.EXPAND, 
                       border=5, pos=(2, 1), span=(1, 4))

        self.text_proj4string.Bind(wx.EVT_TEXT, self.GetProjstring)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.OnPageChanging)
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED, self.OnEnterPage)

        # do page layout
        # self.DoLayout()

    def OnEnterPage(self, event):
        if len(self.customstring) == 0:
            # disable 'next' button by default
            wx.FindWindowById(wx.ID_FORWARD).Enable(False)
        else:
            wx.FindWindowById(wx.ID_FORWARD).Enable(True)

        event.Skip()

    def OnPageChanging(self, event):
        if event.GetDirection() and not self.customstring:
            event.Veto()
        self.GetNext().SetPrev(self)

    def GetProjstring(self, event):
        """Change proj string"""
        # TODO: check PROJ.4 syntax
        self.customstring = event.GetString()
        nextButton = wx.FindWindowById(wx.ID_FORWARD)
        if len(self.customstring) == 0:
            if nextButton.IsEnabled():
                nextButton.Enable(False)
        else:
            if not nextButton.IsEnabled():
                nextButton.Enable()

class SummaryPage(TitledPage):
    """
    Shows summary result of choosing coordinate system parameters
    prior to creating location
    """
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, _("Summary"))

        self.parent = parent

        # labels
        self.ldatabase  =    self.MakeLabel("")
        self.llocation  =    self.MakeLabel("")
        self.lprojection =    self.MakeLabel("")

        self.lprojection.Wrap(500)
                       
        self.sizer.Add(item=self.MakeLabel(_("GRASS Database:")),
                       flag=wx.ALIGN_LEFT | wx.ALL,
                       border=5, pos=(1, 0))
        self.sizer.Add(item=self.ldatabase, 
                       flag=wx.ALIGN_LEFT | wx.ALL,
                       border=5, pos=(1, 1))
        self.sizer.Add(item=self.MakeLabel(_("Location Name:")),
                       flag=wx.ALIGN_LEFT | wx.ALL,
                       border=5, pos=(2, 0))
        self.sizer.Add(item=self.llocation,
                       flag=wx.ALIGN_LEFT | wx.ALL,
                       border=5, pos=(2, 1))
        #         self.sizer.Add(item=wx.StaticLine(self, -1), 
        #                        flag=wx.ALIGN_RIGHT | wx.EXPAND | wx.ALL,
        #                        pos=(3, 0), span=(1, 2))
        #         self.sizer.Add(item=(10,10),
        #                        flag=wx.ALIGN_CENTER_HORIZONTAL | wx.ALL,
        #                        border=5, pos=(4, 0))
        self.sizer.Add(item=self.MakeLabel(_("Projection:")),
                       flag=wx.ALIGN_LEFT | wx.ALL,
                       border=5, pos=(3, 0))
        self.sizer.Add(item=self.lprojection,
                       flag=wx.ALIGN_LEFT | wx.ALL,
                       border=5, pos=(3, 1))
        self.sizer.Add(item=(10,20),
                       flag=wx.ALIGN_CENTER_HORIZONTAL | wx.ALL,
                       border=5, pos=(4, 0))
        self.sizer.Add(item=self.MakeLabel(_("You can set the default extents "
                                             "and resolution after creating new location")),
                       flag=wx.ALIGN_CENTRE | wx.ALL,
                       border=5, pos=(5, 0), span=(1, 3))
        self.sizer.Add(item=self.MakeLabel(_("or you can set them during a working session.")),
                       flag=wx.ALIGN_CENTRE | wx.ALL, border=5, pos=(6, 0),
                       span=(1, 3))

        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGED, self.OnPageChange)
        # self.Bind(wx.EVT_BUTTON, self.OnFinish, wx.ID_FINISH)

        # do page layout
        # self.DoLayout()

    def OnPageChange(self,event):
        """
        Insert values into text controls for summary of location creation options
        """

        database = self.parent.startpage.grassdatabase
        location = self.parent.startpage.location

        global coordsys
        if not coordsys:
            coordsys = 0

        projection = self.parent.projpage.proj
        projdesc = self.parent.projpage.projdesc
        utmzone = self.parent.projtypepage.utmzone
        utmhemisphere = self.parent.projtypepage.utmhemisphere
        ellipse = self.parent.ellipsepage.ellipse
        ellipsedesc = self.parent.ellipsepage.ellipsedesc
        datum = self.parent.datumpage.datum
        datumdesc = self.parent.datumpage.datumdesc
        ellipsoid = self.parent.datumpage.ellipsoid
        datumparams = self.parent.datumpage.datumparams
        transform = self.parent.datumpage.transform
        transregion = self.parent.datumpage.transregion
        transparams = self.parent.datumpage.transparams

        self.ldatabase.SetLabel(str(database))
        self.llocation.SetLabel(str(location))
        label = ''
        if coordsys == 'epsg':
            label = 'EPSG code %s (%s)' % (self.parent.epsgpage.epsgcode,self.parent.epsgpage.epsgdesc)
            self.lprojection.SetLabel(label)
        elif coordsys == 'file':
            label = 'matches file %s' % self.parent.filepage.georeffile
            self.lprojection.SetLabel(label)
        elif coordsys == 'proj':
            label = ('%s, %s%s' % (projdesc, datumdesc, ellipsedesc))
            self.lprojection.SetLabel(label)
        elif coordsys == 'xy':
            label = ('XY coordinate system (not projected).')
            self.lprojection.SetLabel(label)
        elif coordsys == 'custom':
            label = ('%s' % self.parent.custompage.customstring)
            self.lprojection.SetLabel(label)

    def OnFinish(self, event):
        dlg = wx.MessageDialog(parent=self.wizard,
                               message=_("Do you want to create new location '%s'?") % location,
                               caption=_("Create new location?"),
                               style=wx.YES_NO | wx.YES_DEFAULT | wx.ICON_QUESTION)

        if dlg.ShowModal() == wx.ID_NO:
            dlg.Destroy()
            event.Veto()
        else:
            dlg.Destroy()
            event.Skip()

class RegionDef(BaseClass, wx.Frame):
    """
    Page for setting default region extents and resolution
    """

    def __init__(self,parent, id=wx.ID_ANY,
                 title=_("Set default region values"), location=None):
        wx.Frame.__init__(self, parent, id, title, size=(650,300))

        self.parent = parent
        self.location = location

        # 2D
        self.north = 1.0
        self.south = 0.0
        self.east = 1.0
        self.west = 0.0
        self.nsres = 1.0
        self.ewres = 1.0

        # inputs
        self.ttop = self.MakeTextCtrl(str(self.north), size=(150, -1))
        self.tbottom = self.MakeTextCtrl(str(self.south), size=(150, -1))
        self.tleft = self.MakeTextCtrl(str(self.west), size=(150, -1))
        self.tright = self.MakeTextCtrl(str(self.east), size=(150, -1))
        self.tres = self.MakeTextCtrl(str(self.res), size=(150, -1))

        # labels
        # self.lmessage = self.MakeLabel(size=(300,50))

        # buttons
        self.bset = self.MakeButton(_("Set coordinates"), size=(150,-1))
        self.bcancel = self.MakeButton(_("Cancel"), size=(150,-1))

        # set current working environment to PERMANENT mapset
        # in selected location in order to set default region (WIND)
        envval = {}
        cmdlist = ['g.gisenv']
        p = gcmd.Command(cmdlist)
        if p.returncode == 0:
            output = p.ReadStdOutput()
            for line in output:
                line = line.strip()
                if '=' in line:
                    key, val = line.split('=')
                envval[key] = val
            self.currlocation = envval['LOCATION_NAME'].strip("';")
            self.currmapset = envval['MAPSET'].strip("';")
            if self.currlocation == self.location and self.currmapset == 'PERMANENT':
                pass
            else:
                cmdlist = ['g.mapset', 'location=%s' % self.location, 'mapset=PERMANENT']
                gcmd.Command(cmdlist)
        else:
            dlg = wx.MessageBox(parent=self,
                                message=_('A valid location must be selected'),
                                caption=_("Error"), style=wx.ID_OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return

        # get current region settings
        region = {}
        cmdlist = ['g.region', '-gp']
        p = gcmd.Command(cmdlist)
        if p.returncode == 0:
            output = p.ReadStdOutput()
            for line in output:
                line = line.strip()
                if '=' in line:
                    key, val = line.split('=')
                region[key] = float(val)
        else:
            dlg = wx.MessageBox(parent=self,
                                message=_('Invalid region'),
                                caption=_("Error"), style=wx.ID_OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return

        self.north = float(region['n'])
        self.south = float(region['s'])
        self.east = float(region['e'])
        self.west = float(region['w'])
        self.res = float(region['ewres'])

        # Insert current region settings into text controls
        self.ttop.SetValue(str(self.north))
        self.tbottom.SetValue(str(self.south))
        self.tleft.SetValue(str(self.west))
        self.tright.SetValue(str(self.east))
        self.tres.SetValue(str(self.res))

        # layout
        self.sizer = wx.GridBagSizer(vgap=0, hgap=0)

        self.sizer.Add(item=self.MakeLabel(_("Region extents and resolution:")),
                       flag=wx.ALIGN_RIGHT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=10, pos=(0, 0), span=(1, 2))

        self.sizer.Add(item=self.MakeLabel(_("North")),
                       flag=wx.ALIGN_CENTER_HORIZONTAL |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=0, pos=(1, 2))
        self.sizer.Add(item=self.ttop,
                       flag=wx.ALIGN_CENTER_HORIZONTAL |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(2, 2))

        self.sizer.Add(item=self.MakeLabel(_("West")),
                       flag=wx.ALIGN_RIGHT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=0, pos=(3, 0))
        self.sizer.Add(item=self.tleft,
                       flag=wx.ALIGN_RIGHT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5,  pos=(3, 1))

        self.sizer.Add(item=self.tright,
                       flag=wx.ALIGN_CENTER_HORIZONTAL |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5,  pos=(3, 3))
        self.sizer.Add(item=self.MakeLabel("East"),
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=0, pos=(3, 4))

        self.sizer.Add(item=self.tbottom,
                       flag=wx.ALIGN_CENTER_HORIZONTAL |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(4, 2))
        self.sizer.Add(item=self.MakeLabel("South"),
                       flag=wx.ALIGN_CENTER_HORIZONTAL |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=0, pos=(5, 2))

        self.sizer.Add(item=self.MakeLabel("Resolution"),
                       flag=wx.ALIGN_RIGHT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(6, 1))
        self.sizer.Add(item=self.tres,
                       flag=wx.ALIGN_CENTER_HORIZONTAL |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(6, 2))

        self.sizer.Add(item=wx.StaticLine(self, -1), 
                       flag=wx.EXPAND|wx.ALL, border=0, pos=(7, 0),
                       span=(1, 6))

        self.sizer.Add(item=self.bset,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(8, 3))

        self.sizer.Add(item=self.bcancel,
                       flag=wx.ALIGN_LEFT |
                       wx.ALIGN_CENTER_VERTICAL |
                       wx.ALL, border=5, pos=(8, 1))


        self.SetSizer(self.sizer)
        self.SetAutoLayout(True)
        self.Layout()

        self.Bind(wx.EVT_BUTTON, self.OnSetButton, self.bset)
        self.Bind(wx.EVT_BUTTON, self.OnCancel, self.bcancel)
        self.Bind(wx.EVT_TEXT, self.OnNorth, self.ttop)
        self.Bind(wx.EVT_TEXT, self.OnSouth, self.tbottom)
        self.Bind(wx.EVT_TEXT, self.OnEast, self.tright)
        self.Bind(wx.EVT_TEXT, self.OnWest, self.tleft)
        self.Bind(wx.EVT_TEXT, self.OnRes, self.tres)

    def OnNorth(self, event):
        self.north = event.GetString()

    def OnSouth(self, event):
        self.south = event.GetString()

    def OnEast(self, event):
        self.east = event.GetString()

    def OnWest(self, event):
        self.west = event.GetString()

    def OnRes(self, event):
        self.res = event.GetString()

    def OnSetButton(self,event=None):
        cmdlist = ['g.region', '-sgpa', 'n=%s' % self.north, 's=%s' % self.south, \
                   'e=%s' % self.east, 'w=%s' % self.west, 'res=%s' % self.res]
        p = gcmd.Command(cmdlist)
        if p.returncode == 0:
            output = p.ReadStdOutput()[0]
            wx.MessageBox('New default region:\n%s' % output)
        else:
            wx.MessageBox('Setting default region failed\n%s %s' % \
                          (p.ReadErrOutput()[0],p.ReadStdOutput()[0]))
        self.Destroy()

    def OnCancel(self, event):
        self.Destroy()

class LocationWizard(wx.Object):
    """
    Start wizard here and finish wizard here
    """
    def __init__(self, parent, grassdatabase):
        global coordsys
        self.parent = parent

        #
        # define wizard image
        #
        # file = "loc_wizard.png"
        file = "loc_wizard_qgis.png"
        imagePath = os.path.join(os.getenv("GISBASE"), "etc", "wx", "images",
                                 file)
        wizbmp = wx.Image(imagePath, wx.BITMAP_TYPE_PNG)
        # wizbmp.Rescale(250,600)
        wizbmp = wizbmp.ConvertToBitmap()

        #
        # get georeferencing information from tables in $GISBASE/etc
        #
        self.__readData()

        #
        # define wizard pages
        #
        self.wizard = wiz.Wizard(parent, id=wx.ID_ANY, title=_("Define new GRASS Location"),
                                 bitmap=wizbmp)
        self.startpage = DatabasePage(self.wizard, self, grassdatabase)
        self.csystemspage = CoordinateSystemPage(self.wizard, self)
        self.projpage = ProjectionsPage(self.wizard, self)
        self.datumpage = DatumPage(self.wizard, self)
        self.projtypepage = ProjTypePage(self.wizard,self)
        self.epsgpage = EPSGPage(self.wizard, self)
        self.filepage = GeoreferencedFilePage(self.wizard, self)
        self.ellipsepage = EllipsePage(self.wizard, self)
        self.custompage = CustomPage(self.wizard, self)
        self.sumpage = SummaryPage(self.wizard, self)

        #
        # set the initial order of the pages
        # (should follow the epsg line)
        #
        self.startpage.SetNext(self.csystemspage)

        self.csystemspage.SetPrev(self.startpage)
        self.csystemspage.SetNext(self.sumpage)

        self.projpage.SetPrev(self.csystemspage)
        self.projpage.SetNext(self.projtypepage)

        self.projtypepage.SetPrev(self.projpage)
        self.projtypepage.SetNext(self.datumpage)

        self.datumpage.SetPrev(self.projtypepage)
        self.datumpage.SetNext(self.sumpage)

        self.ellipsepage.SetPrev(self.projtypepage)
        self.ellipsepage.SetNext(self.sumpage)

        self.epsgpage.SetPrev(self.csystemspage)
        self.epsgpage.SetNext(self.sumpage)

        self.filepage.SetPrev(self.csystemspage)
        self.filepage.SetNext(self.sumpage)

        self.custompage.SetPrev(self.csystemspage)
        self.custompage.SetNext(self.sumpage)

        self.sumpage.SetPrev(self.csystemspage)

        #
        # do pages layout
        #
        self.startpage.DoLayout()
        self.csystemspage.DoLayout()
        self.projpage.DoLayout()
        self.datumpage.DoLayout()
        self.projtypepage.DoLayout()
        self.epsgpage.DoLayout()
        self.filepage.DoLayout()
        self.ellipsepage.DoLayout()
        self.custompage.DoLayout()
        self.sumpage.DoLayout()
        self.wizard.FitToPage(self.datumpage)

        # new location created?
        self.location = None 
        success = False

        #
        # run wizard...
        #
        if self.wizard.RunWizard(self.startpage):
            success = self.OnWizFinished()
            if success == True:
                self.wizard.Destroy()
                self.location = self.startpage.location
                dlg = wx.MessageDialog(parent=self.parent,
                                       message=_("Do you want to set the default "
                                                 "region extents and resolution now?"),
                                       caption=_("New location '%s' created") % self.location,
                                       style=wx.YES_NO | wx.YES_DEFAULT | wx.ICON_QUESTION)
                dlg.CenterOnScreen()
                if dlg.ShowModal() == wx.ID_YES:
                    dlg.Destroy()
                    defineRegion = RegionDef(None, location=self.location)
                    defineRegion.Show()
                else:
                    dlg.Destroy()

            elif success == False:
                wx.MessageBox(parent=self.wizard,
                              message=_("Unable to create new location."),
                              caption=_("Error"),
                              style=wx.OK | wx.ICON_ERROR)
            else: # None
                pass
        else:
            win = wx.MessageBox(parent=self.parent,
                          message=_("Location wizard canceled.%s"
                                    "New location not created.") % \
                              os.linesep,
                          caption=_("Location wizard"))

    def __readData(self):
        """Get georeferencing information from tables in $GISBASE/etc"""
        # read projection definitions
        f = open(os.path.join(os.getenv("GISBASE"), "etc", "projections"), "r")
        self.projections = {}
        for line in f.readlines():
            line = line.expandtabs(1)
            line = line.strip()
            if line == '' or line[0] == "#":
                continue
            proj, projdesc = line.split(":", 1)
            self.projections[proj.strip()] = projdesc.strip()
        f.close()

        # read datum definitions
        f = open(os.path.join(os.getenv("GISBASE"), "etc", "datum.table"), "r")
        self.datums = {}
        paramslist = []
        for line in f.readlines():
            line = line.expandtabs(1)
            line = line.strip()
            if line == '' or line[0] == "#":
                continue
            datum, info = line.split(" ", 1)
            info = info.strip()
            datumdesc, params = info.split(" ", 1)
            datumdesc = datumdesc.strip('"')
            paramlist = params.split()
            ellipsoid = paramlist.pop(0)
            self.datums[datum] = (datumdesc, ellipsoid, paramlist)
        f.close()

        # read datum transforms parameters
        f = open(os.path.join(os.getenv("GISBASE"), "etc", "datumtransform.table"), "r")
        self.transforms = {}
        j = 1
        for line in f.readlines():
            if j < 10:
                transcode = 'T0' + str(j)
            else:
               transcode = 'T' + str(j)
            line = line.expandtabs(1)
            line = line.strip()
            if line == '' or line[0] == "#":
                continue
            datum, rest = line.split(" ", 1)
            rest = rest.strip('" ')
            params, rest = rest.split('"', 1)
            params = params.strip()
            rest = rest.strip('" ')
            try:
                region, info = rest.split('"', 1)
                info = info.strip('" ')
                info = region + ': ' + info
            except:
                info = rest
            self.transforms[transcode] = (datum, info, params)
            j += 1
        f.close()

        # read ellipsiod definitions
        f = open(os.path.join(os.getenv("GISBASE"), "etc", "ellipse.table"), "r")
        self.ellipsoids = {}
        for line in f.readlines():
            line = line.expandtabs(1)
            line = line.strip()
            if line == '' or line[0] == "#":
                continue
            ellipse, rest = line.split(" ", 1)
            rest = rest.strip('" ')
            desc, params = rest.split('"', 1)
            desc = desc.strip('" ')
            paramslist = params.split()
            self.ellipsoids[ellipse] = (desc, paramslist)
        f.close()

    def OnWizFinished(self):
        database = self.startpage.grassdatabase
        location = self.startpage.location
        global coordsys
        success = False

        # location already exists?
        if os.path.isdir(os.path.join(database,location)):
            dlg = wx.MessageDialog(parent=self.wizard,
                                   message=_("Unable to create new location: %s already exists")
                                             % os.path.join(database, location),
                                   caption=_("ERROR"),
                                   style=wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return False

        if coordsys == "xy":
            success = self.XYCreate()
        elif coordsys == "latlong":
            rows = int(round((float(north) - float(south)) / float(resolution)))
            cols = int(round((float(east) - float(west)) / float(resolution)))
            cells = int(rows * cols)
            success = self.LatlongCreate()
        elif coordsys == "proj":
            success = self.Proj4Create()
        elif coordsys == 'custom':
            success = self.CustomCreate()
        elif coordsys == "epsg":
            success = self.EPSGCreate()
        elif coordsys == "file":
            success = self.FileCreate()

        return success

    def XYCreate(self):
        """Create an XY location"""
        database = self.startpage.grassdatabase
        location = self.startpage.location

        # create location directory and PERMANENT mapset
        try:
            os.mkdir(os.path.join(database, location))
            os.mkdir(os.path.join(database, location, 'PERMANENT'))
            # create DEFAULT_WIND and WIND files
            regioninfo =   ['proj:       0',
                            'zone:       0',
                            'north:      1',
                            'south:      0',
                            'east:       1',
                            'west:       0',
                            'cols:       1',
                            'rows:       1',
                            'e-w resol:  1',
                            'n-s resol:  1',
                            'top:        1',
                            'bottom:     0',
                            'cols3:      1',
                            'rows3:      1',
                            'depths:     1',
                            'e-w resol3: 1',
                            'n-s resol3: 1',
                            't-b resol:  1']
            
            defwind = open(os.path.join(database, location, 
                                        "PERMANENT", "DEFAULT_WIND"), 'w')
            for param in regioninfo:
                defwind.write(param + '%s' % os.linesep)
            defwind.close()

            shutil.copy(os.path.join(database, location, "PERMANENT", "DEFAULT_WIND"),
                        os.path.join(database, location, "PERMANENT", "WIND"))

            # create MYNAME file
            myname = open(os.path.join(database, location, "PERMANENT",
                                       "MYNAME"), 'w')
            myname.write('%s' % os.linesep)
            myname.close()

            return True

        except OSError, e:
            dlg = wx.MessageDialog(parent=self.wizard,
                                   message=_("Unable to create new location: %s") % e,
                                   caption=_("Error"),
                                   style=wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return False

    def Proj4Create(self):
        """
        Create a new location for selected projection
        """

        location = self.startpage.location
        proj = self.projpage.proj
        projdesc = self.projpage.projdesc

        utmzone = self.projtypepage.utmzone
        utmhemisphere = self.projtypepage.utmhemisphere

        datum = self.datumpage.datum
        if self.datumpage.datumdesc:
            datumdesc = self.datumpage.datumdesc+' - '+self.datumpage.ellipsoid
        else: datumdesc = ''
        datumparams = self.datumpage.datumparams
        transparams = self.datumpage.transparams

        ellipse = self.ellipsepage.ellipse
        ellipsedesc = self.ellipsepage.ellipsedesc
        ellipseparams = self.ellipsepage.ellipseparams

        # Creating PROJ.4 string
        if proj == 'll':
            proj = 'longlat'

        if proj == 'utm' and utmhemisphere == 'south':
            proj4string = '+proj=%s +zone=%s +south' % (proj, utmzone)
        elif proj == 'utm':
            proj4string = '+proj=%s +zone=%s' % (proj, utmzone)
        else:
            proj4string = '+proj=%s ' % (proj)

        proj4params = ''
        # set ellipsoid parameters
        for item in ellipseparams:
            if item[:4] == 'f=1/':
                item = '+rf='+item[4:]
            else:
                item = '+'+item
            proj4params = '%s %s' % (proj4params, item)
        # set datum and transform parameters if relevant
        if datumparams:
            for item in datumparams:
                proj4params = '%s +%s' % (proj4params,item)
            if transparams:
                proj4params = '%s +no_defs +%s' % (proj4params,transparams)
            else:
                proj4params = '%s +no_defs' % proj4params
        else:
            proj4params = '%s +no_defs' % proj4params

        proj4string = '%s %s' % (proj4string, proj4params)

        msgtext = "New location '%s' will be created georeferenced to" % location
        georeftext = '%s: %s%s' % (projdesc,datumdesc,ellipsedesc)
        p4text = '(PROJ.4 string: %s)' % proj4string

        dlg = wx.MessageDialog(self.wizard, msgtext+' '+georeftext+' '+p4text,
                            "Create new location?",
                            wx.YES_NO|wx.YES_DEFAULT|wx.ICON_QUESTION)
        if dlg.ShowModal() == wx.ID_NO:
            dlg.Destroy()
            return False
        else:
            dlg.Destroy()

        # Creating location from PROJ.4 string passed to g.proj
        try:
            cmdlist = ['g.proj', '-c', 'proj4=%s' % proj4string, 'location=%s' % location]
            p = gcmd.Command(cmdlist)
            if p.returncode == 0:
                return True
            else:
                return False

        except StandardError, e:
            dlg = wx.MessageDialog(self.wizard, "Could not create new location: %s " % str(e),
                                   "Could not create location",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            return False

    def CustomCreate(self):
        proj4string = self.custompage.customstring
        location = self.startpage.location

        dlg = wx.MessageDialog(self.wizard, "New location '%s' will be created using PROJ.4 string: %s"
                               % (location,proj4string),
                               "Create new location?",
                               wx.YES_NO|wx.YES_DEFAULT|wx.ICON_QUESTION)
        if dlg.ShowModal() == wx.ID_NO:
            dlg.Destroy()
            return False
        else:
            dlg.Destroy()

        try:
            cmdlist = ['g.proj','-c','proj4=%s' % proj4string,'location=%s' % location]
            p = gcmd.Command(cmdlist)
            if p.returncode == 0:
                return True
            else:
                return False

        except StandardError, e:
            dlg = wx.MessageDialog(self.wizard, "Could not create new location: %s " % str(e),
                                   "Could not create location",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            return False


    def EPSGCreate(self):
        """
        Create a new location from an EPSG code.
        """
        epsgcode = self.epsgpage.epsgcode
        epsgdesc = self.epsgpage.epsgdesc
        location = self.startpage.location
        cmdlist = []

        if not epsgcode:
            dlg = wx.MessageDialog(self.wizard, "Could not create new location: EPSG Code value missing",
                    "Could not create location",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            return False

        dlg = wx.MessageDialog(self.wizard, "New location '%s' will be created georeferenced to EPSG code %s: %s"
                               % (location, epsgcode, epsgdesc),
                               "Create new location from EPSG code?",
                               wx.YES_NO|wx.YES_DEFAULT|wx.ICON_QUESTION)
        if dlg.ShowModal() == wx.ID_NO:
            dlg.Destroy()
            return False
        else:
            dlg.Destroy()

        # creating location
        try:
            cmdlist = ['g.proj','epsg=%s' % epsgcode,'datumtrans=-1']
            p = gcmd.Command(cmdlist)
            dtoptions = p.ReadStdOutput()[0]
            if dtoptions != None:
                dtrans = ''
                # open a dialog to select datum transform number
                dlg = wx.TextEntryDialog(self.wizard, dtoptions,
                                         caption='Select the number of a datum transformation to use',
                                         defaultValue='1',
                                         style=wx.TE_WORDWRAP|wx.MINIMIZE_BOX|wx.MAXIMIZE_BOX|
                                         wx.RESIZE_BORDER|wx.VSCROLL|
                                         wx.OK|wx.CANCEL)

                if dlg.ShowModal() == wx.ID_CANCEL:
                    dlg.Destroy()
                    return False
                else:
                    dtrans = dlg.GetValue()
                    if dtrans != '':
                        dlg.Destroy()
                    else:
                        wx.MessageBox('You must select a datum transform')
                        return False

                cmdlist = ['g.proj','-c','epsg=%s' % epsgcode,'location=%s' % location,'datumtrans=%s' % dtrans]
            else:
                cmdlist = ['g.proj','-c','epsg=%s' % epsgcode,'location=%s' % location,'datumtrans=1']

            p = gcmd.Command(cmdlist)
            if p.returncode == 0:
                return True
            else:
                return False

        except StandardError, e:
            dlg = wx.MessageDialog(self.wizard, "Could not create new location: %s " % str(e),
                                   "Could not create location",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            return False

    def FileCreate(self):
        """
        Create a new location from a georeferenced file
        """
        georeffile = self.filepage.georeffile
        location = self.startpage.location

        cmdlist = []

        dlg = wx.MessageDialog(self.wizard, "New location '%s' will be created georeferenced to file '%s'"
                               % (location, georeffile), "Create new location from georeferenced file?",
                               wx.YES_NO|wx.YES_DEFAULT|wx.ICON_QUESTION)
        if dlg.ShowModal() == wx.ID_NO:
            dlg.Destroy()
            return False
        else:
            dlg.Destroy()

        if not os.path.isfile(georeffile):
            dlg = wx.MessageDialog(self.wizard, "Could not create new location: could not find file %s" % georeffile,
                                   "Could not create location",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            return False

        if not georeffile:
            dlg = wx.MessageDialog(self.wizard, "Could not create new location: georeferenced file not set",
                    "Could not create location",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            return False

        # creating location
        try:
            cmdlist = ['g.proj','-c','georef=%s' % georeffile,'location=%s' % location]
            p = gcmd.Command(cmdlist)
            if p.returncode == 0:
                return True
            else:
                return False

        except StandardError, e:
            dlg = wx.MessageDialog(self.wizard, "Could not create new location: %s " % str(e),
                                   "Could not create location",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            return False

if __name__ == "__main__":
    gWizard = GWizard(None,  "")
    GRASSStartUp = GWizard.StartUp(0)
    GRASSStartUp.MainLoop()

