"""!
@package wxplot.dialogs

@brief Dialogs for different plotting routines

Classes:
 - dialogs::ProfileRasterDialog
 - dialogs::ScatterRasterDialog
 - dialogs::PlotStatsFrame
 - dialogs::HistRasterDialog
 - dialogs::TextDialog
 - dialogs::OptDialog

(C) 2011-2012 by the GRASS Development Team

This program is free software under the GNU General Public License
(>=v2). Read the file COPYING that comes with GRASS for details.

@author Michael Barton, Arizona State University
"""

import wx
import wx.lib.colourselect  as csel
import wx.lib.scrolledpanel as scrolled

from core             import globalvar
from core.settings    import UserSettings
from gui_core.gselect import Select

from grass.script import core  as grass

class ProfileRasterDialog(wx.Dialog):
    def __init__(self, parent, id = wx.ID_ANY, 
                 title = _("Select raster maps to profile"),
                 style = wx.DEFAULT_DIALOG_STYLE, **kwargs):
        """!Dialog to select raster maps to profile.
        """

        wx.Dialog.__init__(self, parent, id, title, style = style, **kwargs)


        self.parent = parent
        self.colorList = ["blue", "red", "green", "yellow", "magenta", "cyan", \
                    "aqua", "black", "grey", "orange", "brown", "purple", "violet", \
                    "indigo"]

        self.rasterList = self.parent.rasterList
        
        self._do_layout()
        
    def _do_layout(self):

        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.GridBagSizer (hgap = 3, vgap = 3)
        
        rastText = ''
        for r in self.rasterList:
            rastText += '%s,' % r
            
        rastText = rastText.rstrip(',')
        
        txt = _("Select raster map(s) to profile:")
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = txt)
        box.Add(item = label,
                flag = wx.ALIGN_CENTER_VERTICAL, pos = (0, 0))
        
        selection = Select(self, id = wx.ID_ANY,
                           size = globalvar.DIALOG_GSELECT_SIZE,
                           type = 'cell', multiple=True)
        selection.SetValue(rastText)
        selection.Bind(wx.EVT_TEXT, self.OnSelection)
        
        box.Add(item = selection, pos = (0, 1))
            
        sizer.Add(item = box, proportion = 0,
                  flag = wx.ALL, border = 10)

        line = wx.StaticLine(parent = self, id = wx.ID_ANY, size = (20, -1), style = wx.LI_HORIZONTAL)
        sizer.Add(item = line, proportion = 0,
                  flag = wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.LEFT|wx.RIGHT, border = 5)

        btnsizer = wx.StdDialogButtonSizer()

        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()

        sizer.Add(item = btnsizer, proportion = 0, flag = wx.ALIGN_RIGHT | wx.ALL, border = 5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def OnSelection(self, event):
        """!Choose maps to profile. Convert these into a list
        """
        self.rasterList = []
        self.rasterList = event.GetString().split(',')
        
class PlotStatsFrame(wx.Frame):
    def __init__(self, parent, id, message = '', title = '',
                 style = wx.DEFAULT_FRAME_STYLE, **kwargs):
        """!Dialog to display and save statistics for plots
        """
        wx.Frame.__init__(self, parent, id, style = style, **kwargs)
        self.SetLabel(_("Statistics"))
        
        sp = scrolled.ScrolledPanel(self, -1, size=(400, 400),
                                    style = wx.TAB_TRAVERSAL|wx.SUNKEN_BORDER, name="Statistics" )
                

        #
        # initialize variables
        #
        self.parent = parent
        self.message = message 
        self.title = title   
        self.CenterOnParent()  
        
        #
        # Display statistics
        #
        sizer = wx.BoxSizer(wx.VERTICAL)
        txtSizer = wx.BoxSizer(wx.VERTICAL)

        statstitle = wx.StaticText(parent = self, id = wx.ID_ANY, label = self.title)
        sizer.Add(item = statstitle, proportion = 0,
                  flag = wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, border = 3)
        line = wx.StaticLine(parent = self, id = wx.ID_ANY, size = (20, -1), style = wx.LI_HORIZONTAL)
        sizer.Add(item = line, proportion = 0,
                  flag = wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, border = 3)
        for stats in self.message:
            statstxt = wx.StaticText(parent = sp, id = wx.ID_ANY, label = stats)
            statstxt.SetBackgroundColour("WHITE")
            txtSizer.Add(item = statstxt, proportion = 1, 
                         flag = wx.EXPAND|wx.ALIGN_CENTER_VERTICAL|wx.LEFT|wx.RIGHT, border = 3)
            line = wx.StaticLine(parent = sp, id = wx.ID_ANY, size = (20, -1), style = wx.LI_HORIZONTAL) 
            txtSizer.Add(item = line, proportion = 0,
                         flag = wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, border = 3)      

        sp.SetSizer(txtSizer)
        sp.SetAutoLayout(1)
        sp.SetupScrolling() 

        sizer.Add(item = sp, proportion = 1, 
                  flag = wx.GROW | wx.LEFT | wx.RIGHT | wx.BOTTOM, border = 3)

        line = wx.StaticLine(parent = self, id = wx.ID_ANY, size = (20, -1), style = wx.LI_HORIZONTAL)
        sizer.Add(item = line, proportion = 0,
                  flag = wx.GROW |wx.ALIGN_CENTER_VERTICAL|wx.LEFT|wx.RIGHT, border = 3)

        #
        # buttons
        #
        btnSizer = wx.BoxSizer(wx.HORIZONTAL)

        btn_clipboard = wx.Button(self, id = wx.ID_COPY, label = _('C&opy'))
        btn_clipboard.SetToolTipString(_("Copy regression statistics the clipboard (Ctrl+C)"))
        btnSizer.Add(item = btn_clipboard, proportion = 0, flag = wx.ALIGN_LEFT | wx.ALL, border = 5)
        
        btnCancel = wx.Button(self, wx.ID_CLOSE)
        btnCancel.SetDefault()
        btnSizer.Add(item = btnCancel, proportion = 0, flag = wx.ALIGN_RIGHT | wx.ALL, border = 5)        

        sizer.Add(item = btnSizer, proportion = 0, flag = wx.ALIGN_RIGHT | wx.ALL, border = 5)

        # bindings
        btnCancel.Bind(wx.EVT_BUTTON, self.OnClose)
        btn_clipboard.Bind(wx.EVT_BUTTON, self.OnCopy)
        
        self.SetSizer(sizer)
        sizer.Fit(self)

    def OnCopy(self, event):
        """!Copy the regression stats to the clipboard
        """
        str = self.title + '\n'
        for item in self.message:
            str += item
            
        rdata = wx.TextDataObject()
        rdata.SetText(str)
        
        if wx.TheClipboard.Open():
            wx.TheClipboard.SetData(rdata)
            wx.TheClipboard.Close()
            wx.MessageBox(_("Regression statistics copied to clipboard"))
        
    def OnClose(self, event):
        """!Button 'Close' pressed
        """
        self.Close(True)

class TextDialog(wx.Dialog):
    def __init__(self, parent, id, title, plottype = '', 
                 style = wx.DEFAULT_DIALOG_STYLE, **kwargs):
        """!Dialog to set plot text options: font, title
        and font size, axis labels and font size
        """
        wx.Dialog.__init__(self, parent, id, title, style = style, **kwargs)
        #
        # initialize variables
        #
        # combo box entry lists
        self.ffamilydict = { 'default' : wx.FONTFAMILY_DEFAULT,
                             'decorative' : wx.FONTFAMILY_DECORATIVE,
                             'roman' : wx.FONTFAMILY_ROMAN,
                             'script' : wx.FONTFAMILY_SCRIPT,
                             'swiss' : wx.FONTFAMILY_SWISS,
                             'modern' : wx.FONTFAMILY_MODERN,
                             'teletype' : wx.FONTFAMILY_TELETYPE }

        self.fstyledict = { 'normal' : wx.FONTSTYLE_NORMAL,
                            'slant' : wx.FONTSTYLE_SLANT,
                            'italic' : wx.FONTSTYLE_ITALIC }

        self.fwtdict = { 'normal' : wx.FONTWEIGHT_NORMAL,
                         'light' : wx.FONTWEIGHT_LIGHT,
                         'bold' : wx.FONTWEIGHT_BOLD }

        self.parent = parent
        self.plottype = plottype

        self.ptitle = self.parent.ptitle
        self.xlabel = self.parent.xlabel
        self.ylabel = self.parent.ylabel

        self.properties = self.parent.properties # read-only
        
        # font size
        self.fontfamily = self.properties['font']['wxfont'].GetFamily()
        self.fontstyle = self.properties['font']['wxfont'].GetStyle()
        self.fontweight = self.properties['font']['wxfont'].GetWeight()

        self._do_layout()
                
    def _do_layout(self):
        """!Do layout"""
        # dialog layout
        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.StaticBox(parent = self, id = wx.ID_ANY,
                           label = " %s " % _("Text settings"))
        boxSizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        gridSizer = wx.GridBagSizer(vgap = 5, hgap = 5)

        #
        # profile title
        #
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Profile title:"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (0, 0))
        self.ptitleentry = wx.TextCtrl(parent = self, id = wx.ID_ANY, value = "", size = (250,-1))
        # self.ptitleentry.SetFont(self.font)
        self.ptitleentry.SetValue(self.ptitle)
        gridSizer.Add(item = self.ptitleentry, pos = (0, 1))

        #
        # title font
        #
        tlabel = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Title font size (pts):"))
        gridSizer.Add(item = tlabel, flag = wx.ALIGN_CENTER_VERTICAL, pos = (1, 0))
        self.ptitlesize = wx.SpinCtrl(parent = self, id = wx.ID_ANY, value = "", pos = (30, 50),
                                      size = (50,-1), style = wx.SP_ARROW_KEYS)
        self.ptitlesize.SetRange(5,100)
        self.ptitlesize.SetValue(int(self.properties['font']['prop']['titleSize']))
        gridSizer.Add(item = self.ptitlesize, pos = (1, 1))

        #
        # x-axis label
        #
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("X-axis label:"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (2, 0))
        self.xlabelentry = wx.TextCtrl(parent = self, id = wx.ID_ANY, value = "", size = (250,-1))
        # self.xlabelentry.SetFont(self.font)
        self.xlabelentry.SetValue(self.xlabel)
        gridSizer.Add(item = self.xlabelentry, pos = (2, 1))

        #
        # y-axis label
        #
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Y-axis label:"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (3, 0))
        self.ylabelentry = wx.TextCtrl(parent = self, id = wx.ID_ANY, value = "", size = (250,-1))
        # self.ylabelentry.SetFont(self.font)
        self.ylabelentry.SetValue(self.ylabel)
        gridSizer.Add(item = self.ylabelentry, pos = (3, 1))

        #
        # font size
        #
        llabel = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Label font size (pts):"))
        gridSizer.Add(item = llabel, flag = wx.ALIGN_CENTER_VERTICAL, pos = (4, 0))
        self.axislabelsize = wx.SpinCtrl(parent = self, id = wx.ID_ANY, value = "", pos = (30, 50),
                                         size = (50, -1), style = wx.SP_ARROW_KEYS)
        self.axislabelsize.SetRange(5, 100) 
        self.axislabelsize.SetValue(int(self.properties['font']['prop']['axisSize']))
        gridSizer.Add(item = self.axislabelsize, pos = (4,1))

        boxSizer.Add(item = gridSizer)
        sizer.Add(item = boxSizer, flag = wx.ALL | wx.EXPAND, border = 3)

        #
        # font settings
        #
        box = wx.StaticBox(parent = self, id = wx.ID_ANY,
                           label = " %s " % _("Font settings"))
        boxSizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        gridSizer = wx.GridBagSizer(vgap = 5, hgap = 5)
        gridSizer.AddGrowableCol(1)

        #
        # font family
        #
        label1 = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Font family:"))
        gridSizer.Add(item = label1, flag = wx.ALIGN_CENTER_VERTICAL, pos = (0, 0))
        self.ffamilycb = wx.ComboBox(parent = self, id = wx.ID_ANY, size = (250, -1),
                                choices = self.ffamilydict.keys(), style = wx.CB_DROPDOWN)
        self.ffamilycb.SetStringSelection('swiss')
        for item in self.ffamilydict.items():
            if self.fontfamily == item[1]:
                self.ffamilycb.SetStringSelection(item[0])
                break
        gridSizer.Add(item = self.ffamilycb, pos = (0, 1), flag = wx.ALIGN_RIGHT)

        #
        # font style
        #
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Style:"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (1, 0))
        self.fstylecb = wx.ComboBox(parent = self, id = wx.ID_ANY, size = (250, -1),
                                    choices = self.fstyledict.keys(), style = wx.CB_DROPDOWN)
        self.fstylecb.SetStringSelection('normal')
        for item in self.fstyledict.items():
            if self.fontstyle == item[1]:
                self.fstylecb.SetStringSelection(item[0])
                break
        gridSizer.Add(item = self.fstylecb, pos = (1, 1), flag = wx.ALIGN_RIGHT)

        #
        # font weight
        #
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Weight:"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (2, 0))
        self.fwtcb = wx.ComboBox(parent = self, size = (250, -1),
                                 choices = self.fwtdict.keys(), style = wx.CB_DROPDOWN)
        self.fwtcb.SetStringSelection('normal')
        for item in self.fwtdict.items():
            if self.fontweight == item[1]:
                self.fwtcb.SetStringSelection(item[0])
                break

        gridSizer.Add(item = self.fwtcb, pos = (2, 1), flag = wx.ALIGN_RIGHT)
                      
        boxSizer.Add(item = gridSizer, flag = wx.EXPAND)
        sizer.Add(item = boxSizer, flag = wx.LEFT | wx.RIGHT | wx.BOTTOM | wx.EXPAND, border = 3)

        line = wx.StaticLine(parent = self, id = wx.ID_ANY, size = (20, -1), style = wx.LI_HORIZONTAL)
        sizer.Add(item = line, proportion = 0,
                  flag = wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.LEFT|wx.RIGHT, border = 3)

        #
        # buttons
        #
        btnSave = wx.Button(self, wx.ID_SAVE)
        btnApply = wx.Button(self, wx.ID_APPLY)
        btnOk = wx.Button(self, wx.ID_OK)
        btnCancel = wx.Button(self, wx.ID_CANCEL)
        btnOk.SetDefault()

        # bindings
        btnApply.Bind(wx.EVT_BUTTON, self.OnApply)
        btnApply.SetToolTipString(_("Apply changes for the current session"))
        btnOk.Bind(wx.EVT_BUTTON, self.OnOk)
        btnOk.SetToolTipString(_("Apply changes for the current session and close dialog"))
        btnOk.SetDefault()
        btnSave.Bind(wx.EVT_BUTTON, self.OnSave)
        btnSave.SetToolTipString(_("Apply and save changes to user settings file (default for next sessions)"))
        btnCancel.Bind(wx.EVT_BUTTON, self.OnCancel)
        btnCancel.SetToolTipString(_("Close dialog and ignore changes"))

        # sizers
        btnStdSizer = wx.StdDialogButtonSizer()
        btnStdSizer.AddButton(btnOk)
        btnStdSizer.AddButton(btnApply)
        btnStdSizer.AddButton(btnCancel)
        btnStdSizer.Realize()
        
        btnSizer = wx.BoxSizer(wx.HORIZONTAL)
        btnSizer.Add(item = btnSave, proportion = 0, flag = wx.ALIGN_LEFT | wx.ALL, border = 5)
        btnSizer.Add(item = btnStdSizer, proportion = 0, flag = wx.ALIGN_RIGHT | wx.ALL, border = 5)
        sizer.Add(item = btnSizer, proportion = 0, flag = wx.ALIGN_RIGHT | wx.ALL, border = 5)

        #
        # bindings
        #
        self.ptitleentry.Bind(wx.EVT_TEXT, self.OnTitle)
        self.xlabelentry.Bind(wx.EVT_TEXT, self.OnXLabel)
        self.ylabelentry.Bind(wx.EVT_TEXT, self.OnYLabel)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def OnTitle(self, event):
        self.ptitle = event.GetString()

    def OnXLabel(self, event):
        self.xlabel = event.GetString()

    def OnYLabel(self, event):
        self.ylabel = event.GetString()

    def UpdateSettings(self):
        self.properties['font']['prop']['titleSize'] = self.ptitlesize.GetValue()
        self.properties['font']['prop']['axisSize'] = self.axislabelsize.GetValue()

        family = self.ffamilydict[self.ffamilycb.GetStringSelection()]
        self.properties['font']['wxfont'].SetFamily(family)
        style = self.fstyledict[self.fstylecb.GetStringSelection()]
        self.properties['font']['wxfont'].SetStyle(style)
        weight = self.fwtdict[self.fwtcb.GetStringSelection()]
        self.properties['font']['wxfont'].SetWeight(weight)

    def OnSave(self, event):
        """!Button 'Save' pressed"""
        self.OnApply(None)
        fileSettings = {}
        UserSettings.ReadSettingsFile(settings = fileSettings)
        fileSettings[self.plottype] = UserSettings.Get(group = self.plottype)
        UserSettings.SaveToFile(fileSettings)
        self.parent.parent.GetLayerManager().goutput.WriteLog(_('Plot text sizes saved to file \'%s\'.') % UserSettings.filePath)
        self.EndModal(wx.ID_OK)

    def OnApply(self, event):
        """!Button 'Apply' pressed"""
        self.UpdateSettings()
        self.parent.OnPlotText(self)
        
    def OnOk(self, event):
        """!Button 'OK' pressed"""
        self.OnApply(None)
        self.EndModal(wx.ID_OK)

    def OnCancel(self, event):
        """!Button 'Cancel' pressed"""
        self.EndModal(wx.ID_CANCEL)
        
class OptDialog(wx.Dialog):
    def __init__(self, parent, id, title, plottype = '', 
                 style = wx.DEFAULT_DIALOG_STYLE, **kwargs): 
        """!Dialog to set various options for data plotted, including: line
        width, color, style; marker size, color, fill, and style; grid
        and legend options.
        """
        wx.Dialog.__init__(self, parent, id, title, style = style, **kwargs)

        # init variables
        self.parent = parent
        self.linestyledict = parent.linestyledict
        self.ptfilldict = parent.ptfilldict
        self.parent = parent
        self.plottype = plottype
        
        self.pttypelist = ['circle',
                           'dot',
                           'square',
                           'triangle',
                           'triangle_down',
                           'cross',
                           'plus']
        
        self.axislist = ['min',
                         'auto',
                         'custom']

        # widgets ids
        self.wxId = {}
        
        self.parent = parent

        # read-only
        self.raster = self.parent.raster
        self.rasterList = self.parent.rasterList
        self.properties = self.parent.properties
        self.map = ''
        
        if len(self.rasterList) == 0:
            wx.MessageBox(parent = self,
                              message = _("No map or image group selected to plot."),
                              caption = _("Warning"), style = wx.OK | wx.ICON_ERROR)
            
        self._do_layout()
        
    def _do_layout(self):
        """!Options dialog layout
        """
        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.StaticBox(parent = self, id = wx.ID_ANY,
                           label = " %s " % _("Plot settings")) 
        boxMainSizer = wx.StaticBoxSizer(box, wx.HORIZONTAL)

        self.wxId['pcolor']  = 0
        self.wxId['pwidth']  = 0
        self.wxId['pstyle']  = 0
        self.wxId['psize'] = 0
        self.wxId['ptype'] = 0
        self.wxId['pfill'] = 0
        self.wxId['plegend'] = 0
        self.wxId['marker'] = {}
        self.wxId['x-axis'] = {}
        self.wxId['y-axis'] = {}
        
        #
        # plot line settings and point settings
        #
        if len(self.rasterList) == 0: return
        
        box = wx.StaticBox(parent = self, id = wx.ID_ANY,
                           label = _("Map/image plotted"))
        boxSizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        
        gridSizer = wx.GridBagSizer(vgap = 5, hgap = 5)
        
        row = 0
        choicelist = []
        for i in self.rasterList:
            choicelist.append(str(i))

        self.mapchoice = wx.Choice(parent = self, id = wx.ID_ANY, size = (300, -1),
                                   choices = choicelist)
        self.mapchoice.SetToolTipString(_("Settings for selected map"))

        if not self.map:
            self.map = self.rasterList[self.mapchoice.GetCurrentSelection()]
        else:
            self.mapchoice.SetStringSelection(str(self.map))
            
                
        gridSizer.Add(item = self.mapchoice, flag = wx.ALIGN_CENTER_VERTICAL, 
                      pos = (row, 0), span = (1, 2))
        
        #
        # options for profile
        #
        row +=1            
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Line color"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (row, 0))
        color = csel.ColourSelect(parent = self, id = wx.ID_ANY, colour = self.raster[self.map]['pcolor'])
        self.wxId['pcolor'] = color.GetId()
        gridSizer.Add(item = color, pos = (row, 1))

        row += 1
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Line width"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (row, 0))
        width = wx.SpinCtrl(parent = self, id = wx.ID_ANY, value = "",
                             size = (50,-1), style = wx.SP_ARROW_KEYS)
        width.SetRange(1, 10)
        width.SetValue(self.raster[self.map]['pwidth'])
        self.wxId['pwidth'] = width.GetId()
        gridSizer.Add(item = width, pos = (row, 1))

        row +=1
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Line style"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (row, 0))
        style = wx.Choice(parent = self, id = wx.ID_ANY, 
                             size = (120, -1), choices = self.linestyledict.keys())
        style.SetStringSelection(self.raster[self.map]['pstyle'])
        self.wxId['pstyle'] = style.GetId()
        gridSizer.Add(item = style, pos = (row, 1))

        row += 1
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Legend"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (row, 0))
        legend = wx.TextCtrl(parent = self, id = wx.ID_ANY, value = "", size = (200,-1))
        legend.SetValue(self.raster[self.map]['plegend'])
        gridSizer.Add(item = legend, pos = (row, 1))
        self.wxId['plegend'] = legend.GetId()
        
        boxSizer.Add(item = gridSizer)
        boxMainSizer.Add(item = boxSizer, flag = wx.ALL, border = 3)

        #
        # segment marker settings for profiles
        #       
        box = wx.StaticBox(parent = self, id = wx.ID_ANY,
                           label = " %s " % _("Transect segment marker settings"))
        
        boxSizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        
        gridSizer = wx.GridBagSizer(vgap = 5, hgap = 5)
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Color"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (0, 0))
        ptcolor = csel.ColourSelect(parent = self, id = wx.ID_ANY, colour = self.properties['marker']['color'])
        self.wxId['marker']['color'] = ptcolor.GetId()
        gridSizer.Add(item = ptcolor, pos = (0, 1))

        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Size"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (1, 0))
        ptsize = wx.SpinCtrl(parent = self, id = wx.ID_ANY, value = "",
                             size = (50, -1), style = wx.SP_ARROW_KEYS)
        ptsize.SetRange(1, 10)
        ptsize.SetValue(self.properties['marker']['size'])
        self.wxId['marker']['size'] = ptsize.GetId()
        gridSizer.Add(item = ptsize, pos = (1, 1))
        
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Fill"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (2, 0))
        ptfill = wx.Choice(parent = self, id = wx.ID_ANY, 
                           size = (120, -1), choices = self.ptfilldict.keys())
        ptfill.SetStringSelection(self.properties['marker']['fill'])
        self.wxId['marker']['fill'] = ptfill.GetId()
        gridSizer.Add(item = ptfill, pos = (2, 1))
        
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Legend"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (3, 0))
        ptlegend = wx.TextCtrl(parent = self, id = wx.ID_ANY, value = "", size = (200,-1))
        ptlegend.SetValue(self.properties['marker']['legend'])
        self.wxId['marker']['legend'] = ptlegend.GetId()
        gridSizer.Add(item = ptlegend, pos = (3, 1))
                
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Style"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (4, 0))
        pttype = wx.Choice(parent = self, size = (200, -1), choices = self.pttypelist)
        pttype.SetStringSelection(self.properties['marker']['type'])
        self.wxId['marker']['type'] = pttype.GetId()
        gridSizer.Add(item = pttype, pos = (4, 1))

        boxSizer.Add(item = gridSizer)
        boxMainSizer.Add(item = boxSizer, flag = wx.ALL, border = 3)
                        
        sizer.Add(item = boxMainSizer, flag = wx.ALL | wx.EXPAND, border = 3)

        #
        # axis options for all plots
        #
        box = wx.StaticBox(parent = self, id = wx.ID_ANY,
                           label = " %s " % _("Axis settings"))
        boxMainSizer = wx.StaticBoxSizer(box, wx.HORIZONTAL)

        middleSizer = wx.BoxSizer(wx.HORIZONTAL)

        idx = 0
        for axis, atype in [(_("X-Axis"), 'x-axis'),
                     (_("Y-Axis"), 'y-axis')]:
            box = wx.StaticBox(parent = self, id = wx.ID_ANY,
                               label = " %s " % axis)
            boxSizer = wx.StaticBoxSizer(box, wx.HORIZONTAL)
            gridSizer = wx.GridBagSizer(vgap = 5, hgap = 5)

            prop = self.properties[atype]['prop']
            
            row = 0
            label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Scale"))
            gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (row, 0))
            type = wx.Choice(parent = self, id = wx.ID_ANY, 
                             size = (100, -1), choices = self.axislist)
            type.SetStringSelection(prop['type']) 
            type.SetToolTipString(_("Automatic axis scaling, custom max and min, or scale matches data range (min)" ))
            self.wxId[atype]['type'] = type.GetId()
            gridSizer.Add(item = type, pos = (row, 1))
                        
            row += 1
            label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Custom min"))
            gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (row, 0))
            min = wx.TextCtrl(parent = self, id = wx.ID_ANY, value = "", size = (70, -1))
            min.SetValue(str(prop['min']))
            self.wxId[atype]['min'] = min.GetId()
            gridSizer.Add(item = min, pos = (row, 1))

            row += 1
            label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Custom max"))
            gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (row, 0))
            max = wx.TextCtrl(parent = self, id = wx.ID_ANY, value = "", size = (70, -1))
            max.SetValue(str(prop['max']))
            self.wxId[atype]['max'] = max.GetId()
            gridSizer.Add(item = max, pos = (row, 1))
            
            row += 1
            log = wx.CheckBox(parent = self, id = wx.ID_ANY, label = _("Log scale"))
            log.SetValue(prop['log'])
            self.wxId[atype]['log'] = log.GetId()
            gridSizer.Add(item = log, pos = (row, 0), span = (1, 2))

            if idx == 0:
                flag = wx.ALL | wx.EXPAND
            else:
                flag = wx.TOP | wx.BOTTOM | wx.RIGHT | wx.EXPAND

            boxSizer.Add(item = gridSizer, flag = wx.ALL, border = 3)
            boxMainSizer.Add(item = boxSizer, flag = flag, border = 3)

            idx += 1
            
        middleSizer.Add(item = boxMainSizer, flag = wx.ALL | wx.EXPAND, border = 3)

        #
        # grid & legend options for all plots
        #
        self.wxId['grid'] = {}
        self.wxId['legend'] = {}
        self.wxId['font'] = {}
        box = wx.StaticBox(parent = self, id = wx.ID_ANY,
                           label = " %s " % _("Grid and Legend settings"))
        boxMainSizer = wx.StaticBoxSizer(box, wx.HORIZONTAL)
        gridSizer = wx.GridBagSizer(vgap = 5, hgap = 5)

        row = 0
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Grid color"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (row, 0))
        gridcolor = csel.ColourSelect(parent = self, id = wx.ID_ANY, colour = self.properties['grid']['color'])
        self.wxId['grid']['color'] = gridcolor.GetId()
        gridSizer.Add(item = gridcolor, pos = (row, 1))

        row +=1
        gridshow = wx.CheckBox(parent = self, id = wx.ID_ANY, label = _("Show grid"))
        gridshow.SetValue(self.properties['grid']['enabled'])
        self.wxId['grid']['enabled'] = gridshow.GetId()
        gridSizer.Add(item = gridshow, pos = (row, 0), span = (1, 2))

        row +=1
        label = wx.StaticText(parent = self, id = wx.ID_ANY, label = _("Legend font size"))
        gridSizer.Add(item = label, flag = wx.ALIGN_CENTER_VERTICAL, pos = (row, 0))
        legendfontsize = wx.SpinCtrl(parent = self, id = wx.ID_ANY, value = "", 
                                     size = (50, -1), style = wx.SP_ARROW_KEYS)
        legendfontsize.SetRange(5,100)
        legendfontsize.SetValue(int(self.properties['font']['prop']['legendSize']))
        self.wxId['font']['legendSize'] = legendfontsize.GetId()
        gridSizer.Add(item = legendfontsize, pos = (row, 1))

        row += 1
        legendshow = wx.CheckBox(parent = self, id = wx.ID_ANY, label = _("Show legend"))
        legendshow.SetValue(self.properties['legend']['enabled'])
        self.wxId['legend']['enabled'] = legendshow.GetId()
        gridSizer.Add(item = legendshow, pos = (row, 0), span = (1, 2))

        boxMainSizer.Add(item = gridSizer, flag = flag, border = 3)

        middleSizer.Add(item = boxMainSizer, flag = wx.LEFT | wx.RIGHT | wx.BOTTOM | wx.EXPAND, border = 3)

        sizer.Add(item = middleSizer, flag = wx.ALL, border = 0)
        
        #
        # line & buttons
        #
        line = wx.StaticLine(parent = self, id = wx.ID_ANY, size = (20, -1), style = wx.LI_HORIZONTAL)
        sizer.Add(item = line, proportion = 0,
                  flag = wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.LEFT|wx.RIGHT, border = 3)

        #
        # buttons
        #
        btnSave = wx.Button(self, wx.ID_SAVE)
        btnApply = wx.Button(self, wx.ID_APPLY)
        btnOk = wx.Button(self, wx.ID_OK)
        btnCancel = wx.Button(self, wx.ID_CANCEL)
        btnOk.SetDefault()

        # tooltips for buttons
        btnApply.SetToolTipString(_("Apply changes for the current session"))
        btnOk.SetToolTipString(_("Apply changes for the current session and close dialog"))
        btnSave.SetToolTipString(_("Apply and save changes to user settings file (default for next sessions)"))
        btnCancel.SetToolTipString(_("Close dialog and ignore changes"))

        # sizers
        btnStdSizer = wx.StdDialogButtonSizer()
        btnStdSizer.AddButton(btnOk)
        btnStdSizer.AddButton(btnApply)
        btnStdSizer.AddButton(btnCancel)
        btnStdSizer.Realize()
        
        btnSizer = wx.BoxSizer(wx.HORIZONTAL)
        btnSizer.Add(item = btnSave, proportion = 0, flag = wx.ALIGN_LEFT | wx.ALL, border = 5)
        btnSizer.Add(item = btnStdSizer, proportion = 0, flag = wx.ALIGN_RIGHT | wx.ALL, border = 5)
        sizer.Add(item = btnSizer, proportion = 0, flag = wx.ALIGN_RIGHT | wx.ALL, border = 5)

        #
        # bindings for buttons and map plot settings controls
        #
        self.mapchoice.Bind(wx.EVT_CHOICE, self.OnSetMap)

        # bindings
        btnApply.Bind(wx.EVT_BUTTON, self.OnApply)
        btnOk.Bind(wx.EVT_BUTTON, self.OnOk)
        btnOk.SetDefault()
        btnSave.Bind(wx.EVT_BUTTON, self.OnSave)
        btnCancel.Bind(wx.EVT_BUTTON, self.OnCancel)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def OnSetMap(self, event):
        """!Handler for changing map selection"""
        idx = event.GetSelection()
        self.map = self.rasterList[idx]
        
        # update settings controls for all plots
        self.FindWindowById(self.wxId['pcolor']).SetColour(self.raster[self.map]['pcolor'])
        self.FindWindowById(self.wxId['plegend']).SetValue(self.raster[self.map]['plegend'])
        self.FindWindowById(self.wxId['pwidth']).SetValue(self.raster[self.map]['pwidth'])
        self.FindWindowById(self.wxId['pstyle']).SetStringSelection(self.raster[self.map]['pstyle'])
            
        self.Refresh()
        
    def OnSetOpt(self, event):
        """!Handler for changing any other option"""
        self.map = self.rasterList[self.mapchoice.GetCurrentSelection()]
        self.UpdateSettings()
        self.parent.SetGraphStyle()
        p = self.parent.CreatePlotList()
        self.parent.DrawPlot(p)

    def UpdateSettings(self):
        """!Apply settings to each map and to entire plot"""
        self.raster[self.map]['pcolor'] = self.FindWindowById(self.wxId['pcolor']).GetColour()
        self.properties['raster']['pcolor'] = self.raster[self.map]['pcolor']
        
        self.raster[self.map]['plegend'] = self.FindWindowById(self.wxId['plegend']).GetValue()
        
        self.raster[self.map]['pwidth'] = int(self.FindWindowById(self.wxId['pwidth']).GetValue())
        self.properties['raster']['pwidth'] = self.raster[self.map]['pwidth']
        self.raster[self.map]['pstyle'] = self.FindWindowById(self.wxId['pstyle']).GetStringSelection()
        self.properties['raster']['pstyle'] = self.raster[self.map]['pstyle']
            
        # update settings for entire plot
        for axis in ('x-axis', 'y-axis'):
            self.properties[axis]['prop']['type'] = self.FindWindowById(self.wxId[axis]['type']).GetStringSelection()
            self.properties[axis]['prop']['min'] = float(self.FindWindowById(self.wxId[axis]['min']).GetValue())
            self.properties[axis]['prop']['max'] = float(self.FindWindowById(self.wxId[axis]['max']).GetValue())
            self.properties[axis]['prop']['log'] = self.FindWindowById(self.wxId[axis]['log']).IsChecked()

        if self.plottype == 'profile':
            self.properties['marker']['color'] = self.FindWindowById(self.wxId['marker']['color']).GetColour()
            self.properties['marker']['fill'] = self.FindWindowById(self.wxId['marker']['fill']).GetStringSelection()
            self.properties['marker']['size'] = self.FindWindowById(self.wxId['marker']['size']).GetValue()
            self.properties['marker']['type'] = self.FindWindowById(self.wxId['marker']['type']).GetStringSelection()
            self.properties['marker']['legend'] = self.FindWindowById(self.wxId['marker']['legend']).GetValue()

        self.properties['grid']['color'] = self.FindWindowById(self.wxId['grid']['color']).GetColour()
        self.properties['grid']['enabled'] = self.FindWindowById(self.wxId['grid']['enabled']).IsChecked()

        # this makes more sense in the text properties, including for settings update. But will need to change
        # layout for controls to text dialog too.
        self.properties['font']['prop']['legendSize'] = self.FindWindowById(self.wxId['font']['legendSize']).GetValue()
        self.properties['legend']['enabled'] = self.FindWindowById(self.wxId['legend']['enabled']).IsChecked()

    def OnSave(self, event):
        """!Button 'Save' pressed"""
        self.OnApply(None)
        fileSettings = {}
        UserSettings.ReadSettingsFile(settings = fileSettings)
        fileSettings[self.plottype] = UserSettings.Get(group = self.plottype)
        UserSettings.SaveToFile(fileSettings)
        self.parent.parent.GetLayerManager().goutput.WriteLog(_('Plot settings saved to file \'%s\'.') % UserSettings.filePath)
        self.Close()

    def OnApply(self, event):
        """!Button 'Apply' pressed. Does not close dialog"""
        self.UpdateSettings()
        self.parent.SetGraphStyle()
        p = self.parent.CreatePlotList()
        self.parent.DrawPlot(p)

    def OnOk(self, event):
        """!Button 'OK' pressed"""
        self.OnApply(None)
        self.EndModal(wx.ID_OK)
        
    def OnCancel(self, event):
        """!Button 'Cancel' pressed"""
        self.Close()
     
