"""!
@package wxplot.dialogs

@brief Dialogs for different plotting routines

Classes:
 - dialogs::ProfileRasterDialog
 - dialogs::PlotStatsFrame

(C) 2011 by the GRASS Development Team

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
