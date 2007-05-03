"""
MODULE: defaultfont

CLASSES:
 * SetDefaultFont

PURPOSE: Sets default display font

AUTHORS: The GRASS Development Team

COPYRIGHT: (C) 2007 by the GRASS Development Team
       This program is free software under the GNU General Public
       License (>=v2). Read the file COPYING that comes with GRASS
       for details.
"""


import wx
import wx.lib.filebrowsebutton as filebrowse
import os,sys
try:
   from subprocess import *
except:
   from compat import subprocess
   from compat.subprocess import *

try:
   import subprocess
except:
   CompatPath = os.getenv("GISBASE") + "/etc/wx"
   sys.path.append(CompatPath)
   from compat import subprocess


class SetDefaultFont(wx.Dialog):
    """
    Opens a file selection dialog to select default font
    to use in all GRASS displays
    """

    def __init__(self, parent, ID, title, pos=wx.DefaultPosition, size=wx.DefaultSize,
            style=wx.DEFAULT_DIALOG_STYLE, fonttype = 'grassfont', encoding='ISO-8859-1'):
        wx.Dialog.__init__(self, parent, ID, title, pos, size, style)


        if "GRASS_FONT" in os.environ:
#            self.fontpath = os.path.dirname(os.environ["GRASS_FONT"])
            self.font = os.environ["GRASS_FONT"]
        else:
#            self.fontpath = None
            self.font = None

        self.fonttype = fonttype
        if self.fonttype == 'grassfont':
            rbsel = 0
        elif self.fonttype == 'truetype':
            rbsel = 1

        self.fontlist = self.GetFonts(fonttype)

        self.encoding = encoding

        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        fonttypes = ['GRASS stroke fonts', 'TrueType fonts']
        rb = wx.RadioBox(
                self, -1, "Select font type:", wx.DefaultPosition, wx.DefaultSize,
                fonttypes, 2, wx.RA_SPECIFY_COLS
                )
        box.Add(rb, 0, wx.EXPAND|wx.GROW|wx.ALIGN_RIGHT|wx.ALL, 5)
        self.Bind(wx.EVT_RADIOBOX, self.EvtRadioBox, rb)
        rb.SetToolTip(wx.ToolTip("Select type of font to use for GRASS text displays"))
        rb.SetSelection(rbsel)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_CENTER_HORIZONTAL|wx.RIGHT|wx.LEFT|wx.TOP, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Select Font:", (15, 50))
        box.Add(label, 0, wx.EXPAND|wx.GROW|wx.ALIGN_TOP|wx.RIGHT, 5)
        self.fontlb = wx.ListBox(self, wx.ID_ANY, pos=wx.DefaultPosition,
                                 size=(200,100), choices=self.fontlist,
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

    def GetFonts(self, fonttype):
        """
        parses fonts directory or fretypecap file to get a list of fonts for the listbox
        """
        grassfontpath = os.path.join(os.environ["GISBASE"], "fonts")
        freetypecap = os.path.join(os.environ["GISBASE"], "etc", "freetypecap")
        fontlist = []

        if fonttype == 'grassfont':
            # parse the fonts in the fonts folder
            fontfiles = os.listdir(grassfontpath)
            for file in fontfiles:
                fontlist.append(file.split('.')[0])
        elif fonttype == 'truetype':
            # parse the freetypecap file
            fontinfo = open(freetypecap).read().strip().split('\n')
            for item in fontinfo:
                if not item.startswith('#'):
                    fontlist.append(item.split(':')[0])

        return fontlist
