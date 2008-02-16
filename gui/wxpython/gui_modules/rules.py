"""
MODULE:     rules.py

CLASSES:
    * RulesText

PURPOSE:    Dialog for interactive entry of rules for r.colors,
            r.reclass, r.recode, and v.reclass

AUTHORS:    The GRASS Development Team
            Michael Barton (Arizona State University)

COPYRIGHT:  (C) 2007 by the GRASS Development Team
            This program is free software under the GNU General Public
            License (>=v2). Read the file COPYING that comes with GRASS
            for details.

"""

import wx
import os
import sys

import gselect

class RulesText(wx.Dialog):
    def __init__(self, parent, id=wx.ID_ANY, title="Enter rules",
                 pos=wx.DefaultPosition, size=wx.DefaultSize,
                 style=wx.DEFAULT_DIALOG_STYLE,
                 cmd=None):
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)

        """
        Dialog for interactively entering rules
        for map management commands
        """
        self.parent = parent
        self.cmd = cmd # map management command
        self.inmap = '' # input map to change
        self.outmap = '' # output map for reclass/recode
        self.rules = '' # rules for changing
        self.overwrite = False

        if self.cmd == 'r.colors':
            label1 = 'Create new color table using color rules'
            label2 = 'Raster map:'
            label3 = None
            label4 = 'Enter color rules'
            seltype = 'cell'
        elif self.cmd == 'r.reclass':
            label1 = 'Reclassify raster map using rules'
            label2 = 'Map to reclassify:'
            label3 = 'Reclassified map:'
            label4 = 'Enter reclassification rules'
            seltype = 'cell'
        elif self.cmd == 'r.recode':
            label1 = 'Recode raster map using rules'
            label2 = 'Map to recode:'
            label3 = 'Recoded map:'
            label4 = 'Enter recoding rules'
            seltype = 'cell'
        elif self.cmd == 'v.reclass':
            label1 = 'Reclassify vector map using SQL rules'
            label2 = 'Map to reclassify:'
            label3 = 'Reclassified map:'
            label4 = 'Enter reclassification rules'
            seltype = 'vector'

        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, wx.ID_ANY, label1)
        box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(item=box, proportion=0,
                  flag=wx.ALIGN_CENTER|
                  wx.ALL,border=5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, wx.ID_ANY, label2)
        box.Add(label, 0, wx.ALIGN_RIGHT|wx.ALL, 5)
        self.selection = gselect.Select(self, id=wx.ID_ANY, size=(300,-1),
                                        type=seltype)
        box.Add(self.selection, 0, wx.ALIGN_RIGHT|wx.ALL, 5)
        sizer.Add(item=box, proportion=0,
                  flag=wx.ALIGN_CENTER_VERTICAL|
                  wx.ALIGN_RIGHT|
                  wx.ALL,border=5)

        if cmd != 'r.colors':
            box = wx.BoxSizer(wx.HORIZONTAL)
            label = wx.StaticText(self, wx.ID_ANY, label3)
            box.Add(label, 0, wx.ALIGN_RIGHT|wx.ALL, 5)
            self.textentry = wx.TextCtrl(self, wx.ID_ANY, "", size=(300,-1))
            box.Add(self.textentry, 0, wx.ALIGN_RIGHT|wx.ALL, 5)
            self.textentry.Bind(wx.EVT_TEXT, self.OnText)
            sizer.Add(item=box, proportion=0,
                      flag=wx.ALIGN_CENTER_VERTICAL|
                      wx.ALIGN_RIGHT|
                      wx.ALL,border=5)

            box = wx.BoxSizer(wx.HORIZONTAL)
            self.ovrwrtcheck = wx.CheckBox(self, wx.ID_ANY, 'overwrite existing file')
            self.ovrwrtcheck.SetValue(self.overwrite)
            box.Add(self.ovrwrtcheck, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
            sizer.Add(item=box, proportion=0,
                      flag=wx.ALIGN_CENTER_VERTICAL|
                      wx.ALIGN_RIGHT|
                      wx.ALL,border=5)
            self.Bind(wx.EVT_CHECKBOX, self.OnOverwrite,   self.ovrwrtcheck)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, wx.ID_ANY, label4)
        box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        helpbtn = wx.Button(self, wx.ID_ANY, "Help")
        box.Add(helpbtn, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(item=box, proportion=0,
                  flag=wx.ALIGN_CENTER|
                  wx.ALL,border=5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        self.rulestxt = wx.TextCtrl(self, id=wx.ID_ANY, value='',
                                    pos=wx.DefaultPosition, size=(400,150),
                                    style=wx.TE_MULTILINE|
                                    wx.HSCROLL|
                                    wx.TE_NOHIDESEL)
        self.rulestxt.SetFont(wx.Font(10, wx.FONTFAMILY_MODERN, wx.NORMAL, wx.NORMAL, 0, ''))
        box.Add(self.rulestxt, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(item=box, proportion=0,
                  flag=wx.ALIGN_CENTER|
                  wx.ALL,border=5)

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

        self.Bind(wx.EVT_BUTTON, self.OnHelp, helpbtn)
        self.selection.Bind(wx.EVT_TEXT, self.OnSelection)
        self.Bind(wx.EVT_TEXT, self.OnRules,   self.rulestxt)

    def OnSelection(self, event):
        self.inmap = event.GetString()

    def OnText(self, event):
        self.outmap = event.GetString()

    def OnRules(self, event):
        self.rules = event.GetString().strip()
        if self.cmd == 'r.recode':
            self.rules = self.rules+'\n'

    def OnHelp(self, event):
        os.popen('g.manual --quiet %s ' % self.cmd)

    def OnOverwrite(self, event):
        self.overwrite = event.IsChecked()
