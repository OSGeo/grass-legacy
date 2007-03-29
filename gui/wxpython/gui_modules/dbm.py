#!/usr/bin/python
"""
Database browser for GRASS GIS >= 7

This program is based on FileHunter, publicated in "The wxPython Linux
Tutorial" on wxPython WIKI pages.

Usage:
    dbm.py table_name

"""
############################################################################
#
# MODULE:       dbm.py
# AUTHOR(S):    Jachym Cepicky jachym les-ejk cz
# PURPOSE:      Database manager for vector attribute tables stored in
#               GRASS GIS
# COPYRIGHT:    (C) 2007 by the GRASS Development Team
#
#               This program is free software under the GNU General Public
#               License (>=v2). Read the file COPYING that comes with GRASS
#               for details.
#
############################################################################

# discussion:
# using database drivers is IMHO impossible
# so, first step: parsing output form db.* commands and using SQL for
# manipulation

import wx
import os,sys
import time

import grassenv
import images
imagepath = images.__path__[0]
sys.path.append(imagepath)

ID_BUTTON=100
ID_EXIT=200

class MyListCtrl(wx.ListCtrl):
    def __init__(self, parent, id, tablename=None):
        wx.ListCtrl.__init__(self, parent, id, style=wx.LC_REPORT, )

        lengths =[]
        # FIXME: subprocess.Popen should be used
        # FIXME: Maximal number of columns, when the GUI is still usable
        i = 0
        for column in os.popen("db.columns table=%s" %
                (tablename)).readlines():

            column = column.strip()
            self.InsertColumn(i, column)
            self.SetColumnWidth(i, 5)
            i += 1
            lengths.append(1) # 


        # FIXME: subprocess.Popen should be used
        # FIXME: Max. number of rows, while the GUI is still usable
        j = 0
        for line in os.popen("""db.select -c sql="SELECT * FROM %s" """ % tablename):
            attributes = line.strip().split("|")

            k = 0
            for attribute in attributes:
                if len(attribute) > lengths[k]:
                    lengths[k] = len(attribute)
                if k == 0:
                    self.InsertStringItem(j, attribute)
                else:
                    self.SetStringItem(j, k, attribute)
                k += 1

            if (j % 2) == 0:
                self.SetItemBackgroundColour(j, '#e6f1f5')
            j = j + 1
        
        # setting column widths
        i = 0
        for length in lengths:
            self.SetColumnWidth(i, (length+5)*12)
            i += 1


class DBHunter(wx.Frame):
    def __init__(self, parent, id, title, tablename):
        wx.Frame.__init__(self, parent, -1, title)

        global imagepath
        self.tablename = tablename
        self.SetIcon(wx.Icon(os.path.join(imagepath,'grass_db.png'), wx.BITMAP_TYPE_ANY))


        self.table = MyListCtrl(self, -1, self.tablename)
        

        self.Bind(wx.EVT_SIZE, self.OnSize)

        filemenu= wx.Menu()
        filemenu.Append(ID_EXIT,"E&xit"," Terminate the program")
        editmenu = wx.Menu()
        netmenu = wx.Menu()
        showmenu = wx.Menu()
        configmenu = wx.Menu()
        helpmenu = wx.Menu()

        menuBar = wx.MenuBar()
        menuBar.Append(filemenu,"&File")
        menuBar.Append(editmenu, "&Edit")
        menuBar.Append(netmenu, "&Net")
        menuBar.Append(showmenu, "&Show")
        menuBar.Append(configmenu, "&Config")
        menuBar.Append(helpmenu, "&Help")
        self.SetMenuBar(menuBar)
        self.Bind(wx.EVT_MENU, self.OnExit, id=ID_EXIT)

        tb = self.CreateToolBar( wx.TB_HORIZONTAL | wx.NO_BORDER | wx.TB_FLAT | wx.TB_TEXT)
        tb.AddSimpleTool(10, wx.Bitmap('images/db_open_table.png'), 'Open table')
        #tb.AddSimpleTool(20, wx.Bitmap('images/up.png'), 'Up one directory')
        #tb.AddSimpleTool(30, wx.Bitmap('images/home.png'), 'Home')
        #tb.AddSimpleTool(40, wx.Bitmap('images/refresh.png'), 'Refresh')
        #tb.AddSeparator()
        #tb.AddSimpleTool(50, wx.Bitmap('images/write.png'), 'Editor')
        #tb.AddSimpleTool(60, wx.Bitmap('images/terminal.png'), 'Terminal')
        #tb.AddSeparator()
        #tb.AddSimpleTool(70, wx.Bitmap('images/help.png'), 'Help')
        tb.Realize()

        #self.sizer2 = wx.BoxSizer(wx.HORIZONTAL)

        #button1 = wx.Button(self, ID_BUTTON + 1, "F3 View")
        #button2 = wx.Button(self, ID_BUTTON + 2, "F4 Edit")
        #button3 = wx.Button(self, ID_BUTTON + 3, "F5 Copy")
        #button4 = wx.Button(self, ID_BUTTON + 4, "F6 Move")
        #button5 = wx.Button(self, ID_BUTTON + 5, "F7 Mkdir")
        #button6 = wx.Button(self, ID_BUTTON + 6, "F8 Delete")
        #button7 = wx.Button(self, ID_BUTTON + 7, "F9 Rename")
        #button8 = wx.Button(self, ID_EXIT, "F10 Quit")

        # self.sizer2.Add(button1, 1, wx.EXPAND)
        # self.sizer2.Add(button2, 1, wx.EXPAND)
        # self.sizer2.Add(button3, 1, wx.EXPAND)
        # self.sizer2.Add(button4, 1, wx.EXPAND)
        # self.sizer2.Add(button5, 1, wx.EXPAND)
        # self.sizer2.Add(button6, 1, wx.EXPAND)
        # self.sizer2.Add(button7, 1, wx.EXPAND)
        # self.sizer2.Add(button8, 1, wx.EXPAND)

        self.Bind(wx.EVT_BUTTON, self.OnExit, id=ID_EXIT)

        self.sizer = wx.BoxSizer(wx.VERTICAL)
        #self.sizer.Add(self.splitter,1,wx.EXPAND)
        self.sizer.Add(self.table,1, wx.EXPAND | wx.ALL, 3)
        #self.sizer.Add(self.sizer2,0,wx.EXPAND)
        self.SetSizer(self.sizer)

        size = wx.DisplaySize()
        self.SetSize(size)

        self.sb = self.CreateStatusBar()
        self.dbcon  = "table: %s; " % self.tablename
        for line in os.popen("db.connect -p").readlines():
            self.dbcon += line.strip() +"; "
        self.sb.SetStatusText(self.dbcon)
        self.Center()
        self.Show(True)


    def OnExit(self,e):
        self.Close(True)

    def OnSize(self, event):
        size = self.GetSize()
        #self.splitter.SetSashPosition(size.x / 2)
        self.sb.SetStatusText(self.dbcon)
        event.Skip()


    def OnDoubleClick(self, event):
        size =  self.GetSize()
        #self.splitter.SetSashPosition(size.x / 2)

if __name__ == "__main__":

    if len(sys.argv) != 2:
        print >>sys.stderr, __doc__
        sys.exit()

    app = wx.App(0)
    dbmanager = DBHunter(None, -1, 'GRASS Attribute Table Manager',sys.argv[1])
    app.MainLoop()

