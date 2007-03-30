#!/usr/bin/env python
"""
 Usage:
    sqlbuilder.py table_name

"""

import wx
import os,sys
import time

import grassenv
import images
imagepath = images.__path__[0]
sys.path.append(imagepath)


class SQLFrame(wx.Frame):
    def __init__(self, parent, id, title, table, qtype="select"):
        wx.Frame.__init__(self, parent, -1, title)

        self.SetTitle("SQL Builder for GRASS GIS - %s " % (qtype.upper()))
        self.SetIcon(wx.Icon(os.path.join(imagepath,'grass_sql.png'), wx.BITMAP_TYPE_ANY))

        # 
        # variables
        #
        self.tablename = table   # name of the table "select * from #self.tablename# "
        self.qtype = qtype        # type of the uqery: SELECT, UPDATE, DELETE, ...
        self.column_names = []       # array with column names
        self.columns = {}       # array with colum properties
        self.colvalues = []     # arrya with uniqe values in selected column

        # Init
        self.GetColumns()


        #
        # Buttons
        #
        self.btn_clear = wx.Button(self, -1, "Clear")
        self.btn_verify = wx.Button(self, -1, "Verify")
        self.btn_help = wx.Button(self, -1, "Help")
        self.btn_load = wx.Button(self, -1, "Load")
        self.btn_save = wx.Button(self, -1, "Save")
        self.btn_apply = wx.Button(self, -1, "Apply")
        self.btn_close = wx.Button(self, -1, "Close")
        self.btn_uniqe = wx.Button(self, -1, "Get unique values")

        self.btn_is = wx.Button(self, -1, "=")
        self.btn_isnot = wx.Button(self, -1, "!=")
        self.btn_like = wx.Button(self, -1, "LIKE")
        self.btn_gt = wx.Button(self, -1, ">=")
        self.btn_gtis = wx.Button(self, -1, ">")
        self.btn_lt = wx.Button(self, -1, "<=")
        self.btn_ltis = wx.Button(self, -1, "<")
        self.btn_or = wx.Button(self, -1, "OR")
        self.btn_not = wx.Button(self, -1, "NOT")
        self.btn_and = wx.Button(self, -1, "AND")
        self.btn_brackets = wx.Button(self, -1, "()")
        self.btn_prc = wx.Button(self, -1, "%")
        
        # 
        # Text labels
        #
        #self.label_headding = wx.StaticText(self, -1, '')

        #
        # Textareas
        # 
        self.text_sql = wx.TextCtrl(self, -1, '', size=(-1,50),style=wx.TE_MULTILINE)

        # 
        # List Boxes
        #
        self.list_columns = wx.ListBox(self, -1, wx.DefaultPosition, (130, 130), self.column_names, wx.LB_MULTIPLE|wx.LB_SORT)
        self.list_values = wx.ListBox(self, -1, wx.DefaultPosition, (130, 130), self.colvalues, wx.LB_MULTIPLE|wx.LB_SORT)
        
        #
        # Bindings
        # 
        self.btn_uniqe.Bind(wx.EVT_BUTTON, self.GetUniqueValues)
        self.btn_is.Bind(wx.EVT_BUTTON, self.AddMark)
        self.btn_isnot.Bind(wx.EVT_BUTTON, self.AddMark)
        self.btn_like.Bind(wx.EVT_BUTTON, self.AddMark)
        self.btn_gt.Bind(wx.EVT_BUTTON, self.AddMark)
        self.btn_gtis.Bind(wx.EVT_BUTTON, self.AddMark)
        self.btn_or.Bind(wx.EVT_BUTTON, self.AddMark)
        self.btn_lt.Bind(wx.EVT_BUTTON, self.AddMark)
        self.btn_ltis.Bind(wx.EVT_BUTTON, self.AddMark)
        self.btn_not.Bind(wx.EVT_BUTTON, self.AddMark)
        self.btn_brackets.Bind(wx.EVT_BUTTON, self.AddMark)
        self.btn_prc.Bind(wx.EVT_BUTTON, self.AddMark)
        self.btn_and.Bind(wx.EVT_BUTTON, self.AddMark)

        self.list_columns.Bind(wx.EVT_LISTBOX, self.AddColumnName)
        self.list_values.Bind(wx.EVT_LISTBOX, self.AddValue)

        #
        # Layout
        #
        pagesizer = wx.BoxSizer(wx.VERTICAL)

        buttonsizer1 = wx.BoxSizer(wx.HORIZONTAL)
        buttonsizer1.Add(self.btn_clear, 0, wx.LEFT|wx.RIGHT, 5)
        buttonsizer1.Add(self.btn_verify, 0, wx.LEFT|wx.RIGHT, 5 )
        buttonsizer1.Add(self.btn_help, 0, wx.LEFT|wx.RIGHT, 5, )
        buttonsizer1.Add(self.btn_load, 0, wx.LEFT|wx.RIGHT, 5,)
        buttonsizer1.Add(self.btn_save, 0, wx.LEFT|wx.RIGHT, 5, )
        buttonsizer1.Add(self.btn_apply, 0, wx.LEFT|wx.RIGHT, 5, )

        buttonsizer2 = wx.GridBagSizer(2, 2)
        buttonsizer2.Add(self.btn_is, (0,0))
        buttonsizer2.Add(self.btn_isnot, (1,0))
        buttonsizer2.Add(self.btn_like, (2, 0))

        buttonsizer2.Add(self.btn_gt, (0, 1))
        buttonsizer2.Add(self.btn_gtis, (1, 1))
        buttonsizer2.Add(self.btn_or, (2, 1))

        buttonsizer2.Add(self.btn_lt, (0, 2))
        buttonsizer2.Add(self.btn_ltis, (1, 2))
        buttonsizer2.Add(self.btn_not, (2, 2))

        buttonsizer2.Add(self.btn_brackets, (0, 3))
        buttonsizer2.Add(self.btn_prc, (1, 3))
        buttonsizer2.Add(self.btn_and, (2, 3))

        buttonsizer3 = wx.GridSizer(4, 3, 3, 3)
        buttonsizer3.Add(self.btn_apply,0,wx.RIGHT,5)
        buttonsizer3.Add(self.btn_close,0,wx.RIGHT,5)

        hsizer1 = wx.GridSizer(2, 2, 0, 0)
        hsizer1.Add((wx.StaticText(self,-1,"Columns: ",size=(-1,22))), proportion=0,border=0)
        hsizer1.Add((wx.StaticText(self,-1,"Unique values: ", size=(-1,22))), proportion=0,border=0)
        hsizer1.Add(self.list_columns, 1, wx.EXPAND)
        hsizer1.Add(self.list_values, 1, wx.EXPAND)

        pagesizer.Add((wx.StaticText(self,-1,self.qtype,size=(-1,22))), 0, 0)
        pagesizer.Add(hsizer1, 1, wx.EXPAND, 0)
        pagesizer.Add(self.btn_uniqe,0,wx.ALIGN_LEFT|wx.TOP,border=5)
        pagesizer.Add(buttonsizer2, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.TOP, border=5)
        pagesizer.Add(self.text_sql, proportion=1,  flag=wx.EXPAND|wx.TOP, border=5)
        pagesizer.Add(buttonsizer1, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.TOP, 5)
        pagesizer.Add(buttonsizer3, proportion=0, flag=wx.TOP, border=5)
        self.SetAutoLayout(True)
        self.SetSizer(pagesizer)
        pagesizer.Fit(self)
        #pagesizer.SetSizeHints(self)
        self.Layout()
        self.Show(True)

    def GetColumns(self):
        for line in os.popen("db.columns table=%s" % (self.tablename)):
            self.column_names.append(line.strip())
        for line in os.popen("db.describe -c table=%s" % (self.tablename)).readlines()[1:]:
            x,name,ctype = line.strip().split(":")
            self.columns[name] = {'type':ctype}
        return

    def GetUniqueValues(self,event):
        vals = []
        try:
            idx = self.list_columns.GetSelections()[0]
        except:
            return
        self.list_values.Clear()
        column = self.list_columns.GetString(idx)
        for line in os.popen("""db.select -c sql="SELECT %s FROM %s" """ %\
                (column,self.tablename)):
                self.list_values.Insert(line.strip(),0)

    def AddColumnName(self,event):
        idx = self.list_columns.GetSelections()[0]
        column = self.list_columns.GetString(idx)
        self.__addSomething(column)

    def AddValue(self,event):
        idx = self.list_values.GetSelections()[0]
        value = self.list_values.GetString(idx)
        idx = self.list_columns.GetSelections()[0]
        column = self.list_columns.GetString(idx)

        if self.columns[column]['type'].lower().find("chara") > -1:
            value = "'%s'" % value
        self.__addSomething(value)

    def AddMark(self,event):


        if event.GetId() == self.btn_is.GetId(): mark = "="
        elif event.GetId() == self.btn_isnot.GetId(): mark = "!="
        elif event.GetId() == self.btn_like.GetId(): mark = "LIKE"
        elif event.GetId() == self.btn_gt.GetId(): mark = ">"
        elif event.GetId() == self.btn_gtis.GetId(): mark = ">="
        elif event.GetId() == self.btn_lt.GetId(): mark = "<"
        elif event.GetId() == self.btn_ltis.GetId(): mark =  "<="
        elif event.GetId() == self.btn_or.GetId(): mark =  "OR"
        elif event.GetId() == self.btn_not.GetId(): mark = "NOT"
        elif event.GetId() == self.btn_and.GetId(): mark = "AND"
        elif event.GetId() == self.btn_brackets.GetId(): mark = "()"
        elif event.GetId() == self.btn_prc.GetId(): mark = "%"
        self.__addSomething(mark)

            
    def __addSomething(self,what):
        sqlstr = self.text_sql.GetValue()
        newsqlstr = ''
        position = self.text_sql.GetLastPosition()
        selection = self.text_sql.GetSelection()

        newsqlstr = sqlstr[:position]
        try:
            if newsqlstr[-1] != " ":
                newsqlstr += " "
        except:
            pass
        newsqlstr += what
        newsqlstr += " "+sqlstr[position:]

        self.text_sql.SetValue(newsqlstr)
        self.text_sql.SetInsertionPoint(position)


if __name__ == "__main__":

    if len(sys.argv) != 2:
        print >>sys.stderr, __doc__
        sys.exit()

    app = wx.App(0)
    sqlb = SQLFrame(None, -1, 'SQL Buiilder',sys.argv[1])
    app.MainLoop()


