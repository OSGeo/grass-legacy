"""
MODULE:    dbm.py

CLASSES:
    * Log
    * VirtualAttributeList
    * AttributeManager
    * DisplayAttributesDialog

PURPOSE:   GRASS attribute table manager

           This program is based on FileHunter, published in 'The wxPython Linux
           Tutorial' on wxPython WIKI pages.

           It also uses some functions at
           http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/426407

           dbm.py vector@mapset

AUTHOR(S): GRASS Development Team
           Original author: Jachym Cepicky <jachym.cepicky gmail.com>
           Martin Landa <landa.martin gmail.com>

COPYRIGHT: (C) 2007 by the GRASS Development Team

           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import sys
import os
import locale
import string

import wx
import wx.lib.mixins.listctrl as listmix

import grassenv
import cmd
from debug import Debug as Debug

class Log:
    """
    The log output is redirected to the status bar of the containing frame.
    """
    def __init__(self,parent):
        self.parent = parent

    def write(self,text_string):
        self.parent.SetStatusText(text_string.strip())

class VirtualAttributeList(wx.ListCtrl, listmix.ListCtrlAutoWidthMixin, listmix.ColumnSorterMixin):
    """
    The panel you want to test (VirtualAttributeList)
    """
    def __init__(self, parent, log, vectmap, pointdata=None):
        wx.ListCtrl.__init__( self, parent, -1, style=wx.LC_REPORT|wx.LC_HRULES|wx.LC_VRULES) #wx.VIRTUAL

        self.log=log

        self.vectmap = vectmap
        if not "@" in self.vectmap:
            self.vectmap = self.vectmap+"@"+grassenv.env["MAPSET"]
        self.mapname, self.mapset = self.vectmap.split("@")

        self.icon = ''
        self.pointsize = ''

        if pointdata:
            self.icon = pointdata[0]
            self.pointsize = pointdata[1]

        self.columns = []
        self.selectedCats  = []
        self.lastTurnSelectedCats = [] # just temporary, for comparation
        self.parent = parent
        self.qlayer = None

        #adding some attributes (colourful background for each item rows)
        self.attr1 = wx.ListItemAttr()
        self.attr1.SetBackgroundColour("light blue")
        self.attr2 = wx.ListItemAttr()
        self.attr2.SetBackgroundColour("white")
        self.il = wx.ImageList(16, 16)
        self.sm_up = self.il.Add(wx.ArtProvider_GetBitmap(wx.ART_GO_UP,wx.ART_TOOLBAR,(16,16)))
        self.sm_dn = self.il.Add(wx.ArtProvider_GetBitmap(wx.ART_GO_DOWN,wx.ART_TOOLBAR,(16,16)))
        self.SetImageList(self.il, wx.IMAGE_LIST_SMALL)

        if self.parent.gismgr: # GIS Manager is running?
            self.mapdisp = self.parent.gismgr.curr_page.maptree.mapdisplay
            self.map     = self.parent.gismgr.curr_page.maptree.Map

        # building the columns
        i = 0
        # FIXME: Maximal number of columns, when the GUI is still usable
        dbDescribe = cmd.Command (cmd = ["db.describe", "-c",
           "table=%s" % self.parent.tablename,
           "driver=%s" % self.parent.driver,
           "database=%s" % self.parent.database])

        for line in dbDescribe.module_stdout.readlines()[2:]:
            colnum, column, type, length = line.strip().split(":")
            # FIXME: here will be more types
            if type.lower().find("integer") > -1:
                self.columns.append({"name":column,"type":int})
            elif type.lower().find("double") > -1:
                self.columns.append({"name":column,"type":float})
            elif type.lower().find("float") > -1:
                self.columns.append({"name":column,"type":float})
            else:
                self.columns.append({"name":column,"type":str})

            self.InsertColumn(i, column)
            self.SetColumnWidth(i,  wx.LIST_AUTOSIZE_USEHEADER)
            i += 1
            if i >= 256:
                self.log.write("Can display only 256 columns")
                break

        #These two should probably be passed to init more cleanly
        #setting the numbers of items = number of elements in the dictionary
        self.itemDataMap = {}
        self.itemIndexMap = []

        self.LoadData()
        #self.SetItemCount(len(self.itemDataMap))

        #mixins
        listmix.ListCtrlAutoWidthMixin.__init__(self)
        listmix.ColumnSorterMixin.__init__(self, len(self.columns))

        #sort by genre (column 2), A->Z ascending order (1)
        self.SortListItems(0, 1)

        #events
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected, self)
        self.Bind(wx.EVT_LIST_ITEM_DESELECTED, self.OnItemDeselected, self)
        self.Bind(wx.EVT_LIST_ITEM_ACTIVATED, self.OnItemActivated, self)
        #self.Bind(wx.EVT_LIST_DELETE_ITEM, self.OnItemDelete, self.list)
        self.Bind(wx.EVT_LIST_COL_CLICK, self.OnColClick, self)
        #self.Bind(wx.EVT_LIST_COL_RIGHT_CLICK, self.OnColRightClick, self.list)
        #self.Bind(wx.EVT_LIST_COL_BEGIN_DRAG, self.OnColBeginDrag, self.list)
        #self.Bind(wx.EVT_LIST_COL_DRAGGING, self.OnColDragging, self.list)
        #self.Bind(wx.EVT_LIST_COL_END_DRAG, self.OnColEndDrag, self.list)
        #self.Bind(wx.EVT_LIST_BEGIN_LABEL_EDIT, self.OnBeginEdit, self.list)

        #self.list.Bind(wx.EVT_LEFT_DCLICK, self.OnDoubleClick)
        #self.list.Bind(wx.EVT_RIGHT_DOWN, self.OnRightDown)

        if self.parent.gismgr:
            self.mapdisp.MapWindow.Bind(wx.EVT_LEFT_DOWN, self.onMapClick)

            self.timer = wx.PyTimer(self.RedrawMap)
            # check each 0.1s
            self.timer.Start(100)

    def LoadData(self,where=None):

        # prepare command string
        cmdv = ["db.select", "-c",
                "table=%s" % self.parent.tablename,
                "database=%s" % self.parent.database,
                "driver=%s" % self.parent.driver]

        if where:
            self.ClearAll()
            cmdv = ["db.select", "-c",
                    "sql=SELECT * FROM %s WHERE %s" % (self.parent.tablename, where),
                    "database=%s" % self.parent.database,
                    "driver=%s" % self.parent.driver]

        # run command
        vDbSelect = cmd.Command (cmd=cmdv)

        # FIXME: Max. number of rows, while the GUI is still usable
        i = 0
        # read data
        for line in vDbSelect.module_stdout.readlines():
            attributes = line.strip().split("|")
            self.itemDataMap[i] = []

            # convert to appropriate type
            for j in range(len(attributes)):
                try:
                    attributes[j] = self.columns[j]["type"](attributes[j])
                except:
                    pass

            # insert to table
            index = self.InsertStringItem(sys.maxint, str(attributes[0]))
            self.itemDataMap[i].append(attributes[0])
            for j in range(len(attributes[1:])):
                self.SetStringItem(index, j+1, str(attributes[j+1]))
                self.itemDataMap[i].append(attributes[j+1])

            self.SetItemData(index, i)
            self.itemIndexMap.append(i)

            i += 1
            if i >= 32000:
                self.log.write(_("Can display only 32000 lines"))
                break

    def OnCloseWindow(self, event):
        if self.qlayer:
            self.map.delLayer(item='qlayer')

    def OnColClick(self,event):
        self._col = event.GetColumn()
        event.Skip()

    def OnItemSelected(self, event):
        self.currentItem = event.m_itemIndex
        #self.log.write('OnItemSelected: "%s", "%s"\n' %
        #                   (self.currentItem,
        #                    self.GetItemText(self.currentItem)))

        # now the funny part:
        # make 1,2,3,4 to 1-4

        self.selectedCats.append(int(self.GetItemText(self.currentItem)))
        self.selectedCats.sort()

        event.Skip()

    def RedrawMap(self):
        if self.lastTurnSelectedCats[:] != self.selectedCats[:]:
            if self.qlayer:
                self.map.DeleteLayer(self.qlayer)

            cats = self.selectedCats
            catstr = ""
            i = 0
            while 1:
                next = 0
                j = 0
                while 1:
                    try:
                        if cats[i+j]+1 == cats[i+j+1]:
                            next +=1
                        else:
                            break
                    except IndexError:
                        next = 0
                    if j+i >= len(cats)-2:
                        break
                    else:
                        j += 1
                if next > 1:
                    catstr += "%d-%d," % (cats[i], cats[i+next])
                    i += next
                else:
                    catstr += "%d," % (cats[i])

                i += 1
                if i >= len(cats):
                    break

            if catstr[-1] == ",":
                catstr = string.join(catstr[:-1],"")


            # FIXME: width=1, because of maybe bug in PNG driver elusion
            # should be width=3 or something like this
            cmd = ["d.vect",
                   "map=%s" % self.vectmap,
                   "color=yellow",
                   "fcolor=yellow",
                   "cats=%s" % catstr,
                   "width=3"]
            #print cmd
            if self.icon:
                cmd.append("icon=%s" % (self.icon))
            if self.pointsize:
                cmd.append("size=%s" % (self.pointsize))

            self.qlayer = self.map.AddLayer(type="vector", name='qlayer', command=cmd,
                                            l_active=True, l_hidden=True, l_opacity=1, l_render=False)
            self.mapdisp.ReDraw(None)
            self.lastTurnSelectedCats = self.selectedCats[:]

    def OnItemActivated(self, event):
        self.currentItem = event.m_itemIndex
        self.log.write("OnItemActivated: %s\nTopItem: %s\n" %
                           (self.GetItemText(self.currentItem), self.GetTopItem()))
        event.Skip()

    def getColumnText(self, index, col):
        item = self.GetItem(index, col)
        return item.GetText()

    def OnItemDeselected(self, event):
        #self.log.write("OnItemDeselected: %s" % event.m_itemIndex)
        self.selectedCats.remove(int(self.GetItemText(event.m_itemIndex)))
        self.selectedCats.sort()
        event.Skip()


        # ---------------------------------------------------
        # These methods are callbacks for implementing the
        # "virtualness" of the list...

        # def OnGetItemText(self, item, col):
        #     index=self.itemIndexMap[item]
        #     s = self.itemDataMap[index][col]
        #     return s

        # def OnGetItemImage(self, item):
        #     index=self.itemIndexMap[item]
        #     if ( index % 2) == 0:

    def OnGetItemAttr(self, item):
        index=self.itemIndexMap[item]

        return self.attr2
        # if ( index % 2) == 0:
        #    return self.attr2
        # else:
        #    return self.attr1

        # ---------------------------------------------------
        # Matt C, 2006/02/22
        # Here's a better SortItems() method --
        # the ColumnSorterMixin.__ColumnSorter() method already handles the ascending/descending,
        # and it knows to sort on another column if the chosen columns have the same value.

        # def SortItems(self,sorter=cmp):
        #     items = list(self.itemDataMap.keys())
        #     # for i in range(len(items)):
        #     #     items[i] =  self.columns[self.columnNumber]["type"](items[i])
        #     items.sort(self.Sorter)
        #     #items.sort(sorter)
        #     # for i in range(len(items)):
        #     #     items[i] =  str(items[i])
        #     self.itemIndexMap = items

        #     # redraw the list
        #     self.Refresh()

        # Used by the ColumnSorterMixin, see wx/lib/mixins/listctrl.py
    def GetListCtrl(self):
        return self

    # stolen from python2.4/site-packages/wx-2.8-gtk2-unicode/wx/lib/mixins/listctrl.py
    # def Sorter(self, key1,key2):
    #     col = self._col
    #     ascending = self._colSortFlag[col]
    #     # convert, because the it is allways string
    #     try:
    #         item1 = self.columns[col]["type"](self.itemDataMap[key1][col])
    #     except:
    #         item1 = ''
    #     try:
    #         item2 = self.columns[col]["type"](self.itemDataMap[key2][col])
    #     except:
    #         item2 = ''

    #     #--- Internationalization of string sorting with locale module
    #     if type(item1) == type('') or type(item2) == type(''):
    #         cmpVal = locale.strcoll(str(item1), str(item2))
    #     else:
    #         cmpVal = cmp(item1, item2)
    #     #---

    #     # If the items are equal then pick something else to make the sort v    ->alue unique
    #     if cmpVal == 0:
    #         cmpVal = apply(cmp, self.GetSecondarySortValues(col, key1, key2))

    #     if ascending:
    #         return cmpVal
    #     else:
    #         return -cmpVal

    #return cmp(self.columns[self.columnNumber]["type"](a),
    #           self.columns[self.columnNumber]["type"](b))

    # Used by the ColumnSorterMixin, see wx/lib/mixins/listctrl.py
    def GetSortImages(self):
        return (self.sm_dn, self.sm_up)

    # XXX Looks okay to remove this one (was present in the original demo)
    # def getColumnText(self, index, col):
    #    item = self.GetItem(index, col)
    #    return item.GetText()

    def onMapClick(self, event):
        """
        Gets coordinates from mouse clicking on display window
        """
        # map coordinates
        x, y = self.mapdisp.MapWindow.Pixel2Cell(event.GetPositionTuple())
        #print 'coordinates =',x,y

        category = ""
        for line in os.popen("v.what east_north=%f,%f map=%s" %\
                (x,y,self.vectmap)).readlines():
            if "Category:" in line:
                category = line.strip().split(" ")[1]

        #print category

        for idx in range(self.GetItemCount()):
            item = self.GetItem(idx, 0)
            if item.GetText() == category:
                #print idx
                #self.Select(idx,True)
                self.EnsureVisible( idx )
                self.SetItemState(idx, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)
            else:
                #self.SetItemState(idx, wx.LIST_STATE_DESELECTED, wx.LIST_STATE_DESELECTED)
                self.Select(idx,False)


                #        try:
                #            os.environ["GRASS_MESSAGE_FORMAT"] = "gui"
                #            cmd = "v.what -a east_north=%d,%d distance=%d map=%@%" % (x,y,100,self.tablename, self.self.mapset)
                #            self.cmd_output.write(cmd+"\n----------\n")
                #            p = Popen(cmd +" --verbose", shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True)
                #
                #            oline = p.stderr.readline()
                #            while oline:
                #                oline = oline.strip()
                #                oline = p.stderr.readline()
                #                if oline.find("GRASS_INFO_MESSAGE")>-1:
                #                    self.cmd_output.write(string.split(oline,maxsplit=1)[1]+"\n")
                #                elif oline.find("GRASS_INFO_WARNING")>-1:
                #                    self.cmd_output.write("WARNING: "+string.split(oline,maxsplit=1)[1]+"\n")
                #                elif oline.find("GRASS_INFO_ERROR")>-1:
                #                    self.cmd_output.write("ERROR: "+string.split(oline,maxsplit=1)[1]+"\n")
                #
                #            oline = p.stdout.readline()
                #            while oline:
                #                oline = oline.strip()
                #                self.cmd_output.write(oline+"\n")
                #                print oline+"\n"
                #                print >> sys.stderr, oline
                #                oline = p.stdout.readline()
                #
                #            if p.stdout < 0:
                #                print >> sys.stderr, "Child was terminated by signal", p.stdout
                #            elif p.stdout > 0:
                #                print >> sys.stderr, p.stdout
                #                pass
                #        except OSError, e:
                #            print >> sys.stderr, "Execution failed:", e

                #        try:
                #            os.environ["GRASS_MESSAGE_FORMAT"] = "gui"
                #            cmd = "v.what -a east_north=%d,%d distance=%d map=%@%" % (x,y,100,self.tablename, self.self.mapset)
                #            self.cmd_output.write(cmd+"\n----------\n")
                #            p = Popen(cmd +" --verbose", shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True)
                #
                #            oline = p.stderr.readline()
                #            while oline:
                #                oline = oline.strip()
                #                oline = p.stderr.readline()
                #                if oline.find("GRASS_INFO_MESSAGE")>-1:
                #                    self.cmd_output.write(string.split(oline,maxsplit=1)[1]+"\n")
                #                elif oline.find("GRASS_INFO_WARNING")>-1:
                #                    self.cmd_output.write("WARNING: "+string.split(oline,maxsplit=1)[1]+"\n")
                #                elif oline.find("GRASS_INFO_ERROR")>-1:
                #                    self.cmd_output.write("ERROR: "+string.split(oline,maxsplit=1)[1]+"\n")
                #
                #            oline = p.stdout.readline()
                #            while oline:
                #                oline = oline.strip()
                #                self.cmd_output.write(oline+"\n")
                #                print oline+"\n"
                #                print >> sys.stderr, oline
                #                oline = p.stdout.readline()
                #
                #            if p.stdout < 0:
                #                print >> sys.stderr, "Child was terminated by signal", p.stdout
                #            elif p.stdout > 0:
                #                print >> sys.stderr, p.stdout
                #                pass
                #        except OSError, e:
                #            print >> sys.stderr, "Execution failed:", e

class AttributeManager(wx.Frame):
    """
    The main window

    This is where you populate the frame with a panel from the demo.
    original line in runTest (in the demo source):
    win = TestPanel(nb, log)
    this is changed to:
    self.win=TestPanel(self,log)
    """
    def __init__(self, parent, id, title, vectmap, size = wx.DefaultSize, style = wx.DEFAULT_FRAME_STYLE,
                 pointdata=None):

        # get list of attribute tables (TODO: open more tables)
        vDbConnect = cmd.Command (cmd=["v.db.connect", "-g", "map=%s" % vectmap])

        try:
            if vDbConnect.returncode == 0:
                (self.layer, self.tablename, self.column, self.database, self.driver) = vDbConnect.module_stdout.readlines()[0].strip().split()
            else:
                raise
        except:
            self.layer = None

        if not self.layer:
            dlg = wx.MessageDialog(parent, _("No attribute table available for vector map <%s>") % vectmap, _("Error"), wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return

        wx.Frame.__init__(self, parent, id, title, size=size, style=style)

        self.CreateStatusBar(1)

        self.vectmap = vectmap
        self.parent  = parent
        self.gismgr  = parent

        log=Log(self)

        # most important part
        self.win = VirtualAttributeList(self, log, vectmap=vectmap, pointdata=pointdata)

        # buttons
        self.btn_apply = wx.Button(self, -1, "Apply")
        # self.btn_unselect = wx.Button(self, -1, "Unselect")
        self.btn_sqlbuilder = wx.Button(self, -1, "SQL Builder")

        # check
        # self.check_add_to_selection = wx.CheckBox(self, -1, "Add to selection")

        # textarea
        self.text_query = wx.TextCtrl(self,-1,"")

        # label
        self.sqlabel=wx.StaticText(self,-1,"SELECT * FROM %s WHERE " % self.tablename)
        self.label_query = wx.StaticText(self,-1,"")

        # box
        self.sqlbox = wx.StaticBox(self, -1, "SQL Query:")

        self.btn_sqlbuilder.Bind(wx.EVT_BUTTON, self.OnBuilder)

        self.__layout()
        self.Show()

    def OnBuilder(self,event):
        import sqlbuilder
        self.builder = sqlbuilder.SQLFrame(self,-1,"SQL Builder",self.vectmap)


    def OnApply(self,event):
        self.win.LoadData(where=self.text_query.GetValue().strip())


    def OnTextEnter(self, event):
        pass

    def OnSQLBuilder(self, event):
        pass

    def __layout(self):
        #self.panel = wx.Panel(self,-1, style=wx.SUNKEN_BORDER)

        self.label_query.SetMinSize((500,50))
        self.text_query.SetMinSize((500,-1))

        bsizer = wx.StaticBoxSizer(self.sqlbox, wx.VERTICAL)
        bsizer.Add(self.label_query, flag=wx.EXPAND)


        pagesizer= wx.BoxSizer(wx.VERTICAL)
        toolsizer1 = wx.BoxSizer(wx.HORIZONTAL)
        #toolsizer2 = wx.BoxSizer(wx.HORIZONTAL)

        toolsizer1.Add(self.sqlabel, flag=wx.ALIGN_CENTER_VERTICAL,
                proportion=1)
        toolsizer1.Add(self.text_query,flag=wx.SHAPED|wx.GROW, proportion=2,)
        toolsizer1.Add(self.btn_sqlbuilder,flag=wx.ALIGN_RIGHT,proportion=0)
        toolsizer1.Add(self.btn_apply,flag=wx.ALIGN_RIGHT,proportion=0)

        pagesizer.Add(bsizer,flag=wx.EXPAND)
        pagesizer.Add(self.win, proportion=1, flag=wx.EXPAND, border=1)
        pagesizer.Add(toolsizer1)

        self.SetSizer(pagesizer)
        pagesizer.Fit(self)
        self.Layout()

class DisplayAttributesDialog(wx.Dialog):
    """
    Standard dialog used for adding new/updating existing or
    displaying (read-only mode) attributes of vector map layers.

    Attribute data can be selected based on layer and category number
    or coordinates. If layer=-1 all layer are queried.
    """
    def __init__(self, parent, map,
                 layer=1, cat=1, queryCoords=None, qdist=1,
                 style=wx.DEFAULT_DIALOG_STYLE, pos=wx.DefaultPosition,
                 action="add"):

        self.map         = map
        self.layer       = layer
        self.cat         = cat
        self.queryCoords = queryCoords
        self.qdist       = qdist
        self.action      = action

        self.selectedLines = [] # id of selected feature

        self.mapInfo = VectorAttributesInfo(self.map)

        if self.layer > 0 and \
               self.layer not in self.mapInfo.layers.keys():
            dlg = wx.MessageDialog(None,
                                   _("Attribute table not found.\n"
                                     "Layer %d is not available in vector map <%s>") % \
                                   (layer, self.map),
                                   _("Error"), wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            self.mapInfo = None
            return

        wx.Dialog.__init__(self, parent=parent, id=wx.ID_ANY,
                           title="", style=style, pos=pos)

        # dialog body
        mainSizer = wx.BoxSizer(wx.VERTICAL)

        # notebook
        notebook = wx.Notebook(parent=self, id=wx.ID_ANY, style=wx.BK_DEFAULT)
        for layer in self.mapInfo.layers.keys():
            if self.layer > 0 and \
                    self.layer != layer:
                continue

            # line detected, number of selected records
            line, selected = self.mapInfo.SelectFromTable(layer, self.cat,
                                                          self.queryCoords, self.qdist)

            if (self.action == "add" and selected > 0) or \
                   self.action == "update":
                self.SetTitle(_("Update attributes"))
            elif self.action == "add":
                self.SetTitle(_("Add attributes"))
            else:
                self.SetTitle(_("Display attributes"))

            if self.action == "add":
                pass
            else:
                if not line and selected == 0:
                    continue

            if line:
                self.selectedLines.append(line)

            panel = wx.Panel(parent=notebook, id=wx.ID_ANY)
            notebook.AddPage(page=panel, text=_(" %s %d ") % (_("Layer"), layer))

            # notebook body
            border = wx.BoxSizer(wx.VERTICAL)
            box    = wx.StaticBox (parent=panel, id=wx.ID_ANY,
                                   label="")
            boxFont = self.GetFont()
            boxFont.SetWeight(wx.FONTWEIGHT_BOLD)
            box.SetFont(boxFont)
            sizer  = wx.StaticBoxSizer(box, wx.VERTICAL)
            flexSizer = wx.FlexGridSizer (cols=4, hgap=3, vgap=3)
            flexSizer.AddGrowableCol(0)

            table = self.mapInfo.layers[layer]["table"]
            columns = self.mapInfo.tables[table]
            # columns
            for name in columns.keys():
                type  = columns[name][0]
                value = columns[name][1]
                if name.lower() == "cat":
                    box.SetLabel(" %s %s " % (_("Category"), value))
                    colValue = box
                else:
                    colName = wx.StaticText(parent=panel, id=wx.ID_ANY, label=name)
                    colType = wx.StaticText(parent=panel, id=wx.ID_ANY, label="[" + type.lower() + "]")
                    delimiter = wx.StaticText(parent=panel, id=wx.ID_ANY, label=":")

                    colValue = wx.TextCtrl(parent=panel, id=wx.ID_ANY, value=value, size=(250, -1)) # TODO: validator
                    colValue.SetName(name)
                    self.Bind(wx.EVT_TEXT, self.OnSQLStatement, colValue)

                    flexSizer.Add(colName, proportion=0,
                                  flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
                    flexSizer.Add(colType, proportion=0,
                                  flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
                    flexSizer.Add(delimiter, proportion=0,
                                  flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
                    flexSizer.Add(colValue, proportion=0,
                                  flag=wx.EXPAND | wx.ALIGN_CENTER_VERTICAL)

                    sizer.Add(item=flexSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=5)
                    border.Add(item=sizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=5)
                    panel.SetSizer(border)

                # add widget reference to self.columns
                columns[name].append(colValue.GetId())

        # buttons
        btnSizer = wx.StdDialogButtonSizer()
        btnCancel = wx.Button(self, wx.ID_CANCEL)
        btnSizer.AddButton(btnCancel)
        btnReset  = wx.Button(self, wx.ID_APPLY, _("&Reset"))
        btnSizer.AddButton(btnReset)
        btnSubmit = wx.Button(self, wx.ID_OK, _("Submit"))
        btnSubmit.SetDefault()
        btnSizer.AddButton(btnSubmit)
        btnSizer.Realize()

        mainSizer.Add(item=notebook, proportion=1, flag=wx.EXPAND | wx.ALL, border=5)
        #mainSizer.Add(item=border, proportion=1,
        #flag=wx.EXPAND | wx.LEFT | wx.RIGHT | wx.BOTTOM, border=5)
        mainSizer.Add(item=btnSizer, proportion=0,
                      flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)

        # bindigs
        btnReset.Bind(wx.EVT_BUTTON, self.OnReset)

        self.SetSizer(mainSizer)
        mainSizer.Fit(self)

        if notebook.GetPageCount() == 0:
            Debug.msg(2, "DisplayAttributesDialog(): Nothing found!")
            self.mapInfo = None

    def __SelectAttributes(self, layer):
        """Select attributes"""

    def OnSQLStatement(self, event):
        """Update SQL statement"""
        pass

    def GetSQLString(self):
        """Create SQL statement string based on self.sqlStatement"""
        for layer in self.mapInfo.layers.keys():
            table = self.mapInfo.layers[layer]["table"]
            columns = self.mapInfo.tables[table]
            cat = columns["cat"][1]
            # find updated values
            updatedColumns = []
            updatedValues = []
            for name in columns.keys():
                if name == "cat":
                    continue
                type, value, id  = columns[name]
                try:
                    newvalue = self.FindWindowById(id).GetValue()
                except:
                    newvalue = self.FindWindowById(id).GetLabel()
                if newvalue != value:
                    updatedColumns.append(name)
                    if type != 'character':
                        updatedValues.append(newvalue)
                    else:
                        updatedValues.append("'" + newvalue + "'")

            if self.action != "add" and len(updatedValues) == 0:
                sqlString = ""
                Debug.msg(3, "DisplayAttributesDialog.GetSQLString(): %s" % sqlString)
                return sqlString

            if self.action == "add":
                sqlString = "INSERT INTO %s (cat," % table
            else:
                sqlString = "UPDATE %s SET " % table

            for idx in range(len(updatedColumns)):
                name = updatedColumns[idx]
                if self.action == "add":
                    sqlString += name + ","
                else:
                    sqlString += name + "=" + updatedValues[idx] + ","

            sqlString = sqlString[:-1] # remove last comma

            if self.action == "add":
                sqlString += ") VALUES (%s," % cat
                for value in updatedValues:
                    sqlString += str(value) + ","
                sqlString = sqlString[:-1] # remove last comma
                sqlString += ")"
            else:
                sqlString += " WHERE cat=%s" % cat

        Debug.msg(3, "DisplayAttributesDialog.GetSQLString(): %s" % sqlString)
        return sqlString

    def OnReset(self, event):
        """Reset form"""
        for layer in self.mapInfo.layers.keys():
            table = self.mapInfo.layers[layer]["table"]
            columns = self.mapInfo.tables[table]
        for name in columns.keys():
            type, value, id = columns[name]
            if name.lower() != "cat":
                self.FindWindowById(id).SetValue(value)

    def OnSubmit(self, event):
        """Submit record"""

        self.Close()

class VectorAttributesInfo:
    """Class providing information about attributes
    linked to the vector map"""
    def __init__(self, map):
        self.map = map
        # {layer number : {table, database, driver})
        self.layers = {}
        # {table : [(column name, type, value)]}
        self.tables = {}
        if not self.__CheckDBConnection(): # -> self.layers
            return

        self.__DescribeTables() # -> self.tables

    def __CheckDBConnection(self):
        """Check DB connection"""
        layerCommand = cmd.Command(cmd=["v.db.connect",
                                        "-g", "--q",
                                        "map=%s" % self.map,],
                                   dlgMsg='txt')
        if layerCommand.returncode != 0:
            return False

        # list of available layers & (table, database, driver)
        for line in layerCommand.ReadStdOutput():
            lineList = line.split(' ')
            self.layers[int(lineList[0])] = { "table"    : lineList[1],
                                              "database" : lineList[3],
                                              "driver"   : lineList[4] }

        if (len(self.layers.keys()) == 0):
            return False

        return True

    def __DescribeTables(self):
        """Describe linked tables"""
        for layer in self.layers.keys():
            # determine column names and types
            columnsCommand = cmd.Command (cmd=["v.db.connect", "-c", "--q",
                                               "map=%s" % self.map,
                                               "layer=%d" % layer])
            table = self.layers[layer]["table"]

            columns = {} # {name: [type, value]}

            if columnsCommand.returncode == 0:
                for line in columnsCommand.ReadStdOutput():
                    columnType, columnName = line.split('|')
                    columnType = columnType.lower().strip()
                    columns[columnName] = [columnType, ""] # default value ("")
            else:
                pass

            self.tables[table] = columns

    def SelectFromTable(self, layer=1, cat=1, queryCoords=None, qdist=1):
        """Select records from the table

        Based on coordinates or category
        """
        table = self.layers[layer]["table"]
        selected = 0
        line = None
        if queryCoords:
            # snapping distance
            cmdWhat = cmd.Command(cmd=['v.what',
                                       '-a', '-d', '--q',
                                       'map=%s' % self.map,
                                       'east_north=%f,%f' % \
                                       (float(queryCoords[0]), float(queryCoords[1])),
                                       'distance=%f' % qdist])

            if cmdWhat.returncode == 0:
                read = False
                for item in cmdWhat.ReadStdOutput():
                    found = True
                    if read:
                        name, value = item.split(':')
                        name = name.strip()
                        self.tables[table][name][1] = value.strip()
                        selected+=1;
                    if "line:" in item.lower():
                        line = int(item.split(':')[1].strip())
                    elif "category:" in item.lower():
                        self.tables[table]["cat"][1] = item.split(':')[1].strip()
                    elif "key column:" in item.lower():
                        read = True
        else:
            # select values
            selectCommand = cmd.Command(cmd=["v.db.select", "-v", "--q",
                                             "map=%s" % self.map,
                                             "layer=%d" % layer,
                                             "where=cat=%d" % cat])
            self.tables[table]["cat"][1] = str(cat)
            if selectCommand.returncode == 0:
                for line in selectCommand.ReadStdOutput():
                    name, value = line.split('|')
                    self.tables[table][name][1] = value
                    selected+=1

        return (line, selected)

def main(argv=None):
    if argv is None:
        argv = sys.argv

    if len(argv) != 2:
        print >> sys.stderr, __doc__
        sys.exit()

    # Command line arguments of the script to be run are preserved by the
    # hotswap.py wrapper but hotswap.py and its options are removed that
    # sys.argv looks as if no wrapper was present.
    #print "argv:", `argv`

    #some applications might require image handlers
    #wx.InitAllImageHandlers()

    app = wx.PySimpleApp()
    f = AttributeManager(parent=None, id=wx.ID_ANY, title=_("GRASS Attribute Table Manager"), size=(700,600), vectmap=argv[1])
    app.MainLoop()

if __name__ == '__main__':
    main()
