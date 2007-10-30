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
           Various updates: Martin Landa <landa.martin gmail.com>

COPYRIGHT: (C) 2007 by the GRASS Development Team

           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import sys
import os
import locale
import string
import tempfile

import wx
import wx.lib.mixins.listctrl as listmix

import sqlbuilder
import grassenv
import gcmd
from debug import Debug as Debug

class Log:
    """
    The log output is redirected to the status bar of the containing frame.
    """
    def __init__(self, parent):
        self.parent = parent

    def write(self, text_string):
        """Update status bar"""
        self.parent.SetStatusText(text_string.strip())

class VirtualAttributeList(wx.ListCtrl, listmix.ListCtrlAutoWidthMixin, listmix.ColumnSorterMixin):
    """
    Support virtual attribute list class
    """
    def __init__(self, parent, log, vectmap, pointdata=None):
        wx.ListCtrl.__init__( self, parent=parent, id=wx.ID_ANY,
                              style=wx.LC_REPORT | wx.LC_HRULES | wx.LC_VRULES) # | wx.LC_VIRTUAL)

        #
        # initialize variables
        self.log     = log
        self.vectmap = vectmap
        self.parent  = parent
        self.qlayer  = None
        self.icon      = ''
        self.pointsize = ''

        if not "@" in self.vectmap:
            self.vectmap = self.vectmap + "@" + grassenv.GetGRASSVariable("MAPSET")
        self.mapname, self.mapset = self.vectmap.split("@")

        if pointdata:
            self.icon      = pointdata[0]
            self.pointsize = pointdata[1]

        self.columns              = []
        self.selectedCats         = []
        self.lastTurnSelectedCats = [] # just temporary, for comparation

        #
        # add some attributes (colourful background for each item rows)
        #
        self.attr1 = wx.ListItemAttr()
        self.attr1.SetBackgroundColour("light blue")
        self.attr2 = wx.ListItemAttr()
        self.attr2.SetBackgroundColour("white")
        self.il = wx.ImageList(16, 16)
        self.sm_up = self.il.Add(wx.ArtProvider_GetBitmap(wx.ART_GO_UP,   wx.ART_TOOLBAR, (16,16)))
        self.sm_dn = self.il.Add(wx.ArtProvider_GetBitmap(wx.ART_GO_DOWN, wx.ART_TOOLBAR, (16,16)))
        self.SetImageList(self.il, wx.IMAGE_LIST_SMALL)

        if self.parent.gismgr: # Layer Manager is running?
            self.mapdisp = self.parent.gismgr.curr_page.maptree.mapdisplay
            self.map     = self.parent.gismgr.curr_page.maptree.Map
        else:
            self.mapdisp = self.map = None

        # building the columns
        # FIXME: Maximal number of columns, when the GUI is still usable
        dbDescribe = gcmd.Command (cmd = ["db.describe", "-c", "--q",
                                          "table=%s" % self.parent.tablename,
                                          "driver=%s" % self.parent.driver,
                                          "database=%s" % self.parent.database])

        # structure (e.g.)
        #
        # ncols: 2
        # nrows: 11
        # Column 1: cat:INTEGER:11
        # Column 2: label:CHARACTER:43
        i = 0
        for line in dbDescribe.ReadStdOutput()[2:]: # skip ncols and nrows
            colnum, column, type, length = line.strip().split(":")
            column.strip()
            # FIXME: here will be more types
            if type.lower().find("integer") > -1:
                self.columns.append({"name": column, "type": int})
            elif type.lower().find("double") > -1:
                self.columns.append({"name": column, "type": float})
            elif type.lower().find("float") > -1:
                self.columns.append({"name": column, "type": float})
            else:
                self.columns.append({"name": column, "type": str})

        # These two should probably be passed to init more cleanly
        # setting the numbers of items = number of elements in the dictionary
        self.itemDataMap  = {}
        self.itemIndexMap = []

        self.LoadData()
        # self.SetItemCount(len(self.itemDataMap))

        # setup mixins
        listmix.ListCtrlAutoWidthMixin.__init__(self)
        listmix.ColumnSorterMixin.__init__(self, len(self.columns))

        # sort by cat by default
        self.SortListItems(col=0, ascending=1) # FIXME category column can be different

        #
        # events
        #
        self.Bind(wx.EVT_LIST_ITEM_SELECTED,   self.OnItemSelected,   self)
        self.Bind(wx.EVT_LIST_ITEM_DESELECTED, self.OnItemDeselected, self)
        self.Bind(wx.EVT_LIST_ITEM_ACTIVATED,  self.OnItemActivated,  self)
        # self.Bind(wx.EVT_LIST_COL_CLICK,       self.OnColClick,       self)
        # self.Bind(wx.EVT_LIST_DELETE_ITEM, self.OnItemDelete, self.list)
        # self.Bind(wx.EVT_LIST_COL_RIGHT_CLICK, self.OnColRightClick, self.list)
        # self.Bind(wx.EVT_LIST_COL_BEGIN_DRAG, self.OnColBeginDrag, self.list)
        # self.Bind(wx.EVT_LIST_COL_DRAGGING, self.OnColDragging, self.list)
        # self.Bind(wx.EVT_LIST_COL_END_DRAG, self.OnColEndDrag, self.list)
        # self.Bind(wx.EVT_LIST_BEGIN_LABEL_EDIT, self.OnBeginEdit, self.list)
        # self.list.Bind(wx.EVT_LEFT_DCLICK, self.OnDoubleClick)
        # self.list.Bind(wx.EVT_RIGHT_DOWN, self.OnRightDown)

        if self.parent.gismgr:
            self.mapdisp.MapWindow.Bind(wx.EVT_LEFT_DOWN, self.OnMapClick)

            self.timer = wx.PyTimer(self.RedrawMap)
            # check each 0.1s
            self.timer.Start(100)

    def LoadData(self, cols='*', where=''):
        """Load data into list"""

        self.DeleteAllItems()

        # self.ClearAll()
        for i in range(self.GetColumnCount()):
            self.DeleteColumn(0)

        i = 0
        if cols != '*':
            for column in cols.split(','):
                self.InsertColumn(col=i, heading=column)
                i += 1
                
                if i >= 256:
                    self.log.write(_("Can display only 256 columns"))

        else:
            for column in self.columns:
                self.InsertColumn(col=i, heading=column["name"])
                i += 1

                if i >= 256:
                    self.log.write(_("Can display only 256 columns"))

        if where is '' or where is None:
            cmdv = ["db.select", "-c", "--q",
                    "sql=SELECT %s FROM %s" % (cols, self.parent.tablename),
                    "database=%s" % self.parent.database,
                    "driver=%s"   % self.parent.driver]
        else:
            cmdv = ["db.select", "-c", "--q",
                    "sql=SELECT %s FROM %s WHERE %s" % (cols, self.parent.tablename, where),
                    "database=%s"                    % self.parent.database,
                    "driver=%s"                      % self.parent.driver]


        # run command
        vDbSelect = gcmd.Command(cmd=cmdv)

        # 
        # read data
        #
        # FIXME: Max. number of rows, while the GUI is still usable
        i = 0
        for line in vDbSelect.ReadStdOutput():
            attributes = line.strip().split("|")
            self.itemDataMap[i] = []

            # convert to appropriate type
            for j in range(len(attributes)):
                try:
                    attributes[j] = self.columns[j]["type"](attributes[j])
                except:
                    pass

            # insert to table
            index = self.InsertStringItem(index=sys.maxint, label=str(attributes[0]))
            self.itemDataMap[i].append(attributes[0])
            for j in range(len(attributes[1:])):
                self.SetStringItem(index=index, col=j+1, label=str(attributes[j+1]))
                self.itemDataMap[i].append(attributes[j+1])

            self.SetItemData(item=index, data=i)
            self.itemIndexMap.append(i)

            i += 1
            if i >= 32000:
                self.log.write(_("Can display only 32000 lines"))
                break

        for i in range(self.GetColumnCount()):
            self.SetColumnWidth(col=i, width=wx.LIST_AUTOSIZE)
            if self.GetColumnWidth(col=i) < 20:
                self.SetColumnWidth(col=i, width=wx.LIST_AUTOSIZE_USEHEADER)

    def OnCloseWindow(self, event):
        """Close attribute manager window"""
        if self.qlayer:
            self.map.DeleteLayer(self.qlayer)

    def OnItemSelected(self, event):
        """Item selected"""
        self.selectedCats.append(int(self.GetItemText(event.m_itemIndex)))
        self.selectedCats.sort()

        event.Skip()

    def OnItemDeselected(self, event):
        """Item deselected"""
        self.selectedCats.remove(int(self.GetItemText(event.m_itemIndex)))
        self.selectedCats.sort()

        event.Skip()


    def RedrawMap(self):
        """Redraw a map"""
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
                gcmd.append("icon=%s" % (self.icon))
            if self.pointsize:
                gcmd.append("size=%s" % (self.pointsize))

                self.qlayer = self.map.AddLayer(type="vector", name='qlayer', command=cmd,
                                                l_active=True, l_hidden=True, l_opacity=1, l_render=False)
            self.mapdisp.ReDraw(None)
            self.lastTurnSelectedCats = self.selectedCats[:]

    def OnItemActivated(self, event):
        """Item activated"""
        self.currentItem = event.m_itemIndex
        self.log.write("OnItemActivated: %s\nTopItem: %s\n" %
                       (self.GetItemText(self.currentItem), self.GetTopItem()))
        event.Skip()

    def GetColumnText(self, index, col):
        """Return column text"""
        item = self.GetItem(index, col)
        return item.GetText()

    def GetListCtrl(self):
        """Returt list"""
        return self

#     def OnGetItemText(self, item, col):
#         """Get item text"""
#         index=self.itemIndexMap[item]
#         s = self.itemDataMap[index][col]
#         return s
    
#     def OnGetItemImage(self, item):
#         """Get item image"""
#         return self.OnGetItemAttr(item)

#     def OnGetItemAttr(self, item):
#         """Get item attributes"""
#         index = self.itemIndexMap[item]
        
#         if ( index % 2) == 0:
#             return self.attr2
#         else:
#             return self.attr1

#     def SortItems(self, sorter=cmp):
#         """Sort items"""
#         items = list(self.itemDataMap.keys())
#         for i in range(len(items)):
#             items[i] =  self.columns[0]["type"](items[i]) # FIXME
#         items.sort(self.Sorter)
#         items.sort(sorter)
#         for i in range(len(items)):
#             items[i] =  str(items[i])
#         self.itemIndexMap = items
        
#         # redraw the list
#         self.Refresh()

#         # Used by the ColumnSorterMixin, see wx/lib/mixins/listctrl.py


#     def Sorter(self, key1, key2):
#         col = self._col
#         ascending = self._colSortFlag[col]
#         # convert, because the it is allways string
#         try:
#             item1 = self.columns[col]["type"](self.itemDataMap[key1][col])
#         except:
#             item1 = ''
#         try:
#             item2 = self.columns[col]["type"](self.itemDataMap[key2][col])
#         except:
#             item2 = ''
        
#         # Internationalization of string sorting with locale module
#         if type(item1) == type('') or type(item2) == type(''):
#             cmpVal = locale.strcoll(str(item1), str(item2))
#         else:
#             cmpVal = cmp(item1, item2)
        
#         # If the items are equal then pick something else to make the sort v    ->alue unique
#         if cmpVal == 0:
#             cmpVal = apply(cmp, self.GetSecondarySortValues(col, key1, key2))
            
#             if ascending:
#                 return cmpVal
#             else:
#                 return -cmpVal
        
#         return cmp(self.columns[0]["type"](item1), # FIXME
#                    self.columns[0]["type"](item2))
        
#     def GetSortImages(self):
#         return (self.sm_dn, self.sm_up)

#     def getColumnText(self, index, col):
#         """Get column heading"""
#         item = self.GetItem(index, col)
#         return item.GetText()

    def OnMapClick(self, event):
        """
        Gets coordinates from mouse clicking on display window
        """
        # map coordinates
        x, y = self.mapdisp.MapWindow.Pixel2Cell(event.GetPositionTuple()[:])

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
                # self.Select(idx,True)
                self.EnsureVisible( idx )
                self.SetItemState(idx, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)
            else:
                # self.SetItemState(idx, wx.LIST_STATE_DESELECTED, wx.LIST_STATE_DESELECTED)
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
    GRASS Attribute manager main window
    """
    def __init__(self, parent, id, title, vectmap, size = wx.DefaultSize, style = wx.DEFAULT_FRAME_STYLE,
                 pointdata=None):

        self.vectmap = vectmap
        self.parent  = parent
        self.gismgr  = parent

        # status bar log class
        log = Log(self)

        # get list of attribute tables (TODO: open more tables)
        vDbConnect = gcmd.Command (cmd=["v.db.connect",
                                        "-g",
                                        "map=%s" % self.vectmap])

        if vDbConnect.returncode == 0:
            (self.layer, self.tablename, self.column, self.database, self.driver) = \
                vDbConnect.ReadStdOutput()[0].split(' ') # TODO all layers
        else:
            self.layer = None

        if not self.layer:
            dlg = wx.MessageDialog(patent=parent,
                                   message=_("No attribute table linked to vector map <%s>") % vectmap,
                                   caption=_("Error"), style=wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return

        wx.Frame.__init__(self, parent, id, title, size=size, style=style)

        self.CreateStatusBar(number=1)

        # set up virtual list
        self.win = VirtualAttributeList(parent=self, log=log, vectmap=vectmap, pointdata=pointdata)

        #
        # buttons
        #
        self.btnApply      = wx.Button(parent=self, id=wx.ID_APPLY)
        self.btnQuit       = wx.Button(parent=self, id=wx.ID_CANCEL)
        # self.btn_unselect = wx.Button(self, -1, "Unselect")
        self.btnSqlBuilder = wx.Button(parent=self, id=wx.ID_ANY, label=_("SQL Builder"))

        # radiobuttons
        self.sqlSimple   = wx.RadioButton(parent=self, id=wx.ID_ANY,
                                          label=_("Simple"))
        self.sqlAdvanced = wx.RadioButton(parent=self, id=wx.ID_ANY,
                                          label=_("Advanced"))

        # textarea
        self.sqlWhere     = wx.TextCtrl(parent=self, id=wx.ID_ANY, value="",
                                        style=wx.TE_PROCESS_ENTER)
        self.sqlStatement = wx.TextCtrl(parent=self, id=wx.ID_ANY,
                                        value="SELECT * FROM %s" % self.tablename,
                                        style=wx.TE_PROCESS_ENTER)

        # labels
        self.sqlLabel    = wx.StaticText(parent=self, id=wx.ID_ANY,
                                         label="SELECT * FROM %s WHERE " % self.tablename)
        self.label_query = wx.StaticText(parent=self, id=wx.ID_ANY,
                                         label="")

        # boxes
        self.sqlBox = wx.StaticBox(parent=self, id=wx.ID_ANY,
                                   label=" %s " % _("SQL Query"))
        self.listBox = wx.StaticBox(parent=self, id=wx.ID_ANY,
                                    label=" %s " % _("Attribute data"))

        # collapsible areas
        self.infoCollapseLabelExp = _("Click here to show database connection information")
        self.infoCollapseLabelCol = _("Click here to hide database connection information")
        self.infoCollapse = wx.CollapsiblePane(parent=self,
                                               label=self.infoCollapseLabelExp,
                                               style=wx.CP_DEFAULT_STYLE | wx.CP_NO_TLW_RESIZE | wx.EXPAND)
        self.MakeInfoPaneContent(self.infoCollapse.GetPane())

        # bindings
        self.btnSqlBuilder.Bind(wx.EVT_BUTTON, self.OnBuilder)
        self.btnApply.Bind(wx.EVT_BUTTON, self.OnApplySqlStatement)
        self.btnQuit.Bind(wx.EVT_BUTTON, self.OnCloseWindow)
        self.Bind(wx.EVT_COLLAPSIBLEPANE_CHANGED, self.OnInfoPaneChanged, self.infoCollapse)
        self.sqlSimple.Bind(wx.EVT_RADIOBUTTON,   self.OnChangeSql)
        self.sqlAdvanced.Bind(wx.EVT_RADIOBUTTON, self.OnChangeSql)
        self.sqlWhere.Bind(wx.EVT_TEXT_ENTER,     self.OnApplySqlStatement)
        self.sqlStatement.Bind(wx.EVT_TEXT_ENTER, self.OnApplySqlStatement)

        # do layer
        self.__layout()

        self.OnChangeSql(None)

        self.SetMinSize((640, 480))

        self.Show()

    def OnChangeSql(self, event):
        """Switch simple/advanced sql statement"""
        if self.sqlSimple.GetValue():
            self.sqlWhere.Enable(True)
            self.sqlStatement.Enable(False)
            self.btnSqlBuilder.Enable(False)
        else:
            self.sqlWhere.Enable(False)
            self.sqlStatement.Enable(True)
            self.btnSqlBuilder.Enable(True)

    def OnApplySqlStatement(self, event):
        """Apply simple/advanced sql statement"""
        if self.sqlSimple.GetValue():
            # simple sql statement
            if len(self.sqlWhere.GetValue().strip()) > 0:
                self.win.LoadData(where=self.sqlWhere.GetValue().strip())
            else:
                self.win.LoadData()
        else:
            # advanced sql statement
            valid, cols, where = self.ValidateSelectStatement(self.sqlStatement.GetValue().strip())

            Debug.msg(4, "AttributeManager.OnApplySqlStatament(): valid=%s, cols=%s, where=%s" %
                      (valid, cols, where))

            if valid is True:
                self.win.LoadData(cols=cols, where=where)

    def ValidateSelectStatement(self, statement):
        """Validate Select SQL statement

        TODO: check list of columns and where statement

        Return True if valid, False if not
        Return list of columns (or '*' for all columns)
        Return where statement
        """
        
        if statement[0:7].lower() != 'select ':
            return (False, '', '')

        cols = ''
        index = 7
        for c in statement[index:]:
            if c == ' ':
                break
            cols += c
            index += 1

        tablelen = len(self.tablename)

        if statement[index+1:index+6].lower() != 'from ' or \
                statement[index+6:index+6+tablelen] != '%s' % (self.tablename):
            return (False, '', '')

        if len(statement[index+7+tablelen:]) > 0:
            where = statement[index+7+tablelen:]
        else:
            where = ''

        return (True, cols, where)

    def OnInfoPaneChanged(self, event):
        """Collapse database connection info box"""

        if self.infoCollapse.IsExpanded():
            self.infoCollapse.SetLabel(self.infoCollapseLabelCol)
        else:
            self.infoCollapse.SetLabel(self.infoCollapseLabelExp)

        # redo layout
        self.Layout()

    def MakeInfoPaneContent(self, pane):
        """Create database connection information content"""
            # connection info
        border = wx.BoxSizer(wx.VERTICAL)

        connectionInfoBox = wx.StaticBox(parent=pane, id=wx.ID_ANY,
                                         label=" %s " % _("Database connection"))
        infoSizer = wx.StaticBoxSizer(connectionInfoBox, wx.VERTICAL)
        infoFlexSizer = wx.FlexGridSizer (cols=2, hgap=1, vgap=1)
        infoFlexSizer.AddGrowableCol(1)
        
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="Database:"))
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="%s" % self.database))
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="Driver:"))
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="%s" % self.driver))
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="Table:"))
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="%s" % self.tablename))

        infoSizer.Add(item=infoFlexSizer,
                      proportion=1,
                      flag=wx.EXPAND | wx.ALL,
                      border=3)

        border.Add(item=infoSizer, 
                   proportion=1,
                   flag=wx.EXPAND | wx.ALL,
                   border=3)

        pane.SetSizer(border)

    def OnCloseWindow(self, event):
        """Cancel button pressed"""
        self.win.OnCloseWindow(event)
        self.Close()

    def OnBuilder(self,event):
        """SQL Builder button pressed"""
        self.builder = sqlbuilder.SQLFrame(parent=self, id=wx.ID_ANY,
                                           title=_("SQL Builder"),
                                           vectmap=self.vectmap)
    def OnTextEnter(self, event):
        pass

    def OnSQLBuilder(self, event):
        pass

    def __layout(self):
        """Do layout"""
        #self.panel = wx.Panel(self,-1, style=wx.SUNKEN_BORDER)

        #self.label_query.SetMinSize((500,50))
        self.sqlWhere.SetMinSize((250,-1))

        pageSizer = wx.BoxSizer(wx.VERTICAL)

        # attribute data
        listSizer = wx.StaticBoxSizer(self.listBox, wx.VERTICAL)
        listSizer.Add(item=self.win, proportion=1,
                      flag=wx.EXPAND | wx.ALL,
                      border=3)

        # sql query
        sqlSizer = wx.StaticBoxSizer(self.sqlBox, wx.VERTICAL)
        sqlFlexSizer = wx.FlexGridSizer (cols=3, hgap=5, vgap=5)
        sqlFlexSizer.AddGrowableCol(1)

        sqlFlexSizer.Add(item=self.sqlSimple,
                         flag=wx.ALIGN_CENTER_VERTICAL)
        sqlSimpleSizer = wx.BoxSizer(wx.HORIZONTAL)
        sqlSimpleSizer.Add(item=self.sqlLabel,
                           flag=wx.ALIGN_CENTER_VERTICAL)
        sqlSimpleSizer.Add(item=self.sqlWhere,
                           flag=wx.SHAPED | wx.GROW)
        sqlFlexSizer.Add(item=sqlSimpleSizer,
                         flag=wx.ALIGN_CENTER_VERTICAL)
        sqlFlexSizer.Add((0,0))
        sqlFlexSizer.Add(item=self.sqlAdvanced,
                         flag=wx.ALIGN_CENTER_VERTICAL)
        sqlFlexSizer.Add(item=self.sqlStatement,
                         flag=wx.EXPAND)
        sqlFlexSizer.Add(item=self.btnSqlBuilder,
                         flag=wx.ALIGN_RIGHT)

        sqlSizer.Add(item=sqlFlexSizer,
                     flag=wx.ALL | wx.EXPAND,
                     border=0)

        # buttons
        btnSizer = wx.StdDialogButtonSizer()
        btnSizer.AddButton(self.btnQuit)
        btnSizer.AddButton(self.btnApply)
        btnSizer.Realize()

        pageSizer.Add(item=self.infoCollapse,
                      flag=wx.ALL | wx.EXPAND, 
                      proportion=0,
                      border=3)
        pageSizer.Add(item=listSizer,
                      proportion=1,
                      flag=wx.LEFT | wx.RIGHT | wx.BOTTOM | wx.EXPAND,
                      border=3)
        pageSizer.Add(item=sqlSizer,
                      proportion=0,
                      flag=wx.LEFT | wx.RIGHT | wx.BOTTOM | wx.EXPAND,
                      border=3)
        pageSizer.Add(item=btnSizer,
                      proportion=0,
                      flag=wx.LEFT | wx.RIGHT | wx.BOTTOM | wx.EXPAND,
                      border=3)

        self.SetSizer(pageSizer)
        pageSizer.Fit(self)
        self.Layout()

class DisplayAttributesDialog(wx.Dialog):
    """
    Standard dialog used to add/update/display attributes linked
    to the vector map.

    Attribute data can be selected based on layer and category number
    or coordinates"""
    def __init__(self, parent, map,
                 layer=-1, cat=-1, # select by layer/cat
                 queryCoords=None, qdist=-1, # select by point
                 style=wx.DEFAULT_DIALOG_STYLE | wx.RESIZE_BORDER,
                 pos=wx.DefaultPosition,
                 action="add"):

        self.parent      = parent # mapdisplay.BufferedWindow
        self.map         = map
        self.layer       = layer
        self.action      = action
        self.cat         = cat
        self.queryCoords = queryCoords
        self.qdist       = qdist

        # id of selected line
        self.line = None

        # get layer/table/column information
        self.mapInfo = VectorAttributeInfo(self.map)

        layers = self.mapInfo.layers.keys() # get available layers

        # check if db connection / layer exists
        if (self.layer == -1 and len(layers) <= 0) or \
                (self.layer > 0 and self.layer not in layers):
            if self.layer == -1:
                label = _("Database connection for vector map <%s> " 
                          "is not defined in DB file.") % (self.map)
            else:
                label = _("Layer <%d> is not available for vector map <%s>.") % \
                    (self.layer, self.map)

            dlg = wx.MessageDialog(self.parent,
                                   _("No attribute table found.\n"
                                     "%s") % (label),
                                   _("Error"), wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            self.mapInfo = None
            return

        wx.Dialog.__init__(self, parent=self.parent, id=wx.ID_ANY,
                           title="", style=style, pos=pos)

        # dialog body
        mainSizer = wx.BoxSizer(wx.VERTICAL)

        if self.queryCoords: # select by position
            self.line, nselected = self.mapInfo.SelectByPoint(self.queryCoords,
                                                              self.qdist)

        # notebook
        self.notebook = wx.Notebook(parent=self, id=wx.ID_ANY, style=wx.BK_DEFAULT)

        self.closeDialog = wx.CheckBox(parent=self, id=wx.ID_ANY,
                                       label=_("Close dialog on submit"))
        self.closeDialog.SetValue(True)

        self.UpdateDialog(cat, queryCoords, qdist)

        # set title
        if self.action == "update":
            self.SetTitle(_("Update attributes"))
        elif self.action == "add":
            self.SetTitle(_("Add attributes"))
        else:
            self.SetTitle(_("Display attributes"))

        # buttons
        btnCancel = wx.Button(self, wx.ID_CANCEL)
        btnReset  = wx.Button(self, wx.ID_UNDO, _("&Reload"))
        btnSubmit = wx.Button(self, wx.ID_OK, _("&Submit"))

        btnSizer = wx.StdDialogButtonSizer()
        btnSizer.AddButton(btnCancel)
        btnSizer.AddButton(btnReset)
        btnSizer.SetNegativeButton(btnReset)
        btnSubmit.SetDefault()
        btnSizer.AddButton(btnSubmit)
        btnSizer.Realize()

        mainSizer.Add(item=self.notebook, proportion=1, flag=wx.EXPAND | wx.ALL, border=5)
        mainSizer.Add(item=self.closeDialog, proportion=0, flag=wx.EXPAND | wx.LEFT | wx.RIGHT,
                      border=5)
        mainSizer.Add(item=btnSizer, proportion=0,
                      flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)

        # bindigs
        btnReset.Bind(wx.EVT_BUTTON, self.OnReset)
        btnSubmit.Bind(wx.EVT_BUTTON, self.OnSubmit)
        btnCancel.Bind(wx.EVT_BUTTON, self.OnCancel)

        self.SetSizer(mainSizer)
        mainSizer.Fit(self)

        # set min size for dialog
        self.SetMinSize(self.GetBestSize())

        if self.notebook.GetPageCount() == 0:
            Debug.msg(2, "DisplayAttributesDialog(): Nothing found!")
            self.mapInfo = None

    def __SelectAttributes(self, layer):
        """Select attributes"""
        pass

    def OnSQLStatement(self, event):
        """Update SQL statement"""
        pass

    def GetSQLString(self, updateValues=False):
        """Create SQL statement string based on self.sqlStatement

        If updateValues is True, update dataFrame according to values
        in textfields.
        """
        sqlCommands = []
        # find updated values for each layer/category
        for layer in self.mapInfo.layers.keys(): # for each layer
            table = self.mapInfo.layers[layer]["table"]
            columns = self.mapInfo.tables[table]
            for idx in range(len(columns["cat"]['values'])): # for each category
                updatedColumns = []
                updatedValues = []
                for name in columns.keys():
                    if name == "cat":
                        cat = columns[name]['values'][idx]
                        continue
                    type  = columns[name]['type']
                    value = columns[name]['values'][idx]
                    id    = columns[name]['ids'][idx]
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
                        if updateValues:
                            columns[name]['values'][idx] = newvalue

                if self.action != "add" and len(updatedValues) == 0:
                    continue

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
                sqlCommands.append(sqlString)
            # for each category
        # for each layer END

        Debug.msg(3, "DisplayAttributesDialog.GetSQLString(): %s" % sqlCommands)

        return sqlCommands

    def OnReset(self, event):
        """Reset form"""
        for layer in self.mapInfo.layers.keys():
            table = self.mapInfo.layers[layer]["table"]
            columns = self.mapInfo.tables[table]
            for idx in range(len(columns["cat"]['values'])):
                for name in columns.keys():
                    type  = columns[name]['type']
                    value = columns[name]['values'][idx]
                    id    = columns[name]['ids'][idx]
                    if name.lower() != "cat":
                        self.FindWindowById(id).SetValue(value)

    def OnCancel(self, event):
        """Cancel button pressed"""
        self.parent.parent.digittoolbar.attributesDialog = None
        self.parent.parent.digit.driver.SetSelected([])
        self.parent.UpdateMap(render=False)

        self.Close()

    def OnSubmit(self, event):
        """Submit records"""
        sqlCommands = self.GetSQLString(updateValues=True)
        if len(sqlCommands) > 0:
            sqlfile = tempfile.NamedTemporaryFile(mode="w")
            for sql in sqlCommands:
                sqlfile.file.write(sql + ";\n")
                sqlfile.file.flush()
                gcmd.Command(cmd=["db.execute",
                                  "--q",
                                  "input=%s" % sqlfile.name])

        if self.closeDialog.IsChecked():
            self.OnCancel(event)

    def GetLine(self):
        """Get id of selected line or 'None' if no line is selected"""
        return self.line

    def UpdateDialog(self, cat=-1, queryCoords=None, qdist=-1):
        """Update dialog
        
        Return True if updated otherwise False
        """
        self.cat         = cat
        self.queryCoords = queryCoords
        self.qdist       = qdist

        if not self.mapInfo:
            return False

        self.mapInfo.Reset()

        layers = self.mapInfo.layers.keys() # get available layers

        # id of selected line
        if self.queryCoords: # select by position
            self.line, nselected = self.mapInfo.SelectByPoint(queryCoords,
                                                              qdist)
        # reset notebook
        self.notebook.DeleteAllPages()

        for layer in layers: # for each layer
            if self.layer > 0 and \
                    self.layer != layer:
                continue

            if not self.queryCoords: # select using layer/cat
                nselected = self.mapInfo.SelectFromTable(layer, self.cat)

            if nselected <= 0 and self.action != "add":
                continue # nothing selected ...

            if self.action == "add":
                if nselected <= 0:
                    table = self.mapInfo.layers[layer]["table"]
                    columns = self.mapInfo.tables[table]
                    for name in columns.keys():
                        if name == "cat":
                            self.mapInfo.tables[table][name]['values'].append(self.cat)
                        else:
                            self.mapInfo.tables[table][name]['values'].append('')
                else: # change status 'add' -> 'update'
                    self.action = "update"

            panel = wx.Panel(parent=self.notebook, id=wx.ID_ANY)
            self.notebook.AddPage(page=panel, text=_(" %s %d ") % (_("Layer"), layer))

            # notebook body
            border = wx.BoxSizer(wx.VERTICAL)

            table   = self.mapInfo.layers[layer]["table"]
            columns = self.mapInfo.tables[table]

            # value
            if len(columns["cat"]['values']) == 0: # no cats
                sizer  = wx.BoxSizer(wx.VERTICAL)
                txt = wx.StaticText(parent=panel, id=wx.ID_ANY,
                                    label=_("No categories available."))
                sizer.Add(txt, proportion=1,
                          flag=wx.ALIGN_CENTER_VERTICAL | wx.ALIGN_CENTER | wx.EXPAND)
                border.Add(item=sizer, proportion=1,
                           flag=wx.ALL | wx.EXPAND | wx.ALIGN_CENTER_VERTICAL | wx.ALIGN_CENTER,
                           border=10)
                panel.SetSizer(border)
                continue
            for idx in range(len(columns["cat"]['values'])):
                flexSizer = wx.FlexGridSizer (cols=4, hgap=3, vgap=3)
                flexSizer.AddGrowableCol(3)
                # columns
                for name in columns.keys():
                    type  = columns[name]['type']
                    value = columns[name]['values'][idx]
                    if name.lower() == "cat":
                        box    = wx.StaticBox (parent=panel, id=wx.ID_ANY,
                                               label=" %s %s " % (_("Category"), value))
                        boxFont = self.GetFont()
                        boxFont.SetWeight(wx.FONTWEIGHT_BOLD)
                        box.SetFont(boxFont)
                        sizer  = wx.StaticBoxSizer(box, wx.VERTICAL)
                        colValue = box
                    else:
                        colName = wx.StaticText(parent=panel, id=wx.ID_ANY,
                                                label=name)
                        colType = wx.StaticText(parent=panel, id=wx.ID_ANY,
                                                label="[" + type.lower() + "]")
                        delimiter = wx.StaticText(parent=panel, id=wx.ID_ANY, label=":")

                        colValue = wx.TextCtrl(parent=panel, id=wx.ID_ANY, value=value,
                                               size=(-1, -1)) # TODO: validator
                        colValue.SetName(name)
                        self.Bind(wx.EVT_TEXT, self.OnSQLStatement, colValue)

                        flexSizer.Add(colName, proportion=0,
                                      flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
                        flexSizer.Add(colType, proportion=0,
                                      flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
                        flexSizer.Add(delimiter, proportion=0,
                                      flag=wx.FIXED_MINSIZE | wx.ALIGN_CENTER_VERTICAL)
                        flexSizer.Add(colValue, proportion=1,
                                      flag=wx.EXPAND | wx.ALIGN_CENTER_VERTICAL)
                    # add widget reference to self.columns
                    columns[name]['ids'].append(colValue.GetId()) # name, type, values, id

                # for each attribute (including category) END
                sizer.Add(item=flexSizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=1)
                border.Add(item=sizer, proportion=1, flag=wx.ALL | wx.EXPAND, border=5)
            # for each category END

            panel.SetSizer(border)
        # for each layer END

        self.Layout()

        return True

class VectorAttributeInfo:
    """Class providing information about attribute tables
    linked to the vector map"""
    def __init__(self, map):
        self.map = map
        # {layer number : {table, database, driver})
        self.layers = {}
        # {table : {column name : type, values, ids}}
        self.tables = {}

        if not self.__CheckDBConnection(): # -> self.layers
            return

        self.__DescribeTables() # -> self.tables

    def __CheckDBConnection(self):
        """Check DB connection"""
        layerCommand = gcmd.Command(cmd=["v.db.connect",
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
            columnsCommand = gcmd.Command (cmd=["v.db.connect", "-c", "--q",
                                               "map=%s" % self.map,
                                               "layer=%d" % layer])
            table = self.layers[layer]["table"]

            columns = {} # {name: {type, [values], [ids]}}

            if columnsCommand.returncode == 0:
                for line in columnsCommand.ReadStdOutput():
                    columnType, columnName = line.split('|')
                    columnType = columnType.lower().strip()
                    columns[columnName] = { 'type'   : columnType,
                                            'values' : [],
                                            'ids'    : []}
            else:
                pass

            self.tables[table] = columns

    def SelectByPoint(self, queryCoords, qdist):
        """Get attributes by coordinates (all available layers)

        Return line id or None if no line is found"""
        line = None
        nselected = 0
        cmdWhat = gcmd.Command(cmd=['v.what',
                                   '-a', '--q',
                                   'map=%s' % self.map,
                                   'east_north=%f,%f' % \
                                       (float(queryCoords[0]), float(queryCoords[1])),
                                   'distance=%f' % qdist])

        if cmdWhat.returncode == 0:
            read = False
            for item in cmdWhat.ReadStdOutput():
                litem = item.lower()
                if read:
                    name, value = item.split(':')
                    name = name.strip()
                    # append value to the column
                    try:
                        self.tables[table][name]['values'].append(value.strip())
                    except:
                        read = False
                            
                if "line:" in litem: # get line id
                    line = int(item.split(':')[1].strip())
                elif "key column:" in litem: # start reading attributes
                    read = True
                    nselected = nselected + 1
                elif "layer:" in litem: # get layer id
                    layer = int(item.split(':')[1].strip())
                    table = self.layers[layer]["table"] # get table desc
                    read = False

        return (line, nselected)

    def SelectFromTable(self, layer, cat):
        """Select records from the table

        Return number of selected records, -1 on error
        """
        if layer <= 0:
            return -1

        nselected = 0

        table = self.layers[layer]["table"] # get table desc
        # select values (only one record)
        selectCommand = gcmd.Command(cmd=["v.db.select", "-v", "--q",
                                         "map=%s" % self.map,
                                         "layer=%d" % layer,
                                         "where=cat=%d" % cat])

        #self.tables[table]["cat"][1] = str(cat)
        if selectCommand.returncode == 0:
            for line in selectCommand.ReadStdOutput():
                name, value = line.split('|')
                self.tables[table][name]['values'].append(value)
                nselected = 1

        return nselected

    def Reset(self):
        """Reset"""
        for layer in self.layers:
            table = self.layers[layer]["table"] # get table desc
            columns = self.tables[table]
            for name in self.tables[table].keys():
                self.tables[table][name]['values'] = [] 
                self.tables[table][name]['ids']    = [] 

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
    f = AttributeManager(parent=None, id=wx.ID_ANY,
                         title=_("GRASS Attribute Table Manager: vector map <%s>") % argv[1],
                         size=(700,600), vectmap=argv[1])
    app.MainLoop()

if __name__ == '__main__':
    main()
