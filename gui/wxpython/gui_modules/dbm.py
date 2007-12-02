"""
MODULE:    dbm.py

CLASSES:
    * Log
    * VirtualAttributeList
    * AttributeManager
    * DisplayAttributesDialog
    * VectorDBInfo
    * ModifyTableRecord

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
import tempfile

import wx
import wx.lib.mixins.listctrl as listmix
import wx.lib.flatnotebook as FN
import wx.lib.colourselect as csel

import sqlbuilder
import grassenv
import gcmd
import globalvar
import utils
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


class VirtualAttributeList(wx.ListCtrl,
                           listmix.ListCtrlAutoWidthMixin,
                           listmix.ColumnSorterMixin,
                           listmix.TextEditMixin):
    """
    Support virtual list class
    """
    def __init__(self, parent, log, mapDBInfo, layer):
        wx.ListCtrl.__init__(self, parent=parent, id=wx.ID_ANY,
                             style=wx.LC_REPORT | wx.LC_HRULES |
                             wx.LC_VRULES | wx.LC_VIRTUAL)

        #
        # initialize variables
        #
        self.log     = log
        self.mapDBInfo = mapDBInfo
        self.layer   = layer

        self.columns              = {} # <- LoadData()
        self.itemCatsMap          = {} # <- LoadData() (first call)

        self.selectedCats         = []
        self.lastTurnSelectedCats = [] # just temporary, for comparation

        #
        # add some attributes (colourful background for each item rows)
        #
        self.attr1 = wx.ListItemAttr()
        # self.attr1.SetBackgroundColour("light blue")
        self.attr1.SetBackgroundColour(wx.Colour(238,238,238))
        self.attr2 = wx.ListItemAttr()
        self.attr2.SetBackgroundColour("white")
        self.il = wx.ImageList(16, 16)
        self.sm_up = self.il.Add(wx.ArtProvider_GetBitmap(wx.ART_GO_UP,   wx.ART_TOOLBAR,
                                                          (16,16)))
        self.sm_dn = self.il.Add(wx.ArtProvider_GetBitmap(wx.ART_GO_DOWN, wx.ART_TOOLBAR,
                                                          (16,16)))
        self.SetImageList(self.il, wx.IMAGE_LIST_SMALL)

        self.LoadData(layer)
        
        # setup mixins
        listmix.ListCtrlAutoWidthMixin.__init__(self)
        listmix.ColumnSorterMixin.__init__(self, len(self.columns))
        listmix.TextEditMixin.__init__(self)

        # sort by cat by default
        self.SortListItems(col=0, ascending=1) # FIXME category column can be different

        # events
        self.Bind(wx.EVT_LIST_ITEM_SELECTED,   self.OnItemSelected)
        self.Bind(wx.EVT_LIST_ITEM_DESELECTED, self.OnItemDeselected)
        self.Bind(wx.EVT_LIST_COL_CLICK,       self.OnColumnClick)     # sorting
        # self.Bind(wx.EVT_LIST_DELETE_ITEM, self.OnItemDelete, self.list)
        # self.Bind(wx.EVT_LIST_COL_RIGHT_CLICK, self.OnColRightClick, self.list)
        # self.Bind(wx.EVT_LIST_COL_BEGIN_DRAG, self.OnColBeginDrag, self.list)
        # self.Bind(wx.EVT_LIST_COL_DRAGGING, self.OnColDragging, self.list)
        # self.Bind(wx.EVT_LIST_COL_END_DRAG, self.OnColEndDrag, self.list)
        # self.Bind(wx.EVT_LIST_BEGIN_LABEL_EDIT, self.OnBeginEdit, self.list)
        # self.list.Bind(wx.EVT_LEFT_DCLICK, self.OnDoubleClick)
        # self.list.Bind(wx.EVT_RIGHT_DOWN, self.OnRightDown)

    def Update(self, mapDBInfo):
        """Update list according new mapDBInfo description"""
        self.mapDBInfo = mapDBInfo
        self.LoadData(self.layer)

    def LoadData(self, layer, cols='*', where=''):
        """Load data into list"""

        # These two should probably be passed to init more cleanly
        # setting the numbers of items = number of elements in the dictionary
        self.itemDataMap  = {}
        self.itemIndexMap = []

        self.DeleteAllItems()

        # self.ClearAll()
        for i in range(self.GetColumnCount()):
            self.DeleteColumn(0)

        tableName    = self.mapDBInfo.layers[layer]['table']
        keyColumn    = self.mapDBInfo.layers[layer]['key']
        self.columns = self.mapDBInfo.tables[tableName]

        if cols != '*':
            columnNames = cols.split(',')
        else:
            columnNames = self.mapDBInfo.GetColumns(tableName)

        if len(self.itemCatsMap) > 0:
            keyId = -1
        else:
            keyId = columnNames.index(keyColumn)

        i = 0
        for column in columnNames:
            self.InsertColumn(col=i, heading=column)
            i += 1

            if i >= 256:
                self.log.write(_("Can display only 256 columns"))

        ### self.mapDBInfo.SelectFromTable(layer, cols, where) # <- values (FIXME)
        #
        # read data
        #
        # FIXME: Max. number of rows, while the GUI is still usable
        if where is None or where is '':
            sql="SELECT %s FROM %s" % (cols, tableName)
        else:
            sql="SELECT %s FROM %s WHERE %s" % (cols, tableName, where)

        # stdout can be very large, do not use PIPE, redirect to temp file
        # TODO: more effective way should be implemented...
        outFile = tempfile.NamedTemporaryFile(mode='w+b')
        selectCommand = gcmd.Command(["db.select", "-c", "--q",
                                      "sql=%s" % sql,
                                      "database=%s" % self.mapDBInfo.layers[layer]["database"],
                                      "driver=%s"   % self.mapDBInfo.layers[layer]["driver"],
                                      "fs=|"],
                                     stdout=outFile)
        i = 0
        outFile.seek(0)
        while True:
            record = outFile.readline().replace(os.linesep, '')
            if not record:
                break
            self.itemDataMap[i] = []
            j = 0
            for value in record.split('|'):
                # casting ...
                try:
                    self.itemDataMap[i].append(self.columns[columnNames[j]]['ctype'] (value))
                except:
                    self.itemDataMap[i].append('')
                
                if keyId > -1 and keyId == j:
                    cat = self.columns[columnNames[j]]['ctype'] (value)
                j += 1

            # insert to table
            # index = self.InsertStringItem(index=sys.maxint, label=str(self.itemDataMap[i][0]))
            # for j in range(len(self.itemDataMap[i][1:])):
            # self.SetStringItem(index=index, col=j+1, label=str(self.itemDataMap[i][j+1]))
                
            # self.SetItemData(item=index, data=i)

            self.itemIndexMap.append(i)
            if keyId > -1: # load cats only when LoadData() is called first time
                self.itemCatsMap[i] = cat

            i += 1
            if i >= 100000:
                print >> sys.stderr, _("Limit 100000 records")
                break

        self.SetItemCount(i)

        i = 0
        for col in columnNames:
            width = self.columns[col]['length'] * 6 # FIXME
            if width < 60:
                width = 60
            self.SetColumnWidth(col=i, width=width)
            i += 1

    def OnItemSelected(self, event):
        """Item selected. Add item to selected cats..."""
        cat = int(self.GetItemText(event.m_itemIndex))
        if cat not in self.selectedCats:
            self.selectedCats.append(cat)
            self.selectedCats.sort()

        event.Skip()

    def OnItemDeselected(self, event):
        """Item deselected. Remove item from selected cats..."""
        cat = int(self.GetItemText(event.m_itemIndex))
        if cat in self.selectedCats:
            self.selectedCats.remove(cat)
            self.selectedCats.sort()

        event.Skip()

    def GetSelectedItems(self):
        """Return list of selected items (category numbers)"""
        cats = []
        item = self.GetFirstSelected()
        while item != -1:
            cats.append(self.GetItemText(item))
            item = self.GetNextSelected(item)

        return cats

    def GetColumnText(self, index, col):
        """Return column text"""
        item = self.GetItem(index, col)
        return item.GetText()

    def GetListCtrl(self):
        """Returt list"""
        return self

    def OnGetItemText(self, item, col):
        """Get item text"""
        index = self.itemIndexMap[item]
        s = self.itemDataMap[index][col]
        return s

    def OnGetItemAttr(self, item):
        """Get item attributes"""
        index = self.itemIndexMap[item]
        if ( index % 2) == 0:
            return self.attr2
        else:
            return self.attr1

    def OnColumnClick(self, event):
        """Column heading clicked -> sorting"""
        self._col = event.GetColumn()
        event.Skip()

    def SortItems(self, sorter=cmp):
        """Sort items"""
        items = list(self.itemDataMap.keys())
        #         for i in range(len(items)):
        #             items[i] =  self.columns[0]["type"](items[i]) # FIXME
        items.sort(self.Sorter)
        #         for i in range(len(items)):
        #             items[i] =  str(items[i])
        self.itemIndexMap = items

        # redraw the list
        self.Refresh()

    def Sorter(self, key1, key2):
        col = self._col
        ascending = self._colSortFlag[col]
        # convert always string
        try:
            item1 = self.columns[col]["type"](self.itemDataMap[key1][col])
        except:
            item1 = ''
        try:
            item2 = self.columns[col]["type"](self.itemDataMap[key2][col])
        except:
            item2 = ''

        if type(item1) == type('') or type(item2) == type(''):
            cmpVal = locale.strcoll(str(item1), str(item2))
        else:
            cmpVal = cmp(item1, item2)


        # If the items are equal then pick something else to make the sort v    ->alue unique
        if cmpVal == 0:
            cmpVal = apply(cmp, self.GetSecondarySortValues(col, key1, key2))

        if ascending:
            return cmpVal
        else:
            return -cmpVal

    def GetSortImages(self):
        """Used by the ColumnSorterMixin, see wx/lib/mixins/listctrl.py"""
        return (self.sm_dn, self.sm_up)

    def getColumnText(self, index, col):
        """Get column/item"""
        item = self.GetItem(index, col)
        return item.GetText()

    def SetVirtualData(self, row, col, data):
        pass

class AttributeManager(wx.Frame):
    """
    GRASS Attribute manager main window
    """
    def __init__(self, parent, id, title, vectmap,
                 size = wx.DefaultSize, style = wx.DEFAULT_FRAME_STYLE,
                 pointdata=None):

        self.vectmap   = vectmap
        self.pointdata = pointdata
        self.parent    = parent # GMFrame

        wx.Frame.__init__(self, parent, id, title, size=(900,600), style=style)

        try:
            self.map        = self.parent.curr_page.maptree.Map
            self.mapdisplay = self.parent.curr_page.maptree.mapdisplay
        except:
            self.map = self.mapdisplay = None

        if pointdata:
            self.icon      = pointdata[0]
            self.pointsize = pointdata[1]
        else:
            self.icon      = None
            self.pointsize = None


        # status bar log class
        self.log = Log(self) # -> statusbar

        # query map layer (if parent (GMFrame) is given)
        self.qlayer = None

        # -> layers / tables description
        self.mapDBInfo = VectorDBInfo(self.vectmap)

        if len(self.mapDBInfo.layers.keys()) == 0:
            dlg = wx.MessageDialog(patent=self.parent,
                                   message=_("No attribute table linked to "
                                             "vector map <%s> found.") % \
                                       self.vectmap,
                                   caption=_("Error"), style=wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return

        #
        # default settings (TODO global settings file)
        #
        self.settings = {}
        self.settings['highlight'] = {}
        self.settings['highlight']['color'] = (255, 255, 0, 255) # yellow
        self.settings['highlight']['width'] = 2

        #
        # list of command/SQL statements to be performed
        #
        self.listOfCommands      = []
        self.listOfSQLStatements = []

        self.CreateStatusBar(number=1)

        # set up virtual lists (each layer)
        ### {layer: list, widgets...}
        self.layerPage = {}

        # auinotebook (browse, manage, settings)
        #self.notebook = wx.aui.AuiNotebook(parent=self, id=wx.ID_ANY,
        #                                   style=wx.aui.AUI_NB_BOTTOM)
        # really needed (ML)
        # self.notebook.SetFont(wx.Font(10, wx.FONTFAMILY_MODERN, wx.NORMAL, wx.NORMAL, 0, ''))

        dbmStyle = FN.FNB_VC8 | \
            FN.FNB_BACKGROUND_GRADIENT | \
            FN.FNB_NODRAG | \
            FN.FNB_TABS_BORDER_SIMPLE
        
        self.notebook = FN.FlatNotebook(parent=self, id=wx.ID_ANY,
                                                 style=FN.FNB_BOTTOM |
                                                 FN.FNB_NO_NAV_BUTTONS | FN.FNB_FANCY_TABS)
        self.browsePage = FN.FlatNotebook(self, id=wx.ID_ANY,
                                          style=dbmStyle)
        self.browsePage.SetTabAreaColour(wx.Colour(125,200,175))
        #self.notebook.AddPage(self.browsePage, caption=_("Browse data"))
        self.notebook.AddPage(self.browsePage, text=_("Browse data")) # FN
        self.browsePage.SetTabAreaColour(wx.Colour(125,200,175))

        self.manageTablePage = FN.FlatNotebook(self, id=wx.ID_ANY,
                                          style=dbmStyle)
        self.manageTablePage.SetTabAreaColour(wx.Colour(125,200,175))
        #self.notebook.AddPage(self.manageTablePage, caption=_("Manage tables"))
        self.notebook.AddPage(self.manageTablePage, text=_("Manage tables")) # FN
        self.manageTablePage.SetTabAreaColour(wx.Colour(125,200,175))

        self.manageLayerPage = FN.FlatNotebook(self, id=wx.ID_ANY,
                                               style=dbmStyle)
        self.manageLayerPage.SetTabAreaColour(wx.Colour(125,200,175))
        #self.notebook.AddPage(self.manageLayerPage, caption=_("Manage layers"))
        self.notebook.AddPage(self.manageLayerPage, text=_("Manage tables")) # FN
        self.manageLayerPage.SetTabAreaColour(wx.Colour(125,200,175))

        self.settingsPage = FN.FlatNotebook(self, id=wx.ID_ANY,
                                            style=dbmStyle)
        self.settingsPage.SetTabAreaColour(wx.Colour(125,200,175))
        #self.notebook.AddPage(self.settingsPage, caption=_("Settings"))
        self.notebook.AddPage(self.settingsPage, text=_("Settings")) # FN
        self.settingsPage.SetTabAreaColour(wx.Colour(125,200,175))

        self.infoCollapseLabelExp = _("Click here to show database connection information")
        self.infoCollapseLabelCol = _("Click here to hide database connection information")

        self.__createBrowsePage()
        self.__createManageTablePage()
        self.__createManageLayerPage()
        self.__createSettingsPage()

        self.notebook.SetSelection(0) # select browse tab

        #
        # buttons
        #
        self.btnApply      = wx.Button(parent=self, id=wx.ID_APPLY)
        self.btnQuit       = wx.Button(parent=self, id=wx.ID_CANCEL)
        # self.btn_unselect = wx.Button(self, -1, "Unselect")

        # events
        self.btnApply.Bind(wx.EVT_BUTTON,           self.OnApply)
        self.btnQuit.Bind(wx.EVT_BUTTON,            self.OnCloseWindow)
        self.Bind(FN.EVT_FLATNOTEBOOK_PAGE_CHANGED, self.OnLayerPageChanged, self.browsePage)
        self.Bind(FN.EVT_FLATNOTEBOOK_PAGE_CHANGED, self.OnLayerPageChanged, self.manageTablePage)

        # do layout
        self.__layout()

    def __del__(self):
        pass
        #         if self.qlayer and self.map:
        #             self.map.DeleteLayer(self.qlayer)
        #             self.mapdisplay.ReRender(None)

    def __createBrowsePage(self, onlyLayer=-1):
        """Create browse tab page"""
        for layer in self.mapDBInfo.layers.keys():
            if onlyLayer > 0 and layer != onlyLayer:
                continue

            panel = wx.Panel(parent=self.browsePage, id=wx.ID_ANY)
            self.layerPage[layer] = {'browsePage': panel.GetId()}

            self.browsePage.AddPage(page=panel, text=_(" %s %d ") % (_("Layer"), layer))

            pageSizer = wx.BoxSizer(wx.VERTICAL)

            # attribute data
            listBox = wx.StaticBox(parent=panel, id=wx.ID_ANY,
                                   label=" %s " % _("Attribute data"))
            listSizer = wx.StaticBoxSizer(listBox, wx.VERTICAL)

            sqlBox = wx.StaticBox(parent=panel, id=wx.ID_ANY,
                                  label=" %s " % _("SQL Query"))

            sqlSizer = wx.StaticBoxSizer(sqlBox, wx.VERTICAL)

            win = VirtualAttributeList(panel, self.log,
                                       self.mapDBInfo, layer)
            win.Bind(wx.EVT_LIST_ITEM_ACTIVATED, self.OnDataItemActivated)
            win.Bind(wx.EVT_COMMAND_RIGHT_CLICK, self.OnDataRightUp) #wxMSW
            win.Bind(wx.EVT_RIGHT_UP,            self.OnDataRightUp) #wxGTK

            listSizer.Add(item=win, proportion=1,
                          flag=wx.EXPAND | wx.ALL,
                          border=3)

            # sql statement box
            btnSqlBuilder = wx.Button(parent=panel, id=wx.ID_ANY, label=_("SQL Builder"))
            btnSqlBuilder.Bind(wx.EVT_BUTTON, self.OnBuilder)

            sqlSimple = wx.RadioButton(parent=panel, id=wx.ID_ANY,
                                       label=_("Simple"))
            sqlSimple.SetValue(True)
            sqlAdvanced = wx.RadioButton(parent=panel, id=wx.ID_ANY,
                                         label=_("Advanced"))
            sqlSimple.Bind(wx.EVT_RADIOBUTTON,   self.OnChangeSql)
            sqlAdvanced.Bind(wx.EVT_RADIOBUTTON, self.OnChangeSql)

            sqlWhere = wx.TextCtrl(parent=panel, id=wx.ID_ANY, value="",
                                   style=wx.TE_PROCESS_ENTER,
                                   size=(200, -1))
            sqlStatement = wx.TextCtrl(parent=panel, id=wx.ID_ANY,
                                       value="SELECT * FROM %s" % \
                                           self.mapDBInfo.layers[layer]['table'],
                                       style=wx.TE_PROCESS_ENTER)
            sqlWhere.Bind(wx.EVT_TEXT_ENTER,     self.OnApplySqlStatement)
            sqlStatement.Bind(wx.EVT_TEXT_ENTER, self.OnApplySqlStatement)

            sqlLabel = wx.StaticText(parent=panel, id=wx.ID_ANY,
                                     label="SELECT * FROM %s WHERE " % \
                                         self.mapDBInfo.layers[layer]['table'])
            label_query = wx.StaticText(parent=panel, id=wx.ID_ANY,
                                        label="")

            sqlFlexSizer = wx.FlexGridSizer (cols=3, hgap=5, vgap=5)
            sqlFlexSizer.AddGrowableCol(1)

            sqlFlexSizer.Add(item=sqlSimple,
                             flag=wx.ALIGN_CENTER_VERTICAL)
            sqlSimpleSizer = wx.BoxSizer(wx.HORIZONTAL)
            sqlSimpleSizer.Add(item=sqlLabel,
                               flag=wx.ALIGN_CENTER_VERTICAL)
            sqlSimpleSizer.Add(item=sqlWhere,
                               flag=wx.SHAPED | wx.GROW)
            sqlFlexSizer.Add(item=sqlSimpleSizer,
                             flag=wx.ALIGN_CENTER_VERTICAL)
            sqlFlexSizer.Add((0,0))
            sqlFlexSizer.Add(item=sqlAdvanced,
                             flag=wx.ALIGN_CENTER_VERTICAL)
            sqlFlexSizer.Add(item=sqlStatement,
                             flag=wx.EXPAND)
            sqlFlexSizer.Add(item=btnSqlBuilder,
                             flag=wx.ALIGN_RIGHT)

            sqlSizer.Add(item=sqlFlexSizer,
                         flag=wx.ALL | wx.EXPAND,
                         border=0)

            pageSizer.Add(item=listSizer,
                          proportion=1,
                          flag=wx.ALL | wx.EXPAND,
                          border=3)

            pageSizer.Add(item=sqlSizer,
                          proportion=0,
                          flag=wx.BOTTOM | wx.LEFT | wx.RIGHT | wx.EXPAND,
                          border=3)

            panel.SetSizer(pageSizer)

            self.layerPage[layer]['data']      = win.GetId()
            self.layerPage[layer]['simple']    = sqlSimple.GetId()
            self.layerPage[layer]['advanced']  = sqlAdvanced.GetId()
            self.layerPage[layer]['where']     = sqlWhere.GetId()
            self.layerPage[layer]['builder']   = btnSqlBuilder.GetId()
            self.layerPage[layer]['statement'] = sqlStatement.GetId()


        self.browsePage.SetSelection(0) # select first layer
        self.layer = self.mapDBInfo.layers.keys()[0]
        self.OnChangeSql(None)
        self.log.write(_("Number of loaded records: %d") % \
                           self.FindWindowById(self.layerPage[self.layer]['data']).GetItemCount())

    def __createManageTablePage(self, onlyLayer=-1):
        """Create manage page (create/link and alter tables)"""
        for layer in self.mapDBInfo.layers.keys():
            if onlyLayer > 0 and layer != onlyLayer:
                continue

            panel = wx.Panel(parent=self.manageTablePage, id=wx.ID_ANY)
            self.layerPage[layer]['tablePage'] = panel.GetId()
            self.manageTablePage.AddPage(page=panel, text=_(" %s %d ") % (_("Layer"), layer))

            pageSizer = wx.BoxSizer(wx.VERTICAL)

            # dbInfo
            infoCollapse = wx.CollapsiblePane(parent=panel,
                                              label=self.infoCollapseLabelExp,
                                              style=wx.CP_DEFAULT_STYLE |
                                              wx.CP_NO_TLW_RESIZE | wx.EXPAND)
            self.MakeInfoPaneContent(layer, infoCollapse.GetPane())
            infoCollapse.Collapse(False)
            self.Bind(wx.EVT_COLLAPSIBLEPANE_CHANGED, self.OnInfoPaneChanged, infoCollapse)

            # table description
            table = self.mapDBInfo.layers[layer]['table']
            tableBox = wx.StaticBox(parent=panel, id=wx.ID_ANY,
                                    label=" %s " % _("Table %s") % table)

            tableSizer = wx.StaticBoxSizer(tableBox, wx.VERTICAL)

            list = self.__createTableDesc(panel, table)
            list.Bind(wx.EVT_COMMAND_RIGHT_CLICK, self.OnTableRightUp) #wxMSW
            list.Bind(wx.EVT_RIGHT_UP,            self.OnTableRightUp) #wxGTK
            self.layerPage[layer]['tableData'] = list.GetId()

            addSizer = wx.FlexGridSizer (cols=5, hgap=5, vgap=5)
            addSizer.AddGrowableCol(3)

            # add column
            label  = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Column name"))
            column = wx.TextCtrl(parent=panel, id=wx.ID_ANY, value='',
                                 size=(150, -1), style=wx.TE_PROCESS_ENTER)
            column.Bind(wx.EVT_TEXT,       self.OnTableAddColumnName)
            column.Bind(wx.EVT_TEXT_ENTER, self.OnTableItemAdd)
            self.layerPage[layer]['addColName'] = column.GetId()
            addSizer.Add(item=label,
                         flag=wx.ALIGN_CENTER_VERTICAL)
            addSizer.Add(item=column,
                         flag=wx.ALIGN_CENTER_VERTICAL)
            # data type
            label  = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Data type"))
            addSizer.Add(item=label,
                         flag=wx.ALIGN_CENTER_VERTICAL)

            subSizer = wx.BoxSizer(wx.HORIZONTAL)
            type = wx.Choice (parent=panel, id=wx.ID_ANY,
                              choices = ["integer",
                                         "double",
                                         "varchar",
                                         "date"]) # FIXME
            type.Bind(wx.EVT_CHOICE, self.OnTableChangeType)
            self.layerPage[layer]['addColType'] = type.GetId()
            subSizer.Add(item=type,
                         flag=wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT,
                         border=3)
            # length
            label  = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Length"))
            length = wx.SpinCtrl(parent=panel, id=wx.ID_ANY, size=(65, -1),
                                 initial=250,
                                 min=1, max=1e6)
            length.Enable(False)
            self.layerPage[layer]['addColLength'] = length.GetId()
            subSizer.Add(item=label,
                         flag=wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT,
                         border=3)
            subSizer.Add(item=length,
                         flag=wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT,
                         border=3)

            addSizer.Add(item=subSizer,
                         flag=wx.ALIGN_CENTER_VERTICAL | wx.LEFT | wx.RIGHT,
                         border=3)

            btnAddCol = wx.Button(parent=panel, id=wx.ID_ADD)
            btnAddCol.Bind(wx.EVT_BUTTON, self.OnTableItemAdd)
            btnAddCol.Enable(False)
            self.layerPage[layer]['addColButton'] = btnAddCol.GetId()
            addSizer.Add(item=btnAddCol,
                         proportion=0,
                         flag=wx.EXPAND | wx.ALIGN_RIGHT | wx.FIXED_MINSIZE |
                         wx.ALIGN_CENTER_VERTICAL )

            # rename col
            label  = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Rename column"))
            column = wx.TextCtrl(parent=panel, id=wx.ID_ANY, value='',
                                 size=(150, -1), style=wx.TE_PROCESS_ENTER)
            column.Bind(wx.EVT_TEXT,       self.OnTableRenameColumnName,)
            column.Bind(wx.EVT_TEXT_ENTER, self.OnTableItemChange)
            self.layerPage[layer]['renameCol'] = column.GetId()
            addSizer.Add(item=label,
                         flag=wx.ALIGN_CENTER_VERTICAL)
            addSizer.Add(item=column,
                         flag=wx.ALIGN_CENTER_VERTICAL)
            label  = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("To"))
            columnTo = wx.TextCtrl(parent=panel, id=wx.ID_ANY, value='',
                                   size=(150, -1), style=wx.TE_PROCESS_ENTER)
            columnTo.Bind(wx.EVT_TEXT,       self.OnTableRenameColumnName)
            columnTo.Bind(wx.EVT_TEXT_ENTER, self.OnTableItemChange)
            self.layerPage[layer]['renameColTo'] = columnTo.GetId()
            addSizer.Add(item=label,
                         flag=wx.ALIGN_CENTER_VERTICAL | wx.ALIGN_CENTER)
            addSizer.Add(item=columnTo,
                         flag=wx.ALIGN_CENTER_VERTICAL)
            btnRenameCol = wx.Button(parent=panel, id=wx.ID_ANY, label=_("&Rename"))
            btnRenameCol.Bind(wx.EVT_BUTTON, self.OnTableItemChange)
            btnRenameCol.Enable(False)
            self.layerPage[layer]['renameColButton'] = btnRenameCol.GetId()

            addSizer.Add(item=btnRenameCol,
                         proportion=0,
                         flag=wx.EXPAND | wx.ALIGN_RIGHT | wx.FIXED_MINSIZE |
                         wx.ALIGN_CENTER_VERTICAL )

            tableSizer.Add(item=list,
                           flag=wx.ALL | wx.EXPAND,
                           proportion=1,
                           border=3)

            tableSizer.Add(item=addSizer,
                           flag=wx.ALL | wx.EXPAND,
                           proportion=0,
                           border=3)

            pageSizer.Add(item=infoCollapse,
                          flag=wx.ALL | wx.EXPAND,
                          proportion=0,
                          border=3)

            pageSizer.Add(item=tableSizer,
                          flag=wx.LEFT | wx.RIGHT | wx.BOTTOM | wx.EXPAND,
                          proportion=1,
                          border=3)

            panel.SetSizer(pageSizer)

            self.layerPage[layer]['dbinfo'] = infoCollapse.GetId()

        self.manageTablePage.SetSelection(0) # select first layer
        self.layer = self.mapDBInfo.layers.keys()[0]

    def __createTableDesc(self, parent, table):
        """Create list with table description"""
        list = TableListCtrl(parent=parent, id=wx.ID_ANY,
                             table=self.mapDBInfo.tables[table],
                             columns=self.mapDBInfo.GetColumns(table))
        list.Populate()
        # sorter
        # itemDataMap = list.Populate()
        # listmix.ColumnSorterMixin.__init__(self, 2)

        return list

    def __createManageLayerPage(self):
        """Create settings page"""
        splitterWin = wx.SplitterWindow(parent=self.manageLayerPage, id=wx.ID_ANY)
        self.manageLayerPage.AddPage(page=splitterWin,
                                     text=_("Layers of vector map")) # dummy page

        #
        # list of layers
        #
        panelList = wx.Panel(parent=splitterWin, id=wx.ID_ANY)

        panelListSizer  = wx.BoxSizer(wx.VERTICAL)
        layerBox = wx.StaticBox(parent=panelList, id=wx.ID_ANY,
                                label=" %s " % _("List of layers"))
        layerSizer = wx.StaticBoxSizer(layerBox, wx.VERTICAL)

        self.layerList = self.__createLayerDesc(panelList)
        self.layerList.Bind(wx.EVT_COMMAND_RIGHT_CLICK, self.OnLayerRightUp) #wxMSW
        self.layerList.Bind(wx.EVT_RIGHT_UP,            self.OnLayerRightUp) #wxGTK

        layerSizer.Add(item=self.layerList,
                       flag=wx.ALL | wx.EXPAND,
                       proportion=1,
                       border=3)

        panelListSizer.Add(item=layerSizer,
                           flag=wx.ALL | wx.EXPAND,
                           proportion=1,
                           border=3)

        panelList.SetSizer(panelListSizer)

        #
        # manage part
        #
        panelManage = wx.Panel(parent=splitterWin, id=wx.ID_ANY)

        manageSizer = wx.BoxSizer(wx.VERTICAL)

        self.manageLayerBook = LayerBook(parent=panelManage, id=wx.ID_ANY,
                                         parentDialog=self)

        manageSizer.Add(item=self.manageLayerBook,
                        proportion=1,
                        flag=wx.LEFT | wx.RIGHT | wx.BOTTOM | wx.EXPAND,
                        border=5)

        panelManage.SetSizer(manageSizer)

        #splitterWin.SetMinimumPaneSize(20)
        splitterWin.SplitHorizontally(panelList, panelManage, 100) 

    def __createLayerDesc(self, parent):
        """Create list of linked layers"""
        list = LayerListCtrl(parent=parent, id=wx.ID_ANY,
                             layers=self.mapDBInfo.layers)
        list.Populate()
        # sorter
        # itemDataMap = list.Populate()
        # listmix.ColumnSorterMixin.__init__(self, 2)

        return list

    def __createSettingsPage(self):
        """Create settings page"""
        panel = wx.Panel(parent=self.settingsPage, id=wx.ID_ANY)
        self.settingsPage.AddPage(page=panel, text=_("General settings")) # dummy page

        pageSizer = wx.BoxSizer(wx.VERTICAL)
        highlightBox = wx.StaticBox(parent=panel, id=wx.ID_ANY,
                                    label=" %s " % _("Highlighting"))
        highlightSizer = wx.StaticBoxSizer(highlightBox, wx.VERTICAL)

        flexSizer = wx.FlexGridSizer (cols=2, hgap=5, vgap=5)
        flexSizer.AddGrowableCol(0)
        label = wx.StaticText(parent=panel, id=wx.ID_ANY, label="Color")
        self.hlColor = csel.ColourSelect(parent=panel, id=wx.ID_ANY,
                                          colour=self.settings['highlight']['color'],
                                          size=(25, 25))
        flexSizer.Add(label, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(self.hlColor, proportion=0, flag=wx.ALIGN_RIGHT | wx.FIXED_MINSIZE)

        label = wx.StaticText(parent=panel, id=wx.ID_ANY, label=_("Line width (in pixels)"))
        self.hlWidth = wx.SpinCtrl(parent=panel, id=wx.ID_ANY, size=(50, -1),
                                   initial=self.settings['highlight']['width'],
                                   min=1, max=1e6)
        flexSizer.Add(label, proportion=0, flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(self.hlWidth, proportion=0, flag=wx.ALIGN_RIGHT | wx.FIXED_MINSIZE)


        highlightSizer.Add(item=flexSizer,
                           proportion=0,
                           flag=wx.ALL | wx.EXPAND,
                           border=5)

        pageSizer.Add(item=highlightSizer,
                      proportion=0,
                      flag=wx.ALL | wx.EXPAND,
                      border=5)

        panel.SetSizer(pageSizer)

    def __layout(self):
        """Do layout"""
        # frame body
        mainSizer = wx.BoxSizer(wx.VERTICAL)

        # buttons
        btnSizer = wx.StdDialogButtonSizer()
        btnSizer.AddButton(self.btnQuit)
        btnSizer.AddButton(self.btnApply)
        btnSizer.Realize()

        mainSizer.Add(item=self.notebook, proportion=1, flag=wx.EXPAND)
        mainSizer.Add(item=btnSizer, proportion=0, flag=wx.EXPAND | wx.ALL, border=5)

        self.SetAutoLayout(True)
        self.SetSizer(mainSizer)
        # FIXME
        # mainSizer.Fit(self) # problem connected to aui
        self.Layout()
        
        # self.SetMinSize(self.GetBestSize())
        self.SetSize((680, 520))
        self.SetMinSize(self.GetSize())

    def OnDataRightUp(self, event):
        """Table description area, context menu"""
        if not hasattr(self, "popupDataID1"):
            self.popupDataID1 = wx.NewId()
            self.popupDataID2 = wx.NewId()
            self.popupDataID3 = wx.NewId()
            self.popupDataID4 = wx.NewId()
            self.popupDataID5 = wx.NewId()
            self.popupDataID6 = wx.NewId()
            self.Bind(wx.EVT_MENU, self.OnDataItemEdit,       id=self.popupDataID1)
            self.Bind(wx.EVT_MENU, self.OnDataItemAdd,        id=self.popupDataID2)
            self.Bind(wx.EVT_MENU, self.OnDataItemDelete,     id=self.popupDataID3)
            self.Bind(wx.EVT_MENU, self.OnDataItemDeleteAll,  id=self.popupDataID4)
            self.Bind(wx.EVT_MENU, self.OnDataReload,         id=self.popupDataID5)
            self.Bind(wx.EVT_MENU, self.OnDataDrawSelected,   id=self.popupDataID6)

        list = self.FindWindowById(self.layerPage[self.layer]['data'])
        # generate popup-menu
        menu = wx.Menu()
        menu.Append(self.popupDataID1, _("Edit selected record"))
        selected = list.GetFirstSelected()
        if selected == -1 or list.GetNextSelected(selected) != -1:
            menu.Enable(self.popupDataID1, False)
        menu.AppendSeparator()
        menu.Append(self.popupDataID2, _("Insert new record"))
        menu.AppendSeparator()
        menu.Append(self.popupDataID3, _("Delete selected record(s)"))
        if list.GetFirstSelected() == -1:
            menu.Enable(self.popupDataID3, False)
        menu.Append(self.popupDataID4, _("Delete all records"))
        menu.AppendSeparator()
        menu.Append(self.popupDataID5, _("Reload"))
        menu.AppendSeparator()
        menu.Append(self.popupDataID6, _("Highlight selected in the map"))
        if not self.map:
            menu.Enable(self.popupDataID6, False)

        self.PopupMenu(menu)
        menu.Destroy()

        # update statusbar
        self.log.write(_("Number of loaded records: %d") % \
                           list.GetItemCount())

    def OnDataItemDelete(self, event):
        """Delete selected item(s) from the list (layer/category pair)"""
        list = self.FindWindowById(self.layerPage[self.layer]['data'])
        item = list.GetFirstSelected()

        table    = self.mapDBInfo.layers[self.layer]["table"]
        key      = self.mapDBInfo.layers[self.layer]["key"]

        while item != -1:
            # list.DeleteItem(item)
            cat = int(list.GetItemText(item)) # FIXME
            
            self.listOfSQLStatements.append('DELETE FROM %s WHERE %s=%d' % \
                                                (table, key, cat))

            index = list.itemIndexMap[item]
            del list.itemIndexMap[item]
            del list.itemDataMap[index]
            list.SetItemCount(list.GetItemCount()-1)
            
            item = list.GetNextSelected(item)

        event.Skip()

    def OnDataItemDeleteAll(self, event):
        """Delete all items from the list"""
        list = self.FindWindowById(self.layerPage[self.layer]['data'])
        list.DeleteAllItems()
        list.itemDataMap  = {}
        list.itemIndexMap = []
        list.SetItemCount(0)

        table = self.mapDBInfo.layers[self.layer]["table"]
        self.listOfSQLStatements.append('DELETE FROM %s' % table)

        event.Skip()

    def OnDataDrawSelected(self, event):
        """Reload table description"""
        if self.map and self.mapdisplay:
            # list.lastTurnSelectedCats[:] != list.selectedCats[:]:

            # add map layer with higlighted vector features
            self.AddQueryMapLayer(self.map)
            self.mapdisplay.MapWindow.UpdateMap(render=True)

        event.Skip()

    def OnDataItemAdd(self, event):
        """Add new record to the attribute table"""
        list      = self.FindWindowById(self.layerPage[self.layer]['data'])
        table     = self.mapDBInfo.layers[self.layer]['table']
        keyColumn = self.mapDBInfo.layers[self.layer]['key']

        # (column name, value)
        data = []

        # collect names of all visible columns
        columnName = []
        for i in range(list.GetColumnCount()): 
            columnName.append(list.GetColumn(i).GetText())

        # maximal category number
        maxCat = max(list.itemCatsMap.values())

        # key column must be always presented
        if keyColumn not in columnName:
            columnName.insert(0, keyColumn) # insert key column on first position
            data.append((keyColumn, str(maxCat + 1)))
            missingKey = True
        else:
            missingKey = False
            
        # add other visible columns
        for col in columnName:
            if col == keyColumn: # key 
                if missingKey is False: 
                    data.append((col, str(maxCat + 1)))
            else:
                data.append((col, ''))

        dlg = ModifyTableRecord(parent=self, id=wx.ID_ANY,
                                title=_("Insert new record"),
                                data=data)

        if dlg.ShowModal() == wx.ID_OK:
            try: # get category number
                cat = int(dlg.GetValues(columns=[keyColumn])[0])
            except:
                cat = -1

            try:
                if cat in list.itemCatsMap.values():
                    raise ValueError(_("Record with category number %d "
                                       "already exists in the table.") % cat)

                values = dlg.GetValues() # values (need to be casted)
                columnsString = ''
                valuesString   = ''
                
                for i in range(len(values)):
                    if len(values[i]) == 0: # NULL
                        if columnName[i] == keyColumn:
                            raise ValueError(_("Category number (column %s)"
                                               " is missing.") % keyColumn)
                        else:
                            continue

                    try:
                        values[i] = list.columns[columnName[i]]['ctype'] (values[i])
                    except:
                        raise ValueError(_("Casting value '%s' to %s failed.") % \
                                             (str(values[i]),
                                              list.columns[columnName[i]]['type']))
                    columnsString += '%s,' % columnName[i]
                    if list.columns[columnName[i]]['ctype'] == str:
                        valuesString += "'%s'," % values[i]
                    else:
                        valuesString += "%s," % values[i]

            except ValueError, err:
                dlg = wx.MessageDialog(parent=self,
                                       message=_("Unable to insert new record.%s"
                                                 "%s") % (os.linesep, err),
                                       caption=_("Error"), style=wx.OK | wx.ICON_ERROR)
                dlg.ShowModal()
                dlg.Destroy()
                return

            # remove category if need 
            if missingKey is True:
                del values[0]
                
            # add new item to the list
            index = max(list.itemIndexMap) + 1
            list.itemIndexMap.append(index)
            list.itemDataMap[index] = values
            list.itemCatsMap[index] = cat
            list.SetItemCount(list.GetItemCount() + 1)

            # store statement for OnApply()
            self.listOfSQLStatements.append('INSERT INTO %s (%s) VALUES(%s)' % \
                                                (table,
                                                 columnsString.strip(','),
                                                 valuesString.strip(',')))
            
    def OnDataItemEdit(self, event):
        """Edit selected record of the attribute table"""
        list      = self.FindWindowById(self.layerPage[self.layer]['data'])
        item      = list.GetFirstSelected()
        if item == -1:
            return

        table     = self.mapDBInfo.layers[self.layer]['table']
        keyColumn = self.mapDBInfo.layers[self.layer]['key']
        cat       = list.itemCatsMap[list.itemIndexMap[item]]

        # (column name, value)
        data = []

        # collect names of all visible columns
        columnName = []
        for i in range(list.GetColumnCount()): 
            columnName.append(list.GetColumn(i).GetText())


        # key column must be always presented
        if keyColumn not in columnName:
            columnName.insert(0, keyColumn) # insert key column on first position
            data.append((keyColumn, str(cat)))
            keyId = 0
            missingKey = True
        else:
            missingKey = False
            
        # add other visible columns
        for i in range(len(columnName)):
            if columnName[i] == keyColumn: # key 
                if missingKey is False: 
                    data.append((columnName[i], str(cat)))
                    keyId = i
            else:
                if missingKey is True:
                    value = list.GetItem(item, i-1).GetText()
                else:
                    value = list.GetItem(item, i).GetText()
                data.append((columnName[i], value))

        dlg = ModifyTableRecord(parent=self, id=wx.ID_ANY,
                                title=_("Update existing record"),
                                data=data, keyEditable=(keyId, False))

        if dlg.ShowModal() == wx.ID_OK:
            values = dlg.GetValues() # string
            updateString = ''
            try:
                for i in range(len(values)): 
                    if i == keyId: # skip key column
                        continue

                    if str(list.GetItem(item, i).GetText()) != str(values[i]):
                        if len(values[i]) > 0:
                            try:
                                if missingKey is True:
                                    idx = i - 1
                                else:
                                    idx = i
                                list.itemDataMap[item][idx] = \
                                    list.columns[columnName[i]]['ctype'] (values[i])
                            except:
                                raise ValueError(_("Casting value '%s' to %s failed.") % \
                                                     (str(values[i]),
                                                      list.columns[columnName[i]]['type']))

                            if list.columns[columnName[i]]['ctype'] == str:
                                updateString += "%s='%s'," % (columnName[i], values[i])
                            else:
                                updateString += "%s=%s," % (columnName[i], values[i])
                        else: # NULL
                            updateString += "%s=NULL," % (columnName[i])

            except ValueError, err:
                dlg = wx.MessageDialog(parent=self,
                                       message=_("Unable to update existing record.%s"
                                                 "%s") % (os.linesep, err),
                                       caption=_("Error"), style=wx.OK | wx.ICON_ERROR)
                dlg.ShowModal()
                dlg.Destroy()
                return

            if len(updateString) > 0:
                self.listOfSQLStatements.append('UPDATE %s SET %s WHERE %s=%d' % \
                                                     (table, updateString.strip(','),
                                                      keyColumn, cat))
    def OnDataReload(self, event):
        """Reload list of records"""
        self.OnApplySqlStatement(None)
        self.listOfSQLStatements = []

    def OnTableChangeType(self, event):
        """Data type for new column changed. Enable or disable
        data length widget"""
        win = self.FindWindowById(self.layerPage[self.layer]['addColLength'])
        if event.GetString() == "varchar":
            win.Enable(True)
        else:
            win.Enable(False)

    def OnTableRenameColumnName(self, event):
        """Editing column name to be added to the table"""
        btn  = self.FindWindowById(self.layerPage[self.layer]['renameColButton'])
        col  = self.FindWindowById(self.layerPage[self.layer]['renameCol'])
        colTo = self.FindWindowById(self.layerPage[self.layer]['renameColTo'])
        if len(col.GetValue()) > 0 and len(colTo.GetValue()) > 0:
            btn.Enable(True)
        else:
            btn.Enable(False)

        event.Skip()

    def OnTableAddColumnName(self, event):
        """Editing column name to be added to the table"""
        btn = self.FindWindowById(self.layerPage[self.layer]['addColButton'])
        if len(event.GetString()) > 0:
            btn.Enable(True)
        else:
            btn.Enable(False)

        event.Skip()

    def OnTableItemChange(self, event):
        """Rename column in the table"""
        list   = self.FindWindowById(self.layerPage[self.layer]['tableData'])
        name   = self.FindWindowById(self.layerPage[self.layer]['renameCol']).GetValue()
        nameTo = self.FindWindowById(self.layerPage[self.layer]['renameColTo']).GetValue()

        if not name or not nameTo:
            dlg = wx.MessageDialog(self.parent,
                                   _("Unable to rename column. "
                                     "No column name defined."),
                                   _("Error"), wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
        else:
            item = list.FindItem(start=-1, str=name)
            if item > -1:
                if list.FindItem(start=-1, str=nameTo) > -1:
                    dlg = wx.MessageDialog(self.parent,
                                           _("Unable to rename column. "
                                             "Column <%s> already exists in the table.") % \
                                               nameTo,
                                           _("Error"), wx.OK | wx.ICON_ERROR)
                    dlg.ShowModal()
                    dlg.Destroy()
                else:
                    list.SetItemText(item, nameTo)

                    self.listOfCommands.append(['v.db.renamecol',
                                                'map=%s' % self.vectmap,
                                                'layer=%d' % self.layer,
                                                'column=%s,%s' % (name, nameTo)])
            else:
                dlg = wx.MessageDialog(self.parent,
                                       _("Unable to rename column. "
                                         "Column <%s> doesn't exist in the table.") % name,
                                       _("Error"), wx.OK | wx.ICON_ERROR)
                dlg.ShowModal()
                dlg.Destroy()

        event.Skip()

    def OnTableRightUp(self, event):
        """Table description area, context menu"""
        if not hasattr(self, "popupTableID"):
            self.popupTableID1 = wx.NewId()
            self.popupTableID2 = wx.NewId()
            self.popupTableID3 = wx.NewId()
            self.Bind(wx.EVT_MENU, self.OnTableItemDelete,    id=self.popupTableID1)
            self.Bind(wx.EVT_MENU, self.OnTableItemDeleteAll, id=self.popupTableID2)
            self.Bind(wx.EVT_MENU, self.OnTableReload,        id=self.popupTableID3)

        # generate popup-menu
        menu = wx.Menu()
        menu.Append(self.popupTableID1, _("Drop selected column"))
        if self.FindWindowById(self.layerPage[self.layer]['tableData']).GetFirstSelected() == -1:
            menu.Enable(self.popupTableID1, False)
        menu.Append(self.popupTableID2, _("Drop all columns"))
        menu.AppendSeparator()
        menu.Append(self.popupTableID3, _("Reload"))

        self.PopupMenu(menu)
        menu.Destroy()

    def OnTableItemDelete(self, event):
        """Delete selected item(s) from the list"""
        list = self.FindWindowById(self.layerPage[self.layer]['tableData'])

        item = list.GetFirstSelected()
        while item != -1:
            self.listOfCommands.append(['v.db.dropcol',
                                        'map=%s' % self.vectmap,
                                        'layer=%d' % self.layer,
                                        'column=%s' % list.GetItemText(item)])
            list.DeleteItem(item)
            item = list.GetFirstSelected()

        event.Skip()

    def OnTableItemDeleteAll(self, event):
        """Delete all items from the list"""
        table = self.mapDBInfo.layers[self.layer]['table']
        cols = self.mapDBInfo.GetColumns(table)
        self.listOfCommands = [['v.db.dropcol',
                               'map=%s' % self.vectmap,
                               'layer=%d' % self.layer,
                               'column=%s' % ','.join(cols)]]
        self.FindWindowById(self.layerPage[self.layer]['tableData']).DeleteAllItems()

        event.Skip()

    def OnTableReload(self, event):
        """Reload table description"""
        self.FindWindowById(self.layerPage[self.layer]['tableData']).Populate(update=True)
        self.listOfCommands = []

    def OnTableItemAdd(self, event):
        """Add new column to the table"""
        name = self.FindWindowById(self.layerPage[self.layer]['addColName']).GetValue()

        if not name:
            dlg = wx.MessageDialog(self.parent,
                                   _("Unable to add column to the table. "
                                     "No column name defined."),
                                   _("Error"), wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
        else:
            type = self.FindWindowById(self.layerPage[self.layer]['addColType']). \
                GetStringSelection()
            # cast type if needed
            if type == 'double':
                type = 'double precision'
            if type == 'varchar':
                length = int(self.FindWindowById(self.layerPage[self.layer]['addColLength']). \
                    GetValue())
            else:
                length = '' # FIXME
            # add item to the list of table columns
            list = self.FindWindowById(self.layerPage[self.layer]['tableData'])
            # check for duplicate items
            if list.FindItem(start=-1, str=name) > -1:
                dlg = wx.MessageDialog(self.parent,
                                       _("Unable to add column <%s> to the table. "
                                         "Column already exists.") % name,
                                       _("Error"), wx.OK | wx.ICON_ERROR)
                dlg.ShowModal()
                dlg.Destroy()
                return
            index = list.InsertStringItem(sys.maxint, str(name))
            list.SetStringItem(index, 0, str(name))
            list.SetStringItem(index, 1, str(type))
            list.SetStringItem(index, 2, str(length))

            # add v.db.addcol command to the list
            if type == 'varchar':
                type += ' (%d)' % length
            self.listOfCommands.append(['v.db.addcol',
                                        'map=%s' % self.vectmap,
                                        'layer=%d' % self.layer,
                                        'columns=%s %s' % (name, type)])

    def OnLayerPageChanged(self, event):
        """Layer tab changed"""
        pageNum = event.GetSelection()
        self.layer = self.mapDBInfo.layers.keys()[pageNum]
        try:
            self.OnChangeSql(None)
            # update statusbar
            self.log.write(_("Number of loaded records: %d") % \
                               self.FindWindowById(self.layerPage[self.layer]['data']).\
                               GetItemCount())
        except:
            pass

    def OnLayerRightUp(self, event):
        """Layer description area, context menu"""
        pass

    def OnChangeSql(self, event):
        """Switch simple/advanced sql statement"""
        if self.FindWindowById(self.layerPage[self.layer]['simple']).GetValue():
            self.FindWindowById(self.layerPage[self.layer]['where']).Enable(True)
            self.FindWindowById(self.layerPage[self.layer]['statement']).Enable(False)
            self.FindWindowById(self.layerPage[self.layer]['builder']).Enable(False)
        else:
            self.FindWindowById(self.layerPage[self.layer]['where']).Enable(False)
            self.FindWindowById(self.layerPage[self.layer]['statement']).Enable(True)
            self.FindWindowById(self.layerPage[self.layer]['builder']).Enable(True)

    def OnApply(self, event):
        """Apply button pressed"""
        # page = self.notebook.GetSelection()

        # perform GRASS commands (e.g. v.db.addcol)
        if len(self.listOfCommands) > 0:
            for cmd in self.listOfCommands:
                Debug.msg(3, 'AttributeManager.OnApply() cmd=\'%s\'' %
                          ' '.join(cmd))
                gcmd.Command(cmd)

            self.mapDBInfo = VectorDBInfo(self.vectmap)
            table = self.mapDBInfo.layers[self.layer]['table']

            # update table description
            list = self.FindWindowById(self.layerPage[self.layer]['tableData'])
            list.Update(table=self.mapDBInfo.tables[table],
                        columns=self.mapDBInfo.GetColumns(table))
            self.OnTableReload(None)

            # update data list
            list = self.FindWindowById(self.layerPage[self.layer]['data'])
            list.Update(self.mapDBInfo)
            self.OnDataReload(None)

            # reset list of commands
            self.listOfCommands = []

        # perform SQL non-select statements (e.g. 'delete from table where cat=1')
        if len(self.listOfSQLStatements) > 0:
            sqlFile = tempfile.NamedTemporaryFile(mode="w")
            for sql in self.listOfSQLStatements:
                sqlFile.file.write(sql + ";\n")
                sqlFile.file.flush()

            driver   = self.mapDBInfo.layers[self.layer]["driver"]
            database = self.mapDBInfo.layers[self.layer]["database"]

            cmd = ['db.execute',
                   'input=%s' % sqlFile.name,
                   'driver=%s' % driver,
                   'database=%s' % database]

            Debug.msg(3, 'AttributeManger.OnApply(): %s' %
                      ';'.join(["%s" % s for s in self.listOfSQLStatements]))

            gcmd.Command(cmd)

            # reset list of statements
            self.listOfSQLStatements = []

        # perform select statement
        self.OnApplySqlStatement(event)

        # update settings
        self.UpdateSettings()

    def OnApplySqlStatement(self, event):
        """Apply simple/advanced sql statement"""
        if self.FindWindowById(self.layerPage[self.layer]['simple']).GetValue():
            # simple sql statement
            where = self.FindWindowById(self.layerPage[self.layer]['where']).GetValue().strip()
            if len(where) > 0:
                self.FindWindowById(self.layerPage[self.layer]['data']).LoadData( \
                    self.layer, where=where)
            else:
                self.FindWindowById(self.layerPage[self.layer]['data']).LoadData( \
                    self.layer)
        else:
            # advanced sql statement
            valid, cols, where = \
                self.ValidateSelectStatement( \
                self.FindWindowById(self.layerPage[self.layer]['statement']).GetValue().strip())

            Debug.msg(4, "AttributeManager.OnApplySqlStatament(): valid=%s, cols=%s, where=%s" %
                      (valid, cols, where))

            if valid is True:
                self.FindWindowById(self.layerPage[self.layer]['data']).LoadData( \
                    self.layer, cols=cols, where=where)

        # update statusbar
        self.log.write(_("Number of loaded records: %d") % \
                           self.FindWindowById(self.layerPage[self.layer]['data']).GetItemCount())

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

        tablelen = len(self.mapDBInfo.layers[self.layer]['table'])

        if statement[index+1:index+6].lower() != 'from ' or \
                statement[index+6:index+6+tablelen] != '%s' % \
                (self.mapDBInfo.layers[self.layer]['table']):
            return (False, '', '')

        if len(statement[index+7+tablelen:]) > 0:
            index = statement.lower().find('where ')
            if index > -1:
                where = statement[index+6:]
            else:
                where = ''
        else:
            where = ''

        return (True, cols, where)

    def OnInfoPaneChanged(self, event):
        """Collapse database connection info box"""

        if self.FindWindowById(self.layerPage[self.layer]['dbinfo']).IsExpanded():
            self.FindWindowById(self.layerPage[self.layer]['dbinfo']).SetLabel( \
                self.infoCollapseLabelCol)
        else:
             self.FindWindowById(self.layerPage[self.layer]['dbinfo']).SetLabel( \
                 self.infoCollapseLabelExp)

        # redo layout
        self.Layout()

    def MakeInfoPaneContent(self, layer, pane):
        """Create database connection information content"""
            # connection info
        border = wx.BoxSizer(wx.VERTICAL)

        connectionInfoBox = wx.StaticBox(parent=pane, id=wx.ID_ANY,
                                         label=" %s " % _("Database connection"))
        infoSizer = wx.StaticBoxSizer(connectionInfoBox, wx.VERTICAL)
        infoFlexSizer = wx.FlexGridSizer (cols=2, hgap=1, vgap=1)
        infoFlexSizer.AddGrowableCol(1)

        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="Driver:"))
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="%s" % \
                                                 self.mapDBInfo.layers[layer]['driver']))
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="Database:"))
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="%s" % \
                                                 self.mapDBInfo.layers[layer]['database']))
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="Table:"))
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="%s" % \
                                                 self.mapDBInfo.layers[layer]['table']))
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="Key:"))
        infoFlexSizer.Add(item=wx.StaticText(parent=pane, id=wx.ID_ANY,
                                             label="%s" % \
                                                 self.mapDBInfo.layers[layer]['key']))

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
        self.Close()
        event.Skip()

    def OnBuilder(self,event):
        """SQL Builder button pressed"""
        self.builder = sqlbuilder.SQLFrame(parent=self, id=wx.ID_ANY,
                                           title=_("SQL Builder"),
                                           vectmap=self.vectmap)
    def OnTextEnter(self, event):
        pass

    def OnSQLBuilder(self, event):
        pass

    def OnDataItemActivated(self, event):
        """Item activated, highlight selected item"""
        self.OnDataDrawSelected(event)

        event.Skip()

    def AddQueryMapLayer(self, map):
        """Redraw a map

        Return True if map has been redrawn, False if no map is given
        """
        if self.qlayer:
            map.DeleteLayer(self.qlayer)

        list = self.FindWindowById(self.layerPage[self.layer]['data'])
        # cats = list.selectedCats[:]
        cats = list.GetSelectedItems()

        color = self.settings['highlight']['color']
        colorStr = str(color[0]) + ":" + \
            str(color[1]) + ":" + \
            str(color[2]) + ":"
        cmd = ["d.vect",
               "map=%s" % self.vectmap,
               "color=%s" % colorStr,
               "fcolor=%s" % colorStr,
               #               "cats=%s" % (",".join(["%d" % c for c in cats])),
               # FIXME
               "cats=%s" % utils.ListOfCatsToRange(cats),
               "width=%d"  % self.settings['highlight']['width']]
        if self.icon:
            cmd.append("icon=%s" % (self.icon))
        if self.pointsize:
            cmd.append("size=%s" % (self.pointsize))

        self.qlayer = map.AddLayer(type='vector', name=globalvar.QUERYLAYER, command=cmd,
                                   l_active=True, l_hidden=True, l_opacity=1.0)

    def UpdateSettings(self):
        """Update settings dict"""
        self.settings['highlight']['color'] = self.hlColor.GetColour()
        self.settings['highlight']['width'] = int(self.hlWidth.GetValue())

    def UpdateDialog(self, layer):
        """Updates dialog layout for given layer"""
        #
        # delete page
        #
        if layer in self.mapDBInfo.layers.keys():
            # delete page
            # draging pages disallowed
            # if self.browsePage.GetPageText(page).replace('Layer ', '').strip() == str(layer):
            # self.browsePage.DeletePage(page)
            # break
            self.browsePage.RemovePage(self.mapDBInfo.layers.keys().index(layer))
            self.manageTablePage.RemovePage(self.mapDBInfo.layers.keys().index(layer))
            # set current page selection
            self.notebook.SetSelection(2)

        # fetch fresh db info
        self.mapDBInfo = VectorDBInfo(self.vectmap)    

        #
        # add new page
        #
        if layer in self.mapDBInfo.layers.keys():
            # 'browse data' page
            self.__createBrowsePage(layer)
            # 'manage tables' page
            self.__createManageTablePage(layer)
            # set current page selection
            self.notebook.SetSelection(2)
            
        #
        # 'manage layers' page
        #
        # update list of layers
        self.layerList.Update(self.mapDBInfo.layers)
        self.layerList.Populate(update=True)
        # update selected widgets
        listOfLayers = map(str, self.mapDBInfo.layers.keys())
        self.manageLayerBook.deleteLayer.SetItems(listOfLayers)
        self.manageLayerBook.deleteLayer.SetStringSelection(listOfLayers[0])
        tableName = self.mapDBInfo.layers[int(listOfLayers[0])]['table']
        self.manageLayerBook.deleteTable.SetLabel( \
            _('Drop also linked attribute table (%s)') % \
                tableName)
        self.manageLayerBook.addLayerWidgets['layer'][1].SetValue(\
            max(self.mapDBInfo.layers.keys())+1)

#     def OnMapClick(self, event):
#         """
#         Gets coordinates from mouse clicking on display window
#         """
#         Debug.msg(3, "VirtualAttributeList.OnMapClick()")

#         # map coordinates
#         x, y = self.mapdisp.MapWindow.Pixel2Cell(event.GetPositionTuple()[:])

#         category = ""
#         for line in os.popen("v.what east_north=%f,%f map=%s" %\
#                 (x,y,self.vectmap)).readlines():
#             if "Category:" in line:
#                 category = line.strip().split(" ")[1]

#         #print category

#         for idx in range(self.GetItemCount()):
#             item = self.GetItem(idx, 0)
#             if item.GetText() == category:
#                 #print idx
#                 # self.Select(idx,True)
#                 self.EnsureVisible( idx )
#                 self.SetItemState(idx, wx.LIST_STATE_SELECTED, wx.LIST_STATE_SELECTED)
#             else:
#                 # self.SetItemState(idx, wx.LIST_STATE_DESELECTED, wx.LIST_STATE_DESELECTED)
#                 self.Select(idx,False)


#                 #        try:
#                 #            os.environ["GRASS_MESSAGE_FORMAT"] = "gui"
#                 #            cmd = "v.what -a east_north=%d,%d distance=%d map=%@%" % (x,y,100,self.tablename, self.self.mapset)
#                 #            self.cmd_output.write(cmd+"\n----------\n")
#                 #            p = Popen(cmd +" --verbose", shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True)
#                 #
#                 #            oline = p.stderr.readline()
#                 #            while oline:
#                 #                oline = oline.strip()
#                 #                oline = p.stderr.readline()
#                 #                if oline.find("GRASS_INFO_MESSAGE")>-1:
#                 #                    self.cmd_output.write(string.split(oline,maxsplit=1)[1]+"\n")
#                 #                elif oline.find("GRASS_INFO_WARNING")>-1:
#                 #                    self.cmd_output.write("WARNING: "+string.split(oline,maxsplit=1)[1]+"\n")
#                 #                elif oline.find("GRASS_INFO_ERROR")>-1:
#                 #                    self.cmd_output.write("ERROR: "+string.split(oline,maxsplit=1)[1]+"\n")
#                 #
#                 #            oline = p.stdout.readline()
#                 #            while oline:
#                 #                oline = oline.strip()
#                 #                self.cmd_output.write(oline+"\n")
#                 #                print oline+"\n"
#                 #                print >> sys.stderr, oline
#                 #                oline = p.stdout.readline()
#                 #
#                 #            if p.stdout < 0:
#                 #                print >> sys.stderr, "Child was terminated by signal", p.stdout
#                 #            elif p.stdout > 0:
#                 #                print >> sys.stderr, p.stdout
#                 #                pass
#                 #        except OSError, e:
#                 #            print >> sys.stderr, "Execution failed:", e

#                 #        try:
#                 #            os.environ["GRASS_MESSAGE_FORMAT"] = "gui"
#                 #            cmd = "v.what -a east_north=%d,%d distance=%d map=%@%" % (x,y,100,self.tablename, self.self.mapset)
#                 #            self.cmd_output.write(cmd+"\n----------\n")
#                 #            p = Popen(cmd +" --verbose", shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True)
#                 #
#                 #            oline = p.stderr.readline()
#                 #            while oline:
#                 #                oline = oline.strip()
#                 #                oline = p.stderr.readline()
#                 #                if oline.find("GRASS_INFO_MESSAGE")>-1:
#                 #                    self.cmd_output.write(string.split(oline,maxsplit=1)[1]+"\n")
#                 #                elif oline.find("GRASS_INFO_WARNING")>-1:
#                 #                    self.cmd_output.write("WARNING: "+string.split(oline,maxsplit=1)[1]+"\n")
#                 #                elif oline.find("GRASS_INFO_ERROR")>-1:
#                 #                    self.cmd_output.write("ERROR: "+string.split(oline,maxsplit=1)[1]+"\n")
#                 #
#                 #            oline = p.stdout.readline()
#                 #            while oline:
#                 #                oline = oline.strip()
#                 #                self.cmd_output.write(oline+"\n")
#                 #                print oline+"\n"
#                 #                print >> sys.stderr, oline
#                 #                oline = p.stdout.readline()
#                 #
#                 #            if p.stdout < 0:
#                 #                print >> sys.stderr, "Child was terminated by signal", p.stdout
#                 #            elif p.stdout > 0:
#                 #                print >> sys.stderr, p.stdout
#                 #                pass
#                 #        except OSError, e:
#                 #            print >> sys.stderr, "Execution failed:", e

class TableListCtrl(wx.ListCtrl,
                    listmix.ListCtrlAutoWidthMixin):
                    #                    listmix.TextEditMixin):
    """Table description list"""

    def __init__(self, parent, id, table, columns, pos=wx.DefaultPosition,
                 size=wx.DefaultSize):

        self.parent  = parent
        self.table   = table
        self.columns = columns
        wx.ListCtrl.__init__(self, parent, id, pos, size,
                             style=wx.LC_REPORT | wx.LC_HRULES | wx.LC_VRULES |
                             wx.BORDER_NONE)

        listmix.ListCtrlAutoWidthMixin.__init__(self)
        # listmix.TextEditMixin.__init__(self)

    def Update(self, table, columns):
        """Update column description"""
        self.table   = table
        self.columns = columns

    def Populate(self, update=False):
        """Populate the list"""
        itemData = {} # requested by sorter

        if not update:
            headings = [_("Column name"), _("Type"), _("Length")]
            i = 0
            for h in headings:
                self.InsertColumn(col=i, heading=h)
                self.SetColumnWidth(col=i, width=150)
                i += 1
        else:
            self.DeleteAllItems()

        i = 0
        for column in self.columns:
            index = self.InsertStringItem(sys.maxint, str(column))
            self.SetStringItem(index, 0, str(column))
            self.SetStringItem(index, 1, str(self.table[column]['type']))
            self.SetStringItem(index, 2, str(self.table[column]['length']))
            self.SetItemData(index, i)
            itemData[i] = (str(column),
                           str(self.table[column]['type']),
                           int(self.table[column]['length']))
            i = i + 1

        return itemData

class LayerListCtrl(wx.ListCtrl,
                    listmix.ListCtrlAutoWidthMixin):
                    # listmix.ColumnSorterMixin):
                    # listmix.TextEditMixin):
    """Layer description list"""

    def __init__(self, parent, id, layers,
                 pos=wx.DefaultPosition,
                 size=wx.DefaultSize):

        self.parent = parent
        self.layers = layers
        wx.ListCtrl.__init__(self, parent, id, pos, size,
                             style=wx.LC_REPORT | wx.LC_HRULES | wx.LC_VRULES |
                             wx.BORDER_NONE)

        listmix.ListCtrlAutoWidthMixin.__init__(self)
        # listmix.TextEditMixin.__init__(self)

    def Update(self, layers):
        """Update description"""
        self.layers = layers

    def Populate(self, update=False):
        """Populate the list"""
        itemData = {} # requested by sorter

        if not update:
            headings = [_("Layer"),  _("Driver"), _("Database"), _("Table"), _("Key")]
            i = 0
            for h in headings:
                self.InsertColumn(col=i, heading=h)
                i += 1
        else:
            self.DeleteAllItems()

        i = 0
        for layer in self.layers.keys():
            index = self.InsertStringItem(sys.maxint, str(layer))
            self.SetStringItem(index, 0, str(layer))
            database = str(self.layers[layer]['database'])
            driver   = str(self.layers[layer]['driver'])
            table    = str(self.layers[layer]['table'])
            key      = str(self.layers[layer]['key'])
            self.SetStringItem(index, 1, driver)
            self.SetStringItem(index, 2, database)
            self.SetStringItem(index, 3, table)
            self.SetStringItem(index, 4, key)
            self.SetItemData(index, i)
            itemData[i] = (str(layer),
                           driver,
                           database,
                           table,
                           key)
            i += 1

        for i in range(self.GetColumnCount()):
            self.SetColumnWidth(col=i, width=wx.LIST_AUTOSIZE)
            if self.GetColumnWidth(col=i) < 60:
                self.SetColumnWidth(col=i, width=60)

        return itemData

class LayerBook(wx.Notebook):
    """Manage layers (add, delete, modify)"""
    def __init__(self, parent, id,
                 parentDialog,
                 style=wx.BK_DEFAULT):
        wx.Notebook.__init__(self, parent, id, style=style)

        self.parent       = parent
        self.parentDialog = parentDialog
        self.mapDBInfo    = self.parentDialog.mapDBInfo

        #
        # drivers
        #
        cmdDriver = gcmd.Command(['db.drivers',
                                  '-p',
                                  '--q'])
        self.listOfDrivers = []
        for drv in cmdDriver.ReadStdOutput():
            self.listOfDrivers.append(drv.strip())

        #
        # get default values
        #
        cmdConnect = gcmd.Command(['db.connect',
                                   '-p',
                                   '--q'])
        for line in cmdConnect.ReadStdOutput():
            item, value = line.split(':')
            item  = item.strip()
            value = value.strip()
            if item == 'driver':
                self.defaultDriver = value
            elif item == 'database':
                self.defaultDatabase = value

        self.defaultTables = self.__getTables(self.defaultDriver, self.defaultDatabase)
        try:
            self.defaultColumns = self.__getColumns(self.defaultDriver, self.defaultDatabase,
                                               listOfTables[0])
        except:
            self.defaultColumns = []

        self.__createAddPage()
        self.__createDeletePage()
        self.__createModifyPage()

    def __createAddPage(self):
        """Add new layer"""
        self.addPanel = wx.Panel(parent=self, id=wx.ID_ANY)
        self.AddPage(page=self.addPanel, text=_("Add new layer"))

        #
        # list of layer widgets (label, value)
        #
        self.addLayerWidgets = {'layer':
                                    (wx.StaticText(parent=self.addPanel, id=wx.ID_ANY,
                                                   label='%s:' % _("Layer")),
                                     wx.SpinCtrl(parent=self.addPanel, id=wx.ID_ANY, size=(65, -1),
                                                 initial=max(self.mapDBInfo.layers.keys())+1,
                                                 min=1, max=1e6)),
                                'driver':
                                    (wx.StaticText(parent=self.addPanel, id=wx.ID_ANY,
                                                   label='%s:' % _("Driver")),
                                     wx.Choice(parent=self.addPanel, id=wx.ID_ANY, size=(200, -1),
                                               choices=self.listOfDrivers)),
                                'database':
                                    (wx.StaticText(parent=self.addPanel, id=wx.ID_ANY,
                                                   label='%s:' % _("Database")),
                                     wx.TextCtrl(parent=self.addPanel, id=wx.ID_ANY,
                                                 value='',
                                                 style=wx.TE_PROCESS_ENTER)),
                                'table':
                                    (wx.StaticText(parent=self.addPanel, id=wx.ID_ANY,
                                                   label='%s:' % _("Table")),
                                     wx.Choice(parent=self.addPanel, id=wx.ID_ANY, size=(200, -1),
                                               choices=self.defaultTables)),
                                'key':
                                    (wx.StaticText(parent=self.addPanel, id=wx.ID_ANY,
                                                   label='%s:' % _("Key column")),
                                     wx.Choice(parent=self.addPanel, id=wx.ID_ANY, size=(200, -1),
                                               choices=self.defaultColumns))}
        
        # set default values for widgets
        self.addLayerWidgets['driver'][1].SetStringSelection(self.defaultDriver)
        self.addLayerWidgets['database'][1].SetValue(self.defaultDatabase)
        self.addLayerWidgets['table'][1].SetSelection(0)
        self.addLayerWidgets['key'][1].SetSelection(0)
        # events
        self.addLayerWidgets['driver'][1].Bind(wx.EVT_CHOICE, self.OnDriverChanged)
        self.addLayerWidgets['database'][1].Bind(wx.EVT_TEXT_ENTER, self.OnDatabaseChanged)
        self.addLayerWidgets['table'][1].Bind(wx.EVT_CHOICE, self.OnTableChanged)

        #
        # list of table widgets
        #
        self.tableWidgets = {'table': (wx.StaticText(parent=self.addPanel, id=wx.ID_ANY,
                                                     label='%s:' % _("Table name")),
                                       wx.TextCtrl(parent=self.addPanel, id=wx.ID_ANY,
                                                   value='',
                                                   style=wx.TE_PROCESS_ENTER)),
                             'key': (wx.StaticText(parent=self.addPanel, id=wx.ID_ANY,
                                                   label='%s:' % _("Key column")),
                                     wx.TextCtrl(parent=self.addPanel, id=wx.ID_ANY,
                                                 value='',
                                                 style=wx.TE_PROCESS_ENTER))}
        # events
        self.tableWidgets['table'][1].Bind(wx.EVT_TEXT_ENTER, self.OnCreateTable)
        self.tableWidgets['key'][1].Bind(wx.EVT_TEXT_ENTER, self.OnCreateTable)

        btnTable   = wx.Button(self.addPanel, wx.ID_ANY, _("&Create table"),
                             size=(125,-1))
        btnTable.Bind(wx.EVT_BUTTON, self.OnCreateTable)

        btnLayer   = wx.Button(self.addPanel, wx.ID_ANY, _("&Add layer"),
                             size=(125,-1))
        btnLayer.Bind(wx.EVT_BUTTON, self.OnAddLayer)

        btnDefault = wx.Button(self.addPanel, wx.ID_ANY, _("&Set default"),
                               size=(125,-1))
        btnDefault.Bind(wx.EVT_BUTTON, self.OnSetDefault)

        #
        # do layout
        #
        pageSizer = wx.BoxSizer(wx.HORIZONTAL)

        #
        # layer description
        #
        layerBox = wx.StaticBox (parent=self.addPanel, id=wx.ID_ANY,
                                 label=" %s " % (_("Layer description")))
        layerSizer = wx.StaticBoxSizer(layerBox, wx.VERTICAL)

        # data area
        dataSizer = wx.FlexGridSizer(cols=2, hgap=5, vgap=5)
        dataSizer.AddGrowableCol(1)
        for key in ('layer', 'driver', 'database', 'table', 'key'):
            label, value = self.addLayerWidgets[key]
            dataSizer.Add(item=label,
                          flag=wx.ALIGN_CENTER_VERTICAL)
            if label.GetLabel() == "Layer:":
                dataSizer.Add(item=value,
                              flag=wx.ALIGN_CENTER_VERTICAL | wx.ALIGN_LEFT)
            else:
                dataSizer.Add(item=value,
                              flag=wx.ALIGN_CENTER_VERTICAL | wx.EXPAND)

        layerSizer.Add(item=dataSizer,
                       proportion=1,
                       flag=wx.ALL | wx.EXPAND,
                       border=5)

        btnSizer = wx.BoxSizer(wx.HORIZONTAL)
        btnSizer.Add(item=btnDefault,
                     proportion=0,
                     flag=wx.ALL | wx.ALIGN_LEFT,
                     border=5)

        btnSizer.Add(item=(5,5),
                     proportion=1,
                     flag=wx.ALL | wx.EXPAND,
                     border=5)

        btnSizer.Add(item=btnLayer,
                     proportion=0,
                     flag=wx.ALL | wx.ALIGN_RIGHT,
                     border=5)

        layerSizer.Add(item=btnSizer,
                       proportion=0,
                       flag=wx.ALL | wx.EXPAND,
                       border=0)

        #
        # table description
        #
        tableBox = wx.StaticBox (parent=self.addPanel, id=wx.ID_ANY,
                                 label=" %s " % (_("Table description")))
        tableSizer = wx.StaticBoxSizer(tableBox, wx.VERTICAL)

        # data area
        dataSizer = wx.FlexGridSizer(cols=2, hgap=5, vgap=5)
        dataSizer.AddGrowableCol(1)
        for key in ['table', 'key']:
            label, value = self.tableWidgets[key]
            dataSizer.Add(item=label,
                          flag=wx.ALIGN_CENTER_VERTICAL)
            dataSizer.Add(item=value,
                          flag=wx.ALIGN_CENTER_VERTICAL | wx.EXPAND)

        tableSizer.Add(item=dataSizer,
                       proportion=1,
                       flag=wx.ALL | wx.EXPAND,
                       border=5)

        tableSizer.Add(item=btnTable,
                       proportion=0,
                       flag=wx.ALL | wx.ALIGN_BOTTOM | wx.ALIGN_RIGHT,
                       border=5)


        pageSizer.Add(item=layerSizer,
                      proportion=1,
                      flag=wx.ALL | wx.EXPAND,
                      border=5)

        pageSizer.Add(item=tableSizer,
                      proportion=1,
                      flag=wx.ALL | wx.EXPAND,
                      border=5)

        self.addPanel.SetSizer(pageSizer)

    def __createDeletePage(self):
        """Delete layer"""
        self.deletePanel = wx.Panel(parent=self, id=wx.ID_ANY)
        self.AddPage(page=self.deletePanel, text=_("Delete selected layer"))

        label = wx.StaticText(parent=self.deletePanel, id=wx.ID_ANY,
                              label='%s:' % _("Layer to detele"))

        self.deleteLayer = wx.ComboBox(parent=self.deletePanel, id=wx.ID_ANY, size=(100, -1),
                                       style=wx.CB_SIMPLE | wx.CB_READONLY,
                                       choices=map(str, self.mapDBInfo.layers.keys()))
        self.deleteLayer.SetSelection(0)
        self.deleteLayer.Bind(wx.EVT_COMBOBOX, self.OnChangeLayer)

        tableName = self.mapDBInfo.layers[int(self.deleteLayer.GetStringSelection())]['table']

        self.deleteTable = wx.CheckBox(parent=self.deletePanel, id=wx.ID_ANY,
                                       label=_('Drop also linked attribute table (%s)') % \
                                           tableName)

        btnDelete   = wx.Button(self.deletePanel, wx.ID_DELETE, _("&Delete layer"),
                                size=(125,-1))
        btnDelete.Bind(wx.EVT_BUTTON, self.OnDeleteLayer)

        #
        # do layout
        #
        pageSizer = wx.BoxSizer(wx.VERTICAL)

        dataSizer = wx.BoxSizer(wx.VERTICAL)

        flexSizer = wx.FlexGridSizer(cols=2, hgap=5, vgap=5)
        flexSizer.AddGrowableCol(2)

        flexSizer.Add(item=label,
                      flag=wx.ALIGN_CENTER_VERTICAL)
        flexSizer.Add(item=self.deleteLayer,
                      flag=wx.ALIGN_CENTER_VERTICAL)

        dataSizer.Add(item=flexSizer,
                      proportion=0,
                      flag=wx.ALL | wx.EXPAND,
                      border=5)

        dataSizer.Add(item=self.deleteTable,
                      proportion=0,
                      flag=wx.ALL | wx.EXPAND,
                      border=5)

        pageSizer.Add(item=dataSizer,
                      proportion=1,
                      flag=wx.ALL | wx.EXPAND,
                      border=5)

        pageSizer.Add(item=btnDelete,
                       proportion=0,
                       flag=wx.ALL | wx.ALIGN_RIGHT,
                       border=5)

        self.deletePanel.SetSizer(pageSizer)

    def __createModifyPage(self):
        """Modify layer"""
        self.modifyPanel = wx.Panel(parent=self, id=wx.ID_ANY)
        self.AddPage(page=self.modifyPanel, text=_("Modify selected layer"))

        #
        # list of layer widgets (label, value)
        #
        self.modifyLayerWidgets = {'layer':
                                       (wx.StaticText(parent=self.modifyPanel, id=wx.ID_ANY,
                                                      label='%s:' % _("Layer")),
                                        wx.ComboBox(parent=self.modifyPanel, id=wx.ID_ANY,
                                                    size=(100, -1),
                                                    style=wx.CB_SIMPLE | wx.CB_READONLY,
                                                    choices=map(str, 
                                                                self.mapDBInfo.layers.keys()))),
                                   'driver':
                                       (wx.StaticText(parent=self.modifyPanel, id=wx.ID_ANY,
                                                      label='%s:' % _("Driver")),
                                        wx.Choice(parent=self.modifyPanel, id=wx.ID_ANY,
                                                  size=(200, -1),
                                                  choices=self.listOfDrivers)),
                                   'database':
                                       (wx.StaticText(parent=self.modifyPanel, id=wx.ID_ANY,
                                                      label='%s:' % _("Database")),
                                        wx.TextCtrl(parent=self.modifyPanel, id=wx.ID_ANY,
                                                    value='', size=(350, -1),
                                                    style=wx.TE_PROCESS_ENTER)),
                                   'table':
                                       (wx.StaticText(parent=self.modifyPanel, id=wx.ID_ANY,
                                                      label='%s:' % _("Table")),
                                        wx.Choice(parent=self.modifyPanel, id=wx.ID_ANY,
                                                  size=(200, -1),
                                                  choices=self.defaultTables)),
                                   'key':
                                       (wx.StaticText(parent=self.modifyPanel, id=wx.ID_ANY,
                                                      label='%s:' % _("Key column")),
                                        wx.Choice(parent=self.modifyPanel, id=wx.ID_ANY,
                                                  size=(200, -1),
                                                  choices=self.defaultColumns))}
        
        # set default values for widgets
        self.modifyLayerWidgets['layer'][1].SetSelection(0)
        layer    = int(self.modifyLayerWidgets['layer'][1].GetStringSelection())
        driver   = self.mapDBInfo.layers[layer]['driver']
        database = self.mapDBInfo.layers[layer]['database']
        table    = self.mapDBInfo.layers[layer]['table']
        listOfColumns = self.__getColumns(driver, database, table)
        self.modifyLayerWidgets['driver'][1].SetStringSelection(driver)
        self.modifyLayerWidgets['database'][1].SetValue(database)
        self.modifyLayerWidgets['table'][1].SetStringSelection(table)
        self.modifyLayerWidgets['key'][1].SetItems(listOfColumns)
        self.modifyLayerWidgets['key'][1].SetSelection(0)

        # events
        self.modifyLayerWidgets['layer'][1].Bind(wx.EVT_COMBOBOX, self.OnChangeLayer)
        # self.modifyLayerWidgets['driver'][1].Bind(wx.EVT_CHOICE, self.OnDriverChanged)
        # self.modifyLayerWidgets['database'][1].Bind(wx.EVT_TEXT_ENTER, self.OnDatabaseChanged)
        # self.modifyLayerWidgets['table'][1].Bind(wx.EVT_CHOICE, self.OnTableChanged)

        btnModify = wx.Button(self.modifyPanel, wx.ID_DELETE, _("&Modify layer"),
                              size=(125,-1))
        btnModify.Bind(wx.EVT_BUTTON, self.OnModifyLayer)

        #
        # do layout
        #
        pageSizer = wx.BoxSizer(wx.VERTICAL)

        # data area
        dataSizer = wx.FlexGridSizer(cols=2, hgap=5, vgap=5)
        dataSizer.AddGrowableCol(1)
        for key in ('layer', 'driver', 'database', 'table', 'key'):
            label, value = self.modifyLayerWidgets[key]
            dataSizer.Add(item=label,
                          flag=wx.ALIGN_CENTER_VERTICAL)
            if label.GetLabel() == "Layer:":
                dataSizer.Add(item=value,
                              flag=wx.ALIGN_CENTER_VERTICAL | wx.ALIGN_LEFT)
            else:
                dataSizer.Add(item=value,
                              flag=wx.ALIGN_CENTER_VERTICAL)

        pageSizer.Add(item=dataSizer,
                      proportion=1,
                      flag=wx.ALL | wx.EXPAND,
                      border=5)

        pageSizer.Add(item=btnModify,
                      proportion=0,
                      flag=wx.ALL | wx.ALIGN_RIGHT,
                      border=5)

        self.modifyPanel.SetSizer(pageSizer)

    def __getTables(self, driver, database):
        """Get list of tables for given driver and database"""
        tables = []

        cmdTable = gcmd.Command(['db.tables',
                                 '-p', '--q',
                                 'driver=%s' % driver,
                                 'database=%s' % database])

        for table in cmdTable.ReadStdOutput():
            tables.append(table)

        return tables

    def __getColumns(self, driver, database, table):
        """Get list of column of given table"""
        columns = []

        cmdColumn = gcmd.Command(['db.columns',
                                  '--q',
                                  'driver=%s' % driver,
                                  'database=%s' % database,
                                  'table=%s' % table])

        for column in cmdColumn.ReadStdOutput():
            columns.append(column)

        return columns

    def OnDriverChanged(self, event):
        """Driver selection changed, update list of tables"""
        driver = event.GetString()
        database = self.addLayerWidgets['database'][1].GetValue()

        winTable = self.addLayerWidgets['table'][1]
        winKey   = self.addLayerWidgets['key'][1]
        tables   = self.__getTables(driver, database)

        winTable.SetItems(tables)
        winTable.SetSelection(0)

        if len(tables) == 0:
            winKey.SetItems([])

        event.Skip()

    def OnDatabaseChanged(self, event):
        """Database selection changed, update list of tables"""
        event.Skip()

    def OnTableChanged(self, event):
        """Table name changed, update list of columns"""
        driver   = self.addLayerWidgets['driver'][1].GetStringSelection()
        database = self.addLayerWidgets['database'][1].GetValue()
        table    = event.GetString()

        win  = self.addLayerWidgets['key'][1]
        cols = self.__getColumns(driver, database, table)
        win.SetItems(cols)
        win.SetSelection(0)

        event.Skip()

    def OnSetDefault(self, event):
        """Set default values"""
        driver   = self.addLayerWidgets['driver'][1]
        database = self.addLayerWidgets['database'][1]
        table    = self.addLayerWidgets['table'][1]
        key      = self.addLayerWidgets['key'][1]

        driver.SetStringSelection(self.defaultDriver)
        database.SetValue(self.defaultDatabase)
        tables = self.__getTables(self.defaultDriver, self.defaultDatabase)
        table.SetItems(tables)
        table.SetSelection(0)
        if len(tables) == 0:
            key.SetItems([])
        else:
            cols = self.__getColumns(self.defaultDriver, self.defaultDatabase, tables[0])
            key.SetItems(cols)
            key.SetSelection(0)

        event.Skip()

    def OnCreateTable(self, event):
        """Create new table (name and key column given)"""
        driver   = self.addLayerWidgets['driver'][1].GetStringSelection()
        database = self.addLayerWidgets['database'][1].GetValue()
        table    = self.tableWidgets['table'][1].GetValue()
        key      = self.tableWidgets['key'][1].GetValue()
        
        if not table or not key:
            dlg = wx.MessageDialog(self.parent,
                                   _("Unable to create new table.%s"
                                     "Table name or key column name is missing.") % \
                                       (os.linesep),
                                   _("Error"), wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return
        
        # create table
        sql = 'CREATE TABLE %s (%s INTEGER)' % (table, key)

        gcmd.Command(['db.execute',
                      '--q',
                      'driver=%s' % driver,
                      'database=%s' % database],
                     stdin=sql)

        # update list of tables
        tableList = self.addLayerWidgets['table'][1]
        tableList.SetItems(self.__getTables(driver, database))
        tableList.SetStringSelection(table)

        # update key column selection
        keyList = self.addLayerWidgets['key'][1]
        keyList.SetItems(self.__getColumns(driver, database, table))
        keyList.SetStringSelection(key)
        
        event.Skip()

    def OnAddLayer(self, event):
        """Add new layer to vector map"""
        layer    = int(self.addLayerWidgets['layer'][1].GetValue())
        layerWin = self.addLayerWidgets['layer'][1]
        driver   = self.addLayerWidgets['driver'][1].GetStringSelection()
        database = self.addLayerWidgets['database'][1].GetValue()
        table    = self.addLayerWidgets['table'][1].GetStringSelection()
        key      = self.addLayerWidgets['key'][1].GetStringSelection()
        
        if layer in self.mapDBInfo.layers.keys():
            dlg = wx.MessageDialog(self.parent,
                                   _("Unable to add new layer to vector map <%s>.%s"
                                     "Layer %d already exists.") % \
                                       (self.mapDBInfo.map, os.linesep, layer),
                                   _("Error"), wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            return

        connectCmd = gcmd.Command(['v.db.connect',
                                   '--q',
                                   'map=%s' % self.mapDBInfo.map,
                                   'driver=%s' % driver,
                                   'database=%s' % database,
                                   'table=%s' % table,
                                   'key=%s' % key,
                                   'layer=%d' % layer])

        if connectCmd.returncode == 0:
            # update dialog (only for new layer)
            self.parentDialog.UpdateDialog(layer=layer) 
            # update db info
            self.mapDBInfo = self.parentDialog.mapDBInfo
            # increase layer number
            layerWin.SetValue(layer+1)

    def OnDeleteLayer(self, event):
        """Delete layer"""

        try:
            layer = int(self.deleteLayer.GetValue())
        except:
            return

        gcmd.Command(['v.db.connect',
                      '-d', '--q',
                      'map=%s' % self.mapDBInfo.map,
                      'layer=%d' % layer])

        # drop also table linked to layer which is deleted
        if self.deleteTable.IsChecked():
            driver   = self.addLayerWidgets['driver'][1].GetStringSelection()
            database = self.addLayerWidgets['database'][1].GetValue()
            table    = self.mapDBInfo.layers[layer]['table']
            sql      = 'DROP TABLE %s' % (table)

            gcmd.Command(['db.execute',
                          '--q',
                          'driver=%s' % driver,
                          'database=%s' % database],
                         stdin=sql)

        # update dialog
        self.parentDialog.UpdateDialog(layer=layer) 
        # update db info
        self.mapDBInfo = self.parentDialog.mapDBInfo

        event.Skip()

    def OnChangeLayer(self, event):
        """Layer number of layer to be deleted is changed"""
        try:
            layer = int(event.GetString())
        except:
            try:
                layer = self.mapDBInfo.layers.keys()[0]
            except:
                layer = 1

        if self.GetCurrentPage() == self.modifyPanel:
            driver   = self.mapDBInfo.layers[layer]['driver']
            database = self.mapDBInfo.layers[layer]['database']
            table    = self.mapDBInfo.layers[layer]['table']
            listOfColumns = self.__getColumns(driver, database, table)
            self.modifyLayerWidgets['driver'][1].SetStringSelection(driver)
            self.modifyLayerWidgets['database'][1].SetValue(database)
            self.modifyLayerWidgets['table'][1].SetStringSelection(table)
            self.modifyLayerWidgets['key'][1].SetItems(listOfColumns)
            self.modifyLayerWidgets['key'][1].SetSelection(0)
        else:
            self.deleteTable.SetLabel(_('Drop also linked attribute table (%s)') % \
                                          self.mapDBInfo.layers[layer]['table'])
        if event:
            event.Skip()

    def OnModifyLayer(self, event):
        """Modify layer connection settings"""

        layer = int(self.modifyLayerWidgets['layer'][1].GetStringSelection())

        modify = False
        if self.modifyLayerWidgets['driver'][1].GetStringSelection() != \
                self.mapDBInfo.layers[layer]['driver'] or \
                self.modifyLayerWidgets['database'][1].GetStringSelection() != \
                self.mapDBInfo.layers[layer]['database'] or \
                self.modifyLayerWidgets['table'][1].GetStringSelection() != \
                self.mapDBInfo.layers[layer]['table'] or \
                self.modifyLayerWidgets['key'][1].GetStringSelection() != \
                self.mapDBInfo.layers[layer]['key']:
            modify = True

        if modify:
            # delete layer
            gcmd.Command(['v.db.connect',
                          '-d', '--q',
                          'map=%s' % self.mapDBInfo.map,
                          'layer=%d' % layer])

            # add modified layer
            gcmd.Command(['v.db.connect',
                          '--q',
                          'map=%s' % self.mapDBInfo.map,
                          'driver=%s' % \
                              self.modifyLayerWidgets['driver'][1].GetStringSelection(),
                          'database=%s' % \
                              self.modifyLayerWidgets['database'][1].GetValue(),
                          'table=%s' % \
                              self.modifyLayerWidgets['table'][1].GetStringSelection(),
                          'key=%s' % \
                              self.modifyLayerWidgets['key'][1].GetStringSelection(),
                          'layer=%d' % layer])

            # update dialog (only for new layer)
            self.parentDialog.UpdateDialog(layer=layer) 
            # update db info
            self.mapDBInfo = self.parentDialog.mapDBInfo

        event.Skip()

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
        self.mapDBInfo = VectorDBInfo(self.map)

        layers = self.mapDBInfo.layers.keys() # get available layers

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
                                   _("No attribute table linked to "
                                     "vector map <%s> found.\n"
                                     "%s") % (self.map, label),
                                   _("Error"), wx.OK | wx.ICON_ERROR)
            dlg.ShowModal()
            dlg.Destroy()
            self.mapDBInfo = None
            return

        wx.Dialog.__init__(self, parent=self.parent, id=wx.ID_ANY,
                           title="", style=style, pos=pos)

        # dialog body
        mainSizer = wx.BoxSizer(wx.VERTICAL)

        if self.queryCoords: # select by position
            self.line, nselected = self.mapDBInfo.SelectByPoint(self.queryCoords,
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
            self.mapDBInfo = None

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
        for layer in self.mapDBInfo.layers.keys(): # for each layer
            table = self.mapDBInfo.layers[layer]["table"]
            columns = self.mapDBInfo.tables[table]
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
        for layer in self.mapDBInfo.layers.keys():
            table = self.mapDBInfo.layers[layer]["table"]
            columns = self.mapDBInfo.tables[table]
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

        if not self.mapDBInfo:
            return False

        self.mapDBInfo.Reset()

        layers = self.mapDBInfo.layers.keys() # get available layers

        # id of selected line
        if self.queryCoords: # select by position
            self.line, nselected = self.mapDBInfo.SelectByPoint(queryCoords,
                                                              qdist)
        # reset notebook
        self.notebook.DeleteAllPages()

        for layer in layers: # for each layer
            if self.layer > 0 and \
                    self.layer != layer:
                continue

            if not self.queryCoords: # select using layer/cat
                nselected = self.mapDBInfo.SelectFromTable(layer, where="cat=%d" % self.cat)

            if nselected <= 0 and self.action != "add":
                continue # nothing selected ...

            if self.action == "add":
                if nselected <= 0:
                    table = self.mapDBInfo.layers[layer]["table"]
                    columns = self.mapDBInfo.tables[table]
                    for name in columns.keys():
                        if name == "cat":
                            self.mapDBInfo.tables[table][name]['values'].append(self.cat)
                        else:
                            self.mapDBInfo.tables[table][name]['values'].append('')
                else: # change status 'add' -> 'update'
                    self.action = "update"

            panel = wx.Panel(parent=self.notebook, id=wx.ID_ANY)
            self.notebook.AddPage(page=panel, text=_(" %s %d ") % (_("Layer"), layer))

            # notebook body
            border = wx.BoxSizer(wx.VERTICAL)

            table   = self.mapDBInfo.layers[layer]["table"]
            columns = self.mapDBInfo.tables[table]

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

class VectorDBInfo:
    """Class providing information about attribute tables
    linked to the vector map"""
    def __init__(self, map):
        self.map = map
        # {layer number : {table, database, driver})
        self.layers = {}
        # {table : {column name : type, length, values, ids}}
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
                                              "key"      : lineList[2],
                                              "database" : lineList[3],
                                              "driver"   : lineList[4] }

        if (len(self.layers.keys()) == 0):
            return False

        return True

    def __DescribeTables(self):
        """Describe linked tables"""
        for layer in self.layers.keys():
            # determine column names and types
            table = self.layers[layer]["table"]
            columnsCommand = gcmd.Command (cmd=["db.describe",
                                                "-c", "--q",
                                                "table=%s" % self.layers[layer]["table"],
                                                "driver=%s" % self.layers[layer]["driver"],
                                                "database=%s" % self.layers[layer]["database"]])


            columns = {} # {name: {type, length, [values], [ids]}}

            if columnsCommand.returncode == 0:
                # get rid of nrows and ncols row...
                i = 0
                for line in columnsCommand.ReadStdOutput()[2:]:
                    num, name, type, length = line.strip().split(':')
                    # FIXME: here will be more types
                    if type.lower() == "integer":
                        ctype = int
                    elif type.lower() == "double" or type.lower() == "float":
                        ctype = float
                    else:
                        ctype = str

                    columns[name.strip()] = { 'index'  : i,
                                              'type'   : type.lower(),
                                              'ctype'  : ctype,
                                              'length' : int(length),
                                              'values' : [],
                                              'ids'    : []}
                    i += 1
            else:
                return False

            self.tables[table] = columns

        return True

    def GetColumns(self, table):
        """Return list of columns names (based on their index)"""
        names = [''] * len(self.tables[table].keys())
        for name, desc in self.tables[table].iteritems():
            names[desc['index']] = name

        return names

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
                        # casting ...
                        value = self.tables[table][name]['ctype'] (value.strip())
                        self.tables[table][name]['values'].append(value)
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

    def SelectFromTable(self, layer, cols='*', where=None):
        """Select records from the table

        Return number of selected records, -1 on error
        """
        if layer <= 0:
            return -1

        nselected = 0

        table = self.layers[layer]["table"] # get table desc
        # select values (only one record)
        if where is None or where is '':
            sql="SELECT %s FROM %s" % (cols, table)
        else:
            sql="SELECT %s FROM %s WHERE %s" % (cols, table, where)

        selectCommand = gcmd.Command(["db.select", "-v", "--q",
                                      "sql=%s" % sql,
                                      "database=%s" % self.layers[layer]["database"],
                                      "driver=%s"   % self.layers[layer]["driver"]])

        # self.tables[table]["cat"][1] = str(cat)
        if selectCommand.returncode == 0:
            for line in selectCommand.ReadStdOutput():
                name, value = line.split('|')
                # casting ...
                value = self.tables[table][name]['ctype'] (value)
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

class ModifyTableRecord(wx.Dialog):
    """Dialog for inserting/updating table record"""
    def __init__(self, parent, id, title, data, keyEditable=(-1, True),
                style=wx.DEFAULT_DIALOG_STYLE | wx.RESIZE_BORDER):
        """
        Notes:
         'Data' is a list: [(column, value)]
         'KeyEditable' (id, editable?) indicates if textarea for key column
          is editable(True) or not.
        """
        wx.Dialog.__init__(self, parent, id, title, style=style)

        self.panel = wx.Panel(parent=self, id=wx.ID_ANY)

        self.btnCancel = wx.Button(self.panel, wx.ID_CANCEL)
        self.btnSubmit = wx.Button(self.panel, wx.ID_OK)
        self.btnSubmit.SetDefault()

        self.widgets = []
        id = 0
        for column, value in data:
            label = wx.StaticText(parent=self.panel, id=wx.ID_ANY,
                                  label=column + ":")
            value = wx.TextCtrl(parent=self.panel, id=wx.ID_ANY,
                                value=value, size=(250, -1))
            if keyEditable[0] > -1: # id given
                if keyEditable[0] == id:
                    value.Enable(keyEditable[1])
            self.widgets.append((label.GetId(),
                                 value.GetId()))

            id += 1
        self.__Layout()

    def __Layout(self):
        """Do layout"""
        sizer = wx.BoxSizer(wx.VERTICAL)

        # data area
        dataSizer = wx.FlexGridSizer (cols=2, hgap=3, vgap=3)
        dataSizer.AddGrowableCol(1)

        for labelId, valueId in self.widgets:
            label = self.FindWindowById(labelId)
            value = self.FindWindowById(valueId)

            dataSizer.Add(label, proportion=0,
                          flag=wx.ALIGN_CENTER_VERTICAL)
            dataSizer.Add(value, proportion=0,
                          flag=wx.EXPAND)

        # buttons
        btnSizer = wx.StdDialogButtonSizer()
        btnSizer.AddButton(self.btnCancel)
        btnSizer.AddButton(self.btnSubmit)
        btnSizer.Realize()

        sizer.Add(item=dataSizer, proportion=1,
                  flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)

        sizer.Add(item=btnSizer, proportion=0,
                  flag=wx.EXPAND | wx.ALL | wx.ALIGN_CENTER, border=5)
       
        self.panel.SetSizer(sizer)
        sizer.Fit(self)

    def GetValues(self, columns=None):
        """Return list of values (as string).

        If columns is given (list), return only values of given columns.
        """
        valueList = []
        for labelId, valueId in self.widgets:
            column = self.FindWindowById(labelId).GetLabel().replace(':', '')
            if columns is None or column in columns:
                value = self.FindWindowById(valueId).GetValue()
                valueList.append(value)

        return valueList

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
    wx.InitAllImageHandlers()

    app = wx.PySimpleApp()
    f = AttributeManager(parent=None, id=wx.ID_ANY,
                         title=_("GRASS GIS Attribute Table Manager - vector map layer <%s>") % \
                             argv[1],
                         size=(900,600), vectmap=argv[1])
    f.Show()

    app.MainLoop()

if __name__ == '__main__':
    main()
