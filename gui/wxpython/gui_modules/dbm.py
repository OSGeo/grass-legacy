"""
Database browser for GRASS GIS >= 7

This program is based on FileHunter, publicated in "The wxPython Linux
Tutorial" on wxPython WIKI pages.

It also uses some functions from http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/426407

Usage:
    dbm.py table_name

"""
############################################################################
#
# MODULE:       dbm.py
# AUTHOR(S):    Jachym Cepicky <jachym les-ejk cz>
# PURPOSE:      GRASS attribute table manager
# COPYRIGHT:    (C) 2007 by the GRASS Development Team
#
#               This program is free software under the GNU General Public
#               License (>=v2). Read the file COPYING that comes with GRASS
#               for details.
#
#############################################################################
import wx
import wx.lib.mixins.listctrl  as  listmix

import sys,os,locale

try:
   from subprocess import *
except:
   from compat import subprocess
   from compat.subprocess import *

#----------------------------------------------------------------------
class Log:
    r"""\brief Needed by the wxdemos.
    The log output is redirected to the status bar of the containing frame.
    """
    def __init__(self,parent):
        self.parent = parent

    def write(self,text_string):
        self.parent.SetStatusText(text_string.strip())

#----------------------------------------------------------------------
# The panel you want to test (TestVirtualList)
#----------------------------------------------------------------------

class TestVirtualList(wx.ListCtrl, listmix.ListCtrlAutoWidthMixin, listmix.ColumnSorterMixin):
    def __init__(self, parent,log,vectmap,pointdata=None):
        wx.ListCtrl.__init__( self, parent, -1, style=wx.LC_REPORT|wx.LC_HRULES|wx.LC_VRULES) #wx.VIRTUAL

        self.log=log

        self.vectmap = vectmap
        self.mapname, self.mapset = self.vectmap.split("@")
        self.layer,self.tablename, self.column, self.database, self.driver =\
                 os.popen("v.db.connect -g map=%s" %\
                (self.vectmap)).readlines()[0].strip().split()

        self.icon = ''
        self.pointsize = ''

        if pointdata:
            self.icon = pointdata[0]
            self.pointsize = pointdata[1]

        self.columns = []
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

        # show us the result in map display
        if self.parent.gismanager:

            self.gism = self.parent.gismanager
            curr_pg = self.gism.gm_cb.GetCurrentPage()
            disp_idx = self.gism.track.Track().GetDisp_idx(curr_pg)

            self.mapdisp =  self.parent.gismanager.mapdisplays[disp_idx]
            self.map = self.gism.maptree.Map


        #building the columns
        i = 0
        # FIXME: subprocess.Popen should be used
        # FIXME: Maximal number of columns, when the GUI is still usable
        for line in os.popen("db.describe -c table=%s driver=%s database=%s" %\
                (self.tablename, self.driver, self.database)).readlines()[1:]:

            x,column,type = line.strip().split(":")
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
            self.SetColumnWidth(i, 50)
            i += 1
            if i >= 256:
                self.log.write("Can display only 256 columns")
                break

        #These two should probably be passed to init more cleanly
        #setting the numbers of items = number of elements in the dictionary
        self.itemDataMap = {}
        self.itemIndexMap = []
        # FIXME: subprocess.Popen should be used
        # FIXME: Max. number of rows, while the GUI is still usable
        i = 0
        # read data
        for line in os.popen("""db.select -c table=%s database=%s driver=%s """ %\
                (self.tablename,self.database,self.driver)):
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
                self.log.write("Can display only 32000 lines")
                break

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

        if self.parent.gismanager:
            self.mapdisp.MapWindow.Bind(wx.EVT_LEFT_DOWN, self.onMapClick)


    def OnCloseWindow(self, event):
        if self.qlayer: self.map.delLayer(item='qlayer')

    def OnColClick(self,event):
        self._col = event.GetColumn()
        event.Skip()

    def OnItemSelected(self, event):
        print "Selecting"
        self.currentItem = event.m_itemIndex
        self.log.write('OnItemSelected: "%s", "%s"\n' %
                           (self.currentItem,
                            self.GetItemText(self.currentItem)))

        # show us the result in map display
        #print self.par
        if self.parent.gismanager:

#            gism = self.parent.gismanager
#            curr_pg = gism.gm_cb.GetCurrentPage()
#            disp_idx = gism.track.Track().GetDisp_idx(curr_pg)

#            mapdisp =  self.parent.gismanager.mapdisplays[disp_idx]
#            map = gism.maptree.Map
            if self.qlayer: self.map.delLayer(item='qlayer')

            cat =  self.GetItemText(self.currentItem)

            # FIXME: width=1, because of maybe bug in PNG driver elusion
            # should be width=3 or something like this
            cmd = "d.vect map=%s color=yellow fcolor=yellow cats=%s width=1" % (self.vectmap, cat)
            if self.icon: cmd = cmd +"  icon=%s" % (self.icon)
            if self.pointsize: cmd = cmd + " size=%s" % (self.pointsize)

            self.qlayer = self.map.addLayer(item='qlayer', command=cmd, l_active=True,
                                      l_hidden=False, l_opacity=1, l_render=False)
            self.mapdisp.ReDraw(None)
        event.Skip() 

    def OnItemActivated(self, event):
        self.currentItem = event.m_itemIndex
        self.log.write("OnItemActivated: %s\nTopItem: %s\n" %
                           (self.GetItemText(self.currentItem), self.GetTopItem()))
        event.Skip() 

    def getColumnText(self, index, col):
        item = self.GetItem(index, col)
        return item.GetText()

    def OnItemDeselected(self, event):
        self.log.write("OnItemDeselected: %s" % evt.m_itemIndex)
        event.Skip() 


    #---------------------------------------------------
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
        #if ( index % 2) == 0:
        #    return self.attr2
        #else:
        #    return self.attr1

    #---------------------------------------------------
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

    # # stolen from python2.4/site-packages/wx-2.8-gtk2-unicode/wx/lib/mixins/listctrl.py
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

    #XXX Looks okay to remove this one (was present in the original demo)
    #def getColumnText(self, index, col):
    #    item = self.GetItem(index, col)
    #    return item.GetText()

    def onMapClick(self, event):
        """
        Gets coordinates from mouse clicking on display window
        """
        # screen coordinates
        posx, posy = event.GetPositionTuple()

        # map coordinates
        x, y = self.mapdisp.MapWindow.Pixel2Cell(posx, posy)
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
                self.Select(idx,True)
            else:
                self.Select(idx,False)


#        try:
#            os.environ["GRASS_MESSAGE_FORMAT"] = "gui"
#            cmd = "v.what -a east_north=%d,%d distance=%d map=%@%" % (x,y,100,self.tablename, self.self.mapset)
##            self.cmd_output.write(cmd+"\n----------\n")
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
##                self.cmd_output.write(oline+"\n")
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



        event.Skip()



#        try:
#            os.environ["GRASS_MESSAGE_FORMAT"] = "gui"
#            cmd = "v.what -a east_north=%d,%d distance=%d map=%@%" % (x,y,100,self.tablename, self.self.mapset)
##            self.cmd_output.write(cmd+"\n----------\n")
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
##                self.cmd_output.write(oline+"\n")
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



        event.Skip()










#----------------------------------------------------------------------
# The main window
#----------------------------------------------------------------------
# This is where you populate the frame with a panel from the demo.
#  original line in runTest (in the demo source):
#    win = TestPanel(nb, log)
#  this is changed to:
#    self.win=TestPanel(self,log)
#----------------------------------------------------------------------

class AttributeManager(wx.Frame):

    def __init__(self, parent, id, title, size, style = wx.DEFAULT_FRAME_STYLE,
                 vectmap=None,pointdata=None):

        wx.Frame.__init__(self, parent, id, title, size=size, style=style)

        self.CreateStatusBar(1)

        log=Log(self)

        # probably
        self.gismanager = parent

        self.win = TestVirtualList(self, log,vectmap=vectmap,pointdata=pointdata)
        self.Show()

def main(argv=None):
    if argv is None:
        argv = sys.argv

    if len(argv) != 2:
        print >>sys.stderr, __doc__
        sys.exit()

    # Command line arguments of the script to be run are preserved by the
    # hotswap.py wrapper but hotswap.py and its options are removed that
    # sys.argv looks as if no wrapper was present.
    #print "argv:", `argv`

    #some applications might require image handlers
    #wx.InitAllImageHandlers()

    app = wx.PySimpleApp()
    f = AttributeManager(None, -1, "GRASS Attribute Table Manager",wx.Size(500,300),vectmap=argv[1])
    app.MainLoop()



if __name__ == '__main__':
    main()
