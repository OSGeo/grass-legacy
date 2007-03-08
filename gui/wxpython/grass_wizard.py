import wx
import wx.wizard as wiz
import  wx.lib.rcsizer  as rcs

import os
import sys
import string


class TitledPage(wiz.WizardPageSimple):
    def __init__(self, parent, title):
        wiz.WizardPageSimple.__init__(self, parent)

        self.title = wx.StaticText(self,-1,title)
        self.title.SetFont(wx.Font(13, wx.SWISS, wx.NORMAL, wx.BOLD))

        self.sizer = rcs.RowColSizer()
        tmpsizer = wx.BoxSizer(wx.VERTICAL)

        tmpsizer.Add(self.title, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        tmpsizer.Add(wx.StaticLine(self, -1), 0, wx.EXPAND|wx.ALL, 0)
        tmpsizer.Add(self.sizer, wx.EXPAND)

        self.SetSizer(tmpsizer)
        self.SetAutoLayout(True)

    def MakeLabel(self, text):
        if text[-1] != " ":
            text += " "
        return wx.StaticText(self, -1, text, style=wx.ALIGN_RIGHT)

    def MakeTextCtrl(self,text='', size=(100,20)):
        return wx.TextCtrl(self,-1, text, size=size)

    def MakeButton(self,text, size=(75,25)):
        return wx.Button(self, -1, text,
                style=wx.ALIGN_CENTER_HORIZONTAL|wx.ALIGN_CENTER_VERTICAL,
                size=size)


class DatumPage(TitledPage):
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, "Specify geodetic datum")

        self.parent = parent

        # text input
        self.tdatum = self.MakeTextCtrl("")
        self.ttrans = self.MakeTextCtrl("")

        # search box
        self.searchb = wx.SearchCtrl(self, size=(200,-1), style=wx.TE_PROCESS_ENTER)

        # table
        self.tablewidth=600
        self.datums = wx.ListCtrl(self, -1, style=wx.LC_REPORT |  wx.LC_VRULES | wx.LC_HRULES, size=(700,100))
        self.datums.InsertColumn(0, 'Short Name ')
        self.datums.InsertColumn(1, '                                             Description                                         ')
        self.datums.SetColumnWidth(0, 100)
        self.datums.SetColumnWidth(1, wx.LIST_AUTOSIZE_USEHEADER)

        self.transformlist = wx.ListCtrl(self, -1, style=wx.LC_REPORT |  wx.LC_VRULES | wx.LC_HRULES, size=(700,100))
        self.transformlist.InsertColumn(0, 'Short Name ')
        self.transformlist.InsertColumn(1, '                                             Description                                         ')
        self.transformlist.SetColumnWidth(0, 100)
        self.transformlist.SetColumnWidth(1, wx.LIST_AUTOSIZE_USEHEADER)
        
        # laout
        self.sizer.Add(self.MakeLabel("Geodetic datum:"), 1, col=1, row=1)
        self.sizer.Add(self.tdatum, 0 , wx.ALIGN_LEFT, 1, row=1, col=2)
        self.sizer.Add(self.MakeLabel("Search in description:"), 1, col=1, row=2)
        self.sizer.Add(self.searchb, 0 , wx.ALIGN_LEFT, 1, row=2, col=2)
        self.sizer.Add(self.datums, 0 , wx.EXPAND | wx.ALIGN_CENTER_HORIZONTAL, 1, row=3, col=1, colspan=5)
        self.sizer.Add(self.MakeLabel("Transformation parameters:"), 1, col=2, row=4)
        self.sizer.Add(self.ttrans, 0 , wx.ALIGN_LEFT, 1, row=4, col=3)
        self.sizer.Add(self.transformlist, 0 , wx.EXPAND | wx.ALIGN_CENTER_HORIZONTAL, 1, row=5, col=1, colspan=5)

        # events
        #wx.EVT_BUTTON(self, self.bbrowse.GetId(), self.OnBrowse)
        #wx.EVT_BUTTON(self, self.bbcodes.GetId(), self.OnBrowseCodes)
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected, self.datums)
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnTransformSelected, self.transformlist)
        self.searchb.Bind(wx.EVT_TEXT_ENTER, self.OnDoSearch, self.searchb)
        self.tdatum.Bind(wx.EVT_TEXT_ENTER, self._onBrowseParams, self.tdatum)

        self._onBrowseDatums(None,None)

    def OnDoSearch(self,event):
        str =  self.searchb.GetValue()
        listItem  = self.datums.GetColumn(1)

        for i in range(self.datums.GetItemCount()):
            listItem = self.datums.GetItem(i,1)
            if listItem.GetText().find(str) > -1:
                datumcode = self.datums.GetItem(i, 0)
                self.tdatum.SetValue(datumcode.GetText())
                break

        self._onBrowseDatums(None,str)
        
    def OnTransformSelected(self,event):
        item = event.GetItem()
        self.ttrans.SetValue(str(item.GetText()))

    def OnItemSelected(self,event):
        item = event.GetItem()
        self.tdatum.SetValue(str(item.GetText()))
        self._onBrowseParams()
    
    def _onBrowseParams(self):
        params = ["Use whole region"]
        file = os.path.join(os.getenv("GISBASE"), "etc","datumtransform.table")
        search = self.tdatum.GetValue()

        try:
            f = open(file,"r")
            for line in f.readlines():
                line = line.strip()
                if line[0] == "#": continue
                id,descr = string.split(line," ",maxsplit=1)
                descr=descr.replace('"',"")
                if id == search:
                    params.append(descr)
            f.close()

            self.transformlist.DeleteAllItems()
            for i in range(len(params)):
                self.transformlist.InsertStringItem(i,str(i+1))
                self.transformlist.SetStringItem(i,1,params[i])
        except:
            self.transformlist.DeleteAllItems()


    def _onBrowseDatums(self,event,search=None):
        try:
            self.datums.DeleteAllItems()
            f = open(os.path.join(os.getenv("GISBASE"), "etc","datum.table"),"r")
            i=1
            j = 0
            descr = None
            datum = None
            for line in f.readlines():
                line = line.strip()
                if not line:
                    continue
                if line[0] == "#":
                    continue
                datum,descr = string.split(line, " ", maxsplit=1)
                descr = descr.replace('"',"")
                descr = descr.replace("  "," ")
                if search and (descr.lower().find(search.lower()) > -1 or\
                              datum.lower().find(search.lower()) > -1) or\
                        not search:
                    self.datums.InsertStringItem(j,datum)
                    self.datums.SetStringItem(j,1,descr)
                    j  += 1
                    # reset 
                    descr = None; code = None; params = ""
            f.close()
            self.SendSizeEvent()
        except StandardError, e:
            dlg = wx.MessageDialog(self, "Could not read datums: %s "
                    % e,"Can not read file",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()

    def OnChange(self,event):
            self.item =  event.GetItem()

class SummaryPage(TitledPage):
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, "Summary")

        self.parent = parent

        self.sizer.Add(self.MakeLabel("GRASS database:"), 1, flag=wx.ALIGN_RIGHT, row=1, col=2)
        self.sizer.Add(self.MakeLabel("Location name:"), 1, flag=wx.ALIGN_RIGHT, row=2, col=2)
        self.sizer.Add((200,20), 1, flag=wx.ALIGN_CENTER_HORIZONTAL, row=4, col=2)
        self.sizer.Add(self.MakeLabel("Projection:"), 1, flag=wx.ALIGN_RIGHT, row=5, col=2)
        self.sizer.Add(self.MakeLabel("North:"), 1, flag=wx.ALIGN_RIGHT, row=6, col=2)
        self.sizer.Add(self.MakeLabel("South:"), 1, flag=wx.ALIGN_RIGHT, row=7, col=2)
        self.sizer.Add(self.MakeLabel("East:"), 1, flag=wx.ALIGN_RIGHT, row=8, col=2)
        self.sizer.Add(self.MakeLabel("West:"), 1, flag=wx.ALIGN_RIGHT, row=9, col=2)
        self.sizer.Add(self.MakeLabel("Resolution:"), 1, flag=wx.ALIGN_RIGHT, row=10, col=2)
        self.sizer.Add(self.MakeLabel("Rows:"), 1, flag=wx.ALIGN_RIGHT, row=12, col=2)
        self.sizer.Add(self.MakeLabel("Columns:"), 1, flag=wx.ALIGN_RIGHT, row=13, col=2)
        self.sizer.Add(self.MakeLabel("Cells:"), 1, flag=wx.ALIGN_RIGHT, row=14, col=2)

    def FillVars(self,event=None):
        database = self.parent.startpage.tgisdbase.GetValue()
        location = self.parent.startpage.tlocation.GetValue()
        projection = self.parent.csystemspage.cs
        #zoone = self.pages
        north = self.parent.bboxpage.ttop.GetValue()
        south = self.parent.bboxpage.tbottom.GetValue()
        east  = self.parent.bboxpage.tright.GetValue()
        west  = self.parent.bboxpage.tleft.GetValue()
        res =   self.parent.bboxpage.tres.GetValue()
        #if projection != "latlong":
        rows = int(round((float(north)-float(south))/float(res)))
        cols = int(round((float(east)-float(west))/float(res)))
        cells = int(rows*cols)

        self.sizer.Add(self.MakeLabel(database), 1, flag=wx.ALIGN_LEFT, row=1, col=3)
        self.sizer.Add(self.MakeLabel(location), 1, flag=wx.ALIGN_LEFT, row=4, col=3)
        self.sizer.Add(self.MakeLabel(projection), 1, flag=wx.ALIGN_LEFT, row=5, col=3)
        self.sizer.Add(self.MakeLabel(north), 1, flag=wx.ALIGN_LEFT, row=6, col=3)
        self.sizer.Add(self.MakeLabel(south), 1, flag=wx.ALIGN_LEFT, row=7, col=3)
        self.sizer.Add(self.MakeLabel(east), 1, flag=wx.ALIGN_LEFT, row=8, col=3)
        self.sizer.Add(self.MakeLabel(west), 1, flag=wx.ALIGN_LEFT, row=9, col=3)
        self.sizer.Add(self.MakeLabel(res), 1, flag=wx.ALIGN_LEFT, row=10, col=3)
        self.sizer.Add(self.MakeLabel(str(rows)), 1, flag=wx.ALIGN_LEFT, row=12, col=3)
        self.sizer.Add(self.MakeLabel(str(cols)), 1, flag=wx.ALIGN_LEFT, row=13, col=3)
        self.sizer.Add(self.MakeLabel(str(cells)), 1, flag=wx.ALIGN_LEFT, row=14, col=3)

#projection: 99 (Other Projection)
#zone: 0
#  north:       344444
#  south:       3333
#  east:        3333333
#  west:        33333
#
#  e-w res:     30
#  n-s res:     30.00096746  (Changed to conform to grid)
#
#total rows: 11370
#total cols: 110000
#total cells: 1,250,700,000
#
#
#Do you accept this region? (y/n) [n] >


        # inputs

class BBoxPage(TitledPage):
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, "Set default region")

        self.parent = parent
        # inputs
        self.ttop = self.MakeTextCtrl("1")
        self.tbottom = self.MakeTextCtrl("0")
        self.tleft = self.MakeTextCtrl("0")
        self.tright = self.MakeTextCtrl("1")
        self.tres = self.MakeTextCtrl("1")
        self.tfile = self.MakeTextCtrl("")

        # buttons
        self.bbrowse = self.MakeButton("Browse ...")

        # list of states
        self.states = []
        self.coords = []
        try:
            f = open(os.path.join(os.getenv("GISBASE"),"etc","wx","states.txt"),"r")
            for line in f.readlines():
                if line[0] == "#":
                    continue
                state,coord = line.split(";")
                coord = coord.replace(","," ")
                self.states.append(state)
                self.coords.append(coord.split())
            f.close()
        except:
            pass
        self.cstate = wx.ComboBox(self, -1, pos=(50, 170), size=(150, -1),
                choices=self.states, style=wx.CB_READONLY)

        # layout
        self.sizer.Add(self.MakeLabel("North"), 0, wx.ALIGN_RIGHT, row=1,col=2)
        self.sizer.Add(self.ttop, 0, wx.ALIGN_LEFT, row=1,col=3)

        self.sizer.Add(self.MakeLabel("West"), 0, wx.ALIGN_RIGHT, row=2,col=1)
        self.sizer.Add(self.tleft, 0, wx.ALIGN_LEFT,  row=2,col=2)
        self.sizer.Add(self.MakeLabel("East"), 0, wx.ALIGN_RIGHT, row=2,col=4)
        self.sizer.Add(self.tright, 0, wx.ALIGN_LEFT,  row=2,col=5)

        self.sizer.Add(self.MakeLabel("South"), 0, wx.ALIGN_RIGHT, row=3,col=2)
        self.sizer.Add(self.tbottom, 0, wx.ALIGN_LEFT, row=3,col=3)

        self.sizer.Add(self.MakeLabel("Initial resolution"), 0, wx.ALIGN_RIGHT, row=4,col=2)
        self.sizer.Add(self.tres, 0, wx.ALIGN_LEFT, row=4,col=3)

        self.sizer.Add(wx.StaticLine(self, -1), 0, wx.EXPAND|wx.ALL, 0, row=5, col=1, colspan=5)
        self.sizer.Add(self.MakeLabel("Use georeferenced file"), 3, wx.ALIGN_RIGHT, row=6,col=2)
        self.sizer.Add(self.tfile, 0, wx.ALIGN_CENTER_VERTICAL, row=6,col=3)
        self.sizer.Add(self.bbrowse, 0, wx.ALIGN_LEFT, row=6,col=4)
        self.sizer.Add(wx.StaticLine(self, -1), 0, wx.EXPAND|wx.ALL, 0, row=7, col=1, colspan=5)
        self.sizer.Add(self.MakeLabel("Set exntent according to selected country"), 3, wx.ALIGN_RIGHT, row=8,col=2)
        self.sizer.Add(self.cstate, 0, wx.ALIGN_LEFT, row=8,col=3)

        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.OnWizPageChange)
        self.Bind(wx.EVT_COMBOBOX, self.OnItemSelected, self.cstate)

    def OnWizPageChange(self, event):
        self.GetNext().FillVars()

    def OnItemSelected(self, event):
        item = event.GetSelection()
        w,s,e,n = self.coords[item]
        #  4 
        # 1 3
        #  2

        if self.parent.csystemspage.cs == "latlong":
            pass
        else:
            n = 100
            s = 00
            e = 100
            w = 0

        self.ttop.SetValue( str(n) )
        self.tbottom.SetValue( str(s) ) 
        self.tright.SetValue( str(e) )
        self.tleft.SetValue( str(w) ) 

class ProjectionsPage(TitledPage):
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, "Choose projection name")

        self.parent = parent
        # text input
        self.tproj = self.MakeTextCtrl("")

        # search box
        self.searchb = wx.SearchCtrl(self, size=(200,-1), style=wx.TE_PROCESS_ENTER)

        # table
        self.tablewidth=600
        self.list = wx.ListCtrl(self, -1, style=wx.LC_REPORT|  wx.LC_VRULES | wx.LC_HRULES, size=(700,100))
        self.list.InsertColumn(0, 'Name')
        self.list.InsertColumn(1, '                        Description                     ')
        self.list.SetColumnWidth(0, 50)
        self.list.SetColumnWidth(1, wx.LIST_AUTOSIZE_USEHEADER)

        # laout
        self.sizer.Add(self.MakeLabel("Projection name:"), 0, row=1, col=2)
        self.sizer.Add(self.tproj, 0, wx.ALIGN_LEFT, 1, row=1, col=3)

        self.sizer.Add(self.MakeLabel("Search in projection description"), 0, 1, row=2, col=2)
        self.sizer.Add(self.searchb, 0, wx.ALIGN_LEFT,1, row=2, col=3)

        self.sizer.Add(self.list, wx.EXPAND,  1, row=3, col=1, colspan=5)

        # events
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected, self.list)
        self.searchb.Bind(wx.EVT_TEXT_ENTER, self.OnDoSearch, self.searchb)

        self._onBrowseDatums(None, None)

    def OnDoSearch(self,event):
        str =  self.searchb.GetValue()
        listItem  = self.list.GetColumn(1)

        for i in range(self.list.GetItemCount()):
            listItem = self.list.GetItem(i,1)
            if listItem.GetText().find(str) > -1:
                datumcode = self.list.GetItem(i, 0)
                self.tproj.SetValue(datumcode.GetText())
                break

        self._onBrowseDatums(None,str)
        

    def OnItemSelected(self,event):
        item = event.GetItem()
        self.tproj.SetValue(str(item.GetText()))

    
    def _onBrowseDatums(self,event,search=None):
        try:
            self.list.DeleteAllItems()
            f = open(os.path.join(os.getenv("GISBASE"), "etc","projections"),"r")
            i=1
            j = 0
            descr = None
            proj = None
            for line in f.readlines():
                line = line.strip()
                if not line:
                    continue
                if line[0] == "#":
                    continue
                proj,descr = string.split(line, ":", maxsplit=1)
                if search and (descr.lower().find(search.lower()) > -1 or\
                              proj.lower().find(search.lower()) > -1) or\
                        not search:
                    self.list.InsertStringItem(j,proj)
                    self.list.SetStringItem(j,1,descr)
                    j  += 1
                    # reset 
                    descr = None; proj = ""
            f.close()
            self.SendSizeEvent()
        except StandardError, e:
            dlg = wx.MessageDialog(self, "Could not read datums: %s "
                    % e,"Can not read file",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()

    def OnChange(self,event):
            self.item =  event.GetItem()



class GeoreferencedFilePage(TitledPage):
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, "Choose projection name")

        self.tfile = self.MakeTextCtrl(size=(150,20))

        self.bbrowse = self.MakeButton("Browse ...")

        self.sizer.Add(self.MakeLabel("Georeferenced file:"), 1, row=1, col=2)
        self.sizer.Add(self.tfile, 0, wx.ALIGN_LEFT, 1, row=1, col=3)
        self.sizer.Add(self.bbrowse, 0, wx.ALIGN_LEFT, 1, row=1, col=4)

        wx.EVT_BUTTON(self, self.bbrowse.GetId(), self.OnBrowse)

    def OnBrowse(self, event): 
        
        dlg = wx.FileDialog(self, "Choose a georeferenced file:", os.getcwd(), "", "*.*", wx.OPEN)
        if dlg.ShowModal() == wx.ID_OK:
                    path = dlg.GetPath()
                    self.tfile.SetValue(path)
        dlg.Destroy()

    def OnCreate(self, event):
        pass
    #    if not os.path.isfile(self.tfile.GetValue()):
    #        dlg = wx.MessageDialog(self, "Could not create new location: %s not file"
    #                % self.tfile.GetValue(),"Can not create location",  wx.OK|wx.ICON_INFORMATION)
    #        dlg.ShowModal()
    #        dlg.Destroy()
    #        return

    #    if not self.tname.GetValue():
    #        dlg = wx.MessageDialog(self, "Could not create new location: name not set",
    #                "Can not create location",  wx.OK|wx.ICON_INFORMATION)
    #        dlg.ShowModal()
    #        dlg.Destroy()
    #        return

    #    if os.path.isdir(os.path.join(self.parent.gisdbase,self.tname.GetValue())):
    #        dlg = wx.MessageDialog(self, "Could not create new location: %s exists"
    #                % os.path.join(self.parent.gisdbase,self.tname.GetValue()),"Can not create location",  wx.OK|wx.ICON_INFORMATION)
    #        dlg.ShowModal()
    #        dlg.Destroy()
    #        return

    #    # creating location
    #    # all credit to Michael Barton and his file_option.tcl and
    #    # Markus Neteler
    #    try:
    #        # FIXME: this does not need to work on windows
    #        os.system("g.proj -c georef=%s location=%s >&2" % (self.tfile.GetValue(), self.tname.GetValue())) 

    #        self.parent.OnSetDatabase(None)
    #        self.Destroy()

    #    except StandardError, e:
    #        dlg = wx.MessageDialog(self, "Could not create new location: %s "
    #                % str(e),"Can not create location",  wx.OK|wx.ICON_INFORMATION)
    #        dlg.ShowModal()
    #        dlg.Destroy()

class EPSGPage(TitledPage):
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, "Choose EPSG Code")

        # text input
        self.tfile = self.MakeTextCtrl("/usr/share/proj/epsg", size=(150,20))
        self.tcode = self.MakeTextCtrl("")

        # buttons
        self.bbrowse = self.MakeButton("Browse ...")
        self.bbcodes = self.MakeButton("Browse Codes")

        # search box
        self.searchb = wx.SearchCtrl(self, size=(200,-1), style=wx.TE_PROCESS_ENTER)

        # table
        self.tablewidth=600
        self.epsgs = wx.ListCtrl(self, -1, style=wx.LC_REPORT|  wx.LC_VRULES | wx.LC_HRULES,
                size=(700,100))
        self.epsgs.InsertColumn(0, 'EPSG')
        self.epsgs.InsertColumn(1, '                        Description                     ')
        self.epsgs.InsertColumn(2, '                                            Parameters                                            ')
        self.epsgs.SetColumnWidth(0, 50)
        self.epsgs.SetColumnWidth(1, wx.LIST_AUTOSIZE_USEHEADER)
        self.epsgs.SetColumnWidth(2, wx.LIST_AUTOSIZE_USEHEADER)
        
        # laout
        self.sizer.Add(self.MakeLabel("Path to the EPSG-codes file:"), 1, col=1, row=1)
        self.sizer.Add(self.tfile, 0 , wx.ALIGN_LEFT, 1, row=1, col=2)
        self.sizer.Add(self.bbrowse, 0 , wx.ALIGN_CENTER_HORIZONTAL, 1, row=1, col=3)

        self.sizer.Add(self.MakeLabel("EPSG code:"), 0, col=1, row=2)
        self.sizer.Add(self.tcode, 0, wx.ALIGN_LEFT,1, row=2, col=2)

        self.sizer.Add(self.MakeLabel("Search in code description:"), 0, col=1, row=3)
        self.sizer.Add(self.searchb, 0, wx.ALIGN_LEFT,1, row=3, col=2)
        self.sizer.Add(self.bbcodes, 0 , wx.ALIGN_CENTER_HORIZONTAL, row=3, col=3)

        self.sizer.Add(self.epsgs, wx.EXPAND,  0, row=4, col=1, colspan=5)

        # events
        wx.EVT_BUTTON(self, self.bbrowse.GetId(), self.OnBrowse)
        wx.EVT_BUTTON(self, self.bbcodes.GetId(), self.OnBrowseCodes)
        self.Bind(wx.EVT_LIST_ITEM_SELECTED, self.OnItemSelected, self.epsgs)
        self.searchb.Bind(wx.EVT_TEXT_ENTER, self.OnDoSearch, self.searchb)

    def OnDoSearch(self,event):
        str =  self.searchb.GetValue()
        listItem  = self.epsgs.GetColumn(1)

        for i in range(self.epsgs.GetItemCount()):
            listItem = self.epsgs.GetItem(i,1)
            if listItem.GetText().find(str) > -1:
                epsgcode = self.epsgs.GetItem(i, 0)
                self.tcode.SetValue(epsgcode.GetText())
                break

        self.OnBrowseCodes(None,str)
        

    def OnBrowse(self, event): 
        
        dlg = wx.FileDialog(self, "Choose a georeferenced file:",
        "/", "", "*.*", wx.OPEN)
        if dlg.ShowModal() == wx.ID_OK:
                    path = dlg.GetPath()
                    self.tfile.SetValue(path)
        dlg.Destroy()

    def OnItemSelected(self,event):
        item = event.GetItem()
        self.tcode.SetValue(str(item.GetText()))

    
    def OnBrowseCodes(self,event,search=None):
        try:
            self.epsgs.DeleteAllItems()
            f = open(self.tfile.GetValue(),"r")
            i=1
            j = 0
            descr = None
            code = None
            params = ""
            #self.epsgs.ClearAll()
            for line in f.readlines():
                line = line.strip()
                if line.find("#") == 0:
                    descr= line[1:].strip()
                elif line.find("<") == 0:
                    code = line.split(" ")[0]
                    for par in line.split(" ")[1:]:
                        params += par + " "
                    code = code[1:-1]
                if i%2 == 0:
                    if search and descr.lower().find(search.lower()) > -1 or\
                        not search:
                        self.epsgs.InsertStringItem(j,str(code))
                        self.epsgs.SetStringItem(j, 1, str(descr))
                        self.epsgs.SetStringItem(j, 2, str(params))
                        j  += 1
                    # reset 
                    descr = None; code = None; params = ""
                if i%2 == 0:
                    self.epsgs.SetItemBackgroundColour(i, "grey")
                i += 1
            f.close()
            self.SendSizeEvent()
        except StandardError, e:
            dlg = wx.MessageDialog(self, "Could not read EPGS codes: %s "
                    % e,"Can not read file",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()

    def OnChange(self,event):
            self.item =  event.GetItem()

    #def OnCreate(self, event):
    #    if not self.tcode.GetValue():
    #        dlg = wx.MessageDialog(self, "Could not create new location: EPSG Code value missing",
    #                "Can not create location",  wx.OK|wx.ICON_INFORMATION)
    #        dlg.ShowModal()
    #        dlg.Destroy()
    #        return
    #    
    #    number = -1
    #    try:
    #        number = int(self.tcode.GetValue())
    #    except:
    #        dlg = wx.MessageDialog(self, "Could not create new location: '%s' not a number" % self.tcode.GetValue(),
    #                "Can not create location",  wx.OK|wx.ICON_INFORMATION)
    #        dlg.ShowModal()
    #        dlg.Destroy()
    #        return
    #         
    #    if os.path.isdir(os.path.join(self.parent.gisdbase,self.tname.GetValue())):
    #        dlg = wx.MessageDialog(self, "Could not create new location: %s exists"
    #                % os.path.join(self.parent.gisdbase,self.tname.GetValue()),"Can not create location",  wx.OK|wx.ICON_INFORMATION)
    #        dlg.ShowModal()
    #        dlg.Destroy()
    #        return
    #    
    #    # creating location
    #    # all credit to Michael Barton and his file_option.tcl and
    #    # Markus Neteler
    #    try:
    #        # FIXME: this does not need to work on windows
    #        os.system("g.proj -c georef=%s location=%s >&2" % (self.tfile.GetValue(), self.tname.GetValue())) 
    #        datumtrans = os.popen(" g.proj epsg=%d datumtrans=-1 >&2" % (number)).readlines()

    #        if datumtrans:
    #            #os.system(" g.proj epsg=%d datumtrans=%s >&2" % (number,datumtrans[0]).readlines()
    #            pass
    #        else:
    #            os.system("g.proj -c epsg=%d location=%s datumtrans=1" % (number, self.tname.GetValue()))

    #            self.parent.OnSetDatabase(None)
    #            self.Destroy()

    #    except StandardError, e:
    #        dlg = wx.MessageDialog(self, "Could not create new location: %s "
    #                % str(e),"Can not create location",  wx.OK|wx.ICON_INFORMATION)
    #        dlg.ShowModal()
    #        dlg.Destroy()
    #        return

 
    def OnDoubleClick(self, event):
        print self.epsgs.GetValue()
        pass


class CoordinateSystemPage(TitledPage):
    def __init__(self, wizard, parent):
        TitledPage.__init__(self, wizard, "Choose coordinate system for location")

        self.parent = parent 
        self.cs = "xy"

        # toggles
        self.radio1 = wx.RadioButton( self, -1, " XY ", style = wx.RB_GROUP )
        self.radio2 = wx.RadioButton( self, -1, " Lat/Long " )
        self.radio3 = wx.RadioButton( self, -1, " UTM " )
        self.radio4 = wx.RadioButton( self, -1, " Custom " )
        self.radio5 = wx.RadioButton( self, -1, " EPSG " )
        self.radio6 = wx.RadioButton( self, -1, " Based on Georeferenced file " )

        # layout
        self.sizer.Add(self.radio1, 0, wx.ALIGN_LEFT, row=1, col=2)
        self.sizer.Add(self.radio2, 0, wx.ALIGN_LEFT, row=2, col=2)
        self.sizer.Add(self.radio3, 0, wx.ALIGN_LEFT, row=3, col=2)
        self.sizer.Add(self.radio4, 0, wx.ALIGN_LEFT, row=4, col=2)
        self.sizer.Add(self.radio5, 0, wx.ALIGN_LEFT, row=5, col=2)
        self.sizer.Add(self.radio6, 0, wx.ALIGN_LEFT, row=6, col=2)
        
        # bindings
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio1.GetId())
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio2.GetId())
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio3.GetId())
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio4.GetId())
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio5.GetId())
        self.Bind(wx.EVT_RADIOBUTTON, self.SetVal, id=self.radio6.GetId())
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.OnWizPageChange)

    def SetVal(self,event):
        if event.GetId() == self.radio1.GetId():
            self.cs = "xy"
            self.SetNext(self.parent.bboxpage)
            self.parent.bboxpage.cstate.Enable(False)
        elif event.GetId() == self.radio2.GetId():
            self.cs = "latlong"
            self.SetNext(self.parent.datumpage)
            self.parent.datumpage.SetPrev(self.parent.csystemspage)
            self.parent.bboxpage.SetPrev(self.parent.datumpage)
        elif event.GetId() == self.radio3.GetId():
            self.cs = "utm"
            self.SetNext(self.parent.datumpage)
            self.parent.datumpage.SetPrev(self.parent.csystemspage)
            self.parent.bboxpage.SetPrev(self.parent.datumpage)
        elif event.GetId() == self.radio4.GetId():
            self.cs = "custom"
            self.SetNext(self.parent.projpage)
            self.parent.datumpage.SetPrev(self.parent.projpage)
            self.parent.bboxpage.SetPrev(self.parent.datumpage)
        elif event.GetId() == self.radio5.GetId():
            self.cs = "epsg"
            self.SetNext(self.parent.epsgpage)
            self.parent.datumpage.SetPrev(self.parent.epsgpage)
            self.parent.bboxpage.SetPrev(self.parent.datumpage)
        elif event.GetId() == self.radio6.GetId():
            self.SetNext(self.parent.filepage)
            self.cs = "file"

    def OnWizPageChange(self,event=None):
        if self.cs == "xy":
            self.parent.bboxpage.cstate.Enable(False)
        else:
            self.parent.bboxpage.cstate.Enable(True)
        pass


class DatabasePage(TitledPage):
    def __init__(self, wizard, parent, grassdatabase):
        TitledPage.__init__(self, wizard, "Define GRASS database and new Location Name")

        # buttons
        self.bbrowse = self.MakeButton("Browse ...")

        # text controls
        self.tgisdbase = self.MakeTextCtrl(grassdatabase, size=(300, 20))
        self.tlocation = self.MakeTextCtrl("newLocation")
 
        # layout
        self.sizer.Add(self.MakeLabel("GIS Data Directory:"), 0, wx.ALIGN_RIGHT, row=1, col=2)
        self.sizer.Add(self.tgisdbase,0,wx.ALIGN_LEFT, row=1, col=3)
        self.sizer.Add(self.bbrowse, 0, wx.ALIGN_CENTER_HORIZONTAL, row=1, col=4)
        #
        self.sizer.Add(self.MakeLabel("Project Location\n(projection/coordinate system)"), 0, wx.ALIGN_RIGHT, row=2, col=2)
        self.sizer.Add(self.tlocation,0,wx.ALIGN_LEFT, row=2, col=3)
        
        # bindings
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.OnWizPageChanging)


    def OnWizPageChanging(self,event=None):
        if os.path.isdir(os.path.join(self.tgisdbase.GetValue(),self.tlocation.GetValue())):
            dlg = wx.MessageDialog(self, "Could not create new location: <%s> directory exists "\
                    % str(self.tlocation.GetValue()),"Can not create location",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            event.Veto()
            return

        if not self.tlocation.GetValue():
            dlg = wx.MessageDialog(self, "Could not create new location: not set "\
                    ,"Can not create location",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            event.Veto()
            return

        self.location = self.tlocation.GetValue()
        self.grassdatabase = self.tgisdbase.GetValue()

    def OnWizPageChange(self,event=None):
        self.grassdatabase = self.tgisdbase.GetValue()
        self.location = self.tlocation.GetValue()
        

class GWizard:
    def __init__(self, parent, grassdatabase):
        wizard = wiz.Wizard(parent, -1, "Define new Location")
        self.startpage = DatabasePage(wizard, self, grassdatabase)
        self.csystemspage = CoordinateSystemPage(wizard, self)
        self.epsgpage = EPSGPage(wizard, self)
        self.bboxpage = BBoxPage(wizard, self)
        self.filepage = GeoreferencedFilePage(wizard, self)
        self.projpage = ProjectionsPage(wizard, self)
        self.sumpage = SummaryPage(wizard, self)
        self.datumpage = DatumPage(wizard, self)
        
        # Set the initial order of the pages
        self.startpage.SetNext(self.csystemspage)

        self.csystemspage.SetPrev(self.startpage)
        self.csystemspage.SetNext(self.bboxpage)

        self.epsgpage.SetNext(self.datumpage)
        self.epsgpage.SetPrev(self.csystemspage)

        self.projpage.SetNext(self.datumpage)
        self.projpage.SetPrev(self.csystemspage)

        self.filepage.SetPrev(self.csystemspage)

        self.datumpage.SetNext(self.bboxpage)

        self.bboxpage.SetPrev(self.csystemspage)
        self.bboxpage.SetNext(self.sumpage)

        self.sumpage.SetPrev(self.bboxpage) 

        wizard.FitToPage(self.bboxpage)
        wizard.RunWizard(self.startpage)
        wizard.Destroy()

if __name__ == "__main__":
    gWizard = GWizard(None,  "")
    GRASSStartUp = GWizard.StartUp(0)
    GRASSStartUp.MainLoop()
    #app.MainLoop()
