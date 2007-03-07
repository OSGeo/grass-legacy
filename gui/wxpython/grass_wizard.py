import wx
import wx.wizard as wiz
import  wx.lib.rcsizer  as rcs

import os
import sys

# Selected coordinate system
# Possible values: xy, latlong, utm, epsg, file, custom

class BBoxPage(wiz.WizardPageSimple):
    def __init__(self, parent):

        wiz.WizardPageSimple.__init__(self, parent)

        # text
        self.title = wx.StaticText(self, -1, "Default region")
        self.title.SetFont(wx.Font(13,wx.SWISS, wx.NORMAL, wx.BOLD))
        self.ltop = wx.StaticText(self, -1, "North")
        self.lbottom = wx.StaticText(self, -1, "South")
        self.lleft = wx.StaticText(self, -1, "West")
        self.lright = wx.StaticText(self, -1, "East")
        self.lres = wx.StaticText(self, -1, "Resolution")
        self.lfile = wx.StaticText(self, -1, "Take bbox from georeferenced file")
        self.lstate = wx.StaticText(self, -1, "Select BBox by country")

        # inputs
        self.ttop = wx.TextCtrl(self, -1, "1", size=(200, 20), style=wx.TE_LEFT)
        self.tbottom = wx.TextCtrl(self, -1, "0", size=(200, 20), style=wx.TE_LEFT)
        self.tleft = wx.TextCtrl(self, -1, "0", size=(200, 20), style=wx.TE_LEFT)
        self.tright = wx.TextCtrl(self, -1, "1", size=(200, 20), style=wx.TE_LEFT)
        self.tres = wx.TextCtrl(self, -1, "1", size=(200, 20), style=wx.TE_LEFT)
        self.tfile = wx.TextCtrl(self, -1, "", size=(200, 20), style=wx.TE_LEFT)

        # buttons
        self.bbrowse = wx.Button(self, -1, "Browse ...", size=(95,30))

        # list of states
        self.states = []
        self.coords = []
        try:
            f = open(os.path.join(os.getenv("GISBASE"),"etc","wx","states.txt"),"r")
            for line in f.readlines():
                state,coord = line.split(";")
                self.states.append(state)
                self.coords.append(coord.split())
            f.close()
        except:
            pass
        self.cstate = wx.ComboBox(self, -1, pos=(50, 170), size=(150, -1),
                choices=self.states, style=wx.CB_READONLY)

        # layout
        sizer = rcs.RowColSizer()
        sizer.Add(self.title, 0, wx.ALIGN_CENTER_HORIZONTAL,  row=1, col=1, colspan=5)

        sizer.Add(self.ltop, 0, wx.ALIGN_RIGHT, row=2,col=2)
        sizer.Add(self.ttop, 0, wx.ALIGN_LEFT, row=2,col=3)

        sizer.Add(self.lleft, 0, wx.ALIGN_RIGHT, row=3,col=1)
        sizer.Add(self.tleft, 0, wx.ALIGN_LEFT,  row=3,col=2)
        sizer.Add(self.lright, 0, wx.ALIGN_RIGHT, row=3,col=4)
        sizer.Add(self.tright, 0, wx.ALIGN_LEFT,  row=3,col=5)

        sizer.Add(self.lbottom, 0, wx.ALIGN_RIGHT, row=4,col=2)
        sizer.Add(self.tbottom, 0, wx.ALIGN_LEFT, row=4,col=3)

        sizer.Add(self.lres, 0, wx.ALIGN_RIGHT, row=5,col=2)
        sizer.Add(self.tres, 0, wx.ALIGN_RIGHT, row=5,col=3)

        sizer.Add(self.lfile, 0, wx.ALIGN_RIGHT, row=6,col=2)
        sizer.Add(self.tfile, 0, wx.ALIGN_LEFT, row=6,col=3)
        sizer.Add(self.bbrowse, 0, wx.ALIGN_LEFT, row=6,col=4)

        sizer.Add(self.lstate, 0, wx.ALIGN_RIGHT, row=7,col=2)
        sizer.Add(self.cstate, 0, wx.ALIGN_LEFT, row=7,col=3)

        self.SetSizer(sizer)
        self.SetAutoLayout(True)

class CoordinateSystemPage(wiz.WizardPageSimple):
    def __init__(self, parent):

        wiz.WizardPageSimple.__init__(self, parent)

        self.cs = "xy"

        # variables
        self.location = None
        self.grassdatabase = None

        # text
        self.title = wx.StaticText(self, -1, "Coordinate system for location ")
        self.title.SetFont(wx.Font(13,wx.SWISS, wx.NORMAL, wx.BOLD))

        # toggles
        self.radio1 = wx.RadioButton( self, -1, " XY ", style = wx.RB_GROUP )
        self.radio2 = wx.RadioButton( self, -1, " Lat/Long " )
        self.radio3 = wx.RadioButton( self, -1, " UTM " )
        self.radio4 = wx.RadioButton( self, -1, " Custom " )
        self.radio5 = wx.RadioButton( self, -1, " EPSG " )
        self.radio6 = wx.RadioButton( self, -1, " Based on Georeferenced file " )

        # layout
        sizer = rcs.RowColSizer()
        sizer.Add(self.title, 0, wx.ALIGN_CENTER_HORIZONTAL,  row=1, col=1)
        #
        sizer.Add(self.radio1, 0, wx.ALIGN_LEFT, row=2, col=1)
        sizer.Add(self.radio2, 0, wx.ALIGN_LEFT, row=3, col=1)
        sizer.Add(self.radio3, 0, wx.ALIGN_LEFT, row=4, col=1)
        sizer.Add(self.radio4, 0, wx.ALIGN_LEFT, row=5, col=1)
        sizer.Add(self.radio5, 0, wx.ALIGN_LEFT, row=6, col=1)
        sizer.Add(self.radio6, 0, wx.ALIGN_LEFT, row=7, col=1)
        #
        self.SetSizer(sizer)
        self.SetAutoLayout(True)
        
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
        elif event.GetId() == self.radio2.GetId():
            self.cs = "latlong"
        elif event.GetId() == self.radio2.GetId():
            self.cs = "utm"
        elif event.GetId() == self.radio2.GetId():
            self.cs = "custom"
        elif event.GetId() == self.radio2.GetId():
            self.cs = "epsg"
        elif event.GetId() == self.radio2.GetId():
            self.cs = "file"


    def OnWizPageChange(self,event=None):
        next = self.GetNext()
        if self.cs == "xy" or self.radio1.GetValue():
            next.cstate.Enable(False)
        else:
            next.cstate.Enable(True)


class DatabasePage(wiz.WizardPageSimple):
    def __init__(self, parent, grassdatabase):

        wiz.WizardPageSimple.__init__(self, parent)

        # variables
        self.location = None
        self.grassdatabase = None

        # text
        self.title = wx.StaticText(self, -1, "Define GRASS database and new Location Name")
        self.title.SetFont(wx.Font(13,wx.SWISS, wx.NORMAL, wx.BOLD))
        self.ldbase = wx.StaticText(self, -1, "GIS Data Directory:")
        self.llocation = wx.StaticText(self, -1, "Project Location\n(projection/coordinate system)", style=wx.ALIGN_CENTRE)

        # buttons
        self.bbrowse = wx.Button(self, -1, "Browse ...", size=(95,30))

        # panels
        self.panel = wx.Panel(self,-1)

        # text controls
        self.tgisdbase = wx.TextCtrl(self, -1, grassdatabase, size=(300, 20),
                style=wx.TE_LEFT)
        self.tlocation = wx.TextCtrl(self,-1, "", size=(150,20))
 
        # layout
        sizer = rcs.RowColSizer()
        sizer.Add(self.title, 0, wx.ALIGN_CENTER_HORIZONTAL,  row=1, col=1, colspan=3)
        #
        sizer.Add(self.ldbase, 0, wx.ALIGN_RIGHT, row=2, col=1)
        sizer.Add(self.tgisdbase,0,wx.ALIGN_LEFT, row=2, col=2)
        sizer.Add(self.bbrowse, 0, wx.ALIGN_CENTER_HORIZONTAL, row=2, col=3)
        #
        sizer.Add(self.llocation, 0, wx.ALIGN_RIGHT, row=3, col=1)
        sizer.Add(self.tlocation,0,wx.ALIGN_LEFT, row=3, col=2)
        #
        self.SetSizer(sizer)
        self.SetAutoLayout(True)
        
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
        page1 = DatabasePage(wizard, grassdatabase)
        page2 = CoordinateSystemPage(wizard)
        page3 = BBoxPage(wizard)

        # Set the initial order of the pages
        page1.SetNext(page2)
        page2.SetPrev(page1)
        page2.SetNext(page3)
        page3.SetPrev(page2)
        #page3.SetNext(page4)
        #page4.SetPrev(page3)
        #page4.SetNext(page5)
        #page5.SetPrev(page4)

        wizard.FitToPage(page1)
        wizard.RunWizard(page1)
        wizard.Destroy()

if __name__ == "__main__":
    gWizard = GWizard(None,  "")
    GRASSStartUp = GWizard.StartUp(0)
    GRASSStartUp.MainLoop()
    #app.MainLoop()
