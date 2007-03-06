import wx
import wx.wizard as wiz
import  wx.lib.rcsizer  as rcs

import os
import sys

class CoordinateSystemPage(wiz.WizardPageSimple):
    def __init__(self, parent):

        wiz.WizardPageSimple.__init__(self, parent)

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
        self.Bind(wiz.EVT_WIZARD_PAGE_CHANGING, self.OnWizPageChanging)


    def OnWizPageChanging(self,event=None):
        pass
        #if os.isdir(os.path.join(self.tgisdbase.GetValue(),self.tlocation.GetValue())):
        #    dlg = wx.MessageDialog(self, "Could not create new location: <%s> directory exists "\
        #            % str(self.tlocation.GetValue()),"Can not create location",  wx.OK|wx.ICON_INFORMATION)
        #    dlg.ShowModal()
        #    dlg.Destroy()
        #    return

    def OnWizPageChange(self,event=None):
        pass
        #self.grassdatabase = self.tgisdbase.GetValue()
        #self.location = self.tlocation.GetValue()
        


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
            return

        if not self.tlocation.GetValue():
            dlg = wx.MessageDialog(self, "Could not create new location: not set "\
                    ,"Can not create location",  wx.OK|wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            return


    def OnWizPageChange(self,event=None):
        self.grassdatabase = self.tgisdbase.GetValue()
        self.location = self.tlocation.GetValue()
        

class GWizard:
    def __init__(self, parent, grassdatabase):
        wizard = wiz.Wizard(parent, -1, "Define new Location")
        page1 = DatabasePage(wizard, grassdatabase)

        page2 = CoordinateSystemPage(wizard)
        wiz.WizardPageSimple_Chain(page1, page2)
        wizard.FitToPage(page1)
        wizard.RunWizard(page1)
        wizard.Destroy()

if __name__ == "__main__":
    gWizard = GWizard(None,  "")
    GRASSStartUp = GWizard.StartUp(0)
    GRASSStartUp.MainLoop()
    #app.MainLoop()
