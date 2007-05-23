import wx
import wx.aui
import os, sys, time, glob, math
try:
    import wx.lib.plot as plot
except:
    msg= """
    This module requires the Numeric/numarray or NumPy module,
    which could not be imported.  It probably is not installed
    (it's not part of the standard Python distribution). See the
    Numeric Python site (http://numpy.scipy.org) for information on
    downloading source or binaries."""
    print >> sys.stderr, "profile.py: " + msg
    #raise ImportError, "Numeric,numarray or NumPy not found. \n" + msg

#Needs Numeric or numarray or NumPy
#try:
#    import numpy.oldnumeric as _Numeric
#except:
#    try:
#        import numarray as _Numeric     #if numarray is used it is renamed Numeric
#    except:
#        try:
#            import Numeric as _Numeric
#        except:
#            msg= """
#            This module requires the Numeric/numarray or NumPy module,
#            which could not be imported.  It probably is not installed
#            (it's not part of the standard Python distribution). See the
#            Numeric Python site (http://numpy.scipy.org) for information on
#            downloading source or binaries."""
#            raise ImportError, "Numeric,numarray or NumPy not found. \n" + msg

from threading import Thread

try:
    import subprocess
except:
    CompatPath = os.path.join( os.getenv("GISBASE"),"etc","wx")
    sys.path.append(CompatPath)
    from compat import subprocess as subprocess

gmpath = os.getenv("GISBASE") + "/etc/wx/gui_modules/"
sys.path.append(gmpath)
gmpath = os.getenv("GISBASE") + "/etc/wx/icons/"
sys.path.append(gmpath)

import render
import menuform
import disp_print
import select
import cmd
import gui_modules.defaultfont as defaultfont
from debug import Debug as Debug
from icon import Icons as Icons

import images
imagepath = images.__path__[0]
sys.path.append(imagepath)

icons = ""

if not os.getenv("GRASS_ICONPATH"):
    icons = os.getenv("GISBASE") + "/etc/gui/icons/"
else:
    icons = os.environ["GRASS_ICONPATH"]

class ProfileFrame(wx.Frame):
    """
    Main frame profile of raster map. Uses wx.lib.plot.
    """

    def __init__(self, parent=None, id = wx.ID_ANY, title="Profile of transect in raster map",
                 pos=wx.DefaultPosition, size=wx.DefaultSize,
                 style=wx.DEFAULT_FRAME_STYLE):

        wx.Frame.__init__(self, parent, id, title, pos, size, style)

        toolbar = self.__createToolBar()

        self.Map = render.Map()  # instance of render.Map to be associated with display

        #
        # Set the size & cursor
        #
        self.SetClientSize(size)
        self.iconsize = (16, 16)

        #
        # Add statusbar
        #
        self.rast = ''
        self.statusbar = self.CreateStatusBar(number=2, style=0)
        self.statusbar.SetStatusWidths([-2, -1])
        profile_frame_statusbar_fields = ["Profiling %s" % self.rast]
        for i in range(len(profile_frame_statusbar_fields)):
            self.statusbar.SetStatusText(profile_frame_statusbar_fields[i], i)

        # Init map display
        self.InitDisplay() # initialize region values

        # initialize buffered DC
#        self.ProfileWindow = BufferedWindow(self, id = wx.ID_ANY, Map=self.Map) # initialize buffered DC

        # Bind various events
        self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)


        self.mapwin = self.Parent.MapWindow
        self.mapwin.Bind(wx.EVT_MOUSE_EVENTS, self.mapwin.MouseActions)

        # plot canvas settings
        self.client = plot.PlotCanvas(self)
        #define the function for drawing pointLabels
        self.client.SetPointLabelFunc(self.DrawPointLabel)
        # Create mouse event for showing cursor coords in status bar
        self.client.canvas.Bind(wx.EVT_LEFT_DOWN, self.OnMouseLeftDown)
        # Show closest point when enabled
        self.client.canvas.Bind(wx.EVT_MOTION, self.OnMotion)

        # Init print module and classes
#        self.printopt = disp_print.PrintOptions(self, self.ProfileWindow)

        # Init variables
        self.seglist = [] # segment endpoint list
        self.datalist = [] # profile data list
        self.pline = '' # profile line data
        self.ppoints = '' # segment endpoints data
        self.profile = None # plot draw object
        self.ptitle = 'Profile of %s' % self.rast
        self.xlabel = "Distance"
        self.ylabel = "Raster values"
        self.font = wx.Font(12,wx.FONTFAMILY_SWISS,wx.FONTSTYLE_NORMAL,wx.FONTWEIGHT_NORMAL)
        self.titlefontsize = 14
        self.axisfontsize = 10
        self.legendfontsize = 10
        self.enablegrid = True
        self.gridcolor = wx.Colour(200,200,200)
        self.enablelegend = True

    def __createToolBar(self):
        """Creates toolbar"""

        toolbar = self.CreateToolBar()
        for each in self.toolbarData():
            self.AddToolbarButton(toolbar, *each)
        toolbar.Realize()

    def AddToolbarButton(self, toolbar, label, icon, help, handler):
        """Adds buttons to the toolbar"""

        if not label:
            toolbar.AddSeparator()
            return
        tool = toolbar.AddLabelTool(id=wx.ID_ANY, label=label, bitmap=icon, shortHelp=help)
        self.Bind(wx.EVT_TOOL, handler, tool)

    def toolbarData(self):

        return   (
                 ('raster', Icons["addrast"].GetBitmap(), Icons["addrast"].GetLabel(), self.SelectRaster),
                 ('transect', Icons["transect"].GetBitmap(), Icons["transect"].GetLabel(), self.DrawTransect),
                 ('profiledraw', Icons["profiledraw"].GetBitmap(), Icons["profiledraw"].GetLabel(), self.CreateProfile),
                 ('options', Icons["font"].GetBitmap(), 'Profile options', self.ProfileOptionsMenu),
                 ('unzoom', Icons['zoom_back'].GetBitmap(), 'Unzoom profile', self.OnRedraw),
                 ('erase', Icons["erase"].GetBitmap(), 'Erase profile', self.OnErase),
                 ('', '', '', ''),
                 ('save',  Icons["savefile"].GetBitmap(),  'Save profile',  self.SaveToFile),
                 ('print',  Icons["printmap"].GetBitmap(),  'Print profile',  self.PrintMenu),
                 ('quit',  wx.ArtProvider.GetBitmap(wx.ART_QUIT, wx.ART_TOOLBAR, (16,16)),  Icons["quit"].GetLabel(), self.OnQuit),
                  )

    def InitDisplay(self):
        """
        Initialize profile display, set dimensions and region
        """
        self.width, self.height = self.GetClientSize()
        self.Map.geom = self.width, self.height

    def SelectRaster(self, event):
        """
        Select raster map to profile (i.e., to get distance,value data)
        """
        dlg = SetRaster(self, wx.ID_ANY, "Select raster to profile",
                          pos=wx.DefaultPosition, size=wx.DefaultSize,
                          style=wx.DEFAULT_DIALOG_STYLE)

        if dlg.ShowModal() == wx.ID_CANCEL:
            dlg.Destroy()
            return

        self.rast = dlg.rast
        self.SetStatusText("Profiling %s" % self.rast)
        self.SetTitle("Profile of transect across %s" % self.rast)
        self.ptitle = 'Profile of %s' % self.rast

    def OnActivate(self, event):
        print "window=",event.GetEventObject()
        event.Skip()

    def DrawTransect(self, event):
        self.seglist = []
        self.datalist = []
        self.pline = ''
        self.ppoints = ''
        self.Parent.SetFocus()
        self.Parent.Raise()
        self.mapwin.mouse['use'] = 'profile'
        self.mapwin.mouse['box'] = 'line'
        self.mapwin.pen = wx.Pen(colour='Red', width=2, style=wx.SHORT_DASH)
        self.mapwin.polypen = wx.Pen(colour='dark green', width=2, style=wx.SHORT_DASH)
        self.mapwin.SetCursor(self.Parent.cursors["cross"])

    def SetGraphStyle(self):
        """
        Set plot and text options
        """
        self.client.SetFont(self.font)
        self.client.SetFontSizeAxis(self.axisfontsize)
        self.client.SetFontSizeLegend(self.legendfontsize)
        self.client.setLogScale((False,False))
        self.client.SetEnableZoom(True)
#        self.client.SetEnableDrag(True)
        self.client.SetEnableGrid(self.enablegrid)
        self.client.SetGridColour(self.gridcolor)
        self.client.SetShowScrollbars(True)
        self.client.SetXSpec('auto')
        self.client.SetYSpec('auto')
        self.client.SetEnableLegend(self.enablelegend)
        self.client.SetFontSizeTitle(self.titlefontsize)

    def DrawPointLabel(self, dc, mDataDict):
        """This is the fuction that defines how the pointLabels are plotted
            dc - DC that will be passed
            mDataDict - Dictionary of data that you want to use for the pointLabel

            As an example I have decided I want a box at the curve point
            with some text information about the curve plotted below.
            Any wxDC method can be used.
        """
        # ----------
        dc.SetPen(wx.Pen(wx.BLACK))
        dc.SetBrush(wx.Brush( wx.BLACK, wx.SOLID ) )

        sx, sy = mDataDict["scaledXY"] #scaled x,y of closest point
        dc.DrawRectangle( sx-5,sy-5, 10, 10)  #10by10 square centered on point
        px,py = mDataDict["pointXY"]
        cNum = mDataDict["curveNum"]
        pntIn = mDataDict["pIndex"]
        legend = mDataDict["legend"]
        #make a string to display
        s = "Crv# %i, '%s', Pt. (%.2f,%.2f), PtInd %i" %(cNum, legend, px, py, pntIn)
        dc.DrawText(s, sx , sy+1)
        # -----------

    def OnMouseLeftDown(self,event):
        s= "Left Mouse Down at Point: (%.4f, %.4f)" % self.client._getXY(event)
        self.SetStatusText(s)
        event.Skip()            #allows plotCanvas OnMouseLeftDown to be called

    def OnMotion(self, event):
        # indicate when mouse is outside the plot area
        if self.client.OnLeave(event): print 'out of area'
        #show closest point (when enbled)
        if self.client.GetEnablePointLabel() == True:
            #make up dict with info for the pointLabel
            #I've decided to mark the closest point on the closest curve
            dlst= self.client.GetClosetPoint( self.client._getXY(event), pointScaled= True)
            if dlst != []:      #returns [] if none
                curveNum, legend, pIndex, pointXY, scaledXY, distance = dlst
                #make up dictionary to pass to my user function (see DrawPointLabel)
                mDataDict= {"curveNum":curveNum, "legend":legend, "pIndex":pIndex,\
                    "pointXY":pointXY, "scaledXY":scaledXY}
                #pass dict to update the pointLabel
                self.client.UpdatePointLabel(mDataDict)
        event.Skip()           #go to next handler

    def CreateProfile(self, event):
        """
        Main routine for creating a profile. Uses r.profile to create a list
        of distance,cell value pairs. This is passed to plot to create a
        line graph of the profile. If the profile transect is in multiple
        segments, these are drawn as points. Profile transect is drawn, using
        methods in mapdisp.py
        """
        if self.rast == '':
            dlg = wx.MessageDialog(self, 'You must select a raster map to profile',
                               'Nothing to profile', wx.OK | wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            return

        self.mapwin.SetCursor(self.Parent.cursors["default"])
        coordstr = ''
        dist = 0
        cumdist = 0
        lasteast = lastnorth = None
        for point in self.mapwin.polycoords:
            # convert screen coordinates to map coordinates for transect
            east, north = self.mapwin.Pixel2Cell(point[0],point[1])

            # build string of coordinate points for r.profile
            if coordstr == '':
                coordstr = '%d,%d' % (east,north)
            else:
                coordstr = '%s,%d,%d' % (coordstr,east,north)

            # get value of raster cell at coordinate point
            cmdlist = ['r.what', 'input=%s' % self.rast, 'east_north=%d,%d' % (east,north)]
            p = cmd.Command(cmdlist)
            output = p.module_stdout.read().strip().split('|')
            val = output[3]

            # calculate distance between coordinate points
            if lasteast and lastnorth:
                 dist = math.sqrt(math.pow((lasteast-east),2) + math.pow((lastnorth-north),2))
            cumdist += dist

            # build a list of distance,value pairs for each segment of transect
            self.seglist.append((cumdist,val))
            lasteast = east
            lastnorth = north

        # delete first and last segment point
        try:
            self.seglist.pop(0)
            self.seglist.pop()
        except:
            pass


        # build a list of distance, value pairs for points along transect
        cmdlist = ['r.profile', 'input=%s' % self.rast, 'profile=%s' % coordstr, 'null=nan']
        p = cmd.Command(cmdlist)
        output = p.module_stdout.read().strip().split('\n')
        for outline in output:
            dist,elev = outline.split(' ')
            self.datalist.append((dist,elev))

        # graph the distance, value pairs for the transect
        self.pline = plot.PolyLine(self.datalist, colour='blue', width=2, legend=' Profile')
        self.ppoints = plot.PolyMarker(self.seglist, legend=' Segment breaks', colour='red', size=2,
                                  fillstyle = wx.TRANSPARENT, marker='circle')

        self.profile = plot.PlotGraphics([self.pline, self.ppoints], self.ptitle, self.xlabel, self.ylabel)

        # this is where we can set plot styles
        self.SetGraphStyle()
        self.client.Draw(self.profile)

        # reset transect
        self.mapwin.polycoords = []
        self.mapwin.mouse['begin'] = self.mapwin.mouse['end'] = (0.0,0.0)
        self.mapwin.mouse['use'] = 'pointer'
        self.mapwin.mouse['box'] = 'point'

    def OnRedraw(self, event):
        """
        Redraw the profile window
        """
        self.client.Reset()
        self.client.Redraw()
        self.client.Draw(self.profile)

    def OnErase(self, event):
        """
        Erase the profile window
        """
        self.client.Clear()
        try:
            self.mapwin.pdc.ClearId(self.mapwin.lineid)
            self.mapwin.pdc.ClearId(self.mapwin.plineid)
            self.mapwin.Refresh()
        except:
            pass


    def SaveToFile(self, event):
        """
        Save profile to graphics file
        """
        self.client.SaveFile()


    def ProfileOptionsMenu(self, event):
        """
        Set various profile options.
        """

        point = wx.GetMousePosition()
        popt = wx.Menu()
        # Add items to the menu
        settext = wx.MenuItem(popt, -1, 'Title and axis labels')
        popt.AppendItem(settext)
        self.Bind(wx.EVT_MENU, self.PText, settext)

        setgrid = wx.MenuItem(popt, -1, 'Grid and legend settings')
        popt.AppendItem(setgrid)
        self.Bind(wx.EVT_MENU, self.POptions, setgrid)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(popt)
        popt.Destroy()

    def NotFunctional(self):
        dlg = wx.MessageDialog(self, 'This feature is not yet functional',
                           'Under Construction', wx.OK | wx.ICON_INFORMATION)
        dlg.ShowModal()
        dlg.Destroy()


    def PText(self, event):
        """
        Set custom text values for profile
        title and axis labels.
        """
        dlg = TextDialog(self, -1, 'Profile text settings')

        if dlg.ShowModal() == wx.ID_OK:
            self.ptitle = dlg.ptitle
            self.xlabel = dlg.xlabel
            self.ylabel = dlg.ylabel
            self.font = dlg.font
            self.titlefontsize = dlg.titlefontsize
            self.axisfontsize = dlg.axisfontsize
            self.client.SetFont(dlg.font)
            self.client.SetFontSizeTitle(self.titlefontsize)
            self.client.SetFontSizeAxis(self.axisfontsize)

            try:
                self.profile.setTitle(dlg.ptitle)
                self.profile.setXLabel(dlg.xlabel)
                self.profile.setYLabel(dlg.ylabel)
            except:
                pass

        dlg.Destroy()
        self.OnRedraw(event=None)

    def POptions(self, event):
        """
        Set grid color, enable/disable grid
        """
        self.NotFunctional()

    def LegendOptions(self, event):
        """
        Set legend font, enable/disable legend
        """
        self.NotFunctional()

    def PrintMenu(self, event):
        """
        Print options and output menu
        """
        point = wx.GetMousePosition()
        printmenu = wx.Menu()
        # Add items to the menu
        setup = wx.MenuItem(printmenu, -1,'Page setup')
        printmenu.AppendItem(setup)
        self.Bind(wx.EVT_MENU, self.OnPageSetup, setup)

        preview = wx.MenuItem(printmenu, -1,'Print preview')
        printmenu.AppendItem(preview)
        self.Bind(wx.EVT_MENU, self.OnPrintPreview, preview)

        doprint = wx.MenuItem(printmenu, -1,'Print display')
        printmenu.AppendItem(doprint)
        self.Bind(wx.EVT_MENU, self.OnDoPrint, doprint)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(printmenu)
        printmenu.Destroy()

    def OnPageSetup(self, event):
        self.client.PageSetup()

    def OnPrintPreview(self, event):
        self.client.PrintPreview()

    def OnDoPrint(self, event):
        self.client.Printout()

    def OnQuit(self, event):
        self.Close(True)

    def OnCloseWindow(self, event):
        """
        Window closed
        Also remove associated rendered images
        """

        self.Destroy()

class SetRaster(wx.Dialog):
    def __init__(self, parent, id, title="", pos=wx.DefaultPosition, size=wx.DefaultSize,
            style=wx.DEFAULT_DIALOG_STYLE):
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)
        """
        Select raster map to profile
        """

        self.rast = ''

        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)

        label = wx.StaticText(self, -1, "Select raster:")
        box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        self.selection = select.Select(self, id=wx.ID_ANY, size=(300,-1),
                                          type='cell')
        box.Add(self.selection, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        self.selection.Bind(wx.EVT_TEXT, self.onSelection)

        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.EXPAND|wx.RIGHT|wx.TOP, 5)

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

    def onSelection(self, event):
        self.rast = event.GetString()

    def onText(self, event):
        self.rast = event.GetString()

class TextDialog(wx.Dialog):
    def __init__(self, parent, id, title, pos=wx.DefaultPosition, size=wx.DefaultSize,
            style=wx.DEFAULT_DIALOG_STYLE):
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)
        """
        Controls setting options and displaying/hiding map overlay decorations
        """
        self.parent = parent
        self.ptitle = self.parent.ptitle
        self.titlefontsize = self.parent.titlefontsize
        self.xlabel = self.parent.xlabel
        self.ylabel = self.parent.ylabel
        self.axisfontsize = self.parent.axisfontsize
        self.font = self.parent.font
        self.fontfamily = self.parent.font.GetFamilyString()
        self.fontstyle = self.parent.font.GetStyleString()
        self.fontweight = self.parent.font.GetWeightString()


#        self.legendfontsize = 10
#        self.enablegrid = True
#        self.gridcolor = wx.Colour(200,200,200)
#        self.enablelegend = True


        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Profile title:")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        self.ptitleentry = wx.TextCtrl(self, -1, "", size=(200,-1))
        self.ptitleentry.SetFont(self.font)
        self.ptitleentry.SetValue(self.ptitle)
        box.Add(self.ptitleentry, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        tlabel = wx.StaticText(self, -1, "Title font size (pts):")
        box.Add(tlabel, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        self.ptitlesize = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                        size=(50,-1), style=wx.SP_ARROW_KEYS)
        self.ptitlesize.SetRange(5,100)
        self.ptitlesize.SetValue(int(self.titlefontsize))
        box.Add(self.ptitlesize, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "X-axis label:")
        box.Add(label, 0, wx.wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        self.xlabelentry = wx.TextCtrl(self, -1, "", size=(200,-1))
        self.xlabelentry.SetFont(self.font)
        self.xlabelentry.SetValue(self.xlabel)
        box.Add(self.xlabelentry, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Y-axis label:")
        box.Add(label, 0, wx.wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        self.ylabelentry = wx.TextCtrl(self, -1, "", size=(200,-1))
        self.ylabelentry.SetFont(self.font)
        self.ylabelentry.SetValue(self.ylabel)
        box.Add(self.ylabelentry, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        llabel = wx.StaticText(self, -1, "Label font size (pts):")
        box.Add(llabel, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        self.axislabelsize = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                        size=(50,-1), style=wx.SP_ARROW_KEYS)
        self.axislabelsize.SetRange(5,100)
        self.axislabelsize.SetValue(int(self.axisfontsize))
        box.Add(self.axislabelsize, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Font for title, axis labels, and legend")
        box.Add(label, 0, wx.ALIGN_CENTRE|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER|wx.VERTICAL, 5)


        ffamilylist = ['wx.FONTFAMILY_DEFAULT', 'wx.FONTFAMILY_DECORATIVE', 'wx.FONTFAMILY_ROMAN', 'wx.FONTFAMILY_SCRIPT', 'wx.FONTFAMILY_SWISS', 'wx.FONTFAMILY_MODERN', 'wx.FONTFAMILY_TELETYPE']
        fstylelist = ['wx.FONTSTYLE_NORMAL', 'wx.FONTSTYLE_SLANT', 'wx.FONTSTYLE_ITALIC']
        fwtlist = ['wx.FONTWEIGHT_NORMAL', 'wx.FONTWEIGHT_LIGHT', 'wx.FONTWEIGHT_BOLD']

        box = wx.BoxSizer(wx.HORIZONTAL)
        label1 = wx.StaticText(self, -1, "Font family:")
        box.Add(label1, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        ffamilycb = wx.ComboBox(
            self, 500, self.fontfamily, (90, 50),
            (200, -1), ffamilylist, wx.CB_DROPDOWN #|wxTE_PROCESS_ENTER
            )
        box.Add(ffamilycb, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 2)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label2 = wx.StaticText(self, -1, " Style:")
        box.Add(label2, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        fstylecb = wx.ComboBox(
            self, 501, self.fontstyle, (90, 50),
            (200, -1), fstylelist, wx.CB_DROPDOWN #|wxTE_PROCESS_ENTER
            )
        box.Add(fstylecb, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 2)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label3 = wx.StaticText(self, -1, " Weight:")
        box.Add(label3, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        fwtcb = wx.ComboBox(
            self, 502, self.fontweight, (90, 50),
            (200, -1), fwtlist, wx.CB_DROPDOWN #|wxTE_PROCESS_ENTER
            )
        box.Add(fwtcb, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 2)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.EXPAND|wx.ALIGN_CENTER|wx.RIGHT|wx.TOP, 5)

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

        self.ptitleentry.Bind(wx.EVT_TEXT, self.OnTitle)
        self.xlabelentry.Bind(wx.EVT_TEXT, self.OnXLabel)
        self.ylabelentry.Bind(wx.EVT_TEXT, self.OnYLabel)
        self.ptitlesize.Bind(wx.EVT_TEXT, self.OnPtitlesize)
        self.axislabelsize.Bind(wx.EVT_TEXT, self.OnAxislabelsize)
        self.Bind(wx.EVT_COMBOBOX, self.OnFFamily, ffamilycb)
        self.Bind(wx.EVT_COMBOBOX, self.OnFstyle, fstylecb)
        self.Bind(wx.EVT_COMBOBOX, self.OnFWeight, fwtcb)

    def OnPtitlesize(self, event):
        self.titlefontsize = event.GetString()

    def OnAxislabelsize(self, event):
        self.axisfontsize = event.GetString()

    def OnTitle(self, event):
        self.ptitle = event.GetString()

    def OnXLabel(self, event):
        self.xlabel = event.GetString()

    def OnYLabel(self, event):
        self.xlabel = event.GetString()

    def OnFFamily(self, event):
        family = eval(event.GetString())
        self.font.SetFamily(family)
        self.UpdateDialog()

    def OnFstyle(self, event):
        style = eval(event.GetString())
        self.font.SetStyle(style)
        self.UpdateDialog()

    def OnFWeight(self, event):
        weight = eval(event.GetString())
        self.font.SetWeight(weight)
        self.UpdateDialog()

    def UpdateDialog(self):
        self.ptitleentry.SetFont(self.font)
        self.xlabelentry.SetFont(self.font)
        self.ylabelentry.SetFont(self.font)
        self.Layout()



