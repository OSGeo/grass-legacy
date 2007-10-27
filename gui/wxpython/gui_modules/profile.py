"""
MODULE: profile

CLASSES:
 * ProfileFrame
 * SetRaster
 * Textdialog
 * OptDialog

PURPOSE: Profile analysis of GRASS raster maps and images. Uses PyPlot (wx.lib.plot.py)

AUTHORS: The GRASS Development Team. Michael Barton

COPYRIGHT: (C) 2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import wx
import os, sys, math
import  wx.lib.colourselect as  csel

try:
    import wx.lib.plot as plot
except:
    msg= """
    This module requires the NumPy module,
    which could not be imported.  It probably is not installed
    (it's not part of the standard Python distribution). See the
    Numeric Python site (http://numpy.scipy.org) for information on
    downloading source or binaries."""
    print >> sys.stderr, "profile.py: " + msg

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
import gselect
import gcmd
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
    Mainframe for displaying profile of raster map. Uses wx.lib.plot.
    """

    def __init__(self, parent=None, id = wx.ID_ANY, title="Profile Analysis",
                 pos=wx.DefaultPosition, size=wx.DefaultSize,
                 style=wx.DEFAULT_FRAME_STYLE):

        wx.Frame.__init__(self, parent, id, title, pos, size, style)

        toolbar = self.__createToolBar()

        self.parent = parent

        self.Map = render.Map()  # instance of render.Map to be associated with display

        #
        # Set the size & cursor
        #
        self.SetClientSize(size)
        self.iconsize = (16, 16)

        #
        # Add statusbar
        #
        self.statusbar = self.CreateStatusBar(number=2, style=0)
        self.statusbar.SetStatusWidths([-2, -1])

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

        # Init variables
        self.rast1 = '' # default raster map to profile
        self.rast2 = '' # optional raster map to profile
        self.rast3 = '' # optional raster map to profile
        self.rastunits1 = '' # map data units (used for y axis legend)
        self.rastunits2 = '' # map data units (used for y axis legend)
        self.rastunits3 = '' # map data units (used for y axis legend)
        self.coordstr = '' #string of coordinates for r.profile
        self.seglist = [] # segment endpoint list
        self.plotlist = [] # list of things to plot
        self.ppoints = '' # segment endpoints data
        self.profile = None # plot draw object

        self.ptitle = 'Profile of %s %s %s' % (self.rast1, self.rast2, self.rast3)
        if self.parent.projinfo['units'] != '':
            self.xlabel = 'Distance (%s)' % self.parent.projinfo['units']
        else:
            self.xlabel = "Distance along transect"
        self.ylabel = "Cell values"

        self.datalist1 = [] #list of distance,value pairs for plotting profile
        self.pline1 = None # first (default) profile line
        self.pcolor1 = wx.Colour(0,0,255) # profile line color
        self.pwidth1 = 1 # profile line width
        self.pstyle1 = wx.SOLID # profile line pen style
        self.plegend1 = 'Profile' # profile legend string
        self.datalist2 = [] #list of distance,value pairs for plotting profile
        self.pline2 = None # second (optional) profile line
        self.pcolor2 = wx.Colour(255,0,0) # profile line color
        self.pwidth2 = 1 # profile line width
        self.pstyle2 = wx.SOLID # profile line pen style
        self.plegend2 = 'Profile' # profile legend string
        self.datalist3 = [] #list of distance,value pairs for plotting profile
        self.pline3 = None # third (optional) profile line
        self.pcolor3 = wx.Colour(0,255,0) # profile line color
        self.pwidth3 = 1 # profile line width
        self.pstyle3 = wx.SOLID # profile line pen style
        self.plegend3 = 'Profile' # profile legend string

        self.ptcolor = wx.Colour(0,0,0) # segmenet marker point color
        self.ptfill = wx.TRANSPARENT # segment marker point fill style
        self.ptsize = 2 # segment marker point size
        self.pttype = 'triangle' # segment marker point type
        self.ptlegend = 'Segment break' # segment marker point legend string

        self.font = wx.Font(12,wx.FONTFAMILY_SWISS,wx.FONTSTYLE_NORMAL,wx.FONTWEIGHT_NORMAL) # text font
        self.titlefontsize = 14 # title font size
        self.axisfontsize = 12 # axis label font size

        self.enablelegend = True # enable legend display
        self.legendfontsize = 10 # legend font size

        self.enablegrid = True # enable grid display
        self.gridcolor = wx.Colour(200,200,200) # grid color

        self.xtype = 'auto' # x axis format
        self.ytype = 'auto' # y axis format
        self.xmin = 0 # x axis min for custom axis range
        self.xmax = 0 # x axis max for custom axis range
        self.ymin = 0 # y axis min for custom axis range
        self.ymax = 0 # y axis max for custom axis range
        self.xaxis = None # x axis range tuple
        self.yaxis = None # y axis range tuple
        self.xlog = self.ylog = False

        self.zoom = False # zooming disabled
        self.drag = False # draging disabled

        self.client.SetShowScrollbars(True) # vertical and horizontal scrollbars
        self.client.setLogScale((False,False)) # x and y axis set to normal (non-log)
        self.client.SetXSpec(self.xtype)
        self.client.SetYSpec(self.ytype)




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
                 ('transect', Icons["transect"].GetBitmap(), Icons["transect"].GetLabel(), self.DrawTransect),
                 ('raster', Icons["addrast"].GetBitmap(), Icons["addrast"].GetLabel(), self.SelectRaster),
                 ('profiledraw', Icons["profiledraw"].GetBitmap(), Icons["profiledraw"].GetLabel(), self.CreateProfile),
                 ('options', Icons["profileopt"].GetBitmap(), 'Profile options', self.ProfileOptionsMenu),
                 ('drag', Icons['pan'].GetBitmap(), 'Enable drag', self.OnDrag),
                 ('zoom', Icons['zoom_in'].GetBitmap(), 'Enable zoom', self.OnZoom),
                 ('unzoom', Icons['zoom_back'].GetBitmap(), 'Unzoom profile', self.OnRedraw),
                 ('erase', Icons["erase"].GetBitmap(), 'Erase profile', self.OnErase),
                 ('', '', '', ''),
                 ('save',  Icons["savefile"].GetBitmap(),  'Save profile',  self.SaveToFile),
                 ('print',  Icons["printmap"].GetBitmap(),  'Print profile',  self.PrintMenu),
                 ('quit',  wx.ArtProvider.GetBitmap(wx.ART_QUIT, wx.ART_TOOLBAR, (16,16)),  Icons["quit"].GetLabel(), self.OnQuit),
                  )

    def DrawTransect(self, event):
        """
        Draws transect to profile in map display
        """
        self.mapwin.polycoords = []
        self.seglist = []
        self.mapwin.ClearLines()
        self.ppoints = ''
        self.Parent.SetFocus()
        self.Parent.Raise()
        self.mapwin.mouse['use'] = 'profile'
        self.mapwin.mouse['box'] = 'line'
        self.mapwin.pen = wx.Pen(colour='Red', width=2, style=wx.SHORT_DASH)
        self.mapwin.polypen = wx.Pen(colour='dark green', width=2, style=wx.SHORT_DASH)
        self.mapwin.SetCursor(self.Parent.cursors["cross"])

    def SelectRaster(self, event):
        """
        Select raster map to profile (i.e., to get distance,value data).
        Create coordinate string for profiling. Create segment list for
        transect segment markers.
        """

        # create list of coordinate points for r.profile
        dist = 0
        cumdist = 0
        self.coordstr = ''
        lasteast = lastnorth = None
        if len(self.mapwin.polycoords) > 0:
            for point in self.mapwin.polycoords:
                # convert screen coordinates to map coordinates for transect
                east, north = self.mapwin.Pixel2Cell(point)

                # build string of coordinate points for r.profile
                if self.coordstr == '':
                    self.coordstr = '%d,%d' % (east,north)
                else:
                    self.coordstr = '%s,%d,%d' % (self.coordstr,east,north)

        else:
            dlg = wx.MessageDialog(self, 'You must draw a transect to profile in the map display window',
                               'Nothing to profile', wx.OK | wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            return

        dlg = SetRaster(self, wx.ID_ANY, "Select raster to profile",
                          pos=wx.DefaultPosition, size=wx.DefaultSize,
                          style=wx.DEFAULT_DIALOG_STYLE)

        if dlg.ShowModal() == wx.ID_OK:
            self.rast1 = dlg.rast1
            self.rast2 = dlg.rast2
            self.rast3 = dlg.rast3

        elif dlg.ShowModal() == wx.ID_CANCEL:
            dlg.Destroy()
            return

        self.datalist1 = self.CreateDatalist(self.rast1, self.coordstr)
        self.plegend1 = ' Profile of %s' % self.rast1

        # set default title and legend text
        self.ptitle = 'Profile of %s' % self.rast1

        # set self.ylabel to match units if they exist
        cmdlist = ['r.info', 'map=%s' % self.rast1, '-u', '--quiet']
        p = gcmd.Command(cmdlist)
        try:
            units1 = p.module_stdout.read().strip().split('=')[1]
        except:
            pass

        if units1 == '': units1 = 'Raster values'
        if units1 != 'Raster values':
            self.ylabel = units1

        if self.rast2 == '' and self.rast3 != '':
            self.rast2 = self.rast3
            self.rast3 = ''

        if self.rast2 != '':
            self.datalist2 = self.CreateDatalist(self.rast2, self.coordstr)
            cmdlist = ['r.info', 'map=%s' % self.rast2, '-u', '--quiet']
            p = gcmd.Command(cmdlist)
            try:
                units2 = p.module_stdout.read().strip().split('=')[1]
            except:
                pass

            if units2 == '': units2 = 'Raster values'

            self.ptitle += ' and %s' % self.rast2
            self.plegend2 = 'Profile of %s' % self.rast2

            if units1 == 'Raster values' and units2 == 'Raster values':
                self.ylabel = 'Raster values'
            else:
                self.ylabel = '%s (1), %s (2)' % (units1,units2)

        if self.rast3 != '':
            self.datalist3 = self.CreateDatalist(self.rast3, self.coordstr)
            cmdlist = ['r.info', 'map=%s' % self.rast3, '-u', '--quiet']
            p = gcmd.Command(cmdlist)
            try:
                units3 = p.module_stdout.read().strip().split('=')[1]
            except:
                units3 = 'Raster values'

            self.ptitle += ', %s, and %s' % (self.rast2,self.rast3)
            self.plegend2 = 'Profile of %s' % self.rast2
            self.plegend3 = 'Profile of %s' % self.rast3

            if units1 == 'Raster values' and units2 == 'Raster values' and units3 == 'Raster values':
                self.ylabel = 'Raster values'
            else:
                self.ylabel = '%s (1), %s (2), %s (3)' % (units1,units2,units3)

        # create list of coordinates for transect segment markers
        if len(self.mapwin.polycoords) > 0 and self.rast1 != '':
            for point in self.mapwin.polycoords:
                # convert screen coordinates to map coordinates for transect
                east, north = self.mapwin.Pixel2Cell(point)

                # get value of raster cell at coordinate point
                try:
                    cmdlist = ['r.what', 'input=%s' % self.rast1, 'east_north=%d,%d' % (east,north)]
                    p = gcmd.Command(cmdlist)
                    if p.returncode == 0:
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
                except:
                    pass


            # delete first and last segment point
            try:
                self.seglist.pop(0)
                self.seglist.pop()
            except:
                pass


    def SetGraphStyle(self):
        """
        Set plot and text options
        """
        self.client.SetFont(self.font)
        self.client.SetFontSizeTitle(self.titlefontsize)
        self.client.SetFontSizeAxis(self.axisfontsize)

        self.client.SetEnableZoom(self.zoom)
        self.client.SetEnableDrag(self.drag)
        if self.xtype == 'custom':
            self.client.SetXSpec('min')
        else:
            self.client.SetXSpec(self.xtype)

        if self.ytype == 'custom':
            self.client.SetYSpec('min')
        else:
            self.client.SetYSpec(self.ytype)

        if self.xtype == 'custom' and self.xmin != self.xmax:
            self.xaxis = (self.xmin,self.xmax)
        else:
            self.xaxis = None

        if self.ytype == 'custom' and self.ymin != self.ymax:
            self.yaxis = (self.ymin,self.ymax)
        else:
            self.yaxis = None

        self.client.SetEnableGrid(self.enablegrid)
        self.client.SetGridColour(self.gridcolor)

        self.client.SetFontSizeLegend(self.legendfontsize)
        self.client.SetEnableLegend(self.enablelegend)

        if self.xlog == True:
            self.xaxis = None
            self.client.SetXSpec('min')
        if self.ylog == True:
            self.yaxis = None
            self.client.SetYSpec('min')
        self.client.setLogScale((self.xlog,self.ylog))

#        self.client.SetPointLabelFunc(self.DrawPointLabel())

    def CreateDatalist(self, raster, coords):
        """
        Build a list of distance, value pairs for points along transect
        """
        datalist = []
        try:
#            cmdlist = ['r.profile', 'input=%s' % raster, 'profile=%s' % coords, 'null=nan', '--quiet']
#            p = gcmd.Command(cmdlist, wait=False)
#            output = p.module_stdout.read().strip().split('\n')
#            if p.returncode == 0:
#                for outline in output:
#                    dist,elev = outline.split(' ')
#                    datalist.append((dist,elev))

            output = os.popen('r.profile input=%s profile=%s null=nan --quiet' % (raster,coords), "r").read().strip().split('\n')
            for outline in output:
                dist,elev = outline.split(' ')
                datalist.append((dist,elev))

            return datalist
        except:
            return None

    def CreateProfile(self, event):
        """
        Main routine for creating a profile. Uses r.profile to create a list
        of distance,cell value pairs. This is passed to plot to create a
        line graph of the profile. If the profile transect is in multiple
        segments, these are drawn as points. Profile transect is drawn, using
        methods in mapdisp.py
        """
        if self.rast1 == '':
            dlg = wx.MessageDialog(self, 'You must select at least one raster map to profile',
                               'Nothing to profile', wx.OK | wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            return

        self.mapwin.SetCursor(self.Parent.cursors["default"])
        self.SetCursor(self.Parent.cursors["default"])
        self.SetGraphStyle()

        self.DrawPlot()

        # reset transect
        self.mapwin.mouse['begin'] = self.mapwin.mouse['end'] = (0.0,0.0)
        self.mapwin.mouse['use'] = 'pointer'
        self.mapwin.mouse['box'] = 'point'

    def DrawPlot(self):
        """
        Draw line and point plot from transect datalist and
        transect segment endpoint coordinates.
        """

        # graph the distance, value pairs for the transect
        self.plotlist = []
        if len(self.datalist1) > 0:
            self.pline1 = plot.PolyLine(self.datalist1,
                                        colour=self.pcolor1,
                                        width=self.pwidth1,
                                        style=self.pstyle1,
                                        legend=' '+self.plegend1)

        self.plotlist.append(self.pline1)

        if len(self.datalist2) > 0:
            self.pline2 = plot.PolyLine(self.datalist2,
                                        colour=self.pcolor2,
                                        width=self.pwidth2,
                                        style=self.pstyle2,
                                        legend=' '+self.plegend2)
            self.plotlist.append(self.pline2)



        if len(self.datalist3) > 0:
            self.pline3 = plot.PolyLine(self.datalist3,
                                        colour=self.pcolor3,
                                        width=self.pwidth3,
                                        style=self.pstyle3,
                                        legend=' '+self.plegend3)
            self.plotlist.append(self.pline3)

        if len(self.seglist) > 0 :
            self.ppoints = plot.PolyMarker(self.seglist, legend=' '+self.ptlegend,
                                           colour=self.ptcolor,
                                           size=self.ptsize,
                                           fillstyle = self.ptfill,
                                           marker=self.pttype)
            self.plotlist.append(self.ppoints)

        self.profile = plot.PlotGraphics(self.plotlist, self.ptitle, self.xlabel, self.ylabel)

        if self.xtype == 'custom':
            self.client.SetXSpec('min')
        else:
            self.client.SetXSpec(self.xtype)

        if self.ytype == 'custom':
            self.client.SetYSpec('min')
        else:
            self.client.SetYSpec(self.ytype)

        self.client.Draw(self.profile, self.xaxis, self.yaxis)

    def OnZoom(self, event):
        """
        Enable zooming and disable dragging
        """

        self.zoom = True
        self.drag = False
        self.client.SetEnableZoom(self.zoom)
        self.client.SetEnableDrag(self.drag)

    def OnDrag(self, event):
        """
        Enable dragging and disable zooming
        """

        self.zoom = False
        self.drag = True
        self.client.SetEnableDrag(self.drag)
        self.client.SetEnableZoom(self.zoom)

    def OnRedraw(self, event):
        """
        Redraw the profile window. Unzoom to original size
        """
        self.client.Reset()
        self.client.Redraw()

    def Update(self):
        """
        Update profile after changing options
        """

        self.SetGraphStyle()
        self.DrawPlot()

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

    def ProfileOptionsMenu(self, event):
        """
        Popup menu for profile and text options
        """

        point = wx.GetMousePosition()
        popt = wx.Menu()
        # Add items to the menu
        settext = wx.MenuItem(popt, -1, 'Profile text settings')
        popt.AppendItem(settext)
        self.Bind(wx.EVT_MENU, self.PText, settext)

        setgrid = wx.MenuItem(popt, -1, 'Profile plot settings')
        popt.AppendItem(setgrid)
        self.Bind(wx.EVT_MENU, self.POptions, setgrid)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(popt)
        popt.Destroy()

    def NotFunctional(self):
        """
        Creates a 'not functional' message dialog
        """

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
        Set various profile options, including: line width, color, style;
        marker size, color, fill, and style; grid and legend options.
        Calls OptDialog class.
        """
        dlg = OptDialog(self, -1, 'Profile settings')

        if dlg.ShowModal() == wx.ID_OK:
            self.pcolor1 = dlg.pcolor1
            self.pwidth1 = dlg.pwidth1
            self.pstyle1 = dlg.pstyle1
            self.plegend1 = dlg.plegend1

            self.pcolor2 = dlg.pcolor2
            self.pwidth2 = dlg.pwidth2
            self.pstyle2 = dlg.pstyle2
            self.plegend2 = dlg.plegend2

            self.pcolor3 = dlg.pcolor3
            self.pwidth3 = dlg.pwidth3
            self.pstyle3 = dlg.pstyle3
            self.plegend3 = dlg.plegend3

            self.ptcolor = dlg.ptcolor
            self.ptfill = dlg.ptfill
            self.ptsize = dlg.ptsize
            self.pttype = dlg.pttype
            self.ptlegend = dlg.ptlegend

            self.xtype = dlg.xtype
            self.ytype = dlg.ytype
            self.xmin = dlg.xmin
            self.xmax = dlg.xmax
            self.xlog = dlg.xlog
            self.ymin = dlg.ymin
            self.ymax = dlg.ymax
            self.ylog = dlg.ylog

            self.gridcolor = dlg.gridcolor
            self.enablegrid = dlg.enablegrid
            self.enablelegend = dlg.enablelegend
            self.legendfontsize = dlg.legendfontsize

            self.Update()
        else:
            pass
        dlg.Destroy()

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
        Close profile window and clean up
        """
        self.mapwin.ClearLines()
        self.mapwin.mouse['begin'] = self.mapwin.mouse['end'] = (0.0,0.0)
        self.mapwin.mouse['use'] = 'pointer'
        self.mapwin.mouse['box'] = 'point'
        self.mapwin.polycoords = []
        self.mapwin.SetCursor(self.Parent.cursors["default"])

        self.Destroy()

class SetRaster(wx.Dialog):
    def __init__(self, parent, id, title="", pos=wx.DefaultPosition, size=wx.DefaultSize,
            style=wx.DEFAULT_DIALOG_STYLE):
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)
        """
        Dialog to select raster maps to profile.
        """

        self.parent = parent
        self.rast1 = self.parent.rast1
        self.rast2 = self.parent.rast2
        self.rast3 = self.parent.rast3
        selection1 = selection2 = selection3 = None
        self.datalist1 = self.datalist2 = self.datalist3 = []

        self.parent = parent

        self.coordstr = self.parent.coordstr
        if self.coordstr == '':
            dlg = wx.MessageDialog(self, 'You must draw a transect to profile in the map display window',
                               'Nothing to profile', wx.OK | wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            self.Close(True)
            return

        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Select raster 1 (required):")
        box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        selection1 = gselect.Select(self, id=wx.ID_ANY, size=(300,-1),type='cell')
        try:
            selection1.SetValue(self.rast1)
        except:
            pass
        box.Add(selection1, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Select raster 2 (optional):")
        box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        selection2 = gselect.Select(self, id=wx.ID_ANY, size=(300,-1),type='cell')
        try:
            selection2.SetValue(self.rast2)
        except:
            pass
        box.Add(selection2, 0, wx.ALIGN_CENTRE|wx.ALL, 5)

        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Select raster 3 (optional):")
        box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        selection3 = gselect.Select(self, id=wx.ID_ANY, size=(300,-1),type='cell')
        try:
            selection3.SetValue(self.rast3)
        except:
            pass
        box.Add(selection3, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
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

        selection1.Bind(wx.EVT_TEXT, self.OnSelection1)
        selection2.Bind(wx.EVT_TEXT, self.OnSelection2)
        selection3.Bind(wx.EVT_TEXT, self.OnSelection3)

    def OnSelection1(self, event):
        self.rast1 = event.GetString()

    def OnSelection2(self, event):
        self.rast2 = event.GetString()

    def OnSelection3(self, event):
        self.rast3 = event.GetString()

class TextDialog(wx.Dialog):
    def __init__(self, parent, id, title, pos=wx.DefaultPosition, size=wx.DefaultSize,
            style=wx.DEFAULT_DIALOG_STYLE):
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)
        """
        Dialog to set profile text options: font, title
        and font size, axis labels and font size
        """
        # initialize variables

        # combo box entry lists
        self.ffamilydict = {'default':wx.FONTFAMILY_DEFAULT,
                       'decorative':wx.FONTFAMILY_DECORATIVE,
                       'roman':wx.FONTFAMILY_ROMAN,
                       'script':wx.FONTFAMILY_SCRIPT,
                       'swiss':wx.FONTFAMILY_SWISS,
                       'modern':wx.FONTFAMILY_MODERN,
                       'teletype':wx.FONTFAMILY_TELETYPE}

        self.fstyledict = {'normal':wx.FONTSTYLE_NORMAL,
                      'slant':wx.FONTSTYLE_SLANT,
                      'italic':wx.FONTSTYLE_ITALIC}

        self.fwtdict = {'normal':wx.FONTWEIGHT_NORMAL,
                   'light':wx.FONTWEIGHT_LIGHT,
                   'bold':wx.FONTWEIGHT_BOLD}

        self.parent = parent
        self.ptitle = self.parent.ptitle
        self.titlefontsize = self.parent.titlefontsize
        self.xlabel = self.parent.xlabel
        self.ylabel = self.parent.ylabel
        self.axisfontsize = self.parent.axisfontsize
        self.font = self.parent.font
        self.fontfamily = self.parent.font.GetFamily()
        for item in self.ffamilydict.items():
            if self.fontfamily == item[1]:
                ffamilykey = item[0]
            else:
                ffamilykey = 'swiss'
        self.fontstyle = self.parent.font.GetStyle()
        for item in self.fstyledict.items():
            if self.fontstyle == item[1]:
                fstylekey = item[0]
            else:
                fstylekey = 'normal'
        self.fontweight = self.parent.font.GetWeight()
        for item in self.fwtdict.items():
            if self.fontweight == item[1]:
                fwtkey = item[0]
            else:
                fwtkey = 'normal'

        # dialog layout
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

        box = wx.BoxSizer(wx.HORIZONTAL)
        label1 = wx.StaticText(self, -1, "Font family:")
        box.Add(label1, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        ffamilycb = wx.ComboBox(
            self, 500, ffamilykey, (90, 50),
            (200, -1), self.ffamilydict.keys(), wx.CB_DROPDOWN #|wxTE_PROCESS_ENTER
            )
        box.Add(ffamilycb, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 2)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label2 = wx.StaticText(self, -1, " Style:")
        box.Add(label2, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        fstylecb = wx.ComboBox(
            self, 501, fstylekey, (90, 50),
            (200, -1), self.fstyledict.keys(), wx.CB_DROPDOWN #|wxTE_PROCESS_ENTER
            )
        box.Add(fstylecb, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 2)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label3 = wx.StaticText(self, -1, " Weight:")
        box.Add(label3, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        fwtcb = wx.ComboBox(
            self, 502, fwtkey, (90, 50),
            (200, -1), self.fwtdict.keys(), wx.CB_DROPDOWN #|wxTE_PROCESS_ENTER
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
        self.ylabel = event.GetString()

    def OnFFamily(self, event):
        family = self.ffamilydict[event.GetString()]
        self.font.SetFamily(family)
        self.UpdateDialog()

    def OnFstyle(self, event):
        style = self.fstyledict[event.GetString()]
        self.font.SetStyle(style)
        self.UpdateDialog()

    def OnFWeight(self, event):
        weight = self.fwtdict[event.GetString()]
        self.font.SetWeight(weight)
        self.UpdateDialog()

    def UpdateDialog(self):
        self.ptitleentry.SetFont(self.font)
        self.xlabelentry.SetFont(self.font)
        self.ylabelentry.SetFont(self.font)
        self.Layout()


class OptDialog(wx.Dialog):
    def __init__(self, parent, id, title, pos=wx.DefaultPosition, size=wx.DefaultSize,
            style=wx.DEFAULT_DIALOG_STYLE):
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)
        """
        Dialog to set various profile options, including: line width, color, style;
        marker size, color, fill, and style; grid and legend options.
        """
        # init variables

        self.pstyledict = {'solid': wx.SOLID,
              'dot': wx.DOT,
              'long-dash': wx.LONG_DASH,
              'short-dash': wx.SHORT_DASH,
              'dot-dash': wx.DOT_DASH}

        pttypelist = ['circle','dot','square','triangle','triangle_down','cross','plus']
        self.ptfilldict = {'transparent':wx.TRANSPARENT, 'solid':wx.SOLID}

        axislist = ['min','auto','custom']

        self.parent = parent

        self.pcolor1 = self.parent.pcolor1
        self.pwidth1 = self.parent.pwidth1
        self.pstyle1 = self.parent.pstyle1
        for item in self.pstyledict.items():
            if self.pstyle1 == item[1]:
                self.pstylekey1 = item[0]
        self.plegend1 = self.parent.plegend1

        self.pcolor2 = self.parent.pcolor2
        self.pwidth2 = self.parent.pwidth2
        self.pstyle2 = self.parent.pstyle2
        for item in self.pstyledict.items():
            if self.pstyle2 == item[1]:
                self.pstylekey2 = item[0]
        self.plegend2 = self.parent.plegend2

        self.pcolor3 = self.parent.pcolor3
        self.pwidth3 = self.parent.pwidth3
        self.pstyle3 = self.parent.pstyle3
        for item in self.pstyledict.items():
            if self.pstyle3 == item[1]:
                self.pstylekey3 = item[0]
        self.plegend3 = self.parent.plegend3

        self.ptcolor = self.parent.ptcolor
        self.ptfill = self.parent.ptfill
        for item in self.ptfilldict.items():
            if self.ptfill == item[1]:
                self.ptfillkey = item[0]
        self.ptsize = self.parent.ptsize
        self.pttype = self.parent.pttype
        self.ptlegend = self.parent.ptlegend

        self.xtype = self.parent.xtype
        self.ytype = self.parent.ytype
        self.xmin = self.parent.xmin
        self.xmax = self.parent.xmax
        self.xlog = self.parent.xlog
        self.ymin = self.parent.ymin
        self.ymax = self.parent.ymax
        self.ylog = self.parent.ylog

        self.gridcolor = self.parent.gridcolor
        self.enablegrid = self.parent.enablegrid

        self.enablelegend = self.parent.enablelegend
        self.legendfontsize = self.parent.legendfontsize

        # dialog layout
        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Profile Line Options")
        box.Add(label, 0, wx.ALIGN_CENTER|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER|wx.ALL, 5)

        # profile 1 options
        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Profile 1: color")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        pcolor1 = csel.ColourSelect(self, -1, '', self.pcolor1, size = wx.DefaultSize)
        box.Add(pcolor1, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)

        label = wx.StaticText(self, -1, "width")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        pwidth1 = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                        size=(50,-1), style=wx.SP_ARROW_KEYS)
        pwidth1.SetRange(1,10)
        pwidth1.SetValue(self.pwidth1)
        box.Add(pwidth1, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)

        label = wx.StaticText(self, -1, "style")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        pstyle1 = wx.ComboBox(self, 500, self.pstylekey1, (90, 50),
            (120, -1), self.pstyledict.keys(), wx.CB_DROPDOWN)
        box.Add(pstyle1, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)

        label = wx.StaticText(self, -1, "legend")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        plegend1 = wx.TextCtrl(self, -1, "", size=(200,-1))
        plegend1.SetValue(self.plegend1)
        box.Add(plegend1, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 2)

        # profile 2 options
        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Profile 2: color")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        pcolor2 = csel.ColourSelect(self, -1, '', self.pcolor2, size = wx.DefaultSize)
        box.Add(pcolor2, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)

        label = wx.StaticText(self, -1, "width")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        pwidth2 = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                        size=(50,-1), style=wx.SP_ARROW_KEYS)
        pwidth2.SetRange(1,10)
        pwidth2.SetValue(self.pwidth2)
        box.Add(pwidth2, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)

        label = wx.StaticText(self, -1, "style")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        pstyle2 = wx.ComboBox(self, 500, self.pstylekey2, (90, 50),
            (120, -1), self.pstyledict.keys(), wx.CB_DROPDOWN)
        box.Add(pstyle2, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)

        label = wx.StaticText(self, -1, "legend")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        plegend2 = wx.TextCtrl(self, -1, "", size=(200,-1))
        plegend2.SetValue(self.plegend2)
        box.Add(plegend2, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 2)

        # profile 3 options
        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Profile 3: color")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        pcolor3 = csel.ColourSelect(self, -1, '', self.pcolor3, size = wx.DefaultSize)
        box.Add(pcolor3, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)

        label = wx.StaticText(self, -1, "width")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        pwidth3 = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                        size=(50,-1), style=wx.SP_ARROW_KEYS)
        pwidth3.SetRange(1,10)
        pwidth3.SetValue(self.pwidth3)
        box.Add(pwidth3, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)

        label = wx.StaticText(self, -1, "style")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        pstyle3 = wx.ComboBox(self, 500, self.pstylekey3, (90, 50),
            (120, -1), self.pstyledict.keys(), wx.CB_DROPDOWN)
        box.Add(pstyle3, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)

        label = wx.StaticText(self, -1, "legend")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        plegend3 = wx.TextCtrl(self, -1, "", size=(200,-1))
        plegend3.SetValue(self.plegend3)
        box.Add(plegend3, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 2)

        # segment marker options
        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Transect Segment Marker Options")
        box.Add(label, 0, wx.ALIGN_CENTER|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Marker: color")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        ptcolor = csel.ColourSelect(self, -1, '', self.ptcolor, size = wx.DefaultSize)
        box.Add(ptcolor, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)

        label = wx.StaticText(self, -1, "size")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        ptsize = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                        size=(50,-1), style=wx.SP_ARROW_KEYS)
        ptsize.SetRange(1,10)
        ptsize.SetValue(self.pwidth3)
        box.Add(ptsize, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)

        label = wx.StaticText(self, -1, "style")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        ptfill = wx.ComboBox(self, 500, self.ptfillkey, (90, 50),
            (120, -1), self.ptfilldict.keys(), wx.CB_DROPDOWN)
        box.Add(ptfill, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)

        label = wx.StaticText(self, -1, "legend")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        ptlegend = wx.TextCtrl(self, -1, "", size=(200,-1))
        ptlegend.SetValue(self.ptlegend)
        box.Add(ptlegend, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 2)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "type")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        pttype = wx.ComboBox(self, 500, self.pttype, (90, 50),
            (200, -1), pttypelist, wx.CB_DROPDOWN)
        box.Add(pttype, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.ALL, 2)

        # axis options
        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Axis Options")
        box.Add(label, 0, wx.ALIGN_CENTER|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "X-axis: style")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        xtype = wx.ComboBox(self, 500, self.xtype, (90, 50),
            (100, -1), axislist, wx.CB_DROPDOWN)
        box.Add(xtype, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        label = wx.StaticText(self, -1, "custom min")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        xmin = wx.TextCtrl(self, -1, "", size=(70,-1))
        xmin.SetValue(str(self.xmin))
        box.Add(xmin, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        label = wx.StaticText(self, -1, "custom max")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        xmax = wx.TextCtrl(self, -1, "", size=(70,-1))
        xmax.SetValue(str(self.xmax))
        box.Add(xmax, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        label = wx.StaticText(self, -1, "log scale")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        xlog = wx.CheckBox(self, -1, "")
        xlog.SetValue(self.xlog)
        box.Add(xlog, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER|wx.ALL, 2)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Y-axis: style")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        ytype = wx.ComboBox(self, 500, self.ytype, (90, 50),
            (100, -1), axislist, wx.CB_DROPDOWN)
        box.Add(ytype, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        label = wx.StaticText(self, -1, "custom min")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        ymin = wx.TextCtrl(self, -1, "", size=(70,-1))
        ymin.SetValue(str(self.ymin))
        box.Add(ymin, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        label = wx.StaticText(self, -1, "custom max")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        ymax = wx.TextCtrl(self, -1, "", size=(70,-1))
        ymax.SetValue(str(self.ymax))
        box.Add(ymax, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        label = wx.StaticText(self, -1, "log scale")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        ylog = wx.CheckBox(self, -1, "")
        ylog.SetValue(self.ylog)
        box.Add(ylog, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER|wx.ALL, 2)

        # grid & legend options
        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Grid and Legend Options")
        box.Add(label, 0, wx.ALIGN_CENTER|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, "Grid: color")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        gridcolor = csel.ColourSelect(self, -1, '', self.gridcolor, size = wx.DefaultSize)
        box.Add(gridcolor, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        label = wx.StaticText(self, -1, "show")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        gridshow = wx.CheckBox(self, -1, "")
        gridshow.SetValue(self.enablegrid)
        box.Add(gridshow, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        label = wx.StaticText(self, -1, "Legend: font size")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        legendfontsize = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                        size=(50,-1), style=wx.SP_ARROW_KEYS)
        legendfontsize.SetRange(5,100)
        legendfontsize.SetValue(int(self.legendfontsize))
        box.Add(legendfontsize, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        label = wx.StaticText(self, -1, "show")
        box.Add(label, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        legendshow = wx.CheckBox(self, -1, "")
        legendshow.SetValue(self.enablelegend)
        box.Add(legendshow, 0, wx.ALIGN_CENTER_VERTICAL|wx.ALIGN_RIGHT|wx.RIGHT|wx.LEFT, 5)
        sizer.Add(box, 0, wx.ALIGN_CENTER|wx.ALL, 2)

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

        pcolor1.Bind(csel.EVT_COLOURSELECT, self.OnPcolor1)
        pwidth1.Bind(wx.EVT_TEXT, self.OnPwidth1)
        pstyle1.Bind(wx.EVT_COMBOBOX, self.OnPstyle1)
        plegend1.Bind(wx.EVT_TEXT, self.OnPlegend1)
        pcolor2.Bind(csel.EVT_COLOURSELECT, self.OnPcolor2)
        pwidth2.Bind(wx.EVT_TEXT, self.OnPwidth2)
        pstyle2.Bind(wx.EVT_COMBOBOX, self.OnPstyle2)
        plegend2.Bind(wx.EVT_TEXT, self.OnPlegend2)
        pcolor3.Bind(csel.EVT_COLOURSELECT, self.OnPcolor3)
        pwidth3.Bind(wx.EVT_TEXT, self.OnPwidth3)
        pstyle3.Bind(wx.EVT_COMBOBOX, self.OnPstyle3)
        plegend3.Bind(wx.EVT_TEXT, self.OnPlegend3)
        ptcolor.Bind(csel.EVT_COLOURSELECT, self.OnPtColor)
        ptfill.Bind(wx.EVT_COMBOBOX, self.OnPtFill)
        ptsize.Bind(wx.EVT_TEXT, self.OnPtSize)
        pttype.Bind(wx.EVT_COMBOBOX, self.OnPtType)
        ptlegend.Bind(wx.EVT_TEXT, self.OnPtLegend)
        xtype.Bind(wx.EVT_COMBOBOX, self.OnXtype)
        xmin.Bind(wx.EVT_TEXT, self.OnXmin)
        xmax.Bind(wx.EVT_TEXT, self.OnXmax)
        xlog.Bind(wx.EVT_CHECKBOX, self.OnXlog)
        ytype.Bind(wx.EVT_COMBOBOX, self.OnYtype)
        ymin.Bind(wx.EVT_TEXT, self.OnYmin)
        ymax.Bind(wx.EVT_TEXT, self.OnYmax)
        ylog.Bind(wx.EVT_CHECKBOX, self.OnYlog)
        gridcolor.Bind(csel.EVT_COLOURSELECT, self.OnGridColor)
        gridshow.Bind(wx.EVT_CHECKBOX, self.OnGridshow)
        legendfontsize.Bind(wx.EVT_TEXT, self.OnLegendSize)
        legendshow.Bind(wx.EVT_CHECKBOX, self.OnLegendshow)

    def OnPcolor1(self, event):
        self.pcolor1 = event.GetValue()

    def OnPwidth1(self, event):
        self.pwidth1 = int(event.GetString())

    def OnPstyle1(self, event):
        self.pstyle1 = self.pstyledict[event.GetString()]

    def OnPlegend1(self, event):
        self.plegend1 = event.GetString()

    def OnPcolor2(self, event):
        self.pcolor2 = event.GetValue()

    def OnPwidth2(self, event):
        self.pwidth2 = int(event.GetString())

    def OnPstyle2(self, event):
        self.pstyle2 = self.pstyledict[event.GetString()]

    def OnPlegend2(self, event):
        self.plegend2 = event.GetString()

    def OnPcolor3(self, event):
        self.pcolor3 = event.GetValue()

    def OnPwidth3(self, event):
        self.pwidth3 = int(event.GetString())

    def OnPstyle3(self, event):
        self.pstyle3 = self.pstyledict[event.GetString()]

    def OnPlegend3(self, event):
        self.plegend3 = event.GetString()

    def OnPtColor(self, event):
        self.ptcolor = event.GetValue()

    def OnPtFill(self, event):
        self.ptfill = self.ptfilldict[event.GetString()]

    def OnPtSize(self, event):
        self.ptsize = int(event.GetString())

    def OnPtType(self, event):
        self.pttype = event.GetString()

    def OnPtLegend(self, event):
        self.ptlegend = event.GetString()

    def OnXtype(self, event):
        self.xtype = event.GetString()

    def OnXmin(self, event):
        self.xmin = float(event.GetString())

    def OnXmax(self, event):
        self.xmax = float(event.GetString())

    def OnXlog(self, event):
        self.xlog = event.IsChecked()

    def OnYtype(self, event):
        self.ytype = event.GetString()

    def OnYmin(self, event):
        self.ymin = float(event.GetString())

    def OnYmax(self, event):
        self.ymax = float(event.GetString())

    def OnYlog(self, event):
        self.ylog = event.IsChecked()

    def OnGridColor(self, event):
        self.gridcolor = event.GetValue()

    def OnGridshow(self, event):
        self.enablegrid = event.IsChecked()

    def OnLegendSize(self, event):
        self.legendfontsize = int(event.GetString())

    def OnLegendshow(self, event):
        self.enablelegend = event.IsChecked()




