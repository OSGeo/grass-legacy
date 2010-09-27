"""!
@package gcpmapdisp.py

@brief GIS map display canvas, with toolbar for various display
management functions, and additional toolbars (vector digitizer, 3d
view).

Classes:
- MapFrame


(C) 2006-2010 by the GRASS Development Team
This program is free software under the GNU General Public
License (>=v2). Read the file COPYING that comes with GRASS
for details.

@author Markus Metz
based on mapdisp.py by
@author Michael Barton
@author Jachym Cepicky
@author Martin Landa <landa.martin gmail.com>
"""

import os
import sys
import glob
import math
import tempfile
import copy

import globalvar
if not os.getenv("GRASS_WXBUNDLED"):
    globalvar.CheckForWx()
import wx
import wx.aui

try:
    import subprocess
except:
    CompatPath = os.path.join(globalvar.ETCWXDIR)
    sys.path.append(CompatPath)
    from compat import subprocess

gmpath = os.path.join(globalvar.ETCWXDIR, "icons")
sys.path.append(gmpath)

grassPath = os.path.join(globalvar.ETCDIR, "python")
sys.path.append(grassPath)

import render
import toolbars
import menuform
import gselect
import disp_print
import gcmd
import dbm
import histogram
import profile
import globalvar
import utils
import gdialogs
from grass.script import core as grass
from debug import Debug
from icon  import Icons
from preferences import globalSettings as UserSettings

from gcmd import Command
from mapdisp import BufferedWindow

import images
imagepath = images.__path__[0]
sys.path.append(imagepath)

class MapFrame(wx.Frame):
    """!Main frame for map display window. Drawing takes place in
    child double buffered drawing window.
    """
    def __init__(self, parent=None, id=wx.ID_ANY, title=_("GRASS GIS - Map display"),
                 style=wx.DEFAULT_FRAME_STYLE, toolbars=["gcpdisp"],
                 tree=None, notebook=None, lmgr=None, page=None,
                 Map=None, auimgr=None, **kwargs):
        """!Main map display window with toolbars, statusbar and
        DrawWindow

        @param toolbars array of activated toolbars, here 'gcpdisp'
        @param tree reference to layer tree
        @param notebook control book ID in Layer Manager
        @param lmgr Layer Manager
        @param page notebook page with layer tree
        @param Map instance of render.Map
        @param auimgs AUI manager
        @param kwargs wx.Frame attribures
        """
        self._layerManager = lmgr   # Layer Manager object
        self.gismanager    = lmgr    # GIS Manager object
        self.Map        = Map       # instance of render.Map
        self.tree       = tree      # Layer Manager layer tree object
        self.page       = page      # Notebook page holding the layer tree
        self.layerbook  = notebook  # Layer Manager layer tree notebook
        self.parent     = parent
        
        if not kwargs.has_key('name'):
            kwargs['name'] = 'GCPDisplay'
        wx.Frame.__init__(self, parent, id, title, style = style, **kwargs)
        
        # available cursors
        self.cursors = {
            # default: cross
            # "default" : wx.StockCursor(wx.CURSOR_DEFAULT),
            "default" : wx.StockCursor(wx.CURSOR_ARROW),
            "cross"   : wx.StockCursor(wx.CURSOR_CROSS),
            "hand"    : wx.StockCursor(wx.CURSOR_HAND),
            "pencil"  : wx.StockCursor(wx.CURSOR_PENCIL),
            "sizenwse": wx.StockCursor(wx.CURSOR_SIZENWSE)
            }
        
        #
        # set the size & system icon
        #
        self.SetClientSize(self.GetSize())
        self.iconsize = (16, 16)

        self.SetIcon(wx.Icon(os.path.join(globalvar.ETCICONDIR, 'grass_map.ico'), wx.BITMAP_TYPE_ICO))

        #
        # Fancy gui
        #
        self._mgr = wx.aui.AuiManager(self)

        #
        # Add toolbars
        #
        self.toolbars = { 'map' : None,
                          'vdigit' : None,
                          'georect' : None, 
                          'gcpdisp' : None, 
                          'gcpman' : None, 
                          'nviz' : None }

        for toolb in toolbars:
            self.AddToolbar(toolb)

        self.activemap = self.toolbars['gcpdisp'].togglemap
        self.activemap.SetSelection(0)
        self.SrcMap        = self.grwiz.SrcMap       # instance of render.Map
        self.TgtMap        = self.grwiz.TgtMap       # instance of render.Map
        self._mgr.SetDockSizeConstraint(0.5, 0.5)

        #
        # Add statusbar
        #
        self.statusbar = self.CreateStatusBar(number=4, style=0)
        self.statusbar.SetStatusWidths([-5, -2, -1, -1])
        self.toggleStatus = wx.Choice(self.statusbar, wx.ID_ANY,
                                  choices = [_("Coordinates"),
                                             _("Extent"),
                                             _("Comp. region"),
                                             _("Show comp. extent"),
                                             _("Display mode"),
                                             _("Display geometry"),
                                             _("Map scale"),
                                             _("Go to GCP No."),
                                             _("RMS error")])
        # set StatusBar to Go to GCP No.
        self.toggleStatus.SetSelection(7)

        self.statusbar.Bind(wx.EVT_CHOICE, self.OnToggleStatus, self.toggleStatus)
        # auto-rendering checkbox
        self.autoRender = wx.CheckBox(parent=self.statusbar, id=wx.ID_ANY,
                                                  label=_("Render"))
        self.statusbar.Bind(wx.EVT_CHECKBOX, self.OnToggleRender, self.autoRender)
        self.autoRender.SetValue(UserSettings.Get(group='display',
                                                              key='autoRendering',
                                                              subkey='enabled'))
        self.autoRender.SetToolTip(wx.ToolTip (_("Enable/disable auto-rendering")))
        # show region
        self.showRegion = wx.CheckBox(parent=self.statusbar, id=wx.ID_ANY,
                                                  label=_("Show computational extent"))
        self.statusbar.Bind(wx.EVT_CHECKBOX, self.OnToggleShowRegion, self.showRegion)
        
        self.showRegion.SetValue(False)
        self.showRegion.Hide()
        self.showRegion.SetToolTip(wx.ToolTip (_("Show/hide computational "
                                                             "region extent (set with g.region). "
                                                             "Display region drawn as a blue box inside the "
                                                             "computational region, "
                                                             "computational region inside a display region "
                                                             "as a red box).")))
        # set resolution
        self.compResolution = wx.CheckBox(parent=self.statusbar, id=wx.ID_ANY,
                                                      label=_("Constrain display resolution to computational settings"))
        self.statusbar.Bind(wx.EVT_CHECKBOX, self.OnToggleResolution, self.compResolution)
        self.compResolution.SetValue(UserSettings.Get(group='display', key='compResolution', subkey='enabled'))
        self.compResolution.Hide()
        self.compResolution.SetToolTip(wx.ToolTip (_("Constrain display resolution "
                                                                 "to computational region settings. "
                                                                 "Default value for new map displays can "
                                                                 "be set up in 'User GUI settings' dialog.")))
        # map scale
        self.mapScale = wx.ComboBox(parent = self.statusbar, id = wx.ID_ANY,
                                                    style = wx.TE_PROCESS_ENTER,
                                                    size=(150, -1))
        self.mapScale.SetItems(['1:1000',
                                                '1:5000',
                                                '1:10000',
                                                '1:25000',
                                                '1:50000',
                                                '1:100000',
                                                '1:1000000'])
        self.mapScale.Hide()
        self.statusbar.Bind(wx.EVT_TEXT_ENTER, self.OnChangeMapScale, self.mapScale)
        self.statusbar.Bind(wx.EVT_COMBOBOX, self.OnChangeMapScale, self.mapScale)

        # go to GCP
        self.gotogcp = wx.SpinCtrl(parent=self.statusbar, id=wx.ID_ANY,
                             min=0)
        self.statusbar.Bind(wx.EVT_SPINCTRL, self.OnGoTo, self.gotogcp)
        self.gotogcp.Hide()
        self.statusbar.Bind(wx.EVT_TEXT_ENTER, self.OnGoTo, self.gotogcp)

        # mask
        self.maskInfo = wx.StaticText(parent = self.statusbar, id = wx.ID_ANY,
                                                  label = '')
        self.maskInfo.SetForegroundColour(wx.Colour(255, 0, 0))
        
        # on-render gauge
        self.onRenderGauge = wx.Gauge(parent=self.statusbar, id=wx.ID_ANY,
                                      range=0, style=wx.GA_HORIZONTAL)
        self.onRenderGauge.Hide()
        
        self.StatusbarReposition() # reposition statusbar

        #
        # Init map display (buffered DC & set default cursor)
        #
        self.grwiz.SwitchEnv('source')
        self.SrcMapWindow = BufferedWindow(self, id=wx.ID_ANY,
                                          Map=self.SrcMap, tree=self.tree, gismgr=self._layerManager)

        self.grwiz.SwitchEnv('target')
        self.TgtMapWindow = BufferedWindow(self, id=wx.ID_ANY,
                                          Map=self.TgtMap, tree=self.tree, gismgr=self._layerManager)
        self.MapWindow = self.SrcMapWindow
        self.Map = self.SrcMap
        self.SrcMapWindow.Bind(wx.EVT_MOTION, self.OnMotion)
        self.TgtMapWindow.Bind(wx.EVT_MOTION, self.OnMotion)
        self.SrcMapWindow.SetCursor(self.cursors["cross"])
        self.TgtMapWindow.SetCursor(self.cursors["cross"])

        #
        # initialize region values
        #
        self.__InitDisplay() 

        #
        # Bind various events
        #
        self.Bind(wx.EVT_ACTIVATE, self.OnFocus)
        self.Bind(render.EVT_UPDATE_PRGBAR, self.OnUpdateProgress)
        self.Bind(wx.EVT_SIZE,     self.OnDispResize)
        self.activemap.Bind(wx.EVT_CHOICE, self.OnUpdateActive)
        
        #
        # Update fancy gui style
        #
        # AuiManager wants a CentrePane , workaround to get two equally sized windows
        self.list = self.CreateGCPList()

        #self.SrcMapWindow.SetSize((300, 300))
        #self.TgtMapWindow.SetSize((300, 300))
        self.list.SetSize((100, 150))
        self._mgr.AddPane(self.list, wx.aui.AuiPaneInfo().
                  Name("gcplist").Caption(_("GCP List")).LeftDockable(False).
                  RightDockable(False).PinButton().FloatingSize((600,200)).
                  CloseButton(False).DestroyOnClose(True).
                  Top().Layer(1).MinSize((200,100)))
        self._mgr.AddPane(self.SrcMapWindow, wx.aui.AuiPaneInfo().
                  Name("source").Caption(_("Source Display")).Dockable(False).
                  CloseButton(False).DestroyOnClose(True).Floatable(False).
                  Centre())
        self._mgr.AddPane(self.TgtMapWindow, wx.aui.AuiPaneInfo().
                  Name("target").Caption(_("Target Display")).Dockable(False).
                  CloseButton(False).DestroyOnClose(True).Floatable(False).
                  Right().Layer(0))

        srcwidth, srcheight = self.SrcMapWindow.GetSize()
        tgtwidth, tgtheight = self.TgtMapWindow.GetSize()
        tgtwidth = (srcwidth + tgtwidth) / 2
        self._mgr.GetPane("target").Hide()
        self._mgr.Update()
        self._mgr.GetPane("source").BestSize((tgtwidth, tgtheight))
        self._mgr.GetPane("target").BestSize((tgtwidth, tgtheight))
        if self.show_target:
            self._mgr.GetPane("target").Show()
        else:
            self.activemap.Enable(False)

        # called by GCPWizard
        #self._mgr.Update()

        #
        # Init print module and classes
        #
        self.printopt = disp_print.PrintOptions(self, self.MapWindow)
        
        # set active map
        self.MapWindow = self.SrcMapWindow
        self.Map = self.SrcMap
        #
        # Init zoom history
        #
        self.TgtMapWindow.ZoomHistory(self.Map.region['n'],
                                   self.Map.region['s'],
                                   self.Map.region['e'],
                                   self.Map.region['w'])
        self.MapWindow.ZoomHistory(self.Map.region['n'],
                                   self.Map.region['s'],
                                   self.Map.region['e'],
                                   self.Map.region['w'])

        #
        # Re-use dialogs
        #
        self.dialogs = {}
        self.dialogs['attributes'] = None
        self.dialogs['category'] = None
        self.dialogs['barscale'] = None
        self.dialogs['legend'] = None

        self.decorationDialog = None # decoration/overlays

    def AddToolbar(self, name):
        """!Add defined toolbar to the window
        
        Currently known toolbars are:
         - 'map'     - basic map toolbar
         - 'vdigit'  - vector digitizer
         - 'gcpdisp' - GCP Manager, Display
         - 'gcpman'  - GCP Manager, points management
         - 'georect' - georectifier
         - 'nviz'    - 3D view mode
        """
        # default toolbars for GCP display
        if name == "gcpdisp":
            self.toolbars['gcpdisp'] = toolbars.GCPDisplayToolbar(self)

            self._mgr.AddPane(self.toolbars['gcpdisp'].toolbar,
                              wx.aui.AuiPaneInfo().
                              Name("gcpdisplaytoolbar").Caption(_("GCP Display toolbar")).
                              ToolbarPane().Top().
                              LeftDockable(False).RightDockable(False).
                              BottomDockable(False).TopDockable(True).
                              CloseButton(False).Layer(2))

            if self.show_target == False:
                self.toolbars['gcpdisp'].toolbar.EnableTool(self.toolbars['gcpdisp'].zoommenu, enable = False)

            self.toolbars['gcpman'] = toolbars.GCPManToolbar(self)

            self._mgr.AddPane(self.toolbars['gcpman'].toolbar,
                              wx.aui.AuiPaneInfo().
                              Name("gcpmanagertoolbar").Caption(_("GCP Manager toolbar")).
                              ToolbarPane().Top().Row(1).
                              LeftDockable(False).RightDockable(False).
                              BottomDockable(False).TopDockable(True).
                              CloseButton(False).Layer(2))

        # georectifier
        elif name == "georect":
            self.toolbars['georect'] = toolbars.GRToolbar(self, self.Map)

            self._mgr.AddPane(self.toolbars['georect'].toolbar,
                  wx.aui.AuiPaneInfo().
                  Name("georecttoolbar").Caption(_("Georectification toolbar")).
                  ToolbarPane().Top().
                  LeftDockable(False).RightDockable(False).
                  BottomDockable(False).TopDockable(True).
                  CloseButton(False).Layer(2))

        self._mgr.Update()

    def __InitDisplay(self):
        """
        Initialize map display, set dimensions and map region
        """
        self.width, self.height = self.GetClientSize()

        Debug.msg(2, "MapFrame.__InitDisplay():")
        self.grwiz.SwitchEnv('source')
        self.SrcMap.ChangeMapSize(self.GetClientSize())
        self.SrcMap.region = self.SrcMap.GetRegion() # g.region -upgc
        self.grwiz.SwitchEnv('target')
        self.TgtMap.ChangeMapSize(self.GetClientSize())
        self.TgtMap.region = self.TgtMap.GetRegion() # g.region -upgc
        # self.SrcMap.SetRegion() # adjust region to match display window
        # self.TgtMap.SetRegion() # adjust region to match display window

    def OnUpdateProgress(self, event):
        """
        Update progress bar info
        """
        self.onRenderGauge.SetValue(event.value)
        
        event.Skip()
        
    def OnFocus(self, event):
        """
        Change choicebook page to match display.
        Or set display for georectifying
        """
        if self._layerManager and \
                self._layerManager.georectifying:
            # in georectifying session; display used to get geographic
            # coordinates for GCPs
            self.OnPointer(event)
        elif self._layerManager and \
                self._layerManager.gcpmanagement:
            # in georectifying session; display used to get geographic
            # coordinates for GCPs
            self.OnPointer(event)
            self.MapWindow.SetFocus()
        
        event.Skip()

    def OnMotion(self, event):
        """
        Mouse moved
        Track mouse motion and update status bar
        """
        # update statusbar if required
        if self.toggleStatus.GetSelection() == 0: # Coordinates
            e, n = self.MapWindow.Pixel2Cell(event.GetPositionTuple())
            if self.toolbars['vdigit'] and \
                    self.toolbars['vdigit'].GetAction() == 'addLine' and \
                    self.toolbars['vdigit'].GetAction('type') in ('line', 'boundary') and \
                    len(self.MapWindow.polycoords) > 0:
                # for linear feature show segment and total length
                distance_seg = self.MapWindow.Distance(self.MapWindow.polycoords[-1],
                                                       (e, n), screen=False)[0]
                distance_tot = distance_seg
                for idx in range(1, len(self.MapWindow.polycoords)):
                    distance_tot += self.MapWindow.Distance(self.MapWindow.polycoords[idx-1],
                                                            self.MapWindow.polycoords[idx],
                                                            screen=False )[0]
                self.statusbar.SetStatusText("%.2f, %.2f (seg: %.2f; tot: %.2f)" % \
                                                 (e, n, distance_seg, distance_tot), 0)
            else:
                if self.Map.projinfo['proj'] == 'll':
                    self.statusbar.SetStatusText("%s" % utils.Deg2DMS(e, n), 0)
                else:
                    self.statusbar.SetStatusText("%.2f, %.2f" % (e, n), 0)

        event.Skip()

    def OnDraw(self, event):
        """!Re-display current map composition
        """
        self.MapWindow.UpdateMap(render = False)
        
    def OnRender(self, event):
        """!Re-render map composition (each map layer)
        """
        # delete tmp map layers (queries)
        qlayer = self.Map.GetListOfLayers(l_name=globalvar.QUERYLAYER)
        for layer in qlayer:
            self.Map.DeleteLayer(layer)

        # delete tmp lines
        if self.MapWindow.mouse["use"] in ("measure",
                                           "profile"):
            self.MapWindow.polycoords = []
            self.MapWindow.ClearLines()
        
        if self.toolbars['gcpdisp']:
            self.SrcMapWindow.UpdateMap(render=True)
            self.TgtMapWindow.UpdateMap(render=True)
            self._mgr.Update()
        else:
            self.MapWindow.UpdateMap(render=True)
        
        # update statusbar
        self.StatusbarUpdate()

    def OnPointer(self, event):
        """!Pointer button clicked
        """
        if self.toolbars['map']:
            if event:
                self.toolbars['map'].OnTool(event)
            self.toolbars['map'].action['desc'] = ''
        
        self.MapWindow.mouse['use'] = "pointer"
        self.MapWindow.mouse['box'] = "point"

        # change the cursor
        if self._layerManager.gcpmanagement:
            self.SrcMapWindow.SetCursor(self.cursors["cross"])
            self.SrcMapWindow.mouse['use'] = "pointer"
            self.SrcMapWindow.mouse['box'] = "point"
            self.TgtMapWindow.SetCursor(self.cursors["cross"])
            self.TgtMapWindow.mouse['use'] = "pointer"
            self.TgtMapWindow.mouse['box'] = "point"
        
        elif self._layerManager and self._layerManager.georectifying:
            self.MapWindow.SetCursor(self.cursors["cross"])
        
        else:
            self.MapWindow.SetCursor(self.cursors["default"])

    def OnZoomIn(self, event):
        """
        Zoom in the map.
        Set mouse cursor, zoombox attributes, and zoom direction
        """
        if self.toolbars['map']:
            self.toolbars['map'].OnTool(event)
            self.toolbars['map'].action['desc'] = ''
        
        self.MapWindow.mouse['use'] = "zoom"
        self.MapWindow.mouse['box'] = "box"
        self.MapWindow.zoomtype = 1
        self.MapWindow.pen = wx.Pen(colour='Red', width=2, style=wx.SHORT_DASH)
        
        # change the cursor
        self.MapWindow.SetCursor(self.cursors["cross"])

        if self._layerManager and self._layerManager.gcpmanagement:
            if self.MapWindow == self.SrcMapWindow:
                win = self.TgtMapWindow
            elif self.MapWindow == self.TgtMapWindow:
                win = self.SrcMapWindow

            win.mouse['use'] = "zoom"
            win.mouse['box'] = "box"
            win.zoomtype = 1
            win.pen = wx.Pen(colour='Red', width=2, style=wx.SHORT_DASH)
            
            # change the cursor
            win.SetCursor(self.cursors["cross"])

    def OnZoomOut(self, event):
        """
        Zoom out the map.
        Set mouse cursor, zoombox attributes, and zoom direction
        """
        if self.toolbars['map']:
            self.toolbars['map'].OnTool(event)
            self.toolbars['map'].action['desc'] = ''
        
        self.MapWindow.mouse['use'] = "zoom"
        self.MapWindow.mouse['box'] = "box"
        self.MapWindow.zoomtype = -1
        self.MapWindow.pen = wx.Pen(colour='Red', width=2, style=wx.SHORT_DASH)
        
        # change the cursor
        self.MapWindow.SetCursor(self.cursors["cross"])

        if self._layerManager and self._layerManager.gcpmanagement:
            if self.MapWindow == self.SrcMapWindow:
                win = self.TgtMapWindow
            elif self.MapWindow == self.TgtMapWindow:
                win = self.SrcMapWindow

            win.mouse['use'] = "zoom"
            win.mouse['box'] = "box"
            win.zoomtype = -1
            win.pen = wx.Pen(colour='Red', width=2, style=wx.SHORT_DASH)
            
            # change the cursor
            win.SetCursor(self.cursors["cross"])

    def OnZoomBack(self, event):
        """
        Zoom last (previously stored position)
        """
        self.MapWindow.ZoomBack()

    def OnPan(self, event):
        """
        Panning, set mouse to drag
        """
        if self.toolbars['map']:
            self.toolbars['map'].OnTool(event)
            self.toolbars['map'].action['desc'] = ''
        
        self.MapWindow.mouse['use'] = "pan"
        self.MapWindow.mouse['box'] = "pan"
        self.MapWindow.zoomtype = 0
        
        # change the cursor
        self.MapWindow.SetCursor(self.cursors["hand"])

        if self._layerManager and self._layerManager.gcpmanagement:
            if self.MapWindow == self.SrcMapWindow:
                win = self.TgtMapWindow
            elif self.MapWindow == self.TgtMapWindow:
                win = self.SrcMapWindow

            win.mouse['use'] = "pan"
            win.mouse['box'] = "pan"
            win.zoomtype = 0
            
            # change the cursor
            win.SetCursor(self.cursors["hand"])

    def OnErase(self, event):
        """
        Erase the canvas
        """
        self.MapWindow.EraseMap()

        if self._layerManager and self._layerManager.gcpmanagement:
            if self.MapWindow == self.SrcMapWindow:
                win = self.TgtMapWindow
            elif self.MapWindow == self.TgtMapWindow:
                win = self.SrcMapWindow

            win.EraseMap()

    def OnZoomRegion(self, event):
        """
        Zoom to region
        """
        self.Map.getRegion()
        self.Map.getResolution()
        self.UpdateMap()
        # event.Skip()

    def OnAlignRegion(self, event):
        """
        Align region
        """
        if not self.Map.alignRegion:
            self.Map.alignRegion = True
        else:
            self.Map.alignRegion = False
        # event.Skip()

    def OnToggleRender(self, event):
        """
        Enable/disable auto-rendering
        """
        if self.autoRender.GetValue():
            self.OnRender(None)

    def OnToggleShowRegion(self, event):
        """
        Show/Hide extent in map canvas
        """
        if self.showRegion.GetValue():
            # show extent
            self.MapWindow.regionCoords = []
        else:
            del self.MapWindow.regionCoords

        # redraw map if auto-rendering is enabled
        if self.autoRender.GetValue():
            self.OnRender(None)

    def OnToggleResolution(self, event):
        """
        Use resolution of computation region settings
        for redering image instead of display resolution
        """
        # redraw map if auto-rendering is enabled
        if self.autoRender.GetValue():
            self.OnRender(None)
        
    def OnToggleStatus(self, event):
        """
        Toggle status text
        """
        self.StatusbarUpdate()

    def OnChangeMapScale(self, event):
        """
        Map scale changed by user
        """
        scale = event.GetString()

        try:
            if scale[:2] != '1:':
                raise ValueError
            value = int(scale[2:])
        except ValueError:
            self.mapScale.SetValue('1:%ld' % int(self.mapScaleValue))
            return

        dEW = value * (self.Map.region['cols'] / self.ppm[0])
        dNS = value * (self.Map.region['rows'] / self.ppm[1])
        self.Map.region['n'] = self.Map.region['center_northing'] + dNS / 2.
        self.Map.region['s'] = self.Map.region['center_northing'] - dNS / 2.
        self.Map.region['w'] = self.Map.region['center_easting']  - dEW / 2.
        self.Map.region['e'] = self.Map.region['center_easting']  + dEW / 2.
        
        # add to zoom history
        self.MapWindow.ZoomHistory(self.Map.region['n'], self.Map.region['s'],
                                   self.Map.region['e'], self.Map.region['w'])
        
        # redraw a map
        self.MapWindow.UpdateMap()
        self.mapScale.SetFocus()
        
    def OnGoTo(self, event):
        """
        Go to GCP No
        """
        #GCPNo = int(event.GetString())
        GCPNo = self.gotogcp.GetValue()

        if GCPNo < 0 or GCPNo > len(self.mapcoordlist):
            wx.MessageBox(parent=self,
                  message="%s 1 - %s." % (_("Valid Range:"),
                                 len(self.mapcoordlist)),
                  caption=_("Invalid GCP Number"), style=wx.OK | wx.ICON_ERROR | wx.CENTRE)
            return

        if GCPNo == 0:
            return

        self.list.selectedkey = GCPNo
        self.list.selected = self.list.FindItemData(-1, GCPNo)
        self.list.render = False
        self.list.SetItemState(self.list.selected,
                          wx.LIST_STATE_SELECTED,
                          wx.LIST_STATE_SELECTED)
        self.list.render = True
        
        # Source MapWindow:
        begin = (self.mapcoordlist[GCPNo][1], self.mapcoordlist[GCPNo][2])
        begin = self.SrcMapWindow.Cell2Pixel(begin)
        end = begin
        self.SrcMapWindow.Zoom(begin, end, 0)

        # redraw map
        self.SrcMapWindow.UpdateMap()

        if self.show_target:
            # Target MapWindow:
            begin = (self.mapcoordlist[GCPNo][3], self.mapcoordlist[GCPNo][4])
            begin = self.TgtMapWindow.Cell2Pixel(begin)
            end = begin
            self.TgtMapWindow.Zoom(begin, end, 0)

            # redraw map
            self.TgtMapWindow.UpdateMap()

        self.gotogcp.SetFocus()
        
    def StatusbarUpdate(self):
        """!Update statusbar content"""

        self.showRegion.Hide()
        self.compResolution.Hide()
        self.mapScale.Hide()
        self.gotogcp.Hide()
        self.mapScaleValue = self.ppm = None

        if self.toggleStatus.GetSelection() == 0: # Coordinates
            self.statusbar.SetStatusText("", 0)
            # enable long help
            self.StatusbarEnableLongHelp()

        elif self.toggleStatus.GetSelection() == 1: # Extent
            self.statusbar.SetStatusText("%.2f - %.2f, %.2f - %.2f" %
                                         (self.Map.region["w"], self.Map.region["e"],
                                          self.Map.region["s"], self.Map.region["n"]), 0)
            # enable long help
            self.StatusbarEnableLongHelp()

        elif self.toggleStatus.GetSelection() == 2: # Comp. region
            compregion = self.Map.GetRegion()
            self.statusbar.SetStatusText("%.2f - %.2f, %.2f - %.2f (%.2f, %.2f)" %
                                         (compregion["w"], compregion["e"],
                                          compregion["s"], compregion["n"],
                                          compregion["ewres"], compregion["nsres"]), 0)
            # enable long help
            self.StatusbarEnableLongHelp()

        elif self.toggleStatus.GetSelection() == 3: # Show comp. extent
            self.statusbar.SetStatusText("", 0)
            self.showRegion.Show()
            # disable long help
            self.StatusbarEnableLongHelp(False)

        elif self.toggleStatus.GetSelection() == 4: # Display mode
            self.statusbar.SetStatusText("", 0)
            self.compResolution.Show()
            # disable long help
            self.StatusbarEnableLongHelp(False)

        elif self.toggleStatus.GetSelection() == 5: # Display geometry
            self.statusbar.SetStatusText("rows=%d; cols=%d; nsres=%.2f; ewres=%.2f" %
                                         (self.Map.region["rows"], self.Map.region["cols"],
                                          self.Map.region["nsres"], self.Map.region["ewres"]), 0)
            # enable long help
            self.StatusbarEnableLongHelp()

        elif self.toggleStatus.GetSelection() == 6: # Map scale
            # TODO: need to be fixed...
            ### screen X region problem
            ### user should specify ppm
            dc = wx.ScreenDC()
            dpSizePx = wx.DisplaySize()   # display size in pixels
            dpSizeMM = wx.DisplaySizeMM() # display size in mm (system)
            dpSizeIn = (dpSizeMM[0] / 25.4, dpSizeMM[1] / 25.4) # inches
            sysPpi  = dc.GetPPI()
            comPpi = (dpSizePx[0] / dpSizeIn[0],
                      dpSizePx[1] / dpSizeIn[1])

            ppi = comPpi                  # pixel per inch
            self.ppm = ((ppi[0] / 2.54) * 100, # pixel per meter
                        (ppi[1] / 2.54) * 100)

            Debug.msg(4, "MapFrame.StatusbarUpdate(mapscale): size: px=%d,%d mm=%f,%f "
                      "in=%f,%f ppi: sys=%d,%d com=%d,%d; ppm=%f,%f" % \
                          (dpSizePx[0], dpSizePx[1], dpSizeMM[0], dpSizeMM[1],
                           dpSizeIn[0], dpSizeIn[1],
                           sysPpi[0], sysPpi[1], comPpi[0], comPpi[1],
                           self.ppm[0], self.ppm[1]))

            region = self.Map.region

            heightCm = region['rows'] / self.ppm[1] * 100
            widthCm  = region['cols'] / self.ppm[0] * 100

            Debug.msg(4, "MapFrame.StatusbarUpdate(mapscale): width_cm=%f, height_cm=%f" %
                      (widthCm, heightCm))

            xscale = (region['e'] - region['w']) / (region['cols'] / self.ppm[0])
            yscale = (region['n'] - region['s']) / (region['rows'] / self.ppm[1])
            scale = (xscale + yscale) / 2.
            
            Debug.msg(3, "MapFrame.StatusbarUpdate(mapscale): xscale=%f, yscale=%f -> scale=%f" % \
                          (xscale, yscale, scale))

            self.statusbar.SetStatusText("")
            try:
                self.mapScale.SetValue("1:%ld" % (scale + 0.5))
            except TypeError:
                pass
            self.mapScaleValue = scale
            self.mapScale.Show()

            # disable long help
            self.StatusbarEnableLongHelp(False)

        elif self.toggleStatus.GetSelection() == 7: # go to

            self.statusbar.SetStatusText("")
            max = self.list.GetItemCount()
            if max < 1:
                max = 1
            self.gotogcp.SetRange(0, max)
            self.gotogcp.Show()

            # disable long help
            self.StatusbarEnableLongHelp(False)
        
        elif self.toggleStatus.GetSelection() == 8: # RMS error
            self.statusbar.SetStatusText(_("Forward: %s, Backward: %s") %
                                         (self.fwd_rmserror, self.bkw_rmserror))
            
        else:
            self.statusbar.SetStatusText("", 1)

    def StatusbarEnableLongHelp(self, enable=True):
        """!Enable/disable toolbars long help"""
        for toolbar in self.toolbars.itervalues():
            if toolbar:
                toolbar.EnableLongHelp(enable)
                
    def StatusbarReposition(self):
        """!Reposition checkbox in statusbar"""
        # reposition checkbox
        widgets = [(0, self.showRegion),
                   (0, self.compResolution),
                   (0, self.mapScale),
                   (0, self.onRenderGauge),
                   (0, self.gotogcp),
                   (1, self.toggleStatus),
                   (2, self.maskInfo),
                   (3, self.autoRender)]
        for idx, win in widgets:
            rect = self.statusbar.GetFieldRect(idx)
            wWin, hWin = win.GetBestSize()
            if idx == 0: # show region / mapscale / process bar
                # -> size
                if win == self.onRenderGauge:
                    wWin = rect.width - 6
                # -> position
                # if win == self.showRegion:
                # x, y = rect.x + rect.width - wWin, rect.y - 1
                # align left
                # else:
                x, y = rect.x + 3, rect.y - 1
                w, h = wWin, rect.height + 2
            else: # choice || auto-rendering
                x, y = rect.x, rect.y - 1
                w, h = rect.width, rect.height + 2
                if idx == 1: # choice
                    h = hWin
                elif idx == 2: # mask
                    x += 5
                    y += 4
                elif idx == 3: # render
                    x += 5

            win.SetPosition((x, y))
            win.SetSize((w, h))

    def SaveToFile(self, event):
        """!Save map to image
        """
        img = self.MapWindow.img
        if not img:
            gcmd.GMessage(parent = self,
                 message = _("Nothing to render (empty map). Operation canceled."),
                 msgType = 'info')
            return
        filetype, ltype = gdialogs.GetImageHandlers(img)

        # get size
        dlg = gdialogs.ImageSizeDialog(self)
        dlg.CentreOnParent()
        if dlg.ShowModal() != wx.ID_OK:
            dlg.Destroy()
            return
        width, height = dlg.GetValues()
        dlg.Destroy()
        
        # get filename
        dlg = wx.FileDialog(parent = self,
                            message = _("Choose a file name to save the image "
                                        "(no need to add extension)"),
                            wildcard = filetype,
                            style=wx.SAVE | wx.FD_OVERWRITE_PROMPT)
        
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()
            if not path:
                dlg.Destroy()
                return
            
            base, ext = os.path.splitext(path)
            fileType = ltype[dlg.GetFilterIndex()]['type']
            extType  = ltype[dlg.GetFilterIndex()]['ext']
            if ext != extType:
                path = base + '.' + extType
            
            self.MapWindow.SaveToFile(path, fileType,
                                      width, height)
            
        dlg.Destroy()

    def PrintMenu(self, event):
        """
        Print options and output menu for map display
        """
        point = wx.GetMousePosition()
        printmenu = wx.Menu()
        # Add items to the menu
        setup = wx.MenuItem(printmenu, wx.ID_ANY, _('Page setup'))
        printmenu.AppendItem(setup)
        self.Bind(wx.EVT_MENU, self.printopt.OnPageSetup, setup)

        preview = wx.MenuItem(printmenu, wx.ID_ANY, _('Print preview'))
        printmenu.AppendItem(preview)
        self.Bind(wx.EVT_MENU, self.printopt.OnPrintPreview, preview)

        doprint = wx.MenuItem(printmenu, wx.ID_ANY, _('Print display'))
        printmenu.AppendItem(doprint)
        self.Bind(wx.EVT_MENU, self.printopt.OnDoPrint, doprint)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(printmenu)
        printmenu.Destroy()

    def GetRender(self):
        """!Returns current instance of render.Map()
        """
        return self.Map

    def GetWindow(self):
        """!Get map window"""
        return self.MapWindow
    
    def OnQueryDisplay(self, event):
        """!Query currrent raster/vector map layers (display mode)
        """
        if self.toolbars['map'].GetAction() == 'displayAttrb': # select previous action
            self.toolbars['map'].SelectDefault(event)
            return

        self.toolbars['map'].action['desc'] = 'displayAttrb'
        
        # switch GIS Manager to output console to show query results
        self._layerManager.notebook.SetSelection(1)

        self.MapWindow.mouse['use'] = "query"
        self.MapWindow.mouse['box'] = "point"
        self.MapWindow.zoomtype = 0

        # change the cursor
        self.MapWindow.SetCursor(self.cursors["cross"])

    def OnQueryModify(self, event):
        """
        Query vector map layer (edit mode)
        """
        if self.toolbars['map'].GetAction() == 'modifyAttrb': # select previous action
            self.toolbars['map'].SelectDefault(event)
            return
        
        self.toolbars['map'].action['desc'] = 'modifyAttrb'
        
        self.MapWindow.mouse['use'] = "queryVector"
        self.MapWindow.mouse['box'] = "point"
        self.MapWindow.pen = wx.Pen(colour='Red', width=2, style=wx.SHORT_DASH)
        self.MapWindow.zoomtype = 0

        # change the cursor
        self.MapWindow.SetCursor(self.cursors["cross"])
        
    def QueryMap(self, x, y):
        """!Query map layer features

        Currently only raster and vector map layers are supported.
        
        @param x,y coordinates
        """
        #set query snap distance for v.what at mapunit equivalent of 10 pixels
        qdist = 10.0 * ((self.Map.region['e'] - self.Map.region['w']) / self.Map.width)
        east, north = self.MapWindow.Pixel2Cell((x, y))

        num = 0
        for layer in self.tree.GetSelections():
            type = self.tree.GetPyData(layer)[0]['maplayer'].GetType()
            if type in ('raster', 'rgb', 'his',
                        'vector', 'thememap', 'themechart'):
                num += 1
        
        if num < 1:
            dlg = wx.MessageDialog(parent = self,
                                   message = _('No raster or vector map layer selected for querying.'),
                                   caption = _('No map layer selected'),
                                   style = wx.OK | wx.ICON_INFORMATION | wx.CENTRE)
            dlg.ShowModal()
            dlg.Destroy()
            return
        
        mapname = None
        raststr = ''
        vectstr = ''
        rcmd = ['r.what', '--q']
        vcmd = ['v.what', '--q']
        for layer in self.tree.GetSelections():
            type = self.tree.GetPyData(layer)[0]['maplayer'].GetType()
            dcmd = self.tree.GetPyData(layer)[0]['cmd']
            name = utils.GetLayerNameFromCmd(dcmd)
            if name == '':
                continue
            if type in ('raster', 'rgb', 'his'):
                raststr += "%s," % name
            elif type in ('vector', 'thememap', 'themechart'):
                vectstr += "%s," % name

        # use display region settings instead of computation region settings
        self.tmpreg = os.getenv("GRASS_REGION")
        os.environ["GRASS_REGION"] = self.Map.SetRegion(windres=False)
        
        # build query commands for any selected rasters and vectors
        if raststr != '':
            rcmd.append('-f')
            rcmd.append('input=%s' % raststr.rstrip(','))
            rcmd.append('east_north=%f,%f' % (float(east), float(north)))
        
        if vectstr != '':
            # check for vector maps open to be edited
            digitToolbar = self.toolbars['vdigit']
            if digitToolbar:
                map = digitToolbar.GetLayer().GetName()
                vect = []
                for vector in vectstr.split(','):
                    if map == vector:
                        self._layerManager.goutput.WriteWarning("Vector map <%s> "
                                                                "opened for editing - skipped." % map)
                        continue
                    vect.append(vector)
                vectstr = ','.join(vect)
            
            if len(vectstr) <= 1:
                self._layerManager.goutput.WriteCmdLog("Nothing to query.")
                return
            
            vcmd.append('-a')
            vcmd.append('map=%s' % vectstr.rstrip(','))
            vcmd.append('east_north=%f,%f' % (float(east), float(north)))
            vcmd.append('distance=%f' % float(qdist))
        
        # parse query command(s)
        if self._layerManager:
            if raststr:
                self._layerManager.goutput.RunCmd(rcmd,
                                                  compReg=False,
                                                  onDone = self._QueryMapDone)
            if vectstr:
                self._layerManager.goutput.RunCmd(vcmd,
                                                  onDone = self._QueryMapDone)
        else:
            if raststr:
                gcmd.RunCommand(rcmd)
            if vectstr:
                gcmd.RunCommand(vcmd)
        
    def _QueryMapDone(self, returncode):
        """!Restore settings after querying (restore GRASS_REGION)

        @param returncode command return code
        """
        if hasattr(self, "tmpreg"):
            if self.tmpreg:
                os.environ["GRASS_REGION"] = self.tmpreg
            elif os.environ.has_key('GRASS_REGION'):
                del os.environ["GRASS_REGION"]
        elif os.environ.has_key('GRASS_REGION'):
            del os.environ["GRASS_REGION"]
        
        if hasattr(self, "tmpreg"):
            del self.tmpreg
        
    def QueryVector(self, x, y):
        """
        Query vector map layer features

        Attribute data of selected vector object are displayed in GUI dialog.
        Data can be modified (On Submit)
        """
        if not self.tree.layer_selected or \
                self.tree.GetPyData(self.tree.layer_selected)[0]['type'] != 'vector':
            wx.MessageBox(parent=self,
                          message=_("No vector map selected for querying."),
                          caption=_("Vector querying"),
                          style=wx.OK | wx.ICON_INFORMATION | wx.CENTRE)
            return
        
        if self.tree.GetPyData(self.tree.layer_selected)[0]['maplayer'].GetMapset() != \
                grass.gisenv()['MAPSET']:
            wx.MessageBox(parent=self,
                          message=_("Only vector map from the current mapset can be modified."),
                          caption=_("Vector querying"),
                          style=wx.OK | wx.ICON_INFORMATION | wx.CENTRE)
            return
        
        posWindow = self.ClientToScreen((x + self.MapWindow.dialogOffset,
                                         y + self.MapWindow.dialogOffset))

        qdist = 10.0 * ((self.Map.region['e'] - self.Map.region['w']) /
                        self.Map.width)
        
        east, north = self.MapWindow.Pixel2Cell((x, y))
        
        mapName = self.tree.GetPyData(self.tree.layer_selected)[0]['maplayer'].name
        
        if self.dialogs['attributes'] is None:
            self.dialogs['attributes'] = \
                dbm_dialogs.DisplayAttributesDialog(parent=self.MapWindow,
                                                    map=mapName,
                                                    query=((east, north), qdist),
                                                    pos=posWindow,
                                                    action="update")
        else:
            # selection changed?
            if not self.dialogs['attributes'].mapDBInfo or \
                    self.dialogs['attributes'].mapDBInfo.map != mapName:
                self.dialogs['attributes'].UpdateDialog(map=mapName, query=((east, north), qdist))
            else:
                self.dialogs['attributes'].UpdateDialog(query=((east, north), qdist))
                
        cats = self.dialogs['attributes'].GetCats()
        
        try:
            qlayer = self.Map.GetListOfLayers(l_name=globalvar.QUERYLAYER)[0]
        except IndexError:
            qlayer = None
        
        if self.dialogs['attributes'].mapDBInfo and cats:
            # highlight feature & re-draw map
            if qlayer:
                qlayer.SetCmd(self.AddTmpVectorMapLayer(mapName, cats,
                                                        useId=False,
                                                        addLayer=False))
            else:
                qlayer = self.AddTmpVectorMapLayer(mapName, cats, useId=False)
            
            # set opacity based on queried layer
            opacity = self.tree.GetPyData(self.tree.layer_selected)[0]['maplayer'].GetOpacity(float=True)
            qlayer.SetOpacity(opacity)
            
            self.MapWindow.UpdateMap(render=False, renderVector=False)
            if not self.dialogs['attributes'].IsShown():
                self.dialogs['attributes'].Show()
        else:
            if qlayer:
                self.Map.DeleteLayer(qlayer)
                self.MapWindow.UpdateMap(render=False, renderVector=False)
            if self.dialogs['attributes'].IsShown():
                self.dialogs['attributes'].Hide()
        
    def OnQuery(self, event):
        """!Query tools menu"""
        if self.toolbars['map']:
            self.toolbars['map'].OnTool(event)
            action = self.toolbars['map'].GetAction()
        
        point = wx.GetMousePosition()
        toolsmenu = wx.Menu()
        # Add items to the menu
        display = wx.MenuItem(parentMenu=toolsmenu, id=wx.ID_ANY,
                              text=_("Query raster/vector map(s) (display mode)"),
                              kind=wx.ITEM_CHECK)
        toolsmenu.AppendItem(display)
        self.Bind(wx.EVT_MENU, self.OnQueryDisplay, display)
        numLayers = 0
        for layer in self.tree.GetSelections():
            type = self.tree.GetPyData(layer)[0]['maplayer'].GetType()
            if type in ('raster', 'rgb', 'his',
                        'vector', 'thememap', 'themechart'):
                numLayers += 1
        if numLayers < 1:
            display.Enable(False)
        
        if action == "displayAttrb":
            display.Check(True)
        
        modify = wx.MenuItem(parentMenu=toolsmenu, id=wx.ID_ANY,
                             text=_("Query vector map (edit mode)"),
                             kind=wx.ITEM_CHECK)
        toolsmenu.AppendItem(modify)
        self.Bind(wx.EVT_MENU, self.OnQueryModify, modify)
        digitToolbar = self.toolbars['vdigit']
        if self.tree.layer_selected:
            layer_selected = self.tree.GetPyData(self.tree.layer_selected)[0]['maplayer']
            if layer_selected.GetType() != 'vector' or \
                    (digitToolbar and \
                         digitToolbar.GetLayer() == layer_selected):
                modify.Enable(False)
            else:
                if action == "modifyAttrb":
                    modify.Check(True)
        
        self.PopupMenu(toolsmenu)
        toolsmenu.Destroy()

    def AddTmpVectorMapLayer(self, name, cats, useId=False, addLayer=True):
        """
        Add temporal vector map layer to map composition

        @param name name of map layer
        @param useId use feature id instead of category 
        """
        # color settings from ATM
        color = UserSettings.Get(group='atm', key='highlight', subkey='color')
        colorStr = str(color[0]) + ":" + \
            str(color[1]) + ":" + \
            str(color[2])

        # icon used in vector display and its size
        icon = ''
        size = 0
        vparam = self.tree.GetPyData(self.tree.layer_selected)[0]['cmd']
        for p in vparam:
            if '=' in p:
                parg,pval = p.split('=')
                if parg == 'icon': icon = pval
                elif parg == 'size': size = int(pval)

        pattern = ["d.vect",
                   "map=%s" % name,
                   "color=%s" % colorStr,
                   "fcolor=%s" % colorStr,
                   "width=%d"  % UserSettings.Get(group='atm', key='highlight', subkey='width')]
        if icon != '':
            pattern.append('icon=%s' % icon)
        if size > 0:
            pattern.append('size=%i' % size)
        
        if useId:
            cmd = pattern
            cmd.append('-i')
            cmd.append('cats=%s' % str(cats))
        else:
            cmd = []
            for layer in cats.keys():
                cmd.append(copy.copy(pattern))
                lcats = cats[layer]
                cmd[-1].append("layer=%d" % layer)
                cmd[-1].append("cats=%s" % utils.ListOfCatsToRange(lcats))
        
        #     if self.icon:
        #         cmd.append("icon=%s" % (self.icon))
        #     if self.pointsize:
        #         cmd.append("size=%s" % (self.pointsize))

        if addLayer:
            if useId:
                return self.Map.AddLayer(type='vector', name=globalvar.QUERYLAYER, command=cmd,
                                         l_active=True, l_hidden=True, l_opacity=1.0)
            else:
                return self.Map.AddLayer(type='command', name=globalvar.QUERYLAYER, command=cmd,
                                         l_active=True, l_hidden=True, l_opacity=1.0)
        else:
            return cmd

    def OnAnalyze(self, event):
        """
        Analysis tools menu
        """
        point = wx.GetMousePosition()
        toolsmenu = wx.Menu()
        # Add items to the menu
        measure = wx.MenuItem(toolsmenu, wx.ID_ANY, Icons["measure"].GetLabel())
        measure.SetBitmap(Icons["measure"].GetBitmap(self.iconsize))
        toolsmenu.AppendItem(measure)
        self.Bind(wx.EVT_MENU, self.OnMeasure, measure)

        profile = wx.MenuItem(toolsmenu, wx.ID_ANY, Icons["profile"].GetLabel())
        profile.SetBitmap(Icons["profile"].GetBitmap(self.iconsize))
        toolsmenu.AppendItem(profile)
        self.Bind(wx.EVT_MENU, self.Profile, profile)

        histogram = wx.MenuItem(toolsmenu, wx.ID_ANY, Icons["histogram"].GetLabel())
        histogram.SetBitmap(Icons["histogram"].GetBitmap(self.iconsize))
        toolsmenu.AppendItem(histogram)
        self.Bind(wx.EVT_MENU, self.Histogram, histogram)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(toolsmenu)
        toolsmenu.Destroy()

    def OnMeasure(self, event):
        """
        Init measurement routine that calculates
        map distance along transect drawn on
        map display
        """

        self.totaldist = 0.0 # total measured distance

        # switch GIS Manager to output console to show measure results
        self._layerManager.notebook.SetSelection(1)

        # change mouse to draw line for measurement
        self.MapWindow.mouse['use'] = "measure"
        self.MapWindow.mouse['box'] = "line"
        self.MapWindow.zoomtype = 0
        self.MapWindow.pen     = wx.Pen(colour='red', width=2, style=wx.SHORT_DASH)
        self.MapWindow.polypen = wx.Pen(colour='green', width=2, style=wx.SHORT_DASH)

        # change the cursor
        self.MapWindow.SetCursor(self.cursors["pencil"])

        # initiating output
        style = self._layerManager.goutput.cmd_output.StyleWarning
        self._layerManager.goutput.WriteLog(_('Click and drag with left mouse button '
                                              'to measure.%s'
                                              'Double click with left button to clear.') % \
                                                (os.linesep), style)
        if self.Map.projinfo['proj'] != 'xy':
            units = self.Map.projinfo['units']
            style = self._layerManager.goutput.cmd_output.StyleCommand
            self._layerManager.goutput.WriteLog(_('Measuring distance') + ' ('
                                                + units + '):',
                                                style)
        else:
            self._layerManager.goutput.WriteLog(_('Measuring distance:'),
                                                style)

    def MeasureDist(self, beginpt, endpt):
        """!Calculate map distance from screen distance
        and print to output window
        """
        if self._layerManager.notebook.GetSelection() != 1:
            self._layerManager.notebook.SetSelection(1)

        dist, (north, east) = self.MapWindow.Distance(beginpt, endpt)

        dist = round(dist, 3)
        d, dunits = self.FormatDist(dist)

        self.totaldist += dist
        td, tdunits = self.FormatDist(self.totaldist)

        strdist = str(d)
        strtotdist = str(td)

        if self.Map.projinfo['proj'] == 'xy' or 'degree' not in self.Map.projinfo['unit']:
            angle = int(math.degrees(math.atan2(north,east)) + 0.5)
            angle = 180 - angle
            if angle < 0:
                angle = 360+angle

            mstring = 'segment = %s %s\ttotal distance = %s %s\tbearing = %d deg' \
                % (strdist,dunits,strtotdist,tdunits,angle)
        else:
            mstring = 'segment = %s %s\ttotal distance = %s %s' \
                % (strdist,dunits,strtotdist,tdunits)

        self._layerManager.goutput.WriteLog(mstring)

        return dist

    def Profile(self, event):
        """
        Init profile canvas and tools
        """
        raster = []
        if self.tree.layer_selected and \
                self.tree.GetPyData(self.tree.layer_selected)[0]['type'] == 'raster':
            raster.append(self.tree.GetPyData(self.tree.layer_selected)[0]['maplayer'].name)

        self.profile = profile.ProfileFrame(self,
                                            id=wx.ID_ANY, pos=wx.DefaultPosition, size=(700,300),
                                            style=wx.DEFAULT_FRAME_STYLE, rasterList=raster)
        self.profile.Show()
        # Open raster select dialog to make sure that a raster (and the desired raster)
        # is selected to be profiled
        self.profile.OnSelectRaster(None)

    def FormatDist(self, dist):
        """!Format length numbers and units in a nice way,
        as a function of length. From code by Hamish Bowman
        Grass Development Team 2006"""

        mapunits = self.Map.projinfo['units']
        if mapunits == 'metres': mapunits = 'meters'
        outunits = mapunits
        dist = float(dist)
        divisor = 1.0

        # figure out which units to use
        if mapunits == 'meters':
            if dist > 2500.0:
                outunits = 'km'
                divisor = 1000.0
            else: outunits = 'm'
        elif mapunits == 'feet':
            # nano-bug: we match any "feet", but US Survey feet is really
            #  5279.9894 per statute mile, or 10.6' per 1000 miles. As >1000
            #  miles the tick markers are rounded to the nearest 10th of a
            #  mile (528'), the difference in foot flavours is ignored.
            if dist > 5280.0:
                outunits = 'miles'
                divisor = 5280.0
            else:
                outunits = 'ft'
        elif 'degree' in mapunits:
            if dist < 1:
                outunits = 'min'
                divisor = (1/60.0)
            else:
                outunits = 'deg'

        # format numbers in a nice way
        if (dist/divisor) >= 2500.0:
            outdist = round(dist/divisor)
        elif (dist/divisor) >= 1000.0:
            outdist = round(dist/divisor,1)
        elif (dist/divisor) > 0.0:
            outdist = round(dist/divisor,int(math.ceil(3-math.log10(dist/divisor))))
        else:
            outdist = float(dist/divisor)

        return (outdist, outunits)


    def Histogram(self, event):
        """
        Init histogram display canvas and tools
        """
        self.histogram = histogram.HistFrame(self,
                                             id=wx.ID_ANY, size=globalvar.HIST_WINDOW_SIZE,
                                             style=wx.DEFAULT_FRAME_STYLE)

        #show new display
        self.histogram.Show()
        self.histogram.Refresh()
        self.histogram.Update()


    def OnDecoration(self, event):
        """
        Decorations overlay menu
        """
        point = wx.GetMousePosition()
        decmenu = wx.Menu()
        # Add items to the menu
        AddScale = wx.MenuItem(decmenu, wx.ID_ANY, Icons["addbarscale"].GetLabel())
        AddScale.SetBitmap(Icons["addbarscale"].GetBitmap(self.iconsize))
        decmenu.AppendItem(AddScale)
        self.Bind(wx.EVT_MENU, self.OnAddBarscale, AddScale)

        AddLegend = wx.MenuItem(decmenu, wx.ID_ANY, Icons["addlegend"].GetLabel())
        AddLegend.SetBitmap(Icons["addlegend"].GetBitmap(self.iconsize))
        decmenu.AppendItem(AddLegend)
        self.Bind(wx.EVT_MENU, self.OnAddLegend, AddLegend)

        AddText = wx.MenuItem(decmenu, wx.ID_ANY, Icons["addtext"].GetLabel())
        AddText.SetBitmap(Icons["addtext"].GetBitmap(self.iconsize))
        decmenu.AppendItem(AddText)
        self.Bind(wx.EVT_MENU, self.OnAddText, AddText)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(decmenu)
        decmenu.Destroy()

    def OnAddBarscale(self, event):
        """
        Handler for scale/arrow map decoration menu selection.
        """
        if self.dialogs['barscale']:
            return

        id = 0 # unique index for overlay layer

        # If location is latlon, only display north arrow (scale won't work)
        #        proj = self.Map.projinfo['proj']
        #        if proj == 'll':
        #            barcmd = 'd.barscale -n'
        #        else:
        #            barcmd = 'd.barscale'

        # decoration overlay control dialog
        self.dialogs['barscale'] = \
            gdialogs.DecorationDialog(parent=self, title=_('Scale and North arrow'),
                                      size=(350, 200),
                                      style=wx.DEFAULT_DIALOG_STYLE | wx.CENTRE,
                                      cmd=['d.barscale', 'at=0,5'],
                                      ovlId=id,
                                      name='barscale',
                                      checktxt = _("Show/hide scale and North arrow"),
                                      ctrltxt = _("scale object"))

        self.dialogs['barscale'].CentreOnParent()
        ### dialog cannot be show as modal - in the result d.barscale is not selectable
        ### self.dialogs['barscale'].ShowModal()
        self.dialogs['barscale'].Show()
        self.MapWindow.mouse['use'] = 'pointer'        

    def OnAddLegend(self, event):
        """
        Handler for legend map decoration menu selection.
        """
        if self.dialogs['legend']:
            return
        
        id = 1 # index for overlay layer in render

        cmd = ['d.legend', 'at=5,50,2,5']
        if self.tree.layer_selected and \
                self.tree.GetPyData(self.tree.layer_selected)[0]['type'] == 'raster':
            cmd.append('map=%s' % self.tree.GetPyData(self.tree.layer_selected)[0]['maplayer'].name)

        # Decoration overlay control dialog
        self.dialogs['legend'] = \
            gdialogs.DecorationDialog(parent=self, title=('Legend'),
                                      size=(350, 200),
                                      style=wx.DEFAULT_DIALOG_STYLE | wx.CENTRE,
                                      cmd=cmd,
                                      ovlId=id,
                                      name='legend',
                                      checktxt = _("Show/hide legend"),
                                      ctrltxt = _("legend object")) 

        self.dialogs['legend'].CentreOnParent() 
        ### dialog cannot be show as modal - in the result d.legend is not selectable
        ### self.dialogs['legend'].ShowModal()
        self.dialogs['legend'].Show()
        self.MapWindow.mouse['use'] = 'pointer'

    def OnAddText(self, event):
        """
        Handler for text decoration menu selection.
        """
        if self.MapWindow.dragid > -1:
            id = self.MapWindow.dragid
        else:
            # index for overlay layer in render
            if len(self.MapWindow.textdict.keys()) > 0:
                id = self.MapWindow.textdict.keys()[-1] + 1
            else:
                id = 101

        self.dialogs['text'] = gdialogs.TextLayerDialog(parent=self, ovlId=id, 
                                                        title=_('Add text layer'),
                                                        size=(400, 200))
        self.dialogs['text'].CenterOnParent()

        # If OK button pressed in decoration control dialog
        if self.dialogs['text'].ShowModal() == wx.ID_OK:
            text = self.dialogs['text'].GetValues()['text']
            active = self.dialogs['text'].GetValues()['active']
            coords, w, h = self.MapWindow.TextBounds(self.dialogs['text'].GetValues())
        
            # delete object if it has no text or is not active
            if text == '' or active == False:
                try:
                    self.MapWindow.pdc.ClearId(id)
                    self.MapWindow.pdc.RemoveId(id)
                    del self.MapWindow.textdict[id]
                except:
                    pass
                return

            self.MapWindow.pdc.ClearId(id)
            self.MapWindow.pdc.SetId(id)
            self.MapWindow.textdict[id] = self.dialogs['text'].GetValues()
            
            self.MapWindow.Draw(self.MapWindow.pdcDec, img=self.MapWindow.textdict[id],
                                drawid=id, pdctype='text', coords=coords)
            
            self.MapWindow.UpdateMap(render=False, renderVector=False)
            
        self.MapWindow.mouse['use'] = 'pointer'

    def GetOptData(self, dcmd, type, params, propwin):
        """
        Callback method for decoration overlay command generated by
        dialog created in menuform.py
        """
        # Reset comand and rendering options in render.Map. Always render decoration.
        # Showing/hiding handled by PseudoDC
        self.Map.ChangeOverlay(ovltype=type, type='overlay', name='', command=dcmd,
                               l_active=True, l_render=False)
        self.params[type] = params
        self.propwin[type] = propwin

    def OnZoomToMap(self, event):
        """!
        Set display extents to match selected raster (including NULLs)
        or vector map.
        """
        if self.toolbars['gcpdisp']:
            self.MapWindow.ZoomToMap(layer = self.Map.GetListOfLayers())
        else:
            self.MapWindow.ZoomToMap()

    def OnZoomToRaster(self, event):
        """!
        Set display extents to match selected raster map (ignore NULLs)
        """
        self.MapWindow.ZoomToMap(ignoreNulls = True)

    def OnZoomToWind(self, event):
        """!Set display geometry to match computational region
        settings (set with g.region)
        """
        self.MapWindow.ZoomToWind()
        
    def OnZoomToDefault(self, event):
        """!Set display geometry to match default region settings
        """
        self.MapWindow.ZoomToDefault()
        
    def OnZoomToSaved(self, event):
        """!Set display geometry to match extents in
        saved region file
        """
        self.MapWindow.ZoomToSaved()
        
    def OnDisplayToWind(self, event):
        """!Set computational region (WIND file) to match display
        extents
        """
        self.MapWindow.DisplayToWind()
 
    def SaveDisplayRegion(self, event):
        """!Save display extents to named region file.
        """
        self.MapWindow.SaveDisplayRegion()
        
    def OnZoomMenu(self, event):
        """!Popup Zoom menu
        """
        point = wx.GetMousePosition()
        zoommenu = wx.Menu()
        # Add items to the menu

        zoomwind = wx.MenuItem(zoommenu, wx.ID_ANY, _('Zoom to computational region (set with g.region)'))
        zoommenu.AppendItem(zoomwind)
        self.Bind(wx.EVT_MENU, self.OnZoomToWind, zoomwind)

        zoomdefault = wx.MenuItem(zoommenu, wx.ID_ANY, _('Zoom to default region'))
        zoommenu.AppendItem(zoomdefault)
        self.Bind(wx.EVT_MENU, self.OnZoomToDefault, zoomdefault)

        zoomsaved = wx.MenuItem(zoommenu, wx.ID_ANY, _('Zoom to saved region'))
        zoommenu.AppendItem(zoomsaved)
        self.Bind(wx.EVT_MENU, self.OnZoomToSaved, zoomsaved)

        savewind = wx.MenuItem(zoommenu, wx.ID_ANY, _('Set computational region from display'))
        zoommenu.AppendItem(savewind)
        self.Bind(wx.EVT_MENU, self.OnDisplayToWind, savewind)

        savezoom = wx.MenuItem(zoommenu, wx.ID_ANY, _('Save display geometry to named region'))
        zoommenu.AppendItem(savezoom)
        self.Bind(wx.EVT_MENU, self.SaveDisplayRegion, savezoom)

        # Popup the menu. If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(zoommenu)
        zoommenu.Destroy()
        
    def SetProperties(self, render=False, mode=0, showCompExtent=False,
                      constrainRes=False, projection=False):
        """!Set properies of map display window"""
        self.autoRender.SetValue(render)
        self.toggleStatus.SetSelection(mode)
        self.StatusbarUpdate()
        self.showRegion.SetValue(showCompExtent)
        self.compResolution.SetValue(constrainRes)
        self.projInfo.SetValue(projection)
        if showCompExtent:
            self.MapWindow.regionCoords = []
        
    def IsStandalone(self):
        """!Check if Map display is standalone"""
        if self._layerManager:
            return False
        
        return True
    
    def GetLayerManager(self):
        """!Get reference to Layer Manager

        @return window reference
        @return None (if standalone)
        """
        return self._layerManager
    
# end of class MapFrame
