"""
MODULE:    mapdisp

PURPOSE:   To be used either from GIS Manager or as p.mon backend

           Usage:
            python mapdisp.py monitor-identifier /path/to/command/file

AUTHORS:   The GRASS Development Team
           Michael Barton
           Jachym Cepicky
           Martin Landa

COPYRIGHT: (C) 2006-2007 by the GRASS Development Team
           This program is free software under the GNU General Public
           License (>=v2). Read the file COPYING that comes with GRASS
           for details.
"""

import wx
import wx.aui
import os, sys, time, glob, math
from threading import Thread

try:
    import subprocess
except:
    CompatPath = os.path.join( os.getenv("GISBASE"),"etc","wx")
    sys.path.append(CompatPath)
    from compat import subprocess

gmpath = os.path.join( os.getenv("GISBASE"),"etc","wx","gui_modules" )
sys.path.append(gmpath)
gmpath = os.path.join( os.getenv("GISBASE"),"etc","wx","icons" )
sys.path.append(gmpath)

import render
import toolbars
import grassenv
import track
import menuform
import select
import disp_print
import cmd
import defaultfont as defaultfont
import histogram as histogram
import profile as profile
from digit import Digit as Digit
from debug import Debug as Debug
from icon import Icons as Icons

import images
imagepath = images.__path__[0]
sys.path.append(imagepath)

# for cmdlinef
cmdfilename = None

class Command(Thread):
    """
    Creates  thread, which will observe the command file and see, if there
    is new command to be executed
    """
    def __init__ (self,parent,Map):
        Thread.__init__(self)

        global cmdfilename

        self.parent = parent
        self.map = Map
        self.cmdfile = open(cmdfilename,"r")

    def run(self):
        """
        Run this in thread
        """
        dispcmd = []
        while 1:
            self.parent.redraw = False
            line = self.cmdfile.readline().strip()
            if line == "quit":
                break

            if line:
                try:
                    Debug.msg (3, "Command.run(): cmd=%s" % (line))

                    self.map.AddLayer(item=None, type="raster",
                                      name='',
                                      command=line,
                                      l_opacity=1)

                    self.parent.redraw =True

                except Exception, e:
                    print "Command Thread: ",e
                    pass

            time.sleep(0.1)

        sys.exit()

class BufferedWindow(wx.Window):
    """
    A Buffered window class.

    When the drawing needs to change, you app needs to call the
    UpdateMap() method. Since the drawing is stored in a bitmap, you
    can also save the drawing to file by calling the
    SaveToFile(self,file_name,file_type) method.
    """

    def __init__(self, parent, id,
                 pos = wx.DefaultPosition,
                 size = wx.DefaultSize,
                 style=wx.NO_FULL_REPAINT_ON_RESIZE,
                 Map=None, tree=None):

        wx.Window.__init__(self, parent, id, pos, size, style)
        self.parent = parent
        self.Map = Map
        self.tree = tree

        #
        # Flags
        #
        self.render = True  # re-render the map from GRASS or just redraw image
        self.resize = False # indicates whether or not a resize event has taken place
        self.dragimg = None # initialize variable for map panning

        #
        # Variable for drawing on DC
        #
        self.pen = None     # pen for drawing zoom boxes, etc.
        self.polypen = None # pen for drawing polylines (measurements, profiles, etc)
        self.polycoords = []  # List of wx.Point tuples defining a polyline
        self.lineid = None #ID of rubber band line
        self.plineid = None #ID of poly line resulting from cumulative rubber band lines (e.g. measurement)

        #
        # Event bindings
        #
        self.Bind(wx.EVT_PAINT,        self.OnPaint)
        self.Bind(wx.EVT_SIZE,         self.OnSize)
        self.Bind(wx.EVT_IDLE,         self.OnIdle)
        self.Bind(wx.EVT_MOTION,       self.MouseActions)
        self.Bind(wx.EVT_MOUSE_EVENTS, self.MouseActions)

        #
        # Render output objects
        #
        self.mapfile = None   # image file to be rendered
        self.img = ""         # wx.Image object (self.mapfile)
        self.ovldict = {}     # list of images for overlays
        self.ovlcoords = {}   # positioning coordinates for decoration overlay
        self.ovlchk = {}      # showing/hiding decorations
        self.imagedict = {}   # images and their PseudoDC ID's for painting and dragging
        self.crop = {}        # coordinates to crop overlays to their data, indexed by image ID
        self.select = {}      # selecting/unselecting decorations for dragging
        self.textdict = {}    # text, font, and color indexed by id
        self.currtxtid = None # PseudoDC id for currently selected text

        #
        # Zoom objects
        #
        self.zoomhistory = [] # list of past zoom extents
        self.currzoom = 0 # current set of extents in zoom history being used

        #
        # mouse attributes like currently pressed buttons, position on
        # the screen, begin and end of dragging, and type of drawing
        #
        self.mouse = {
            'l': False,
            'r': False,
            'm': False,
            'pos': [None, None],
            'begin': [0, 0],
            'end': [0, 0],
            'use': "pointer",
            'box': "point"
            }

        self.zoomtype = 1   # 1 zoom in, 0 no zoom, -1 zoom out
        self.hitradius = 10 # distance for selecting map decorations

        # OnSize called to make sure the buffer is initialized.
        # This might result in OnSize getting called twice on some
        # platforms at initialization, but little harm done.
        #!!! self.OnSize(None)

        # create a PseudoDC for map decorations like scales and legends
        self.pdc = wx.PseudoDC()
        self._Buffer = '' # will store an off screen empty bitmap for saving to file
        self.Map.SetRegion() # make sure that extents are updated at init

        self.Bind(wx.EVT_ERASE_BACKGROUND, lambda x:None)

        # vars for handling mouse clicks
        self.dragid   = -1
        self.lastpos  = (0, 0)
        self.savedpos = []

    def Draw(self, pdc, img=None, drawid=None, pdctype='image', coords=[0,0,0,0]):
        """
        Draws map and overlay decorations
        """

        if drawid == None:
            if pdctype == 'image' :
                drawid = imagedict[img]
            elif pdctype == 'clear':
                drawid == None
            else:
                drawid = wx.NewId()
        else:
            self.ovlcoords[drawid] = coords
            self.ovlchk[drawid] = True
            pdc.SetId(drawid)
            self.select[drawid] = False

        pdc.BeginDrawing()
        if drawid != 99:
            bg = wx.TRANSPARENT_BRUSH
        else:
            bg = wx.Brush(self.GetBackgroundColour())
        pdc.SetBackground(bg)
        #pdc.Clear() #FIXME (to avoid black background)
        self.Refresh()

        Debug.msg (5, "BufferedWindow.Draw(): id=%s, pdctype=%s, coord=%s" % (drawid, pdctype, coords))

        if pdctype == 'clear': # erase the display
            bg = wx.WHITE_BRUSH
#            bg = wx.Brush(self.GetBackgroundColour())
            pdc.SetBackground(bg)
            pdc.Clear()
            self.Refresh()
            pdc.EndDrawing()
            return

        if pdctype == 'image':
            bitmap = wx.BitmapFromImage(img)
            w,h = bitmap.GetSize()
            pdc.DrawBitmap(bitmap, coords[0], coords[1], True) # draw the composite map
            pdc.SetIdBounds(drawid, (coords[0],coords[1],w,h))

        elif pdctype == 'box': # draw a box on top of the map
            pdc.SetBrush(wx.Brush(wx.CYAN, wx.TRANSPARENT))
            pdc.SetPen(self.pen)
            x2 = max(coords[0],coords[2])
            x1 = min(coords[0],coords[2])
            y2 = max(coords[1],coords[3])
            y1 = min(coords[1],coords[3])
            rwidth = x2-x1
            rheight = y2-y1
            rect = wx.Rect(x1,y1,rwidth,rheight)
            pdc.DrawRectangleRect(rect)
            pdc.SetIdBounds(drawid,rect)
            self.ovlcoords[drawid] = coords

        elif pdctype == 'line': # draw a line on top of the map
            pdc.SetBrush(wx.Brush(wx.CYAN, wx.TRANSPARENT))
            pdc.SetPen(self.pen)
            pdc.DrawLine(coords[0], coords[1], coords[2], coords[3])
            pdc.SetIdBounds(drawid,(coords[0], coords[1], coords[2], coords[3]))
            self.ovlcoords[drawid] = coords

        elif pdctype == 'polyline': # draw a polyline on top of the map
            pdc.SetBrush(wx.Brush(wx.CYAN, wx.TRANSPARENT))
            pdc.SetPen(self.polypen)
            pdc.DrawLines(self.polycoords)

            # get bounding rectangle for polyline
            xlist = []
            ylist = []
            if len(self.polycoords) > 0:
                for point in self.polycoords:
                    x,y = point
                    xlist.append(x)
                    ylist.append(y)
                x1=min(xlist)
                x2=max(xlist)
                y1=min(ylist)
                y2=max(ylist)
                pdc.SetIdBounds(drawid,(x1,y1,x2,y2))
                self.ovlcoords[drawid] = [x1,y1,x2,y2]

        elif pdctype == 'point': #draw point
            pen = self.RandomPen()
            pdc.SetPen(pen)
            pdc.DrawPoint(coords[0], coords[1])
            coords[0] = coords[0] - 5
            coords[1] = coords[1] - 5
            coords[2] = coords[0] + 5
            coords[3] = coords[1] + 5
            pdc.SetIdBounds(drawid,(coords[0], coords[1], coords[2], coords[3]))
            self.ovlcoords[drawid] = coords

        elif pdctype == 'text': # draw text on top of map
            text = img[0]
            rotation = float(img[3])
            w,h = self.GetFullTextExtent(img[0])[0:2]
            pdc.SetFont(img[1])
            pdc.SetTextForeground(img[2])
            coords,w,h = self.TextBounds(img,coords)
            if rotation == 0:
                pdc.DrawText(img[0], coords[0], coords[1])
            else:
                pdc.DrawRotatedText(img[0], coords[0], coords[1], rotation)
            pdc.SetIdBounds(drawid, (coords[0], coords[1], w, h))
            self.ovlcoords[drawid] = coords

        pdc.EndDrawing()
        self.Refresh()

    def TextBounds(self, textinfo, coords):
        """
        Return text boundary data
        """
        rotation = float(textinfo[3])

        Debug.msg (4, "BufferedWindow.TextBounds(): text=%s, rotation=%f" % \
                   (textinfo[0], rotation))

        self.Update()
        self.Refresh()
        self.SetFont(textinfo[1])
        w,h = self.GetTextExtent(textinfo[0])
        if rotation == 0:
            coords[2], coords[3] = coords[0] + w, coords[1] + h
            return coords, w, h
        else:
            boxh = math.fabs(math.sin(math.radians(rotation)) * w) + h
            boxw = math.fabs(math.cos(math.radians(rotation)) * w) + h
            coords[2] = coords[0] + boxw
            coords[3] = coords[1] + boxh
            return coords, boxw, boxh

    def OnPaint(self, event):
        """
        Draw psuedo DC to buffered paint DC
        """

        dc = wx.BufferedPaintDC(self, self._Buffer)

        # use PrepareDC to set position correctly
        self.PrepareDC(dc)
        # we need to clear the dc BEFORE calling PrepareDC
        bg = wx.Brush(self.GetBackgroundColour())
        dc.SetBackground(bg)
        dc.Clear()
        # create a clipping rect from our position and size
        # and the Update Region
        rgn = self.GetUpdateRegion()
        r = rgn.GetBox()
        # draw to the dc using the calculated clipping rect
        self.pdc.DrawToDCClipped(dc,r)


    def OnSize(self, event):
        """
        Scale map image so that it is
        the same size as the Window
        """

            # set size of the input image
        self.Map.width, self.Map.height = self.GetClientSize()

        # Make new off screen bitmap: this bitmap will always have the
        # current drawing in it, so it can be used to save the image to
        # a file, or whatever.
        self._Buffer = wx.EmptyBitmap(self.Map.width, self.Map.height)

        # get the image to be rendered
        self.img = self.GetImage()

        # update map display
        if self.img and self.Map.width + self.Map.height > 0: # scale image during resize
            self.img = self.img.Scale(self.Map.width, self.Map.height)
            self.render = False
            self.UpdateMap()

        # re-render image on idle
        self.resize = True

    def OnIdle(self, event):
        """
        Only re-render a compsite map image from GRASS during
        idle time instead of multiple times during resizing.
        """

        if self.resize:
            self.render = True
            self.UpdateMap()
        event.Skip()

    def SaveToFile(self, FileName, FileType):
        """
        This draws the psuedo DC to a buffer that
        can be saved to a file.
        """
        dc = wx.BufferedPaintDC(self, self._Buffer)
        self.pdc.DrawToDC(dc)
        self._Buffer.SaveFile(FileName, FileType)

    def GetOverlay(self):
        """
        Converts rendered overlay files to wx.Image
        """
        self.ovldict = {}
        for overlay in self.Map.GetListOfLayers(l_type="overlay", l_active=True):
            if os.path.isfile(overlay.mapfile) and os.path.getsize(overlay.mapfile):
                img = wx.Image(overlay.mapfile, wx.BITMAP_TYPE_ANY)
                pdc_id = self.Map.overlays.index(overlay)
                self.ovldict[pdc_id] = img  # image information for each overlay image
                self.imagedict[img] = pdc_id # set image PeudoDC ID

        Debug.msg (3, "BufferedWindow.GetOverlay(): numberof=%d" % len(self.ovldict))

        return self.ovldict


    def GetImage(self):
        """
        Converts redered map files to wx.Image
        """
        if self.Map.mapfile and os.path.isfile(self.Map.mapfile) and \
                os.path.getsize(self.Map.mapfile):
            img = wx.Image(self.Map.mapfile, wx.BITMAP_TYPE_ANY)
        else:
            img = None

        self.imagedict[img] = 99 # set image PeudoDC ID
        return img


    def UpdateMap(self, img=None):
        """
        Updates the canvas anytime there is a change to the underlying images
        or to the geometry of the canvas.
        """

        Debug.msg (2, "BufferedWindow.UpdateMap(%s): render=%s" % (img, self.render))
        if self.render:
            # render new map images
            self.Map.width, self.Map.height = self.GetClientSize()
            self.mapfile = self.Map.Render(force=self.render)
            self.img = self.GetImage()
            self.resize = False

        if not self.img: return
        try:
            id = self.imagedict[self.img]
        except:
            return

        # paint images to PseudoDC
        self.pdc.Clear()
        self.pdc.RemoveAll()
        self.Draw(self.pdc, self.img, drawid=id) # draw map image background
        self.ovldict = self.GetOverlay() # list of decoration overlay images
        if self.ovldict != {}: # draw scale and legend overlays
            for id in self.ovldict:
                img = self.ovldict[id]
                if id not in self.ovlcoords: self.ovlcoords[id] = wx.Rect(0,0,0,0)
                if id not in self.ovlchk: self.ovlchk[id] = False
                if self.ovlchk[id] == True: # draw any active and defined overlays
                    self.Draw(self.pdc, img=img, drawid=id,
                             pdctype='image', coords=self.ovlcoords[id])

        if self.textdict != None: # draw text overlays
            for id in self.textdict:
                self.Draw(self.pdc, img=self.textdict[id], drawid=id,
                          pdctype='text', coords=self.ovlcoords[id])

        self.resize = False

        # update statusbar
        #Debug.msg (3, "BufferedWindow.UpdateMap(%s): region=%s" % self.Map.region)
        self.Map.SetRegion()
        self.parent.statusbar.SetStatusText("Ext: %.2f(W)-%.2f(E), %.2f(N)-%.2f(S)" %
                                            (self.Map.region["w"], self.Map.region["e"],
                                             self.Map.region["n"], self.Map.region["s"]), 0)

    def EraseMap(self):
        """
        Erase the map display
        """
        self.Draw(self.pdc, pdctype='clear')

    def DragMap(self, moveto):
        """
        Drag the entire map image for panning.
        """

        dc = wx.BufferedDC(wx.ClientDC(self))
        dc.SetBackground(wx.Brush("White"))
        bitmap = wx.BitmapFromImage(self.img)
        self.dragimg = wx.DragImage(bitmap)
        self.dragimg.BeginDrag((0, 0), self)
        self.dragimg.GetImageRect(moveto)
        self.dragimg.Move(moveto)
        dc.Clear()
        self.dragimg.DoDrawImage(dc, moveto)
        self.dragimg.EndDrag()

    def DragItem(self, id, event):
        """
        Drag an overlay decoration item
        """
        x,y = self.lastpos
        dx = event.GetX() - x
        dy = event.GetY() - y
        self.pdc.SetBackground(wx.Brush(self.GetBackgroundColour()))
        r = self.pdc.GetIdBounds(id)
        if self.dragid > 100: # text dragging
            rtop = (r[0],r[1]-r[3],r[2],r[3])
            r = r.Union(rtop)
            rleft = (r[0]-r[2],r[1],r[2],r[3])
            r = r.Union(rleft)
        self.pdc.TranslateId(id, dx, dy)
        r2 = self.pdc.GetIdBounds(id)
        r = r.Union(r2)
        r.Inflate(4,4)
        self.Update()
        self.RefreshRect(r, False)
        self.lastpos = (event.GetX(),event.GetY())

    def MouseDraw(self):
        """
        Mouse zoom rectangles and lines
        """
        if self.mouse['box'] == "box":
            boxid = wx.ID_NEW
            mousecoords = [self.mouse['begin'][0], self.mouse['begin'][1], \
                           self.mouse['end'][0], self.mouse['end'][1]]
            r = self.pdc.GetIdBounds(boxid)
            r.Inflate(4,4)
            self.pdc.ClearId(boxid)
            self.RefreshRect(r, False)
            self.pdc.SetId(boxid)
            self.Draw(self.pdc, drawid=boxid, pdctype='box', coords=mousecoords)
        elif self.mouse['box'] == "line":
            self.lineid = wx.ID_NEW
            mousecoords = [self.mouse['begin'][0], self.mouse['begin'][1], \
                           self.mouse['end'][0], self.mouse['end'][1]]
            x1=min(self.mouse['begin'][0],self.mouse['end'][0])
            x2=max(self.mouse['begin'][0],self.mouse['end'][0])
            y1=min(self.mouse['begin'][1],self.mouse['end'][1])
            y2=max(self.mouse['begin'][1],self.mouse['end'][1])
            r = wx.Rect(x1,y1,x2-x1,y2-y1)
            r.Inflate(4,4)
            try:
                self.pdc.ClearId(self.lineid)
            except:
                pass
            self.RefreshRect(r, False)
            self.pdc.SetId(self.lineid)

            self.Draw(self.pdc, drawid=self.lineid, pdctype='line', coords=mousecoords)

    def DrawLines(self):
        self.plineid = wx.ID_NEW+1
        if len(self.polycoords) > 0:
            self.Draw(self.pdc, drawid=self.plineid, pdctype='polyline', coords=self.polycoords)

    def MouseActions(self, event):
        """
        Mouse motion and button click notifier
        """
        wheel = event.GetWheelRotation() # +- int

        # zoom with mouse wheel
        if wheel != 0:
            Debug.msg (5, "BufferedWindow.MouseAction(): wheel=%d" % wheel)
            # zoom 1/2 of the screen
            begin = [self.Map.width/4, self.Map.height/4]
            end = [self.Map.width - self.Map.width/4,
                   self.Map.height - self.Map.height/4]
            if wheel > 0:
                zoomtype = 1
            else:
                zoomtype = -1
            # zoom
            self.Zoom(begin, end, zoomtype)
            # redraw map
            self.render=True
            self.UpdateMap()
            return

        # left mouse button pressed
        if event.LeftDown():
            self.OnLeftDown(event)

        # left mouse button released
        elif event.LeftUp():
            self.OnLeftUp(event)

        # dragging
        elif event.Dragging():
            Debug.msg (5, "BufferedWindow.MouseAction(): Dragging")
            currpos = event.GetPositionTuple()[:]
            end = (currpos[0]-self.mouse['begin'][0], \
                             currpos[1]-self.mouse['begin'][1])

            # dragging or drawing box with left button
            if self.mouse['use'] == 'pan':
                self.DragMap(end)

            # dragging decoration overlay item
            elif self.mouse['use'] == 'pointer' and self.dragid != None and self.dragid != 99:
                self.DragItem(self.dragid, event)

            # dragging anything else - rubber band box or line
            else:
                self.mouse['end'] = event.GetPositionTuple()[:]
                self.MouseDraw()


        # double click
        elif event.ButtonDClick():
            self.OnButtonDClick(event)

        # right mouse button pressed
        elif event.RightDown():
            self.OnRightDown(event)

        # right mouse button released
        elif event.RightUp():
            self.OnRightUp(event)

        # store current mouse position
        self.mouse['pos'] = event.GetPositionTuple()[:]

    def OnLeftDown(self, event):
        """
        Left mouse button pressed
        """
        Debug.msg (5, "BufferedWindow.OnLeftDown(): use=%s" % \
                   self.mouse["use"])

        if self.mouse["use"] in ["measure", "profile"] or \
               (self.mouse["use"] == "pointer" and self.parent.digittoolbar):
            # measure || profile || digit tool
            if len(self.polycoords) == 0:
                self.mouse['begin'] = event.GetPositionTuple()[:]
                self.mouse['end'] = self.mouse['begin']
                self.polycoords.append(self.mouse['begin'])
                self.ClearLines()
            else:
                self.mouse['begin'] = self.mouse['end']
        else: # get decoration id
            self.lastpos = self.mouse['begin'] = event.GetPositionTuple()[:]
            idlist = self.pdc.FindObjects(x=self.lastpos[0], y=self.lastpos[1],
                                          radius=self.hitradius)
            if idlist != []:
                self.dragid = idlist[0]

    def OnLeftUp(self, event):
        """
        Left mouse button released
        """
        Debug.msg (5, "BufferedWindow.OnLeftUp(): use=%s" % \
                   self.mouse["use"])

        if self.mouse['use'] in ["zoom", "pan"]:
            # end point of zoom box or drag
            self.mouse['end'] = event.GetPositionTuple()[:]

            # set region in zoom or pan
            self.Zoom(self.mouse['begin'], self.mouse['end'], self.zoomtype)

            # redraw map
            self.render=True
            self.UpdateMap()

        elif self.mouse["use"] == "query":
            # querying
            self.parent.QueryMap(self.mouse['begin'][0],self.mouse['begin'][1])

        elif self.mouse["use"] in ["measure", "profile"]:
            # measure or profile 
            self.mouse['end'] = event.GetPositionTuple()[:]
            if self.mouse["use"] == "measure":
                self.parent.MeasureDist(self.mouse['begin'], self.mouse['end'])
            try:
                self.polycoords.append(self.mouse['end'])
                self.pdc.ClearId(self.lineid)
                self.DrawLines()
            except:
                pass
        elif self.mouse["use"] == "pointer" and self.parent.digittoolbar:
            # digit tool
            digit = self.parent.digittoolbar
            if digit.action == "add":
                try:
                    map = digit.layers[digit.layerID].name
                except:
                    map = None
                    dlg = wx.MessageDialog(self, _("No vector map layer selected for editing"),
                                           _("Error"), wx.OK | wx.ICON_ERROR)
                    dlg.ShowModal()
                    dlg.Destroy()

                    if map:
                        east, north = self.Pixel2Cell(self.mouse['begin'][0],
                                                      self.mouse['begin'][1])
                        if digit.type in ["point", "centroid"]:
                         # add new point
                            Digit.AddPoint(map=map,
                                           type=digit.type,
                                           x=east, y=north)
                        elif digit.type in ["line", "boundary"]:
                            # add new point to the line
                            Debug.msg (3, "BufferedWindow.MouseAction(): new saved pos=%f,%f" % \
                                           (east, north))
                            self.savedpos.append ((east, north))

                    # redraw map
                    self.render=True
                    self.UpdateMap()

        elif self.dragid != None:
            # end drag of overlay decoration
            self.ovlcoords[self.dragid] = self.pdc.GetIdBounds(self.dragid)
            self.dragid = None
            self.currtxtid = None
            id = None
            self.Update()


    def OnButtonDClick(self, event):
        """
        Mouse button double click
        """
        Debug.msg (5, "BufferedWindow.OnButtonDClick(): use=%s" % \
                   self.mouse["use"])

        if self.mouse["use"] == "measure":
            # measure
            self.ClearLines()
            self.polycoords = []
            self.mouse['use'] = 'pointer'
            self.mouse['box'] = 'point'
            self.mouse['end'] = [0, 0]
            self.Refresh()
            self.SetCursor(self.parent.cursors["default"])
        elif self.mouse["use"] == "profile":
            # profile
            pass
        #                self.pdc.ClearId(self.lineid)
        #                self.pdc.ClearId(self.plineid)
        #                print 'coordinates: ',self.polycoords
        #                self.polycoords = []
        #                self.mouse['begin'] = self.mouse['end'] = [0, 0]
        #                self.Refresh()
        else:
            # select overlay decoration options dialog
            clickposition = event.GetPositionTuple()[:]
            idlist  = self.pdc.FindObjects(clickposition[0], clickposition[1], self.hitradius)
            if idlist == []:
                return
            self.dragid = idlist[0]

            self.ovlcoords[self.dragid] = self.pdc.GetIdBounds(self.dragid)
            if self.dragid > 100:
                self.currtxtid = self.dragid
                self.parent.AddText(None)
            elif self.dragid == 0:
                self.parent.AddBarscale(None)
            elif self.dragid == 1:
                self.parent.AddLegend(None)

    def OnRightDown(self, event):
        """
        Right mouse button pressed
        """
        Debug.msg (5, "BufferedWindow.OnRightDown(): use=%s" % \
                   self.mouse["use"])

        x,y = event.GetPositionTuple()[:]
        l = self.pdc.FindObjects(x=x, y=y, radius=self.hitradius)
        if not l:
            return

        id = l[0]

        if id != 99:
            if self.pdc.GetIdGreyedOut(id) == True:
                self.pdc.SetIdGreyedOut(id, False)
            else:
                self.pdc.SetIdGreyedOut(id, True)

                r = self.pdc.GetIdBounds(id)
                r.Inflate(4,4)
                self.RefreshRect(r, False)

    def OnRightUp(self, event):
        """
        Right mouse button released
        """
        Debug.msg (5, "BufferedWindow.OnRightUp(): use=%s" % \
                   self.mouse["use"])

        if self.parent.digittoolbar and self.parent.digittoolbar.action == "add":
            if self.parent.digittoolbar.type in ["line", "boundary"]:
                try:
                    map = self.parent.digittoolbar.layers[self.parent.digittoolbar.layerID].name
                except:
                    map = None
                    dlg = wx.MessageDialog(self, _("No vector map layer is selected"), _("Error"), wx.OK | wx.ICON_ERROR)
                    dlg.ShowModal()
                    dlg.Destroy()

                if map:
                    # add new line
                    Digit.AddLine(map=map,
                                  type=self.parent.digittoolbar.type,
                                  xy=self.savedpos)
                    # clean up saved positions
                    self.savedpos = []
                    # redraw map
                    self.render=True
                    self.UpdateMap()

    def ClearLines(self):
        """
        Clears lines drawn for measurement and profiling
        """
        try:
            self.pdc.ClearId(self.lineid)
            self.pdc.ClearId(self.plineid)
            self.Refresh()
            return True
        except:
            return False


    def Pixel2Cell(self, x, y):
        """
        Calculates real word coordinates to image coordinates

        Input : x, y
        Output: int x, int y
        """
        newx = self.Map.region['w'] + x * self.Map.region["ewres"]
        newy = self.Map.region['n'] - y * self.Map.region["nsres"]

        return newx, newy

    def Zoom(self, begin, end, zoomtype):
        """
        Calculates new region while (un)zoom/pan-ing
        """
        x1, y1, x2, y2 = begin[0], begin[1], end[0], end[1]
        newreg = {}

        # threshold - too small squares do not make sense
        # can only zoom to windows of > 10x10 screen pixels
        if x2 > 10 and y2 > 10 and zoomtype != 0:

            if x1 > x2:
                x1, x2 = x2, x1
            if y1 > y2:
                y1, y2 = y2, y1
            # zoom in
            if zoomtype > 0:
                newreg['w'], newreg['n'] = self.Pixel2Cell(x1, y1)
                newreg['e'], newreg['s'] = self.Pixel2Cell(x2, y2)

            # zoom out
            elif zoomtype < 0:
                newreg['w'], newreg['n'] = self.Pixel2Cell(-x1 * 2, -y1 * 2)
                newreg['e'], newreg['s'] = self.Pixel2Cell(self.Map.width  + 2 * (self.Map.width  - x2),
                                                           self.Map.height + 2 * (self.Map.height - y2))
        # pan
        elif zoomtype == 0:
            newreg['w'], newreg['n'] = self.Pixel2Cell(x1 - x2, y1 - y2)
            newreg['e'], newreg['s'] = self.Pixel2Cell(self.Map.width  + x1 - x2,
                                                       self.Map.height + y1 - y2)

        # if new region has been calculated, set the values
        if newreg :
            self.Map.region['n'] = newreg['n']
            self.Map.region['s'] = newreg['s']
            self.Map.region['e'] = newreg['e']
            self.Map.region['w'] = newreg['w']

            self.ZoomHistory(newreg['n'], newreg['s'], newreg['e'], newreg['w'])

    def ZoomBack(self):
        """
        Zoom to previous extents in zoomhistory list
        """

        zoom = []
        if len(self.zoomhistory) > 1:
            self.zoomhistory.pop()
            zoom = self.zoomhistory[len(self.zoomhistory)-1]

        if zoom:
            self.Map.region['n'] = zoom[0]
            self.Map.region['s'] = zoom[1]
            self.Map.region['e'] = zoom[2]
            self.Map.region['w'] = zoom[3]
            self.render=True
            self.UpdateMap()

    def ZoomHistory(self, n,s,e,w):
        """
        Manages a list of last 10 zoom extents
        """
        self.zoomhistory.append((n,s,e,w))
        if len(self.zoomhistory) > 10:
            self.zoomhistory.pop(0)

    def ZoomToMap(self, event):
        """
        Set display extents to match selected raster
        or vector map.
        """

        if not self.tree.GetSelection():
            return

        item  = self.tree.GetSelection()
        try:
            layer = self.tree.layers[item].maplayer
        except:
            return

        Debug.msg (3, "BufferedWindow.ZoomToMap(): layer=%s, type=%s" % \
                   (layer.name, layer.type))

        # selected layer must be a valid map
        if layer.type in ('raster', 'rgb', 'his'):
            cmdVec = ["r.info", "-g", "map=%s" % layer.name]
        elif layer.type in ('vector', 'thememap', 'themechart'):
            cmdVec = ["v.info", "-g", "map=%s" % layer.name]
        else:
            return

        p = cmd.Command(cmdVec)

        output = p.module_stdout.read().split('\n')
        for oline in output:
            extent = oline.split('=')
            if extent[0] == 'north':
                self.Map.region['n'] = float(extent[1])
            elif extent[0] == 'south':
                self.Map.region['s'] = float(extent[1])
            elif extent[0] == 'east':
                self.Map.region['e'] = float(extent[1])
            elif extent[0] == 'west':
                self.Map.region['w'] = float(extent[1])

        self.ZoomHistory(self.Map.region['n'],self.Map.region['s'],self.Map.region['e'],self.Map.region['w'])
        self.UpdateMap()

    def ZoomToWind(self, event):
        """
        Set display geometry to match computational
        region extents (set with g.region)
        """

        self.Map.region = self.Map.GetRegion()
        self.Map.SetRegion()
        self.ZoomHistory(self.Map.region['n'],self.Map.region['s'],self.Map.region['e'],self.Map.region['w'])
        self.UpdateMap()

    def DisplayToWind(self, event):
        """
        Set computational region (WIND file) to
        match display extents
        """
        tmpreg = os.getenv("GRASS_REGION")
        os.unsetenv("GRASS_REGION")

        # We ONLY want to set extents here. Don't mess with resolution. Leave that
        # for user to set explicitly with g.region
        new = self.Map.alignResolution()

        cmdRegion = ["g.region", "--o",
                     "n=%f"    % new['n'],
                     "s=%f"    % new['s'],
                     "e=%f"    % new['e'],
                     "w=%f"    % new['w'],
                     "rows=%f" % new['rows'],
                     "cols=%f" % new['cols']]

        p = cmd.Command(cmdRegion)

        if tmpreg:
            os.environ["GRASS_REGION"] = tmpreg

    def ZoomToSaved(self, event):
        """
        Set display geometry to match extents in
        saved region file
        """

        zoomreg = {}

        dlg = SavedRegion(self, wx.ID_ANY, "Zoom to saved region extents",
                          pos=wx.DefaultPosition, size=wx.DefaultSize,
                          style=wx.DEFAULT_DIALOG_STYLE,
                          loadsave='load')

        if dlg.ShowModal() == wx.ID_CANCEL:
            dlg.Destroy()
            return

        wind = dlg.wind

        p = cmd.Command (["g.region", "-ugp", "region=%s" % wind])

        if p.returncode == 0:
            output = p.module_stdout.read().split('\n')
            for line in output:
                line = line.strip()
                if '=' in line: key,val = line.split('=')
                zoomreg[key] = float(val)
            self.Map.region['n'] = zoomreg['n']
            self.Map.region['s'] = zoomreg['s']
            self.Map.region['e'] = zoomreg['e']
            self.Map.region['w'] = zoomreg['w']
            self.ZoomHistory(self.Map.region['n'],self.Map.region['s'],self.Map.region['e'],self.Map.region['w'])
            self.UpdateMap()

        dlg.Destroy()

    def SaveDisplayRegion(self, event):
        """
        Save display extents to named region file.
        """

        dlg = SavedRegion(self, wx.ID_ANY, "Save display extents to region file",
                          pos=wx.DefaultPosition, size=wx.DefaultSize,
                          style=wx.DEFAULT_DIALOG_STYLE,
                          loadsave='save')
        if dlg.ShowModal() == wx.ID_CANCEL:
            dlg.Destroy()
            return

        wind = dlg.wind

        # test to see if it already exists and ask permission to overwrite
        windpath = os.path.join(self.Map.env["GISDBASE"], self.Map.env["LOCATION_NAME"],
                                self.Map.env["MAPSET"],"windows",wind)

        if windpath and not os.path.exists(windpath):
            self.SaveRegion(wind)
        elif windpath and os.path.exists(windpath):
            overwrite = wx.MessageBox(_("Region file <%s>already exists.\nDo you want to overwrite it?") % (wind),
                                      _("Warning"), wx.YES_NO)
            if (overwrite == wx.YES):
                self.SaveRegion(wind)
        else:
            pass

        dlg.Destroy()

    def SaveRegion(self, wind):
        """Save region settings"""
        new = self.Map.alignResolution()

        cmdRegion = ["g.region",
                     "-u",
                     "n=%f" % new['n'],
                     "s=%f" % new['s'],
                     "e=%f" % new['e'],
                     "w=%f" % new['w'],
                     "rows=%d" % new['rows'],
                     "cols=%d" % new['cols'],
                     "save=%s" % wind,
                     "--o"]

        tmpreg = os.getenv("GRASS_REGION")
        os.unsetenv("GRASS_REGION")

        p = cmd.Command(cmdRegion)

        if tmpreg:
            os.environ["GRASS_REGION"] = tmpreg

class MapFrame(wx.Frame):
    """
    Main frame for map display window. Drawing takes place in child double buffered
    drawing window.
    """

    def __init__(self, parent=None, id = wx.ID_ANY, title="GRASS GIS Map display",
                 pos=wx.DefaultPosition, size=wx.DefaultSize,
                 style=wx.DEFAULT_FRAME_STYLE, toolbars=["map"],
                 tree=None, notebook=None, gismgr=None, page=None,
                 Map=None, auimgr=None):
        """
            Main map display window with toolbars, statusbar and
            DrawWindow

            Parameters:
                parent  -- parent window, None, wx.Window()
                id      -- window ID, int, wx.NewId()
                title   -- window title, string
                pos     -- where to place it, tupple, wx.Position
                size    -- window size, tupple, wx.Size
                style   -- window style
                toolbars-- array of default toolbars, which should appear
                    map, digit
                notebook-- control book ID in GIS Manager
                tree    -- associated layer tree
                gismgr  -- GIS Manager panel
                page    -- notebook page with layer tree
                Map     -- instance of render.Map
        """

        Debug.msg (1, "MapFrame.__init__(): size=%d,%d" % (size[0], size[1]))

        wx.Frame.__init__(self, parent, id, title, pos, size, style)

        # most of the thime, this will be the gis manager
        self.gismanager = gismgr # GIS Manager object
        self.Map = Map # instance of render.Map
        self.tree = tree # GIS Manager layer tree object
        self.page = page # Notebook page holding the layer tree
        self.layerbook = notebook #GIS Manager layer tree notebook
        # available cursors
        self.cursors = {
            # default: cross
            # "default" : wx.StockCursor(wx.CURSOR_DEFAULT),
            "default" : wx.StockCursor(wx.CURSOR_CROSS),
            "cross"   : wx.StockCursor(wx.CURSOR_CROSS),
            "hand"    : wx.StockCursor(wx.CURSOR_HAND),
            "pencil"  : wx.StockCursor(wx.CURSOR_PENCIL),
            "sizenwse": wx.StockCursor(wx.CURSOR_SIZENWSE)
            }

        #
        # Set the size & cursor
        #
        self.SetClientSize(size)
        self.iconsize = (16, 16)

        #
        # Fancy gui
        #
#        self._mgr = auimgr
        self._mgr = wx.aui.AuiManager(self)
        self.SetIcon(wx.Icon(os.path.join(imagepath,'grass.map.gif'), wx.BITMAP_TYPE_ANY))

        #
        # Add toolbars
        #
        self.maptoolbar = None
        self.digittoolbar = None
        for toolb in toolbars:
            self.AddToolbar(toolb)

        #
        # Add statusbar
        #
        self.statusbar = self.CreateStatusBar(number=2, style=0)
        self.statusbar.SetStatusWidths([-5, -2])
        self.Map.SetRegion()
        map_frame_statusbar_fields = ["Ext: %.2f(W)-%.2f(E), %.2f(N)-%.2f(S)" %
                                      (self.Map.region["w"], self.Map.region["e"],
                                       self.Map.region["n"], self.Map.region["s"]),
                                      "%s,%s" %(None, None)]
        for i in range(len(map_frame_statusbar_fields)):
            self.statusbar.SetStatusText(map_frame_statusbar_fields[i], i)


        # d.barscale overlay added to rendering overlay list
        self.Map.AddOverlay(0, type='overlay', command=['d.barscale'], l_active=False, l_render=False)
        # d.barscale overlay added to rendering overlay list as placeholder for d.legend
        self.Map.AddOverlay(1, type='overlay', command=['d.barscale'], l_active=False, l_render=False)

        #
        # Init map display
        #
        self.InitDisplay() # initialize region values

        self.dist = 0.0 #segment length for measurement
        self.totaldist = 0.0 # total length measured
        # initialize buffered DC
        # self.MapWindow = DrawWindow(self)
        self.MapWindow = BufferedWindow(self, id = wx.ID_ANY, Map=self.Map, tree=self.tree) # initialize buffered DC
        self.MapWindow.Bind(wx.EVT_MOTION, self.OnMotion)
        self.MapWindow.SetCursor(self.cursors["default"]) # default

        #
        # Init zoomhistory
        #
        self.MapWindow.ZoomHistory(self.Map.region['n'],self.Map.region['s'],self.Map.region['e'],self.Map.region['w'])

        # decoration overlays
        self.ovlchk = self.MapWindow.ovlchk
        self.ovlcoords = self.MapWindow.ovlcoords
        self.params = {} # previously set decoration options parameters to insert into options dialog
        self.propwin = {} # ID of properties window open for overlay, indexed by overlay ID

        #
        # Bind various events
        #
        self.Bind(wx.EVT_ACTIVATE, self.OnFocus)
        self.Bind(wx.EVT_CLOSE,    self.OnCloseWindow)

        #
        # Update fancy gui style
        #
        self._mgr.AddPane(self.MapWindow, wx.aui.AuiPaneInfo().CentrePane().
                   Dockable(False).BestSize((-1,-1)).
                   CloseButton(False).DestroyOnClose(True).
                   Layer(0))
        self._mgr.Update()

        #
        # Init print module and classes
        #
        self.printopt = disp_print.PrintOptions(self, self.MapWindow)

        #
        # Current location information
        #
        self.projinfo = self.Map.ProjInfo()

    def AddToolbar(self, name):
        """
        Add defined toolbar to the window

        Currently known toolbars are:
            * map
            * digit
        """
        if name == "map":
            self.maptoolbar = toolbars.MapToolbar(self, self.Map)

            self._mgr.AddPane(self.maptoolbar.toolbar,
                              wx.aui.AuiPaneInfo().
                              Name("maptoolbar").Caption("Map Toolbar").
                              ToolbarPane().Top().
                              LeftDockable(False).RightDockable(False).
                              BottomDockable(False).TopDockable(True).
                              CloseButton(False).Layer(2))

        elif name == "digit":
            self.digittoolbar = toolbars.DigitToolbar(self, self.Map)

            self._mgr.AddPane(self.digittoolbar.toolbar, wx.aui.AuiPaneInfo().
                              Name("digittoolbar").Caption("Digit Toolbar").
                              ToolbarPane().Top().Row(1).
                              LeftDockable(False).RightDockable(True).
                              BottomDockable(False).TopDockable(False).
                              CloseButton(False).Layer(2))

        self._mgr.Update()

    def RemoveToolbar (self, name):
        """
        Removes toolbar from the window

        TODO: Only hide, activate by calling AddToolbar()
        """

        # cannot hide main toolbar
        if name == "map":
            return
        elif name == "digit":
            # TODO: not destroy only hide
            self._mgr.DetachPane (self.digittoolbar.toolbar)
            self.digittoolbar.toolbar.Destroy()
            self.digittoolbar = None

        self.maptoolbar.combo.SetValue ("");
        self._mgr.Update()

    def InitDisplay(self):
        """
        Initialize map display, set dimensions and map region
        """
        self.width, self.height = self.GetClientSize()
        self.Map.geom = self.width, self.height
        self.Map.GetRegion()
        self.Map.SetRegion()

    def OnFocus(self, event):
        """
        Change choicebook
        page to match display
        """

        # change bookcontrol page to page associted with display
        if self.page:
            pgnum = self.layerbook.GetPageIndex(self.page)
            if pgnum > -1:
                self.layerbook.SetSelection(pgnum)
        event.Skip()

    def OnMotion(self, event):
        """
        Mouse moved
        Track mouse motion and update status bar
        """
        # store current mouse position
        posx, posy = event.GetPositionTuple()

        # upsdate coordinates
        x, y = self.MapWindow.Pixel2Cell(posx, posy)
        self.statusbar.SetStatusText("%.2f,%.2f" % (x, y), 1)

        event.Skip()

    def ReDraw(self, event):
        """
        Redraw button clicked
        """
        self.MapWindow.UpdateMap()

    def ReRender(self, event):
        """
        Rerender button clicked
        """
        self.render = True
        self.MapWindow.UpdateMap()

    def Pointer(self, event):
        """Pointer button clicled"""
        self.MapWindow.mouse['use'] = "pointer"
        self.MapWindow.mouse['box'] = "point"

        # change the cursor
        self.MapWindow.SetCursor(self.cursors["default"]) # default

    def OnZoomIn(self, event):
        """
        Zoom in the map.
        Set mouse cursor, zoombox attributes, and zoom direction
        """
        self.MapWindow.mouse['use'] = "zoom"
        self.MapWindow.mouse['box'] = "box"
        self.MapWindow.zoomtype = 1
        self.MapWindow.pen = wx.Pen(colour='Red', width=2, style=wx.SHORT_DASH)

        # change the cursor
        self.MapWindow.SetCursor(self.cursors["cross"])

    def OnZoomOut(self, event):
        """
        Zoom out the map.
        Set mouse cursor, zoombox attributes, and zoom direction
        """
        self.MapWindow.mouse['use'] = "zoom"
        self.MapWindow.mouse['box'] = "box"
        self.MapWindow.zoomtype = -1
        self.MapWindow.pen = wx.Pen(colour='Red', width=2, style=wx.SHORT_DASH)

        # change the cursor
        self.MapWindow.SetCursor(self.cursors["cross"])

    def OnZoomBack(self, event):
        """
        Zoom last (previously stored position)
        """
        self.MapWindow.ZoomBack()

    def OnPan(self, event):
        """
        Panning, set mouse to drag
        """
        self.MapWindow.mouse['use'] = "pan"
        self.MapWindow.mouse['box'] = "pan"
        self.MapWindow.zoomtype = 0
#        event.Skip()

        # change the cursor
        self.MapWindow.SetCursor(self.cursors["hand"])

    def OnErase(self, event):
        """
        Erase the canvas
        """
        self.MapWindow.EraseMap()

    def OnZoomRegion(self, event):
        """
        Zoom to region
        """
        self.Map.getRegion()
        self.Map.getResolution()
        self.UpdateMap()
#        event.Skip()

    def OnAlignRegion(self, event):
        """
        Align region
        """
        if not self.Map.alignRegion:
            self.Map.alignRegion = True
        else:
            self.Map.alignRegion = False
#        event.Skip()

    def SaveToFile(self, event):
        """
        Save to file
        """
        filetype =  "PNG file (*.png)|*.png|"\
                    "TIF file (*.tif)|*.tif|"\
                    "GIF file (*.gif)|*.gif"

        dlg = wx.FileDialog(self, "Choose a file name to save the image as a PNG to",
            defaultDir = "",
            defaultFile = "",
            wildcard = filetype,
            style=wx.SAVE|wx.FD_OVERWRITE_PROMPT)
        if dlg.ShowModal() == wx.ID_OK:
            base = os.path.splitext(dlg.GetPath())[0]
            ext = os.path.splitext(dlg.GetPath())[1]
            if dlg.GetFilterIndex() == 0:
                type = wx.BITMAP_TYPE_PNG
                path = dlg.GetPath()
                if ext != '.png': path = base+'.png'
            elif dlg.GetFilterIndex() == 1:
                type = wx.BITMAP_TYPE_TIF
                if ext != '.tif': path = base+'.tif'
            elif dlg.GetFilterIndex() == 2:
                type = wx.BITMAP_TYPE_TIF
                if ext != '.gif': path = base+'.gif'
            self.MapWindow.SaveToFile(path, type)
        dlg.Destroy()

    def PrintMenu(self, event):
        """
        Print map display
        """

        """
        Print options and output menu
        """
        point = wx.GetMousePosition()
        printmenu = wx.Menu()
        # Add items to the menu
        setup = wx.MenuItem(printmenu, -1,'Page setup')
        printmenu.AppendItem(setup)
        self.Bind(wx.EVT_MENU, self.printopt.OnPageSetup, setup)

        preview = wx.MenuItem(printmenu, -1,'Print preview')
        printmenu.AppendItem(preview)
        self.Bind(wx.EVT_MENU, self.printopt.OnPrintPreview, preview)

        doprint = wx.MenuItem(printmenu, -1,'Print display')
        printmenu.AppendItem(doprint)
        self.Bind(wx.EVT_MENU, self.printopt.OnDoPrint, doprint)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(printmenu)
        printmenu.Destroy()

    def OnCloseWindow(self, event):
        """
        Window closed
        Also close associated layer tree page
        """
        pgnum = None
        self.Map.Clean()
        if self.page:
            pgnum = self.layerbook.GetPageIndex(self.page)
            if pgnum > -1:
                self.layerbook.DeletePage(pgnum)

        #self.Destroy()

    def getRender(self):
        """
        returns the current instance of render.Map()
        """
        return self.Map

    def OnQuery(self, event):
        """
        Query currrent or last map
        """
        # switch GIS Manager to output console to show query results
        self.gismanager.notebook.SetSelection(1)

        self.MapWindow.mouse['use'] = "query"
        self.MapWindow.mouse['box'] = "point"
        self.MapWindow.zoomtype = 0
        # event.Skip()

        # change the cursor
        self.MapWindow.SetCursor(self.cursors["cross"])

    def QueryMap(self, x, y):
        """
        Run *.what command in gis manager output window
        """
        #set query snap distance for v.what at mapunit equivalent of 10 pixels
        qdist = 10.0 * ((self.Map.region['e'] - self.Map.region['w']) / self.Map.width)
        east,north = self.MapWindow.Pixel2Cell(x, y)

        if self.tree.GetSelections():
            mapname = None
            raststr = ''
            vectstr = ''
            rcmd = []
            vcmd = []
            for layer in self.tree.GetSelections():
                type =   self.tree.layers[layer].type
                dcmd = self.tree.GetPyData(layer)[0]
                if type in ('raster', 'rgb', 'his'):
                    for item in dcmd:
                        if 'map=' in item:
                            raststr += "%s," % item.split('=')[1]
                        elif 'red=' in item:
                            raststr += "%s," % item.split('=')[1]
                        elif 'h_map=' in item:
                            raststr += "%s," % item.split('=')[1]
                elif type in ('vector', 'thememap', 'themechart'):
                    for item in dcmd:
                        if 'map=' in item:
                            vectstr += "%s," % item.split('=')[1]

            # build query commands for any selected rasters and vectors
            if raststr != '':
                raststr = raststr.rstrip(',')
                rcmd = ['r.what',
                        '-f',
                        'input=%s' % (raststr),
                        'east_north=%f,%f' % (float(east), float(north))]
            if vectstr != '':
                vectstr = vectstr.rstrip(',')
                vcmd = ['v.what',
                        '-a',
                        'map=%s' % vectstr,
                        'east_north=%f,%f' % (float(east), float(north)),
                        'distance=%f' % qdist]
        else:
            dlg = wx.MessageDialog(self, _('You must select a map in the GIS Manager to query'),
                                   _('Nothing to query'), wx.OK | wx.ICON_INFORMATION)
            dlg.ShowModal()
            dlg.Destroy()
            return

        # parse query command(s)
        if self.gismanager:
            if rcmd:
                self.gismanager.goutput.RunCmd(' '.join(rcmd))
            if vcmd:
                self.gismanager.goutput.RunCmd(' '.join(vcmd))
        else:
            os.system(' '.join(rcmd))
            os.system(' '.join(vcmd))

    def OnAnalyze(self, event):
        """
        Analysis tools menu
        """
        point = wx.GetMousePosition()
        toolsmenu = wx.Menu()
        # Add items to the menu
        measure = wx.MenuItem(toolsmenu, -1, Icons["measure"].GetLabel())
        measure.SetBitmap(Icons["measure"].GetBitmap(self.iconsize))
        toolsmenu.AppendItem(measure)
        self.Bind(wx.EVT_MENU, self.OnMeasure, measure)

        profile = wx.MenuItem(toolsmenu, -1, Icons["profile"].GetLabel())
        profile.SetBitmap(Icons["profile"].GetBitmap(self.iconsize))
        toolsmenu.AppendItem(profile)
        self.Bind(wx.EVT_MENU, self.Profile, profile)

        histogram = wx.MenuItem(toolsmenu, -1, Icons["histogram"].GetLabel())
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

        # switch GIS Manager to output console to show measure results
        self.gismanager.notebook.SetSelection(1)

        # change mouse to draw line for measurement
        self.MapWindow.mouse['use'] = "measure"
        self.MapWindow.mouse['box'] = "line"
        self.MapWindow.zoomtype = 0
        self.MapWindow.pen = wx.Pen(colour='Red', width=2, style=wx.SHORT_DASH)
        self.MapWindow.polypen = wx.Pen(colour='green', width=2, style=wx.SHORT_DASH)

        # change the cursor
        self.MapWindow.SetCursor(self.cursors["pencil"])

        # initiating output
        if self.projinfo['proj'] != 'xy':
            units = self.projinfo['units']
            self.gismanager.goutput.cmd_output.write('\nMeasuring distance ('+units+'):\n')
        else:
            self.gismanager.goutput.cmd_output.write('\nMeasuring distance:\n')

    def MeasureDist(self, beginpt, endpt):
        """
        Calculate map distance from screen distance
        and print to output window
        """
        x1,y1 = beginpt
        x2,y2 = endpt
        east = (x2-x1) * self.Map.region["ewres"]
        north = (y2-y1) * self.Map.region["nsres"]
        self.dist = round(math.sqrt(math.pow((east),2) + math.pow((north),2)),3)
        self.totaldist += self.dist
        d,dunits = self.FormatDist(self.dist)
        td,tdunits = self.FormatDist(self.totaldist)
        strdist = str(d)
        strtotdist = str(td)

        if self.projinfo['proj'] == 'xy' or 'degree' not in self.projinfo['unit']:
            angle = int(math.degrees(math.atan2(north,east)) + 0.5)
            angle = angle+90
            if angle < 0: angle = 360+angle

            mstring = 'segment = %s %s\ttotal distance = %s %s\tbearing = %d deg\n' \
                % (strdist,dunits,strtotdist,tdunits,angle)
        else:
            mstring = 'segment = %s %s\ttotal distance = %s %s\n' \
                % (strdist,dunits,strtotdist,tdunits)
        self.gismanager.goutput.cmd_output.write(mstring)

    def Profile(self, event):
        """
        Init profile canvas and tools
        """
        self.profile = profile.ProfileFrame(self,
                                           id=wx.ID_ANY, pos=wx.DefaultPosition, size=(700,300),
                                           style=wx.DEFAULT_FRAME_STYLE)
        self.profile.Show()

    def FormatDist(self, dist):
        """Format length numbers and units in a nice way,
        as a function of length. From code by Hamish Bowman
        Grass Development Team 2006"""

        mapunits = self.projinfo['units']
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
            outdist = float('%g' % dist/divisor)

        return (outdist,outunits)


    def Histogram(self, event):
        """
        Init histogram display canvas and tools
        """
        self.histogram = histogram.HistFrame(self,
                                             id=wx.ID_ANY, pos=wx.DefaultPosition, size=(400,300),
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
        addscale = wx.MenuItem(decmenu, -1, Icons["addbarscale"].GetLabel())
        addscale.SetBitmap(Icons["addbarscale"].GetBitmap(self.iconsize))
        decmenu.AppendItem(addscale)
        self.Bind(wx.EVT_MENU, self.AddBarscale, addscale)

        AddLegend = wx.MenuItem(decmenu, -1, Icons["addlegend"].GetLabel())
        AddLegend.SetBitmap(Icons["addlegend"].GetBitmap(self.iconsize))
        decmenu.AppendItem(AddLegend)
        self.Bind(wx.EVT_MENU, self.AddLegend, AddLegend)

        addtext = wx.MenuItem(decmenu, -1, Icons["addtext"].GetLabel())
        addtext.SetBitmap(Icons["addtext"].GetBitmap(self.iconsize))
        decmenu.AppendItem(addtext)
        self.Bind(wx.EVT_MENU, self.AddText, addtext)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(decmenu)
        decmenu.Destroy()

    def AddBarscale(self, event):
        """
        Handler for scale/arrow map decoration menu selection.
        """

        ovltype = id = 0 # index for overlay layer in render

        if ovltype not in self.Map.ovlookup:
            self.Map.AddOverlay(ovltype, type='overlay', command='d.barscale', l_active=False, l_render=False)

        if ovltype in self.params:
            params = self.params[ovltype]
        else:
            params = ''

        # get overlay images (overlay must be active)
        if not self.Map.ovlookup[ovltype].active:
            self.Map.ovlookup[ovltype].active = True
            self.Map.Render(force=True)

        ovldict = self.MapWindow.GetOverlay()

        if id not in ovldict:
            return

        img = ovldict[id]

        if id not in self.ovlcoords:
            self.ovlcoords[id] = [10,10]

        # decoration overlay control dialog
        dlg = DecDialog(self, wx.ID_ANY, _('Scale and North arrow'), size=(350, 200),
                        style=wx.DEFAULT_DIALOG_STYLE,
                        ovltype=ovltype,
                        cmd='d.barscale',
                        drawid=id,
                        checktxt = _("Show/hide scale and arrow"),
                        ctrltxt = _("scale object"),
                        params = params)

        dlg.CenterOnScreen()

        # if OK button pressed in decoration control dialog
        if dlg.ShowModal() == wx.ID_OK:
            if self.ovlchk[id] == True:
                self.MapWindow.Draw(self.MapWindow.pdc, drawid=id,
                                    img=img, pdctype='image',
                                    coords=self.ovlcoords[id])

        # update the map canvas
        self.MapWindow.UpdateMap()

        dlg.Destroy()

        # close properties dialog if open
        #        try:
        #            self.propwin[ovltype].Close(True)
        #        except:
        #            pass

    def AddLegend(self, event):
        """
        Handler for legend map decoration menu selection.
        """
        ovltype = id = 1 # index for overlay layer in render

        if ovltype not in self.Map.ovlookup:
            self.Map.AddOverlay(ovltype, type='overlay', command='d.barscale', l_active=False, l_render=False)

        if ovltype in self.params:
            params = self.params[ovltype]
        else:
            params = ''

        # get overlay images (overlay must be active)
        if not self.Map.ovlookup[ovltype].active:
            self.Map.ovlookup[ovltype].active = True
            self.Map.Render(force=True)

        ovldict = self.MapWindow.GetOverlay()

        if id not in ovldict:
            return

        img = ovldict[id]

        if id not in self.ovlcoords:
            self.ovlcoords[id] = [10,10]

        # Decoration overlay control dialog
        dlg = DecDialog(self, wx.ID_ANY, 'Legend', size=(350, 200),
                         style=wx.DEFAULT_DIALOG_STYLE,
                         ovltype=ovltype,
                         cmd='d.legend',
                         drawid=id,
                         checktxt = "Show/hide legend",
                         ctrltxt = "legend object",
                         params = params)

        dlg.CenterOnScreen()

        # If OK button pressed in decoration control dialog
        val = dlg.ShowModal()
        if val == wx.ID_OK:
            if self.ovlchk[id] == True:
                self.MapWindow.Draw(self.MapWindow.pdc, drawid=id,
                                       img=img, pdctype='image',
                                       coords=self.ovlcoords[id])

        self.MapWindow.UpdateMap()
        dlg.Destroy()
        # close properties dialog if open
#        try:
#            self.propwin[ovltype].Close(True)
#        except:
#            pass

    def AddText(self, event):
        """
        Handler for text decoration menu selection.
        """
        ovltype = 2 # index for overlay layer in render

        # default values
        maptext = ''
        textfont = self.GetFont()
        textcolor = wx.BLACK
        textcoords = [10, 10, 10, 10]
        rotation = 0.0

        if self.MapWindow.currtxtid == None: # text doesn't already exist
            id = wx.NewId() + 100
        else: # text already exists
            id = self.MapWindow.currtxtid
            textcoords = self.ovlcoords[id]

        dlg = TextDialog(self, wx.ID_ANY, 'Text', size=(400, 200),
                         style=wx.DEFAULT_DIALOG_STYLE,
                         ovltype=ovltype,
                         drawid=id)

        dlg.CenterOnScreen()

        # If OK button pressed in decoration control dialog
        val = dlg.ShowModal()
        if val == wx.ID_OK:
            maptext    = dlg.currText
            textfont   = dlg.currFont
            textcolor  = dlg.currClr
            rotation   = dlg.currRot
            coords,w,h = self.MapWindow.TextBounds((maptext, textfont, textcolor, rotation),textcoords)

        # delete object if if it has no text
        if maptext == '':
            self.MapWindow.pdc.ClearId(id)
            self.MapWindow.pdc.RemoveId(id)
            del self.MapWindow.textdict[id]
            del self.ovlcoords[id]
            return

        self.MapWindow.pdc.ClearId(id)
        self.MapWindow.pdc.SetId(id)
        self.MapWindow.textdict[id] = (maptext,textfont,textcolor,rotation)
        self.MapWindow.Draw(self.MapWindow.pdc, img=self.MapWindow.textdict[id],
                            drawid=id, pdctype='text', coords=textcoords)
        self.MapWindow.UpdateMap()


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

    def OnZoomMenu(self, event):
        """
        Decorations overlay menu
        """
        point = wx.GetMousePosition()
        zoommenu = wx.Menu()
        # Add items to the menu
        zoommap = wx.MenuItem(zoommenu, -1,'Zoom to selected map')
        zoommenu.AppendItem(zoommap)
        self.Bind(wx.EVT_MENU, self.MapWindow.ZoomToMap, zoommap)

        zoomwind = wx.MenuItem(zoommenu, -1,'Zoom to computational region (set with g.region)')
        zoommenu.AppendItem(zoomwind)
        self.Bind(wx.EVT_MENU, self.MapWindow.ZoomToWind, zoomwind)

        savewind = wx.MenuItem(zoommenu, -1,'Set computational region from display')
        zoommenu.AppendItem(savewind)
        self.Bind(wx.EVT_MENU, self.MapWindow.DisplayToWind, savewind)

        zoomsaved = wx.MenuItem(zoommenu, -1,'Zoom to saved region')
        zoommenu.AppendItem(zoomsaved)
        self.Bind(wx.EVT_MENU, self.MapWindow.ZoomToSaved, zoomsaved)

        savezoom = wx.MenuItem(zoommenu, -1,'Save display geometry to named region')
        zoommenu.AppendItem(savezoom)
        self.Bind(wx.EVT_MENU, self.MapWindow.SaveDisplayRegion, savezoom)

        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.PopupMenu(zoommenu)
        zoommenu.Destroy()

# end of class MapFrame

class DecDialog(wx.Dialog):
    """
    Controls setting options and displaying/hiding map overlay decorations
    """
    def __init__(self, parent, id, title, pos=wx.DefaultPosition, size=wx.DefaultSize,
                 style=wx.DEFAULT_DIALOG_STYLE, ovltype=0, cmd='d.barscale',
                 drawid=None, checktxt='', ctrltxt='', params=''):
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)

        self.ovltype = ovltype
        self.drawid  = drawid
        self.ovlcmd  = cmd
        self.parent  = parent
        self.ovlchk  = self.parent.MapWindow.ovlchk
        self.params  = params #previously set decoration options to pass back to options dialog

        #self.MakeModal(True)

        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        self.chkbox = wx.CheckBox(self, wx.ID_ANY, checktxt)
        if not drawid in self.ovlchk:
            self.ovlchk[drawid] = True
        self.chkbox.SetValue(self.ovlchk[drawid])
        box.Add(self.chkbox, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        optnbtn = wx.Button(self, wx.ID_ANY, "Set options")
        box.Add(optnbtn, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, -1, ("Drag %s with mouse in pointer mode\nto position. Double-click to change options" % ctrltxt))
        box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

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

        self.Bind(wx.EVT_CHECKBOX, self.OnCheck,   self.chkbox)
        self.Bind(wx.EVT_BUTTON,   self.OnOptions, optnbtn)

    def OnCheck(self, event):
        """
        Handler for checkbox for displaying/hiding decoration
        """
        check = event.IsChecked()
        self.ovlchk[self.drawid] = check

    def OnOptions(self, event):
        """
        Sets option for decoration map overlays
        """
        # display properties dialog (modal mode)
        menuform.GUI().ParseCommand(self.ovlcmd, gmpath,
                                    completed=(self.parent.GetOptData, self.ovltype, self.params),
                                    parentframe=self, modal=True)

class TextDialog(wx.Dialog):
    """
    Controls setting options and displaying/hiding map overlay decorations
    """

    def __init__(self, parent, id, title, pos=wx.DefaultPosition, size=wx.DefaultSize,
                 style=wx.DEFAULT_DIALOG_STYLE,
                 ovltype=2,drawid=None):

        wx.Dialog.__init__(self, parent, id, title, pos, size, style)

        self.ovltype = ovltype
        self.drawid  = drawid
        self.parent  = parent

        if drawid in self.parent.MapWindow.textdict:
            self.currText, self.currFont, self.currClr, self.currRot = self.parent.MapWindow.textdict[drawid]
        else:
            self.currClr = wx.BLACK
            self.currText = ''
            self.currFont = self.GetFont()
            self.currRot = 0.0

        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, wx.ID_ANY, _("Enter text:"))
        box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)

        self.textentry = wx.TextCtrl(self, wx.ID_ANY, "", size=(200,-1))
        self.textentry.SetFont(self.currFont)
        self.textentry.SetForegroundColour(self.currClr)
        self.textentry.SetValue(self.currText)
        box.Add(self.textentry, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, wx.ID_ANY, "Rotation:")
        box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        self.rotation = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                        size=(75,-1), style=wx.SP_ARROW_KEYS)
        self.rotation.SetRange(-360, 360)
        self.rotation.SetValue(int(self.currRot))
        box.Add(self.rotation, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        fontbtn = wx.Button(self, wx.ID_ANY, "Set font")
        box.Add(fontbtn, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        box = wx.BoxSizer(wx.HORIZONTAL)
        label = wx.StaticText(self, wx.ID_ANY, ("Drag text with mouse in pointer mode\nto position. Double-click to change options"))
        box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
        sizer.Add(box, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL, 5)

        line = wx.StaticLine(self, wx.ID_ANY, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(line, 0, wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.RIGHT|wx.TOP, 5)

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

        # bindings
        self.Bind(wx.EVT_BUTTON,     self.OnSelectFont, fontbtn)
        self.Bind(wx.EVT_TEXT,       self.OnText,       self.textentry)
        self.Bind(wx.EVT_SPINCTRL,   self.OnRotation,   self.rotation)

    def OnText(self, event):
        """Change text string"""
        self.currText = event.GetString()

    def OnRotation(self, event):
        """Change rotation"""
        self.currRot = event.GetInt()
        Debug.msg (5, "TextDialog.OnRotation(): rotation=%f" % \
               self.currRot)

        event.Skip()

    def OnSelectFont(self, event):
        """Change font"""
        data = wx.FontData()
        data.EnableEffects(True)
        data.SetColour(self.currClr)         # set colour
        data.SetInitialFont(self.currFont)

        dlg = wx.FontDialog(self, data)

        if dlg.ShowModal() == wx.ID_OK:
            data = dlg.GetFontData()
            self.currFont = data.GetChosenFont()
            self.currClr = data.GetColour()

            self.textentry.SetFont(self.currFont)
            self.textentry.SetForegroundColour(self.currClr)

            self.Layout()

        dlg.Destroy()


class SavedRegion(wx.Dialog):
    def __init__(self, parent, id, title="", pos=wx.DefaultPosition, size=wx.DefaultSize,
                 style=wx.DEFAULT_DIALOG_STYLE,
                 loadsave='load'):
        """
        Loading and saving of display extents to saved region file
        """
        wx.Dialog.__init__(self, parent, id, title, pos, size, style)

        self.loadsave = loadsave
        self.wind = ''

        sizer = wx.BoxSizer(wx.VERTICAL)

        box = wx.BoxSizer(wx.HORIZONTAL)
        if loadsave == 'load':
            label = wx.StaticText(self, wx.ID_ANY, "Load region:")
            box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
            self.selection = select.Select(self, id=wx.ID_ANY, size=(300,-1),
                                              type='windows')
            box.Add(self.selection, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
            self.selection.Bind(wx.EVT_TEXT, self.onSelection)

        elif loadsave == 'save':
            label = wx.StaticText(self, wx.ID_ANY, "Save region:")
            box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
            self.textentry = wx.TextCtrl(self, wx.ID_ANY, "", size=(200,-1))
            box.Add(self.textentry, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
            self.textentry.Bind(wx.EVT_TEXT, self.onText)

        sizer.Add(item=box, proportion=0, flag=wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.ALL,
                  border=5)

        line = wx.StaticLine(self, wx.ID_ANY, size=(20,-1), style=wx.LI_HORIZONTAL)
        sizer.Add(item=line, proportion=0,
                  flag=wx.GROW|wx.ALIGN_CENTER_VERTICAL|wx.LEFT|wx.RIGHT, border=5)

        btnsizer = wx.StdDialogButtonSizer()

        btn = wx.Button(self, wx.ID_OK)
        btn.SetDefault()
        btnsizer.AddButton(btn)

        btn = wx.Button(self, wx.ID_CANCEL)
        btnsizer.AddButton(btn)
        btnsizer.Realize()


        sizer.Add(item=btnsizer, proportion=0, flag=wx.ALIGN_CENTER|wx.ALL, border=5)

        self.SetSizer(sizer)
        sizer.Fit(self)

    def onSelection(self, event):
        self.wind = event.GetString()

    def onText(self, event):
        self.wind = event.GetString()

class MapApp(wx.App):
    """
    MapApp class
    """

    def OnInit(self):
        wx.InitAllImageHandlers()
        if __name__ == "__main__":
            Map = render.Map() # instance of Map class to render GRASS display output to PPM file
        else:
            Map = None

        self.mapFrm = MapFrame(parent=None, id=wx.ID_ANY, Map=Map, size=(640,480))
        #self.SetTopWindow(Map)
        self.mapFrm.Show()

        if __name__ == "__main__":
            # redraw map, if new command appears
            self.redraw = False
            status = Command(self, Map)
            status.start()
            self.timer = wx.PyTimer(self.watcher)
            # check each 0.1s
            self.timer.Start(100)

        return 1

    def OnExit(self):
        if __name__ == "__main__":
            # stop the timer
            self.timer.Stop()
            # terminate thread (a bit ugly)
            os.system("""echo "quit" >> %s""" % (cmdfilename))

    def watcher(self):
        """Redraw, if new layer appears"""
        if self.redraw:
            self.mapFrm.ReDraw(None)
        self.redraw = False
        return
# end of class MapApp

if __name__ == "__main__":

    ###### SET command variable
    if len(sys.argv) != 3:
        print __doc__
        sys.exit()

    title = sys.argv[1]
    cmdfilename = sys.argv[2]

    import gettext
    gettext.install("gm_map") # replace with the appropriate catalog name

    print "Starting monitor <%s>" % (title)

    gm_map = MapApp(0)
    # set title
    gm_map.mapFrm.SetTitle ("GRASS GIS - Map Display: " + title + " - Location: " + grassenv.env["LOCATION_NAME"])
    gm_map.MainLoop()

    if grassenv.env.has_key("MONITOR"):
        os.system("d.mon sel=%s" % grassenv.env["MONITOR"])

    os.remove(cmdfilename)
    os.system("""g.gisenv set="GRASS_PYCMDFILE" """)

    print "Stoping monitor <%s>" % (title)

    sys.exit()
