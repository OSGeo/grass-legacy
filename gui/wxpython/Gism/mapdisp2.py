"""
mapdisp2 Package

Classes:
* BufferedWindow
* DrawWindow
* MapFrame
* MapApp
"""

import wx
import wx.aui
import os, sys, time, glob

import render
import gismutils
import toolbars
import grassenv

Map = render.Map() # instance of Map class to render GRASS display output to PPM file

DEBUG = False

class BufferedWindow(wx.Window):
    """
    A Buffered window class.

    To use it, subclass it and define a Draw(DC) method that takes a DC
    to draw to. In that method, put the code needed to draw the picture
    you want. The window will automatically be double buffered, and the
    screen will be automatically updated when a Paint event is received.

    When the drawing needs to change, you app needs to call the
    UpdateMap() method. Since the drawing is stored in a bitmap, you
    can also save the drawing to file by calling the
    SaveToFile(self,file_name,file_type) method.
    """

    def __init__(self, parent, id, 
            pos = wx.DefaultPosition, 
            size = wx.DefaultSize, 
            style=wx.NO_FULL_REPAINT_ON_RESIZE):

    	wx.Window.__init__(self, parent, id, pos, size, style)
        self.parent = parent

        #
        # Flags
        #
    	self.render = True  # re-render the map from GRASS or just redraw image
    	self.resize = False # indicates whether or not a resize event has taken place
    	self.dragimg = None # initialize variable for map panning
    	self.pen = None     # pen for drawing zoom boxes, etc.

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
    	self.mapfile = None # image file to be rendered
    	self.Img = ""       # wx.Image object (self.mapfile)

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
    	    'box': "point"
    	    }
        self.zoomtype = 1  # 1 zoom in, 0 no zoom, -1 zoom out

    	# OnSize called to make sure the buffer is initialized.
    	# This might result in OnSize getting called twice on some
    	# platforms at initialization, but little harm done.
        #!!! self.OnSize(None)

    def Draw(self, dc, img=None, dctype='image', coords='0,0'):
    	"""
        Just here as a place holder.
    	This method should be over-ridden when sub-classed
        """
    	pass

    def OnPaint(self, event):
    	"""
        All that is needed here is to draw the buffer to screen
        """
    	dc = wx.BufferedPaintDC(self, self._Buffer)

    def OnSize(self, event):
    	"""
        The Buffer init is done here, to make sure the buffer is always
    	the same size as the Window
        """

        # set size of the input image
    	Map.width, Map.height = self.GetClientSize()

    	# Make new off screen bitmap: this bitmap will always have the
    	# current drawing in it, so it can be used to save the image to
    	# a file, or whatever.
    	self._Buffer = wx.EmptyBitmap(Map.width, Map.height)

        # get the image to be rendered
    	self.Img = self.GetImage()

        # update map display
    	if self.Img and Map.width + Map.height > 0: # scale image during resize
    	    self.Img = self.Img.Scale(Map.width, Map.height)
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
        This will save the contents of the buffer
    	to the specified file. See the wx.Windows docs for
    	wx.Bitmap::SaveFile for the details
        """
    	self._Buffer.SaveFile(FileName, FileType)

    def UpdateMap(self, img=None):
    	"""
    	This would get called if the drawing needed to change, for whatever reason.

    	The idea here is that the drawing is based on some data generated
    	elsewhere in the system. IF that data changes, the drawing needs to
    	be updated.

    	"""
    	if self.render:
    	    Map.width, Map.height = self.GetClientSize()
    	    self.mapfile = Map.Render(force=self.render)
    	    self.Img = self.GetImage()
    	    self.resize = False
    	    if not self.Img: return
    	    dc = wx.BufferedDC(wx.ClientDC(self), self._Buffer)
    	    self.Draw(dc, self.Img)
    	else:
    	    if not self.Img: return
    	    dc = wx.BufferedDC(wx.ClientDC(self), self._Buffer)
    	    self.Draw(dc, self.Img)
    	self.resize = False

    def EraseMap(self):
        """
        Erase the map display
        """
    	dc = wx.BufferedDC(wx.ClientDC(self), self._Buffer)
    	self.Draw(dc, dctype='clear')

    def DragMap(self, moveto):
    	"""
        Drag a bitmap image for panning.
        """

    	dc = wx.BufferedDC(wx.ClientDC(self), self._Buffer)
    	dc.SetBackground(wx.Brush("White"))
    	bitmap = wx.BitmapFromImage(self.Img)
    	self.dragimg = wx.DragImage(bitmap)
    	self.dragimg.BeginDrag((0, 0), self)
    	self.dragimg.GetImageRect(moveto)
    	self.dragimg.Move(moveto)
    	dc.Clear()
    	self.dragimg.DoDrawImage(dc, moveto)
    	self.dragimg.EndDrag()
    ##	self.dragimg.UpdateBackingFromWindow(dc, memdc, sourcerect,destrect)

    def MouseDraw(self):
    	"""
        Mouse zoom rectangles and lines
        """
    	img = self.Img # composite map in background
    	dc = wx.BufferedDC(wx.ClientDC(self), self._Buffer)
    	if self.mouse['box'] == "box":
    	    mousecoords = [self.mouse['begin'][0], self.mouse['begin'][1], \
    		self.mouse['end'][0] - self.mouse['begin'][0], \
    		self.mouse['end'][1] - self.mouse['begin'][1]]
    	    self.Draw(dc, img, dctype='box', coords=mousecoords)
    	elif self.mouse['box'] == "line":
    	    mousecoords = [self.mouse['begin'][0], self.mouse['begin'][1], \
    		self.mouse['end'][0] - self.mouse['begin'][0], \
    		self.mouse['end'][1] - self.mouse['begin'][1]]
    	    self.Draw(dc, img, dctype='line', coords=mousecoords)

    def MouseActions(self, event):
    	"""
    	Mouse motion and button click notifier
    	"""
    	wheel = event.GetWheelRotation() # +- int
    	# left mouse button pressed
    	if event.LeftDown():
    	    # start point of zoom box or drag
    	    self.mouse['begin'] = event.GetPositionTuple()[:]

    	# left mouse button released and not just a pointer
        elif event.LeftUp():
            if self.mouse['box'] != "point":
                # end point of zoom box or drag
                self.mouse['end'] = event.GetPositionTuple()[:]

                # set region in zoom or pan
                self.Zoom(self.mouse['begin'], self.mouse['end'], self.zoomtype)

            else:
                # digitizing
                if self.parent.digittoolbar:
                    if self.parent.digittoolbar.digitize == "point":
                        east,north= self.Pixel2Cell(self.mouse['end'][0],self.mouse['end'][1])
                        self.parent.digittoolbar.AddPoint(east,north)
            # redraw map
            self.render=True
            self.UpdateMap()

    	# dragging or drawing box with left button
    	elif event.Dragging() and event.LeftIsDown:
    	    currpos = event.GetPositionTuple()[:]
    	    end = (currpos[0]-self.mouse['begin'][0], \
    		currpos[1]-self.mouse['begin'][1])
    	    if self.mouse['box'] == 'drag':
    		self.DragMap(end)
    	    else:
    		self.mouse['end'] = event.GetPositionTuple()[:]
    		self.MouseDraw()

    	# zoom on mouse wheel
    	elif wheel != 0:

    	    # zoom 1/2 of the screen
    	    begin = [Map.width/4, Map.height/4]
    	    end = [Map.width - Map.width/4,
    		Map.height - Map.height/4]

    	# store current mouse position
    	self.mouse['pos'] = event.GetPositionTuple()[:]

    def GetImage(self):
        """
        Converts files to wx.Image
        """
    	if Map.mapfile and os.path.isfile(Map.mapfile):
    	    self.Img = wx.Image(Map.mapfile, wx.BITMAP_TYPE_ANY)
    	else:
    	    self.Img = None
    	return self.Img

    def Pixel2Cell(self, x, y):
    	"""
    	Calculates real word coordinates to image coordinates

    	Input : x, y
    	Output: int x, int y
    	"""
    	newx = Map.region['w'] + x * Map.region["ewres"]
    	newy = Map.region['n'] - y * Map.region["nsres"]
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
    		newreg['w'], newreg['n'] = self.Pixel2Cell(
    		    -x1*2,
    		    -y1*2)
                newreg['e'], newreg['s'] = self.Pixel2Cell(
    		    Map.width+2*(Map.width-x2),
    		    Map.height+2*(Map.height-y2))
    	# pan
    	elif zoomtype == 0:
    	    newreg['w'], newreg['n'] = self.Pixel2Cell(
    		x1-x2,
    		y1-y2)
    	    newreg['e'], newreg['s'] = self.Pixel2Cell(
    		Map.width+x1-x2,
    		Map.height+y1-y2)

    	# if new region has been calculated, set the values
    	if newreg :
    	    Map.region['n'] = newreg['n']
    	    Map.region['s'] = newreg['s']
    	    Map.region['e'] = newreg['e']
    	    Map.region['w'] = newreg['w']

class DrawWindow(BufferedWindow):
    """
    Drawing routine for double buffered drawing. Overwrites Draw method
    in the BufferedWindow class
    """
    def __init__(self, parent, id = -1):
        """
        """
    	## Any data the Draw() function needs must be initialized before
    	## calling BufferedWindow.__init__, as it will call the Draw
    	## function.
    	self.dcmd_list = [] # list of display commands to process
    	BufferedWindow.__init__(self, parent, id)

    def Draw(self, dc, img=None, dctype='image', coords=[0, 0]):
        """
        Draws image, box and line in the background
        """
    	dc.BeginDrawing()
    	dc.SetBackground(wx.Brush(self.GetBackgroundColour()))
    	dc.Clear() # make sure you clear the bitmap!

    	if dctype == 'clear': # erase the display
    	    dc.EndDrawing()
    	    return
    	bitmap = wx.BitmapFromImage(img)
    	dc.DrawBitmap(bitmap, 0, 0) # draw the composite map

    	if dctype == 'box': # draw a box on top of the map
    	    dc.SetBrush(wx.Brush(wx.CYAN, wx.TRANSPARENT))
    	    dc.SetPen(self.pen)
    	    dc.DrawRectangle(coords[0], coords[1], coords[2], coords[3])
    	elif dctype == 'line': # draw a line on top of the map
    	    dc.SetBrush(wx.Brush(wx.CYAN, wx.TRANSPARENT))
    	    dc.SetPen(self.pen)
    	    dc.DrawLine(coords[0], coords[1], coords[2], coords[3])

    	dc.EndDrawing()

class MapFrame(wx.Frame):
    """
    Main frame for map display window. Drawing takes place in child double buffered
    drawing window.
    """

    def __init__(self, parent=None, id=-1, title="Map display",
            pos=wx.DefaultPosition, size=wx.DefaultSize,
            style=wx.DEFAULT_FRAME_STYLE, toolbars=["map"]):
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
        """

        wx.Frame.__init__(self, parent, id, title, pos, size, style)

        #
        # Set the size
        #
        self.SetClientSize((600, 475))

        #
        # Fancy gui
        #
        self._mgr = wx.aui.AuiManager(self)

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
    	self.statusbar.SetStatusWidths([-2, -1])
    	map_frame_statusbar_fields = ["Extent: %d,%d : %d,%d" %
                                      (Map.region["n"], Map.region["s"],
                                       Map.region["w"], Map.region["e"]),
                                      "%s,%s" %(None, None)]
    	for i in range(len(map_frame_statusbar_fields)):
    	    self.statusbar.SetStatusText(map_frame_statusbar_fields[i], i)

        # update statusbar (extent)
        self.statusbar.SetStatusText("Extent: %d,%d : %d,%d" %
                                     (Map.region["n"], Map.region["s"],
                                      Map.region["w"], Map.region["e"]), 0)

            
        #
    	# Init map display
        #
    	self.InitDisplay() # initialize region values
    	self.MapWindow = DrawWindow(self) # initialize buffered DC
    	self.MapWindow.Bind(wx.EVT_MOTION, self.OnMotion)

        #
        # Bind various events
        #
    	self.Bind(wx.EVT_ACTIVATE, self.OnFocus)
    	self.Bind(wx.EVT_CLOSE,    self.OnCloseWindow)

        #
        # Update fancy gui style
        #
        self._mgr.AddPane(self.MapWindow, wx.CENTER)
        self._mgr.Update()

    def AddToolbar(self, name):
        """
        Add defined toolbar to the window
        
        Currently known toolbars are:
            * map
            * digit
        """
        if name == "map":
            self.maptoolbar = toolbars.MapToolbar(self, Map)
            self._mgr.AddPane(self.maptoolbar.toolbar, wx.aui.AuiPaneInfo().
                          Name("maptoolbar").Caption("Map Toolbar").
                          ToolbarPane().Top().
                          LeftDockable(True).RightDockable(True).CloseButton(False))

        if name == "digit":
            self.digittoolbar = toolbars.DigitToolbar(self,Map)
            self._mgr.AddPane(self.digittoolbar.toolbar, wx.aui.AuiPaneInfo().
                          Name("digittoolbar").Caption("Digit Toolbar").
                          ToolbarPane().Top().
                          LeftDockable(True).RightDockable(True),)
        self._mgr.Update()

    def InitDisplay(self):
        """
        Initialize map display, set dimensions and map region
        """
        self.width, self.height = self.GetClientSize()
        Map.geom = self.width, self.height
        Map.GetRegion()
        #FIXME
        #This was Map.getResolution().
        #I'm guessing at the moment that this is replaced by Map.SetRegion()
        Map.SetRegion()

    def OnFocus(self, event):
        """
        Store information about active display
        in tracking variables and change choicebook
        page to match display
        """
        title = self.GetTitle()
        num = title[12:]
        
        # FIXME removed
        event.Skip()
            
    def SetDcommandList(self, clst):
        self.MapWindow.dcmd_list = clst
        self.MapWindow.ProcessDcommand()
        self.MapWindow.UpdateMap()

    def OnMotion(self, event):
        """
        Mouse moved
        Track mouse motion and update status bar
        """
        # store current mouse position
    	posx, posy = event.GetPositionTuple()


    	# upsdate coordinates
    	x, y = self.MapWindow.Pixel2Cell(posx, posy)
    	self.statusbar.SetStatusText("%d,%d" % (x, y), 1)

    	event.Skip()

    def ReDraw(self, event):
        """
        Redraw button clicked
        """
        self.MapWindow.UpdateMap()


    def Pointer(self, event):
        """Pointer button clicled"""
        self.MapWindow.mouse['box'] = "point"

    def OnZoomIn(self, event):
        """
        Zoom in the map.
        Set mouse pointer and zoom direction
        """
    	self.MapWindow.mouse['box'] = "box"
    	self.MapWindow.zoomtype = 1
    	self.MapWindow.pen = wx.Pen('Red', 2)

    def OnZoomOut(self, event):
        """
        Zoom out the map.
        Set mouse pointer and zoom direction
        """
    	self.MapWindow.mouse['box'] = "box"
    	self.MapWindow.zoomtype = -1
    	self.MapWindow.pen = wx.Pen('Green', 2)

    def OnZoomBack(self, event):
        """
        Zoom last (previously stored position)
        """
        # FIXME
        pass

    def OnPan(self, event):
        """
        Panning, set mouse to drag
        """
    	self.MapWindow.mouse['box'] = "drag"
    	self.MapWindow.zoomtype = 0
    	event.Skip()
                
    def OnErase(self, event):
        """
        Erase the canvas
        """
        self.MapWindow.EraseMap()

    def OnZoomRegion(self, event):
        """
        Zoom to region 
        """
    	Map.getRegion()
    	Map.getResolution()
    	self.draw(dc)
    	event.Skip()

    def OnAlignRegion(self, event):
        """
        Align region
        """
    	if not Map.alignRegion:
    	    Map.alignRegion = True
    	else:
    	    Map.alignRegion = False
    	event.Skip()

    def SaveToFile(self, event):
        """
        Save to file
        """
    	dlg = wx.FileDialog(self, "Choose a file name to save the image as a PNG to",
    	    defaultDir = "",
    	    defaultFile = "",
    	    wildcard = "*.png",
    	    style=wx.SAVE)
    	if dlg.ShowModal() == wx.ID_OK:
    	    self.MapWindow.SaveToFile(dlg.GetPath(), wx.BITMAP_TYPE_PNG)
    	dlg.Destroy()

    def OnCloseWindow(self, event):
        """
        Window closed
        """
        Map.Clean()
    	self.Destroy()
    

# end of class MapFrame

class MapApp(wx.App):
    """
    MapApp class
    """
    
    def OnInit(self):
	wx.InitAllImageHandlers()
	Mapfrm = MapFrame(None, -1)
	#self.SetTopWindow(Map)
	Mapfrm.Show()

        # only for testing purpose
        if __name__ == "__main__":
            Map.AddRasterLayer(name="elevation.dem", mapset="PERMANENT")
            os.system("g.copy vect=roads,tmp --o")
            Map.AddVectorLayer(name="tmp", color="red")
            Map.AddVectorLayer(name="bugsites", mapset="PERMANENT",
                               color="blue")

	return 1

# end of class MapApp

if __name__ == "__main__":
    import gettext
    gettext.install("gm_map") # replace with the appropriate catalog name


    if not os.getenv("GRASS_ICONPATH"):
        os.environ["GRASS_ICONPATH"]=os.getenv("GISBASE")+"/etc/gui/icons/"

    gm_map = MapApp(0)
    gm_map.MainLoop()
    if grassenv.env.has_key("MONITOR"):
        os.system("d.mon sel=%s" % grassenv.env["MONITOR"])
