"""gism.py module to manage map display window and display management tools"""

# Authors: Michael Barton and Jachym Cepicky
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
# Double buffered drawing concepts from the wxPython Cookbook

import wx
import os, sys, time, glob
import render
import gism

RenderMap = render.Render() # instantiate module to render GRASS display output to PPM file

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


    def __init__(self, parent, id, pos = wx.DefaultPosition, size = wx.DefaultSize,style=wx.NO_FULL_REPAINT_ON_RESIZE):
	wx.Window.__init__(self, parent, id, pos, size, style)

	self.Bind(wx.EVT_PAINT, self.OnPaint)
	self.Bind(wx.EVT_SIZE, self.OnSize)
	self.Bind(wx.EVT_IDLE, self.OnIdle)
	self.Bind(wx.EVT_MOTION, self.MouseActions)
	self.Bind(wx.EVT_MOUSE_EVENTS, self.MouseActions)

##	self.dcmd_list = [] #list of display commands to process

	# tempfile for display command output
	self.gtemp = os.popen("g.tempfile pid=%d" % \
	    os.getpid()).readlines()[0].strip()
	self.outfile = self.gtemp+".ppm"
	self.img = "" # wx.Image object from self.outfile

	# mouse attributes like currently pressed buttons, position on
	# the screen, begin and end of dragging, and type of drawing
	self.mouse = {
	    'l': False,
	    'r':False,
	    'm':False,
	    'pos':[None,None],
	    'begin':[0,0],
	    'end':[0,0],
	    'box':"point"
	    }
	self.zoomtype = 1	 # 1 zoom out, 0 no zoom, -1 zoom in
	self.resize = False # indicates whether or not a resize event has taken place
	self.dragimg = None # initialize variable for map panning
	self.pen = None # pen for drawing zoom boxes, etc.
	self.render = True #re-render the map from GRASS or just redraw image
##	self.RenderMap = render.Render() # instantiate module to render GRASS display output to PPM file

	# OnSize called to make sure the buffer is initialized.
	# This might result in OnSize getting called twice on some
	# platforms at initialization, but little harm done.
	self.OnSize(None)

    def Draw(self, dc, img=None, dctype='image', coords='0,0'):
	## just here as a place holder.
	## This method should be over-ridden when sub-classed
	pass

    def OnPaint(self, event):
	# All that is needed here is to draw the buffer to screen
	dc = wx.BufferedPaintDC(self, self._Buffer)

    def OnSize(self,event):
	# The Buffer init is done here, to make sure the buffer is always
	# the same size as the Window

	self.Width, self.Height = self.GetClientSize()

	# Make new off screen bitmap: this bitmap will always have the
	# current drawing in it, so it can be used to save the image to
	# a file, or whatever.
	self._Buffer = wx.EmptyBitmap(self.Width, self.Height)

	self.img = self.GetImage()

	if self.img and self.Width + self.Height > 0: # scale image during resize
	    self.img = self.img.Scale(self.Width, self.Height)
	    self.render = False
	    self.UpdateMap()
	self.resize = True # re-render image on idle

    def OnIdle(self, event):
	'''Only re-render a compsite map image from GRASS during
	idle time instead of multiple times during resizing.'''
	if self.resize:
	    self.render = True
	    self.UpdateMap()
	event.Skip()

    def SaveToFile(self,FileName,FileType):
	## This will save the contents of the buffer
	## to the specified file. See the wx.Windows docs for
	## wx.Bitmap::SaveFile for the details
	self._Buffer.SaveFile(FileName,FileType)

    def UpdateMap(self, img=None):
	"""
	This would get called if the drawing needed to change, for whatever reason.

	The idea here is that the drawing is based on some data generated
	elsewhere in the system. IF that data changes, the drawing needs to
	be updated.

	"""
	if self.render:
	    self.Width, self.Height = self.GetClientSize()
	    RenderMap.geom = self.Width, self.Height
	    self.ProcessDcommand()
	    self.img = self.GetImage()
	    self.resize = False
	    if not self.img: return
	    dc = wx.BufferedDC(wx.ClientDC(self), self._Buffer)
	    self.Draw(dc, self.img)
	else:
	    if not self.img: return
	    dc = wx.BufferedDC(wx.ClientDC(self), self._Buffer)
	    self.Draw(dc, self.img)
	self.resize = False

    def EraseMap(self):
	dc = wx.BufferedDC(wx.ClientDC(self), self._Buffer)
	self.Draw(dc, dctype='clear')

    def DragMap(self, moveto):
	'''drag a bitmap image for panning.'''
	dc = wx.BufferedDC(wx.ClientDC(self), self._Buffer)
	dc.SetBackground(wx.Brush(self.GetBackgroundColour()))
	bitmap = wx.BitmapFromImage(self.img)
	self.dragimg = wx.DragImage(bitmap)
	self.dragimg.BeginDrag((0,0), self)
	self.dragimg.GetImageRect(moveto)
	self.dragimg.Move(moveto)
	dc.Clear()
	self.dragimg.DoDrawImage(dc, moveto)
	self.dragimg.EndDrag()
##	self.dragimg.UpdateBackingFromWindow(dc, memdc, sourcerect,destrect)

    def MouseDraw(self):
	''' mouse zoom rectangles and lines '''
	img = self.img # composite map in background
	dc = wx.BufferedDC(wx.ClientDC(self), self._Buffer)
	if self.mouse['box'] == "box":
	    mousecoords = [self.mouse['begin'][0],self.mouse['begin'][1], \
		self.mouse['end'][0] - self.mouse['begin'][0], \
		self.mouse['end'][1] - self.mouse['begin'][1]]
	    self.Draw(dc, img, dctype='box', coords=mousecoords)
	elif self.mouse['box'] == "line":
	    mousecoords = [self.mouse['begin'][0],self.mouse['begin'][1], \
		self.mouse['end'][0] - self.mouse['begin'][0], \
		self.mouse['end'][1] - self.mouse['begin'][1]]
	    self.Draw(dc, img, dctype='line', coords=mousecoords)

    def MouseActions(self,event):
	"""
	Mouse motion and button click notifier
	"""
	wheel = event.GetWheelRotation() # +- int
	# left mouse button pressed
	if event.LeftDown():
	    # start point of zoom box or drag
	    self.mouse['begin'] = event.GetPositionTuple()[:]

	# left mouse button released and not just a pointer
	elif event.LeftUp() and self.mouse['box'] != "point":
	    # end point of zoom box or drag
	    self.mouse['end'] = event.GetPositionTuple()[:]

	    # set region in zoom or pan
	    RenderMap.zoom(self.mouse['begin'],self.mouse['end'],self.zoomtype)

	    # redraw map
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
	    begin = [RenderMap.geom[0]/50,RenderMap.geom[1]/50]
	    end = [RenderMap.geom[0] - RenderMap.geom[0]/50,
		RenderMap.geom[1] - RenderMap.geom[1]/50]
	    if wheel > 0:
		RenderMap.zoom(begin,end,1)
	    else:
		RenderMap.zoom(begin,end,-1)
	    self.UpdateMap()

	# store current mouse position
	self.mouse['pos'] = event.GetPositionTuple()[:]

    def ProcessDcommand(self, event=None):
	'''Get a command from the console or from the layer tree
	and send it to be processed for display
	Put it in a command list
	Send to render.py to be rendered into a *.ppm file
	In gism, each layer creates its own tmp ppm file name; after
	rendering to a generic ppm, this is renamed to the specific
	ppm of that layer and the name is added to the compositing list'''

	# initialize lists of maps, masks, and variables for g.composite
	self.maplist = []
	self.masklist = []
	self.opaclist = []
	if not self.dcmd_list: return
	for cmd in self.dcmd_list:
	    gtemp = os.popen("g.tempfile pid=%d" % \
		os.getpid()).readlines()[0].strip()
	    mapFile = gtemp+".ppm"
	    maskFile = gtemp+".pgm"
	    # send command to render.py
	    # returns the name of the generic image and mask
	    img, mask = RenderMap.display_background(cmd,forceredraw=True)
	    # get the mask (pgm) name from the img (ppm) name
	    # rename the image to tmp file name
	    os.rename(img,mapFile)
	    os.rename(mask,maskFile)
	    opacity = '1'
	    # add image to compositing list
	    self.maplist.append(mapFile)
	    self.masklist.append(maskFile)
	    self.opaclist.append(opacity)
	    mapstr = ",".join(self.maplist)
	    maskstr = ",".join(self.masklist)
	    opacstr = repr(",".join(self.opaclist))
	compcmd = "g.pnmcomp in="+mapstr+" mask="+maskstr+" opacity="+opacstr+" background=255:255:255"\
	    +" width="+repr(RenderMap.geom[0])+" height="+repr(RenderMap.geom[1])\
	    +" out="+self.outfile
	# run g.composite to get composite image
	os.system(compcmd)

    def GetImage(self):
	if os.path.isfile(self.outfile):
	    self.img = wx.Image(self.outfile, wx.BITMAP_TYPE_ANY)
	else:
	    self.img = None
	return self.img

class DrawWindow(BufferedWindow):
    '''Drawing routine for double buffered drawing. Overwrites Draw method
    in the BufferedWindow class'''
    def __init__(self, parent, id = -1):
	## Any data the Draw() function needs must be initialized before
	## calling BufferedWindow.__init__, as it will call the Draw
	## function.
	self.dcmd_list = [] #list of display commands to process
	BufferedWindow.__init__(self, parent, id)

    def Draw(self, dc, img=None, dctype='image', coords=[0,0]):
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
	    dc.DrawRectangle(coords[0],coords[1],coords[2],coords[3])
	elif dctype == 'line': # draw a line on top of the map
	    dc.SetBrush(wx.Brush(wx.CYAN, wx.TRANSPARENT))
	    dc.SetPen(self.pen)
	    dc.DrawLine(coords[0],coords[1],coords[2],coords[3])
	dc.EndDrawing()

class MyFrame(wx.Frame):
    '''Main frame for map display window. Drawing takes place in child double buffered
    drawing window.'''
    def __init__(self, *args, **kwds):
	kwds["style"] = wx.DEFAULT_FRAME_STYLE
	wx.Frame.__init__(self, *args, **kwds)
	self.SetClientSize((600, 475))

	#---status bar---#000000#FFFFFF-------------------------------------------------
	self.statusbar = self.CreateStatusBar(2, 0)
	self.statusbar.SetStatusWidths([-3, -1])
	map_frame_statusbar_fields = [_("map_frame_statusbar"), _("map_frame_statusbar2")]
	for i in range(len(map_frame_statusbar_fields)):
	    self.statusbar.SetStatusText(map_frame_statusbar_fields[i], i)

	#---tool bar---#000000#FFFFFF---------------------------------------------------
	self.mtoolbar = wx.ToolBar(self, -1)
	self.SetToolBar(self.mtoolbar)

	iconpath = os.environ['GRASS_ICONPATH']

	self.displaymap = self.mtoolbar.AddLabelTool(-1, "displaymap", wx.Bitmap(iconpath+r'/gui-display.gif', wx.BITMAP_TYPE_ANY), wx.NullBitmap, wx.ITEM_NORMAL, "Display map", "")
	self.mtoolbar.AddSeparator()
	self.pointer = self.mtoolbar.AddLabelTool(-1, "pointer", wx.Bitmap(iconpath+r'/gui-pointer.gif', wx.BITMAP_TYPE_ANY), wx.NullBitmap, wx.ITEM_RADIO, "pointer", "")
	self.zoomin = self.mtoolbar.AddLabelTool(-1, "zoom_in", wx.Bitmap(iconpath+r'/gui-zoom_in.gif', wx.BITMAP_TYPE_ANY), wx.NullBitmap, wx.ITEM_RADIO, "Zoom in", "")
	self.zoomout = self.mtoolbar.AddLabelTool(-1, "zoom_out", wx.Bitmap(iconpath+r'/gui-zoom_out.gif', wx.BITMAP_TYPE_ANY), wx.NullBitmap, wx.ITEM_RADIO, "Zoom out", "")
	self.pan = self.mtoolbar.AddLabelTool(-1, "pan", wx.Bitmap(iconpath+r'/gui-pan.gif', wx.BITMAP_TYPE_ANY), wx.NullBitmap, wx.ITEM_RADIO, "Pan", "")
	self.erase = self.mtoolbar.AddLabelTool(-1, "erase", wx.Bitmap(iconpath+r'/gui-erase.gif', wx.BITMAP_TYPE_ANY), wx.NullBitmap, wx.ITEM_RADIO, "Erase display", "")
	self.savefile = self.mtoolbar.AddLabelTool(-1, "savefile", wx.Bitmap(iconpath+r'/file-save.gif', wx.BITMAP_TYPE_ANY), wx.NullBitmap, wx.ITEM_RADIO, "Save display to PNG file", "")
	self.Bind(wx.EVT_TOOL, self.ReDraw, self.displaymap)
	self.Bind(wx.EVT_TOOL, self.Pointer, self.pointer)
	self.Bind(wx.EVT_TOOL, self.OnZoomIn, self.zoomin)
	self.Bind(wx.EVT_TOOL, self.OnZoomOut, self.zoomout)
	self.Bind(wx.EVT_TOOL, self.OnPan, self.pan)
	self.Bind(wx.EVT_TOOL, self.OnErase, self.erase)
	self.Bind(wx.EVT_TOOL, self.SaveToFile, self.savefile)
	self.mtoolbar.Realize()

	self.Bind(wx.EVT_ACTIVATE, self.OnFocus)
	self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)

	# init map display
	self.InitDisplay() # initialize region values
	self.MapWindow = DrawWindow(self) # initialize buffered DC
	self.MapWindow.Bind(wx.EVT_MOTION, self.OnMotion)

    def InitDisplay(self):
	self.Width, self.Height = self.GetClientSize()
	RenderMap.geom = self.Width, self.Height
	RenderMap.getRegion()
	RenderMap.getResolution()

    def OnFocus(self, event):
	'''get map display index number from title
	and store it in variable in render.py
	so it can be found by gism.py'''
	title = self.GetTitle()
	md = title[12:]
	self.mdindex = md
	render.Render().setMdIdx(self.mdindex)
##	  render.Render().setMD(self)
##	  gism.SetVal().setMdFocus(md) #this doesn't work for some reason ??
	event.Skip()

    def SetDcommandList(self, clst):
	self.MapWindow.dcmd_list = clst
	self.MapWindow.ProcessDcommand()
	self.MapWindow.UpdateMap()

    def OnMotion(self, event):
	# store current mouse position
	posx, posy = event.GetPositionTuple()

	# set coordinates to status bar
	x, y = RenderMap.pixel2cell(posx, posy)
	self.statusbar.SetStatusText("%f,%f" % (x,y))
	event.Skip()

    def ReDraw(self, event):
	self.MapWindow.UpdateMap()

    def Pointer(self, event):
	self.MapWindow.mouse['box'] = "point"

    def OnZoomIn(self, event):
	self.MapWindow.mouse['box'] = "box"
	self.MapWindow.zoomtype = 1
	self.MapWindow.pen = wx.Pen('Red', 2)

    def OnZoomOut(self, event):
	self.MapWindow.mouse['box'] = "box"
	self.MapWindow.zoomtype = -1
	self.MapWindow.pen = wx.Pen('Green', 2)

    def OnZoomBack(self, event):
	pass

    def OnPan(self, event):
	self.MapWindow.mouse['box'] = "drag"
	self.MapWindow.zoomtype = 0
	event.Skip()

    def OnErase(self, event):
	self.MapWindow.EraseMap()

    def OnZoomRegion(self, event):
	RenderMap.getRegion()
	RenderMap.getResolution()
	self.draw(dc)
	event.Skip()

    def OnAlignRegion(self, event):
	if not RenderMap.alignRegion:
	    RenderMap.alignRegion = True
	else:
	    RenderMap.alignRegion = False
	event.Skip()

    def SaveToFile(self, event):
	dlg = wx.FileDialog(self, "Choose a file name to save the image as a PNG to",
	    defaultDir = "",
	    defaultFile = "",
	    wildcard = "*.png",
	    style=wx.SAVE)
	if dlg.ShowModal() == wx.ID_OK:
	    self.MapWindow.SaveToFile(dlg.GetPath(),wx.BITMAP_TYPE_PNG)
	dlg.Destroy()

    def OnCloseWindow(self, event):
	# delete all the temp files
	tmpdir, tmpfiles = os.path.split(self.MapWindow.gtemp)
	tmpbase = tmpfiles.split('.')[0]
	delfiles = glob.glob(os.path.join(tmpdir,tmpbase)+'.*')
	for delfile in delfiles:
	    os.remove(delfile)
	self.Destroy()

# end of class MyFrame


class MapApp(wx.App):
    def OnInit(self):
	wx.InitAllImageHandlers()
	map_frame = MyFrame(None, -1)
	self.SetTopWindow(map_frame)
	map_frame.Show()

	return 1

# end of class MapApp

if __name__ == "__main__":
    import gettext
    gettext.install("gm_map") # replace with the appropriate catalog name

    gm_map = MapApp(0)
    gm_map.MainLoop()




