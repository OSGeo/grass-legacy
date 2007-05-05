import wx
import wx.aui
import os, sys, time, glob, math
from threading import Thread

try:
    import subprocess
    from subprocess import *
except:
    CompatPath = os.getenv("GISBASE") + "/etc/wx"
    sys.path.append(CompatPath)
    from compat import subprocess
    from compat.subprocess import *

gmpath = os.getenv("GISBASE") + "/etc/wx/gui_modules/"
sys.path.append(gmpath)
gmpath = os.getenv("GISBASE") + "/etc/wx/icons/"
sys.path.append(gmpath)

import render
import toolbars
import grassenv
import track
import menuform
import select
import disp_print
import gui_modules.defaultfont as defaultfont
from digit import Digit as Digit
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

class BufferedWindow(wx.Window):
    """
    A Buffered window class.

    When the drawing needs to change, you app needs to call the
    UpdateHist() method. Since the drawing is stored in a bitmap, you
    can also save the drawing to file by calling the
    SaveToFile(self,file_name,file_type) method.
    """

    def __init__(self, parent, id,
                 pos = wx.DefaultPosition,
                 size = wx.DefaultSize,
                 style=wx.NO_FULL_REPAINT_ON_RESIZE,
                 Map=None):

        wx.Window.__init__(self, parent, id, pos, size, style)

        self.parent = parent
        self.Map = Map
        self.mapname = self.parent.mapname

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
#        self.Bind(wx.EVT_MOTION,       self.MouseActions)
#        self.Bind(wx.EVT_MOUSE_EVENTS, self.MouseActions)


        #
        # Render output objects
        #
        self.mapfile = None # image file to be rendered
        self.img = ""       # wx.Image object (self.mapfile)
        self.ovldict = {}   # list of images for overlays
        self.ovlcoords = {} # positioning coordinates for decoration overlay
        self.ovlchk = {}    # showing/hiding decorations
        self.imagedict = {} # images and their PseudoDC ID's for painting and dragging
        self.crop = {} # coordinates to crop overlays to their data, indexed by image ID
        self.select = {} # selecting/unselecting decorations for dragging
        self.textdict = {} # text, font, and color indexed by id
        self.currtxtid = None # PseudoDC id for currently selected text

        self.pdc = wx.PseudoDC()
        self._Buffer = '' # will store an off screen empty bitmap for saving to file
        self.Map.SetRegion() # make sure that extents are updated at init

        self.Bind(wx.EVT_ERASE_BACKGROUND, lambda x:None)

    def Draw(self, pdc, img=None, drawid=None, pdctype='image', coords=[0,0,0,0]):
        """
        Draws map decorations on top of map
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

        Debug.msg (3, "BufferedWindow.Draw(): id=%s, pdctype=%s, coord=%s" % (drawid, pdctype, coords))

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


        elif pdctype == 'text': # draw text on top of map
            text = img[0]
            rotation = float(img[3])
            w,h = self.GetFullTextExtent(img[0])[0:2]
            pdc.SetFont(img[1])
            pdc.SetTextForeground(img[2])
            coords,w,h = self.textBounds(img,coords)
            if rotation == 0:
                pdc.DrawText(img[0], coords[0], coords[1])
            else:
                pdc.DrawRotatedText(img[0], coords[0], coords[1], rotation)
            pdc.SetIdBounds(drawid, (coords[0], coords[1], w, h))
            self.ovlcoords[drawid] = coords

        pdc.EndDrawing()
        self.Refresh()

    def textBounds(self, textinfo, coords):
        rotation = float(textinfo[3])
        self.Update()
        self.Refresh()
        self.SetFont(textinfo[1])
        w,h = self.GetTextExtent(textinfo[0])
        if rotation == 0:
            coords[2], coords[3] = coords[0] + w, coords[1] + h
            return coords,w,h
        else:
            boxh = math.fabs(math.sin(math.radians(rotation)) * w) + h
            boxw = math.fabs(math.cos(math.radians(rotation)) * w) + h
            coords[2] = coords[0] + boxw
            coords[3] = coords[1] + boxh
            return coords,boxw,boxh

    def OnPaint(self, event):
        """
        All that is needed here is to draw the buffer to screen
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
            The Buffer init is done here, to make sure the buffer is always
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
            self.UpdateHist()

        # re-render image on idle
        self.resize = True

    def OnIdle(self, event):
        """
            Only re-render a compsite map image from GRASS during
        idle time instead of multiple times during resizing.
            """

        if self.resize:
            self.render = True
            self.UpdateHist()
        event.Skip()

    def SaveToFile(self, FileName, FileType):
        """
        This will save the contents of the buffer
        to the specified file. See the wx.Windows docs for
        wx.Bitmap::SaveFile for the details
        """
        dc = wx.BufferedPaintDC(self, self._Buffer)
        self.pdc.DrawToDC(dc)
        self._Buffer.SaveFile(FileName, FileType)

    def GetImage(self):
        """
        Converts files to wx.Image
        """
        if self.Map.mapfile and os.path.isfile(self.Map.mapfile) and \
                os.path.getsize(self.Map.mapfile):
            img = wx.Image(self.Map.mapfile, wx.BITMAP_TYPE_ANY)
        else:
            img = None

        self.imagedict[img] = 99 # set image PeudoDC ID
        return img


    def UpdateHist(self, img=None):
        """
        This would get called if the drawing needed to change, for whatever reason.

        The idea here is that the drawing is based on some data generated
        elsewhere in the system. IF that data changes, the drawing needs to
        be updated.
        """

        Debug.msg (2, "BufferedWindow.UpdateHist(%s): render=%s" % (img, self.render))
        oldfont = ""
        oldencoding = ""

        if self.render:
            # render new map images

            # set default font and encoding environmental variables
            if "GRASS_FONT" in os.environ:
                oldfont = os.environ["GRASS_FONT"]
            os.environ["GRASS_FONT"] = self.parent.font
            if "GRASS_FT_ENCODING" in os.environ:
                oldencoding = os.environ["GRASS_FT_ENCODING"]
            if self.parent.encoding != None and self.parent.encoding != "ISO-8859-1":
                os.environ[GRASS_FT_ENCODING] = self.parent.encoding

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

        if self.textdict != None: # draw text overlays
            for id in self.textdict:
                self.Draw(self.pdc, img=self.textdict[id], drawid=id,
                          pdctype='text', coords=self.ovlcoords[id])

        self.resize = False

        # update statusbar
        #Debug.msg (3, "BufferedWindow.UpdateHist(%s): region=%s" % self.Map.region)
        self.Map.SetRegion()
        self.parent.statusbar.SetStatusText("Histogramming %s" % self.parent.mapname)

        # set default font and encoding environmental variables
        if oldfont != "":
            os.environ["GRASS_FONT"] = oldfont
        if oldencoding != "":
            os.environ["GRASS_FT_ENCODING"] = oldencoding

    def EraseMap(self):
        """
        Erase the map display
        """
        self.Draw(self.pdc, pdctype='clear')

class HistFrame(wx.Frame):
    """
    Main frame for hisgram display window.
    Uses d.histogram rendered onto canvas
    """

    def __init__(self, parent=None, id = wx.ID_ANY, title="Histogram of image or raster file",
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

        # Init variables
        self.params = {} # previously set histogram parameters

        self.font = "romans"
        self.fonttype = 'grassfont' # stroke or truetype font for default display font
        self.encoding = 'ISO-8859-1' # default encoding for display fonts

        #
        # Add statusbar
        #
        self.mapname = ''
        self.statusbar = self.CreateStatusBar(number=2, style=0)
        self.statusbar.SetStatusWidths([-2, -1])
        hist_frame_statusbar_fields = ["Histogramming %s" % self.mapname]
        for i in range(len(hist_frame_statusbar_fields)):
            self.statusbar.SetStatusText(hist_frame_statusbar_fields[i], i)

        #
        # Init map display
        #
        self.InitDisplay() # initialize region values

        # initialize buffered DC
        # self.HistWindow = DrawWindow(self)
        self.HistWindow = BufferedWindow(self, id = wx.ID_ANY, Map=self.Map) # initialize buffered DC
#        self.HistWindow.Bind(wx.EVT_MOTION, self.OnMotion)
#        self.HistWindow.SetCursor (self.cursors["default"]) # default

        #
        # Bind various events
        #
#        self.Bind(wx.EVT_ACTIVATE, self.OnFocus)
        self.Bind(wx.EVT_CLOSE,    self.OnCloseWindow)

        #
        # Init print module and classes
        #
        self.printopt = disp_print.PrintOptions(self, self.HistWindow)

    def __createToolBar(self):
        """Creates toolbar"""

        toolbar = self.CreateToolBar()
        for each in self.toolbarData():
            self.AddToolbarButton(toolbar, *each)
        toolbar.Realize()

    def AddToolbarButton(self, toolbar, label, icon, help, handler):
        """Adds button to the given toolbar"""

        if not label:
            toolbar.AddSeparator()
            return
        tool = toolbar.AddLabelTool(id=wx.ID_ANY, label=label, bitmap=icon, shortHelp=help)
        self.Bind(wx.EVT_TOOL, handler, tool)

    def toolbarData(self):

        return   (
                 ('histogram', Icons["histogram"].GetBitmap(), Icons["histogram"].GetLabel(), self.OnUpdate),
                 ('erase', Icons["erase"].GetBitmap(), Icons["erase"].GetLabel(), self.OnErase),
                 ('options',  wx.ArtProvider.GetBitmap(wx.ART_LIST_VIEW, wx.ART_TOOLBAR, (16,16)),  Icons["options"].GetLabel(), self.OnOptions),
                 ('font', Icons["font"].GetBitmap(), Icons["font"].GetLabel(), self.SetHistFont),
                 ('', '', '', ''),
                 ('save',  Icons["savefile"].GetBitmap(),  Icons["savefile"].GetLabel(),  self.SaveToFile),
                 ('print',  Icons["printmap"].GetBitmap(),  Icons["printmap"].GetLabel(),  self.PrintMenu),
                  )

    def InitDisplay(self):
        """
        Initialize histogram display, set dimensions and region
        """
        self.width, self.height = self.GetClientSize()
        self.Map.geom = self.width, self.height

    def OnUpdate(self, event):
        self.HistWindow.UpdateHist()

    def OnOptions(self, event):
        global gmpath
        completed = ''

        if self.Map.layers == []:
            self.Map.AddLayer(item="histlayer", type="command", name='', command="d.histogram",
                          l_active=False, l_hidden=False, l_opacity=1, l_render=False)

        menuform.GUI().parseCommand('d.histogram', gmpath,
                                    completed=(self.GetOptData,"hist",self.params),
                                    parentframe=None)


    def GetOptData(self, dcmd, layer, params):
        """
        Callback method for histogram command generated by
        dialog created in menuform.py
        """

        # Reset comand and rendering options in render.Map. Always render decoration.
        # Showing/hiding handled by PseudoDC

        for item in dcmd.split(' '):
            if 'map=' in item:
                self.mapname = item.split('=')[1]

        self.Map.changeLayer(item="histlayer", type="command", name='', command=dcmd,
                             l_active=True, l_hidden=False, l_opacity=1, l_render=False)
        self.params = params

    def SetHistFont(self, event):
        """
        Set font for histogram
        """

        dlg = defaultfont.SetDefaultFont(self, wx.ID_ANY, 'Select font for histogram text',
                                   pos=wx.DefaultPosition, size=wx.DefaultSize,
                                   style=wx.DEFAULT_DIALOG_STYLE,
                                   fonttype=self.fonttype, encoding=self.encoding)
        dlg.fontlb.SetStringSelection(self.font, True)
        if dlg.ShowModal() == wx.ID_CANCEL:
            dlg.Destroy()
            return

        # set default font type, font, and encoding to whatever selected in dialog
        if dlg.fonttype != None:
            self.fonttype = dlg.fonttype
        if dlg.font != None:
            self.font = dlg.font
        if dlg.encoding != None:
            self.encoding = dlg.encoding

        dlg.Destroy()

    def OnErase(self, event):
        """
        Erase the histogram display
        """
        self.Draw(self.pdc, pdctype='clear')

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
            self.HistWindow.SaveToFile(path, type)
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

        self.Map.Clean()

        self.Destroy()


