"""
MODULE:     wxgui_utils.py

CLASSES:
    * AbstractLayer
    * Layer
    * LayerTree
    * GMConsole

PURPOSE:    Utility classes for GRASS wxPython GUI. Main functions include tree control
            for GIS map layer management, command console, and command parsing.

AUTHORS:    The GRASS Development Team
            Michael Barton (Arizona State University)
            Jachym Cepicky (Mendel University of Agriculture)
            Martin Landa <landa.martin gmail.com>

COPYRIGHT:  (C) 2007 by the GRASS Development Team
            This program is free software under the GNU General Public
            License (>=v2). Read the file COPYING that comes with GRASS
            for details.

"""

import os,sys
import wx
import wx.lib.customtreectrl as CT
import wx.combo
import string

gmpath = os.path.join( os.getenv("GISBASE"),"etc","wx","gui_modules" )
sys.path.append(gmpath)

import select
import menuform
import mapdisp
import render
import gcmd
import grassenv
import histogram
from debug import Debug as Debug
from icon import Icons as Icons
try:
    import subprocess
except:
    from compat import subprocess

    def __del__(self):
        Debug.msg (3, "Layer.__del__(): type=%s" % \
                   self.type)

    def AddMapLayer (self, maplayer):
        """Add reference to MapLayer instance"""
        self.maplayer = maplayer

    def AddProperties (self, properties):
        """Add menuform properties"""
        self.properties = properties

class LayerTree(CT.CustomTreeCtrl):
    """
    Creates layer tree structure
    """
    #	def __init__(self, parent, id, pos, size, style):
    def __init__(self, parent,
                 id=wx.ID_ANY, pos=wx.DefaultPosition,
                 size=wx.DefaultSize, style=wx.SUNKEN_BORDER,
                 ctstyle=CT.TR_HAS_BUTTONS | CT.TR_HAS_VARIABLE_ROW_HEIGHT |
                 CT.TR_HIDE_ROOT | CT.TR_ROW_LINES | CT.TR_FULL_ROW_HIGHLIGHT|
                 CT.TR_EDIT_LABELS|CT.TR_MULTIPLE,
                 idx=None, gismgr=None, notebook=None, auimgr=None):
        CT.CustomTreeCtrl.__init__(self, parent, id, pos, size, style,ctstyle)

        self.SetAutoLayout(True)
        self.SetGradientStyle(1)
        self.EnableSelectionGradient(True)
        self.SetFirstGradientColour(wx.Colour(150, 150, 150))

        self.Map = render.Map()    # instance of render.Map to be associated with display
        self.root = None           # ID of layer tree root node
        self.groupnode = 0         # index value for layers
        self.optpage = {}          # dictionary of notebook option pages for each map layer
        self.layer_selected = None # ID of currently selected layer
        self.saveitem = {}         # dictionary to preserve layer attributes for drag and drop
        self.first = True          # indicates if a layer is just added or not
        self.drag = False          # flag to indicate a drag event is in process
        self.disp_idx = idx
        self.gismgr = gismgr
        self.notebook = notebook   # GIS Manager notebook for layer tree
        self.treepg = parent       # notebook page holding layer tree
        self.auimgr = auimgr       # aui manager

        # init associated map display
        self.mapdisplay = mapdisp.MapFrame(self,
                                           id=wx.ID_ANY, pos=wx.DefaultPosition, size=(640,480),
                                           style=wx.DEFAULT_FRAME_STYLE,
                                           tree=self, notebook=self.notebook,
                                           gismgr=self.gismgr, page=self.treepg,
                                           Map=self.Map, auimgr=self.auimgr)

        # title
        self.mapdisplay.SetTitle(_("GRASS GIS - Map Display: " + \
                                       str(self.disp_idx) + \
                                       " - Location: " + grassenv.GetGRASSVariable("LOCATION_NAME")))

        #show new display
        self.mapdisplay.Show()
        self.mapdisplay.Refresh()
        self.mapdisplay.Update()

        self.root = self.AddRoot("Map Layers")
        self.SetPyData(self.root, (None,None))

        #create image list to use with layer tree
        il = wx.ImageList(16, 16, False)

        trart = wx.ArtProvider.GetBitmap(wx.ART_FOLDER_OPEN, wx.ART_OTHER, (16,16))
        self.folder_open = il.Add(trart)
        trart = wx.ArtProvider.GetBitmap(wx.ART_FOLDER, wx.ART_OTHER, (16,16))
        self.folder = il.Add(trart)

        bmpsize = (16, 16)
        trgif = Icons["addrast"].GetBitmap(bmpsize)
        self.rast_icon = il.Add(trgif)

        trgif = Icons["addrgb"].GetBitmap(bmpsize)
        self.rgb_icon = il.Add(trgif)

        trgif = Icons["addhis"].GetBitmap(bmpsize)
        self.his_icon = il.Add(trgif)

        trgif = Icons["addshaded"].GetBitmap(bmpsize)
        self.shaded_icon = il.Add(trgif)

        trgif = Icons["addrarrow"].GetBitmap(bmpsize)
        self.rarrow_icon = il.Add(trgif)

        trgif = Icons["addrnum"].GetBitmap(bmpsize)
        self.rnum_icon = il.Add(trgif)

        trgif = Icons["addvect"].GetBitmap(bmpsize)
        self.vect_icon = il.Add(trgif)

        trgif = Icons["addthematic"].GetBitmap(bmpsize)
        self.theme_icon = il.Add(trgif)

        trgif = Icons["addchart"].GetBitmap(bmpsize)
        self.chart_icon = il.Add(trgif)

        trgif = Icons["addgrid"].GetBitmap(bmpsize)
        self.grid_icon = il.Add(trgif)

        trgif = Icons["addgeodesic"].GetBitmap(bmpsize)
        self.geodesic_icon = il.Add(trgif)

        trgif = Icons["addrhumb"].GetBitmap(bmpsize)
        self.rhumb_icon = il.Add(trgif)

        trgif = Icons["addlabels"].GetBitmap(bmpsize)
        self.labels_icon = il.Add(trgif)

        trgif = Icons["addcmd"].GetBitmap(bmpsize)
        self.cmd_icon = il.Add(trgif)

        checksize = il.GetSize(0)
        checkbmp = il.GetBitmap(0)
        self.AssignImageList(il)

        # use when groups implemented
        ## self.tree.SetItemImage(self.root, fldridx, wx.TreeItemIcon_Normal)
        ## self.tree.SetItemImage(self.root, fldropenidx, wx.TreeItemIcon_Expanded)

        self.Bind(wx.EVT_TREE_ITEM_EXPANDING,   self.OnExpandNode)
        self.Bind(wx.EVT_TREE_ITEM_COLLAPSED,   self.OnCollapseNode)
        self.Bind(wx.EVT_TREE_ITEM_ACTIVATED,   self.OnActivateLayer)
        self.Bind(wx.EVT_TREE_SEL_CHANGED,      self.OnChangeSel)
        self.Bind(CT.EVT_TREE_ITEM_CHECKED,     self.OnLayerChecked)
        self.Bind(wx.EVT_TREE_DELETE_ITEM,      self.OnDeleteLayer)
        self.Bind(wx.EVT_TREE_BEGIN_DRAG,       self.OnBeginDrag)
        self.Bind(wx.EVT_TREE_END_DRAG,         self.OnEndDrag)
        self.Bind(wx.EVT_TREE_ITEM_RIGHT_CLICK, self.OnContextMenu)
        self.Bind(wx.EVT_TREE_END_LABEL_EDIT,   self.OnChangeLayerName)
        # self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)

    def OnChangeLayerName (self, event):
        """Change layer name"""
        Debug.msg (3, "LayerTree.OnChangeLayerName: name=%s" % event.GetLabel())

    def OnContextMenu (self, event):
        """Context Layer Menu"""

        if not self.layer_selected:
            event.Skip()
            return

        ltype = self.GetPyData(self.layer_selected)[0]['type']

        Debug.msg (4, "LayerTree.OnContextMenu: layertype=%s" % \
                       ltype)

        ## pos = event.GetPosition()
        ## pos = self.ScreenToClient(pos)

        if not hasattr (self, "popupID1"):
            self.popupID1 = wx.NewId()
            self.popupID2 = wx.NewId()
            self.popupID3 = wx.NewId()
            self.popupID4 = wx.NewId()
            self.popupID5 = wx.NewId()
            self.popupID6 = wx.NewId()

        self.popupMenu = wx.Menu()
        # general item
        self.popupMenu.Append(self.popupID1, text=_("Remove"))
        self.Bind(wx.EVT_MENU, self.gismgr.DeleteLayer, id=self.popupID1)

        if ltype != "command": # rename
            self.popupMenu.Append(self.popupID2, text=_("Rename"))
            self.Bind(wx.EVT_MENU, self.RenameLayer, id=self.popupID2)

        # map layer items
        if ltype != "group" and \
                ltype != "command": # properties
            self.popupMenu.AppendSeparator()
            self.popupMenu.Append(self.popupID3, text=_("Properties"))
            self.Bind(wx.EVT_MENU, self.OnPopupProperties, id=self.popupID3)

        # specific items
        try:
            mltype = self.GetPyData(self.layer_selected)[0]['type']
        except:
            mltype = None
        # vector specific items
        if mltype and mltype == "vector":
            self.popupMenu.AppendSeparator()
            self.popupMenu.Append(self.popupID4, text=_("Show attribute table"))
            self.Bind (wx.EVT_MENU, self.gismgr.ShowAttributeTable, id=self.popupID4)

            self.popupMenu.Append(self.popupID5, text=_("Start editing"))
            self.popupMenu.Append(self.popupID6, text=_("Stop editing"))
            self.popupMenu.Enable(self.popupID6, False)
            self.Bind (wx.EVT_MENU, self.OnStartEditing, id=self.popupID5)
            self.Bind (wx.EVT_MENU, self.OnStopEditing,  id=self.popupID6)

            layer = self.GetPyData(self.layer_selected)[0]['maplayer']
            # enable editing only for vector map layers available in the current mapset
            digit = self.mapdisplay.digittoolbar
            if layer.GetMapset() != grassenv.GetGRASSVariable("MAPSET"):
                # only vector map in current mapset can be edited
                self.popupMenu.Enable (self.popupID5, False)
                self.popupMenu.Enable (self.popupID6, False)
            elif digit and digit.layerSelectedID != None:
                # vector map already edited
                if digit.layers[digit.layerSelectedID] == layer:
                    self.popupMenu.Enable (self.popupID5, False)
                    self.popupMenu.Enable(self.popupID6, True)
                    self.popupMenu.Enable(self.popupID1, False)
                else:
                    self.popupMenu.Enable(self.popupID5, False)
                    self.popupMenu.Enable(self.popupID6, False)

        # raster
        elif mltype and mltype == "raster":
            self.popupMenu.AppendSeparator()
            self.popupMenu.Append(self.popupID4, _("Histogram"))
            self.Bind (wx.EVT_MENU, self.OnHistogram, id=self.popupID4)

        ## self.PopupMenu(self.popupMenu, pos)
        self.PopupMenu(self.popupMenu)
        self.popupMenu.Destroy()

    def OnHistogram(self, event):
        """
        Plot histogram for given raster map layer
        """
        rastName = self.GetPyData(self.layer_selected)[0]['maplayer'].name

        if not hasattr (self, "histogramFrame"):
            self.histogramFrame = None

        if hasattr (self.mapdisplay, "histogram") and self.mapdisplay.histogram:
            self.histogramFrame = self.mapdisplay.histogram

        if not self.histogramFrame:
            self.histogramFrame = histogram.HistFrame(self,
                                                      id=wx.ID_ANY, pos=wx.DefaultPosition, size=(400,300),
                                                      style=wx.DEFAULT_FRAME_STYLE)
            # show new display
            self.histogramFrame.Show()

        self.histogramFrame.SetHistLayer(['d.histogram', 'map=%s' % rastName])
        self.histogramFrame.HistWindow.UpdateHist()
        self.histogramFrame.Refresh()
        self.histogramFrame.Update()

    def OnStartEditing (self, event):
        """
        Start editing vector map layer requested by the user
        """
        try:
            maplayer = self.GetPyData(self.layer_selected)[0]['maplayer']
        except:
            event.Skip()
            return

        if not self.mapdisplay.digittoolbar: # enable tool
            self.mapdisplay.AddToolbar("digit")
        else: # tool already enabled
            pass

        # mark layer as 'edited'
        self.mapdisplay.digittoolbar.StartEditing (maplayer)

    def OnStopEditing (self, event):
        """
        Stop editing the current vector map layer
        """
        try:
            maplayer = self.GetPyData(self.layer_selected)[0]['maplayer']
        except:
            event.Skip()
            return

        # mark layer as 'edited'
        self.mapdisplay.digittoolbar.StopEditing(maplayer)

        if self.mapdisplay.digittoolbar: # disable the tool
            self.mapdisplay.RemoveToolbar("digit")
        else: # tool already enabled
            pass

        self.mapdisplay.imgVectorMap = None

    def OnPopupProperties (self, event):
        """Popup properties dialog"""
        self.PropertiesDialog(self.layer_selected)

    def RenameLayer (self, event):
        """Rename layer"""
        self.EditLabel(self.layer_selected)

    def AddLayer(self, ltype, lname=None, lchecked=None, lopacity=None, lcmd=None, lgroup=None):
        """Add new item to the layer tree, create corresponding MapLayer instance.
        Launch property dialog if needed (raster, vector, etc.)

        Note: lcmd is given as a list
        """
        self.first = True
        checked    = False
        params = {} # no initial options parameters

        # deselect active item
        if self.layer_selected:
            self.SelectItem(self.layer_selected, select=False)

        Debug.msg (3, "LayerTree().AddLayer(): ltype=%s" % (ltype))
        
        if ltype == 'command':
            # generic command item
            ctrl = wx.TextCtrl(self, id=wx.ID_ANY, value='',
                               pos=wx.DefaultPosition, size=(250,25),
                               style=wx.TE_MULTILINE|wx.TE_WORDWRAP)
            checked = True
            ctrl.Bind(wx.EVT_TEXT_ENTER, self.OnCmdChanged)
            ctrl.Bind(wx.EVT_TEXT,       self.OnCmdChanged)
        elif ltype == 'group':
            # group item
            ctrl = None
            grouptext = 'Layer group:' + str(self.groupnode)
            self.groupnode += 1
            checked = True
        else:
            # all other items (raster, vector, ...)
            ctrl = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                               style=wx.SP_ARROW_KEYS, initial=100, min=0, max=100)
            
            self.Bind(wx.EVT_SPINCTRL, self.OnOpacity, ctrl)

        # add layer to the layer tree
        if self.layer_selected and self.layer_selected != self.GetRootItem():
            if self.GetPyData(self.layer_selected)[0]['type'] != 'group':
                if lgroup is False:
                    # last child of root
                    layer = self.AppendItem(parentId=self.root,
                                            text='', ct_type=1, wnd=ctrl)
                elif lgroup is None or lgroup is True:
                    # insert item on given position
                    parent = self.GetItemParent(self.layer_selected)
                    layer = self.InsertItem(parentId=parent, input=self.GetPrevSibling(self.layer_selected),
                                            text='', ct_type=1, wnd=ctrl)

            else: # group (first child of self.layer_selected)
                layer = self.PrependItem(parent=self.layer_selected,
                                         text='', ct_type=1, wnd=ctrl)
                self.Expand(self.layer_selected)
        else: # add first layer to the layer tree (first child of root)
            layer = self.PrependItem(parent=self.root, text='', ct_type=1, wnd=ctrl)

        # layer is initially unchecked as inactive (beside 'command')
        # use predefined value if given
        if lchecked:
            checked = lchecked
        self.CheckItem(layer, checked=checked)

        # select new item
        self.SelectItem(layer, select=True)
        self.layer_selected = layer

        # add text and icons for each layer ltype
        if ltype == 'raster':
            self.SetItemImage(layer, self.rast_icon)
            self.SetItemText(layer, 'raster (double click to set properties)')
        elif ltype == 'rgb':
            self.SetItemImage(layer, self.rgb_icon)
            self.SetItemText(layer, 'RGB (double click to set properties)')
        elif ltype == 'his':
            self.SetItemImage(layer, self.his_icon)
            self.SetItemText(layer, 'HIS (double click to set properties)')
        elif ltype == 'shaded':
            self.SetItemImage(layer, self.shaded_icon)
            self.SetItemText(layer, 'Shaded relief (double click to set properties)')
        elif ltype == 'rastnum':
            self.SetItemImage(layer, self.rnum_icon)
            self.SetItemText(layer, 'raster cell numbers (double click to set properties)')
        elif ltype == 'rastarrow':
            self.SetItemImage(layer, self.rarrow_icon)
            self.SetItemText(layer, 'raster flow arrows (double click to set properties)')
        elif ltype == 'vector':
            self.SetItemImage(layer, self.vect_icon)
            self.SetItemText(layer, 'vector (double click to set properties)')
        elif ltype == 'thememap':
            self.SetItemImage(layer, self.theme_icon)
            self.SetItemText(layer, 'thematic map (double click to set properties)')
        elif ltype == 'themechart':
            self.SetItemImage(layer, self.chart_icon)
            self.SetItemText(layer, 'thematic charts (double click to set properties)')
        elif ltype == 'grid':
            self.SetItemImage(layer, self.grid_icon)
            self.SetItemText(layer, 'grid (double click to set properties)')
        elif ltype == 'geodesic':
            self.SetItemImage(layer, self.geodesic_icon)
            self.SetItemText(layer, 'geodesic line (double click to set properties)')
        elif ltype == 'rhumb':
            self.SetItemImage(layer, self.rhumb_icon)
            self.SetItemText(layer, 'rhumbline (double click to set properties)')
        elif ltype == 'labels':
            self.SetItemImage(layer, self.labels_icon)
            self.SetItemText(layer, 'vector labels (double click to set properties)')
        elif ltype == 'command':
            self.SetItemImage(layer, self.cmd_icon)
        elif ltype == 'group':
            self.SetItemImage(layer, self.folder)
            self.SetItemText(layer, grouptext)

        self.first = False

        if ltype != 'group':
            if lopacity:
                opacity = lopacity
            else:
                opacity = 1.0
            if lcmd and len(lcmd) > 1:
                cmd = lcmd
                render = True
                name = self.GetLayerNameFromCmd(lcmd)
            else:
                cmd = []
                render = False
                name = None

            # add a data object to hold the layer's command (does not apply to generic command layers)
            self.SetPyData(layer, ({'cmd': cmd,
                                    'type' : ltype,
                                    'ctrl' : ctrl,
                                    'maplayer' : None,
                                    'prowin' : None}, 
                                   None))

            maplayer = self.Map.AddLayer(type=ltype, command=self.GetPyData(layer)[0]['cmd'], name=name,
                                         l_active=checked, l_hidden=False,
                                         l_opacity=opacity, l_render=render)
            self.GetPyData(layer)[0]['maplayer'] = maplayer

            # run properties dialog if no properties given
            if len(cmd) > 1:
                self.PropertiesDialog(layer, show=False)
            else:
                self.PropertiesDialog(layer, show=True)

        else: # group
            self.SetPyData(layer, ({'cmd': None,
                                    'type' : ltype,
                                    'ctrl' : None,
                                    'maplayer' : None,
                                    'prowin' : None}, 
                                   None))

        # use predefined layer name if given
        if lname:
            if ltype != 'command':
                self.SetItemText(layer, lname)
            else:
                ctrl.SetValue(lname)

        return layer

    def PropertiesDialog (self, layer, show=True):
        """Launch the properties dialog"""
        global gmpath
        completed = ''
        params = self.GetPyData(layer)[1]
        ltype  = self.GetPyData(layer)[0]['type']

        Debug.msg (3, "LayerTree.PropertiesDialog(): ltype=%s" % \
                   ltype)

        if self.GetPyData(layer)[0]['cmd']:
            cmdValidated = menuform.GUI().ParseCommand(self.GetPyData(layer)[0]['cmd'],
                                                       completed=(self.GetOptData,layer,params),
                                                       parentframe=self, show=show)
            self.GetPyData(layer)[0]['cmd'] = cmdValidated
        elif ltype == 'raster':
            menuform.GUI().ParseCommand(['d.rast'], completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'rgb':
            menuform.GUI().ParseCommand(['d.rgb'], completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'his':
            menuform.GUI().ParseCommand(['d.his'], completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'shaded':
            menuform.GUI().ParseCommand(['d.shadedmap'], completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'rastarrow':
            menuform.GUI().ParseCommand(['d.rast.arrow'], completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'rastnum':
            menuform.GUI().ParseCommand(['d.rast.num'], completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'vector':
            menuform.GUI().ParseCommand(['d.vect'], completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'thememap':
            menuform.GUI().ParseCommand(['d.vect.thematic'],
                                        completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'themechart':
            menuform.GUI().ParseCommand(['d.vect.chart'],
                                        completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'grid':
            menuform.GUI().ParseCommand(['d.grid'], completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'geodesic':
            menuform.GUI().ParseCommand(['d.geodesic'], completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'rhumb':
            menuform.GUI().ParseCommand(['d.rhumbline'], completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'labels':
            menuform.GUI().ParseCommand(['d.labels'], completed=(self.GetOptData,layer,params),
                                        parentframe=self)
        elif ltype == 'cmdlayer':
            pass
        elif ltype == 'group':
            pass

    def OnActivateLayer(self, event):
        """Click on the layer item.
        Launch property dialog, or expand/collapse group of items, etc."""
        
        layer = event.GetItem()
        self.layer_selected = layer

        self.PropertiesDialog (layer)

        if self.GetPyData(layer)[0]['type'] == 'group':
            if self.IsExpanded(layer):
                self.Collapse(layer)
            else:
                self.Expand(layer)

    def OnDeleteLayer(self, event):
        """Remove selected layer item from the layer tree"""

        item = event.GetItem()

        try:
            item.properties.Close(True)
        except:
            pass

        if item != self.root:
            Debug.msg (3, "LayerTree.OnDeleteLayer(): name=%s" % \
                           (self.GetItemText(item)))
        else:
            self.root = None

        # unselect item
        self.Unselect()
        self.layer_selected = None

        try:
            if self.GetPyData(item)[0]['type'] != 'group':
                self.Map.DeleteLayer( self.GetPyData(item)[0]['maplayer'])
        except:
            pass

        # redraw map if auto-rendering is enabled
        if self.mapdisplay.autoRender.GetValue(): 
            self.mapdisplay.ReRender(None)

        if self.mapdisplay.digittoolbar:
            self.mapdisplay.digittoolbar.UpdateListOfLayers (updateTool=True)

        event.Skip()

    def OnLayerChecked(self, event):
        """Enable/disable given layer item"""
        item    = event.GetItem()
        checked = item.IsChecked()
        
        if self.drag == False and self.first == False:
            # change active parameter for item in layers list in render.Map
            if self.GetPyData(item)[0]['type'] == 'group':
                childitem = self.GetFirstChild(item)
                child = childitem[0]
                cookie = childitem[1]
                for n in range(0, self.GetChildrenCount(item)):
                    if checked == False:
                        childchecked = False
                    else:
                        childchecked = child.IsChecked()
                        self.Map.ChangeLayerActive(self.GetPyData(child)[0]['maplayer'], childchecked)
                    child = self.GetNextChild(item, cookie)[0]
            else:
                self.Map.ChangeLayerActive(self.GetPyData(item)[0]['maplayer'], checked)

        # redraw map if auto-rendering is enabled
        if self.mapdisplay.autoRender.GetValue(): 
            self.mapdisplay.ReRender(None)

    def OnCmdChanged(self, event):
        """Change command string"""
        ctrl = event.GetEventObject()
        cmd = event.GetString()
        layer = None

        layer = self.GetFirstVisibleItem()

        while layer and layer.IsOk():
            if self.GetPyData(layer)[0]['ctrl'] == ctrl:
                break
            
            layer = self.GetNextVisible(layer)

        # change parameters for item in layers list in render.Map
        if layer and self.drag == False:
            self.ChangeLayer(layer)
            self.GetPyData(layer)[0]['cmd'] = cmd.split(' ')
            maplayer = self.GetPyData(layer)[0]['maplayer']
            for option in maplayer.GetCmd():
                if 'map=' in option:
                    mapname = option.split('=')[1]
                    self.Map.ChangeLayerName(maplayer, mapname)

        event.Skip()

    def OnOpacity(self, event):
        """
        Set opacity level for map layer
        """
        Debug.msg (3, "LayerTree.OnOpacity(): %s" % event.GetInt())

        ctrl = event.GetEventObject()
        maplayer = None

        vislayer = self.GetFirstVisibleItem()

        layer = None
        for item in range(0, self.GetCount()):
            if self.GetPyData(vislayer)[0]['ctrl'] == ctrl:
                layer = vislayer

            if not self.GetNextVisible(vislayer):
                break
            else:
                vislayer = self.GetNextVisible(vislayer)

        if layer:
            maplayer = self.GetPyData(layer)[0]['maplayer']

        opacity = event.GetInt() / 100.
        # change opacity parameter for item in layers list in render.Map
        if maplayer and self.drag == False:
            self.Map.ChangeOpacity(maplayer, opacity)

        # redraw map if auto-rendering is enabled
        if self.mapdisplay.autoRender.GetValue(): 
            self.mapdisplay.ReRender(None)

    def OnChangeSel(self, event):
        oldlayer = event.GetOldItem()
        layer = event.GetItem()
        self.layer_selected = layer
        try:
            self.RefreshLine(oldlayer)
            self.RefreshLine(layer)
        except:
            pass

    def OnCollapseNode(self, event):
        """
        Collapse node
        """
        if self.GetPyData(self.layer_selected)[0]['type'] == 'group':
            self.SetItemImage(self.layer_selected, self.folder)

    def OnExpandNode(self, event):
        """
        Expand node
        """
        self.layer_selected = event.GetItem()
        if self.GetPyData(self.layer_selected)[0]['type'] == 'group':
            self.SetItemImage(self.layer_selected, self.folder_open)

    def OnBeginDrag(self, event):
        """
        Drag and drop of tree nodes
        """

        item  = event.GetItem()
        Debug.msg (3, "LayerTree.OnBeginDrag(): layer=%s" % \
                   (self.GetItemText(item)))

        event.Allow()
        self.drag = True
        self.DoSelectItem(item, unselect_others=True)

        # save everthing associated with item to drag
        self.dragItem = item

    def RecreateItem (self, event, oldItem, parent=None):
        """
        Recreate item (needed for OnEndDrag())
        """
        Debug.msg (4, "LayerTree.RecreateItem(): layer=%s" % \
                   self.GetItemText(oldItem))

        # recreate spin/text control for layer
        if self.GetPyData(oldItem)[0]['type'] == 'command':
            newctrl = wx.TextCtrl(self, id=wx.ID_ANY, value='',
                                  pos=wx.DefaultPosition, size=(250,25),
                                  style=wx.TE_MULTILINE|wx.TE_WORDWRAP)
            try:
                newctrl.SetValue(self.GetPyData(oldItem)[0]['maplayer'].GetCmd(string=True))
            except:
                pass
            newctrl.Bind(wx.EVT_TEXT_ENTER, self.OnCmdChanged)
            newctrl.Bind(wx.EVT_TEXT,       self.OnCmdChanged)
        elif self.GetPyData(oldItem)[0]['type'] == 'group':
            newctrl = None
        else:
            newctrl = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                                  style=wx.SP_ARROW_KEYS, min=0, max=100)
            try:
                newctrl.SetValue(self.GetPyData(oldItem)[0]['maplayer'].GetOpacity())
            except:
                newctrl.SetValue(100)
            self.Bind(wx.EVT_SPINCTRL, self.OnOpacity, newctrl)

        # decide where to put new layer and put it there
        if not parent:
            flag = self.HitTest(event.GetPoint())[1]
        else:
            flag = 0

        # fetch data
        text    = self.GetItemText(oldItem)
        image   = self.GetItemImage(oldItem, 0)
        wind    = self.GetItemWindow(oldItem)
        checked = self.IsItemChecked(oldItem)
        if self.GetPyData(oldItem)[0]['type'] == 'group':
            windval = None
            data    = None
        else:
            windval = self.GetItemWindow(oldItem).GetValue()
            data    = self.GetPyData(oldItem)

        # create GenericTreeItem instance
        if flag & wx.TREE_HITTEST_ABOVE:
            newItem = self.PrependItem(self.root, text=text, \
                                   ct_type=1, wnd=newctrl, image=image, \
                                   data=data)
        elif (flag &  wx.TREE_HITTEST_BELOW) or (flag & wx.TREE_HITTEST_NOWHERE) \
                 or (flag & wx.TREE_HITTEST_TOLEFT) or (flag & wx.TREE_HITTEST_TORIGHT):
            newItem = self.AppendItem(self.root, text=text, \
                                  ct_type=1, wnd=newctrl, image=image, \
                                  data=data)
        else:
            if parent:
                afteritem = parent
            else:
                afteritem = event.GetItem()

            if  self.GetPyData(afteritem)[0]['type'] == 'group':
                parent = afteritem
                newItem = self.AppendItem(parent, text=text, \
                                      ct_type=1, wnd=newctrl, image=image, \
                                      data=data)
                self.Expand(afteritem)
            else:
                parent = self.GetItemParent(afteritem)
                newItem = self.InsertItem(parent, afteritem, text=text, \
                                      ct_type=1, wnd=newctrl, image=image, \
                                      data=data)

        # add layer at new position
        self.SetPyData(newItem, self.GetPyData(oldItem))
        self.GetPyData(newItem)[0]['ctrl'] = newctrl

        self.CheckItem(newItem, checked=checked)

        event.Skip()

        return newItem

    def OnEndDrag(self, event):
        """
        Insert copy of layer in new
        position and delete original at old position
        """

        self.drag = True
        try:
            old = self.dragItem  # make sure this member exists
        except:
            return

        Debug.msg (4, "LayerTree.OnEndDrag(): layer=%s" % \
                   (self.GetItemText(self.dragItem)))

        newItem  = self.RecreateItem (event, self.dragItem)

        if  self.GetPyData(newItem)[0]['type'] == 'group':
            (child, cookei) = self.GetFirstChild(self.dragItem)
            if child:
                while child:
                    self.RecreateItem(event, child, parent=newItem)
                    self.Delete(child)
                    child = self.GetNextChild(old, cookei)[0]

            self.Expand(newItem)

        # delete layer at original position
        self.Delete(old) # entry in render.Map layers list automatically deleted by OnDeleteLayer handler

        # reorder layers in render.Map to match new order after drag and drop
        self.ReorderLayers()

        # select new item
        self.SelectItem(newItem)

        # completed drag and drop
        self.drag = False

    def GetLayerNameFromCmd(self, dcmd):
        """Get layer name from GRASS command"""
        mapname = ''
        for item in dcmd:
            if 'map=' in item:
                mapname = item.split('=')[1]
            elif 'red=' in item:
                mapname = item.split('=')[1]
            elif 'h_map=' in item:
                mapname = item.split('=')[1]
            elif 'reliefmap' in item:
                mapname = item.split('=')[1]
            elif 'd.grid' in item:
                mapname = 'grid'
            elif 'd.geodesic' in item:
                mapname = 'geodesic'
            elif 'd.rhumbline' in item:
                mapname = 'rhumb'
            elif 'labels=' in item:
                mapname = item.split('=')[1]+' labels'
        
            if mapname != '':
                break

        return mapname

    def GetOptData(self, dcmd, layer, params, propwin):
        """Process layer data"""

        # set layer text to map name
        mapname = self.GetLayerNameFromCmd(dcmd)
        self.SetItemText(layer, mapname)

        # update layer data
        self.SetPyData(layer, (self.GetPyData(layer)[0], params))
        self.GetPyData(layer)[0]['cmd'] = dcmd
        self.GetPyData(layer)[0]['propwin'] = propwin

        # check layer as active
        self.CheckItem(layer, checked=True)

        # change parameters for item in layers list in render.Map
        self.ChangeLayer(layer)

    def ReorderLayers(self):
        """Add commands from data associated with
        any valid layers (checked or not) to layer list in order to
        match layers in layer tree."""

        # make a list of visible layers
        treelayers = []

        vislayer = self.GetFirstVisibleItem()

        if not vislayer:
            return

        itemList = ""

        for item in range(0, self.GetCount()):
            itemList += self.GetItemText(vislayer) + ','
            if self.GetPyData(vislayer)[0]['type'] != 'group':
                treelayers.append(self.GetPyData(vislayer)[0]['maplayer'])

            if not self.GetNextVisible(vislayer):
                break
            else:
                vislayer = self.GetNextVisible(vislayer)

        Debug.msg (4, "LayerTree.ReoderLayers(): items=%s" % \
                   (itemList))

        # reorder map layers
        treelayers.reverse()
        self.Map.ReorderLayers(treelayers)

    def ChangeLayer(self, item):
        """Change layer"""

        type = self.GetPyData(item)[0]['type']

        if type == 'command':
            if self.GetItemWindow(item).GetValue() != None:
                cmdlist = self.GetItemWindow(item).GetValue().split(' ')
                opac = 1.0
                chk = self.IsItemChecked(item)
                hidden = not self.IsVisible(item)
        elif type != 'group':
            if self.GetPyData(item)[0] is not None:
                cmdlist = self.GetPyData(item)[0]['cmd']
                opac = float(self.GetItemWindow(item).GetValue())/100
                chk = self.IsItemChecked(item)
                hidden = not self.IsVisible(item)
        maplayer = self.Map.ChangeLayer(layer=self.GetPyData(item)[0]['maplayer'], type=type,
                                        command=cmdlist, name=self.GetItemText(item),
                                        l_active=chk, l_hidden=hidden, l_opacity=opac, l_render=False)

        self.GetPyData(item)[0]['maplayer'] = maplayer

        # if digitization tool enabled -> update list of available vector map layers
        if self.mapdisplay.digittoolbar:
            self.mapdisplay.digittoolbar.UpdateListOfLayers(updateTool=True)

        # redraw map if auto-rendering is enabled
        if self.mapdisplay.autoRender.GetValue(): 
            self.mapdisplay.ReRender(None)

    def setNotebookPage(self,pg):
        self.parent.notebook.SetSelection(pg)

    def OnCloseWindow(self, event):
        pass
    # self.Map.Clean()

class GMConsole(wx.Panel):
    """
    Create and manage output console for commands entered on the
    GIS Manager command line.
    """
    def __init__(self, parent, id=wx.ID_ANY,
                 pos=wx.DefaultPosition, size=wx.DefaultSize,
                 style=wx.TAB_TRAVERSAL|wx.FULL_REPAINT_ON_RESIZE):
        wx.Panel.__init__(self, parent, id, pos, size, style)

        # initialize variables
        self.Map             = None
        self.parent          = parent # GMFrame
        self.cmd_output      = ""
        self.console_command = ""
        self.console_clear   = ""
        self.console_save    = ""
        self.gcmdlst         = [] # list of commands in bin and scripts

        # text control for command output
        self.cmd_output = wx.TextCtrl(parent=self, id=wx.ID_ANY, value="",
                                      style=wx.TE_MULTILINE| wx.TE_READONLY)
        self.cmd_output.SetFont(wx.Font(10, wx.FONTFAMILY_MODERN, wx.NORMAL, wx.NORMAL, 0, ''))

        # buttons
        self.console_clear = wx.Button(parent=self, id=wx.ID_CLEAR)
        self.console_save  = wx.Button(parent=self, id=wx.ID_SAVE)
        self.Bind(wx.EVT_BUTTON, self.ClearHistory, self.console_clear)
        self.Bind(wx.EVT_BUTTON, self.SaveHistory, self.console_save)

        # progress bar
        self.console_progressbar = wx.Gauge(parent=self, id=wx.ID_ANY,
                                            range=100, pos=(110, 50), size=(-1, 25))

        # output control layout
        boxsizer1 = wx.BoxSizer(wx.VERTICAL)
        gridsizer1 = wx.GridSizer(rows=1, cols=2, vgap=0, hgap=0)
        boxsizer1.Add(item=self.cmd_output, proportion=1,
                      flag=wx.EXPAND | wx.ADJUST_MINSIZE, border=0)
        gridsizer1.Add(item=self.console_clear, proportion=0,
                       flag=wx.ALIGN_CENTER_HORIZONTAL | wx.ADJUST_MINSIZE, border=0)
        gridsizer1.Add(item=self.console_save, proportion=0,
                       flag=wx.ALIGN_CENTER_HORIZONTAL | wx.ADJUST_MINSIZE, border=0)


        boxsizer1.Add(item=gridsizer1, proportion=0,
                      flag=wx.EXPAND | wx.ALIGN_CENTRE_VERTICAL | wx.TOP | wx.BOTTOM,
                      border=5)
        boxsizer1.Add(item=self.console_progressbar, proportion=0,
                      flag=wx.EXPAND | wx.ADJUST_MINSIZE, border=0)

        boxsizer1.Fit(self)
        boxsizer1.SetSizeHints(self)

        self.SetAutoLayout(True)
        self.SetSizer(boxsizer1)

    def GetGRASSCmds(self):
        """
        Create list of all available GRASS commands to use when
        parsing string from the command line
        """
        self.gcmdlst = []
        gisbase = os.environ['GISBASE']
        self.gcmdlst = os.listdir(os.path.join(gisbase,'bin'))
        self.gcmdlst = self.gcmdlst + os.listdir(os.path.join(gisbase,'scripts'))
        #self.gcmdlst = self.gcmdlst + os.listdir(os.path.join(gisbase,'etc','gm','script'))

        return self.gcmdlst

    def RunCmd(self, command):
        """
        Run in GUI or shell GRASS (or other) commands typed into
        console command text widget, and send stdout output to output
        text widget.

        Command is transformed into a list for processing.

        TODO: Display commands (*.d) are captured and
        processed separately by mapdisp.py. Display commands are
        rendered in map display widget that currently has
        the focus (as indicted by mdidx).
        """

        # create list of available GRASS commands
        gcmdlst = self.GetGRASSCmds()

        # map display window available ?
        try:
            curr_disp = self.parent.curr_page.maptree.mapdisplay
            self.Map = curr_disp.GetRender()
        except:
            curr_disp = None

        try:
            # if command is not already a list, make it one
            cmdlist = command.strip().split(' ')
        except:
            cmdlist = command

        if cmdlist[0] in gcmdlst:
            # send GRASS command without arguments to GUI command interface
            # except display commands (they are handled differently)
            if cmdlist[0][0:2] == "d.":
                try:
                    layertype = {'d.rast'         : 'raster',
                                 'd.rgb'          : 'rgb',
                                 'd.his'          : 'his',
                                 'd.shaded'       : 'shaded',
                                 'd.legend'       : 'rastleg',
                                 'd.rast.arrow'   : 'rastarrow',
                                 'd.rast.num'     : 'rastnum',
                                 'd.vect'         : 'vector',
                                 'd.vect.thematic': 'thememap',
                                 'd.vect.chart'   : 'themechart',
                                 'd.grid'         : 'grid',
                                 'd.geodesic'     : 'geodesic',
                                 'd.rhumbline'    : 'rhumb',
                                 'd.labels'       : 'labels'}[cmdlist[0]]
                except KeyError:
                    print _('Command type not yet implemented')
                    return False

                # add layer
                self.parent.curr_page.maptree.AddLayer(ltype=layertype,
                                                       lcmd=cmdlist)

            else:
                if len(cmdlist) > 1:
                    menuform.GUI().ParseCommand(cmdlist, parentframe=self, show=False)
                else:
                    menuform.GUI().ParseCommand(cmdlist, parentframe=self, show=True)

        else:
            # Send any other command to the shell. Send output to
            # console output window.

            if self.parent.notebook.GetSelection() != 1:
                # select 'Command output' tab
                self.parent.notebook.SetSelection(1)

            self.cmd_output.write("$ " + ' '.join(cmdlist) + "\n")
            
            if cmdlist[0] not in gcmdlst:
                # if command is not a GRASS command, treat it like a shell command
                generalCmd = subprocess.Popen(cmdlist,
                                          stdout=subprocess.PIPE,
                                          stderr=subprocess.PIPE,
                                          close_fds=True)
                
                for outline in runCmd.stdout:
                    self.cmd_output.write(outline)
            else:
                # activate compuational region (set with g.region) for all non-display commands.
                tmpreg = os.getenv("GRASS_REGION")
                os.unsetenv("GRASS_REGION")

                # process GRASS command with argument
                grassCmd = gcmd.Command(cmdlist, verbose=3)

                
                # deactivate computational region and return to display settings
                if tmpreg:
                    os.environ["GRASS_REGION"] = tmpreg

                if grassCmd.returncode != 0:
                    return False

                # if oline.find("GRASS_INFO_PERCENT")>-1:
                #    self.console_progressbar.SetValue(int(oline.split()[1]))

                for line in grassCmd.ReadStdOutput():
                    self.cmd_output.write(line + '\n')
                    
        return True

    def ClearHistory(self, event):
        """Clear history of commands"""
        self.cmd_output.Clear()
        self.console_progressbar.SetValue(0)

    def SaveHistory(self, event):
        """Save history of commands"""
        self.history = self.cmd_output.GetStringSelection()
        if self.history == "":
            self.cmd_output.SetSelection(-1,-1)
            self.history = self.cmd_output.GetStringSelection()

        #Use a standard dialog for this
        wildcard = "Text file (*.txt)|*.txt"
        dlg = wx.FileDialog(
            self, message=_("Save file as ..."), defaultDir=os.getcwd(),
            defaultFile="grass_cmd_history.txt", wildcard=wildcard,
            style=wx.SAVE|wx.FD_OVERWRITE_PROMPT)

        # Show the dialog and retrieve the user response. If it is the OK response,
        # process the data.
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()

            output = open(path,"w")
            output.write(self.history)
            output.close()

        dlg.Destroy()
