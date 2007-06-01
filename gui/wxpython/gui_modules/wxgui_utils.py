import os,sys
import wx
import wx.lib.customtreectrl as CT
import wx.combo
import string

gmpath = os.path.join( os.getenv("GISBASE"),"etc","wx","gui_modules" )
sys.path.append(gmpath)
gmpath = os.path.join( os.getenv("GISBASE"),"etc","wx","icons")
sys.path.append(gmpath)

import select
import menuform
import mapdisp
import render
import cmd
import grassenv
import histogram
from debug import Debug as Debug
from icon import Icons as Icons
try:
    import subprocess
except:
    from compat import subprocess

class AbstractLayer:
    """
    Abstract layer in LayerTree
    
    Attributes:
    * type - layer type ('cmdlayer', 'group', etc) -- see LayerTree.AddLayer() 
    """

    def __init__(self, type):
        self.type = type
        
class Layer(AbstractLayer):
    """
    This class represents general item in LayerTree

    Attributes:
    * maplayer   - reference to MapLayer instance
    * properties - menuform properties (needed for PropertiesDialog)
    """
    def __init__ (self, type, wxCtrl=None):
        AbstractLayer.__init__(self, type)

        Debug.msg (3, "Layer.__init__(): type=%s" % \
                   type)

        self.wxCtrl = wxCtrl

        # reference to MapLayer instance
        self.maplayer = None

        # properties
        self.properties = None

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
                 CT.TR_EDIT_LABELS|CT.TR_MULTIPLE, idx=None, gismgr=None, notebook=None):
        CT.CustomTreeCtrl.__init__(self, parent, id, pos, size, style,ctstyle)

        self.SetAutoLayout(True)
        self.SetGradientStyle(1)
        self.EnableSelectionGradient(True)
        self.SetFirstGradientColour(wx.Colour(150, 150, 150))

        self.Map = render.Map()    # instance of render.Map to be associated with display
        self.root = None             # ID of layer tree root node
        self.groupnode = 0         # index value for layers
        self.optpage = {}          # dictionary of notebook option pages for each map layer
        self.layer_selected = None # ID of currently selected layer
        self.layers = {}           # dictionary of layers (see Layer class)
        self.saveitem = {}         # dictionary to preserve layer attributes for drag and drop
        self.first = True          # indicates if a layer is just added or not
        self.drag = False          # flag to indicate a drag event is in process
        self.disp_idx = idx
        self.gismgr = gismgr
        self.notebook = notebook   # GIS Manager notebook for layer tree
        self.treepg = parent       # notebook page holding layer tree


        # init associated map display
        self.mapdisplay = mapdisp.MapFrame(self,
                                           id=wx.ID_ANY, pos=wx.DefaultPosition, size=(640,480),
                                           style=wx.DEFAULT_FRAME_STYLE,
                                           tree=self, notebook=self.notebook, gismgr=self.gismgr, page=self.treepg,
                                           Map=self.Map)

        # title
        self.mapdisplay.SetTitle(_("GRASS GIS - Map Display: " + str(self.disp_idx) + " - Location: " + grassenv.env["LOCATION_NAME"]))
        #self.maptree[self.disp_idx] = self.mapdisplays[self.disp_idx].getTree()

        #show new display
        self.mapdisplay.Show()
        self.mapdisplay.Refresh()
        self.mapdisplay.Update()

        self.Map = self.mapdisplay.getRender()

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

        trgif = Icons["addrarrow"].GetBitmap(bmpsize)
        self.rarrow_icon = il.Add(trgif)

        trgif = Icons["addrnum"].GetBitmap(bmpsize)
        self.rnum_icon = il.Add(trgif)

        trgif = Icons["elvect"].GetBitmap(bmpsize)
        self.vect_icon = il.Add(trgif)

        trgif = Icons["addthematic"].GetBitmap(bmpsize)
        self.theme_icon = il.Add(trgif)

        trgif = Icons["addchart"].GetBitmap(bmpsize)
        self.chart_icon = il.Add(trgif)

        trgif = Icons["addgrid"].GetBitmap(bmpsize)
        self.grid_icon = il.Add(trgif)

        trgif = Icons["addlabels"].GetBitmap(bmpsize)
        self.labels_icon = il.Add(trgif)

        trgif = Icons["addcmd"].GetBitmap(bmpsize)
        self.cmd_icon = il.Add(trgif)

        checksize = il.GetSize(0)
        checkbmp = il.GetBitmap(0)
        self.AssignImageList(il)

        # use when groups implemented
        # self.tree.SetItemImage(self.root, fldridx, wx.TreeItemIcon_Normal)
        # self.tree.SetItemImage(self.root, fldropenidx, wx.TreeItemIcon_Expanded)

        self.Bind(wx.EVT_TREE_ITEM_EXPANDING,   self.OnExpandNode)
        self.Bind(wx.EVT_TREE_ITEM_COLLAPSED,   self.OnCollapseNode)
        self.Bind(wx.EVT_TREE_ITEM_ACTIVATED,   self.OnActivateLayer)
        self.Bind(wx.EVT_TREE_SEL_CHANGED,      self.OnChangeSel)
        self.Bind(CT.EVT_TREE_ITEM_CHECKED,     self.OnLayerChecked)
        self.Bind(wx.EVT_TREE_DELETE_ITEM,      self.OnDeleteLayer)
        self.Bind(wx.EVT_TREE_BEGIN_DRAG,       self.OnBeginDrag)
        self.Bind(wx.EVT_TREE_END_DRAG,         self.OnEndDrag)
        self.Bind(wx.EVT_CONTEXT_MENU,          self.OnContextMenu)
        self.Bind(wx.EVT_TREE_END_LABEL_EDIT,   self.OnChangeLayerName)
        # self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)

    def OnChangeLayerName (self, event):
        """Change layer name"""
        Debug.msg (3, "LayerTree.OnChangeLayerName: name=%s" % event.GetLabel())

        self.Map.ChangeLayerName (self.layer_selected, event.GetLabel())

    def OnContextMenu (self, event):
        """Context Layer Menu"""

        if not self.layer_selected:
            event.Skip()
            return

        pos = event.GetPosition()
        pos = self.ScreenToClient(pos)
        
        ltype = self.layers[self.layer_selected].type

        if not hasattr (self, "popupID1"):
            self.popupID1 = wx.NewId()
            self.popupID2 = wx.NewId()
            self.popupID3 = wx.NewId()
            self.popupID4 = wx.NewId()
            self.popupID5 = wx.NewId()
            self.popupID6 = wx.NewId()

            self.Bind (wx.EVT_MENU, self.gismgr.DeleteLayer,        id=self.popupID1)
            self.Bind (wx.EVT_MENU, self.RenameLayer,               id=self.popupID2)
            self.Bind (wx.EVT_MENU, self.OnPopupProperties,         id=self.popupID3)

        self.popupMenu = wx.Menu()
        # general item
        self.popupMenu.Append (self.popupID1, _("Delete"))
        self.popupMenu.Append (self.popupID2, _("Rename"))

        # map layer items
        if ltype != "group": # properties
            self.popupMenu.AppendSeparator()
            self.popupMenu.Append(self.popupID3, text=_("Properties"))

        # specific items
        try:
            mltype = self.layers[self.layer_selected].maplayer.type
        except:
            return
        # vector specific items
        if mltype == "vector": 
            self.popupMenu.AppendSeparator()
            self.popupMenu.Append(self.popupID4, _("Show attribute table"))
            self.popupMenu.Append(self.popupID5, _("Start editing"))
            layer = self.layers[self.layer_selected].maplayer
            # enable editing only for vector map layers available in the current mapset
            if layer.GetMapset() != grassenv.env["MAPSET"]:
                self.popupMenu.Enable (self.popupID5, False)
            self.popupMenu.Append(self.popupID6, _("Stop editing"))
            self.popupMenu.Enable(self.popupID6, False)

            self.Bind (wx.EVT_MENU, self.gismgr.ShowAttributeTable, id=self.popupID4)
            self.Bind (wx.EVT_MENU, self.OnStartEditing,            id=self.popupID5)
            self.Bind (wx.EVT_MENU, self.OnStopEditing,             id=self.popupID6)

        # raster
        elif mltype == "raster":
            self.popupMenu.AppendSeparator()
            self.popupMenu.Append(self.popupID4, _("Histogram"))
            self.Bind (wx.EVT_MENU, self.OnHistogram, id=self.popupID4)

        self.PopupMenu(self.popupMenu, pos)
        self.popupMenu.Destroy()

    def OnHistogram(self, event):
        """
        Plot histogram for given raster map layer
        """
        rastName = self.layers[self.layer_selected].maplayer.name

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
            maplayer = self.layers[self.layer_selected].maplayer
        except:
            event.Skip()
            return

        if not self.mapdisplay.digittoolbar: # enable tool
            self.mapdisplay.AddToolbar("digit")
        else: # tool already enabled
            pass

        # mark layer as 'edited'
        self.mapdisplay.digittoolbar.StartEditing (maplayer)

        # enable 'stop editing'
        self.popupMenu.Enable (self.popupID5, False)
        self.popupMenu.Enable (self.popupID6, True)
        self.popupMenu.UpdateUI(self)

    def OnStopEditing (self, event):
        pass

    def OnPopupProperties (self, event):
        """Popup properties dialog"""
        self.PropertiesDialog(self.layer_selected)

    def RenameLayer (self, event):
        """Rename layer"""
        pass

    def AddLayer(self, ltype):
        """Add layer, create MapLayer instance"""
        self.first = True
        checked    = False
        params = {} # no initial options parameters
        
        if self.layer_selected:
            self.SelectItem(self.layer_selected, select=False)

        Debug.msg (3, "LayerTree().AddLayer(): ltype=%s" % (ltype))
        if ltype == 'command':
            # generic command layer
            ctrl = wx.TextCtrl(self, id=wx.ID_ANY, value='',
                               pos=wx.DefaultPosition, size=(250,25),
                               style=wx.TE_MULTILINE|wx.TE_WORDWRAP)
            checked = True
            ctrl.Bind(wx.EVT_TEXT_ENTER, self.OnCmdChanged)
            ctrl.Bind(wx.EVT_TEXT,       self.OnCmdChanged)
        elif ltype == 'group':
            ctrl = None
            grouptext = 'Layer group:' + str(self.groupnode)
            self.groupnode += 1
            checked = True
        else:
            # all other layers
            ctrl = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                               style=wx.SP_ARROW_KEYS)
            ctrl.SetRange(1,100)
            ctrl.SetValue(100)
            self.Bind(wx.EVT_SPINCTRL, self.OnOpacity, ctrl)

        # add layer to the layer tree
        if (self.layer_selected and self.layer_selected != self.GetRootItem() and \
            self.layers[self.layer_selected].type != 'group'): 
            parent = self.GetItemParent(self.layer_selected)
            layer = self.InsertItem(parent, self.GetPrevSibling(self.layer_selected),
                                    text='', ct_type=1, wnd=ctrl)
        # add layer to the group
        elif (self.layer_selected and self.layer_selected != self.GetRootItem() and \
              self.layers[self.layer_selected].type == 'group'): 
            layer = self.PrependItem(parent=self.layer_selected,
                                     text='', ct_type=1, wnd=ctrl)
            self.Expand(self.layer_selected)
        # add first layer to the layer tree
        else: 
            layer = self.PrependItem(parent=self.root, text='', ct_type=1, wnd=ctrl)

        # create Layer instance & add to self.layers dictionary
        newlayer = self.layers[layer] = Layer(type=ltype, wxCtrl=ctrl)

        # layer is initially unchecked as inactive (beside 'command')
        self.CheckItem(layer, checked=checked)

        # select item
        self.SelectItem(layer)

        # add a data object to hold the layer's command (does not apply to generic command layers)
        self.SetPyData(layer, (None,None))

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
            newlayer.AddMapLayer(self.Map.AddLayer(type=ltype, command=[],
                                                   l_active=checked, l_hidden=False, l_opacity=1, l_render=False))
            self.PropertiesDialog(layer)

    def PropertiesDialog (self, layer):
        """Launch the properties dialog"""
        global gmpath
        completed = ''
        params = self.GetPyData(layer)[1]
        ltype  = self.layers[layer].type

        Debug.msg (3, "LayerTree.PropertiesDialog(): ltype=%s" % \
                   ltype)
        
        if ltype == 'raster':
            menuform.GUI().ParseCommand('d.rast', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'rgb':
            menuform.GUI().ParseCommand('d.rgb', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'his':
            menuform.GUI().ParseCommand('d.his', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'rastarrow':
            menuform.GUI().ParseCommand('d.rast.arrow', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'rastnum':
            menuform.GUI().ParseCommand('d.rast.num', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'vector':
            menuform.GUI().ParseCommand('d.vect', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'thememap':
            menuform.GUI().ParseCommand('d.vect.thematic', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'themechart':
            menuform.GUI().ParseCommand('d.vect.chart', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'grid':
            menuform.GUI().ParseCommand('d.grid', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'labels':
            menuform.GUI().ParseCommand('d.labels', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'cmdlayer':
            pass
        elif ltype == 'group':
            pass

    def OnActivateLayer(self, event):
        layer = event.GetItem()
        self.layer_selected = layer

        self.PropertiesDialog (layer)

        if self.layers[layer].type == 'group':
            if self.IsExpanded(layer):
                self.Collapse(layer)
            else:
                self.Expand(layer)

    def OnDeleteLayer(self, event):
        """Remove selected layer for the layer tree"""

        item = event.GetItem()
        
        try:
            item.properties.Close(True)
        except:
            pass
        
        Debug.msg (3, "LayerTree.OnDeleteLayer(): name=%s" % \
                   (self.GetItemText(item)))

        # unselect item
        self.Unselect()
        self.layer_selected = None

        layer = self.layers[item]
        if layer.type != 'group':
            self.Map.DeleteLayer(layer.maplayer)

        self.layers.pop(item)

    def OnLayerChecked(self, event):
        item    = event.GetItem()
        checked = item.IsChecked()
        layer   = self.layers[item]
        
        if self.drag == False and self.first == False:
            # change active parameter for item in layers list in render.Map
            if layer.type == 'group':
                childitem = self.GetFirstChild(item)
                child = childitem[0]
                cookie = childitem[1]
                for n in range(0, self.GetChildrenCount(item)):
                    if checked == False:
                        childchecked = False
                    else:
                        childchecked = child.IsChecked()
                    self.Map.ChangeLayerActive(self.layers[child].maplayer, childchecked)
                    child = self.GetNextChild(item, cookie)[0]
            else:
                self.Map.ChangeLayerActive(self.layers[item].maplayer, checked)

    def OnCmdChanged(self, event):
        """Change command string"""
        ctrl = event.GetEventObject()
        cmd = event.GetString()

        layer = item = None
        
        for item, layer in self.layers.iteritems():
            if layer.wxCtrl == ctrl:
                break
            
        # change parameters for item in layers list in render.Map
        if item and self.drag == False:
            self.ChangeLayer(item)
            for option in layer.maplayer.GetCmd():
                if 'map=' in option:
                    mapname = option.split('=')[1]
                    self.Map.ChangeLayerName(layer.maplayer, mapname)
            
        event.Skip()

    def OnOpacity(self, event):
        """
        Set opacity level for map layer
        """
        Debug.msg (3, "LayerTree.OnOpacity(): %s" % event.GetInt())

        ctrl = event.GetEventObject()
        maplayer = None
        for layer in self.layers.itervalues():
            if layer.wxCtrl == ctrl:
                maplayer = layer.maplayer

        opacity = event.GetInt() / 100.
        # change opacity parameter for item in layers list in render.Map
        if maplayer and self.drag == False:
            self.Map.ChangeOpacity(maplayer, opacity)

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
        if self.layers[self.layer_selected].type == 'group':
            self.SetItemImage(self.layer_selected, self.folder)

    def OnExpandNode(self, event):
        """
        Expand node
        """
        self.layer_selected = event.GetItem()
        if self.layers[self.layer_selected].type == 'group':
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
        oldLayer = self.layers[oldItem]
        
        Debug.msg (4, "LayerTree.RecreateItem(): layer=%s" % \
                   self.GetItemText(oldItem))

        # recreate spin/text control for layer
        if oldLayer.type == 'command':
            newctrl = wx.TextCtrl(self, id=wx.ID_ANY, value='',
                                  pos=wx.DefaultPosition, size=(250,25),
                                  style=wx.TE_MULTILINE|wx.TE_WORDWRAP)
            try:
                newctrl.SetValue(oldLayer.maplayer.GetCmd(string=True))
            except:
                pass
            newctrl.Bind(wx.EVT_TEXT_ENTER, self.OnCmdChanged)
            newctrl.Bind(wx.EVT_TEXT,       self.OnCmdChanged)
        elif oldLayer.type == 'group':
            newctrl = None
        else:
            newctrl = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                                  style=wx.SP_ARROW_KEYS)
            newctrl.SetRange(1,100)
            try:
                newctrl.SetValue(oldLayer.maplayer.GetOpacity())
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
        if oldLayer.type == 'group':
            windval = None
            data    = None
        else:
            windval = self.GetItemWindow(oldItem).GetValue()
            data    = self.GetPyData(oldItem)

        # create GenericTreeItem instance
        if flag & wx.TREE_HITTEST_ABOVE:
            new = self.PrependItem(self.root, text=text, \
                                   ct_type=1, wnd=newctrl, image=image, \
                                   data=data)
        elif (flag &  wx.TREE_HITTEST_BELOW) or (flag & wx.TREE_HITTEST_NOWHERE) \
                 or (flag & wx.TREE_HITTEST_TOLEFT) or (flag & wx.TREE_HITTEST_TORIGHT):
            new = self.AppendItem(self.root, text=text, \
                                  ct_type=1, wnd=newctrl, image=image, \
                                  data=data)
        else:
            if parent:
                afteritem = parent
            else:
                afteritem = event.GetItem()

            if self.layers[afteritem].type == 'group':
                parent = afteritem
                new = self.AppendItem(parent, text=text, \
                                      ct_type=1, wnd=newctrl, image=image, \
                                      data=data)
                self.Expand(afteritem)
            else:
                parent = self.GetItemParent(afteritem)
                new = self.InsertItem(parent, afteritem, text=text, \
                                      ct_type=1, wnd=newctrl, image=image, \
                                      data=data)

        # add layer at new position
        self.layers[new]        = self.layers[oldItem]
        self.layers[new].wxCtrl = newctrl

        self.CheckItem(new, checked=checked)
        
        event.Skip()
        
        return new

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

        # recreate item and update self.layers dictionary
        newItem  = self.RecreateItem (event, self.dragItem)

        if self.layers[newItem].type == 'group':
            (child, cookei) = self.GetFirstChild(self.dragItem)
            if child:
                while child:
                    self.RecreateItem(event, child, parent=newItem)
                    self.Delete(child)
                    child = self.GetNextChild(old, cookei)[0]

            self.Expand(newItem)

        # delete layer at original position
        self.Delete(old) # entry in render.Map layers list automatically deleted by OnDeleteLayer handler
        # self.layers.pop(old)

        # reorder layers in render.Map to match new order after drag and drop
        self.ReorderLayers()

        # completed drag and drop
        self.drag = False

    def getOptData(self, dcmd, layer, params, propwin):
        for item in dcmd:
            if 'map=' in item:
                mapname = item.split('=')[1]
            elif 'red=' in item:
                mapname = item.split('=')[1]
            elif 'h_map=' in item:
                mapname = item.split('=')[1]
            elif 'd.grid' in item:
                mapname = 'grid'
            elif 'labels=' in item:
                mapname = item.split('=')[1]+' labels'

        # set layer text to map name
        self.SetItemText(layer, mapname)

        # add command to layer's data
        self.SetPyData(layer, (dcmd,params))

        # check layer as active
        self.CheckItem(layer, checked=True)

        # change parameters for item in layers list in render.Map
        self.ChangeLayer(layer)

        # set the layer properties dialog dictionary entry
        self.layers[layer].AddProperties (propwin)


    def writeDCommand(self, dcmd):
        # echos d.* command to output console
        global goutput
        goutput.write(dcmd+"\n----------\n")

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
            if self.layers[vislayer].type != 'group':
                treelayers.append(self.layers[vislayer].maplayer)

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
        layer = self.layers[item]
        
        if layer.type == 'command':
            if self.GetItemWindow(item).GetValue() != None:
                cmdlist = self.GetItemWindow(item).GetValue().split(' ')
                opac = 1.0
                chk = self.IsItemChecked(item)
                hidden = not self.IsVisible(item)
        elif layer.type != 'group':
            if self.GetPyData(item)[0] != None:
                cmdlist = self.GetPyData(item)[0]
                opac = float(self.GetItemWindow(item).GetValue())/100
                chk = self.IsItemChecked(item)
                hidden = not self.IsVisible(item)
        
        layer.maplayer = self.Map.ChangeLayer(layer=layer.maplayer, type=layer.maplayer.type, command=cmdlist, name=self.GetItemText(item),
                                              l_active=chk, l_hidden=hidden, l_opacity=opac, l_render=False)

        # if digitization tool enabled -> update list of available vector map layers
        if self.mapdisplay.digittoolbar:
            self.mapdisplay.digittoolbar.UpdateListOfLayers(updateTool=True)

    def setNotebookPage(self,pg):
        self.Parent.notebook.SetSelection(pg)

    def OnCloseWindow(self, event):
        pass
    # self.Map.Clean()

class TreeCtrlComboPopup(wx.combo.ComboPopup):
    """
    Create a tree ComboBox for selecting maps and other GIS elements
    in accessible mapsets within the current location
    """

    # overridden ComboPopup methods

    def Init(self):
        self.value = None
        self.curitem = None


    def Create(self, parent):
        self.tree = wx.TreeCtrl(parent, style=wx.TR_HIDE_ROOT
                                |wx.TR_HAS_BUTTONS
                                |wx.TR_SINGLE
                                |wx.TR_LINES_AT_ROOT
                                |wx.SIMPLE_BORDER
                                |wx.TR_FULL_ROW_HIGHLIGHT)
        self.tree.Bind(wx.EVT_MOTION, self.OnMotion)
        self.tree.Bind(wx.EVT_LEFT_DOWN, self.OnLeftDown)


    def GetControl(self):
        return self.tree


    def GetStringValue(self):
        if self.value:
            return self.tree.GetItemText(self.value)
        return ""


    def OnPopup(self):
        if self.value:
            self.tree.EnsureVisible(self.value)
            self.tree.SelectItem(self.value)


    def SetStringValue(self, value):
        # this assumes that item strings are unique...
        root = self.tree.GetRootItem()
        if not root:
            return
        found = self.FindItem(root, value)
        if found:
            self.value = found
            self.tree.SelectItem(found)


    def GetAdjustedSize(self, minWidth, prefHeight, maxHeight):
        return wx.Size(minWidth, min(200, maxHeight))


    def getElementList(self, element):
        """
        Get list of GIS elements in accessible mapsets and display as tree
        with all relevant elements displayed beneath each mapset branch
        """
        #set environmental variables
        gisdbase = os.popen('g.gisenv get=GISDBASE', "r").read().strip()
        location = os.popen('g.gisenv get=LOCATION_NAME', "r").read().strip()
        curr_mapset = os.popen('g.gisenv get=MAPSET', "r").read().strip()
        location_path = os.path.join (gisdbase,location)
        windfile = os.path.join (location_path,'PERMANENT','WIND')
        symbol_path = os.path.join (os.environ['GISBASE'],'etc','symbol')

        #valid location test if needed
        if windfile != None:
            pass

        #mapsets in current location
        mapsets = os.popen('g.mapsets -p', "r").read().lstrip().rstrip().split(' ')

        #Get directory tree nodes
        for dir in mapsets:
            if dir == curr_mapset:
                #TODO: make current mapset node expanded
                dir_node = self.AddItem('Mapset: '+dir)
                self.tree.SetItemTextColour(dir_node,wx.Colour(50,50,200))
                self.tree.Expand(dir_node)
                elem_list = os.listdir(os.path.join (location_path, dir, element))
                #TODO: sort list items?
                for elem in elem_list:
                    self.AddItem(elem, parent=dir_node)
            else:
                dir_node = self.AddItem('Mapset: '+dir)
                self.tree.SetItemTextColour(dir_node,wx.Colour(50,50,200))
                elem_list = os.listdir(os.path.join (location_path, dir, element))
                #TODO: sort list items?
                for elem in elem_list:
                    self.AddItem(elem+'@'+dir, parent=dir_node)

    # helpers

    def FindItem(self, parentItem, text):
        item, cookie = self.tree.GetFirstChild(parentItem)
        while item:
            if self.tree.GetItemText(item) == text:
                return item
            if self.tree.ItemHasChildren(item):
                item = self.FindItem(item, text)
            item, cookie = self.tree.GetNextChild(parentItem, cookie)
        return wx.TreeItemId();


    def AddItem(self, value, parent=None):
        if not parent:
            root = self.tree.GetRootItem()
            if not root:
                root = self.tree.AddRoot("<hidden root>")
            parent = root

        item = self.tree.AppendItem(parent, value)
        return item


    def OnMotion(self, evt):
        # have the selection follow the mouse, like in a real combobox
        item, flags = self.tree.HitTest(evt.GetPosition())
        if item and flags & wx.TREE_HITTEST_ONITEMLABEL:
            self.tree.SelectItem(item)
            self.curitem = item
        evt.Skip()


    def OnLeftDown(self, evt):
        # do the combobox selection
        item, flags = self.tree.HitTest(evt.GetPosition())
        if item and flags & wx.TREE_HITTEST_ONITEMLABEL:
            self.curitem = item
            self.value = item
            self.Dismiss()
        evt.Skip()


class GMConsole(wx.Panel):
    """
    Create and manage output console for commands entered on the
    GIS Manager command line.
    """
    def __init__(self, parent, id=-1,
                     pos=wx.DefaultPosition, size=wx.DefaultSize,
                     style=wx.TAB_TRAVERSAL|wx.FULL_REPAINT_ON_RESIZE):
        wx.Panel.__init__(self, parent, id, pos, size, style)
        #initialize variables

        self.parent = parent
        self.cmd_output = ""
        self.console_command = ""
        self.console_clear = ""
        self.console_save = ""
        self.gcmdlst = [] #list of commands in bin and scripts

        #text control for command output
        self.cmd_output = wx.TextCtrl(self, -1, "",
                                                  style=wx.TE_MULTILINE|
                                                  wx.TE_READONLY)
        self.cmd_output.SetFont(wx.Font(10, wx.FONTFAMILY_MODERN, wx.NORMAL, wx.NORMAL, 0, ''))

        global goutput
        goutput = self.cmd_output
        self.console_clear = wx.Button(self, -1, "Clear")
        self.console_save = wx.Button(self, -1, "Save")

        self.console_progressbar = wx.Gauge(self, -1, 100, (110, 50), (250, 25))

        self.Bind(wx.EVT_BUTTON, self.ClearHistory, self.console_clear)
        self.Bind(wx.EVT_BUTTON, self.SaveHistory, self.console_save)

        # output control layout
        boxsizer1 = wx.BoxSizer(wx.VERTICAL)
        gridsizer1 = wx.GridSizer(1, 2, 0, 0)
        boxsizer1.Add(self.cmd_output, 1,
                              wx.EXPAND|wx.ADJUST_MINSIZE, 0)
        gridsizer1.Add(self.console_clear, 0,
                               wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)
        gridsizer1.Add(self.console_save, 0,
                               wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)


        boxsizer1.Add((0,5))
        boxsizer1.Add(gridsizer1, 0, wx.EXPAND|wx.ALIGN_CENTRE_VERTICAL)
        boxsizer1.Add((0,5))
        boxsizer1.Add(self.console_progressbar, 0,
                              wx.EXPAND|wx.ADJUST_MINSIZE, 0)
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
        self.gcmdlst = os.listdir(gisbase+r'/bin')
        self.gcmdlst = self.gcmdlst + os.listdir(gisbase+r'/scripts')
        
        return self.gcmdlst

    def RunCmd(self, command):
        """
        Run in GUI or shell GRASS (or other) commands typed into
        console command text widget, echo command to
        output text widget, and send stdout output to output
        text widget.

        Note: 'command' is given as list

        TODO: Display commands (*.d) are captured and
        processed separately by mapdisp.py. Display commands are
        rendered in map display widget that currently has
        the focus (as indicted by mdidx).
        """

        # create list of available GRASS commands
        gcmdlst = self.GetGRASSCmds()
        # cmd = self.console_command.GetLineText(0)
        try:
            curr_disp = self.Parent.Parent.curr_page.maptree.mapdisplay
        except:
            #            disp_idx = None
            curr_disp = None

        if len(command) == 1 and command[0] in gcmdlst:
            # send GRASS command without arguments to GUI command interface
            # except display commands (they are handled differently)
            global gmpath
            if command[0][0:2] == "d.":
                try:
                    layertype = {'d.rast'         : 'raster',
                                 'd.rgb'          : 'rgb',
                                 'd.his'          : 'his',
                                 'd.legend'       : 'rastleg',
                                 'd.rast.arrow'   : 'rastarrow',
                                 'd.rast.num'     : 'rastnum',
                                 'd.vect'         : 'vector',
                                 'd.vect.thematic': 'thememap',
                                 'd.vect.chart'   : 'themechart',
                                 'd.grid'         : 'grid',
                                 'd.labels'       : 'labels'}[command[0]]
                except KeyError:
                    print _('Command type not yet implemented')
                    return

                # add layer
                self.Parent.Parent.curr_page.maptree.AddLayer(layertype)

            else:
                menuform.GUI().ParseCommand(command[0], gmpath, parentframe=None)
                self.command_output.write(command[0] + "\n----------\n")

        elif command[0][0:2] == "d." and len(command) > 1 and command[0] in gcmdlst:
            """
            Send GRASS display command(s)with arguments
            to the display processor and echo to command output console.
            Accepts a list of d.* commands separated by commas.
            Display with focus receives display command(s).
            """
            #self.cmd_output.write(command + "\n----------\n")
            self.cmd_output.write("$" + ' '.join(command))
            # TODO This needs to be fixed to use current rendering procedures
            # dcmds = command.split(',')
            #            curr_disp.addMapsToList(type='command', map=dcmds, mset=None)
            #            curr_disp.ReDrawCommand()

        else:
            # Send any other command to the shell. Send output to
            # console output window.
            try:
                os.environ["GRASS_MESSAGE_FORMAT"] = "gui"
                #self.cmd_output.write(command+"\n----------\n")
                self.cmd_output.write("$ " + ' '.join(command) + "\n")
                # activate compuational region (set with g.region) for all non-display commands.
                tmpreg = os.getenv("GRASS_REGION")
                os.unsetenv("GRASS_REGION")

                p = cmd.Command(command + ["--verbose"])

                # deactivate computational region and return to display settings
                if tmpreg:
                    os.environ["GRASS_REGION"] = tmpreg

                oline = p.module_stderr.readline()
                while oline:
                    oline = oline.strip()
                    oline = p.module_stderr.readline()
                    # make some progress
                    #GRASS_INFO_PERCENT: 100
                    if oline.find("GRASS_INFO_PERCENT")>-1:
                        self.console_progressbar.SetValue(int(oline.split()[1]))
                    elif oline.find("GRASS_INFO_MESSAGE")>-1:
                        self.cmd_output.write(string.split(oline,maxsplit=1)[1]+"\n")
                    elif oline.find("GRASS_INFO_WARNING")>-1:
                        self.cmd_output.write("WARNING: "+string.split(oline,maxsplit=1)[1]+"\n")
                    elif oline.find("GRASS_INFO_ERROR")>-1:
                        self.cmd_output.write("ERROR: "+string.split(oline,maxsplit=1)[1]+"\n")

                oline = p.module_stdout.readline()
                while oline:
                    oline = oline.strip()
                    if command[0] == 'r.what':
                        rastqlist = oline.split('|')
                        self.cmd_output.write('East: '+rastqlist[0]+"\n")
                        self.cmd_output.write('North: '+rastqlist[1]+"\n")
                        self.cmd_output.write(rastqlist[2]+"\n")
                        data = rastqlist[3:]
                        print 'data=',data
                        for x in range(0,len(data),2):
                            self.cmd_output.write('Category: '+data[x]+"\n")
                            self.cmd_output.write('Label: '+data[x+1]+"\n")
                    else:
                        self.cmd_output.write(oline+"\n")
                    print >> sys.stderr, oline
                    oline = p.module_stdout.readline()
                #self.cmd_output.write("\n==========\n")
                self.cmd_output.write("\n")
                if p.module_stdout < 0:
                    print >> sys.stderr, "Child was terminated by signal", p.module_stdout
                elif p.module_stdout > 0:
                    #print >> sys.stderr, p.module_stdout
                    pass
            except OSError, e:
                print >> sys.stderr, "Execution failed:", e

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
            self, message="Save file as ...", defaultDir=os.getcwd(),
            defaultFile="grass_cmd_history.txt", wildcard=wildcard, style=wx.SAVE|wx.FD_OVERWRITE_PROMPT
            )

        # Show the dialog and retrieve the user response. If it is the OK response,
        # process the data.
        if dlg.ShowModal() == wx.ID_OK:
            path = dlg.GetPath()

        output = open(path,"w")
        output.write(self.history)
        output.close()
        dlg.Destroy()
