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
from debug import Debug as Debug
from icon import Icons as Icons
try:
    from subprocess import *
except:
    from compat import subprocess


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

        self.Map = render.Map()  # instance of render.Map to be associated with display
        self.root = ""           # ID of layer tree root node
        self.groupnode = 0       # index value for layers
        self.optpage = {}        # dictionary of notebook option pages for each map layer
        self.layer_selected = "" # ID of currently selected layer
        self.layertype = {}      # dictionary of layer types for each layer
        self.layerctrl = {}      # dictionary of layers indexed by special wind controls (spinctrl and textctrl)
        self.saveitem = {}       # dictionary to preserve layer attributes for drag and drop
        self.first = True        # indicates if a layer is just added or not
        self.drag = False        # flag to indicate a drag event is in process
        self.disp_idx = idx
        self.gismgr = gismgr
        self.notebook = notebook # GIS Manager notebook for layer tree
        self.treepg = parent     # notebook page holding layer tree


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

        self.Bind(wx.EVT_TREE_ITEM_EXPANDING,   self.onExpandNode)
        self.Bind(wx.EVT_TREE_ITEM_COLLAPSED,   self.onCollapseNode)
        self.Bind(wx.EVT_TREE_ITEM_ACTIVATED,   self.onActivateLayer)
        self.Bind(wx.EVT_TREE_SEL_CHANGED,      self.onChangeSel)
        self.Bind(CT.EVT_TREE_ITEM_CHECKED,     self.onLayerChecked)
        self.Bind(wx.EVT_TREE_DELETE_ITEM,      self.onDeleteLayer)
        self.Bind(wx.EVT_TREE_BEGIN_DRAG,       self.onBeginDrag)
        self.Bind(wx.EVT_TREE_END_DRAG,         self.onEndDrag)
        self.Bind(wx.EVT_CONTEXT_MENU,          self.OnContextMenu)
        self.Bind(wx.EVT_TREE_END_LABEL_EDIT,   self.OnChangeLayerName)
        # self.Bind(wx.EVT_CLOSE, self.onCloseWindow)

    def OnChangeLayerName (self, event):
        """Change layer name"""
        Debug.msg (3, "LayerTree.OnChangeLayerName: name=%s" % event.GetLabel())

        self.Map.ChangeLayerName (self.layer_selected, event.GetLabel())

    def OnContextMenu (self, event):
        """Context Layer Menu"""

        if not self.layer_selected:
            event.Skip()
            return

        ltype = self.layertype[self.layer_selected]

        if not hasattr (self, "popupID1"):
            self.popupID1 = wx.NewId()
            self.popupID2 = wx.NewId()
            self.popupID3 = wx.NewId()
            self.popupID4 = wx.NewId()
            self.popupID5 = wx.NewId()
            self.popupID6 = wx.NewId()

            self.Bind (wx.EVT_MENU, self.gismgr.deleteLayer,        id=self.popupID1)
            self.Bind (wx.EVT_MENU, self.RenameLayer,               id=self.popupID2)
            self.Bind (wx.EVT_MENU, self.OnPopupProperties,         id=self.popupID3)
            self.Bind (wx.EVT_MENU, self.gismgr.ShowAttributeTable, id=self.popupID4)
            self.Bind (wx.EVT_MENU, self.OnStartEditing,            id=self.popupID5)
            self.Bind (wx.EVT_MENU, self.OnStopEditing,             id=self.popupID6)

        self.popupMenu = wx.Menu()
        # general item
        self.popupMenu.Append (self.popupID1, _("Delete"))
        self.popupMenu.Append (self.popupID2, _("Rename"))

        # map layer items
        if ltype != "command" and ltype != "group": # properties
            self.popupMenu.AppendSeparator()
            self.popupMenu.Append(self.popupID3, text=_("Properties"))

        # specific items
        if ltype == "vector": # show attribute table
            self.popupMenu.AppendSeparator()
            self.popupMenu.Append(self.popupID4, _("Show attribute table"))
            self.popupMenu.Append(self.popupID5, _("Start editing"))
            layer = self.Map.GetLayer(self.layer_selected)
            # enable editing only for vector map layers available in the current mapset
            if layer.mapset != grassenv.env["MAPSET"]:
                self.popupMenu.Enable (self.popupID5, False)
            self.popupMenu.Append(self.popupID6, _("Stop editing"))
            self.popupMenu.Enable(self.popupID6, False)

        self.PopupMenu(self.popupMenu)
        self.popupMenu.Destroy()

    def OnStartEditing (self, event):
        """
        Editing of vector map layer requested by the user
        """
        layer = self.Map.GetLayer(self.layer_selected)

        if not layer:
            event.Skip()
            return

        if not self.mapdisplay.digittoolbar: # activate tool
            self.mapdisplay.AddToolbar("digit")
        else: # tool already active
            pass

        # mark layer as 'edited'
        print "#", self.mapdisplay.digittoolbar.StartEditing (layer)

        # enable 'stop editing'
        self.popupMenu.Enable (self.popupID6, True)

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
        params = {} # no initial options parameters

        if self.layer_selected:
            self.SelectItem(self.layer_selected, select=False)

        Debug.msg (3, "LayerTree().AddLayer(): ltype=%s" % (ltype))
        if ltype == 'command':
            # generic command layer
            self.ctrl = wx.TextCtrl(self, id=wx.ID_ANY, value='',
                                    pos=wx.DefaultPosition, size=(250,40),
                                    style=wx.TE_MULTILINE|wx.TE_WORDWRAP)
            self.ctrl.Bind(wx.EVT_TEXT_ENTER, self.onCmdChanged)
            self.ctrl.Bind(wx.EVT_TEXT, self.onCmdChanged)
        elif ltype == 'group':
            self.ctrl = None
            grouptext = 'Layer group:' + str(self.groupnode)
            self.groupnode += 1
        else:
            # all other layers
            self.ctrl = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                                    style=wx.SP_ARROW_KEYS)
            self.ctrl.SetRange(1,100)
            self.ctrl.SetValue(100)
            self.Bind(wx.EVT_SPINCTRL, self.OnOpacity, self.ctrl)

        if (self.layer_selected and self.layer_selected != self.GetRootItem() and \
                self.layertype[self.layer_selected] != 'group'):
            parent = self.GetItemParent(self.layer_selected)
            layer = self.InsertItem(parent, self.GetPrevSibling(self.layer_selected),
                                '', ct_type=1, wnd=self.ctrl )
        elif (self.layer_selected and self.layer_selected != self.GetRootItem() and \
                self.layertype[self.layer_selected] == 'group'):
            layer = self.PrependItem(self.layer_selected,
                                '', ct_type=1, wnd=self.ctrl )
            self.Expand(self.layer_selected)
        else:
            layer = self.PrependItem(self.root, '', ct_type=1, wnd=self.ctrl)

        self.SelectItem(layer)

        # add to layertype and layerctrl dictionaries
        self.layertype[layer] = ltype
        self.layerctrl[self.ctrl] = layer

        # add a data object to hold the layer's command (does not apply to generic command layers)
        self.SetPyData(layer, (None,None))

        #layer is initially unchecked as inactive
        self.CheckItem(layer, checked=False)

        # add layer to layers list in render.Map
        if self.layertype[layer] != 'group':
            self.Map.AddLayer(item=layer, type="command", command='',
                              l_active=False, l_hidden=False, l_opacity=1, l_render=False)

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
            # if digitization tool enable, update list of available vector maps
            if self.mapdisplay.digittoolbar:
                self.mapdisplay.digittoolbar.UpdateListOfLayers(updateTool=True)
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
            self.CheckItem(layer, checked=True)

        self.first = False

        self.PropertiesDialog(layer)

    def PropertiesDialog (self, layer):
        """Launch the properties dialog"""
        global gmpath
        completed = ''
        params = self.GetPyData(layer)[1]
        ltype   = self.layertype[layer]

        if ltype == 'raster':
            menuform.GUI().parseCommand('d.rast', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'rgb':
            menuform.GUI().parseCommand('d.rgb', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'his':
            menuform.GUI().parseCommand('d.his', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'rastarrow':
            menuform.GUI().parseCommand('d.rast.arrow', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'rastnum':
            menuform.GUI().parseCommand('d.rast.num', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'vector':
            menuform.GUI().parseCommand('d.vect', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'thememap':
            menuform.GUI().parseCommand('d.vect.thematic', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'themechart':
            menuform.GUI().parseCommand('d.vect.chart', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'grid':
            menuform.GUI().parseCommand('d.grid', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'labels':
            menuform.GUI().parseCommand('d.labels', gmpath, completed=(self.getOptData,layer,params), parentframe=self)
        elif ltype == 'command':
            pass
        elif ltype == 'group':
            pass

    def onActivateLayer(self, event):
        layer = event.GetItem()
        self.layer_selected = layer

        self.PropertiesDialog (layer)

        if self.layertype[layer] == 'group':
            if self.IsExpanded(layer):
                self.Collapse(layer)
            else:
                self.Expand(layer)

    def onDeleteLayer(self, event):
        """Remove selected layer for the layer tree"""

        Debug.msg (3, "LayerTree.onDeleteLayer():")

        layer = event.GetItem()

        # delete layer in render.Map
        if self.layertype[layer] != 'group':
            self.Map.delLayer(item=layer)

        self.layertype.pop(layer)
        self.Unselect()
        self.layer_selected = None

    def onLayerChecked(self, event):
        layer = event.GetItem()
        checked = layer.IsChecked()

        if self.drag == False and self.first == False:
            # change active parameter for item in layers list in render.Map
            if self.layertype[layer] == 'group':
                childitem = self.GetFirstChild(layer)
                child = childitem[0]
                cookie = childitem[1]
                for n in range(0,self.GetChildrenCount(layer)):
                    if checked == False:
                        childchecked = False
                    else:
                        childchecked = child.IsChecked()
                    self.Map.changeActive(child, childchecked)
                    child = self.GetNextChild(layer, cookie)[0]
            else:
                self.Map.changeActive(layer, checked)


    def onCmdChanged(self, event):
        layer = self.layerctrl[event.GetEventObject()]
        cmd = event.GetString()

        if self.drag == False:
            # change parameters for item in layers list in render.Map
            self.ChangeLayer(layer)
        event.Skip()

    def OnOpacity(self, event):
        """
        Set opacity level for map layer
        """
        Debug.msg (3, "LayerTree.OnOpacity(): %s" % event.GetInt())

        if 'Spin' in str(event.GetEventObject()):
            layer = self.layerctrl[event.GetEventObject()]
        else:
            layer = self.layerctrl[event.GetEventObject().GetParent()]
        opacity = float(event.GetInt()) / 100

        if self.drag == False:
            # change opacity parameter for item in layers list in render.Map
            self.Map.ChangeOpacity(layer, opacity)

    def onChangeSel(self, event):
        layer = event.GetItem()
        self.layer_selected = layer

    def onCollapseNode(self, event):
        if self.layertype[self.layer_selected] == 'group':
            self.SetItemImage(self.layer_selected, self.folder)

    def onExpandNode(self, event):
        self.layer_selected = event.GetItem()
        if self.layertype[self.layer_selected] == 'group':
            self.SetItemImage(self.layer_selected, self.folder_open)

    def onBeginDrag(self, event):
        """ Drag and drop of single tree nodes
        """

        # node cannot be a parent
        if self.GetChildrenCount(event.GetItem()) == 0:
            event.Allow()
            self.drag = True
            self.DoSelectItem(event.GetItem(), unselect_others=True)

            # save everthing associated with item to drag
            self.dragItem = event.GetItem()
            self.saveitem['ltype'] = self.layertype[self.dragItem]
            self.saveitem['check'] = self.IsItemChecked(self.dragItem)
            self.saveitem['image'] = self.GetItemImage(self.dragItem, 0)
            self.saveitem['text'] = self.GetItemText(self.dragItem)
            self.saveitem['wind'] = self.GetItemWindow(self.dragItem)
            if self.layertype[self.dragItem] == 'group':
                self.saveitem['windval'] = None
                self.saveitem['data'] = None
            else:
                self.saveitem['windval'] = self.GetItemWindow(self.dragItem).GetValue()
                self.saveitem['data'] = self.GetPyData(self.dragItem)
        else:
            print ("Can't drag a node that has children")

    def onEndDrag(self, event):
        """
        Insert copy of layer in new position and
        delete original at old position
        """

        self.drag = True
        # Make sure this memeber exists.
        try:
            old = self.dragItem
        except:
            return

        # recreate spin/text control for layer
        if self.layertype[old] == 'command':
            newctrl = wx.TextCtrl(self, id=wx.ID_ANY, value='',
                               pos=wx.DefaultPosition, size=(250,40),
                               style=wx.TE_MULTILINE|wx.TE_WORDWRAP)
            newctrl.Bind(wx.EVT_TEXT_ENTER, self.onCmdChanged)
        elif self.layertype[old] == 'group':
            newctrl = None
        else:
            newctrl = wx.SpinCtrl(self, id=wx.ID_ANY, value="", pos=(30, 50),
                                    style=wx.SP_ARROW_KEYS)
            newctrl.SetRange(1,100)
            newctrl.SetValue(100)
            newctrl.Bind(wx.EVT_TEXT, self.OnOpacity)

        # Decide where to put new layer and put it there
        flag = self.HitTest(event.GetPoint())[1]

        if flag & wx.TREE_HITTEST_ABOVE:
            new = self.PrependItem(self.root, text=self.saveitem['text'], \
                              ct_type=1, wnd=newctrl, image=self.saveitem['image'], \
                              data=self.saveitem['data'])
        elif (flag &  wx.TREE_HITTEST_BELOW) or (flag & wx.TREE_HITTEST_NOWHERE) \
            or (flag & wx.TREE_HITTEST_TOLEFT) or (flag & wx.TREE_HITTEST_TORIGHT):
            new = self.AppendItem(self.root, text=self.saveitem['text'], \
                              ct_type=1, wnd=newctrl, image=self.saveitem['image'], \
                              data=self.saveitem['data'])

        else:
            if not event.GetItem():
                return
            else:
                afteritem = event.GetItem()
                if self.layertype[afteritem] == 'group':
                    parent = afteritem
                    new = self.AppendItem(parent, text=self.saveitem['text'], \
                                  ct_type=1, wnd=newctrl, image=self.saveitem['image'], \
                                  data=self.saveitem['data'])
                    self.Expand(afteritem)
                else:
                    parent = self.GetItemParent(afteritem)
                    new = self.InsertItem(parent, afteritem, text=self.saveitem['text'], \
                                  ct_type=1, wnd=newctrl, image=self.saveitem['image'], \
                                  data=self.saveitem['data'])

        self.layertype[new] = self.saveitem['ltype']
        self.CheckItem(new, checked=self.saveitem['check'])
        if self.layertype[new] != 'group':
            self.layerctrl[newctrl] = new
            newctrl.SetValue(self.saveitem['windval'])

        # update lookup dictionary in render.Map
        if self.layertype[new] != 'group':
            self.Map.updateLookup(old, new)

        # delete layer at original position
        self.Delete(old) # entry in render.Map layers list automatically deleted by onDeleteLayer handler

        # reorder layers in render.Map to match new order after drag and drop
        self.reorderLayers()

        # completed drag and drop
        self.drag = False

    def getOptData(self, dcmd, layer, params):
        for item in dcmd.split(' '):
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
        self.ChangeLayer(layer, mapname)


    def writeDCommand(self, dcmd):
        # echos d.* command to output console
        global goutput
        goutput.write(dcmd+"\n----------\n")

    def reorderLayers(self):
        """
        add commands from data associated with
        any valid layers (checked or not) to layer list in order to
        match layers in layer tree
        """

        # make a list of visible layers
        treelayers = []
        vislayer = self.GetFirstVisibleItem()
        for item in range(0,self.GetCount()):
            if self.layertype[vislayer] != 'group':
                treelayers.append(vislayer)
            if self.GetNextVisible(vislayer) == None:
                break
            else:
                vislayer = self.GetNextVisible(vislayer)
        treelayers.reverse()
        self.Map.reorderLayers(treelayers)

    def ChangeLayer(self, layer, mapname):
        """Change layer"""
        if self.layertype[layer] == 'command':
            if self.GetItemWindow(layer).GetValue() != None:
                cmd = self.GetItemWindow(layer).GetValue()
                opac = 1.0
                chk = self.IsItemChecked(layer)
                hidden = not self.IsVisible(layer)
        elif self.layertype[layer] != 'group':
            if self.GetPyData(layer)[0] != None:
                cmd = self.GetPyData(layer)[0]
                opac = float(self.GetItemWindow(layer).GetValue())/100
                chk = self.IsItemChecked(layer)
                hidden = not self.IsVisible(layer)

        # mapset?
        mapset = None
        mapidx = cmd.find("map=")
        if mapidx > -1:
            mapset=cmd[mapidx:].split(' ')[0]
            mapidx = mapset.find('@')
            if mapidx > -1:
                mapset = mapset[mapidx+1:]

        self.Map.ChangeLayer(item=layer, type='command', command=cmd, name=mapname, mapset=mapset,
                             l_active=chk, l_hidden=hidden, l_opacity=opac, l_render=False)

    def setNotebookPage(self,pg):
        self.Parent.notebook.SetSelection(pg)

    def onCloseWindow(self, event):
        pass
#        self.Map.Clean()

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

        self.Bind(wx.EVT_BUTTON, self.clearHistory, self.console_clear)
        self.Bind(wx.EVT_BUTTON, self.saveHistory, self.console_save)

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

    def getGRASSCmds(self):
        '''
        Create list of all available GRASS commands to use when
        parsing string from the command line
        '''
        self.gcmdlst = []
        gisbase = os.environ['GISBASE']
        self.gcmdlst = os.listdir(gisbase+r'/bin')
        self.gcmdlst = self.gcmdlst + os.listdir(gisbase+r'/scripts')
        return self.gcmdlst

    def runCmd(self, command):
        """
        Run in GUI or shell GRASS (or other) commands typed into
        console command text widget, echo command to
        output text widget, and send stdout output to output
        text widget.

        TODO: Display commands (*.d) are captured and
        processed separately by mapdisp.py. Display commands are
        rendered in map display widget that currently has
        the focus (as indicted by mdidx).
        """

        gcmdlst = self.getGRASSCmds()
        cmdlst = []
        #    	cmd = self.console_command.GetLineText(0)
        cmdlst = command.split(' ')
        try:
            curr_disp = self.Parent.Parent.curr_page.maptree.mapdisplay
        except:
            #            disp_idx = None
            curr_disp = None

        if len(cmdlst) == 1 and command in gcmdlst:
            # Send GRASS command without arguments to GUI command interface
            # except display commands (they are handled differently)
            global gmpath
            if command[0:2] == "d.":
                try:
                    layertype = {'d.rast': 'raster',
                        'd.rgb'          : 'rgb',
                        'd.his'          : 'his',
                        'd.legend'       : 'rastleg',
                        'd.rast.arrow'   : 'rastarrow',
                        'd.rast.num'     : 'rastnum',
                        'd.vect'         : 'vector',
                        'd.vect.thematic': 'thememap',
                        'd.vect.chart'   : 'themechart',
                        'd.grid'         : 'grid',
                        'd.labels'       : 'labels'} [command]
                except KeyError:
                    print 'Command type not yet implemented'
                    return

                # add layer
                self.Parent.Parent.curr_page.maptree.AddLayer(layertype)

            else:
                menuform.GUI().parseCommand(command, gmpath, parentframe=None)
                self.command_output.write(cmdlst[0] +
                                                          "\n----------\n")

        elif command[0:2] == "d." and len(cmdlst) > 1 and cmdlst[0] in gcmdlst:
            """
            Send GRASS display command(s)with arguments
            to the display processor and echo to command output console.
            Accepts a list of d.* commands separated by commas.
            Display with focus receives display command(s).
            """
            #self.cmd_output.write(command + "\n----------\n")
            self.cmd_output.write("$" + command)
            dcmds = command.split(',')
            # TODO This needs to be fixed to use current rendering procedures

#            curr_disp.addMapsToList(type='command', map=dcmds, mset=None)
#            curr_disp.ReDrawCommand()

        else:
            # Send any other command to the shell. Send output to
            # console output window.
            try:
                os.environ["GRASS_MESSAGE_FORMAT"] = "gui"
                #self.cmd_output.write(command+"\n----------\n")
                self.cmd_output.write("$ " + command + "\n")
                # activate compuational region (set with g.region) for all non-display commands.
                tmpreg = os.getenv("GRASS_REGION")
                os.unsetenv("GRASS_REGION")

                p = cmd.Command(cmd=command + " --verbose")

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
                    if command.split(' ')[0] == 'r.what':
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
                if p.module_stdout < 0:
                    print >> sys.stderr, "Child was terminated by signal", p.module_stdout
                elif p.module_stdout > 0:
                    #print >> sys.stderr, p.module_stdout
                    pass
            except OSError, e:
                print >> sys.stderr, "Execution failed:", e

    def clearHistory(self, event):
        self.cmd_output.Clear()
        self.console_progressbar.SetValue(0)

    def saveHistory(self, event):
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
