"""!
@package vdigit.toolbars

@brief wxGUI vector digitizer toolbars

List of classes:
 - toolbars::VDigitToolbar

(C) 2007-2011 by the GRASS Development Team

This program is free software under the GNU General Public License
(>=v2). Read the file COPYING that comes with GRASS for details.

@author Martin Landa <landa.martin gmail.com>
"""
import wx

from grass.script import core as grass

from gui_core.toolbars  import BaseToolbar, BaseIcons
from gui_core.dialogs   import CreateNewVector
from vdigit.preferences import VDigitSettingsDialog
from vdigit.main        import VDigit
from core.debug         import Debug
from core.settings      import UserSettings
from core.gcmd          import GError
from icons.icon         import MetaIcon

class VDigitToolbar(BaseToolbar):
    """!Toolbar for digitization
    """
    def __init__(self, parent, mapcontent, layerTree = None, log = None):
        self.mapcontent    = mapcontent # Map class instance
        self.layerTree     = layerTree  # reference to layer tree associated to map display
        self.log           = log        # log area
        BaseToolbar.__init__(self, parent)
        self.digit         = None
        
        # currently selected map layer for editing (reference to MapLayer instance)
        self.mapLayer = None
        # list of vector layers from Layer Manager (only in the current mapset)
        self.layers   = [] 
        
        self.comboid    = None
        
        # only one dialog can be open
        self.settingsDialog   = None
        
        # create toolbars (two rows optionally)
        self.InitToolbar(self._toolbarData())
        self.Bind(wx.EVT_TOOL, self.OnTool)
        
        # default action (digitize new point, line, etc.)
        self.action = { 'desc' : '',
                        'type' : '',
                        'id'   : -1 }
        
        # list of available vector maps
        self.UpdateListOfLayers(updateTool = True)
        
        # realize toolbar
        self.Realize()
        # workaround for Mac bug. May be fixed by 2.8.8, but not before then.
        self.combo.Hide()
        self.combo.Show()
        
        # disable undo/redo
        self.EnableTool(self.undo, False)
        ### hide undo - this tool is quite buggy in GRASS 6 
        self.RemoveTool(self.undo) 
        
        # toogle to pointer by default
        self.OnTool(None)
        
        self.FixSize(width = 105)
        
    def _toolbarData(self):
        """!Toolbar data
        """
        data = []
        
        icons = {
            'addPoint'        : MetaIcon(img = 'point-create',
                                         label = _('Digitize new point'),
                                         desc = _('Left: new point')),
            'addLine'         : MetaIcon(img = 'line-create',
                                         label = _('Digitize new line'),
                                         desc = _('Left: new point; Ctrl+Left: undo last point; Right: close line')),
            'addBoundary'     : MetaIcon(img = 'boundary-create',
                                         label = _('Digitize new boundary'),
                                         desc = _('Left: new point; Ctrl+Left: undo last point; Right: close line')),
            'addCentroid'     : MetaIcon(img = 'centroid-create',
                                         label = _('Digitize new centroid'),
                                         desc = _('Left: new point')),
            'addArea'         : MetaIcon(img = 'polygon-create',
                                         label = _('Digitize new area (composition of boundaries without category and one centroid with category)'),
                                         desc = _('Left: new point')),
            'addVertex'       : MetaIcon(img = 'vertex-create',
                                         label = _('Add new vertex'),
                                         desc = _('Left: Select; Ctrl+Left: Unselect; Right: Confirm')),
            'deleteLine'      : MetaIcon(img = 'line-delete',
                                         label = _('Delete feature(s)'),
                                         desc = _('Left: Select; Ctrl+Left: Unselect; Right: Confirm')),
            'displayAttr'     : MetaIcon(img = 'attributes-display',
                                         label = _('Display/update attributes'),
                                         desc = _('Left: Select')),
            'displayCats'     : MetaIcon(img = 'cats-display',
                                         label = _('Display/update categories'),
                                         desc = _('Left: Select')),
            'editLine'        : MetaIcon(img = 'line-edit',
                                         label = _('Edit line/boundary'),
                                         desc = _('Left: new point; Ctrl+Left: undo last point; Right: close line')),
            'moveLine'        : MetaIcon(img = 'line-move',
                                         label = _('Move feature(s)'),
                                         desc = _('Left: Select; Ctrl+Left: Unselect; Right: Confirm')),
            'moveVertex'      : MetaIcon(img = 'vertex-move',
                                         label = _('Move vertex'),
                                         desc = _('Left: Select; Ctrl+Left: Unselect; Right: Confirm')),
            'removeVertex'    : MetaIcon(img = 'vertex-delete',
                                         label = _('Remove vertex'),
                                         desc = _('Left: Select; Ctrl+Left: Unselect; Right: Confirm')),
            'settings'        : BaseIcons['settings'].SetLabel(_('Digitization settings')),
            'quit'            : BaseIcons['quit'].SetLabel(label = _('Quit digitizer'),
                                                           desc = _('Quit digitizer and save changes')),
            'help'            : BaseIcons['help'].SetLabel(label = _('Vector Digitizer manual'),
                                                           desc = _('Show Vector Digitizer manual')),
            'additionalTools' : MetaIcon(img = 'tools',
                                         label = _('Additional tools '
                                                   '(copy, flip, connect, etc.)'),
                                         desc = _('Left: Select; Ctrl+Left: Unselect; Right: Confirm')),
            'undo'             : MetaIcon(img = 'undo',
                                          label = _('Undo'),
                                          desc = _('Undo previous changes')),
            }
        
        return self._getToolbarData(((None, ),
                                     ("addPoint", icons["addPoint"],
                                      self.OnAddPoint,
                                      wx.ITEM_CHECK),
                                     ("addLine", icons["addLine"],
                                      self.OnAddLine,
                                      wx.ITEM_CHECK),
                                     ("addBoundary", icons["addBoundary"],
                                      self.OnAddBoundary,
                                      wx.ITEM_CHECK),
                                     ("addCentroid", icons["addCentroid"],
                                      self.OnAddCentroid,
                                      wx.ITEM_CHECK),
                                     ("addArea", icons["addArea"],
                                      self.OnAddArea,
                                      wx.ITEM_CHECK),
                                     ("moveVertex", icons["moveVertex"],
                                      self.OnMoveVertex,
                                      wx.ITEM_CHECK),
                                     ("addVertex", icons["addVertex"],
                                      self.OnAddVertex,
                                      wx.ITEM_CHECK),
                                     ("removeVertex", icons["removeVertex"],
                                      self.OnRemoveVertex,
                                      wx.ITEM_CHECK),
                                     ("editLine", icons["editLine"],
                                      self.OnEditLine,
                                      wx.ITEM_CHECK),
                                     ("moveLine", icons["moveLine"],
                                      self.OnMoveLine,
                                      wx.ITEM_CHECK),
                                     ("deleteLine", icons["deleteLine"],
                                      self.OnDeleteLine,
                                      wx.ITEM_CHECK),
                                     ("displayCats", icons["displayCats"],
                                      self.OnDisplayCats,
                                      wx.ITEM_CHECK),
                                     ("displayAttr", icons["displayAttr"],
                                      self.OnDisplayAttr,
                                      wx.ITEM_CHECK),
                                     ("additionalTools", icons["additionalTools"],
                                      self.OnAdditionalToolMenu,
                                      wx.ITEM_CHECK),                                      
                                     (None, ),
                                     ("undo", icons["undo"],
                                      self.OnUndo),
                                     ("settings", icons["settings"],
                                      self.OnSettings),
                                     ("help", icons["help"],
                                      self.OnHelp),
                                     ("quit", icons["quit"],
                                      self.OnExit))
                                    )
    
    def OnTool(self, event):
        """!Tool selected -> disable selected tool in map toolbar"""
        aId = self.parent.toolbars['map'].GetAction(type = 'id')
        self.parent.toolbars['map'].ToggleTool(aId, False)
        
        # set cursor
        cursor = self.parent.cursors["cross"]
        self.parent.MapWindow.SetCursor(cursor)
        
        # pointer
        self.parent.OnPointer(None)
        
        if event:
            # deselect previously selected tool
            aId = self.action.get('id', -1)
            if aId != event.GetId() and \
                    self.action['id'] != -1:
                self.ToggleTool(self.action['id'], False)
            else:
                self.ToggleTool(self.action['id'], True)
            
            self.action['id'] = event.GetId()
            
            event.Skip()
        
        if self.action['id'] != -1:
            self.ToggleTool(self.action['id'], True)
        
        # clear tmp canvas
        if self.action['id'] != aId:
            self.parent.MapWindow.ClearLines(pdc = self.parent.MapWindow.pdcTmp)
            if self.digit and \
                    len(self.parent.MapWindow.digit.GetDisplay().GetSelected()) > 0:
                # cancel action
                self.parent.MapWindow.OnMiddleDown(None)
        
        # set focus
        self.parent.MapWindow.SetFocus()
        
    def OnAddPoint(self, event):
        """!Add point to the vector map Laier"""
        Debug.msg (2, "VDigitToolbar.OnAddPoint()")
        self.action = { 'desc' : "addLine",
                        'type' : "point",
                        'id'   : self.addPoint }
        self.parent.MapWindow.mouse['box'] = 'point'
        
    def OnAddLine(self, event):
        """!Add line to the vector map layer"""
        Debug.msg (2, "VDigitToolbar.OnAddLine()")
        self.action = { 'desc' : "addLine",
                        'type' : "line",
                        'id'   : self.addLine }
        self.parent.MapWindow.mouse['box'] = 'line'
        ### self.parent.MapWindow.polycoords = [] # reset temp line
                
    def OnAddBoundary(self, event):
        """!Add boundary to the vector map layer"""
        Debug.msg (2, "VDigitToolbar.OnAddBoundary()")
        if self.action['desc'] != 'addLine' or \
                self.action['type'] != 'boundary':
            self.parent.MapWindow.polycoords = [] # reset temp line
        self.action = { 'desc' : "addLine",
                        'type' : "boundary",
                        'id'   : self.addBoundary }
        self.parent.MapWindow.mouse['box'] = 'line'
        
    def OnAddCentroid(self, event):
        """!Add centroid to the vector map layer"""
        Debug.msg (2, "VDigitToolbar.OnAddCentroid()")
        self.action = { 'desc' : "addLine",
                        'type' : "centroid",
                        'id'   : self.addCentroid }
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnAddArea(self, event):
        """!Add area to the vector map layer"""
        Debug.msg (2, "VDigitToolbar.OnAddCentroid()")
        self.action = { 'desc' : "addLine",
                        'type' : "area",
                        'id'   : self.addArea }
        self.parent.MapWindow.mouse['box'] = 'line'

    def OnExit (self, event=None):
        """!Quit digitization tool"""
        # stop editing of the currently selected map layer
        if self.mapLayer:
            self.StopEditing()
        
        # close dialogs if still open
        if self.settingsDialog:
            self.settingsDialog.OnCancel(None)
            
        # set default mouse settings
        self.parent.MapWindow.mouse['use'] = "pointer"
        self.parent.MapWindow.mouse['box'] = "point"
        self.parent.MapWindow.polycoords = []
        
        # disable the toolbar
        self.parent.RemoveToolbar("vdigit")
        
    def OnMoveVertex(self, event):
        """!Move line vertex"""
        Debug.msg(2, "Digittoolbar.OnMoveVertex():")
        self.action = { 'desc' : "moveVertex",
                        'id'   : self.moveVertex }
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnAddVertex(self, event):
        """!Add line vertex"""
        Debug.msg(2, "Digittoolbar.OnAddVertex():")
        self.action = { 'desc' : "addVertex",
                        'id'   : self.addVertex }
        self.parent.MapWindow.mouse['box'] = 'point'
        
    def OnRemoveVertex(self, event):
        """!Remove line vertex"""
        Debug.msg(2, "Digittoolbar.OnRemoveVertex():")
        self.action = { 'desc' : "removeVertex",
                        'id'   : self.removeVertex }
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnEditLine(self, event):
        """!Edit line"""
        Debug.msg(2, "Digittoolbar.OnEditLine():")
        self.action = { 'desc' : "editLine",
                        'id'   : self.editLine }
        self.parent.MapWindow.mouse['box'] = 'line'

    def OnMoveLine(self, event):
        """!Move line"""
        Debug.msg(2, "Digittoolbar.OnMoveLine():")
        self.action = { 'desc' : "moveLine",
                        'id'   : self.moveLine }
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnDeleteLine(self, event):
        """!Delete line"""
        Debug.msg(2, "Digittoolbar.OnDeleteLine():")
        self.action = { 'desc' : "deleteLine",
                        'id'   : self.deleteLine }
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnDisplayCats(self, event):
        """!Display/update categories"""
        Debug.msg(2, "Digittoolbar.OnDisplayCats():")
        self.action = { 'desc' : "displayCats",
                        'id'   : self.displayCats }
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnDisplayAttr(self, event):
        """!Display/update attributes"""
        Debug.msg(2, "Digittoolbar.OnDisplayAttr():")
        self.action = { 'desc' : "displayAttrs",
                        'id'   : self.displayAttr }
        self.parent.MapWindow.mouse['box'] = 'point'
        
    def OnUndo(self, event):
        """!Undo previous changes"""
        self.digit.Undo()
        
        event.Skip()

    def EnableUndo(self, enable=True):
        """!Enable 'Undo' in toolbar
        
        @param enable False for disable
        """
        if not self.FindById(self.undo):
            return
        
        if enable:
            if self.GetToolEnabled(self.undo) is False:
                self.EnableTool(self.undo, True)
        else:
            if self.GetToolEnabled(self.undo) is True:
                self.EnableTool(self.undo, False)
        
    def OnSettings(self, event):
        """!Show settings dialog"""
        if self.digit is None:
            try:
                self.digit = self.parent.MapWindow.digit = VDigit(mapwindow = self.parent.MapWindow)
            except SystemExit:
                self.digit = self.parent.MapWindow.digit = None
        
        if not self.settingsDialog:
            self.settingsDialog = VDigitSettingsDialog(parent = self.parent, title = _("Digitization settings"),
                                                       style = wx.DEFAULT_DIALOG_STYLE)
            self.settingsDialog.Show()

    def OnHelp(self, event):
        """!Show digitizer help page in web browser"""
        log = self.parent.GetLayerManager().GetLogWindow()
        log.RunCmd(['g.manual',
                    'entry=wxGUI.Vector_Digitizer'])

    def OnAdditionalToolMenu(self, event):
        """!Menu for additional tools"""
        point = wx.GetMousePosition()
        toolMenu = wx.Menu()
        
        for label, itype, handler, desc in (
            (_('Break selected lines/boundaries at intersection'),
             wx.ITEM_CHECK, self.OnBreak, "breakLine"),
            (_('Connect selected lines/boundaries'),
             wx.ITEM_CHECK, self.OnConnect, "connectLine"),
            (_('Copy categories'),
             wx.ITEM_CHECK, self.OnCopyCats, "copyCats"),
            (_('Copy features from (background) vector map'),
             wx.ITEM_CHECK, self.OnCopy, "copyLine"),
            (_('Copy attributes'),
             wx.ITEM_CHECK, self.OnCopyAttrb, "copyAttrs"),
            (_('Feature type conversion'),
             wx.ITEM_CHECK, self.OnTypeConversion, "typeConv"),
            (_('Flip selected lines/boundaries'),
             wx.ITEM_CHECK, self.OnFlip, "flipLine"),
            (_('Merge selected lines/boundaries'),
             wx.ITEM_CHECK, self.OnMerge, "mergeLine"),
            (_('Snap selected lines/boundaries (only to nodes)'),
             wx.ITEM_CHECK, self.OnSnap, "snapLine"),
            (_('Split line/boundary'),
             wx.ITEM_CHECK, self.OnSplitLine, "splitLine"),
            (_('Query features'),
             wx.ITEM_CHECK, self.OnQuery, "queryLine"),
            (_('Z bulk-labeling of 3D lines'),
             wx.ITEM_CHECK, self.OnZBulk, "zbulkLine")):
            # Add items to the menu
            item = wx.MenuItem(parentMenu = toolMenu, id = wx.ID_ANY,
                               text = label,
                               kind = itype)
            toolMenu.AppendItem(item)
            self.parent.MapWindow.Bind(wx.EVT_MENU, handler, item)
            if self.action['desc'] == desc:
                item.Check(True)
        
        # Popup the menu.  If an item is selected then its handler
        # will be called before PopupMenu returns.
        self.parent.MapWindow.PopupMenu(toolMenu)
        toolMenu.Destroy()
        
        if self.action['desc'] == 'addPoint':
            self.ToggleTool(self.additionalTools, False)
        
    def OnCopy(self, event):
        """!Copy selected features from (background) vector map"""
        if self.action['desc'] == 'copyLine': # select previous action
            self.ToggleTool(self.addPoint, True)
            self.ToggleTool(self.additionalTools, False)
            self.OnAddPoint(event)
            return
        
        Debug.msg(2, "Digittoolbar.OnCopy():")
        self.action = { 'desc' : "copyLine",
                        'id'   : self.additionalTools }
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnSplitLine(self, event):
        """!Split line"""
        if self.action['desc'] == 'splitLine': # select previous action
            self.ToggleTool(self.addPoint, True)
            self.ToggleTool(self.additionalTools, False)
            self.OnAddPoint(event)
            return
        
        Debug.msg(2, "Digittoolbar.OnSplitLine():")
        self.action = { 'desc' : "splitLine",
                        'id'   : self.additionalTools }
        self.parent.MapWindow.mouse['box'] = 'point'


    def OnCopyCats(self, event):
        """!Copy categories"""
        if self.action['desc'] == 'copyCats': # select previous action
            self.ToggleTool(self.addPoint, True)
            self.ToggleTool(self.copyCats, False)
            self.OnAddPoint(event)
            return
        
        Debug.msg(2, "Digittoolbar.OnCopyCats():")
        self.action = { 'desc' : "copyCats",
                        'id'   : self.additionalTools }
        self.parent.MapWindow.mouse['box'] = 'point'

    def OnCopyAttrb(self, event):
        """!Copy attributes"""
        if self.action['desc'] == 'copyAttrs': # select previous action
            self.ToggleTool(self.addPoint, True)
            self.ToggleTool(self.copyCats, False)
            self.OnAddPoint(event)
            return
        
        Debug.msg(2, "Digittoolbar.OnCopyAttrb():")
        self.action = { 'desc' : "copyAttrs",
                        'id'   : self.additionalTools }
        self.parent.MapWindow.mouse['box'] = 'point'
        

    def OnFlip(self, event):
        """!Flip selected lines/boundaries"""
        if self.action['desc'] == 'flipLine': # select previous action
            self.ToggleTool(self.addPoint, True)
            self.ToggleTool(self.additionalTools, False)
            self.OnAddPoint(event)
            return
        
        Debug.msg(2, "Digittoolbar.OnFlip():")
        self.action = { 'desc' : "flipLine",
                        'id'   : self.additionalTools }
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnMerge(self, event):
        """!Merge selected lines/boundaries"""
        if self.action['desc'] == 'mergeLine': # select previous action
            self.ToggleTool(self.addPoint, True)
            self.ToggleTool(self.additionalTools, False)
            self.OnAddPoint(event)
            return
        
        Debug.msg(2, "Digittoolbar.OnMerge():")
        self.action = { 'desc' : "mergeLine",
                        'id'   : self.additionalTools }
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnBreak(self, event):
        """!Break selected lines/boundaries"""
        if self.action['desc'] == 'breakLine': # select previous action
            self.ToggleTool(self.addPoint, True)
            self.ToggleTool(self.additionalTools, False)
            self.OnAddPoint(event)
            return
        
        Debug.msg(2, "Digittoolbar.OnBreak():")
        self.action = { 'desc' : "breakLine",
                        'id'   : self.additionalTools }
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnSnap(self, event):
        """!Snap selected features"""
        if self.action['desc'] == 'snapLine': # select previous action
            self.ToggleTool(self.addPoint, True)
            self.ToggleTool(self.additionalTools, False)
            self.OnAddPoint(event)
            return
        
        Debug.msg(2, "Digittoolbar.OnSnap():")
        self.action = { 'desc' : "snapLine",
                        'id'   : self.additionalTools }
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnConnect(self, event):
        """!Connect selected lines/boundaries"""
        if self.action['desc'] == 'connectLine': # select previous action
            self.ToggleTool(self.addPoint, True)
            self.ToggleTool(self.additionalTools, False)
            self.OnAddPoint(event)
            return
        
        Debug.msg(2, "Digittoolbar.OnConnect():")
        self.action = { 'desc' : "connectLine",
                        'id'   : self.additionalTools }
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnQuery(self, event):
        """!Query selected lines/boundaries"""
        if self.action['desc'] == 'queryLine': # select previous action
            self.ToggleTool(self.addPoint, True)
            self.ToggleTool(self.additionalTools, False)
            self.OnAddPoint(event)
            return
        
        Debug.msg(2, "Digittoolbar.OnQuery(): %s" % \
                      UserSettings.Get(group = 'vdigit', key = 'query', subkey = 'selection'))
        self.action = { 'desc' : "queryLine",
                        'id'   : self.additionalTools }
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnZBulk(self, event):
        """!Z bulk-labeling selected lines/boundaries"""
        if not self.digit.IsVector3D():
            GError(parent = self.parent,
                   message = _("Vector map is not 3D. Operation canceled."))
            return
        
        if self.action['desc'] == 'zbulkLine': # select previous action
            self.ToggleTool(self.addPoint, True)
            self.ToggleTool(self.additionalTools, False)
            self.OnAddPoint(event)
            return
        
        Debug.msg(2, "Digittoolbar.OnZBulk():")
        self.action = { 'desc' : "zbulkLine",
                        'id'   : self.additionalTools }
        self.parent.MapWindow.mouse['box'] = 'line'

    def OnTypeConversion(self, event):
        """!Feature type conversion

        Supported conversions:
         - point <-> centroid
         - line <-> boundary
        """
        if self.action['desc'] == 'typeConv': # select previous action
            self.ToggleTool(self.addPoint, True)
            self.ToggleTool(self.additionalTools, False)
            self.OnAddPoint(event)
            return
        
        Debug.msg(2, "Digittoolbar.OnTypeConversion():")
        self.action = { 'desc' : "typeConv",
                        'id'   : self.additionalTools }
        self.parent.MapWindow.mouse['box'] = 'box'

    def OnSelectMap (self, event):
        """!Select vector map layer for editing

        If there is a vector map layer already edited, this action is
        firstly terminated. The map layer is closed. After this the
        selected map layer activated for editing.
        """
        if event.GetSelection() == 0: # create new vector map layer
            if self.mapLayer:
                openVectorMap = self.mapLayer.GetName(fullyQualified = False)['name']
            else:
                openVectorMap = None
            dlg = CreateNewVector(self.parent,
                                  exceptMap = openVectorMap, log = self.log,
                                  cmd = (('v.edit',
                                          { 'tool' : 'create' },
                                          'map')),
                                  disableAdd = True)
            
            if dlg and dlg.GetName():
                # add layer to map layer tree
                if self.layerTree:
                    mapName = dlg.GetName() + '@' + grass.gisenv()['MAPSET']
                    self.layerTree.AddLayer(ltype = 'vector',
                                            lname = mapName,
                                            lcmd = ['d.vect', 'map=%s' % mapName])
                    
                    vectLayers = self.UpdateListOfLayers(updateTool = True)
                    selection = vectLayers.index(mapName)
                
                # create table ?
                if dlg.IsChecked('table'):
                    lmgr = self.parent.GetLayerManager()
                    if lmgr:
                        lmgr.OnShowAttributeTable(None, selection = 'table')
                dlg.Destroy()
            else:
                self.combo.SetValue(_('Select vector map'))
                if dlg:
                    dlg.Destroy()
                return
        else:
            selection = event.GetSelection() - 1 # first option is 'New vector map'
        
        # skip currently selected map
        if self.layers[selection] == self.mapLayer:
            return
        
        if self.mapLayer:
            # deactive map layer for editing
            self.StopEditing()
        
        # select the given map layer for editing
        self.StartEditing(self.layers[selection])
        
        event.Skip()
        
    def StartEditing (self, mapLayer):
        """!Start editing selected vector map layer.
        
        @param mapLayer MapLayer to be edited
        """
        # deactive layer
        self.mapcontent.ChangeLayerActive(mapLayer, False)
        
        # clean map canvas
        self.parent.MapWindow.EraseMap()
        
        # unset background map if needed
        if mapLayer:
            if UserSettings.Get(group = 'vdigit', key = 'bgmap',
                                subkey = 'value', internal = True) == mapLayer.GetName():
                UserSettings.Set(group = 'vdigit', key = 'bgmap',
                                 subkey = 'value', value = '', internal = True)
            
            self.parent.SetStatusText(_("Please wait, "
                                        "opening vector map <%s> for editing...") % mapLayer.GetName(),
                                        0)
        
        self.parent.MapWindow.pdcVector = wx.PseudoDC()
        self.digit = self.parent.MapWindow.digit = VDigit(mapwindow = self.parent.MapWindow)
        
        self.mapLayer = mapLayer
        
        # open vector map
        if self.digit.OpenMap(mapLayer.GetName()) is None:
            self.mapLayer = None
            self.StopEditing()
            return False
        
        # update toolbar
        self.combo.SetValue(mapLayer.GetName())
        self.parent.toolbars['map'].combo.SetValue (_('Digitize'))
        lmgr = self.parent.GetLayerManager()
        if lmgr:
            lmgr.toolbars['tools'].Enable('vdigit', enable = False)
        
        Debug.msg (4, "VDigitToolbar.StartEditing(): layer=%s" % mapLayer.GetName())
        
        # change cursor
        if self.parent.MapWindow.mouse['use'] == 'pointer':
            self.parent.MapWindow.SetCursor(self.parent.cursors["cross"])
        
        if not self.parent.MapWindow.resize:
            self.parent.MapWindow.UpdateMap(render = True)
        
        # respect opacity
        opacity = mapLayer.GetOpacity(float = True)
        if opacity < 1.0:
            alpha = int(opacity * 255)
            self.digit.GetDisplay().UpdateSettings(alpha = alpha)
        
        return True

    def StopEditing(self):
        """!Stop editing of selected vector map layer.

        @return True on success
        @return False on failure
        """
        self.combo.SetValue (_('Select vector map'))
        
        # save changes
        if self.mapLayer:
            Debug.msg (4, "VDigitToolbar.StopEditing(): layer=%s" % self.mapLayer.GetName())
            if UserSettings.Get(group = 'vdigit', key = 'saveOnExit', subkey = 'enabled') is False:
                if self.digit.GetUndoLevel() > -1:
                    dlg = wx.MessageDialog(parent = self.parent,
                                           message = _("Do you want to save changes "
                                                     "in vector map <%s>?") % self.mapLayer.GetName(),
                                           caption = _("Save changes?"),
                                           style = wx.YES_NO | wx.YES_DEFAULT | wx.ICON_QUESTION)
                    if dlg.ShowModal() == wx.ID_NO:
                        # revert changes
                        self.digit.Undo(0)
                    dlg.Destroy()
            
            self.parent.SetStatusText(_("Please wait, "
                                        "closing and rebuilding topology of "
                                        "vector map <%s>...") % self.mapLayer.GetName(),
                                      0)
            self.digit.CloseMap()
            
            lmgr = self.parent.GetLayerManager()
            if lmgr:
                lmgr.toolbars['tools'].Enable('vdigit', enable = True)
                lmgr.GetLogWindow().GetProgressBar().SetValue(0)
                lmgr.GetLogWindow().WriteCmdLog(_("Editing of vector map <%s> successfully finished") % \
                                                    self.mapLayer.GetName(),
                                                switchPage = False)
            # re-active layer 
            item = self.parent.tree.FindItemByData('maplayer', self.mapLayer)
            if item and self.parent.tree.IsItemChecked(item):
                self.mapcontent.ChangeLayerActive(self.mapLayer, True)
        
        # change cursor
        self.parent.MapWindow.SetCursor(self.parent.cursors["default"])
        self.parent.MapWindow.pdcVector = None
        
        # close dialogs
        for dialog in ('attributes', 'category'):
            if self.parent.dialogs[dialog]:
                self.parent.dialogs[dialog].Close()
                self.parent.dialogs[dialog] = None
        
        del self.digit
        del self.parent.MapWindow.digit
        
        self.mapLayer = None
        
        self.parent.MapWindow.redrawAll = True
        
        return True
    
    def UpdateListOfLayers (self, updateTool = False):
        """!Update list of available vector map layers.
        This list consists only editable layers (in the current mapset)

        @param updateTool True to update also toolbar
        """
        Debug.msg (4, "VDigitToolbar.UpdateListOfLayers(): updateTool=%d" % \
                   updateTool)
        
        layerNameSelected = None
         # name of currently selected layer
        if self.mapLayer:
            layerNameSelected = self.mapLayer.GetName()
        
        # select vector map layer in the current mapset
        layerNameList = []
        self.layers = self.mapcontent.GetListOfLayers(l_type = "vector",
                                                      l_mapset = grass.gisenv()['MAPSET'])
        
        for layer in self.layers:
            if not layer.name in layerNameList: # do not duplicate layer
                layerNameList.append (layer.GetName())
        
        if updateTool: # update toolbar
            if not self.mapLayer:
                value = _('Select vector map')
            else:
                value = layerNameSelected

            if not self.comboid:
                self.combo = wx.ComboBox(self, id = wx.ID_ANY, value = value,
                                         choices = [_('New vector map'), ] + layerNameList, size = (80, -1),
                                         style = wx.CB_READONLY)
                self.comboid = self.InsertControl(0, self.combo)
                self.parent.Bind(wx.EVT_COMBOBOX, self.OnSelectMap, self.comboid)
            else:
                self.combo.SetItems([_('New vector map'), ] + layerNameList)
            
            self.Realize()
        
        return layerNameList

    def GetLayer(self):
        """!Get selected layer for editing -- MapLayer instance"""
        return self.mapLayer
