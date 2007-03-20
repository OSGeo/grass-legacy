import os,sys
import wx
import wx.lib.customtreectrl as CT
import wx.combo
from wx.lib.combotreebox import ComboTreeBox

import track
import select
import optpanels.raster_prop as raster_prop
import optpanels.vectopt as vectopt
import optpanels.cmdopt as cmdopt

#FIXME??
try:
   import subprocess
except:
   from compat import subprocess


gmpath = os.getenv("GISBASE") + "/etc/wx/gism/"
sys.path.append(gmpath)

icons = ""

if not os.getenv("GRASS_ICONPATH"):
    icons = os.getenv("GISBASE") + "/etc/gui/icons/"
else:
    icons = os.environ["GRASS_ICONPATH"]

class LayerTree(CT.CustomTreeCtrl):
    """
    Creates layer tree structure
    """
    #	def __init__(self, parent, id, pos, size, style):
    def __init__(self, parent,
                 id=wx.ID_ANY, pos=wx.DefaultPosition,
                 size=wx.DefaultSize, style=wx.SUNKEN_BORDER,
                 ctstyle=CT.TR_HAS_BUTTONS | CT.TR_HAS_VARIABLE_ROW_HEIGHT |
                 CT.TR_HIDE_ROOT | CT.TR_ROW_LINES | CT.TR_FULL_ROW_HIGHLIGHT,
                 disp=None, log=None):
        CT.CustomTreeCtrl.__init__(self, parent, id, pos, size, style,ctstyle)

        # we need this only for GIS Manager, but not for e.g. mapdisp
        import menuform

        self.SetAutoLayout(True)
        self.SetGradientStyle(1)
        self.EnableSelectionGradient(True)
        self.SetFirstGradientColour(wx.Colour(150, 150, 150))

        self.display = ""   # ID of map display associated with layer tree
        self.root = ""      # ID of layer tree root node
        self.layer = {}     # dictionary to index layers in layer tree
        self.node = 0       # index value for layers
        self.optpage = {}   # dictionary of notebook option pages for each map layer
        self.map = {}       # dictionary of map layers, indexed by tree node.
        self.layer_selected = ""   # ID of currently selected layer
        self.layername = "" # name off currently selected layer
        self.layertype = {} # dictionary of layer types for each layer
        self.saveitem = {} # dictionary to preserve layer attributes for drag and drop
        self.dcmdopts = ''

        self.display = disp

        self.root = self.AddRoot("Map Layers")
        self.SetPyData(self.root, None)

        #create image list to use with layer tree
        il = wx.ImageList(16, 16, False)

        trgif = wx.Image(icons + r'/element-cell.gif', wx.BITMAP_TYPE_GIF)
        trgif.Rescale(16, 16)
        trgif = trgif.ConvertToBitmap()
        self.rast_icon = il.Add(trgif)
        trgif = wx.Image(icons + r'/element-vector.gif', wx.BITMAP_TYPE_GIF)
        trgif.Rescale(16, 16)
        trgif = trgif.ConvertToBitmap()
        self.vect_icon = il.Add(trgif)

        trgif = wx.Image(icons + r'/gui-cmd.gif', wx.BITMAP_TYPE_GIF)
        trgif.Rescale(16, 16)
        trgif = trgif.ConvertToBitmap()
        self.cmd_icon = il.Add(trgif)

        checksize = il.GetSize(0)
        checkbmp = il.GetBitmap(0)
        self.AssignImageList(il)

        # use when groups implemented
        # self.tree.SetItemImage(self.root, fldridx, wx.TreeItemIcon_Normal)
        # self.tree.SetItemImage(self.root, fldropenidx, wx.TreeItemIcon_Expanded)

        self.Bind(wx.EVT_TREE_ITEM_EXPANDING, self.onExpandNode)
        self.Bind(wx.EVT_TREE_ITEM_COLLAPSED, self.onCollapseNode)
        self.Bind(wx.EVT_TREE_ITEM_ACTIVATED, self.onActivateLayer)
        self.Bind(wx.EVT_TREE_SEL_CHANGED,    self.onChangeSel)
        self.Bind(CT.EVT_TREE_ITEM_CHECKED, self.onLayerChecked)
        self.Bind(wx.EVT_TREE_BEGIN_DRAG, self.onBeginDrag)
        self.Bind(wx.EVT_TREE_END_DRAG, self.onEndDrag)

    def AddLayer(self, idx, type):
#        layername = type + ':' + str(self.node)

        if type == 'raster':
            self.map = select.Select(self, id=wx.ID_ANY, size=(250,-1), type='cell')
            self.map.Bind(wx.EVT_TEXT, self.onMapChanged)

        elif type == 'vector':
            self.map = select.Select(self, id=wx.ID_ANY, size=(250,-1), type='vector')
            self.map.Bind(wx.EVT_TEXT, self.onMapChanged)

        elif type == 'command':
            self.map = wx.TextCtrl(self, -1,
                                               '',
                                               wx.DefaultPosition, (250,40),
                                               style=wx.TE_MULTILINE|wx.TE_WORDWRAP)
            self.map.Bind(wx.EVT_TEXT_ENTER, self.onMapChanged)

        if self.layer_selected and self.layer_selected != self.GetRootItem():
            self.layer = self.InsertItem(self.root, self.GetPrevSibling(self.layer_selected),
                                                    '', ct_type=1,
                                                    wnd=self.map )
        else:
            self.layer = self.PrependItem(self.root, '', ct_type=1, wnd=self.map)

        #add to layertype dictionary
        self.layertype[self.layer] = type

        self.SetPyData(self.layer, None)

#        #add icons for each layer
        if type == 'raster':
            self.SetItemImage(self.layer, self.rast_icon)
        elif type == 'vector':
            self.SetItemImage(self.layer, self.vect_icon)
        elif type == 'command':
            self.SetItemImage(self.layer, self.cmd_icon)

        #layer is initially checked as active
        self.CheckItem(self.layer)
        self.node += 1
        self.createLayerList()

    def onCollapseNode(self, event):
        print 'group collapsed'
        event.Skip()

    def onExpandNode(self, event):
        self.layer_selected = event.GetItem()
        print 'group expanded'
        event.Skip()

    def onBeginDrag(self, event):
        '''Allow drag-and-drop for leaf nodes.'''
        if self.GetChildrenCount(event.GetItem()) == 0:
            event.Allow()
            self.dragItem = event.GetItem()
            self.saveitem['check'] = self.IsItemChecked(self.dragItem)
            self.saveitem['image'] = self.GetItemImage(self.dragItem, 0)
            self.saveitem['text'] = self.GetItemText(self.dragItem)
            self.saveitem['wind'] = self.GetItemWindow(self.dragItem)
            self.saveitem['windval'] = self.GetItemWindow(self.dragItem).GetValue()
            self.saveitem['data'] = self.GetPyData(self.dragItem)
        else:
            print ("Cant drag a node that has children")

    def onEndDrag(self, event):
        '''Do the re-organization if possible'''

        #If we dropped somewhere that isn't on top of an item, ignore the event
        if not event.GetItem():
            return

        # Make sure this memeber exists.
        try:
            old = self.dragItem
        except:
            return

        # Get the other IDs that are involved
        afteritem = event.GetItem()
        parent = self.GetItemParent(afteritem)
        if not parent:
            return

        newwind = self.saveitem['wind']
        new = self.InsertItem(parent, afteritem, text=self.saveitem['text'], \
                              ct_type=1, wnd=newwind, image=self.saveitem['image'], \
                              data=self.saveitem['data'])
        self.CheckItem(new, checked=self.saveitem['check'])
        newwind.SetValue(self.saveitem['windval'])

        self.Delete(old)

    def onActivateLayer(self, event):
        global gmpath
        layer = event.GetItem()
        self.layer_selected = layer
       # When double clicked, open options dialog
        if self.layertype[layer] == 'raster':
#            raster_prop.MyFrame(self)
            self.dcmdopts = menuform.GUI().parseCommand('d.rast', gmpath, completed=self.getOptData)
        elif self.layertype[layer] == 'vector':
#            print 'its a vector'
#            vectopt.MyPanel(self)
            self.dcmdopts = menuform.GUI().parseCommand('d.vect', gmpath, completed=self.getOptData)
        self.createLayerList()

    def onLayerChecked(self, event):
        Layer = event.GetItem()
        self.createLayerList()

    def onChangeSel(self, event):
        layer = event.GetItem()
        self.layer_selected = layer

    def onMapChanged(self, event):
        map = event.GetString()
        self.createLayerList()
#        event.Skip()

    def getOptData(self):
        print 'the options are =', self.dcmdopts

    def createLayerList(self):
        self.display.cleanLayersList()
        for layer in self.layertype.keys():
            if self.GetItemWindow(layer) != None:
                name = self.GetItemWindow(layer).GetValue()
                if '@' in name:
                    msname = name.split('@')[1]
                    name = name.split('@')[0]
                else:
                    msname = None
                if self.IsItemChecked(layer) == True and \
                    self.GetItemWindow(layer).GetValue() != '' and \
                    self.GetItemWindow(layer).GetValue()[0:7] != 'Mapset:':
                    if self.layertype[layer] == 'raster':
                        self.display.addMapsToList(type='raster', map=name, mset=msname)
                        #TODO: need to add options for layer
                    elif self.layertype[layer] == 'vector':
                        self.display.addMapsToList(type='vector', map=name, mset=msname)
                    elif self.layertype[layer] == 'command':
                        self.display.addMapsToList(type='command', map=name, mset=msname)


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

        self.cmd_output = ""
        self.console_command = ""
        self.console_clear = ""
        self.console_save = ""
        self.gcmdlst = [] #list of commands in bin and scripts

        #text control for command output
        self.cmd_output = wx.TextCtrl(self, -1, "",
                                                  style=wx.TE_MULTILINE|
                                                  wx.TE_READONLY|wx.HSCROLL)

    	self.console_clear = wx.Button(self, -1, _("Clear"))
    	self.console_save = wx.Button(self, -1, _("Save"))

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
        boxsizer1.Fit(self)
    	boxsizer1.SetSizeHints(self)
    	self.SetAutoLayout(True)
    	self.SetSizer(boxsizer1)

    def getGRASSCmds(self):
		'''
        Create list of all available GRASS commands to use when
        parsing string from the command line
        '''
		gisbase = os.environ['GISBASE']
		self.gcmdlst = os.listdir(gisbase+r'/bin')
		self.gcmdlst.append(os.listdir(gisbase+r'/scripts'))
		return self.gcmdlst

    def runCmd(self, cmd):
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
    	cmdlst = cmd.split(' ')
    	disp_idx = int(track.Track().GetDisp()[0])
    	curr_disp = track.Track().GetDisp()[1]

    	if len(cmdlst) == 1 and cmd in gcmdlst:
    		# Send GRASS command without arguments to GUI command interface
    		# except display commands (they are handled differently)
            global gmpath
            if cmd[0:2] == "d.":
                if cmd == 'd.rast':
                    layertype = 'raster'
                elif cmd == 'd.vect':
                    layertype = 'vector'
                else:
                    print 'Command type not yet implemented'
                    return

                if disp_idx != None:
                    # get layer tree for active display
                    layertree = track.Track().GetCtrls(disp_idx, 2)
                    # add layer
                    layertree.AddLayer(disp_idx, layertype)

            else:
                menuform.GUI().parseCommand(cmd, gmpath)
                self.cmd_output.write(cmdlst[0] +
                                                          "\n----------\n")

    	elif cmd[0:2] == "d." and len(cmdlst) > 1 and cmdlst[0] in gcmdlst:
            """
            Send GRASS display command(s)with arguments
            to the display processor and echo to command output console.
            Accepts a list of d.* commands separated by commas.
            Display with focus receives display command(s).
            """
            self.cmd_output.write(cmd +
                                                  "\n----------\n")
            dcmds = cmd.split(',')
            curr_disp.addMapsToList(type='command', map=dcmds, mset=None)
            curr_disp.ReDrawCommand()

    	else:
    		# Send any other command to the shell. Send output to
    		# console output window.
            try:
                retcode = subprocess.call(cmd, shell=True)

                if retcode < 0:
    				print >> sys.stderr, "Child was terminated by signal", retcode
                elif retcode > 0:
    				print >> sys.stderr, "Child returned", retcode
            except OSError, e:
    			print >> sys.stderr, "Execution failed:", e

            self.cmd_output.write(cmd+"\n----------\n")
            #FIXME - why is PIPE not recognized?
#            self.out = subprocess.Popen(cmd, shell=True, stdout=PIPE).stdout
            self.out = os.popen(cmd, "r").read()
            self.cmd_output.write(self.out+"\n")

    def clearHistory(self, event):
		self.cmd_output.Clear()

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

def GetTempfile( pref=None):
    """
    Creates GRASS temporary file using defined prefix.

    Returns:
        Path to file name (string) or None
    """

    tempfile = os.popen("g.tempfile pid=%d" %
                        os.getpid()).readlines()[0].strip()

    if not tempfile:
        return None
    else:
        path,file = os.path.split(tempfile)
        if pref:
            file = "%s%s" % (pref,file)
        return os.path.join(path,file)
