import os,sys,subprocess
import wx
import wx.lib.customtreectrl as CT

import render
import grassenv
import track
import menuform
import optpanels.rastopt as rastopt
import optpanels.vectopt as vectopt
import optpanels.cmdopt as cmdopt

#FIXME??
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
                 CT.TR_HIDE_ROOT | CT.TR_ROW_LINES | CT.TR_EDIT_LABELS,
                 log=None):
        CT.CustomTreeCtrl.__init__(self, parent, id, pos, size, style,ctstyle)

        self.SetAutoLayout(True)

        self.root = ""      # ID of layer tree root node
        self.layer = {}     # dictionary to index layers in layer tree
        self.node = 0       # index value for layers
        self.optpage = {}   # dictionary of notebook option pages for each map layer
        self.map = {}       # dictionary of map layers, indexed by tree node.
        self.layerID = ""   # ID of currently selected layer
        self.layername = "" # name off currently selected layer

        self.root = self.AddRoot("Map Layers")
        self.SetPyData(self.root, None)

        #create image list to use with layer tree
        il = wx.ImageList(16, 16, False)

        trgif = wx.Image(icons + r'/element-cell.gif', wx.BITMAP_TYPE_GIF)
        trgif.Rescale(16, 16)
        trgif = trgif.ConvertToBitmap()
        self.rast_icon = il.Add(trgif)
        # print "width=",trgif.GetWidth()
        # print "height=",trgif.GetHeight()
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
        # print "checksize=",checksize
        self.AssignImageList(il)

        # self.tree.SetItemImage(self.root, fldridx, wx.TreeItemIcon_Normal)
        # self.tree.SetItemImage(self.root, fldropenidx, wx.TreeItemIcon_Expanded)


        #        for x in range(15):
        #            child = self.AppendItem(self.root, "Item %d" % x)
        #            self.SetPyData(child, None)
        #            self.tree.SetItemImage(child, fldridx, wx.TreeItemIcon_Normal)
        #            self.tree.SetItemImage(child, fldropenidx, wx.TreeItemIcon_Expanded)

        self.Bind(wx.EVT_TREE_ITEM_EXPANDING, self.onExpandNode)
        self.Bind(wx.EVT_TREE_ITEM_COLLAPSED, self.onCollapseNode)
        self.Bind(wx.EVT_TREE_ITEM_ACTIVATED, self.onActivateLayer)
        self.Bind(wx.EVT_TREE_SEL_CHANGED,    self.onChangeSel)

    def AddLayer(self, idx, layertype):
        layername = layertype + ':' + str(self.node)

        if layertype == 'raster':
            self.map[self.node] = wx.ComboBox(self, -1,
                                              choices=["rast.map.1", "rast.map.2",
                                                       "rast.map.3", "rast.map.4",
                                                       "rast.map.5"],
                                              style=wx.CB_READONLY|wx.CB_DROPDOWN)
        elif layertype == 'vector':
            self.map[self.node]  = wx.ComboBox(self, -1,
                                               choices=["vect.map.1", "vect.map.2",
                                                        "vect.map.3", "vect.map.4",
                                                        "vect.map.5"],
                                               style=wx.CB_READONLY|wx.CB_DROPDOWN)

        elif layertype == 'command':
            self.map[self.node]  = wx.TextCtrl(self, -1,
                                               "Enter a GRASS command here",
                                               wx.DefaultPosition, (200,40),
                                               style=wx.TE_MULTILINE|wx.TE_WORDWRAP)

        if self.node >0 and self.layerID:
            self.layer[self.node] = self.InsertItem(self.root, self.layerID,
                                                    layername, ct_type=1,
                                                    wnd=self.map[self.node] )
        else:
            self.layer[self.node] = self.AppendItem(self.root, layername,
                                                    ct_type=1, wnd=self.map[self.node] )

        self.SetPyData(self.layer[self.node], None)

#        #add icons for each layer
        if layertype == 'raster':
            self.SetItemImage(self.layer[self.node], self.rast_icon)
        elif layertype == 'vector':
            self.SetItemImage(self.layer[self.node], self.vect_icon)
        elif layertype == 'command':
            self.SetItemImage(self.layer[self.node], self.cmd_icon)

        self.node += 1

    def onCollapseNode(self, event):
        print 'group collapsed'
        event.Skip()

    def onExpandNode(self, event):
        self.layerID = event.GetItem()
        self.layername = self.GetItemText(event.GetItem())
        print 'group expanded'
        event.Skip()

    def onActivateLayer(self, event):
        self.layerID = event.GetItem()
        self.layername = self.GetItemText(event.GetItem())
        # call a method to make this item display or not display?
        # change associated icon accordingly?
        print self.layername,'is activated'
        if self.layername[0:4] == 'rast':
            print 'its a raster'
            rastopt.MyFrame(self)
        elif self.layername[0:4] == 'vect':
            print 'its a vector'
            vectopt.MyPanel(self)
#        elif self.layername[0:4] == 'comm':
#            print 'its a command'
#            cmdopt.MyPanel(self)
        event.Skip()

    def onChangeSel(self, event):
        self.layerID = event.GetItem()
        self.layername = self.GetItemText(event.GetItem())
        # old code for selecting options panels for each layer
        #        old_layername = ""
        #        if str(event.GetOldItem()) != str(event.GetItem()):
        #            old_layername = self.GetItemText(event.GetOldItem())
        #        new_layername = self.GetItemText(event.GetItem())
        #        if old_layername:
        #            self.optpage[old_layername].Show(False)
        #        self.optpage[new_layername].Show(True)
        event.Skip()

class GMConsole(wx.Panel):

    def __init__(self, parent, id=-1,
                     pos=wx.DefaultPosition, size=wx.DefaultSize,
                     style=wx.TAB_TRAVERSAL|wx.FULL_REPAINT_ON_RESIZE):
        wx.Panel.__init__(self, parent, id, pos, size, style)
        #initialize variables

        self.cmd_output = ""
        self.console_command = ""
#        self.console_run = ""
        self.console_clear = ""
        self.console_save = ""
        self.gcmdlst = [] #list of commands in bin and scripts

        #text control for command output
        self.cmd_output = wx.TextCtrl(self, -1, "",
                                                  style=wx.TE_MULTILINE|
                                                  wx.TE_READONLY|wx.HSCROLL)

        #"run" button deactivated because I don't know how to get the command from gism
#    	self.console_run = wx.Button(self, -1, _("Run"))
#    	self.console_run.SetDefault()
    	self.console_clear = wx.Button(self, -1, _("Clear"))
    	self.console_save = wx.Button(self, -1, _("Save"))
#		self.cmd_output.SetMinSize((100, 100))
#		self.console_command.SetMinSize((100, 50))

#    	self.Bind(wx.EVT_BUTTON, self.runCmd, self.console_run)
    	self.Bind(wx.EVT_BUTTON, self.clearHistory, self.console_clear)
    	self.Bind(wx.EVT_BUTTON, self.saveHistory, self.console_save)
#		self.Bind(wx.EVT_TEXT_ENTER, self.runCmd, self.console_command)

		# console layout
    	boxsizer1 = wx.BoxSizer(wx.VERTICAL)
    	gridsizer1 = wx.GridSizer(1, 2, 0, 0)
    	boxsizer1.Add(self.cmd_output, 1,
                              wx.EXPAND|wx.ADJUST_MINSIZE, 0)
#		boxsizer1.Add(self.console_command, 0,
#                              wx.EXPAND|wx.ADJUST_MINSIZE, 0)
#    	gridsizer1.Add(self.console_run, 0,
#                               wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)
    	gridsizer1.Add(self.console_clear, 0,
                               wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)
    	gridsizer1.Add(self.console_save, 0,
                               wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)

    	boxsizer1.Add(gridsizer1, 0, wx.EXPAND|wx.BORDER, 100)
    	boxsizer1.Fit(self)
    	boxsizer1.SetSizeHints(self)
    	self.SetAutoLayout(True)
    	self.SetSizer(boxsizer1)

    def getGRASSCmds(self):
		'''Create list of all available GRASS commands'''
		gisbase = os.environ['GISBASE']
		self.gcmdlst = os.listdir(gisbase+r'/bin')
		self.gcmdlst.append(os.listdir(gisbase+r'/scripts'))
		return self.gcmdlst

    def runCmd(self, cmd):
    	'''Run in GUI or shell GRASS (or other) commands typed into
    	console command text widget, echo command to console
    	output text widget, and send stdout output to output
    	text widget. Display commands (*.d) are captured and
    	processed separately by mapdisp.py. Display commands are
    	rendered in map display widget that currently has
    	the focus (as indicted by mdidx).'''
    	gcmdlst = self.getGRASSCmds()
    	cmdlst = []
#    	cmd = self.console_command.GetLineText(0)
    	cmdlst = cmd.split(' ')
        print 'command = ', cmd
    	disp_idx = int(track.Track().GetDisp()[0])
        print 'display = ', disp_idx
    	curr_disp = track.Track().GetDisp()[1]
        print 'Im here'

    	if len(cmdlst) == 1 and cmd in gcmdlst:
    		# Send GRASS command without arguments to GUI command interface
    		# except display commands (they are handled differently)
    		global gmpath
    		if cmd[0:2] == "d.":
    			print "Add map layer to GIS Manager to see " \
                                    "GUI for display command"
    			return
    		else:
    			menuform.GUI().parseCommand(cmd, gmpath)
    			self.cmd_output.write(cmdlst[0] +
                                                          "\n----------\n")

    	elif cmd[0:2] == "d." and len(cmdlst) > 1 and cmdlst[0] in gcmdlst:
    		# Send GRASS display command(s)with arguments
    		# to the display processor and echo to command output console.
    		# Accepts a list of d.* commands separated by commas.
    		# Display with focus receives display command(s).
    		self.cmd_output.write(cmd +
                                                  "\n----------\n")
    		dcmds = cmd.split(',')
    		curr_disp.setDcommandList(dcmds)

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
		#could use a standard dialog for this
		output = open("history.txt","w")
		output.write(self.history)
		output.close()


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
