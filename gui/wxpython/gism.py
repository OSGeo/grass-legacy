#!/usr/bin/env python
import sys
import os
import wx
import wx.lib.customtreectrl as CT
import re
import tempfile

try:
    import subprocess
except:
    from compat import subprocess

import Gism
gmpath = Gism.__path__[0]
sys.path.append(gmpath)

if not os.getenv("GISBASE"):
    sys.stderr.write("GISBASE not set, you have to be in running GRASS session!\n")
    sys.exit(1)

gipath = ""
if not os.getenv("GRASS_ICONPATH"):
    gipath = os.getenv("GISBASE")+"/etc/gui/icons/"
else:
    gipath = os.environ["GRASS_ICONPATH"]

os.environ["GRASS_ICONPATH"] = gipath

import Gism.gismutils as gmutils
import Gism.mapdisp as mapdisp
import Gism.render as render
import Gism.menudata as menudata
import Gism.menuform as menuform


"""Main Python app to set up GIS Manager window and trap commands
Only command console is working currently, but windows for
panels and layer tree done and demo tree items appear"""

##########################################################################
#
# gism.py - wxPython prototype GUI for GRASS 6+
#
# Authors: Michael Barton (Arizona State University) &
#	Jachym Cepicky (Mendel University of Agriculture)
#
# August 2006
#
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
##########################################################################



menucmd = {}

class GMFrame(wx.Frame):
    '''GIS Manager frame with notebook widget for controlling
    GRASS GIS. Includes command console page for typing GRASS
    (and other) commands, tree widget page for managing GIS map layers,'''
    def __init__(self, *args, **kwds):
        kwds["style"] = wx.DEFAULT_FRAME_STYLE
        wx.Frame.__init__(self, *args, **kwds)
        self.createToolBar()
        self.createMenuBar()
        self.SetTitle(_("GRASS GIS Manager - wxPython Prototype"))
        self.SetSize((450, 450))
        self.SetMinSize((100, 100))
        self.cb_panel = wx.Panel(self)
        
        # create choicebook for GIS controls - one page for each display
        self.gm_cb = GMChoicebook(self.cb_panel, -1, wx.DefaultPosition, wx.DefaultSize, wx.CHB_TOP)
        boxsizer = wx.BoxSizer()
        boxsizer.Add(self.gm_cb, 1, wx.EXPAND)
        self.cb_panel.SetSizer(boxsizer)
        self.cb_panel.SetAutoLayout(True)
        self.Centre()
        render.Track().SetChbk(self.gm_cb)
        
        # initialize variables
        self.MapDisplay = {} #dictionary to index open map displays
        self.disp_idx = 0 #index value for map displays and layer trees
        self.maptree = {} #dictionary to index a layer tree to accompanying a map display
        self.mapfocus = 0 #track which display currently has focus
        
        self.Bind(wx.EVT_CLOSE, self.onCloseWindow)
        
        #start default initial display
        self.NewDisplay()
    
    #---Menubar creation---#000000#FFFFFF-------------------------------------------
    
    def createMenuBar(self):
        menuBar = wx.MenuBar()
        # data object with descriptions for each menu item is in separate module menudata.py
        self.Data = menudata.Data()
        for eachMenuData in self.Data.GetMenu():
            for eachHeading in eachMenuData:
                menuLabel = eachHeading[0]
                menuItems = eachHeading[1]
                menuBar.Append(self.createMenu(menuItems), menuLabel)
        self.SetMenuBar(menuBar)
    
    def createMenu(self, menuData):
        menu = wx.Menu()
        for eachItem in menuData:
            if len(eachItem) == 2:
                label = eachItem[0]
                subMenu = self.createMenu(eachItem[1])
                menu.AppendMenu(wx.NewId(), label, subMenu)
            else:
                self.createMenuItem(menu, *eachItem)
        return menu
    
    def createMenuItem(self, menu, label, help, handler, gcmd, kind=wx.ITEM_NORMAL):
        if not label:
            menu.AppendSeparator()
            return
        menuItem = menu.Append(-1, label, help, kind)
        if label:
            menucmd[label] = gcmd
        rhandler = eval(handler)
        self.Bind(wx.EVT_MENU, rhandler, menuItem)
    
    def runMenuCmd(self, event):
        '''Run menu command'''
        menuitem = self.GetMenuBar().FindItemById(event.GetId())
        itemtext = menuitem.GetText()
        cmd = menucmd[itemtext]
        global gmpath
        menuform.GUI().parseCommand(cmd, gmpath)
    
    #---Toolbar creation---#000000#FFFFFF-------------------------------------------
    def createToolBar(self):
        toolbar = self.CreateToolBar()
        for each in self.toolbarData():
            self.addToolbarButton(toolbar, *each)
        toolbar.Realize()
    
    def addToolbarButton(self, toolbar, label, iconfile, help, handler):
        if not label:
            toolbar.AddSeparator()
            return
        icon = wx.Bitmap(iconfile, wx.BITMAP_TYPE_ANY)
        tool = toolbar.AddSimpleTool(-1, icon, label, help)
        self.Bind(wx.EVT_TOOL, handler, tool)
    
    def toolbarData(self):

        global gipath 
        return (
            ('newdisplay', gipath+r'/gui-startmon.gif', 'Start new display', self.NewDisplay),
            ('', '', '', ''),
            ('addraster', gipath+r'/element-cell.gif', 'Add raster layer', self.AddRaster),
            ('addvect', gipath+r'/element-vector.gif', 'Add vector layer', self.AddVector),
            ('addcmd', gipath+r'/gui-cmd.gif', 'Add command layer', self.AddCommand)
            )
    
    #---Start display---#000000#FFFFFF----------------------------------------------
    def NewDisplay(self, event=None):
        '''Create new map display widget'''
        #update display index
        print 'disp_idx=',self.disp_idx
        #start a new display, indexed by disp_idx
        #mID = wx.NewId()
        self.MapDisplay[self.disp_idx] = mapdisp.MapFrame(self, -1, 'Map Display', wx.DefaultPosition, wx.DefaultSize,
            style=wx.DEFAULT_FRAME_STYLE, cb=self.gm_cb, idx=self.disp_idx)
        self.MapDisplay[self.disp_idx].SetTitle(_("Map Display-"+str(self.disp_idx)))
        #self.maptree[self.disp_idx] = self.MapDisplay[self.disp_idx].getTree()
        self.MapDisplay[self.disp_idx].Show()
        self.disp_idx += 1
    
    #---ToolBar button handlers---#000000#FFFFFF------------------------------------
    def AddRaster(self, event):
        self.SetTree('raster')
        event.Skip()
    
    def AddVector(self, event):
        self.SetTree('vector')
        event.Skip()
    
    def AddCommand(self, event):
        self.SetTree('command')
        event.Skip()
    
    def SetTree(self, layertype):
        print 'hello world'
        disp_idx = self.MapDisplay[self.disp_idx].getdisp_idx()
        print 'disp_idx in settree=',disp_idx
        self.maptree[self.disp_idx].AddLayer(self.gm_nb, disp_idx, layertype)
        return
    
    #---Misc methods---#000000#FFFFFF-----------------------------------------------
    def onCloseWindow(self, event):
        '''Cleanup when gism.py is quit'''
        mdlist = range(0, self.disp_idx+1)
        try:
            for md in mdlist:
                if self.MapDisplay.has_key(md):
                    if self.MapDisplay[md].Close(False):
                        self.MapDisplay[md].Close(True)
        except:
            self.DestroyChildren()
        self.Destroy()
    
    def Nomethod(self, event):
        '''Stub for testing'''
        pass
        event.Skip()
    
    def printmd(self):
        print 'self.disp_idx is now', self.disp_idx

class GMChoicebook(wx.Choicebook):
    '''This class creates a choicebook widget for the GIS Manager 
    control panel. The choice book allows and controls an independent 
    ayer tree, command console, and layer options for each display 
    opened.'''
    def __init__(self, parent, id, pos, size, style):
        wx.Choicebook.__init__(self, parent, id, pos, size, style)
        #nb_panel=wx.Panel(self,-1)
##        self.curr_page=self.GetCurrentPage()
##        print "the page=",self.curr_page
##        gm_nb =  wx.Notebook(self,-1,pos=wx.DefaultPosition, size=wx.DefaultSize,style=wx.NB_RIGHT)
        #self.AddPage(gm_nb, "the first page")
        # set up initial pages
        # page 0 for layer tree - modified by mapdisplay module
##        nb_page0 = LayerTree(gm_nb)
##        gm_nb.AddPage(nb_page0, _("Layers"), select=True)
##        nb_page2 = GMConsole(gm_nb)
##        gm_nb.AddPage(nb_page2, _("Console"))

##		# create choicebook on page 0 for layer tree pages
##		# one choicebook page for each layer tree/display
##		self.treeCB = wx.Choicebook(self.page0, -1, wx.DefaultPosition, wx.DefaultSize, style=wx.CHB_TOP)
##		boxsizer0 = wx.BoxSizer()
##		boxsizer0.Add(self.treeCB, 1, wx.EXPAND)
##		self.page0.SetSizer(boxsizer0)
##		self.page0.SetAutoLayout(True)
##		self.Centre()
##
##		self.treeCB.Bind(wx.EVT_CHOICEBOOK_PAGE_CHANGED, self.OnCBPageChanged)
##		self.treeCB.Bind(wx.EVT_CHOICEBOOK_PAGE_CHANGING, self.OnCBPageChanging)
##
####		# page 1 for layer options - modified by LayerTree class
####		self.page1 = wx.ScrolledWindow(self, -1, style=wx.FULL_REPAINT_ON_RESIZE)
####		self.page1.SetScrollRate(10,10)
####		self.page1.SetBackgroundColour(wx.BLUE)
######		self.page1 = wx.Panel(self, -1)
####		self.AddPage(self.page1, _("Options"))
####		boxsizer1 = wx.BoxSizer()
####		boxsizer1.Add(self.page1, 1, wx.EXPAND)
####		self.page1.SetSizer(boxsizer1)
####		self.page1.SetAutoLayout(True)
####		self.Centre()
##
##		# page 2 for console
##		self.page2 = GMConsole(self)
##		self.AddPage(self.page2, 'Console', select=True)

	# choicebook methods
	def OnCBPageChanged(self, event):
		old = event.GetOldSelection()
		new = event.GetSelection()
		sel = self.GetSelection()
		event.Skip()

	def OnCBPageChanging(self, event):
		old = event.GetOldSelection()
		new = event.GetSelection()
		sel = self.GetSelection()
		event.Skip()

	# notebook methods
	def changePage(self, pg, content, name):
		self.DeletePage(pg)
		self.InsertPage(pg, content, name)

	def getPage():
		pass


#---Layer tree creation ---#000000#FFFFFF-------------------------------------------------
class LayerTree(CT.CustomTreeCtrl):
    #	def __init__(self, parent, id, pos, size, style):
    def __init__(self, parent, id=wx.ID_ANY, pos=wx.DefaultPosition,
            size=wx.DefaultSize,
            style=wx.SUNKEN_BORDER,
            ctstyle=CT.TR_HAS_BUTTONS |CT.TR_HAS_VARIABLE_ROW_HEIGHT,
            log=None):
        CT.CustomTreeCtrl.__init__(self, parent, id, pos, size, style,ctstyle)
        self.SetAutoLayout(True)
        
        self.root = "" #ID of layer tree root node
        self.layer = {} #dictionary to index layers in layer tree
        self.node = 0 #index value for layers
        self.optpage = {} # dictionary of notebook option pages for each map layer
        self.layerID = "" # ID of currently selected layer
        self.layername = "" # name off currently selected layer
        
        self.root = self.AddRoot("Map Layers")

        self.Bind(wx.EVT_TREE_ITEM_EXPANDING, self.onExpandNode)
        self.Bind(wx.EVT_TREE_ITEM_COLLAPSED, self.onCollapseNode)
        self.Bind(wx.EVT_TREE_ITEM_ACTIVATED, self.onActivateLayer)
        self.Bind(wx.EVT_TREE_SEL_CHANGED, self.onChangeSel)
        
    def AddLayer(self, nb, disp_idx, type):
        layername = type+':'+str(self.node)
        gm_nb_pg1 = render.Track().GetChbk().GetPage(1)
        print "nb page1 =", gm_nb_pg1
        
        if self.node >0 and self.layerID:
            self.layer[self.node] = self.InsertItem(self.root, self.layerID, type+':'+str(self.node))
        else:
            self.layer[self.node] = self.PrependItem(self.root, type+':'+str(self.node))

        print "page1 =", nb.page1

        if type == 'raster':
            pass
            #self.optpage[layername] = spare.Frame(nb.page1, -1)
##			self.optpage[layername] = rastopt.MyPanel(gm_nb_pg1, -1, style=wx.TAB_TRAVERSAL)
        elif type == 'vector':
            self.optpage[layername] = vectopt.MyPanel(nb.page1, -1, style=wx.TAB_TRAVERSAL)
        elif type == 'command':
            self.optpage[layername] = cmdopt.MyPanel(nb.page1, -1, style=wx.TAB_TRAVERSAL)
##        self.optpage[layername].Show(True)
##        print "optpage1 = ", self.optpage[layername]
        self.node += 1

    def onCollapseNode(self, event):
        print 'group collapsed'
        event.Skip()
        
    def onExpandNode(self, event):
        layerID = event.GetItem()
        print 'group expanded'
        event.Skip()
        
    def onActivateLayer(self, event):
        layername = self.GetItemText(event.GetItem())
        # call a method to make this item display or not display?
        # change associated icon accordingly?
        print layername,'is activated'
        event.Skip()
        
    def onChangeSel(self, event):
        old_layername = ""
        if str(event.GetOldItem()) != str(event.GetItem()):
            old_layername = self.GetItemText(event.GetOldItem())
        new_layername = self.GetItemText(event.GetItem())
        if old_layername:
            self.optpage[old_layername].Show(False)
        self.optpage[new_layername].Show(True)
        event.Skip()

#---Console functions ---#000000#FFFFFF------------------------
class GMConsole(wx.Panel):
	def __init__(self, parent, id=-1, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.TAB_TRAVERSAL|wx.FULL_REPAINT_ON_RESIZE):
		wx.Panel.__init__(self, parent, id, pos, size, style)
		# initialize variables
		self.console_output = ""
		self.console_command = ""
		self.console_run = ""
		self.console_clear = ""
		self.console_save = ""
		self.gcmdlst = [] #list of commands in bin and scripts

		self.console_output = wx.TextCtrl(self, -1, "", style=wx.TE_MULTILINE|wx.TE_READONLY|wx.HSCROLL)
		self.console_command = wx.TextCtrl(self, -1, "", style=wx.HSCROLL|wx.TE_LINEWRAP|wx.TE_PROCESS_ENTER)
		self.console_run = wx.Button(self, -1, _("Run"))
		self.console_run.SetDefault()
		self.console_clear = wx.Button(self, -1, _("Clear"))
		self.console_save = wx.Button(self, -1, _("Save"))
		self.console_output.SetMinSize((100, 100))
		self.console_command.SetMinSize((100, 50))

		self.Bind(wx.EVT_BUTTON, self.runCmd, self.console_run)
		self.Bind(wx.EVT_BUTTON, self.clearHistory, self.console_clear)
		self.Bind(wx.EVT_BUTTON, self.saveHistory, self.console_save)
		self.Bind(wx.EVT_TEXT_ENTER, self.runCmd, self.console_command)

		# console layout
		boxsizer1 = wx.BoxSizer(wx.VERTICAL)
		gridsizer1 = wx.GridSizer(1, 3, 0, 0)
		boxsizer1.Add(self.console_output, 3, wx.EXPAND|wx.ADJUST_MINSIZE, 0)
		boxsizer1.Add(self.console_command, 0, wx.EXPAND|wx.ADJUST_MINSIZE, 0)
		gridsizer1.Add(self.console_run, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)
		gridsizer1.Add(self.console_clear, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)
		gridsizer1.Add(self.console_save, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)
		boxsizer1.Add(gridsizer1, 0, wx.EXPAND, 0)
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

	def runCmd(self, event):
		'''Run in GUI or shell GRASS (or other) commands typed into
		console command text widget, echo command to console
		output text widget, and send stdout output to output
		text widget. Display commands (*.d) are captured and
		processed separately by mapdisp.py. Display commands are
		rendered in map display widget that currently has
		the focus (as indicted by mdidx).'''
		gcmdlst = self.getGRASSCmds()
		cmdlst = []
		cmd = self.console_command.GetLineText(0)
		cmdlst = cmd.split(' ')
		disp_idx = int(render.Track().GetDisp_idx())
		curr_disp = render.Track().GetCurrDisp()

		if len(cmdlst) == 1 and cmd in gcmdlst:
			# Send GRASS command without arguments to GUI command interface
			# except display commands (they are handled differently)
			global gmpath
			if cmd[0:2] == "d.":
				print "Add map layer to GIS Manager to see GUI for display command"
				return
			else:
				menuform.GUI().parseCommand(cmd, gmpath)
				self.console_output.write(cmdlst[0]+"\n----------\n")

		elif cmd[0:2] == "d." and len(cmdlst) > 1 and cmdlst[0] in gcmdlst:
			# Send GRASS display command(s)with arguments
			# to the display processor and echo to command output console.
			# Accepts a list of d.* commands separated by commas.
			# Display with focus receives display command(s).
			self.console_output.write(cmd+"\n----------\n")
			dcmds = cmd.split(',')
			currmap.setDcommandList(dcmds)

		else:
			# Send any other command to the shell. Send output to
			# console output window.
			try:
				retcode = subprocess.call(cmd, shell=True)
				if retcode < 0:
					print >>sys.stderr, "Child was terminated by signal", -retcode
				elif retcode > 0:
					print >>sys.stderr, "Child returned", retcode
			except OSError, e:
				print >>sys.stderr, "Execution failed:", e

			self.console_output.write(cmd+"\n----------\n")
##		  self.out = subprocess.Popen(cmd, shell=True, stdout=Pipe).stdout
			self.out = os.popen(cmd, "r").read()
			self.console_output.write(self.out+"\n")

	def clearHistory(self, event):
		self.console_output.Clear()

	def saveHistory(self, event):
		self.history = self.console_output.GetStringSelection()
		if self.history == "":
			self.console_output.SetSelection(-1,-1)
			self.history = self.console_output.GetStringSelection()
		#could use a standard dialog for this
		output = open("history.txt","w")
		output.write(self.history)
		output.close()

class SetVal:
	'''Class to store and set values needed by map, gism,
	and other modules. This should work but doesn't for some reason.'''

	def setMdFocus(self, mdnum=-1):
		#get the id number of map display that has the focus
		#and use it to set md
		global mdfocus
		if mdnum > -1:
			mdfocus = mdnum
		else:
			return mdfocus

	def getMdFocus(self):
		global mdfocus
		return mdfocus



class GMApp(wx.App):
	def OnInit(self):
##	  reexec_with_pythonw()
		wx.InitAllImageHandlers()
		mainframe = GMFrame(None, -1, "")
		self.SetTopWindow(mainframe)
		mainframe.Show()
		return 1

def reexec_with_pythonw():
	if sys.platform == 'darwin' and\
		not sys.executable.endswith('MacOS/Python'):
		print >>sys.stderr,'re-executing using pythonw'
		os.execvp('pythonw',['pythonw',__file__] + sys.argv[1:])


if __name__ == "__main__":

	reexec_with_pythonw()

	import gettext
	gettext.install("GMApp") # replace with the appropriate catalog name


	app = GMApp(0)
	app.MainLoop()








