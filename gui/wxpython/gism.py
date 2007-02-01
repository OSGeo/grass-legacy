#!/usr/bin/env python
import sys,os, subprocess

gmpath = os.getenv('PYTHONSTARTUP')


sys.path.append(gmpath)

import wx
import mapdisp
import render
import re
import tempfile
import grassgui


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


class MainFrame(wx.Frame):
    '''GIS Manager frame with notebook widget for controlling
    GRASS GIS. Includes command console page for typing GRASS
    (and other) commands, tree widget page for managing GIS map and
    decoration layers, and options page for managing each layer.'''
    def __init__(self, *args, **kwds):
	kwds["style"] = wx.DEFAULT_FRAME_STYLE
	wx.Frame.__init__(self, *args, **kwds)
	self.createToolBar()
	self.createMenuBar()
	self.panel_1 = wx.Panel(self, -1)
	self.notebook_1 = wx.Notebook(self.panel_1, -1, style=wx.NB_RIGHT)
	self.notebook_1_pane_3 = wx.Panel(self.notebook_1, -1, style=wx.TAB_TRAVERSAL|wx.FULL_REPAINT_ON_RESIZE)

	self.notebook_1_pane_2 = wx.Panel(self.notebook_1, -1, style=wx.FULL_REPAINT_ON_RESIZE)
	self.console_output = wx.TextCtrl(self.notebook_1_pane_3, -1, "", style=wx.TE_MULTILINE|wx.TE_READONLY|wx.HSCROLL)
	self.console_command = wx.TextCtrl(self.notebook_1_pane_3, -1, "", style=wx.HSCROLL|wx.TE_LINEWRAP|wx.TE_PROCESS_ENTER)
	self.console_run = wx.Button(self.notebook_1_pane_3, -1, _("Run"))
	self.console_clear = wx.Button(self.notebook_1_pane_3, -1, _("Clear"))
	self.console_save = wx.Button(self.notebook_1_pane_3, -1, _("Save"))

	self.__set_properties()
	self.__do_layout()

	self.MapDisplay = {} #dictionary to index open map displays
	self.LayerTree = {} #dictionary to index a layer tree to accompanying a map display
	self.root = "" #ID of layer tree root node
	self.layer = {} #dictionary to index layers in layer tree
	self.md = 0 #index value for map displays and layer trees
	self.mdfocus = 0 #track which display currently has focus
	self.node = 0 #index value for layers
	self.gcmdlst = [] #list of commands in bin and scripts

	self.Bind(wx.EVT_BUTTON, self.RunCmd, self.console_run)
	self.Bind(wx.EVT_BUTTON, self.ClearHistory, self.console_clear)
	self.Bind(wx.EVT_BUTTON, self.SaveHistory, self.console_save)
	self.Bind(wx.EVT_CLOSE, self.onCloseWindow)
	self.Bind(wx.EVT_TEXT_ENTER, self.RunCmd, self.console_command)

	#start default initial display
	self.NewDisplay()

    def __set_properties(self):
	self.SetTitle(_("GRASS GIS Manager"))
	self.console_output.SetMinSize((300, 300))
	self.console_command.SetMinSize((300, 50))
	self.console_run.SetDefault()

    def __do_layout(self):
	sizer_1 = wx.BoxSizer(wx.HORIZONTAL)
	sizer_3 = wx.BoxSizer(wx.VERTICAL)
	sizer_4 = wx.BoxSizer(wx.HORIZONTAL)
	sizer_5 = wx.BoxSizer(wx.VERTICAL)
	grid_sizer_1 = wx.GridSizer(1, 3, 0, 0)
	sizer_5.Add(self.console_output, 3, wx.EXPAND|wx.ADJUST_MINSIZE, 0)
	sizer_5.Add(self.console_command, 0, wx.EXPAND|wx.ADJUST_MINSIZE, 0)
	grid_sizer_1.Add(self.console_run, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)
	grid_sizer_1.Add(self.console_clear, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)
	grid_sizer_1.Add(self.console_save, 0, wx.ALIGN_CENTER_HORIZONTAL|wx.ADJUST_MINSIZE, 0)
	sizer_5.Add(grid_sizer_1, 0, wx.EXPAND, 0)
	self.notebook_1_pane_3.SetAutoLayout(True)
	self.notebook_1_pane_3.SetSizer(sizer_5)
	sizer_5.Fit(self.notebook_1_pane_3)
	sizer_5.SetSizeHints(self.notebook_1_pane_3)
	self.notebook_1.AddPage(self.notebook_1_pane_2, _("Options"))
	self.notebook_1.AddPage(self.notebook_1_pane_3, _("Console"), select=True)
	sizer_4.Add(self.notebook_1, 1, wx.EXPAND, 0)
	self.panel_1.SetAutoLayout(True)
	self.panel_1.SetSizer(sizer_4)
	sizer_4.Fit(self.panel_1)
	sizer_4.SetSizeHints(self.panel_1)
	sizer_3.Add(self.panel_1, 1, wx.EXPAND, 0)
	self.SetAutoLayout(True)
	self.SetSizer(sizer_3)
	sizer_3.Fit(self)
	sizer_3.SetSizeHints(self)
	self.Layout()
	self.Centre()

#---Menubar creation---#000000#FFFFFF-------------------------------------------
    def menuData(self):
	'''Create data object of parameters to create menu.
	Probably should be changed to XML or *.dtd file.
	Could be reworked to make this a toolbox data object'''
	return	 [(("Files", (
	    ("Import", "Import files", self.runMenuCmd, "r.in.gdal"),
	    ("Export", "Export files", self.runMenuCmd, "r.out.gdal"),
	    ("","","", ""),
	    ("E&xit", "Exit from gism.py", self.onCloseWindow, "")
	    )),
	("Config", (
	    ("Region", "Set region", self.runMenuCmd, "g.region"),
	    ("","","", "")
	    )),
	("Raster", (
	    ("Develop map", (
		("Digitize raster", "Digitize raster", self.runMenuCmd, "r.digit"),
		("","","", ""),
		("Compress/decompress raster file", "Compress/decompress raster file", self.runMenuCmd, "r.compress"),
		("Manage boundary definition (WHICH COMMAND?)", "Manage boundary definition", self.runMenuCmd, "r.region"),
		("Manage null values", "Manage null values", self.runMenuCmd, "r.null"),
		("Manage timestamp for files", "Manage timestamp for files", self.runMenuCmd, "r.timestamp"),
		("Quantization for floating-point maps", "Quantization for floating-point maps", self.runMenuCmd, "r.quant"),
		("Resample (change resolution) using nearest neighbor method", "Resample (change resolution) using nearest neighbor method", self.runMenuCmd, "r.resample"),
		("Resample (change resolution) using regularized spline tension", "Resample (change resolution) using regularized spline tension", self.runMenuCmd, "r.resamp.rst"),
		("Support file creation and maintenance", "Support file creation and maintenance", self.runMenuCmd, "r.support.sh"),
		("","","", ""),
		("Reproject raster from other location", "Reproject raster from other location", self.runMenuCmd, "r.proj"),
		("Generate tiling for other projection", "Generate tiling for other projection", self.runMenuCmd, "r.tileset"),
	    )),
	    ("Manage map colors", (
		("Set colors to predefined color tables", "Set colors to predefined color tables", self.runMenuCmd, "r.colors"),
		("Set colors using color rules", "Set colors using color rules", self.runMenuCmd, "r.colors.rules"),
		("","","", ""),
		("Blend 2 color maps to produce 3 RGB files", "Blend 2 color maps to produce 3 RGB files", self.runMenuCmd, "r.blend"),
		("Create color image from RGB files", "Create color image from RGB files", self.runMenuCmd, "r.composite"),
		("Create 3 RGB (red, green, blue) maps from 3 HIS (hue, intensity, saturation) maps", "Create 3 RGB (red, green, blue) maps from 3 HIS (hue, intensity, saturation) maps", self.runMenuCmd, "r.his"),
	    )),
	    ("Query by coordinates", "Query by coordinates", self.runMenuCmd, "r.what"),
	    ("","","", ""),
	    ("Create raster buffers", "Develop raster buffer", self.runMenuCmd, "r.buffer"),
	    ("Create raster MASK", "Develop raster mask", self.runMenuCmd, "r.mask"),
	    ("Locate closest points between areas in 2 raster maps", "r.distance", self.runMenuCmd, "r.distance"),
	    ("Map calculator", "Map calculator", self.runMenuCmd, "scripts/mapcalc_gparser.sh"),
	    ("Neighborhood analysis", (
		("Moving window analysis of raster cells", "Moving window analysis of raster cells", self.runMenuCmd, "r.neighbors"),
		("Analyze vector points in neighborhood of raster cells", "Analyze vector points in neighborhood of raster cells", self.runMenuCmd, "v.neighbors"),
	    )),
	    ("Overlay maps", (
		("Cross product", "Cross product", self.runMenuCmd, "r.cross"),
		("Function of map series (time series)", "Function of map series (time series)", self.runMenuCmd, "r.series"),
		("Patch maps", "Patch maps", self.runMenuCmd, "r.patch"),
		("","","", ""),
		("Statistical calculations for cover map over base map", "Statistical calculations for cover map over base map", self.runMenuCmd, "r.statistics"),
	    )),
	    ("Solar radiance and shadows", (
		("Solar irradiance and daily irradiation", "Solar irradiance and daily irradiation", self.runMenuCmd, "r.sun"),
		("Shadow map for sun position or date/time", "Shadow map for sun position or date/time", self.runMenuCmd, "r.sunmask"),
	    )),
	    ("Terrain analysis", (
		("Calculate cumulative movement costs between locales", "Calculate cumulative movement costs between locales", self.runMenuCmd, "r.walk"),
		("cost surface", "cost surface", self.runMenuCmd, "r.cost"),
		("Least cost route or flow", "Least cost route or flow", self.runMenuCmd, "r.drain"),
		("Profile analysis", "Profile analysis", self.runMenuCmd, "d.profile"),
		("Shaded relief map", "Shaded relief map", self.runMenuCmd, "r.shaded.relief"),
		("Slope and aspect", "Slope and aspect", self.runMenuCmd, "r.slope.aspect"),
		("Terrain parameters", "Terrain parameters", self.runMenuCmd, "r.param.scale"),
		("Textural features", "Textural features", self.runMenuCmd, "r.texture"),
		("Visibility/Line of sight", "Visibility/Line of sight", self.runMenuCmd, "r.los"),
	    )),
	    ("Transform features", (
		("Clump small areas (statistics calculated by r.volume)", "Clump small areas (statistics calculated by r.volume)", self.runMenuCmd, "r.clump"),
		("Grow areas", "Grow areas", self.runMenuCmd, "r.grow"),
		("Thin linear features", "Thin linear features", self.runMenuCmd, "r.thin"),
	    )),
	    ("","","", ""),
	    ("Hydrologic modeling", (
		("Carve stream channels into elevation map using vector streams map", "Carve stream channels into elevation map using vector streams map", self.runMenuCmd, "r.carve"),
		("Depressionless elevation map and flowline map", "Depressionless elevation map and flowline map", self.runMenuCmd, "r.fill.dir"),
		("Fill lake from seed point to specified level", "Fill lake from seed point to specified level", self.runMenuCmd, "r.lake"),
		("Flow accumulation for massive grids (WHICH COMMAND?)", "Flow accumulation for massive grids", self.runMenuCmd, "r.flow"),
		("Generate flow lines for raster map (WHICH COMMAND?)", "Generate flow lines for raster map", self.runMenuCmd, "r.flow"),
		("SIMWE overland flow modeling (WHICH COMMAND?)", "SIMWE overland flow modeling", self.runMenuCmd, "r.simwe"),
		("SIMWE sediment erosion, transport, deposition modeling (WHICH COMMAND?)", "SIMWE sediment erosion, transport, deposition modeling", self.runMenuCmd, "r.simwe"),
		("Topographic index map", "Topographic index map", self.runMenuCmd, "r.topidx"),
		("TOPMODEL simulation", "TOPMODEL simulation", self.runMenuCmd, "r.topmodel"),
		("Watershed subbasins", "Watershed subbasins", self.runMenuCmd, "r.basins.fill"),
		("Watershed analysis", "Watershed analysis", self.runMenuCmd, "r.watershed"),
		("Watershed basin creation", "Watershed basin creation", self.runMenuCmd, "r.water.outlet"),
	    )),
	    ("Landscape structure modeling", (
		("Set up sampling and analysis framework", "Set up sampling and analysis framework", self.runMenuCmd, "r.le.setup"),
		("","","", ""),
		("Analyze landscape characteristics", "Analyze landscape characteristics", self.runMenuCmd, "r.le.pixel"),
		("Analyze landscape patch characteristics", "Analyze landscape patch characteristics", self.runMenuCmd, "r.le.patch"),
		("Output landscape patch information", "Output landscape patch information", self.runMenuCmd, "r.le.trace"),
	    )),
	    ("Wildfire modeling", (
		("Generate rate of spread (ROS) maps", "Generate rate of spread (ROS) maps", self.runMenuCmd, "r.ros"),
		("Generate least-cost spread paths", "Generate least-cost spread paths", self.runMenuCmd, "r.spreadpath"),
		("Simulate anisotropic spread phenomena", "Simulate anisotropic spread phenomena", self.runMenuCmd, "r.spread"),
	    )),
	    ("","","", ""),
	    ("Change category values and labels", (
		("Edit category values of individual cells for displayed raster map", "Edit category values of individual cells for displayed raster map", self.runMenuCmd, "d.rast.edit"),
		("","","", ""),
		("Reclassify categories for areas of specified sizes", "Reclassify categories for areas of specified sizes", self.runMenuCmd, "r.reclass.area"),
		("Reclassify categories using rules", "Reclassify categories using rules", self.runMenuCmd, "r.reclass.rules"),
		("Reclassify categories using rules file", "Reclassify categories using rules file", self.runMenuCmd, "r.reclass.file"),
		("","","", ""),
		("Recode categories using rules (create new map)", "Recode categories using rules (create new map)", self.runMenuCmd, "r.recode.rules"),
		("Recode categories using rules file (create new map)", "Recode categories using rules file (create new map)", self.runMenuCmd, "r.recode.file"),
		("","","", ""),
		("Rescale categories (create new map)", "Rescale categories (create new map)", self.runMenuCmd, "r.rescale"),
		("Rescale categories with equalized histogram (create new map)", "Rescale categories with equalized histogram (create new map)", self.runMenuCmd, "r.rescale.eq"),
	    )),
	    ("","","", ""),
	    ("Generate concentric circles around points", "Generate concentric circles around points", self.runMenuCmd, "r.circle"),
	    ("Generate random raster cells", (
		("Generate random cells", "Generate random cells", self.runMenuCmd, "r.random.cells"),
		("Generate random cells and vector points from raster map", "Generate random cells and vector points from raster map", self.runMenuCmd, "r.random"),
	    )),
	    ("Generate surfaces", (
		("Generate density surface using moving Gaussian kernel", "Generate density surface using moving Gaussian kernel", self.runMenuCmd, "v.kernel"),
		("Generate fractal surface", "Generate fractal surface", self.runMenuCmd, "r.surf.fractal"),
		("Generate gaussian deviates surface", "Generate gaussian deviates surface", self.runMenuCmd, "r.surf.gauss"),
		("Generate plane", "Generate plane", self.runMenuCmd, "r.plane"),
		("Generate random deviates surface", "Generate random deviates surface", self.runMenuCmd, "r.surf.random"),
		("Generate random surface with spatial dependence", "Generate random surface with spatial dependence", self.runMenuCmd, "r.random.surface"),
	    )),
	    ("Generate vector contour lines", "Generate vector contour lines", self.runMenuCmd, "r.contour"),
	    ("Interpolate surfaces", (
		("Bilinear interpolation from raster points", "Bilinear interpolation from raster points", self.runMenuCmd, "r.bilinear"),
		("Inverse distance weighted interpolation from raster points", "Inverse distance weighted interpolation from raster points", self.runMenuCmd, "r.surf.idw"),
		("Interpolation from raster contour", "Interpolation from raster contour", self.runMenuCmd, "r.surf.contour"),
		("","","", ""),
		("Inverse distance weighted interpolation from vector points", "Inverse distance weighted interpolation from vector points", self.runMenuCmd, "v.surf.idw"),
		("Regularized spline tension interpolation from vector points or contours (WHICH COMMAND ?)", "Regularized spline tension interpolation from vector points or contours", self.runMenuCmd, "v.surf.rst"),
		("","","", ""),
		("Fill NULL cells by interpolation using regularized spline tension", "Fill NULL cells by interpolation using regularized spline tension", self.runMenuCmd, "r.fillnulls"),
	    )),
	    ("","","", ""),
	    ("Report and statistics", (
		("Report basic file information", "Report basic file information", self.runMenuCmd, "r.info"),
		("Report category labels and values", "Report category labels and values", self.runMenuCmd, "r.cats"),
		("","","", ""),
		("General statistics", "General statistics", self.runMenuCmd, "r.stats"),
		("Range of all category values", "Range of all category values", self.runMenuCmd, "r.describe"),
		("Sum all cell category values", "Sum all cell category values", self.runMenuCmd, "r.sum"),
		("Sum area by map and category", "Sum area by map and category", self.runMenuCmd, "r.report"),
		("Summary statistics for clumped cells (work with r.clump)", "Summary statistics for clumped cells (work with r.clump)", self.runMenuCmd, "r.volume"),
		("Total surface area corrected for topography", "Total surface area corrected for topography", self.runMenuCmd, "r.surf.area"),
		("Univariate statistics", "Univariate statistics", self.runMenuCmd, "r.univar"),
		("Univariate statistics (script version)", "Univariate statistics (script version)", self.runMenuCmd, "r.univar.sh"),
		("","","", ""),
		("Sample values along transects", "Sample values along transects", self.runMenuCmd, "r.profile"),
		("Sample values along transects (use azimuth, distance)", "Sample values along transects (use azimuth, distance)", self.runMenuCmd, "r.transect"),
		("","","", ""),
		("Covariance/correlation", "Covariance/correlation", self.runMenuCmd, "r.covar"),
		("Linear regression between 2 maps", "Linear regression between 2 maps", self.runMenuCmd, "r.regression.line"),
		("Mutual category occurrences (coincidence)", "Mutual category occurrences (coincidence)", self.runMenuCmd, "r.coin"),
	    )),
	    ("","","", "")
	    )),
	("Vector", (
	    ("Develop map", (
		("Digitize", "Digitize vector", self.runMenuCmd, "v.digit"),
		("","","", ""),
		("Create/Rebuild topology", "Create/Rebuild topology", self.runMenuCmd, "v.build"),
		("Clean vector files", "clean vector files", self.runMenuCmd, "v.clean"),
		("","","", ""),
		("Break lines at intersections", "Break lines at intersections", self.runMenuCmd, "v.topo.check"),
		("Build polylines from adjacent segments", "Build polylines from adjacent segments", self.runMenuCmd, "v.build.polylines"),
		("Split polylines into segments", "Split polylines into segments", self.runMenuCmd, "v.segment"),
		("Create lines parallel to existing lines", "Create lines parallel to existing lines", self.runMenuCmd, "v.parallel"),
		("","","", ""),
		("Convert vector feature types", "Convert vector feature types", self.runMenuCmd, "v.type"),
		("Convert 2D vector to 3D by sampling raster", "Convert 2D vector to 3D by sampling raster", self.runMenuCmd, "v.drape"),
		("Extrude 2D vector into 3D vector", "Extrude 2D vector into 3D vector", self.runMenuCmd, "v.extrude"),
		("","","", ""),
		("Create text label file for vector features", "Create text label file for vector features", self.runMenuCmd, "v.label"),
		("","","", ""),
		("Reproject vector from other location", "Reproject vector from other location", self.runMenuCmd, "v.proj"),
		("","","", "")
	    )),
	    ("","","", ""),
	    ("vector<->database connections", (
		("Create new vector as link to external OGR layer", "Create new vector as link to external OGR layer", self.runMenuCmd, "v.external"),
		("Set database connection for vector attributes", "Set database connection for vector attributes", self.runMenuCmd, "v.db.connect"),
	    )),
	    ("Query by attributes", "Query by attributes", self.runMenuCmd, "v.extract"),
	    ("Query by coordinate(s)", "Query by coordinate(s)", self.runMenuCmd, "v.what"),
	    ("Query by map features", "Query by map features", self.runMenuCmd, "v.select"),
	    ("","","", ""),
	    ("Create vector buffers", "Create vector buffers", self.runMenuCmd, "v.buffer"),
	    ("Linear referencing for vectors", (
		("Create linear reference system", "Create linear reference system", self.runMenuCmd, "v.lrs.create"),
		("Create stationing from input lines, and linear reference system", "Create stationing from input lines, and linear reference system", self.runMenuCmd, "v.lrs.label"),
		("Create points/segments from input lines, linear reference system and positions read from stdin", "Create points/segments from input lines, linear reference system and positions read from stdin", self.runMenuCmd, "v.lrs.segment"),
		("Find line id and real km+offset for given points in vector map using linear reference system", "Find line id and real km+offset for given points in vector map using linear reference system", self.runMenuCmd, "v.lrs.where"),
	    )),
	    ("Neighborhood analysis", (
		("Locate nearest feature to points or centroids", "Locate nearest feature to points or centroids", self.runMenuCmd, "v.distance"),
		("Generate Thiessen polygons around points (Voronoi diagram)", "Generate Thiessen polygons around points (Voronoi diagram)", self.runMenuCmd, "v.voronoi"),
		("Connect points to create Delaunay triangles", "Connect points to create Delaunay triangles", self.runMenuCmd, "v.delaunay"),
	    )),
	    ("Network analysis", (
		("Allocate subnets", "Allocate subnets", self.runMenuCmd, "v.net.alloc"),
		("Network maintenance", "Network maintenance", self.runMenuCmd, "v.net"),
		("Shortest route", "Shortest route", self.runMenuCmd, "v.net.path"),
		("Shortest route (visualization only)", "Shortest route (visualization only)", self.runMenuCmd, "d.path"),
		("Split net to bands between cost isolines", "Split net to bands between cost isolines", self.runMenuCmd, "v.net.iso"),
		("Steiner tree", "Steiner tree", self.runMenuCmd, "v.net.steiner"),
		("Traveling salesman analysis", "Traveling salesman analysis", self.runMenuCmd, "v.net.salesman"),
	    )),
	    ("Overlay maps", (
		("Overlay/combine 2 vector maps", "Overlay/combine 2 vector maps", self.runMenuCmd, "v.overlay"),
		("Patch multiple maps (combine)", "Patch multiple maps (combine)", self.runMenuCmd, "v.patch"),
	    )),
	    ("Generate area feature for extent of current region", "Generate area feature for extent of current region", self.runMenuCmd, "v.in.region"),
	    ("Generate rectangular vector grid", "Generate rectangular vector grid", self.runMenuCmd, "v.mkgrid"),
	    ("","","", ""),
	    ("Change attributes", (
		("Attach/delete, or report categories", "Attach/delete, or report categories", self.runMenuCmd, "v.category"),
		("Reclassify features using rules file", "Reclassify features using rules file", self.runMenuCmd, "v.reclass"),
	    )),
	    ("","","", ""),
	    ("Work with vector points", (
		("Generate points", (
		    ("Generate points from database", "Generate points from database", self.runMenuCmd, "v.in.db"),
		    ("Generate random points", "Generate random points", self.runMenuCmd, "v.random"),
		    ("Random location perturbations of points", "Random location perturbations of points", self.runMenuCmd, "v.perturb"),
		)),
		("Generate areas from points", (
		    ("Generate convex hull for point set", "Generate convex hull for point set", self.runMenuCmd, "v.hull"),
		    ("Generate Delaunay triangles for point set", "Generate Delaunay triangles for point set", self.runMenuCmd, "v.delaunay"),
		    ("Generate Voronoi diagram/Thiessen polygons for point set", "Generate Voronoi diagram/Thiessen polygons for point set", self.runMenuCmd, "v.voronoi"),
		)),
		("Sample raster maps", (
		    ("Calculate statistics for raster map overlain by vector map", "Calculate statistics for raster map overlain by vector map", self.runMenuCmd, "v.rast.stats"),
		    ("Sample raster maps at point locations", "Sample raster maps at point locations", self.runMenuCmd, "v.what.rast"),
		    ("Sample raster neighborhood around points", "Sample raster neighborhood around points", self.runMenuCmd, "v.sample"),
		)),
		("Partition points into test/training sets for k-fold cross validation", "Partition points into test/training sets for k-fold cross validation", self.runMenuCmd, "v.kcv"),
		("Transfer attribute data from queried vector map to points", "Transfer attribute data from queried vector map to points", self.runMenuCmd, "v.what.vect"),
	    )),
	    ("","","", ""),
	    ("Reports and statistics", (
		("Basic information", "Basic information", self.runMenuCmd, "v.info"),
		("Load vector attributes to database or create reports", "Load vector attributes to database or create reports", self.runMenuCmd, "v.to.db"),
		("Report areas for vector attribute categories", "Report areas for vector attribute categories", self.runMenuCmd, "v.report"),
		("Univariate statistics", "Univariate statistics", self.runMenuCmd, "v.univar"),
		("","","", ""),
		("Test normality of point distribution", "Test normality of point distribution", self.runMenuCmd, "v.normal"),
		("Calculate stats for raster map underlying vector objects", "Calculate stats for raster map underlying vector objects", self.runMenuCmd, "v.rast.stats"),
		("Indices of point counts in quadrats", "Indices of point counts in quadrats", self.runMenuCmd, "v.qcount"),
	    )),
	    ("","","", "")
	    )),
	("Image", (
	    ("Develop images and groups", (
		("Create/edit imagery group", "Create/edit imagery group", self.runMenuCmd, "i.group"),
		("Target imagery group", "Target imagery group", self.runMenuCmd, "i.target"),
		("","","", ""),
		("Mosaic up to 4 adjacent images", "Mosaic up to 4 adjacent images", self.runMenuCmd, "i.image.mosaic"),
		)),
	    ("Manage image colors", (
		("Color balance and enhance color tables of multiband imagery for rgb display", "Color balance and enhance color tables of multiband imagery for rgb display", self.runMenuCmd, "i.landsat.rgb"),
		("Transform HIS (Hue/Intensity/Saturation) color image to RGB (Red/Green/Blue)", "Transform HIS (Hue/Intensity/Saturation) color image to RGB (Red/Green/Blue)", self.runMenuCmd, "i.his.rgb"),
		("Transform RGB (Red/Green/Blue) color image to HIS (Hue/Intensity/Saturation)", "Transform RGB (Red/Green/Blue) color image to HIS (Hue/Intensity/Saturation)", self.runMenuCmd, "i.rgb.his"),
		)),
	    ("Rectify and georeference image group", (
		("Set ground control points (GCP's) from raster map or keyboard entry", "Set ground control points (GCP's) from raster map or keyboard entry", self.runMenuCmd, "i.points"),
		("Set ground control points (GCP's) from vector map or keyboard entry", "Set ground control points (GCP's) from vector map or keyboard entry", self.runMenuCmd, "i.vpoints"),
		("Affine and Polynomial rectification (rubber sheet)", "Affine and Polynomial rectification (rubber sheet)", self.runMenuCmd, "i.rectify"),
		("Ortho Photo rectification", "Ortho Photo rectification", self.runMenuCmd, "i.ortho.photo"),
		)),
	    ("","","", ""),
	    ("Brovey transformation and pan sharpening", "Brovey transformation and pan sharpening", self.runMenuCmd, "i.fusion.brovey"),
	    ("Classify image", (
		("Clustering input for unsupervised classification", "Clustering input for unsupervised classification", self.runMenuCmd, "i.cluster"),
		("","","", ""),
		("Maximum likelihood Classification (MLC)", "Maximum likelihood Classification (MLC)", self.runMenuCmd, "i.maxlik"),
		("Sequential maximum a posteriori classification (SMAP)", "Sequential maximum a posteriori classification (SMAP)", self.runMenuCmd, "i.smap"),
		("","","", ""),
		("Interactive input for supervised classification", "Interactive input for supervised classification", self.runMenuCmd, "i.class"),
		("Non-interactive input for supervised classification (MLC)", "Non-interactive input for supervised classification (MLC)", self.runMenuCmd, "i.gensig"),
		("Non-interactive input for supervised classification (SMAP)", "Non-interactive input for supervised classification (SMAP)", self.runMenuCmd, "i.gensigset"),
		)),
	    ("Filter image", (
		("Zero edge crossing detection", "Zero edge crossing detection", self.runMenuCmd, "i.zc"),
		("User defined matrix/convolving filter", "User defined matrix/convolving filter", self.runMenuCmd, "r.mfilter"),
		)),
	    ("Spectral response", "Spectral response", self.runMenuCmd, "i.spectral"),
	    ("Tasseled cap vegetation index", "Tasseled cap vegetation index", self.runMenuCmd, "i.tasscap"),
	    ("Transform image", (
		("Canonical component", "Canonical component", self.runMenuCmd, "i.cca"),
		("Principal component", "Principal component", self.runMenuCmd, "i.pca"),
		("Fast Fourier Transform", "Fast Fourier Transform", self.runMenuCmd, "i.fft"),
		("Inverse Fast Fourier Transform", "Inverse Fast Fourier Transform", self.runMenuCmd, "i.ifft"),
		)),
	    ("","","", ""),
	    ("Report and statistics", (
		("Report basic file information", "Report basic file information", self.runMenuCmd, "r.info"),
		("Range of image values", "Range of image values", self.runMenuCmd, "r.describe"),
		("","","", ""),
		("Bit pattern comparison for ID of low quality pixels", "Bit pattern comparison for ID of low quality pixels", self.runMenuCmd, "r.bitpattern"),
		("Kappa classification accuracy assessment", "Kappa classification accuracy assessment", self.runMenuCmd, "r.kappa"),
		("Optimum index factor for LandSat TM", "Optimum index factor for LandSat TM", self.runMenuCmd, "i.oif"),
		)),
	    ("","","", "")
	    )),
	("Database", (
	    ("Query", "Query database", self.Nomethod, ""),
	    ("","","", "")
	    )))]

    def createMenuBar(self):
	menuBar = wx.MenuBar()
	for eachMenuData in self.menuData():
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
	self.Bind(wx.EVT_MENU, handler, menuItem)

    def runMenuCmd(self, event):
	'''Run menu command'''
	menuitem = self.GetMenuBar().FindItemById(event.GetId())
	itemtext = menuitem.GetText()
	cmd = menucmd[itemtext]
	global gmpath
	grassgui.GUI().parseCommand(cmd, gmpath)

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
	iconpath = os.environ['GRASS_ICONPATH']
	return (
	    ("newdisplay", iconpath+r'/gui-startmon.gif', "Start new display", self.NewDisplay),
	    ("", "", "", ""),
	    ("addraster", iconpath+r'/element-cell.gif', "Add raster layer", self.AddRaster),
	    ("addvect", iconpath+r'/element-vector.gif', "Add vector layer", self.AddVector)
	    )


#---Start display---#000000#FFFFFF----------------------------------------------
    def NewDisplay(self, event=None):
	'''Create new map display widget'''
	#update display index
	self.md += 1

	#start a new display, indexed by md
	self.MapDisplay[self.md] = mapdisp.MyFrame(self)
	self.MapDisplay[self.md].SetTitle(_("Map Display-"+str(self.md)))
	self.MapDisplay[self.md].Show()
	self.CreateTree(self.md)


#---Layer tree creation ---#000000#FFFFFF-------------------------------------------------
    def CreateTree(self, idx):
	'''Create a tree indexed by md, create bindings, and
	set root node of tree'''
	#need to figure out how to show/hide trees attached to each display
	#probably should move tree(s) to separate module or at least class
	self.LayerTree[idx] = wx.TreeCtrl(self.notebook_1, -1, style=wx.TR_HAS_BUTTONS|wx.TR_LINES_AT_ROOT|wx.TR_EDIT_LABELS|wx.TR_HIDE_ROOT|wx.TR_DEFAULT_STYLE|wx.SUNKEN_BORDER|wx.NO_BORDER|wx.FULL_REPAINT_ON_RESIZE)
	self.LayerTree[idx].SetAutoLayout(True)
	#how to change tree displayed on page?
	self.notebook_1.InsertPage(0, self.LayerTree[idx], "Layers")
	self.Bind(wx.EVT_TREE_ITEM_EXPANDING, self.onExpandNode, self.LayerTree[self.md])
	self.Bind(wx.EVT_TREE_ITEM_COLLAPSED, self.onCollapseNode, self.LayerTree[self.md])
	self.Bind(wx.EVT_TREE_ITEM_ACTIVATED, self.onActivateLayer, self.LayerTree[self.md])
	self.Bind(wx.EVT_TREE_SEL_CHANGED, self.onChangeSel, self.LayerTree[self.md])
	self.root = self.LayerTree[idx].AddRoot("wx.Object")

    def AddLayer(self, idx, type):
	self.layer[self.node] =	 self.LayerTree[idx].AppendItem(self.root, type+str(self.node))
	self.node += 1

    def onCollapseNode(self, event):
	print 'group collapsed'
	event.Skip()

    def onExpandNode(self, event):
	groupID = event.GetItem()
	print 'group expanded'
	event.Skip()

    def onActivateLayer(self, event):
	layername = self.LayerTree[self.md].GetItemText(event.GetItem())
	# call a method to make this item display or not display?
	# change associated icon accordingly?
	print layername,'is activated'
	event.Skip()

    def onChangeSel(self, event):
	layername = self.LayerTree[self.md].GetItemText(event.GetItem())
	print layername,'is selected'
	# call method to check the name/type and change options panel
	event.Skip()

#---ToolBar button handlers---#000000#FFFFFF------------------------------------
    def AddRaster(self, event):
	self.AddLayer(self.md, "raster")
	event.Skip()

    def AddVector(self, event):
	self.AddLayer(self.md, "vector")
	event.Skip()

#---Console functions ---#000000#FFFFFF------------------------
    def getGRASSCmds(self):
	'''Create list of all available GRASS commands'''
	gisbase = os.environ['GISBASE']
	self.gcmdlst = os.listdir(gisbase+r'/bin')
	self.gcmdlst.append(os.listdir(gisbase+r'/scripts'))
	return self.gcmdlst

    def RunCmd(self, event):
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
	mdidx = int(render.Render().getMdIdx())
	print 'in gism'
	render.Render().setMD(self.MapDisplay[mdidx])

	if len(cmdlst) == 1 and cmd in gcmdlst:
	    # Send GRASS command without arguments to GUI command interface
	    global gmpath
	    grassgui.GUI().parseCommand(cmd, gmpath)
	    self.console_output.write(cmdlst[0]+"\n----------\n")

	elif cmd[0:2] == "d." and len(cmdlst) > 1 and cmdlst[0] in gcmdlst:
	    # Send GRASS display command(s)with arguments
	    # to the display processor and echo to command output console.
	    # Accepts a list of d.* commands separated by commas.
	    # Display with focus receives display command(s).
	    self.console_output.write(cmd+"\n----------\n")
	    dcmds = cmd.split(',')
	    self.MapDisplay[mdidx].SetDcommandList(dcmds)

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

    def ClearHistory(self, event):
	self.console_output.Clear()

    def SaveHistory(self, event):
	self.history = self.console_output.GetStringSelection()
	if self.history == "":
	    self.console_output.SetSelection(-1,-1)
	    self.history = self.console_output.GetStringSelection()
	#could use a standard dialog for this
	output = open("history.txt","w")
	output.write(self.history)
	output.close()

#---Misc methods---#000000#FFFFFF-----------------------------------------------
    def onCloseWindow(self, event):
	'''Cleanup when gism.py is quit'''
	mdlist = range(0, self.md+1)
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
	print 'self.md is now', self.md

# end of class MainFrame

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
	mainframe = MainFrame(None, -1, "")
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

    
    if not os.getenv("GRASS_ICONPATH"):
        os.environ["GRASS_ICONPATH"]=os.getenv("GISBASE")+"/etc/gui/icons/"

    app = GMApp(0)
    app.MainLoop()




