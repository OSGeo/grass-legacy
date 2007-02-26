"""
class Layer
class Map
"""

import os,sys
import utils

layertree = {} #layer tree in GIS Manager, indexed by display.
nb = {} #notebook in GIS Manager, indexed by display.
cb_page = {} #choicbook page in GIS Manager, indexed by display.
cb_pgnum = {} #choicebook page number, indexed by page ID
curr_disp = {} #display ID indexed by display index number
disp_ctrl = {} #distionary of associated choicebook pages and displays
disp_idx = "" #index for each display

# Authors: Michael Barton and Jachym Cepicky
# COPYRIGHT:	(C) 1999 - 2007 by the GRASS Development Team

DEBUG = False

class Layer:
	"""
	This class serves for storing map layers to be displayed
	
	Common layer attributes:
		name	- layer name
		mapset	- mapset name

		l_type     - layer type
		l_active   - layer is active, will be rendered only if True
		l_hidden   - layer is hidden, won't be listed in GIS Manager if True
		l_opacity  - layer opacity [0-1]
		l_mapfile  - file name of rendered layer
		l_maskfile - mask name of rendered layer
	"""

	def __renderRasterLayer(self):
		"""
		Stores d.rast command with all parameters in the self.cmd variable
		"""

		try:
			self.cmd = "d.rast -o map=%s@%s" % (self.name, self.mapset)

			if self.catlist:    self.cmd += " catlist=%s" % (self.catlist)
			if self.vallist:    self.cmd += " vallist=%s" % (self.vallist)
			if self.invertCats: self.cmd +=" -i"
		except StandardError, e:
			sys.stderr.write("Could not render raster layer <%s>: %s\n" %\
						 (self.name, str(e)))
			self.cmd = None

	def __renderVectorLayer(self):
		"""
		Stores d.vect command with all parameters in the self.cmd variable
		"""

		try:
			self.cmd = "d.vect"

			if self.colorfromtable: self.cmd += " -a"
			if self.randomcolor:    self.cmd += " -c"
			if self.catsasid:	self.cmd += " -i"

			self.cmd += " map=%s@%s" % (self.name, self.mapset)

			if self.type:	     self.cmd += " type=%s" % (self.type)
			if self.display:     self.cmd += " display=%s" % (self.display)
			if self.attrcol:     self.cmd += " attrcol=%s" % (self.attrcol)
			if self.icon:        self.cmd += " icon=%s" % (self.icon)
			if self.size:        self.cmd += " size=%s" % (self.size)
			if self.layer:	     self.cmd += " layer=%s" % (self.layer)
			if self.cats:        self.cmd += " cats=%s" % (self.cats)
			if self.where:       self.cmd += " where=%s" % (self.where)
			if self.width:       self.cmd += " width=%s" % (self.width)
			if self.wcolumn:     self.cmd += " wcolumn=%s" % (self.wcolumn)
			if self.wscale:	     self.cmd += " wscale=%s" % (self.wscale)
			if self.color:       self.cmd += " color=%s" % (self.color)
			if self.fcolor:      self.cmd += " fcolor=%s" % (self.fcolor)
			if self.rgb_column : self.cmd += " rgb_column=%s" % (self.rgb_column)
			if self.llayer:      self.cmd += " llayer=%s" % (self.llayer)
			if self.lcolor:      self.cmd += " lcolor=%s" % (self.lcolor)
			if self.bgcolor:     self.cmd += " bgcolor=%s" % (self.bgcolor)
			if self.lsize:       self.cmd += " lsize=%s" % (self.lsize)
			if self.font:        self.cmd += " font=%s" % (self.font)
			if self.xref:        self.cmd += " xref=%s" % (self.xref)
			if self.yref:        self.cmd += " yref=%s" % (self.yref)
			if self.minreg:      self.cmd += " minreg=%s" % (self.minreg)
			if self.maxreg:      self.cmd += " maxreg=%s" % (self.maxreg)
		except StandardError, e:
			sys.stderr.write("Could not render vector layer <%s>: %s\n" %\
						 (self.name, str(e)))

	def Render(self):
		"""
		Runs all d.* commands.

		Returns:
			Name of file with rendered image or None
		"""

		#
		# create command variable
		#
		self.cmd = ""

		#
		# to be sure, set temporary file with layer and mask
		#
		if not self.l_mapfile:
			gtemp = utils.GetTempfile()
			self.l_maskfile = gtemp + ".pgm"
			self.l_mapfile  = gtemp + ".ppm"

		#
		# prepare command for each layer
		#
		if self.l_type == "raster":
			self.__renderRasterLayer()

		elif self.l_type == "vector":
			self.__renderVectorLayer()

		elif self.l_type == "wms":
			print "Type wms is not supported yet"
		else:
			print "Type <%s> of layer <%s> is not supported yet" % \
			    (self.l_type, self.name)

		#
		# Start monitor
		#
		os.environ["GRASS_PNGFILE"] = self.l_mapfile

		if os.system("d.mon --quiet start=gism"):
			# try again
			# os.system("d.mon --quiet stop=gism")
			# if os.system("d.mon --quiet start=gism"):
                        raise CouldNotStartMonitor ("gism")

		os.unsetenv("GRASS_PNGFILE")

		#
		# execute command
		#
		if not self.cmd:
			sys.stderr.write("Could not render layer <%s> with command: #%s#" %\
						 (self.name, self.cmd))
			return None

		if os.system(self.cmd):
			print "Could not execute '%s'" % (self.cmd)
			self.l_mapfile = None
			self.l_maskfile = None
			return None

		#
		# Stop monitor
		#
		if os.system("d.mon --quiet stop=gism"):
                        raise CouldNotStopMonitor("gism")
			
		return self.l_mapfile

class Map:
	"""
	This class serves for rendering of output images.

	Attributes:
	        env       - for some environment variables
		verbosity - verbosity level
		width     - width of the map in pixels
		height    - height of the map in pixels
		windfile  - content of WIND file for temporary region
			settings
			n-s resol: 30;
			e-w resol: 30;
			...
		region - g.region -gp output
			"n":1000,
			"s":0,
			"e":1000,
			"w":0,
			...
		layers - list of all available layers
		renderRegion - dictionary:
			"color" : "RRR:GGG:BBB",
			"width" : 3,
			"render": True/False
	"""

	def __init__(self):
		"""
		While initalization, necessary variables are set, monitor size is
		determinated and
		"""

		self.WIND      = {}  # WIND settings
		self.Region    = {}  # region settings
		self.Width     = 300 # map width 
		self.Height    = 400 # map height

		self.Layers    = []  # stack of available layer
		self.Env       = {}  # enviroment variables, like MAPSET, LOCATION_NAME, etc.
		self.Verbosity = 0
		self.MapFile   = utils.GetTempfile() + ".png"

		self.RenderRegion = {
			"render" : True,     # should the region be displayed?
			"color"	 :"255:0:0",
			"width"	 : 3
			}

		# setting some initial env. variables
		self.__initEnv()
		self.__initRegion()
		os.environ["GRASS_TRANSPARENT"]     = "TRUE"
		os.environ["GRASS_BACKGROUNDCOLOR"] = "ffffff"
		os.environ["GRASS_HEIGHT"]          = str(self.Height)
		os.environ["GRASS_WIDTH"]           = str(self.Width)
		os.environ["GRASS_MESSAGE_FORMAT"]  = "gui"
		os.environ["GRASS_PNG_AUTO_WRITE"]  = "TRUE"
		os.environ["GRASS_TRUECOLOR"]       = "TRUE"
		os.environ["WIND_OVERRIDE"]         = "TRUE"
		os.environ["GRASS_COMPRESSION"]     = "0"
		os.environ["GRASS_VERBOSE"]         = str(self.Verbosity)

	def __initRegion(self):
		"""
		Reads current region settings from g.region command
		"""

		#
		# setting region
		#
		self.Region = self.GetRegion()

		#
		# setting WIND
		#
		# FIXME: duplicated region WIND == g.region (at least some values)
		# FIXME: cannot open WIND file -> raise exception
		windfile = os.path.join (self.Env['GISDBASE'],
					 self.Env['LOCATION_NAME'],
					 self.Env['MAPSET'],
					 "WIND")

		windfile = open (windfile, "r")

		for line in windfile.readlines():
			line = line.strip()
			key, value = line.split(":")
			key = key.strip()
			value = value.strip()
			self.WIND[key] = value

		windfile.close()

		self.__adjustRegion()

		#
		# setting resolution
		#
		# self.Region['ewres'] = self.Region['nsres'] = abs(float(self.Region['e']) 
		# - float(self.Region['w']))/self.Width

	def __initMonSize(self):
		"""
		Reads current GRASS monitor dimensions from env or
		use the default values [640x480]
		"""
		
		try:
			self.Width = int (os.getenv("GRASS_WIDTH"))
		except:
			self.Width = 640
			
		try:
			self.Height = int(os.getenv("GRASS_HEIGHT"))

		except:
			self.Height = 480
			
			
	def __initEnv(self):
		"""
		Stores environment variables to self.Env variable
		"""
		
		if not os.getenv("GISBASE"):
			sys.stderr.write("GISBASE not set, you must be "
					 "in GRASS GIS to run this program\n")
			sys.exit(1)
			
		#os.system("d.mon --quiet stop=gism")
			
		for line in os.popen("g.gisenv").readlines():
			line = line.strip()
			key, val = line.split("=")
			val = val.replace(";","")
			val = val.replace("'","")
			self.Env[key] = val

	def __adjustRegion(self):
		"""
		Adjust region according to monitor size
                """

		# adjusting region to monitor size
		if self.Width > self.Height and \
			self.Region["w"] - self.Region["e"] > \
			self.Region['n'] - self.Region['s']:

			# changing e-w region
			self.Region["ewres"] = self.Region["nsres"] = \
			    (self.Region['n'] - self.Region['s']) / self.Height

			center = self.Region['w'] + \
			    (self.Region['e'] - self.Region['w']) / 2

			self.Region['w'] = center - self.Width / 2 * \
			    self.Region["nsres"]

			self.Region['e'] = center + self.Width / 2 * \
			    self.Region["nsres"]

			self.Region['rows'] = self.Width
			self.Region['cols'] = self.Height

		else:
			# changing n-s region
			self.Region["ewres"] = self.Region["nsres"] = \
			    (self.Region['e'] - self.Region['w']) / self.Width

			center = self.Region['s'] + \
			    (self.Region['n'] - self.Region['s']) / 2

			self.Region['s'] = center - self.Height / 2 * \
			    self.Region["ewres"]

			self.Region['n'] = center + self.Height / 2 * \
			    self.Region["ewres"]

			self.Region['rows'] = self.Width
			self.Region['cols'] = self.Height


	def GetRegion(self):
		"""
		Returns dictionary with output from g.region -gp

		Example:
			{"n":"4928010", "s":"4913700", "w":"589980",...}
		"""

		region = {}

		tmpreg = os.getenv("GRASS_REGION")
		os.unsetenv("GRASS_REGION")

		for reg in os.popen("g.region -gp").readlines():
			reg = reg.strip()
			key, val = reg.split("=")
			region[key] = float(val)

		if tmpreg:
			os.environ["GRASS_REGION"] = tmpreg

		return region

	def SetRegion(self):
		"""
		Render string for GRASS_REGION env. variable, so that the images will be rendred
		from desired zoom level.

		Returns:
			string usable for GRASS_REGION variable or None
		"""

		grass_region = ""

		self.__adjustRegion()

		try:
			for key in self.WIND.keys():
				if key == 'north':
					grass_region += "north: %s; " % \
					    (self.Region['n'])
					continue
				elif key == "south":
					grass_region += "south: %s; " % \
					    (self.Region['s'])
					continue
				elif key == "east":
					grass_region += "east: %s; " % \
					    (self.Region['e'])
					continue
				elif key == "west":
					grass_region += "west: %s; " % \
					    (self.Region['w'])
					continue
				elif key == "e-w resol":
					grass_region += "e-w resol: %f; " % \
					    (self.Region['ewres'])
					continue
				elif key == "n-s resol":
					grass_region += "n-s resol: %f; " % \
					    (self.Region['nsres'])
					continue
				elif key == "cols":
					grass_region += 'cols: %d; ' % \
					    (self.Width)
					continue
				elif key == "rows":
					grass_region += 'rows: %d; ' % \
					    (self.Height)
					continue
				else:
					grass_region += key + ": "  + self.WIND[key] + "; "

			return grass_region

		except:
			return None

	def GetListOfLayers(self, l_type=None, l_active=None, l_hidden=None):
		"""
		Returns list of layers of selected type or list of all layers. It
		is also possible to get list of active or hidden layers.

		Parameters:
			l_type	 - layer type. raster/vector/wms/...
			l_active - only layers with "active" attribute set to True or False
			l_hidden - only layers with "hidden" attribute set to True or False

		Returns:
			List of selected layers or None
		"""

		selected = []

		# ["raster", "vector", "wms", ... ]

		for layer in self.Layers:

			# specified type only
			if l_type != None and layer.l_type != l_type:
				continue

			# hidden and active layers
			if l_active != None and \
				    l_hidden != None:
				if layer.l_active == l_active and \
					    layer.l_hidden == l_hidden:
					selected.append(layer)

			# active layers
			elif l_active != None:
				if layer.l_active == l_active:
					selected.append(layer)

			# hidden layers
			elif l_hidden != None:
				if layer.l_hidden == l_hidden:
					selected.append(layer)

			# all layers
			else:
				selected.append(layer)

		if len(selected) > 0:
			return selected
		else:
			return None

	def Render(self, force=False):
		"""
		Creates final image composite

		NOTE: This function should be done by high-level tools, which
		should be avaliable in wxPython library

		Returns:
			Name of file with rendered image or None
		"""

		## FIXME: not needed?
		maps = []
		masks =[]
		opacities = []

		tmp_region = os.getenv("GRASS_REGION")
		os.environ["GRASS_REGION"] = self.SetRegion()
		os.environ["GRASS_WIDTH"]  = str(self.Width)
		os.environ["GRASS_HEIGHT"] = str(self.Height)

		if DEBUG:
			print ("mapimg.py: Map: Render: force=%s" % (force))
		try:
			for layer in self.Layers:

				# render if there is no mapfile
				if layer.l_mapfile == None:
					layer.Render()

				# redraw layer content
				if force:
					if not layer.Render():
						continue

				# add image to compositing list
				maps.append(layer.l_mapfile)
				masks.append(layer.l_maskfile)
				opacities.append(str(layer.l_opacity))

			# make arrays to strings
			mapstr = ",".join(maps)
			maskstr = ",".join(masks)
			opacstr = repr(",".join(opacities))

			# compose command
			compcmd = "g.pnmcomp in=" + mapstr + \
			    " mask=" + maskstr + \
			    " opacity=" + opacstr + \
			    " background=255:255:255" + \
			    " width=" + str(self.Width) + \
			    " height=" + str(self.Height) + \
			    " output=" + self.MapFile

			# run g.composite to get composite image
			if os.system(compcmd):
				sys.stderr.write("Could not run g.pnmcomp\n")
				raise Exception (compcmd)

			os.unsetenv("GRASS_REGION")

			if tmp_region:
				os.environ["GRASS_REGION"] = tmp_region

			return self.MapFile
		except Exception, e:
			os.unsetenv("GRASS_REGION")

			if tmp_region:
				os.environ["GRASS_REGION"] = tmp_region
			return None

	def AddRasterLayer(self, name, mapset=None, catlist=None,
		vallist=None, invertCats=False, l_active=True, l_hidden=False,
		l_opacity=1, l_render=False):
		"""
		Adds raster layer to list of layers

		Layer Attributes:
			name	   - raster layer name
			mapset	   - mapset name, default: current
			catlist    - list of categories
			vallist    - list of values
			invertCats - invert catlist, True/False

			l_active   - see Layer class
			l_hidden
			l_opacity
			l_render   - render an image

		Returns:
                    Added layer on success or None

		"""

		if not mapset:
			mapset = self.Env["MAPSET"]

                layer = Layer()

		## FIXME: l_opacity must <0;1>
		if l_opacity > 1:
			l_opacity = float(l_opacity) / 100

		# FIXME: create Layer.__init__()
		layer.l_type	= "raster"
		layer.name	= name
		layer.mapset	= mapset
		layer.catlist	= catlist
		layer.vallist	= vallist
		layer.invertCats= invertCats
		layer.l_active	= l_active
		layer.l_hidden	= l_hidden
		layer.l_opacity = l_opacity
		gtemp = utils.GetTempfile()
		layer.l_maskfile = gtemp + ".pgm"
		layer.l_mapfile = gtemp + ".ppm"

		self.Layers.append(layer)

		if l_render:
			if not layer.Render():
				sys.stderr.write("Could not render layer <%s@%s>\n" % \
							 (name,mapset))
				
		return self.Layers[-1]

	def AddGraphLayer(self, name, graph=None, color="255:0:0",
		coordsinmapunits=False,
		l_active=True, l_hidden=True, l_opacity=1, l_render=False):
            """
            Adds graph layer to list of layers (for d.graph definition)

            Layer attributes:
                    name             - graphics name
                    graph            - graphics definition (string)
                    color            - color triplet

                    coordsinmapunits - coordinates are given in map units

                    l_active         - see Layer class
                    l_hidden 
                    l_opacity
                    l_render         - render an image 

            Returns:
                    Added layer on success or None
            """

            layer = Layer()

            if l_opacity > 1:
                    l_opacity = float(l_opacity) / 100

            layer.l_type	   = "graph"
            layer.name		   = name
            layer.graph            = graph
            layer.color            = color
            layer.coordsinmapunits = coordsinmapunits
            layer.l_active	   = l_active
            layer.l_hidden	   = l_hidden
            layer.l_opacity        = l_opacity
            gtemp                  = utils.GetTempfile()
            layer.l_maskfile       = gtemp + ".pgm"
            layer.l_mapfile        = gtemp + ".ppm"

            self.Layers.append(layer)

            if l_render:
                    if not layer.Render():
                            sys.stderr.write ("Could not render layer <%s>\n" % \
						      (name))

            return self.Layers[-1]

	def AddVectorLayer(self, name, mapset=None,
		type = "point,line,boundary,centroid,area,face",
		display= "shape", attrcol= None, icon = "basic/circle",
		size = 8, layer = 1, cats = None, where = None, width = 1,
		wcolumn = None, wscale = 1, color = "000:000:000",
		fcolor = "200:200:200", rgb_column = "GRASSRGB", llayer = 1,
		lcolor = "256:000:000", bgcolor = None, bcolor = None,
		lsize = 8, font = None, xref = "left", yref = "center",
		minreg = None, maxreg = None, colorfromtable=False,
		randomcolor=False, catsasid=False, l_active=True,
		l_hidden=False, l_opacity=1, l_render=False):
            """
            Adds vector layer to list of layers

            Layer attributes:
                    name	- raster layer name
                    mapset	- mapset name, default: current
                    type	- feature type
                    display     - display
                    attrcol     - name of column to be displayed
                    icon	- point and centroid symbol
                    size	- symbol size
                    layer	- layer number
                    cats	- category values
                    where	- WHERE conditions of SQL statement
                    width	- line width
                    wcolumn     - name of column for line widths
                    wscale	- scale factor for wcolumn
                    color	- line color
                    fcolor	- area fill color
                    rgb_column  - name of color definition column
                    llayer	- layer number
                    lcolor	- label color
                    bgcolor     - lable border color
                    lsize	- label size
                    font	- font name
                    xref	- label horizontal justification, default: left
                    yref	- label horizontal justification, default: center
                    minreg	- minimum region size when map is displayed
                    maxreg	- maximum region size when map is displayed

                    colorfromtable - get colors from map table column
                    randomcolor    - random colors according to category number
                    catsasid       - use values from 'cats' option as line id

                    l_active  - see Layer class
                    l_hidden  
                    l_opacity 
                    l_render  - render an image

            Returns:
                    Added layer if succeeded or None
            """

            maplayer = Layer()

            if not mapset:
                    mapset = self.Env["MAPSET"]

            if l_opacity > 1:
                    l_opacity = float(l_opacity) / 100

	    maplayer.name	    = name
            maplayer.mapset         = mapset	 
	    maplayer.type	    = type         
	    maplayer.display        = display
	    maplayer.attrcol        = attrcol
            maplayer.icon	    = icon
	    maplayer.size	    = size
	    maplayer.layer	    = layer
	    maplayer.cats	    = cats
            maplayer.where	    = where
	    maplayer.width	    = width
	    maplayer.wcolumn        = wcolumn
	    maplayer.wscale	    = wscale
            maplayer.color	    = color
	    maplayer.fcolor	    = fcolor
	    maplayer.rgb_column     = rgb_column
	    maplayer.llayer	    = llayer
	    maplayer.lcolor         = lcolor
	    maplayer.bgcolor        = bgcolor
	    maplayer.bcolor         = bcolor
	    maplayer.lsize	    = lsize
	    maplayer.font	    = font
	    maplayer.xref	    = xref
	    maplayer.yref	    = yref
	    maplayer.minreg	    = minreg
	    maplayer.maxreg         = maxreg
	    maplayer.colorfromtable = colorfromtable
            maplayer.randomcolor    = randomcolor
	    maplayer.catsasid       = catsasid

	    maplayer.l_type     = "vector";	
	    maplayer.l_active   = l_active
	    maplayer.l_hidden   = l_hidden
            maplayer.l_opacity  = l_opacity
            gtemp               = utils.GetTempfile()
            maplayer.l_maskfile = gtemp+".pgm"
            maplayer.l_mapfile  = gtemp+".ppm"

            self.Layers.append(maplayer)

            if l_render:
                    if not maplayer.Render():
                            sys.stderr.write("Could not render layer <%s@%s>\n" % \
						     (name,mapset))
			    
	    return self.Layers[-1]

	def PopLayer(self, name=None, mapset=None, id=None):
		"""
		Removes layer from list of layers, defined by name@mapset or id

		Parameters:
			name	- map name
			mapset	- mapset name, default: current
			id	- index of the layer in layer list

		Returns:
			Removed layer on success or None
		"""

		# get mapset name
		if not mapset:
			mapset = self.Env['MAPSET']

		# del by name
		if name:
			retlayer = None
			for layer in self.Layers:
				if layer.name == name and layer.mapset == mapset:
					retlayer = layer
                                        os.remove(layer.l_mapfile)
                                        os.remove(layer.l_maskfile)
					self.Layers.remove(layer)
					return layer
		# del by id
		elif id != None:
			return self.Layers.pop(id)

		return None

	def GetLayerIndex(self, name, mapset=None):
		"""
		Returns index of layer in layer list

		Parameters:
			name	- map name
			mapset	- mapset name, default: current

		Returns:
			Integer or None
		"""

		if not mapset:
			mapset = self.Env['MAPSET']

		for i in range(0, len(self.Layers)):
			if self.Layers[i].name == name and \
				    self.Layers[i].mapset == mapset:
				return i

		return None
        
        def Clean(self):
            """
            Go trough all layers and remove them from layer list
            Removes also l_mapfile and l_maskfile
            
            Returns 1 if failed or None if ok"
            """

            try:
                for layer in self.Layers:
                    os.remove(layer.l_mapfile)
                    os.remove(layer.l_maskfile)
                    self.Layers.remove(layer)
                return
            except:
                return 1


if __name__ == "__main__":
	print """
	Test of Display class.
	Usage: display=Render()
	"""

	print "Initializing..."
	os.system("g.region -d")

	map = Map()
	map.Width = 300
	map.Height = 400

	map.AddRasterLayer("elevation.dem", mapset="PERMANENT")
	map.AddVectorLayer("roads", color="red", width=3, mapset="PERMANENT",
			   l_opacity=50)

	image = map.Render(force=True)

	os.system("display %s" % image)

	#image = map.Render()
	#os.system("display %s" % image)

	#print "Rendering only vector layer, and region, on shifted region"
	#map.Region["n"] ="4937550"
	#map.Region["s"] ="4904160"
	#map.Region["w"] ="577290"
	#map.Region["e"] ="621690"
	#map.PopLayer("elevation.dem", mapset="PERMANENT")
	#layer = map.GetLayerIndex("roads", mapset="PERMANENT")
	#map.Layers[layer].color = "green"
	##map.RenderRegion["render"] = True
	#image = map.Render(force=True)
	#os.system("display %s" % image)
