"""
class GRASSLayer
class MapLayer
class Map
"""

import os,sys,glob
import utils

# Authors  : Michael Barton, Jachym Cepicky, Martin Landa
#
# COPYRIGHT:(C) 1999 - 2007 by the GRASS Development Team

DEBUG = False

class GRASSLayer:
	"""
	This class stores GRASS layer metainformation
	(command line parameters and flags) needed for creating
	MapLayer instance

	Attributes:
	        params - based on a given GRASS layer (raster, vector, graph, etc.)
	"""

	def __init__(self, parameters):
		self.params = parameters

class MapLayer:
	"""
	This class serves for storing map layers to be displayed

	Common layer attributes:
		name	- layer name
		mapset	- mapset name
		type     - layer type

		active   - layer is active, will be rendered only if True
		hidden   - layer is hidden, won't be listed in GIS Manager if True
		opacity  - layer opacity [0-1]

		mapfile  - file name of rendered layer
		maskfile - mask name of rendered layer
	"""

	def __init__(self, type, name, mapset,
		     active, hidden, opacity,
		     **parameters):
		self.name   = name
		self.mapset = mapset

		self.type    = type
		self.active  = active
		self.hidden  = hidden
		self.opacity = opacity

		self.grassLayer = GRASSLayer(parameters)

		gtemp = utils.GetTempfile()
		self.maskfile = gtemp + ".pgm"
		self.mapfile  = gtemp + ".ppm"
		self.ovlfile = gtemp + ".png"

	def __renderRasterLayer(self):
		"""
		Stores d.rast command with all parameters in the self.cmd variable
		"""

		try:
			self.cmd = "d.rast -o map=%s@%s" % (self.name, self.mapset)

			for key in self.grassLayer.params.keys():
				value = self.grassLayer.params[key]

				if self.grassLayer.params[key]:
					##FIXME: test data type not strlen
					if type(value) == type(""):
						self.cmd += " %s=%s" % \
						    (key, value)
					else:
						self.cmd += " -%s" % key

		except StandardError, e:
			sys.stderr.write("Could not render raster layer <%s>: %s\n" %\
						 (self.name, str(e)))
			self.cmd = None

	def __renderVectorLayer(self):
		"""
		Stores d.vect command with all parameters in the self.cmd variable
		"""

		try:
			self.cmd = "d.vect map=%s@%s" % (self.name, self.mapset)

			for key in self.grassLayer.params:
				if self.grassLayer.params[key]:
					##FIXME: test data type not strlen
					if len(key) > 1:
						self.cmd += " %s=%s" % \
						    (key, self.grassLayer.params[key])
					else:
						self.cmd += " -%s" % key

		except StandardError, e:
			sys.stderr.write("Could not render vector layer <%s>: %s\n" %\
						 (self.name, str(e)))
			self.cmd = None

	def __renderCommandLayer(self):
		"""
		Stores generic command with all parameters in the self.cmd variable
		"""

		try:
			self.cmd = self.name + " --q"

		except StandardError, e:
			sys.stderr.write("Could not render command layer <%s>: %s\n" %\
						 (self.name, str(e)))
			self.cmd = None

	def __renderOverlay(self):
		"""
		Stores overlay command with all parameters in the self.cmd variable
		"""

		try:
			if self.name != '':
				self.cmd = self.name + " --q"
			else:
				self.cmd = None

		except StandardError, e:
			sys.stderr.write("Could not render overlay <%s>: %s\n" %\
						 (self.name, str(e)))
			self.cmd = None

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

		if self.type == 'overlay':
			if not self.ovlfile:
				gtemp = utils.GetTempfile()
				self.ovlfile  = gtemp + ".png"
		else:
			if not self.mapfile:
				gtemp = utils.GetTempfile()
				self.maskfile = gtemp + ".pgm"
				self.mapfile  = gtemp + ".ppm"


		#
		# prepare command for each layer
		#
		if self.type == "raster":
			self.__renderRasterLayer()

		elif self.type == "vector":
			self.__renderVectorLayer()

		elif self.type == "command":
			self.__renderCommandLayer()

		elif self.type == "overlay":
			self.__renderOverlay()

		elif self.type == "wms":
			print "Type wms is not supported yet"
		else:
			print "Type <%s> of layer <%s> is not supported yet" % \
			    (self.type, self.name)

		#
		# Start monitor
		#
		if self.type == 'overlay':
			os.environ["GRASS_PNGFILE"] = self.ovlfile
		else:
			os.environ["GRASS_PNGFILE"] = self.mapfile
		os.environ["GRASS_RENDER_IMMEDIATE"] = "TRUE"

		#
		# execute command
		#
		if not self.cmd:
			sys.stderr.write("Could not render layer <%s> with command: #%s#" %\
						 (self.name, self.cmd))
			return None

		if os.system(self.cmd):
			print "Could not execute '%s'" % (self.cmd)
			self.mapfile = None
			self.maskfile = None
			return None

		#
		# Stop monitor
		#
		os.unsetenv("GRASS_PNGFILE")
		os.unsetenv("GRASS_RENDER_IMMEDIATE")

		if self.type == 'overlay':
			pass
			return self.ovlfile
		else:
			return self.mapfile

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
		mapfile   - rendered final image filename
	"""

	def __init__(self):
		"""
		While initalization, necessary variables are set, monitor size is
		determined and
		"""

		self.wind      = {}  # WIND settings
		self.region    = {}  # region settings
		self.width     = 300 # map width
		self.height    = 400 # map height

		self.layers    = []  # stack of available layer
		self.overlays  = []
		self.lookup    = {}  # lookup dictionary for tree items and layers
		self.env       = {}  # enviroment variables, like MAPSET, LOCATION_NAME, etc.
		self.verbosity = 0
		self.mapfile   = utils.GetTempfile()
		self.ovlist = []

#		self.renderRegion = {
#			"render" : True,     # should the region be displayed?
#			"color"	 :"255:0:0",
#			"width"	 : 3
#			}

		# setting some initial env. variables
		self.__initEnv()
		self.__initRegion()
		os.environ["GRASS_TRANSPARENT"]     = "TRUE"
		os.environ["GRASS_BACKGROUNDCOLOR"] = "ffffff"
		os.environ["GRASS_HEIGHT"]          = str(self.height)
		os.environ["GRASS_WIDTH"]           = str(self.width)
		os.environ["GRASS_MESSAGE_FORMAT"]  = "gui"
		os.environ["GRASS_PNG_AUTO_WRITE"]  = "TRUE"
		os.environ["GRASS_TRUECOLOR"]       = "TRUE"
		os.environ["GRASS_COMPRESSION"]     = "0"
		os.environ["GRASS_VERBOSE"]         = str(self.verbosity)

	def __initRegion(self):
		"""
		Reads current region settings from g.region command
		"""

		#
		# setting region
		#
		self.region = self.GetRegion()

		#
		# setting WIND
		#
		# FIXME: duplicated region WIND == g.region (at least some values)
		# FIXME: cannot open WIND file -> raise exception
		windfile = os.path.join (self.env['GISDBASE'],
					 self.env['LOCATION_NAME'],
					 self.env['MAPSET'],
					 "WIND")

		try:
			windfile = open (windfile, "r")
		except StandardError, e :
			sys.stderr.write("Could open file <%s>: %s\n" % \
						 (windfile,e))
			sys.exit(1)

		for line in windfile.readlines():
			line = line.strip()
			key, value = line.split(":",1)
			key = key.strip()
			value = value.strip()
			self.wind[key] = value

		self.__adjustRegion()

		windfile.close()



		#
		# setting resolution
		#
		# self.region['ewres'] = self.region['nsres'] = abs(float(self.region['e'])
		# - float(self.region['w']))/self.width

	def __initMonSize(self):
		"""
		Reads current GRASS monitor dimensions from env or
		use the default values [640x480]
		"""

		try:
			self.width = int (os.getenv("GRASS_WIDTH"))
		except:
			self.width = 640

		try:
			self.height = int(os.getenv("GRASS_HEIGHT"))

		except:
			self.height = 480


	def __initEnv(self):
		"""
		Stores environment variables to self.env variable
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
			self.env[key] = val

	def __adjustRegion(self):
		"""
		Adjust region according to monitor size
                """

		# adjusting region to monitor size
		if self.width > self.height and \
			self.region["w"] - self.region["e"] > \
			self.region['n'] - self.region['s']:

			# changing e-w region
			self.region["ewres"] = self.region["nsres"] = \
			    (self.region['n'] - self.region['s']) / self.height

			center = self.region['w'] + \
			    (self.region['e'] - self.region['w']) / 2

			self.region['w'] = center - self.width / 2 * \
			    self.region["nsres"]

			self.region['e'] = center + self.width / 2 * \
			    self.region["nsres"]

			self.region['rows'] = self.width
			self.region['cols'] = self.height

		else:
			# changing n-s region
			self.region["ewres"] = self.region["nsres"] = \
			    (self.region['e'] - self.region['w']) / self.width

			center = self.region['s'] + \
			    (self.region['n'] - self.region['s']) / 2

			self.region['s'] = center - self.height / 2 * \
			    self.region["ewres"]

			self.region['n'] = center + self.height / 2 * \
			    self.region["ewres"]

			self.region['rows'] = self.width
			self.region['cols'] = self.height


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
			key, val = reg.split("=",1)
			try:
                            region[key] = float(val)
                        except ValueError:
                            region[key] = val

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
			for key in self.wind.keys():
				if key == 'north':
					grass_region += "north: %s; " % \
					    (self.region['n'])
					continue
				elif key == "south":
					grass_region += "south: %s; " % \
					    (self.region['s'])
					continue
				elif key == "east":
					grass_region += "east: %s; " % \
					    (self.region['e'])
					continue
				elif key == "west":
					grass_region += "west: %s; " % \
					    (self.region['w'])
					continue
				elif key == "e-w resol":
					grass_region += "e-w resol: %f; " % \
					    (self.region['ewres'])
					continue
				elif key == "n-s resol":
					grass_region += "n-s resol: %f; " % \
					    (self.region['nsres'])
					continue
				elif key == "cols":
					grass_region += 'cols: %d; ' % \
					    (self.width)
					continue
				elif key == "rows":
					grass_region += 'rows: %d; ' % \
					    (self.height)
					continue
				else:
					grass_region += key + ": "  + self.wind[key] + "; "

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

		for layer in self.layers:

			# specified type only
			if l_type != None and layer.type != l_type:
				continue

			# hidden and active layers
			if l_active != None and \
				    l_hidden != None:
				if layer.active == l_active and \
					    layer.hidden == l_hidden:
					selected.append(layer)

			# active layers
			elif l_active != None:
				if layer.active == l_active:
					selected.append(layer)

			# hidden layers
			elif l_hidden != None:
				if layer.hidden == l_hidden:
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
		os.environ["GRASS_WIDTH"]  = str(self.width)
		os.environ["GRASS_HEIGHT"] = str(self.height)

		if DEBUG:
			print ("mapimg.py: Map: Render: force=%s" % (force))
		try:
			# render overlays
			self.ovlist = []
			for overlay in self.overlays:
				if overlay == None or overlay.active == False:
					continue

				# render if there is no mapfile
				if overlay.ovlfile == None:
					overlay.Render()

				# redraw layer content
				if force:
					if not overlay.Render():
						continue
				self.ovlist.append(overlay.ovlfile)

			# render map layers
			for layer in self.layers:
				# skip if hidden or not active
				if layer.active == False or layer.hidden == True:
					continue

				# render if there is no mapfile
				if layer.mapfile == None:
					layer.Render()

				# redraw layer content
				if force:
					if not layer.Render():
						continue

				# add image to compositing list
				maps.append(layer.mapfile)
				masks.append(layer.maskfile)
				opacities.append(str(layer.opacity))

			# make arrays to strings
			mapstr = ",".join(maps)
			maskstr = ",".join(masks)
			opacstr = repr(",".join(opacities))

			# compose command
			compcmd = "g.pnmcomp in=" + mapstr + \
			    " mask=" + maskstr + \
			    " opacity=" + opacstr + \
			    " background=255:255:255" + \
			    " width=" + str(self.width) + \
			    " height=" + str(self.height) + \
			    " output=" + self.mapfile

			# render overlays

			os.unsetenv("GRASS_REGION")

			if tmp_region:
				os.environ["GRASS_REGION"] = tmp_region

			# run g.composite to get composite image
			if os.system(compcmd):
				sys.stderr.write("Could not run g.pnmcomp\n")
				raise Exception (compcmd)

			return self.mapfile, self.ovlist

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

			l_active   - see MapLayer class
			l_hidden
			l_opacity
			l_render   - render an image

		Returns:
                    Added layer on success or None

		"""

		if not mapset:
			mapset = self.env["MAPSET"]

		# l_opacity must be <0;1>
		if l_opacity < 0: l_opacity = 0
		elif l_opacity > 1: l_opacity = 1

		layer = MapLayer("raster", name, mapset,
				 l_active, l_hidden, l_opacity,
				 catlist    = catlist,
				 vallist    = vallist,
				 i          = invertCats)

		# add maplayer to the list of layers
		self.layers.append(layer)

		if l_render:
			if not layer.Render():
				sys.stderr.write("Could not render layer <%s@%s>\n" % \
							 (name,mapset))

		return self.layers[-1]

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

                    l_active         - see MapLayer class
                    l_hidden
                    l_opacity
                    l_render         - render an image

            Returns:
                    Added layer on success or None
            """

	    # l_opacity must be <0;1>
	    if l_opacity < 0: l_opacity = 0
	    elif l_opacity > 1: l_opacity = 1

            layer = MapLayer("graph", name, "", # current mapset
			     l_active, l_hidden, l_opacity,
			     color = color,
			     coordsinmapunits = coordsinmapunits)

            self.layers.append(layer)

            if l_render:
                    if not layer.Render():
                            sys.stderr.write ("Could not render layer <%s>\n" % \
						      (name))

            return self.layers[-1]

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

                    l_active  - see MapLayer class
                    l_hidden
                    l_opacity
                    l_render  - render an image

            Returns:
                    Added layer if succeeded or None
            """

            if not mapset:
                    mapset = self.env["MAPSET"]

	    # l_opacity must be <0;1>
	    if l_opacity < 0: l_opacity = 0
	    elif l_opacity > 1: l_opacity = 1


            maplayer = MapLayer("vector", name, mapset,
				l_active, l_hidden, l_opacity,
				display = display,
				attrcol = attrcol,
				icon = icon,
				size = size,
				layer = layer,
				cats = cats,
				where = where,
				width = width,
				wcolumn = wcolumn,
				wscale = wscale,
				color = color,
				fcolor = fcolor,
				rgb_column = rgb_column,
				llayer = llayer,
				lcolor = lcolor,
				bgcolor = bgcolor,
				bcolor = bcolor,
				lsize = lsize,
				font = font,
				xref = xref,
				yref = yref,
				minreg = minreg,
				maxreg = maxreg,
				colorfromtable = colorfromtable,
				randomcolor = randomcolor,
				catsasid = catsasid)

	    self.layers.append(maplayer)

            if l_render:
                    if not maplayer.Render():
                            sys.stderr.write("Could not render layer <%s@%s>\n" % \
						     (name,mapset))

	    return self.layers[-1]

	def AddCommandLayer(self, name, mapset=None, l_active=True, l_hidden=False,
		l_opacity=1, l_render=False):
		"""
		Adds generic layer to list of layers

		Layer Attributes:
			name	   - raster layer name
			mapset	   - mapset name, default: current

			l_active   - see MapLayer class
			l_hidden
			l_opacity
			l_render   - render an image

		Returns:
                    Added layer on success or None

		"""
		if not mapset:
			mapset = self.env["MAPSET"]

		# l_opacity must be <0;1>
		if l_opacity < 0: l_opacity = 0
		elif l_opacity > 1: l_opacity = 1
			# the following won't work in all situations. What
			# if opacity had somehow been set to 1000?
#			l_opacity = float(l_opacity) / 100

		layer = MapLayer("command", name, mapset,
				 l_active, l_hidden, l_opacity)

		# add maplayer to the list of layers
		self.layers.append(layer)

		if l_render:
			if not layer.Render():
				sys.stderr.write("Could not render layer <%s@%s>\n" % \
							 (name, mapset))

		return self.layers[-1]

	def addLayer(self, item, command, mapset=None, l_active=True, l_hidden=False,
		l_opacity=1, l_render=False):
		"""
		Adds generic layer to list of layers

		Layer Attributes:
			command	   - display command
			mapset	   - mapset name, default: current

			l_active   - see MapLayer class
			l_hidden
			l_opacity
			l_render   - render an image

		Returns:
                    Added layer on success or None

		"""
		if not mapset:
			mapset = self.env["MAPSET"]

		# l_opacity must be <0;1>
		if l_opacity < 0: l_opacity = 0
		elif l_opacity > 1: l_opacity = 1

		layer = MapLayer("command", command, mapset,
				 l_active, l_hidden, l_opacity)

		# add maplayer to the list of layers
		self.layers.append(layer)
		# add item and layer to lookup dictionary
		self.lookup[item] = layer

		if l_render:
			if not layer.Render():
				sys.stderr.write("Could not render layer <%s@%s>\n" % \
							 (name, mapset))

		return self.layers[-1]

	def delLayer(self, item):
		"""
		Removes layer from list of layers, defined by name@mapset or id

		Parameters:
			name	- map name
			mapset	- mapset name, default: current
			id	- index of the layer in layer list

		Returns:
			Removed layer on success or None
		"""
		layer = self.lookup[item]

		if layer in self.layers:
			if layer.mapfile:
				base = os.path.split(layer.mapfile)[0]
				mapfile = os.path.split(layer.mapfile)[1]
				tempbase = mapfile.split('.')[0]
				basefile = os.path.join(base,tempbase)+r'.*'
				for f in glob.glob(basefile):
					os.remove(f)
			self.layers.remove(layer)
			del self.lookup[item]
			return layer

		return None

	def reorderLayers(self, item_list):

		# make a new reordered list
		temp = []

		for item in item_list:
			temp.append(self.lookup[item])

		# replace original layers list with reordered one
		self.layers = temp


	def changeLayer(self, item, command, mapset=None, l_active=True, l_hidden=False,
		l_opacity=1, l_render=False):

		if not mapset:
			mapset = self.env["MAPSET"]

		# l_opacity must be <0;1>
		if l_opacity < 0: l_opacity = 0
		elif l_opacity > 1: l_opacity = 1

		newlayer = MapLayer("command", command, mapset,
				 l_active, l_hidden, l_opacity)

		oldlayerindex = self.layers.index(self.lookup[item])

		# add maplayer to the list of layers
		if self.lookup[item]:
			self.layers[oldlayerindex] = newlayer
			self.lookup[item] = newlayer


		if l_render:
			if not layer.Render():
				sys.stderr.write("Could not render layer <%s@%s>\n" % \
							 (name, mapset))

		return self.layers[-1]

	def changeOpacity(self, item, l_opacity):
		# l_opacity must be <0;1>
		if l_opacity < 0: l_opacity = 0
		elif l_opacity > 1: l_opacity = 1
		layer = self.lookup[item]
		layer.opacity = l_opacity

	def changeActive(self, item, activ):
		layer = self.lookup[item]
		layer.active = activ

	def RemoveLayer(self, name=None, mapset=None, id=None):
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
			mapset = self.env['MAPSET']

		# del by name
		if name:
			retlayer = None
			for layer in self.layers:
				if layer.name == name and layer.mapset == mapset:
					retlayer = layer
					os.remove(layer.mapfile)
					os.remove(layer.maskfile)
					self.layers.remove(layer)
					return layer
		# del by id
		elif id != None:
			return self.layers.pop(id)

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
			mapset = self.env['MAPSET']

		for i in range(0, len(self.layers)):
			if self.layers[i].name == name and \
				    self.layers[i].mapset == mapset:
				return i

		return None

	def addOverlay(self, type, command, mapset=None, l_active=True,
				   l_hidden=False, l_opacity=1, l_render=False):

		"""
		Adds overlay (grid, barscale, others?) to list of overlays

		Overlay Attributes:
			command	   - display command
			l_active   - see MapLayer class
			l_render   - render an image

		Returns:
                    Added layer on success or None

		"""

		overlay = MapLayer("overlay", command, mapset,
				 l_active, l_hidden, l_opacity)


		# add maplayer to the list of layers
		self.overlays.append(overlay)
		# add item and layer to lookup dictionary

		if l_render and command != '':
			if not overlay.Render():
				sys.stderr.write("Could not render overlay <%s>\n" % (command))

		return self.overlays[-1]

	def changeOverlay(self, type, command, mapset=None, l_active=True,
				   l_hidden=False, l_opacity=1, l_render=False):

		overlay = MapLayer('overlay', command, mapset,
				 l_active, l_hidden, l_opacity)

		# add maplayer to the list of layers
		self.overlays[type] = overlay

		if l_render and command != '':
			if not overlay.Render():
				sys.stderr.write("Could not render overlay <%s>\n" % (command))

		return self.overlays[-1]



	def Clean(self):
		"""
		Go trough all layers and remove them from layer list
		Removes also l_mapfile and l_maskfile

		Returns 1 if failed or None if ok"
		"""
		try:
			for layer in self.layers:
				if layer.mapfile:
					base = os.path.split(layer.mapfile)[0]
					mapfile = os.path.split(layer.mapfile)[1]
					tempbase = mapfile.split('.')[0]
					basefile = os.path.join(base,tempbase)+r'.*'
					for f in glob.glob(basefile):
						os.remove(f)
				self.layers.remove(layer)
			for overlay in self.overlays:
				if overlay.ovlfile:
					base = os.path.split(overlay.ovlfile)[0]
					mapfile = os.path.split(overlay.ovlfile)[1]
					tempbase = mapfile.split('.')[0]
					basefile = os.path.join(base,tempbase)+r'.*'
					for f in glob.glob(basefile):
						os.remove(f)
				self.overlays.remove(overlay)
			return None
		except:
			return 1
		self.layers = []


if __name__ == "__main__":
	print """
	Test of Display class.
	Usage: display=Render()
	"""

	print "Initializing..."
	os.system("g.region -d")

	map = Map()
	map.width = 300
	map.height = 400

	map.AddRasterLayer("elevation.dem", mapset="PERMANENT", catlist="1000-1500", invertCats=True)

	map.AddVectorLayer("roads", color="red", width=3, mapset="PERMANENT",
			   l_opacity=50)

	image = map.Render(force=True)

	if image:
		os.system("display %s" % image)

	#image = map.Render()
	#os.system("display %s" % image)

	#print "Rendering only vector layer, and region, on shifted region"
	#map.region["n"] ="4937550"
	#map.region["s"] ="4904160"
	#map.region["w"] ="577290"
	#map.Region["e"] ="621690"
	#map.RemoveLayer("elevation.dem", mapset="PERMANENT")
	#layer = map.GetLayerIndex("roads", mapset="PERMANENT")
	#map.layers[layer].color = "green"
	##map.renderRegion["render"] = True
	#image = map.Render(force=True)
	#os.system("display %s" % image)
