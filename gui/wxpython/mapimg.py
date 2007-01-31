import os

# Authors: Michael Barton and Jachym Cepicky
# COPYRIGHT:	(C) 1999 - 2007 by the GRASS Development Team

class Layer:
    """
    This class servs for storing map layers to be displayed

    Common layer attributes:
        name    - layer name
        mapset  - mapset name

        l_active - layer is active, will be rendered only if True
        l_hidden - layer is hidden, will be allways rendered
        l_opacity - layer opacity [0-1]
        l_mapfile  - File name of new layer
        l_maskfile  - Mask name of new layer
    """

    def Render(self):
        """
        Runs all d.* commands.

        Returns:
            Name of file with rendered image or None
        """
    
        cmd = ""
        
        os.environ["GRASS_PNGFILE"] = self.l_mapfile

        #
        # Start monitor
        #
        if os.system("d.mon --quiet start=gism"):
            print "Could not run d.mon start=gism"
            return None

        #
        # Render layers
        #
        if self.l_type == "raster":
            cmd = "d.rast -o map=%s@%s" % (self.name,self.mapset)
            if self.catlist: cmd += " catlist=%s" % (self.catlist)
            if self.vallist: cmd += " vallist=%s" % (self.vallist)

        elif self.l_type == "vector":
            cmd = "d.vect map=%s@%s " % (self.name,self.mapset)
            if self.display: cmd += " display=%s" % (self.display)
            if self.attrcol: cmd += " attrcol=%s" % (self.attrcol)
            if self.icon: cmd += " icon=%s" % (self.icon)
            if self.size: cmd += " size=%s" % (self.size)
            if self.cats: cmd += " cats=%s" % (self.cats)
            if self.where: cmd += " where=%s" % (self.where)
            if self.width: cmd += " width=%s" % (self.width)
            if self.color: cmd += " color=%s" % (self.color)
            if self.fcolor: cmd += " fcolor=%s" % (self.fcolor)
        
        elif layer['l_type'] == "wms":
            print "Type wms is not supported yet"
        else:
            print "Type [%s] of layer [%s] is not supported yet" % (layer['l_type'], layer['name'])

        if os.system(cmd):
            print "Could not execute '%s'" % (cmd)
            return None

        #
        # Stop monitor
        #
        if os.system("d.mon --quiet stop=gism"):
            print "Could not run d.mon stop=gism"
            return None
        os.unsetenv("GRASS_PNGFILE")

        return self.l_mapfile

class Map:
    """
    This class serves for rendering of output images.

    Attributes:
        self.Env    - for some environment variables

        self.Verbosity - verbosity level

        self.Width  - stores width of the map in pixels

        self.Height - stores height of the map in pixels
        
        self.WIND   - stores content of WIND file for temporary region
                      settings
                      n-s resol: 30;
                      e-w resol: 30; 
                      ...

        self.Region - stores content of g.region -gp output 
                      "n":1000,
                      "s":0,
                      "e":1000,
                      "w":0,
                      ...
                      
        self.Layers - list of all layers
                    "l_type"    :   "raster/vector/wms/...",
                    "l_hidden"  :   False/True,
                    "l_active"  :   True/False,
                    "name"      :   "name",
                    "mapset"    :   "mapset",
                    ...

        self.RenderRegion - dictionary:
                        "color" :   "RRR:GGG:BBB", 
                        "width" :   3, 
                        "render":   True/False
    """

    def __init__(self):
        """
        While initalization, necessary variables are set, monitor size is
        determinated and
        """

        self.WIND = {}  # for the WIND settings
        self.Region = {}    # for region settings
        self.Width = 0  # for width of the map
        self.Height = 0 # for height of the map

        self.Layers = []# stack of commands for render map
        self.Env = {} # enviroment variables, like MAPSET, LOCATION_NAME, etc.
        self.Verbosity = 0
        self.MapFile = self.__getTempfile()+".png"

        self.RenderRegion = {
                "render"    :False,   # Should the region be displayed?
                "color"     :"255:0:0",
                "width"     :3
                }
        
        # setting some initial env. variables
        self.__initMonSize()
        self.__initEnv()
        self.__initRegion()
        os.environ["GRASS_TRANSPARENT"] ="TRUE"
        os.environ["GRASS_BACKGROUNDCOLOR"] ="ffffff"
        os.environ["GRASS_HEIGHT"] =str(self.Height)
        os.environ["GRASS_WIDTH"] =str(self.Width)
        os.environ["GRASS_MESSAGE_FORMAT"] ="gui"
	os.environ["GRASS_PNG_AUTO_WRITE"] = "TRUE"
        os.environ["GRASS_TRUECOLOR"] ="TRUE"
        os.environ["WIND_OVERRIDE"] ="TRUE"
        os.environ["GRASS_COMPRESSION"] ="0"
        os.environ["GRASS_VERBOSE"] =str(self.Verbosity)


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
	windfile = os.path.join(self.Env['GISDBASE'],
	    self.Env['LOCATION_NAME'],
	    self.Env['MAPSET'],
	    "WIND")
	windfile = open(windfile,"r")
	for line in windfile.readlines():
	    line = line.strip()
	    key,value = line.split(":")
	    key = key.strip()
	    value = value.strip()
	    self.WIND[key] = value
        windfile.close()

        # 
        # Setting resolution
        #
        self.Region['ewres'] = self.Region['nsres'] = abs(float(self.Region['e']) - float(self.Region['w']))/self.Width

    def __initMonSize(self):
        """
        Reads current GRASS monitor dimensions from env or sets it to default values.
        Default means 640x480
        """
        try:
            self.Width = int(os.getenv("GRASS_WIDTH"))
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
            import sys
            sys.stderr.write("GISBASE not set, you have to be in running GRASS session!\n")
            sys.exit(1)

        os.system("d.mon --quiet stop=gism")

        for line in os.popen("g.gisenv").readlines():
            line = line.strip()
            key,val = line.split("=")
	    val = val.replace(";","")
	    val = val.replace("'","")
            self.Env[key] = val

    def __getTempfile(self, pref=None):
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
            key,val = reg.split("=")
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

        try:
            for key in self.WIND.keys():
                if key == 'north':
                    grass_region += "north: %s; " % self.Region['n']
                    continue
                elif key == "south":
                    grass_region += "south: %s; " % self.Region['s']
                    continue
                elif key == "east":
                    grass_region += "east: %s; " % self.Region['e']
                    continue
                elif key == "west":
                    grass_region += "west: %s; " % self.Region['w']
                    continue
                elif key == "e-w resol":
                    grass_region += "e-w resol: %f; " % (self.Region['ewres'])
                    continue
                elif key == "n-s resol":
                    grass_region += "n-s resol: %f; " % (self.Region['nsres'])
                    continue
                elif key == "cols":
                    grass_region += 'cols: %d; ' % (self.Width)
                    continue
                elif key == "rows":
                    grass_region += 'rows: %d; ' % (self.Height)
                    continue
                else:
                    grass_region += key+": "+self.WIND[key]+"; "

            return grass_region

        except:
            return None

    def GetListOfLayers(self, l_type=None, l_active=None, l_hidden=None):
        """
        Returns list of layers of selected type or list of all layers. It
        is also possible to get list of active or hidden layers.

        Parameters:
            l_type  - layer type. raster/vector/wms/...
            l_active - only layers with "active" attribute set to True or False
            l_hidden - only layers with "hidden" attribute set to True or False

        Returns:
            Array with layers or None
        """
        
        selected = []

        # ["raster", "vector", "wms", ... ]

        for layer in self.Layers:

            # specified type only
            if l_type != None and layer['l_type'] != l_type:
                continue

            # hidden and active layers 
            if l_active != None and\
                l_hidden != None:
                if layer['l_active'] == l_active and\
                        layer['l_hidden'] == l_hidden:
                    selected.append(layer)

            # active layers
            elif l_active != None:
                if layer['l_active'] == l_active:
                    selected.append(layer)

            # hidden layers
            elif l_hidden != None:
                if layer['l_hidden'] == l_hidden:
                    selected.append(layer)

            # all layers
            else:
                selected.append(layer)

        if len(selected) > 0:
            return selected
        else:
            return None

    def Render(self,force=False):
        """
        Creates final image composite

        NOTE: This function should be done by high-level tools, which
        should be avaliable in wxPython library

        Returns:
            Name of file with rendered image or None
        """

        maps = []
        masks =[]
        opacities = []

   	for layer in self.Layers:
            if force:
                layer.Render()
	    # add image to compositing list
	    maps.append(layer.l_mapfile)
	    masks.append(layer.l_maskfile)
	    opacities.append(str(layer.l_opacity))

        mapstr = ",".join(maps)
        maskstr = ",".join(masks)
        opacstr = repr(",".join(opacities))
	compcmd = "g.pnmcomp in="+mapstr+" mask="+maskstr+" opacity="+opacstr+" background=255:255:255"\
	    +" width="+str(self.Width)+" height="+str(self.Height)\
	    +" output="+self.MapFile
	# run g.composite to get composite image
        if os.system(compcmd):
            sys.stderr.write("Could not run g.pnmcomp\n")
 
        return self.MapFile

    def AddRasterLayer(self, name, mapset=None, catlist=None,
            vallist=None, invertCats=False, l_active=True, l_hidden=False,
            l_opacity=1, l_render=True):
        """
        Adds raster layer to list of layers
        
        Layer Attributes:
            name    - raster file name
            mapset  - mapset name, default: current
            catlist - string, list of categories
            vallist - string, list of values

            invertCats - invert catlist, True/False

            l_active - layer is active, will be rendered only if True
            l_hidden - layer is hidden, will be allways rendered
            l_opacity - layer opacity [0-1]
            l_mapfile  - File name of new layer
            l_maskfile  - Mask name of new layer
            l_render - Make an image of this layer while adding

        Returns:
            Added layer if succeeded or None
        """

        layer = Layer()

        if not mapset:
            mapset = self.Env["MAPSET"]

        if l_opacity > 1:
            l_opacity = float(l_opacity)/100

        layer.l_type  	= "raster"
        layer.name  	= name
        layer.mapset	= mapset
        layer.catlist	= catlist
        layer.vallist	= vallist
        layer.invertCats= invertCats
        layer.l_active	= l_active
        layer.l_hidden	= l_hidden
        layer.l_opacity = l_opacity
        gtemp = self.__getTempfile()
        layer.l_maskfile = gtemp+".pgm"
        layer.l_mapfile = gtemp+".ppm"

        self.Layers.append(layer)

        if l_render:
            if not layer.Render():
                sys.stderr.write("Could not render layer %s@%s\n" %\
                        (name,mapset))

        return self.Layers[-1]

    def AddGraphLayer(self, name, graph=None, color="255:0:0",
            coordsinmapunits=False,
            l_active=True, l_hidden=True, l_opacity=1, l_render=True):
        """
        Adds graph layer to list of layers (for d.graph definition)
        
        Layer attributes:
            name    - graphics name
            graph   - string with graphics definition
            color   - Color triplet

            coordsinmapunits - coordinates are given in map units

            l_active - layer is active, will be rendered only if True
            l_hidden - layer is hidden, will be allways rendered
            l_opacity - layer opacity [0-1]
            l_mapfile  - File name of new layer
            l_maskfile  - Mask name of new layer
            l_render - Make an image of this layer while adding

        Returns:
            Added layer if succeeded or None
        """

        layer = Layer()

        if l_opacity > 1:
            l_opacity = float(l_opacity)/100

        layer.l_type  	=   "graph"
        layer.name  	=   name
        layer.graph	=  graph
        layer.color	=  color
        layer.coordsinmapunits	= coordsinmapunits
        layer.l_active	= l_active
        layer.l_hidden	= l_hidden
        layer.l_opacity	= l_opacity
        gtemp = self.__getTempfile()
        layer.l_maskfile = gtemp+".pgm"
        layer.l_mapfile = gtemp+".ppm"
        
        self.Layers.append(layer)

        if l_render:
            if not layer.Render():
                sys.stderr.write("Could not render layer %s\n" %\
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
            l_hidden=False, l_opacity=1, l_render=True):
        """
        Adds vector layer to list of layers

        Layer attributes:
            name    - raster file name
            mapset  - mapset name, default: current
            type    - feature type
            display - display
            attrcol - name of column to be displayed
            icon    - point and centroid symbol
            size    -  symbol size
            layer   - layer number
            cats    - category values
            where   - WHERE conditions of SQL statement
            width   - line width
            wcolumn - name of column for line widths
            wscale  - scale factor for wcolumn
            color   - line color
            fcolor  - area fill color
            rgb_column - name of color definition column
            llayer  - layer number
            lcolor  - label color
            bgcolor - lable border color
            lsize   - label size
            font    - font name
            xref    - label horizontal justification, default: left
            yref    - label horizontal justification, default: center
            minreg  - minimum region size when map is displayed
            maxreg  - maximum region size when map is displayed

            colorfromtable - get colors from map table column, True/False
            randomcolor - random colors according to category number, True/False
            catsasid - use values from 'cats' option as line id

            l_active - layer is active, will be rendered only if True
            l_hidden - layer is hidden, will be allways rendered
            l_opacity - layer opacity [0-100]
            l_mapfile  - File name of new layer
            l_maskfile - Mask name of new layer
            l_render- Make a image of this layer while adding

        Returns:
            Added layer if succeeded or None
        """

        layer = Layer()

        if not mapset:
            mapset = self.Env["MAPSET"]

        if l_opacity > 1:
            l_opacity = float(l_opacity)/100

        layer.l_type= "vector";         layer.name      = name
        layer.mapset= mapset  ;         layer.type      = type
        layer.display= display;         layer.attrcol   = attrcol
        layer.icon	= icon;         layer.size	    = size
        layer.layer	= layer;        layer.cats	    = cats
        layer.where	= where;        layer.width	    = width
        layer.wcolumn= wcolumn;         layer.wscale    = wscale
        layer.color	= color;        layer.fcolor    = fcolor
        layer.rgb_column= rgb_column;   layer.llayer  = llayer
        layer.lcolor= lcolor;           layer.bgcolor   = bgcolor
        layer.bcolor= bcolor;           layer.lsize	    = lsize
        layer.font	= font;         layer.xref	    = xref
        layer.yref	= yref;         layer.minreg    = minreg
        layer.maxreg= maxreg;           layer.colorfromtable= colorfromtable
        layer.randomcolor= randomcolor; layer.catsasid= catsasid
        layer.l_active  =l_active;      layer.l_hidden  = l_hidden
        layer.l_opacity = l_opacity
        gtemp = self.__getTempfile()
        layer.l_maskfile = gtemp+".pgm"
        layer.l_mapfile = gtemp+".ppm"

        self.Layers.append(layer)

        if l_render:
            if not layer.Render():
                sys.stderr.write("Could not render layer %s@%s\n" %\
                        (name,mapset))

        return self.Layers[-1]

    def PopLayer(self, name=None, mapset=None, id=None):
        """
        Removes layer from list of layers

        Parameters:
            name    - map name
            mapset  - mapset name, default: current
            id      - index of the layer in layer list

        Returns: 
            Removed layer if succeeded or None
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
            name    - map name
            mapset  - mapset name, default: current

        Returns: 
            Integer or None
        """

        if not mapset:
            mapset = self.Env['MAPSET']

        for i in range(0,len(self.Layers)):
            if self.Layers[i].name == name and self.Layers[i].mapset == mapset:
                return i

        return None


if __name__ == "__main__":
    print """
    Test of Display class.
    Usage: display=Render()
    """

    print "Initializing"
    os.system("g.region -d")

    map = Map()

    print "Rendering raster file"
    map.AddRasterLayer("elevation.dem", mapset="PERMANENT")
    image = map.Render()
    if not image:
        print "Something went wrong, could not render image"
        import sys
        sys.exit(1)
    os.system("display %s" % image)

    print "Adding vector layer"
    map.AddVectorLayer("roads", color="red", width=3, mapset="PERMANENT",
            l_opacity=50)
    image = map.Render()
    os.system("display %s" % image)

    print "Rendering only vector layer, and region, on shifted region"
    #map.Region["n"] ="4937550"
    #map.Region["s"] ="4904160"
    #map.Region["w"] ="577290"
    #map.Region["e"] ="621690"
    map.PopLayer("elevation.dem", mapset="PERMANENT")
    layer = map.GetLayerIndex("roads", mapset="PERMANENT")
    map.Layers[layer].color = "green"
    map.RenderRegion["render"] = True
    image = map.Render()
    os.system("display %s" % image)
