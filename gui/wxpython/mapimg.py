import os

# Authors: Michael Barton and Jachym Cepicky
# COPYRIGHT:	(C) 1999 - 2007 by the GRASS Development Team


class Map:
    """
    This class serves for rendering of output images.

    Attributes:
        self.Env    - for some environment variables

        self.Verbosity - verbosity level

        self.Width  - stores width of the map in pixels

        self.Height - stores height of the map in pixels
        
        self.MapFile- name of the file with rendered image

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
        self.MapFile = "" # name of the PNG file

        self.Layers = []# stack of commands for render map
        self.Env = {} # enviroment variables, like MAPSET, LOCATION_NAME, etc.
        self.Verbosity = 0

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
        os.environ["GRASS_PNGFILE"] =self.MapFile
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

        self.MapFile = os.popen("g.tempfile pid=%d" % 
                os.getpid()).readlines()[0].strip()+".ppm"
        for line in os.popen("g.gisenv").readlines():
            line = line.strip()
            key,val = line.split("=")
	    val = val.replace(";","")
	    val = val.replace("'","")
            self.Env[key] = val

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

    def Render(self):
        """
        Runs all d.* commands.

        Returns:
            Name of file with rendered image or None
        """
    
        cmd = ""
        
        #
        # Set temporary region
        #
        os.environ["GRASS_REGION"] = self.SetRegion()

        #
        # Start monitor
        #
        if os.system("d.mon --quiet start=gism"):
            print "Could not run d.mon start=gism"
            return None

        #
        # Add layers
        #
        for layer in self.GetListOfLayers(l_active=True):
            if layer['l_type'] == "raster":
                cmd = "d.rast -o map=%s@%s" % (layer['name'],layer['mapset'])
                if layer['catlist']: cmd += " catlist=%s" % (layer['catlist'])
                if layer['vallist']: cmd += " vallist=%s" % (layer['vallist'])

            elif layer['l_type'] == "vector":
                cmd = "d.vect map=%s@%s " % (layer['name'],layer['mapset'])
                if layer['display']: cmd += " display=%s" % (layer['display'])
                if layer['attrcol']: cmd += " attrcol=%s" % (layer['attrcol'])
                if layer['icon']: cmd += " icon=%s" % (layer['icon'])
                if layer['size']: cmd += " size=%s" % (layer['size'])
                if layer['cats']: cmd += " cats=%s" % (layer['cats'])
                if layer['where']: cmd += " where=%s" % (layer['where'])
                if layer['width']: cmd += " width=%s" % (layer['width'])
                if layer['color']: cmd += " color=%s" % (layer['color'])
                if layer['fcolor']: cmd += " fcolor=%s" % (layer['fcolor'])
            
            elif layer['l_type'] == "wms":
                print "Type wms is not supported yet"
            else:
                print "Type [%s] of layer [%s] is not supported yet" % (layer['l_type'], layer['name'])

            if os.system(cmd):
                print "Could not execute '%s'" % (cmd)
                return None

        #
        # Draw region
        #
        if self.RenderRegion["render"]:
            region = self.GetRegion()
            cmd = "d.graph -m color=%s" % (self.RenderRegion["color"])
            regcmd = "width %d\n" % (self.RenderRegion["width"])
            regcmd += "polyline\n"
            regcmd += "%f %f\n" % (float(region["w"]), float(region["n"]))
            regcmd += "%f %f\n" % (float(region["e"]), float(region["n"]))
            regcmd += "%f %f\n" % (float(region["e"]), float(region["s"]))
            regcmd += "%f %f\n" % (float(region["w"]), float(region["s"]))
            regcmd += "%f %f\n" % (float(region["w"]), float(region["n"]))

            #print "echo \"",regcmd,"\"|",cmd
            (w,r) = os.popen2(cmd)
            w.write(regcmd)
            w.close()
            r.read()

        #
        # Stop monitor
        #
        if os.system("d.mon --quiet stop=gism"):
            print "Could not run d.mon stop=gism"
            return None
        os.unsetenv("GRASS_REGION")
        return self.MapFile

    def AddRasterLayer(self, name, mapset=None, catlist=None,
            vallist=None, invertCats=False, l_active=True, l_hidden=False):
        """
        Adds raster layer to list of layers
        
        Parameters:
            name    - raster file name
            mapset  - mapset name, default: current
            catlist - string, list of categories
            vallist - string, list of values

            invertCats - invert catlist, True/False

            l_active - layer is active, will be rendered only if True
            l_hidden - layer is hidden, will be allways rendered

        Returns:
            Added layer if succeeded or None
        """

        try:
            if not mapset:
                mapset = self.Env["MAPSET"]

            self.Layers.append({
                'l_type'  :   "raster",
                'name'  :   name,
                'mapset':   mapset,
                'catlist':  catlist,
                'vallist':  vallist,
                'invertCats': invertCats,
                'l_active': l_active,
                'l_hidden': l_hidden,
                })
            return self.Layers[-1]
        except:
            return None

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
            l_hidden=False):
        """
        Adds vector layer to list of layers

        Parameters:
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

        Returns:
            Added layer if succeeded or None
        """

        if not mapset:
            mapset = self.Env["MAPSET"]

        try:
            self.Layers.append({
                'l_type': "vector",     "name"      : name,
                "mapset": mapset  ,     "type"      : type,
                "display": display,     "attrcol"   : attrcol,
                "icon"	: icon,         "size"	    : size,
                "layer"	: layer,        "cats"	    : cats,
                "where"	: where,        "width"	    : width,
                "wcolumn": wcolumn,     "wscale"    : wscale,
                "color"	: color,        "fcolor"    : fcolor,
                "rgb_column": rgb_column, "llayer"  : llayer,
                "lcolor": lcolor,       "bgcolor"   : bgcolor,
                "bcolor": bcolor,       "lsize"	    : lsize,
                "font"	: font,         "xref"	    : xref,
                "yref"	: yref,         "minreg"    : minreg,
                "maxreg": maxreg,       "colorfromtable": colorfromtable,
                "randomcolor": randomcolor, "catsasid": catsasid,
                "l_active"  :l_active,  "l_hidden"  : l_hidden,
                })
            return self.Layers[-1]
        except:
            return None

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
                if layer['name'] == name and layer['mapset'] == mapset:
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
            if self.Layers[i]['name'] == name and self.Layers[i]['mapset'] == mapset:
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
    map.AddVectorLayer("roads", color="red", width=3, mapset="PERMANENT")
    image = map.Render()
    os.system("display %s" % image)

    print "Rendering only vector layer, and region, on shifted region"
    map.Region["n"] ="4937550"
    map.Region["s"] ="4904160"
    map.Region["w"] ="577290"
    map.Region["e"] ="621690"
    map.PopLayer("elevation.dem", mapset="PERMANENT")
    layer = map.GetLayerIndex("roads", mapset="PERMANENT")
    map.Layers[layer]['color'] = "green"
    map.RenderRegion["render"] = True
    image = map.Render()
    os.system("display %s" % image)
