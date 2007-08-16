"""
MODULE: render

CLASSES:
    * MapLayer
    * Map

PURPOSE: Rendering

AUTHORS: The GRASS Development Team
         Michael Barton, Jachym Cepicky, Martin Landa

COPYRIGHT: (C) 2006-2007 by the GRASS Development Team
         This program is free software under the GNU General Public
         License (>=v2). Read the file COPYING that comes with GRASS
         for details.
"""

import os, sys, glob, math
import utils

import cmd
from debug import Debug as Debug

class MapLayer(object):
    """
    This class serves for storing map layers to be displayed

    Common layer attributes:
    type     - layer type (raster, vector, overlay, command)
    name     - layer name, e.g. map name ('elevation@PERMANENT')
    cmd      - GRASS command string

    active   - layer is active, will be rendered only if True
    hidden   - layer is hidden, won't be listed in GIS Manager if True
    opacity  - layer opacity [0-1]

    mapfile  - file name of rendered layer
    maskfile - mask name of rendered layer
    """
    def __init__(self, type, cmd, name=None,
                 active=True, hidden=False, opacity=1):
        self.type    = type
        self.name    = name
        self.cmdlist = cmd

        self.active  = active
        self.hidden  = hidden
        self.opacity = opacity

        Debug.msg (3, "MapLayer.__init__(): type=%s, cmd=%s, name=%s, active=%d, opacity=%d, hidden=%d" %
                       (self.type, self.GetCmd(string=True), self.name, self.active, self.opacity, self.hidden))

        gtemp = utils.GetTempfile()
        self.maskfile = gtemp + ".pgm"
        if self.type == "overlay":
            self.mapfile = gtemp + ".png"
        else:
            self.mapfile = gtemp + ".ppm"

    def __del__(self):
        Debug.msg (3, "MapLayer.__del__(): layer=%s, cmd=%s" %
                   (self.name, self.GetCmd(string=True)))

    def __renderLayer(self):
        """
        Stores generic command with all parameters in the self.cmdlist variable
        """
        try:
            Debug.msg (3, "MapLayer.__renderLayer(): cmd=%s" % self.cmdlist)

        except StandardError, e:
            sys.stderr.write("Could not render command layer <%s>: %s\n" %\
                 (self.name, str(e)))
            self.cmdlist = None

    def Render(self):
        """
        Runs all d.* commands.

        Returns:
            Name of file with rendered image or None
        """

        Debug.msg (3, "MapLayer.Render(): type=%s" % \
                   (self.type))

        #
        # to be sure, set temporary file with layer and mask
        #
        if self.type == 'overlay':
            if not self.mapfile:
                gtemp = utils.GetTempfile()
                self.mapfile  = gtemp + ".png"
        else:
            if not self.mapfile:
                gtemp = utils.GetTempfile()
                self.maskfile = gtemp + ".pgm"
                self.mapfile  = gtemp + ".ppm"


        #
        # prepare command for each layer
        #
        layertypes = ['raster','rgb','his','shaded','rastarrow','rastnum','vector','thememap','themechart',\
                      'grid','geodesic','rhumb','labels','command','overlay']
        if self.type in layertypes:
            self.__renderLayer()
        elif self.type == "wms":
            print "Type wms is not supported yet"
        else:
            print "Type <%s> of layer <%s> is not supported yet" % \
                  (self.type, self.name)

        #
        # start monitor
        #
        os.environ["GRASS_PNGFILE"] = self.mapfile

        #
        # execute command
        #
        if not self.cmdlist:
            sys.stderr.write("Cannot render layer <%s> with command: #%s#" %\
                             (self.name, self.cmdlist))
            return None

        runcmd = cmd.Command(cmd=self.cmdlist + ['--q']) # run quiet
        if runcmd.returncode != 0:
            print "Could not execute '%s'" % (self.cmdlist)
            for msg in runcmd.msg:
                print msg[1]
            self.mapfile = None
            self.maskfile = None
            return None

        #
        # stop monitor
        #
        os.unsetenv("GRASS_PNGFILE")

        return self.mapfile

    def GetMapset(self):
        """Return mapset name of the layer or None"""
        if not self.name:
            return None

        idxAt = self.name.find('@')
        if idxAt > -1:
            return self.name[idxAt+1:]
        else:
            return None

    def GetCmd(self, string=False):
        """Get command list/string"""
        if string:
            return ' '.join(self.cmdlist)
        else:
            return self.cmdlist

    def GetOpacity(self, float=False):
        """Get opacity level"""
        if float:
            return self.opacity
        else:
            return int (self.opacity * 100)

class Map(object):
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
        'n':1000,
        's':0,
        'e':1000,
        'w':0,
        ...
    layers - list of all available layers
    renderRegion - dictionary:
        'color' : 'RRR:GGG:BBB',
        'width' : 3,
        'render': True/False
    mapfile   - rendered final image filename
    """

    def __init__(self):
        """
        During initalization, necessary variables are set, monitor size is
        determined and
        """

        self.wind      = {}  # WIND settings
        self.region    = {}  # region settings
        self.width     = 640.0 # map width
        self.height    = 480.0 # map height

        self.layers    = []  # stack of available layer
        self.overlays  = []  # stack of available overlays
        self.ovlookup  = {}  # lookup dictionary for overlay items and overlays
        self.env       = {}  # enviroment variables, like MAPSET, LOCATION_NAME, etc.
        self.verbosity = 0
        self.mapfile   = utils.GetTempfile()

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

        windfile.close()

        #
        # setting resolution
        #
        self.SetRegion()


        # self.region['ewres'] = self.region['nsres'] = abs(float(self.region['e'])
        # - float(self.region['w']))/self.width

# I don't think that this method is used anymore (Michael 8/6/2007
#    def __initMonSize(self):
#        """
#        Reads current GRASS monitor dimensions from env or
#        use the default values [640x480]
#        """
#        print 'in initmonsize'
#
#        try:
#            self.width = int (os.getenv("GRASS_WIDTH"))
#        except:
#            self.width = 640
#
#        try:
#            self.height = int(os.getenv("GRASS_HEIGHT"))
#
#        except:
#            self.height = 480


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
        Adjusts display resolution to match monitor size in pixels.
        Maintains constant display resolution, not related to computational
        region. Do NOT use the display resolution to set computational
        resolution. Set computational resolution through g.region.
        """

        self.width = float(self.width)
        self.height = float(self.height)
        mapwidth = abs(self.region["e"] - self.region["w"])
        mapheight = abs(self.region['n'] - self.region['s'])

        self.region["nsres"] = mapheight / self.height
        self.region["ewres"] =  mapwidth / self.width
        self.region['rows'] = round(mapheight/self.region["nsres"])
        self.region['cols'] = round(mapwidth/self.region["ewres"])

        Debug.msg (3, "Map.__adjustRegion(): %s" % self.region)

        return self.region

    def alignResolution(self):
        """
        Sets display extents to even multiple of
        current resolution in WIND file from SW corner.
        This must be done manually as using the -a flag
        can produce incorrect extents.

        Used for saving current display settings to WIND file
        """

        # new values to use for saving to region file
        new = {}
#        windreg = {}
        n = s = e = w = 0.0
        nwres = ewres = 0.0

        # Get current values for region and display
#        windreg = self.GetRegion()
        nsres = self.GetRegion()['nsres']
        ewres = self.GetRegion()['ewres']

        n = float(self.region['n'])
        s = float(self.region['s'])
        e = float(self.region['e'])
        w = float(self.region['w'])

        # Calculate rows, columns, and extents
        new['rows'] = math.fabs(round((n-s)/nsres))
        new['cols'] = math.fabs(round((e-w)/ewres))

        # Calculate new extents
        new['s'] = nsres * round(s/nsres)
        new['w'] = ewres * round(w/ewres)
        new['n'] = new['s'] + (new['rows'] * nsres)
        new['e'] = new['w'] + (new['cols'] * ewres)
        return new

    def GetRegion(self):
        """
        Returns dictionary with output from g.region -gp

        Example:
            {"n":"4928010", "s":"4913700", "w":"589980",...}
        """

        region = {}

        tmpreg = os.getenv("GRASS_REGION")
        os.unsetenv("GRASS_REGION")

        for reg in os.popen("g.region -ugp").readlines():
            reg = reg.strip()
            key, val = reg.split("=",1)
            try:
                region[key] = float(val)
            except ValueError:
                region[key] = val

        if tmpreg:
            os.environ["GRASS_REGION"] = tmpreg

        Debug.msg (3, "Map.GetRegion(): %s" % region)

        return region

    def SetRegion(self, windres=False):
        """
        Render string for GRASS_REGION env. variable, so that the images will be rendered
        from desired zoom level.

        Returns:
        string usable for GRASS_REGION variable or None
        If windres set to True, uses resolution from WIND file rather than display
        (for modules that require set resolution like d.rast.num)
        """

        grass_region = ""

        self.region = self.__adjustRegion()

#        newextents = self.alignResolution()
#        self.region['n'] = newextents['n']
#        self.region['s'] = newextents['s']
#        self.region['e'] = newextents['e']
#        self.region['w'] = newextents['w']

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

            Debug.msg (4, "Map.SetRegion(): %s" % grass_region)

            return grass_region

        except:
            return None

    def ProjInfo(self):
        """
        Return region projection and map units information
        """

        projinfo = {}
        cmdlist = ['g.proj', '-p']

        p = cmd.Command(cmdlist)

        if p.returncode == 0:
            output = p.module_stdout.read().split('\n')
            for line in output:
                if ':' in line:
                    key,val = line.split(':')
                    key = key.strip()
                    val = val.strip()
                    projinfo[key] = val
                elif "XY location (unprojected)" in line:
                    projinfo['proj'] = "xy"
            return projinfo
        else:
            return None

    def GetListOfLayers(self, l_type=None, l_mapset=None, l_active=None, l_hidden=None):
        """
        Returns list of layers (including overlays [l_type='overlay'] of
        selected type or list of all layers. It
        is also possible to get list of active or hidden layers.

        Parameters:
            l_type	 - layer type, e.g. raster/vector/wms/overlay ...
            l_active - only layers with 'active' attribute set to True or False
            l_hidden - only layers with 'hidden' attribute set to True or False

        Returns:
            List of selected layers or None
        """

        selected = []

        # ["raster", "vector", "wms", ... ]
        for layer in self.layers + self.overlays:
            # specified type only
            if l_type != None and layer.type != l_type:
                continue

            # mapset
            if l_mapset != None and layer.GetMapset() != l_mapset:
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

        Debug.msg (3, "Map.GetListOfLayers(): numberof=%d" % len(selected))

        return selected

    def Render(self, force=False):
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

        tmp_region = os.getenv("GRASS_REGION")
        os.environ["GRASS_REGION"] = self.SetRegion()
        os.environ["GRASS_WIDTH"]  = str(self.width)
        os.environ["GRASS_HEIGHT"] = str(self.height)


        try:
            # render map layers
            for layer in self.layers + self.overlays:
                # skip if not active
                if layer == None or layer.active == False:
                    continue

                # render if there is no mapfile
                if layer.mapfile == None:
                    layer.Render()

                # redraw layer content
                if force:
                    if not layer.Render():
                        continue

                # add image to compositing list
                if layer.type != "overlay":
                    maps.append(layer.mapfile)
                    masks.append(layer.maskfile)
                    opacities.append(str(layer.opacity))

            Debug.msg (3, "Map.Render() type=%s, layer=%s " % (layer.type, layer.name))

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

            Debug.msg (2, "Map.Render() force=%s file=%s" % (force, self.mapfile))

            return self.mapfile

        except Exception, e:
            os.unsetenv("GRASS_REGION")

            if tmp_region:
                os.environ["GRASS_REGION"] = tmp_region
            return None

    def AddLayer(self, type, command, name=None,
                 l_active=True, l_hidden=False, l_opacity=1, l_render=False):
        """
        Adds generic display command layer to list of layers

        Layer Attributes:
        item - gis manager layer tree item
        type - layer type
        name - layer name
        cmd  - GRASS command string

        l_active   - checked/not checked for display in layer tree
        l_hidden   - not used here
        l_opacity  - range from 0-1
        l_render   - render an image if False

        Returns:
            Added layer on success or None

        """
        # l_opacity must be <0;1>
        if l_opacity < 0: l_opacity = 0
        elif l_opacity > 1: l_opacity = 1

        layer = MapLayer(type=type, name=name, cmd=command,
                         active=l_active, hidden=l_hidden, opacity=l_opacity)

        # add maplayer to the list of layers
        self.layers.append(layer)

        Debug.msg (3, "Map.AddLayer(): layer=%s" % layer.name)
        if l_render:
            if not layer.Render():
                print >> sys.stderr, _("Could not render layer <%s>\n") % \
                                      (name)

        return self.layers[-1]

    def DeleteLayer(self, layer):
        """
        Removes layer from list of layers,
        defined by reference to MapLayer instance

        Returns:
            Removed layer on success or None
        """

        Debug.msg (3, "Map.DeleteLayer(): name=%s" % layer.name)
        if layer in self.layers:
            if layer.mapfile:
                base = os.path.split(layer.mapfile)[0]
                mapfile = os.path.split(layer.mapfile)[1]
                tempbase = mapfile.split('.')[0]
                basefile = os.path.join(base,tempbase)+r'.*'
                for f in glob.glob(basefile):
                    os.remove(f)
            self.layers.remove(layer)
            return layer

        return None

    def ReorderLayers(self, layerList):
        """
        Make a new reordered list to match reordered
        layer tree - for drag and drop
        """
        self.layers = layerList

        layerNameList = ""
        for layer in self.layers:
            if layer.name:
                layerNameList += layer.name + ','
        Debug.msg (4, "Map.ReoderLayers(): layers=%s" % \
                   (layerNameList))

    def ChangeLayer(self, layer, type, command, name=None,
                    l_active=True, l_hidden=False, l_opacity=1, l_render=False):
        """
        Change the command and other other options for a layer
        """

        # l_opacity must be <0;1>
        if l_opacity < 0: l_opacity = 0
        elif l_opacity > 1: l_opacity = 1

        Debug.msg (3, "Map.ChangeLayer():")

        newlayer = MapLayer(type=type, cmd=command, name=name,
                            active=l_active, hidden=l_hidden, opacity=l_opacity)

        oldlayerindex = self.layers.index(layer)

        # add maplayer to the list of layers
        if layer:
            self.layers[oldlayerindex] = newlayer

        if l_render:
            if not layer.Render():
                sys.stderr.write("Could not render layer <%s>\n" % \
                       (name))

        return self.layers[-1]

    def ChangeOpacity(self, layer, l_opacity):
        """
        Changes opacity value for rendering
        """
        # l_opacity must be <0;1>
        if l_opacity < 0: l_opacity = 0
        elif l_opacity > 1: l_opacity = 1

        layer.opacity = l_opacity
        Debug.msg (3, "Map.ChangeOpacity(): layer=%s, opacity=%f" % \
                   (layer.name, layer.opacity))


    def ChangeLayerActive(self, layer, active):
        """
        Change the active state of a layer
        """
        layer.active = active

        Debug.msg (3, "Map.ChangeLayerActive(): layer=%s, active=%d" % \
                   (layer.name, layer.active))

    def ChangeLayerName (self, layer, name):
        """
        Change name of the layer
        """
        Debug.msg (3, "Map.ChangeLayerName(): from=%s to=%s" % \
                   (layer.name, name))
        layer.name =  name

    def RemoveLayer(self, name=None, id=None):
        """
        Removes layer from list of layers, defined by name@mapset or id

        Parameters:
            name	- map name
            id	- index of the layer in layer list

        Returns:
            Removed layer on success or None
        """

        # del by name
        if name:
            retlayer = None
            for layer in self.layers:
                if layer.name == name:
                    retlayer = layer
                    os.remove(layer.mapfile)
                    os.remove(layer.maskfile)
                    self.layers.remove(layer)
                    return layer
        # del by id
        elif id != None:
            return self.layers.pop(id)

        return None

    def GetLayerIndex(self, layer):
        """
        Returns index of layer in layer list.

        Parameters:
         layer -

        Returns:
         Integer or None
        """

        if layer in self.layers:
            return self.layers.index(layer)
        else:
            return None

    def AddOverlay(self, ovltype=None, type='overlay', command=None,
                   l_active=True, l_hidden=False, l_opacity=1, l_render=False):
        """
        Adds overlay (grid, barscale, others?) to list of overlays

        Overlay Attributes:
            command	   - display command
            l_active   - see MapLayer class
            l_render   - render an image

        Returns:
            Added layer on success or None
        """

        Debug.msg (2, "Map.AddOverlay(): cmd=%s, render=%d" % (command, l_render))
        overlay = MapLayer(type='overlay', name=None, cmd=command,
                           active=l_active, hidden=l_hidden, opacity=l_opacity)

        # add maplayer to the list of layers
        self.overlays.append(overlay)

        if l_render and command != '':
            if not overlay.Render():
                sys.stderr.write("Could not render overlay <%s>\n" % (command))

        self.ovlookup[ovltype] = overlay

        return self.overlays[-1]

    def ChangeOverlay(self, ovltype, type, name, command,
                      l_active=True, l_hidden=False, l_opacity=1, l_render=False):
        """
        Change overlay properities
        """

        newoverlay = MapLayer(type='overlay', name=name, cmd=command,
                              active=l_active, hidden=l_hidden, opacity=l_opacity)

        oldovlindex = self.overlays.index(self.ovlookup[ovltype])

        # add overlay to the list of layers
        if self.ovlookup[ovltype]:
            self.overlays[oldovlindex] = newoverlay
            self.ovlookup[ovltype] = newoverlay

        if l_render and command != '':
            if not overlay.Render():
                sys.stderr.write("Could not render overlay <%s>\n" % (command))

        return self.overlays[-1]

    def changeOverlayActive(self, ovltype, activ):
        """
        Change active status of overlay
        """
        try:
            overlay = self.ovlookup[ovltype]
            overlay.active = activ
            Debug.msg (3, "Map.changeOverlayActive(): type=%d, active=%d" % (type, activ))
        except:
            sys.stderr.write("Cannot change status of overlay index [%d]\n" % type)

    def Clean(self):
        """
        Go trough all layers and remove them from layer list
        Removes also l_mapfile and l_maskfile

        Returns 1 if failed or None if ok
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
    """
    Test of Display class.
    Usage: display=Render()
    """

    print "Initializing..."
    os.system("g.region -d")

    map = Map()
    map.width = 640
    map.height = 480

    map.AddLayer(item=None, type="raster", name="elevation.dem", command = "d.rast elevation.dem@PERMANENT catlist=1000-1500 -i", l_opacity=.7)

    map.AddLayer(item=None, type="vector", name="streams", command = "d.vect streams@PERMANENT color=red width=3 type=line")

    image = map.Render(force=True)

    if image:
        os.system("display %s" % image)
