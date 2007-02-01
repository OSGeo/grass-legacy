import os

"""gism.py module to run GRASS display commands and render them to *.ppm files.
Also sets region geometry for zooming and panning"""

# Authors: Jachym Cepicky with modifications by Michael Barton
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team

class Render:
    """
    This class has functions and variables for creating temporary ppm files for
    display in python canvas.
    """
    def __init__(self):
	self.region = {}
	self.grassEnvVars = {}
	self.WIND = {}
	self.debug = None
	self.lastRegion = {}
	self.lastCommand = None
	self.res = None
	self.geom = []
	self.alignRegion = False

	# file name where the resulting map in ppm format will be stored
	#gtemp = 'g.tempfile pid='+repr(os.getpid())
	gtemp = os.popen("g.tempfile pid=%d" % \
	    os.getpid()).readlines()[0].strip()
	self.mapFile = gtemp+".ppm"
	self.maskFile = gtemp+".pgm"

    def display_background(self, command, forceredraw=False):
	"""
	* Set geometry,
	* run GRASS display command,
	* output will be to /tmp
	* show

	the variable is called self.backgroundImage, which is gtk.Image
	"""
	if self.debug > 1:
	    print "REGION:     ", self.region,self.res
	    print "LASTREGION: ", self.lastRegion

	# if no command is set, no reason to continue
	if not command:
	    return

	# if regions are the same, no reason to continue
	# if last command was the same, no reason to continue
	if self.lastRegion != {}:
	    if self.lastRegion['e'] == self.region['e'] and\
	    self.lastRegion['n'] == self.region['n'] and\
	    self.lastRegion['w'] == self.region['w'] and\
	    self.lastRegion['s'] == self.region['s'] and \
	    self.lastCommand == command:
		if forceredraw != True:
		    return self.mapFile, self.maskFile

	# copy this region to last region
	for key in self.region:
	    self.lastRegion[key] = self.region[key]

	# copy to old command
	self.lastCommand = command

	# set GRASS_REGION env variable
	grass_region = ""
	for key in self.WIND.keys():
	    if key == 'north':
		grass_region += "north: %s; " % self.region['n']
		continue
	    elif key == "south":
		grass_region += "south: %s; " % self.region['s']
		continue
	    elif key == "east":
		grass_region += "east: %s; " % self.region['e']
		continue
	    elif key == "west":
		grass_region += "west: %s; " % self.region['w']
		continue
	    elif key == "e-w resol":
		grass_region += "e-w resol: %f; " % (self.res)
		continue
	    elif key == "n-s resol":
		grass_region += "n-s resol: %f; " % (self.res)
		continue
	    elif key == "cols":
		grass_region += 'cols: %d; ' % (self.geom[0])
		continue
	    elif key == "rows":
		grass_region += 'rows: %d; ' % (self.geom[1])
		continue
	    else:
		grass_region += key+": "+self.WIND[key]+"; "
	os.putenv("GRASS_REGION",grass_region)

	# set GRASS region, if you have to
	if self.alignRegion:
	    os.system("g.region n=%f s=%f e=%f w=%f" %\
		(self.region['n'],self.region['s'],
		    self.region['e'],self.region['w']))

	# make the image: Note os.putenv may be unstable in Mac OS X
	os.system("d.mon stop=gism > /dev/null 2>&1")
	#os.environ["GRASS_TRUECOLOR"] = "TRUE"
	os.putenv("GRASS_TRUECOLOR","TRUE")
	os.putenv("GRASS_TRANSPARENT","TRUE")
	os.putenv("GRASS_BACKGROUNDCOLOR","ffffff")
	os.putenv("GRASS_PNG_AUTO_WRITE", "TRUE")
	os.putenv("GRASS_PNGFILE",self.mapFile)
	os.putenv("GRASS_WIDTH",repr(int(self.geom[0])))
	os.putenv("GRASS_HEIGHT",repr(int(self.geom[1])))
	os.system("d.mon start=gism > /dev/null 2>&1 ")
	os.system("%s > /dev/null 2>&1" % command)

	# clean
        print grass_region
	os.unsetenv("GRASS_REGION")
	return self.mapFile, self.maskFile

    def getRegion(self,):
	"""
	g.region -g stored in self.region and self.res
	g.gisenv stored to self.grassEnvVars
	WIND of curent region to self.WIND
	"""
	# region
	for line in os.popen("g.region -gu","r").readlines():
	    line = line.strip()
	    key,value = line.split("=")
	    self.region[key] = float(value)

	# grass env. variables
	for line in os.popen("g.gisenv","r").readlines():
	    line = line.strip()
	    key,value = line.split("=")
	    value = value.replace(";","")
	    value = value.replace("'","")
	    self.grassEnvVars[key] = value

	# wind
	# FIXME: duplicated region WIND == g.region (at least some values)
	windfile = os.path.join(self.grassEnvVars['GISDBASE'],
	    self.grassEnvVars['LOCATION_NAME'],
	    self.grassEnvVars['MAPSET'],
	    "WIND")
	windfile = open(windfile,"r")
	for line in windfile.readlines():
	    line = line.strip()
	    key,value = line.split(":")
	    key = key.strip()
	    value = value.strip()
	    self.WIND[key] = value
	return

    #
    # Calculate current resolution
    def getResolution(self):
	"""
	Calculates current (drawing area) resolution and store it to
	self.res. Previous resolution will be stored to self.lastRes
	"""

	# store the previous resolution
	self.lastRes = self.res
	self.res = abs(float(self.region['w']-self.region['e']))/float(self.geom[0])

	if self.debug > 1:
	    print "RESOLUTION: ", self.region, self.geom

    #
    # coordinates to pixel
    #
    def cell2pixel(self,x,y):
	"""
	Calculates image coordinates to real word coordinates

	Inputs: x,y
	Outputs: float x, foat y
	"""
	newx = (x - self.region['w'])/self.res
	newy = (y - self.region['s'] )/self.res
	newy = self.geom[1] - newy
	return newx,newy

    #
    # image to coordinates
    #
    def pixel2cell(self,x,y):
	"""
	Calculates real word coordinates to image coordinates

	Inputs: x,y
	Outputs: int x, int y
	"""
	newx = self.region['w']+x*self.res
	newy = self.region['s']+ (self.geom[1] - y)*self.res
	return newx,newy


    def zoom(self,begin,end,zoomtype):
	x1,y1,x2,y2 = begin[0],begin[1],end[0],end[1]
	newreg = {}

	# threshold - too small squares do not make sense
	# can only zoom to windows of > 10x10 screen pixels
	if x2 > 10 and y2 > 10 and zoomtype != 0:

	    if x1 > x2:
		x1,x2 = x2,x1
	    if y1 > y2:
		y1,y2 = y2,y1
	    # zoom in
	    if zoomtype > 0:
		newreg['w'],newreg['n'] = self.pixel2cell(x1,y1)
		newreg['e'],newreg['s'] = self.pixel2cell(x2,y2)

	    # zoom out
	    elif zoomtype < 0:
		newreg['w'],newreg['n'] = self.pixel2cell(
		    -x1*2,
		    -y1*2
		    )
		newreg['e'],newreg['s'] = self.pixel2cell(
		    self.geom[0]+2*(self.geom[0]-x2),
		    self.geom[1]+2*(self.geom[1]-y2)
		    )
	# pan
	elif zoomtype == 0:
	    newreg['w'],newreg['n'] = self.pixel2cell(
		x1-x2,
		y1-y2
		)
	    newreg['e'],newreg['s'] = self.pixel2cell(
		self.geom[0]+x1-x2,
		self.geom[1]+y1-y2
		)

	# if new region has been calculated, set the values
	if not newreg == {}:
	    self.region['n'] = newreg['n']
	    self.region['s'] = newreg['s']
	    self.region['e'] = newreg['e']
	    self.region['w'] = newreg['w']
	    # set new resolution to self.res
	    self.getResolution()

    # storing values of map display with focus

    def setMdIdx(self, idx):
	global mdidx
	mdidx = idx

    def getMdIdx(self):
	global mdidx
	return mdidx

    def setMD(self, md):
	global currmap
	currmap = md

    def getMD(self):
	global currmap
	return currmap


