###############################################################
# gmmenu.tcl - menu file for GRASS GIS Manager
# January 2006 Michael Barton, Arizona State University
# COPYRIGHT:	(C) 1999 - 2006 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
###############################################################

global tmenu
global keyctrl
global execom 
global mon
global filename
global env


# if extensions dir exists: create an "Xtns" menu item
# and read all menu descriptions from .gem files
set dirName [set env(GISBASE)]/etc/gm/Xtns
set XtnsMenu "False"
set splitError "False"

if { [file exists $dirName] && [file isdirectory $dirName] } {	
	lappend listNames "Dummy"; # we need this to check for num of elements later
	foreach fileName [glob -nocomplain [file join $dirName *.gem]] {
		lappend listNames $fileName
	}
	if { [llength $listNames] > 1 } { #only do this, if there is at least one menu file
		set listNames [lreplace $listNames 0 0]; # let's get rid of the dummy element
		set listNames [lsort $listNames]
		#now read each menu file and append to list
		foreach fileName $listNames {
			set inputFile [open $fileName "r"]
			set line [read $inputFile]
			set splitLines [split $line "\n"]
			if { [llength $splitLines] == 1 } {
				# splitting didn't work.
				# maybe we have Mac style newlines, let's split again!
				set splitLines [split $line "\r"]
			}
			# split up into individual lines for processing
			foreach line $splitLines {				
				# strip off comments
				set commentPos [string first "#" $line]
				# 1.: leading comment
				if { $commentPos == 0 } {
					set line ""
				}
				if { $commentPos > 0 } {
					set line [string range $line 0 [expr $commentPos-1]]
				}					
				set line [subst $line]; # substitute variables like $tmenu
				lappend splitLinesDone $line
			}
			# now join individual lines back into one string ...
			set line [join $splitLinesDone]
			# ... and append to list of submenus
			lappend XtnsMenuList [subst {$line}]
			close $inputFile
		}
		set XtnsMenu "True"
	}
}


 set descmenu [subst  {
 "&File" all file $tmenu {
	 {cascad "Import" {} "" $tmenu {			
 		{cascad "Raster map" {} "" $tmenu {			
            {command "Multiple formats using GDAL" {} "r.in.gdal" {} -command { execute r.in.gdal }}
            {separator}
            {command "ASCII GRID (includes GRASS ASCII)" {} "r.in.ascii" {} -command { execute r.in.ascii }}
            {command "Polygons and lines from ASCII file" {} "r.in.poly" {} -command { execute r.in.poly }}
            {separator}
            {command "Binary file (includes GTOPO30 format)" {} "r.in.bin" {} -command { execute r.in.bin }}
            {command "ESRI Arc/Info ASCII grid" {} "r.in.arc" {} -command { execute r.in.arc }}
            {command "GRIDATB.FOR map file (TOPMODEL)" {} "r.in.gridatb" {} -command { execute r.in.gridatb }}
            {command "MAT-File (v.4) array (Matlab or Octave)" {} "r.in.mat" {} -command { execute r.in.mat }}
            {command "SPOT vegetation NDVI data sets" {} "i.in.spotvgt" {} -command { execute i.in.spotvgt }}
            {command "SRTM hgt files" {} "r.in.srtm" {} -command { execute r.in.srtm }}
            {command "Terra ASTER HDF files" {} "r.in.aster" {} -command { execute r.in.aster }}
            {separator}
            {command "Web Mapping Server" {} "r.in.wms" {} -command { execute r.in.wms }}
        }}
        {cascad "Vector map" {} "" $tmenu {			
            {command "Various formats using OGR" {} "v.in.ogr" {} -command { execute v.in.ogr }}
            {separator}
            {command "ASCII points file or GRASS ASCII vector file" {} "v.in.ascii" {} -command { execute v.in.ascii }}
            {command "Import old GRASS vector format" {} "v.convert" {} -command { execute v.convert }}
            {separator}
            {command "dxf file" {} "v.in.dxf" {} -command { execute v.in.dxf }}
            {command "ESRI e00 format" {} "v.in.e00" {} -command { execute v.in.e00 }}
            {command "Garmin GPS Waypoints/Routes/Tracks" {} "v.in.garmin" {} -command { execute v.in.garmin }}
            {command "Garmin GPS Waypoints/Routes/Tracks using GPSBabel" {} "v.in.gpsbabel" {} -command { execute v.in.gpsbabel }}
            {command "GEOnet Name server country files (US-NGA GNS)" {} "v.in.gns" {} -command { execute v.in.gns }}
            {command "Matlab and MapGen files" {} "v.in.mapgen" {} -command { execute v.in.mapgen }}
        }}
        {cascad "Grid 3D" {} "" $tmenu {			
            {command "ASCII 3D file" {} "r3.in.ascii" {} -command { execute r3.in.ascii }}
            {command "Vis5D file" {} "r3.in.v5d" {} -command { execute r3.in.v5d }}
        }}
 	}}
    {cascad "Export" {} "" $tmenu {
        {cascad "Raster map" {} "" $tmenu {
            {command "Multiple formats using GDAL" {} "r.out.gdal" {} -command { execute r.out.gdal }}
            {separator}
            {command "ASCII grid (for GRASS, Surfer, Modflow, etc)" {} "r.out.ascii" {} -command { execute r.out.ascii }}
            {separator}
            {command "ESRI ARC/INFO ASCII grid" {} "r.out.arc" {} -command { execute r.out.arc }}
            {command "GRIDATB.FOR map file (TOPMODEL)" {} "r.out.gridatb" {} -command { execute r.out.gridatb }}
            {command "MAT-File (v.4) array (Matlab or Octave)" {} "r.out.mat" {} -command { execute r.out.mat }}
            {separator}
            {command "Binary file" {} "r.out.bin" {} -command { execute r.out.bin }}
            {separator}
            {command "MPEG-1 animations" {} "r.out.mpeg" {} -command { execute r.out.mpeg }}
            {command "PNG image (not georeferenced)" {} "r.out.png" {} -command { execute r.out.png }}
            {command "PPM image (24bit)" {} "r.out.ppm" {} -command { execute r.out.ppm }}
            {command "PPM image from red, green, blue raster maps" {} "r.out.ppm3" {} -command { execute r.out.ppm3 }}
            {command "POVray height-field" {} "r.out.pov" {} -command { execute r.out.pov }}
            {command "TIFF image (8/24bit)" {} "r.out.tiff" {} -command { execute r.out.tiff }}
            {command "VTK ASCII file" {} "r.out.vtk" {} -command { execute r.out.vtk }}
        }}
        {cascad "Vector map" {} "" $tmenu {			
            {command "Various formats using OGR (SHAPE, MapInfo etc)" {} "v.out.ogr" {} -command { execute v.out.ogr }}
            {separator}
            {command "DXF file (ASCII)" {} "v.out.dxf" {} -command { execute v.out.dxf }}
            {command "ASCII vector or point file/old GRASS ASCII vector file" {} "v.out.ascii" {} -command { execute v.out.ascii }}
            {command "POV-Ray format" {} "v.out.pov" {} -command { execute v.out.pov }}
            {command "VTK ASCII file" {} "v.out.vtk" {} -command { execute v.out.vtk }}
        }}
        {cascad "Grid 3D" {} "" $tmenu {			
            {command "ASCII 3D file" {} "r3.out.ascii" {} -command { execute r3.out.ascii }}
            {command "Vis5D file" {} "r3.out.v5d" {} -command { execute r3.out.v5d }}
            {command "VTK ASCII file" {} "r3.out.vtk" {} -command { execute r3.out.vtk }}
        }}
 	}}
    {separator}
 	{cascad "Manage maps and volumes" {} "" $tmenu {			
        {command "Copy maps" {} "g.copy" {} -command {execute g.copy }}
        {command "List maps" {} "g.list" {} -command {execute g.list}}
        {command "List maps using expressions and 'wildcards'" {} "g.mlist" {} -command {$execom g.mlist }}
        {command "Rename maps" {} "g.rename" {} -command {execute g.rename }}
        {command "Remove maps" {} "g.remove" {} -command {execute g.remove }}
        {command "Remove maps using expressions and 'wildcards'" {} "g.mremove" {} -command {execute g.mremove }}
    }}
 	{cascad "Map type conversions" {} "" $tmenu {			
        {command "Raster to vector map" {} "r.to.vect" {} -command {execute r.to.vect }}
        {command "Raster map series to volume" {} "r.to.rast3" {} -command {execute r.to.rast3 }}
        {command "Vector to raster" {} "v.to.rast" {} -command {execute v.to.rast }}
	{command "Vector to vector" {} "v.type" {} -command {execute v.type }}
        {command "Vector lines to points" {} "v.to.points" {} -command {execute v.to.points }}
        {command "Vector 3D points to volume voxels" {} "v.to.rast3" {} -command {execute v.to.rast3 }}
        {command "Sites (GRASS 5.x) to vector" {} "v.in.sites" {} -command {execute v.in.sites }}
        {command "Volumes to raster map series" {} "r3.to.rast" {} -command {execute r3.to.rast }}
 	}}
    {separator}
    {cascad "Settings" {} "" $tmenu {			
        {command "Open..." {} "Open gis.m settings file" {} -accelerator $keyctrl-O -command { Gm::OpenFileBox }}
        {command "Save" {} "Save gis.m settings file" {} -accelerator $keyctrl-S -command { Gm::SaveFileBox }}
        {command "Save as..." {} "Save gis.m settings file as new name" {} -command { set filename($mon) "" ; Gm::SaveFileBox }}
        {command "Close" {} "Close gis.m settings" {} -accelerator $keyctrl-W -command { GmTree::FileClose {}}}
    }}
    {separator}
    {command "Create ps.map file for postscript printing" {} "ps.map" {} -command { execute ps.map }}
    {separator}
    {command "E&xit" {} "Exit GIS Manager" {} -accelerator $keyctrl-Q -command { exit } }
 }
 "&Config" all options $tmenu {
 	{cascad "Region" {} "" $tmenu {			
        {command "Display region settings" {} "g.region -p" {} -command {run_panel "g.region -p" }}
        {command "Manage region" {} " g.region" {} -command {execute g.region }}
        {command "Zoom to maximum extent of all displayed maps" {} "d.extend" {} -command {run d.extend }}
        {separator}
        {command "Create WIND3 (default 3D window) from current 2D region" {} "g3.createwind" {} -command {execute g3.createwind }}
        {command "Manage 3D region" {} "g3.setregion" {} -command {execute g3.setregion }}
    }}
 	{cascad "GRASS working environment" {} "" $tmenu {			
        {command "Modify access by other users to current mapset" {} "g.access" {} -command {term g.access }}
        {command "Modify mapset search path" {} "g.mapsets.tcl" {} -command {spawn $env(GISBASE)/etc/g.mapsets.tcl}}
        {command "Change current working session to new mapset, location, or GISDBASE" {} "g.mapset" {} -command {execute g.mapset }}
        {command "Show current GRASS environment settings" {} "g.gisenv" {} -command {execute g.gisenv }}
        {command "Show current GRASS version" {} "g.version -c" {} -command {run_panel "g.version -c" }}
 	}}
 	{cascad "Manage projections" {} "" $tmenu {			
        {command "Create/edit projection information for current location" {} "g.setproj" {} -command {term g.setproj }}
        {command "Show projection information and create projection files" {} "g.proj" {} -command {execute g.proj }}
        {separator}
        {command "Convert coordinates from one projection to another" {} "m.proj" {} -command {execute m.proj }}
 	}}
    {cascad "Text" {} "" $tmenu {			
        {command "Select default text font" {} "d.font" {} -command {execute term d.font }}
        {command "Select default freetype text font" {} "" {} -command {execute d.font.freetype }}
        {command "Show standard GRASS fonts" {} "show.fonts.sh" {} -command {$execom show.fonts.sh }}
	 }}
 	{cascad "X-monitor displays" {} "" $tmenu {
        {command "Configure xmonitor displays" {} "d.mon" {} -command {execute d.mon }}
        {command "Configure frames for xmonitors" {} "d.frame" {} -command {execute d.frame }}
        {command "Start/restart xmonitor at specified window size" {} "d.monsize" {} -command {execute d.monsize }}
        {command "Set active xmonitor to specified size" {} "d.resize" {} -command {execute d.resize }}
        {command "Display information about active xmonitor" {} "d.info" {} -command {execute d.info }}
    }}
 } 
 "&Raster" all options $tmenu {
    {cascad "Develop map" {} "" $tmenu {			
        {command "Digitize raster" {} "r.digit" {} -command {Gm::xmon term r.digit }}
        {separator}
        {command "Compress/decompress raster file" {} "r.compress" {} -command {execute r.compress }}
        {command "Manage boundary definitions" {} "r.region" {} -command {execute r.region }}
        {command "Manage null values" {} "r.null" {} -command {execute r.null }}
        {command "Manage timestamps for files" {} "r.timestamp" {} -command {execute r.timestamp }}
        {command "Quantization for floating-point maps" {} "r.quant" {} -command {execute r.quant }}
        {command "Resample (change resolution) using nearest neighbor method" {} "r.resample" {} -command {execute r.resample }}
        {command "Resample (change resolution) using regularized spline tension" {} "r.resamp.rst" {} -command {execute r.resamp.rst }}
        {command "Support file creation and maintenance" {} "r.support" {} -command {execute r.support.sh }}
        {separator}
        {command "Reproject raster from other location" {} "r.proj" {} -command {execute r.proj }}
        {command "Generate tiling for other projection" {} "r.tileset" {} -command {execute r.tileset }}
    }}
    {cascad "Manage map colors" {} "" $tmenu {			
        {command "Set colors to predefined color tables" {} "r.colors" {} -command {execute r.colors }}
        {command "Set colors using color rules" {} "r.colors.rules" {} -command {execute $env(GISBASE)/etc/gm/script/r.colors.rules }}
        {separator}
        {command "Blend 2 color maps to produce 3 RGB files" {} "r.blend" {} -command {execute r.blend }}
        {command "Create color image from RGB files" {} "r.composite" {} -command {execute r.composite }}
        {command "Create 3 RGB (red, green, blue) maps from 3 HIS (hue, intensity, saturation) maps" {} "r.his" {} -command {execute r.his }}
    }}
    {separator}
    {command "Query by coordinate(s)" {} "r.what" {} -command { execute r.what }}
    {separator}
    {command "Create raster buffers" {} "r.buffer" {} -command { execute r.buffer }}
    {command "Create raster MASK" {} "r.mask" {} -command { execute r.mask }}
    {command "Locate closest points between areas in 2 raster maps" {} "r.distance" {} -command { execute r.distance }}
    {command "Map calculator" {} "r.mapcalculator" {} -command { execute r.mapcalculator }}
    {cascad "Neighborhood analysis" {} "" $tmenu {			
        {command "Moving window analysis of raster cells" {} "r.neighbors" {} -command { execute r.neighbors }}
        {command "Analyze vector points in neighborhood of raster cells" {} "v.neighbors" {} -command { execute v.neighbors }}
    }}
    {cascad "Overlay maps" {} "" $tmenu {			
        {command "Cross product" {} "r.cross" {} -command {execute r.cross }}
        {command "Function of map series (time series)" {} "r.series" {} -command {execute r.series }}
        {command "Patch maps" {} "r.patch" {} -command {execute r.patch }}
        {separator}
        {command "Statistical calculations for cover map over base map" {} "r.statistics" {} -command {execute r.statistics }}
    }}
    {cascad "Solar radiance and shadows" {} "" $tmenu {			
        {command "Solar irradiance and daily irradiation" {} "r.sun" {} -command {execute r.sun }}
        {command "Shadows map for sun position or date/time" {} "r.sunmask" {} -command {execute r.sunmask }}
    }}
    {cascad "Terrain analysis" {} "" $tmenu {			
        {command "Calculate cumulative movement costs between locales" {} "r.walk" {} -command {execute r.walk }}
        {command "Cost surface" {} "r.cost" {} -command {execute r.cost }}
        {command "Least cost route or flow" {} "r.drain" {} -command {execute r.drain }}
        {command "Profile analysis" {} "d.profile" {} -command {Gm::xmon run d.profile }}
        {command "Shaded relief map" {} "r.shaded.relief" {} -command {execute r.shaded.relief }}
        {command "Slope and aspect" {} "r.slope.aspect" {} -command {execute r.slope.aspect }}
        {command "Terrain parameters" {} "r.param.scale" {} -command {execute r.param.scale }}
        {command "Textural features" {} "r.texture" {} -command {execute r.texture }}
        {command "Visibility/line of sight" {} "r.los" {} -command {execute r.los }}
    }}
    {cascad "Transform features" {} "" $tmenu {			
        {command "Clump small areas (statistics calculated by r.volume)" {} "r.clump" {} -command {execute r.clump }}
        {command "Grow areas" {} "r.grow2" {} -command {execute r.grow2 }}
        {command "Thin linear features" {} "r.thin" {} -command {execute r.thin }}
    }}
    {separator}
    {cascad "Hydrologic modeling" {} "" $tmenu {			
        {command "Carve stream channels into elevation map using vector streams map" {} "r.carve" {} -command {execute r.carve }}
        {command "Depressionless elevation map and flowline map" {} "r.fill.dir" {} -command {execute r.fill.dir }}
        {command "Flow accumulation for massive grids" {} "r.terraflow" {} -command {spawn r.terraflow }}
        {command "Generate flow lines for raster map" {} "r.flow" {} -command {exec r.flow > /dev/null & }}
        {command "SIMWE overland flow modeling" {} "r.sim.water" {} -command {execute r.sim.water }}
        {command "SIMWE sediment erosion, transport, & deposition modeling" {} "r.sim.sediment" {} -command {execute r.sim.sediment }}
        {command "Topographic index map" {} "r.topidx" {} -command {execute r.topidx }}
        {command "TOPMODEL simulation" {} "r.topmodel" {} -command {execute r.topmodel }}
        {command "Watershed subbasins" {} "r.basins.fill" {} -command {execute r.basins.fill }}
        {command "Watershed analysis" {} "r.watershed" {} -command {execute r.watershed }}
        {command "Watershed basin creation" {} "r.water.outlet" {} -command {execute r.water.outlet }}
    }}
    {cascad "Landscape structure modeling" {} "" $tmenu {			
        {command "Set up sampling and analysis framework" {} "r.le.setup" {} -command {term r.le.setup }}
        {separator}
        {command "Analyze landscape characteristics" {} "r.le.pixel" {} -command {execute r.le.pixel }}
        {command "Analyze landscape patch characteristics" {} " r.le.patch" {} -command {execute r.le.patch }}
        {command "Output landscape patch information" {} "r.le.trace" {} -command {execute r.le.trace }}
    }}
    {cascad "Wildfire modeling" {} "" $tmenu {			
        {command "Generate rate of spread (ROS) maps" {} "r.ros" {} -command {execute r.ros }}
        {command "Generate least-cost spread paths" {} "r.spreadpath" {} -command {execute r.spreadpath }}
        {command "Simulate anisotropic spread phenomena" {} "r.spread" {} -command {execute r.spread }}
    }}
    {separator}
    {cascad "Change category values and labels" {} "" $tmenu {			
        {command "Edit category values of individual cells for displayed raster map" {} "d.rast.edit" {} \
        	-command {Gm::xmon term d.rast.edit }}
        {separator}
        {command "Reclassify categories for areas of specified sizes" {} "r.reclass.area" {} -command {execute r.reclass.area }}
        {command "Reclassify categories using rules" {} "r.reclass.rules" {} -command {execute $env(GISBASE)/etc/gm/script/r.reclass.rules }}
        {command "Reclassify categories using rules file" {} "r.reclass.file" {} -command {execute $env(GISBASE)/etc/gm/script/r.reclass.file }}
        {separator}
        {command "Recode categories using rules (create new map)" {} "r.recode.rules" {} -command {execute $env(GISBASE)/etc/gm/script/r.recode.rules }}
        {command "Recode categories using rules file (create new map)" {} "r.recode.file " {} -command {execute $env(GISBASE)/etc/gm/script/r.recode.file }}
        {separator}
        {command "Rescale categories (create new map)" {} "r.rescale" {} -command {execute r.rescale }}
        {command "Rescale categories with equalized histogram (create new map)" {} "r.rescale.eq" {} -command {execute r.rescale.eq }}
    }}
    {separator}
    {command "Generate concentric circles around points" {} "r.circle" {} -command { execute r.circle }}
    {cascad "Generate random raster cells" {} "" $tmenu {			
        {command "Generate random cells" {} "r.random.cells" {} -command {execute r.random.cells }}
        {command "Generate random cells and vector points from raster map" {} "r.random" {} -command {execute r.random }}
    }}
    {cascad "Generate surfaces" {} "" $tmenu {			
        {command "Generate density surface using moving Gausian kernal" {} "v.kernel" {} -command {execute v.kernel }}
        {command "Generate fractal surface" {} "r.surf.fractal" {} -command {execute r.surf.fractal }}
        {command "Generate gaussian deviates surface" {} "r.surf.gauss" {} -command {execute r.surf.gauss }}
        {command "Generate plane" {} "r.plane" {} -command {execute r.plane }}
        {command "Generate random deviates surface" {} "r.surf.random" {} -command {execute r.surf.random }}
        {command "Generate random surface with spatial dependence" {} "r.random.surface" {} -command {execute r.random.surface }}
    }}
    {command "Generate vector contour lines" {} "r.contour" {} -command { execute r.contour }}
    {cascad "Interpolate surfaces" {} "" $tmenu {			
        {command "Bilinear interpolation from raster points" {} "r.bilinear" {} -command { execute r.bilinear }}
        {command "Inverse distance weighted interpolation from raster points" {} "r.surf.idw" {} -command { execute r.surf.idw }}
        {command "Interpolation from raster contours" {} "r.surf.contour" {} -command { execute r.surf.contour }}
        {separator}
        {command "Inverse distance weighted interpolation from vector points" {} "v.surf.idw" {} -command { execute v.surf.idw }}
        {command "Regularized spline tension interpolation from vector points or contours" {} "v.surf.rst" {} -command { execute v.surf.rst }}
        {separator}
        {command "Fill NULL cells by interpolation using regularized spline tension" {} " r.fillnulls" {} -command {execute r.fillnulls }}
    }}
    {separator}
    {cascad "Reports and statistics" {} "" $tmenu {			
        {command "Report basic file information" {} "r.info" {} -command {execute r.info }}
        {command "Report category labels and values" {} "r.cats" {} -command {execute r.cats }}
        {separator}
        {command "General statistics" {} "r.stats" {} -command {execute r.stats }}
        {command "Range of all category values" {} "r.describe" {} -command {execute r.describe }}
        {command "Sum all cell category values" {} "r.sum" {} -command {execute r.sum }}
        {command "Sum area by map and category" {} "r.report" {} -command {execute r.report }}
        {command "Summary statistics for clumped cells (works with r.clump)" {} "r.volume" {} -command {execute r.volume }}
        {command "Total surface area corrected for topography" {} "r.surf.area" {} -command {execute r.surf.area }}
        {command "Univariate statistics" {} "r.univar" {} -command {execute r.univar }}
        {command "Univariate statistics (script version)" {} " r.univar.sh" {} -command {execute r.univar.sh }}
        {separator}
        {command "Sample values along transects" {} "r.profile" {} -command {execute r.profile }}
        {command "Sample values along transects (use azimuth, distance)" {} " r.transect" {} -command {execute r.transect }}
        {separator}
        {command "Covariance/correlation" {} "r.covar" {} -command {execute r.covar }}
        {command "Linear regression between 2 maps" {} "r.regression.line" {} -command {execute r.regression.line }}
        {command "Mutual category occurences (coincidence)" {} "r.coin" {} -command {execute r.coin }}
    }}
 } 
 "&Vector" all options $tmenu {
			{cascad "Develop map" {} "" $tmenu {			
			 {command "Digitize" {} "v.digit" {} -command {Gm::xmon run v.digit }}
			 {separator}
			 {command "Create/rebuild topology" {} "v.build" {} -command {execute v.build }}
			 {command "Clean vector files" {} "v.clean" {} -command {execute v.clean }}
			 {separator}
			 {command "Break lines at intersections" {} "v.topo.check" {} -command {execute v.topo.check }}
			 {command "Build polylines from adjacent segments" {} "v.build.polylines" {} -command {execute v.build.polylines }}
			 {command "Split polylines into segments" {} "v.segment" {} -command {execute v.segment }}
			 {command "Create lines parallel to existing lines" {} "v.parallel" {} -command {execute v.parallel }}
			 {separator}
			 {command "Convert vector feature types" {} "v.type" {} -command {execute v.type }}
			 {command "Convert 2D vector to 3D by sampling raster" {} "v.drape" {} -command {execute v.drape }}
			 {command "Extrude 2D vector into 3D vector" {} "v.extrude" {} -command {execute v.extrude }}
			 {separator}
			 {command "Create text label file for vector features" {} "v.label" {} -command {execute v.label }}
			 {separator}
			 {command "Reproject vector from other location" {} "v.proj" {} -command {execute v.proj }}
			}}
			{cascad "Vector<->database connections" {} "" $tmenu {			
			 {command "Create new vector as link to external OGR layer" {} "v.external" {} -command {execute v.external }}
			 {command "Set database connection for vector attributes" {} "v.db.connect" {} -command {execute v.db.connect }}
			}}
			{command "Rectify and georeference vector map" {} "v.transform" {} -command {execute v.transform }}
			{separator}
			{command "Query by attributes" {} "v.extract" {} -command {execute v.extract }}
    		{command "Query by coordinate(s)" {} "v.what" {} -command { execute v.what }}
			{command "Query by map features" {} " v.select" {} -command {execute v.select }}
			{separator}
			{command "Create vector buffers" {} "v.buffer" {} -command {execute v.buffer }}
			{cascad "Neighborhood analysis" {} "" $tmenu {			
			 {command "Locate nearest features to points or centroids" {} "v.distance" {} -command {execute v.distance }}
			 {command "Generate Thiessen polygons around points (Voronoi diagram)" {} "v.voronoi" {} -command {execute v.voronoi }}
			 {command "Connect points to create Delaunay triangles" {} "v.delauney" {} -command {execute v.delaunay }}
			}}
			{cascad "Network analysis" {} "" $tmenu {			
			 {command "Allocate subnets" {} "v.net.alloc" {} -command {execute v.net.alloc }}
			 {command "Network maintenance" {} "v.net" {} -command {execute v.net }}
			 {command "Shortest route" {} "v.net.path" {} -command {execute v.net.path }}
			 {command "Shortest route (visualization only)" {} "d.path" {} -command {Gm::xmon run d.path }}
			 {command "Split net to bands between cost isolines" {} "v.net.iso" {} -command {execute v.net.iso }}
			 {command "Steiner tree" {} "v.net.steiner" {} -command {execute v.net.steiner }}
			 {command "Traveling salesman analysis" {} "v.net.salesman" {} -command {execute v.net.salesman }}
			}}
			{cascad "Overlay maps" {} "" $tmenu {			
			 {command "Overlay/combine 2 vector maps" {} "v.overlay" {} -command {execute v.overlay }}
			 {command "Patch multiple maps (combine)" {} "v.patch" {} -command {execute v.patch }}
			}}
			{command "Generate area feature for extent of current region" {} "v.in.region" {} -command {execute v.in.region }}
			{command "Generate rectangular vector grid" {} "v.mkgrid" {} -command {execute v.mkgrid }}
			{separator}
			{cascad "Change attributes" {} "" $tmenu {			
			 {command "Attach, delete, or report categories" {} "v.category" {} -command {execute v.category }}
			 {command "Reclassify features using rules file" {} "v.reclass" {} -command {execute v.reclass }}
			}}
			{separator}
			{cascad "Work with vector points" {} "" $tmenu {			
 			{cascad "Generate points" {} "" $tmenu {			
			 {command "Generate points from database with x/y coordinates" {} "v.in.db" {} -command {execute v.in.db }}
 			 {command "Generate random points" {} "v.random" {} -command {execute v.random }}
 			 {command "Random location perturbations of points" {} "v.perturb" {} -command {execute v.perturb }}
 			}}
 			{cascad "Generate areas from points" {} "" $tmenu {			
 			 {command "Generate convex hull for point set" {} "v.hull" {} -command {execute v.hull }}
 			 {command "Generate Delaunay triangles for point set" {} "v.delaunay" {} -command {execute v.delaunay }}
 			 {command "Generate Voronoi diagram/Thiessen polygons for point set" {} "v.voronoi" {} -command {execute v.voronoi }}
 			}}
 			{cascad "Sample raster maps" {} "" $tmenu {			
 			{command "Calculate statistics for raster map overlain by vector map" {} "v.rast.stats" {} -command {execute v.rast.stats }}
 			{command "Sample raster map at point locations" {} "v.what.rast" {} -command {execute v.what.rast }}
 			{command "Sample raster neighborhood around points" {} "v.sample" {} -command {execute v.sample }}
 			}}
 			{command "Partition points into test/training sets for k-fold cross validatation" {} "v.kcv" {} -command {execute v.kcv }}
 			{command "Transfer attribute data from queried vector map to points" {} "v.what.vect" {} -command {execute v.what.vect }}
			}}
			{separator}
			{cascad "Reports and statistics" {} "" $tmenu {			
			 {command "Basic information" {} "v.info" {} -command {execute v.info }}
			 {command "Load vector attributes to database or create reports" {} "v.to.db" {} -command {execute v.to.db }}
			 {command "Report areas for vector attribute categories" {} "v.report" {} -command {execute v.report }}
			 {command "Univariate statistics" {} "v.univar" {} -command {execute v.univar }}
 			{separator}
			 {command "Test normality of point distribution" {} "v.normal" {} -command {execute v.normal }}
			 {command "Calculate stats for raster map underlying vector objects" {} "v.rast.stats" {} -command {execute v.rast.stats }}
			 {command "Indices of point counts in quadrats" {} "v.qcount" {} -command {execute v.qcount }}
			}}
 } 
 "&Image" all options $tmenu {			
			{cascad "Develop images and groups" {} "" $tmenu {			
			 {command "Create/edit imagery group" {} "i.group" {} -command {execute i.group }}			
			 {command "Target imagery group" {} "i.target" {} -command {execute i.target }}
			 {separator}
			 {command "Mosaic up to 4 adjacent images" {} "i.image.mosaic" {} -command {execute i.image.mosaic }}
			}}
			{cascad "Manage image colors" {} "" $tmenu {			
			 {command "Color balance and enhance color tables of multiband imagery for rgb display" {} "i.landsat.rgb" {} -command {execute i.landsat.rgb }}
			 {command "Transform HIS (Hue/Intensity/Saturation) color image to RGB (Red/Green/Blue)" {} "i.his.rgb" {} -command {execute i.his.rgb }}
			 {command "Transform RGB (Red/Green/Blue) color image to HIS (Hue/Intensity/Saturation)" {} "i.rgb.his" {} -command {execute i.rgb.his }}
			}}
			{cascad "Rectify and georeference image group" {} "" $tmenu {			
			 {command "Set ground control points (GCP's) from raster map or keyboard entry" {} "i.points" {} \
			 	-command {Gm::xmon term i.points }}
			 {command "Set ground control points (GCP's) from vector map or keyboard entry" {} "i.vpoints" {} \
			 	-command {Gm::xmon term i.vpoints }}
			 {command "Affine and Polynomial rectification (rubber sheet)" {} "i.rectify" {} -command {execute i.rectify }}
			 {command "Ortho photo rectification" {} "i.ortho.photo" {} -command {term i.ortho.photo }}
			}}
			{separator}
			{command "Brovey transformation and pan sharpening" {} "i.fusion.brovey" {} -command {execute i.fusion.brovey }}
			{cascad "Classify image" {} "" $tmenu {			
			 {command "Clustering input for unsupervised classification" {} "i.cluster" {} -command {execute i.cluster }}
			 {separator}
			 {command "Maximum likelyhood classification (MLC)" {} "i.maxlik" {} -command {execute i.maxlik }}
			 {command "Sequential maximum a posteriory classification (SMAP)" {} "i.smap" {} -command {execute i.smap }}
			 {separator}
			 {command "Interactive input for supervised classification" {} "i.class" {} -command {term i.class }}
			 {command "Non-interactive input for supervised classification (MLC)" {} "i.gensig" {} -command {execute i.gensig }}
			 {command "Non-interactive input for supervised classification (SMAP)" {} "i.gensigset" {} -command {execute i.gensigset }}
			}}
			{cascad "Filter image" {} "" $tmenu {			
			 {command "Zero edge crossing detection" {} "i.zc" {} -command {execute i.zc }}
			 {command "User defined matrix/convolving filter" {} "r.mfilter" {} -command {execute r.mfilter }}
			}}
			{command "Spectral response" {} "i.spectral" {} -command {execute i.spectral }}
			{command "Tassled cap vegetation index" {} "i.tasscap" {} -command {execute i.tasscap }}
			{cascad "Transform image" {} "" $tmenu {			
			 {command "Canonical component" {} "i.cca" {} -command {execute i.cca }}
			 {command "Principal component" {} "i.pca" {} -command {execute i.pca }}
			 {command "Fast Fourier Transform" {} "i.fft" {} -command {execute i.fft }}
			 {command "Inverse Fast Fourier Transform" {} "i.ifft" {} -command {execute i.ifft }}
			}}
			{separator}
			{cascad "Reports and statistics" {} "" $tmenu {			
			 {command "Report basic file information" {} "r.info" {} -command {execute r.info }}
			 {command "Range of image values" {} "r.describe" {} -command {execute r.describe }}
			 {separator}
			 {separator}
			 {command "Bit pattern comparison for ID of low quality pixels" {} "r.bitpattern" {} -command {execute r.bitpattern }}
			 {command "Kappa classification accuracy assessment" {} "r.kappa" {} -command {execute r.kappa }}
			 {command "Optimum index factor for LandSat TM" {} "i.oif" {} -command {execute i.oif }}
			}}
 } 
 "&Grid3D" all options $tmenu {
			{cascad "Develop grid3D volumes" {} "" $tmenu {			
			 {command "Manage nulls for grid3D volume" {} "r3.null" {} -command {execute r3.null }}
			 {command "Manage timestamp for grid3D volume" {} "r3.timestamp" {} -command {execute r3.timestamp }}
			}}
			{command "Create 3D mask for grid3D operations" {} "r3.mask" {} -command {execute r3.mask }}
			{command "Create display file for grid3D volume" {} "r3.mkdspf" {} -command { execute r3.mkdspf }}
			{command "Map calculator for grid3D operations" {} "r3.mapcalculator" {} -command {execute r3.mapcalculator }}
			{command "Interpolate volume from vector points using splines" {} "v.vol.rst" {} -command {execute v.vol.rst }}
			{cascad "Report and Statistics" {} "" $tmenu {			
			 {command "Display information about grid3D volume" {} "r3.info" {} -command {execute r3.info }}
			}}
 } 
 "&Databases" all options $tmenu {
			{cascad "Manage database" {} "" $tmenu {			
			 {command "Connect to database" {} "db.connect" {} -command {execute db.connect }}
			 {command "PERMANTLY remove table" {} "db.droptable" {} -command {execute db.droptable }}
			 {command "Copy table" {} "db.copy" {} -command {execute db.copy }}
			 {command "Add columns to table" {} "v.db.addcol" {} -command {execute v.db.addcol }}
			 {command "Change values in a column" {} "v.db.update" {} -command {execute v.db.update }}
			 {command "Test database" {} "db.test" {} -command {execute db.test }}
			}}
			{cascad "Database information" {} "" $tmenu {			
			 {command "Describe table" {} "db.describe" {} -command {execute db.describe }}
			 {command "List columns" {} "db.columns" {} -command {execute db.columns }}
			 {command "List drivers" {} "db.drivers" {} -command {execute db.drivers }}
			 {command "List tables" {} "db.tables" {} -command {execute db.tables }}
			}}
			{separator}
			{cascad "Query" {} "" $tmenu {			
			 {command "Query data (SQL select)" {} "db.select" {} -command {execute db.select }}
			 {command "Execute SQL statement" {} "db.execute" {} -command {execute db.execute }}
			}}
 } 

 }]

# Should we add an Xtns menu entry?
if { $XtnsMenu == "True" } {
	# build extension menu
	set XtnsMenuStart "\"&Xtns\" all options $tmenu \{"
	set XtnsMenuEnd " \}"	
	set descmenu "$descmenu $XtnsMenuStart"
	foreach XtnsMenuItem $XtnsMenuList {
	 	set descmenu "$descmenu [subst {{$XtnsMenuItem}}]"
	}	
	set descmenu "$descmenu $XtnsMenuEnd"
}

# add help menu to the end of the menubar 
set descmenu "$descmenu [subst {  
 	 "&Help" all options $tmenu {
    {command "GRASS help" {} "g.manual" {} -command { exec g.manual -i > /dev/null & } }
    {command "GIS Manager &help" {} "GIS Manager help" {} -command { exec g.manual gis.m > /dev/null & } }
    {command "About &GRASS" {} "About GRASS" {} -command { source $env(GISBASE)/etc/gm/grassabout.tcl} }
    {command "About &System" {} "About System" {} -command { exec $env(GRASS_WISH) $env(GISBASE)/etc/gm/tksys.tcl --tcltk & }}
    }}
]"

 
