proc read_moncap {} {
	global env moncap

	set file [open [file join $env(GISBASE) etc monitorcap] r]
	set data [read $file]
	close $file

	set data [subst -nocommands -novariables $data]
	set moncap {}
	foreach line [split $data \n] {
		if {[string match {\#*} $line]} continue
		if {![string match {*:*:*:*:*:*} $line]} continue
		set fields {}
		foreach field [split $line :] {
			lappend fields [string trim $field]
		}
		lappend moncap $fields
	}
}

proc monitor_menu {op} {
	global moncap

	set submenu {}
	set last_driver {}
	foreach mon $moncap {
		set name [lindex $mon 0]
		set driver [lindex $mon 1]
		if {$last_driver != "" && $last_driver != $driver} {
			lappend submenu -separator "" ""
		}
		set last_driver $driver
		lappend submenu $name "" [list "run d.mon $op=$name"]
	}

	return $submenu
}

frame .main_menu
pack .main_menu -expand yes -fill both

# tcltkgrass menu.tcl v 4.1 for GRASS 5.7 2004/08/17 Michael Barton
# based on menu.tcl for GRASS 5.3 by Michael Barton, Jacques Bouchard, and Markus Neteler

# main menu

read_moncap

menu_build 1 .main_menu [subst {
"File" "Files in/out" {
	"Import" "Import maps into GRASS" {
	    "Raster map" "" {
		"Multiple formats using GDAL" "r.in.gdal"
		    {"execute r.in.gdal"}
		-separator "" ""
		"ASCII GRID (includes GRASS ASCII)" "r.in.ascii"
		    {"execute r.in.ascii"}
		"GRASS ASCII vector map" "r.in.poly"
		    {"execute r.in.poly"}
		-separator "" ""
		"Binary file (includes GTOPO30 format)" "r.in.bin"
		    {"execute r.in.bin"}
		"ERDAS LAN" "i.in.erdas"
		    {"execute i.in.erdas"}
		"GRIDATB.FOR map file (TOPMODEL)" "r.in.gridatb"
		    {"execute r.in.gridatb"}
		"MAT-File (v.4) map file (Matlab or Octave)" "r.in.gridatb"
		    {"execute r.in.mat"}
	    }
	    "Vector map" "" {
		"Various formats using OGR" "v.in.ogr"
		    {"execute v.in.ogr"}
		-separator "" ""
		"ASCII points file" ""
		    {"execute v.in.asciipoints"}
		"GRASS vector file" "v.in.ascii"
		    {"execute v.in.ascii"}
		"Import/export old GRASS vector format" "v.convert"
		    {"execute v.convert"}
		-separator "" ""
		"Garmin GPS Waypoints/Routes/Tracks" "v.in.garmin"
		    {"execute v.in.garmin"}
		"MATLAB MapGen files" "v.in.mapgen.sh"
		    {"execute v.in.mapgen.sh"}
		-separator "" ""
		"ASCII points file to GRASS sites format" "s.in.ascii"
		    {"execute s.in.ascii"}
	    }
	    "Grid 3D" "" {
		"ASCII 3D file" "r3.in.ascii"
		    {"execute r3.in.ascii"}
		"Vis5D file" "r3.in.v5d"
		    {"execute r3.in.v5d"}
	    }
	}
	"Export" "Export maps from GRASS" {
	    "Raster map" "" {
		"ASCII grid (for GRASS, Surfer, Modflow, etc)" "r.out.ascii"
		    {"execute r.out.ascii"}
		-separator "" ""
		"ESRI ARC/INFO ASCII grid" "r.out.arc"
		    {"execute r.out.arc"}
		"ERDAS LAN file" "i.out.erdas"
		    {"execute i.out.erdas"}
		"GRIDATB.FOR map file (TOPMODEL)" "r.out.gridatb"
		    {"execute r.out.gridatb"}
		"MAT-File (v.4) map file (Matlab or Octave)" "r.out.mat"
		    {"execute r.out.mat"}
		-separator "" ""
		"Binary file" "r.out.bin"
		    {"execute r.out.bin"}
		-separator "" ""
		"MPEG-1 animations" "r.out.mpeg"
		    {"execute r.out.mpeg"}
		"PNG image (not georeferenced)" "r.out.png"
		    {"execute r.out.png"}
		"PPM image (24bit)" "r.out.ppm"
		    {"execute r.out.ppm"}
		"PPM image from red, green, blue raster maps" "r.out.ppm3"
		    {"execute r.out.ppm3"}
		"POVray height-field" "r.out.pov"
		    {"execute r.out.pov"}
		"TIFF image (8/24bit)" "r.out.pov"
		    {"execute r.out.tiff"}
	    }
	    "Vector map" "" {
		"Various formats using OGR (SHAPE, MapInfo etc)" "v.out.ogr"
		    {"execute v.out.ogr"}
		-separator "" ""
		"GRASS vector file" "v.out.ascii"
		    {"execute v.out.ascii"}
		"Import/export old GRASS vector format" "v.convert"
		    {"execute v.convert"}
		"POV-Ray format" "v.out.pov"
		    {"execute v.out.pov"}
		-separator "" ""
		"ASCII points file from GRASS sites file" "s.out.ascii"
		    {"execute s.out.ascii"}
	    }
	    "Grid 3D" "" {
		"ASCII 3D file" "r3.out.ascii"
		    {"execute r3.out.ascii"}
		"Vis5D file" "r3.out.v5d"
		    {"execute r3.out.v5d"}
	    }
	}
	"Save display to image file" "" {
	    "XWD (Save display, selected with mouse, to map.xwd in home directory )" ""
	        {"spawn xwd -out map.xwd"}
	    "PNG (save currently selected display to 24 bit PNG file)" ""
	        {"execute d.out.png"}
	}
	"Postscript map creation" "ps.map"
	    {"execute ps.map"}
	"Print (Use display manager)" "" {"do_nothing"}
	"Quit tcltkgrass" "" {"resize_menu;quit"}
}
"GIS" "Manage GRASS GIS files" {
	"Maps & grid3D files" "Map management (map files operations)" {
	    "Copy maps" ""
	        {"execute g.copy"}
	    "List maps" ""
	        {"execute g.list"}
	    "List maps using expressions & 'wildcards'" ""
	        {"execute g.mlist"}
	    "Rename maps" ""
	        {"execute g.rename"}
	    "Remove maps" ""
	        {"execute g.remove"}
	    "Remove maps using expressions & 'wildcards'" ""
	        {"execute g.mremove"}
	    -separator "" ""
	    "List Grid3D files" ""
	        {"execute g3.list"}
	    "Rename Grid3D volumes" ""
	        {"execute g3.rename"}
	    "Remove Grid3D volumes" ""
	        {"execute g3.remove"}
	    -separator "" ""
	    "Modify access to current mapset" ""
	        {"execute g.access"}
	    "Modify mapset search path" ""
	        {"execute g.mapsets"}
	    "Change current working session to new mapset, location, or GISDBASE" ""
	    	{"execute g.mapset"}
	}
	"Region" "Region management" {
	    "Display region settings" ""
	        {"run g.region -p"}
	    "Manage region" ""
	        {"execute g.region"}
	    "Select default region" ""
	        {"run g.region -d ; run d.redraw"}
	    -separator "" ""
	    "Create WIND3 (default 3D window) from current 2D region" ""
	        {"execute g3.createwind"}
	    "Manage 3D region" ""
	        {"execute g3.setregion"}
	}
	"Map type conversions" "raster<->vector<->sites<->grid3D" {
	    "Raster to vector map" "r.to.vect"
	        {"execute r.to.vect"}
	    "Vector to raster" "v.to.rast"
	        {"execute v.to.rast"}
	    "Vector to points" "v.to.points"
	        {"execute v.to.points"}
	    "Sites to vector" "v.in.sites"
	        {"execute v.in.sites"}
	}
	"Projections & GRASS environment" "" {
	    "Create/edit projection information for current location" "g.setproj"
	        {"term g.setproj"}
	    "Show projection information & create projection files" "g.proj"
	        {"execute g.proj"}
	    "Show current GRASS environment settings" "g.gisenv"
	        {"execute g.gisenv"}
	    "Show current GRASS version" "g.version -c"
	        {"run g.version -c"}
	}
}
"Display" "Display maps" {
	"Start display manager" "d.m"
	    {"execute d.m"}
	"Start NVIZ (n-dimensional visualization module)" "nviz -q"
	    {"spawn nviz -q"}
	-separator "" ""
	"Start displays" ""  {[monitor_menu start]}
	"Stop displays" ""   {[monitor_menu stop]}
	"Select displays" "" {[monitor_menu select]}
	-separator "" ""
	"Raster" "Display raster maps" {
	    "Display raster maps" ""
	        {"execute d.rast"}
	    "Display raster map, legend, & title in active display" ""
	        {"execute d.rast.leg"}
	    -separator "" ""
	    "Display HIS overlays" ""
	        {"execute d.his"}
	    "Display RGB overlays" ""
	        {"execute d.rgb"}
	    "Drape raster map over shaded relief or aspect map" ""
	        {"execute d.shadedmap"}
	    -separator "" ""
	    "Slide show of all raster maps in current mapset" ""
	        {"execute d.slide.show"}
	}
	"Vector" "Display vector maps"
	    {"execute d.vect"}
	"Grid3D" ""
		{"execute r3.showdspf"}
	"Text" "Display text on maps" {
	    "Display legend for raster maps" ""
	        {"execute d.legend"}
	    "Display category values in raster map cells" ""
	        {"execute d.rast.num"}
	    -separator "" ""
	    "Create map title file for use by d.text" ""
	        {"execute d.title"}
	    "Display text labels for paint output" ""
	        {"execute d.paint.labels"}
	    -separator "" ""
	    "Select text font" ""
	        {"execute d.font"}
	    -separator "" ""
 	    "Draw text" ""
	        {"execute d.text"}
	    "Draw text using TrueType fonts" ""
	        {"execute d.text.freetype"}
	    -separator "" ""
	    "Display standard GRASS fonts" ""
	        {"execute show.fonts.sh"}
	}
	"Graphics" "Display graphics on maps" {
	    "Display histogram" ""
	        {"execute d.histogram"}
	    "Display line graph" ""
	        {"execute d.linegraph"}
	    "Display graphs at vector point localities" ""
	        {"execute d.vect.chart"}
	    "Display geodesic line" ""
	        {"execute d.geodesic"}
	    "Display rhumbline" ""
	        {"execute d.rhumbline"}
	    -separator "" ""
	    "Display color table" ""
	        {"execute d.colortable"}
	    -separator "" ""
	    "Overlay scale and north arrow" ""
	        {"execute d.barscale"}
	    "Overlay grid" ""
	        {"execute d.grid"}
	    "Overlay slope arrows on aspect raster map" ""
	        {"execute d.rast.arrow"}
	    -separator "" ""
	    "Draw simple graphics in active display monitor (display coordinates)" ""
	        {"execute d.graph"}
	    "Draw simple graphics in active display monitor (map coordinates)" ""
	        {"execute d.mapgraph"}
	}
	-separator "" ""
	"Split active display and show maps in each half" ""
	    {"execute d.split"}
	-separator "" ""
	"Manage displays" ""
	    {"execute d.mon"}
	"Manage display frames" ""
	    {"execute d.frame"}
	-separator "" ""
	"Save file of commands to recreate active display" ""
	    {"execute d.save"}
	-separator "" ""
	"Redraw active display (Note: some items may not be redrawn)" ""
	    {"execute d.redraw"}
	"Set active display to specified size" ""
	    {"execute d.resize"}
	-separator "" ""
	"Create fly-through animation path for NVIZ" ""
		{"execute d.nviz"}
	"Pan and zoom in active display" ""
	    {"term d.zoom -f"}
	"Show geographical position" ""
	    {"execute d.where"}
	"Measure lengths and areas" ""
	    {"execute d.measure"}
	"Zoom/Unzoom in active display" ""
	    {"term d.zoom"}
	-separator "" ""
	"Erase active display/frame" ""
	    {"execute d.erase"}
}
"Raster" "Raster map analysis" {
	"Develop map" "" {
	    "Digitize" ""
	        {"term r.digit"}
	    -separator "" ""
	    "Compress/decompress raster file" ""
	        {"execute r.compress"}
	    "Manage boundary definitions" ""
	        {"execute r.region"}
	    "Manage null values" ""
	        {"execute r.null"}
	    "Manage timestamps for files" ""
	        {"execute r.timestamp"}
	    "Quantization for floating-point maps" ""
	        {"execute r.quant"}
	    "Resample (change resolution) using nearest neighbor method" ""
	        {"execute r.resample"}
	    "Resample (change resolution) using regularized spline tension" ""
	        {"execute r.resample.rst"}
	    -separator "" ""
	    "Reproject raster from other location" ""
	        {"execute r.proj"}
	}
	"Manage map colors" "" {
	    "Modify color table" ""
	        {"execute d.colors"}
	    "Set colors to predefined color tables" ""
	        {"execute r.colors"}
	    "Set colors using color rules" ""
	        {"execute r.colors.rules"}
	    -separator "" ""
	    "Blend 2 color maps to produce 3 RGB files" ""
	        {"execute r.blend"}
	    "Create color image from RGB files" ""
	        {"execute r.composite"}
	    "Create 3 RGB (red, green, blue) maps from 3 HIS (hue, intensity, saturation) maps" ""
	        {"execute r.his"}
	}
	-separator "" ""
	"Query by coordinate(s)" ""
	    {"execute r.what"}
	"Query with mouse" ""
	    {"execute d.what.rast"}
	-separator "" ""
	"Create buffers" ""
	    {"execute r.buffer"}
	"Locate closest points between areas in 2 raster maps" ""
	    {"execute r.distance"}
	"Map calculator" ""
	    {"execute r.mapcalculator"}
	"Neighborhood analysis" ""
	    {"execute r.neighbors"}
	"Overlay maps" "" {
	    "Cross product" ""
	        {"execute r.cross"}
	    "Function of map series" ""
	        {"execute r.series"}
	    "Patch maps" ""
	        {"execute r.patch"}
	    -separator "" ""
	    "Statistical calculations for cover map over base map" ""
	        {"execute r.statistics"}
	}
	"Solar radiance and shadows" "" {
	    "Solar irradiance & daily irradiation" ""
	        {"execute r.sun"}
	    "Shadows map for sun position or date/time" ""
	        {"execute r.sunmask"}
	}
	"Terrain analysis" "" {
	    "Cost surface" ""
	        {"execute r.cost"}
	    "Least cost route or flow" ""
	        {"execute r.drain"}
	    "Profile analysis" ""
	        {"execute d.profile"}
	    "Shaded relief map" ""
	        {"execute r.shaded.relief"}
	    "Slope and aspect" ""
	        {"execute r.slope.aspect"}
	    "Terrain parameters" ""
	        {"execute r.param.scale"}
	    "Textural features" ""
	        {"execute r.texture"}
	    "Visibility/line of sight" ""
	        {"execute r.los"}
	}
	"Transform features" "" {
	    "Clump small areas" ""
	        {"execute r.clump"}
	    "Grow areas" ""
	        {"execute r.grow"}
	    "Thin linear features" ""
	        {"execute r.thin"}
	}
	-separator "" ""
	"Hydrologic modeling" "" {
	    "Depressionless elevation map and flowline map" ""
	        {"execute r.fill.dir"}
	    "Flow accumulation for massive grids (floating-point version)" ""
	        {"execute r.terraflow"}
	    "Flow accumulation for massive grids (integer version)" ""
	        {"execute r.terraflow.short"}
	    "Topographic index map" ""
	        {"execute r.topidx"}
	    "TOPMODEL simulation" ""
	        {"execute r.topmodel"}
	    "Watershed subbasins" ""
	        {"execute r.basins.fill"}
	    "Watershed basin creation" ""
	        {"execute r.water.outlet"}
	}
	"Landscape structure modeling" "" {
	    "Set up sampling and analysis framework" ""
	        {"term r.le.setup"}
	    -separator "" ""
	    "Analyze landscape characteristics" ""
	        {"execute r.le.pixel"}
	    "Analyze landscape patch characteristics" ""
	        {"execute r.le.patch"}
	    "Output landscape patch information" ""
	        {"execute r.le.trace"}
	}
	"Wildfire modeling" "" {
	    "Generate rate of spread (ROS) maps" ""
	        {"execute r.ros"}
	    "Generate least-cost spread paths" ""
	        {"execute r.spreadpath"}
	    "Simulate anisotropic spread phenomena" ""
	        {"execute r.spread"}
	}
	-separator "" ""
	"Change category values and labels" "" {
	    "Edit category values of individual cells for displayed raster map" ""
	        {"execute d.rast.edit"}
	    -separator "" ""
	    "Reclassify categories for areas of specified sizes" ""
	        {"execute r.reclass.area"}
	    "Reclassify categories using rules" ""
	        {"execute r.reclass.rules"}
	    "Reclassify categories using rules file" ""
	        {"execute r.reclass"}
	    -separator "" ""
	    "Recode categories using rules (create new map)" ""
	        {"execute r.recode.rules"}
	    "Recode categories using rules file (create new map)" ""
	        {"execute r.recode"}
	    -separator "" ""
	    "Rescale categories (create new map)" ""
	        {"execute r.rescale"}
	    "Rescale categories with equalized histogram (create new map)" ""
	        {"execute r.rescale.eq"}
	}
	-separator "" ""
	"Generate concentric circles around points" ""
	    {"execute r.circle"}
	"Generate & interpolate surfaces" "" {
	    "Interpolate surfaces from points" "" {
		"Bilinear from points" ""
		    {"execute r.bilinear"}
		"Inverse distance weighted from raster (Lat./Long. locations)" ""
		    {"execute r.surf.idw"}
		"Inverse distance weighted from vector points" ""
		    {"execute v.surf.idw"}
		"Regularized spline tension from vector points" ""
		    {"execute v.surf.rst"}
	    }
	    "Fill NULL areas using regularized spline tension" ""
	        {"execute r.fillnulls"}
	    "Interpolate surfaces from contours" "" {
		"Regularized spline tension from raster contours" ""
		    {"execute r.surf.contour"}
		"Regularized spline tension from vector contours" ""
		    {"execute v.surf.rst"}
	    }
	    -separator "" ""
	    "Generate density surface using moving Gausian kernal" ""
	        {"execute v.kernel"}
	    "Generate fractal surface" ""
	        {"execute r.surf.fractal"}
	    "Generate gaussian deviates surface" ""
	        {"execute r.surf.gauss"}
	    "Generate plane" ""
	        {"execute r.plane"}
	    "Generate random deviates surface" ""
	        {"execute r.surf.random"}
	    "Generate random surface with spatial dependence" ""
	        {"execute r.random.surface"}
	}
	"Generate points" "" {
	    "Generate random cells" ""
	        {"execute r.random.cells"}
	    "Generate random cells & sites from raster map" ""
	        {"execute r.random"}
	}
	"Generate vector contour lines" ""
	    {"execute r.contour" }
	-separator "" ""
	"Reports & statistics" "" {
	    "Report basic file information" ""
	        {"execute r.info"}
	    "Report category labels and values" ""
	        {"execute r.cats"}
	    -separator "" ""
	    "General statistics" ""
	        {"execute r.stats"}
	    "Range of all category values" ""
	        {"execute r.describe"}
	    "Sum all cell category values" ""
	        {"execute r.sum"}
	    "Sum area by map and category" ""
	        {"execute r.report"}
	    "Total surface area, considering toppography" ""
	        {"execute r.surf.area"}
	    "Univariate statistics" ""
	        {"execute r.univar"}
	    "Univariate statistics (script version)" ""
	        {"execute r.univar.sh"}
	    -separator "" ""
	    "Sample values along transects" ""
	        {"execute r.profile"}
	    "Sample values along transects (use azimuth, distance)" ""
	        {"execute r.transect"}
	    -separator "" ""
	    "Covariance/correlation" ""
	        {"execute r.covar"}
	    "Linear regression between 2 maps" ""
	        {"execute r.regression.line"}
	    "Mutual category occurences (coincidence)" ""
	        {"execute r.coin"}
	}
}
"Vector" "Vector map analysis" {
	"Develop map" "" {
	    "Digitize" ""
	        {"execute v.digit"}
	    -separator "" ""
	    "Create/rebuild topology" ""
	        {"execute v.build"}
	    "Clean vector files" ""
	        {"execute v.clean"}
	    -separator "" ""
	    "Break lines at intersections" ""
	        {"execute v.topo.check"}
	    "Build polylines from adjacent segments" ""
	        {"execute v.build.polylines"}
	    "Split polylines into segments" ""
	        {"execute v.segment"}
	    -separator "" ""
	    "Convert vector feature types" ""
	        {"execute v.type"}
	    -separator "" ""
	    "Create text label file for vector features" ""
	        {"execute v.label"}
	    -separator "" ""
	    "Reproject vector from other location" ""
	        {"execute v.proj"}
	}
	"Register/unregister connections" "" {
	    "Create new vector as link to external OGR layer" ""
	        {"execute v.external"}
	    "Register ESRI shapefile" ""
	        {"execute v.shape.register"}
	    "Unregister ESRI shapefile" ""
	        {"execute v.shape.unregister"}
	    "Register ESRI shapefiles and define PostGRASS connections" ""
	        {"execute v.format"}
	    -separator "" ""
	    "Set database connection for vector attributes" ""
	        {"execute v.db.connect"}
	    "Set database & driver for vector attributes" ""
	        {"execute v.database"}
	}
	"Rectify & georeference vector map" ""
	    {"execute v.transform"}
	-separator "" ""
	"Query by attributes" ""
	    {"execute v.extract"}
	"Query by map features" ""
	    {"execute v.select"}
	"Query with mouse" ""
	    {"spawn d.what.vect -xf"}
	-separator "" ""
	"Buffer features" ""
	    {"execute v.buffer"}
	"Locate nearest features to points or centroids" ""
	    {"execute v.distance"}
	"Network analysis" "" {
	    "Allocate subnets" ""
	        {"execute v.net.alloc"}
	    "Network maintenance" ""
	        {"execute v.net"}
	    "Shortest route" ""
	        {"execute v.net.path"}
	    "Shortest route (visualization only)" ""
	        {"execute d.path"}
	    "Split net to bands between cost isolines" ""
	        {"execute v.net.iso"}
	    "Steiner tree" ""
	        {"execute v.net.steiner"}
	    "Traveling salesman analysis" ""
	        {"execute v.net.salesman"}
	}
	"Overlay maps" "" {
	    "Overlay/combine 2 vector maps" ""
	        {"execute v.overlay"}
	    "Patch multiple maps (combine)" ""
	        {"execute v.patch"}
	}
	"Sample raster map at vector points" ""
	    {"execute v.what.rast"}
	-separator "" ""
	"Change attributes" "" {
	    "Attach, delete, or report categories" ""
	        {"execute v.category"}
	    "Reclassify features using rules file" ""
	        {"execute v.reclass"}
	}
	-separator "" ""
	"Generate vector maps" "" {
	    "Generate convex hull for point set" ""
	        {"execute v.hull"}
	    "Generate vector points from database x&y coordinates" ""
	        {"execute v.in.db"}
	    "Generate area feature for extent of current region" ""
	        {"execute v.in.region"}
	}
	"Generate points" "" {
	    "Generate random points" ""
	        {"execute v.random"}
	}
	-separator "" ""
	"Reports & statistics" "" {
	    "Basic information" ""
	        {"execute v.info"}
	    "Load vector attributes to database or create reports" ""
	        {"execute v.to.db"}
	    "Univariate statistics" ""
	        {"execute v.univar"}
	}
}
"Image" "Image processing" {
	"Develop images & groups" "" {
	    "Create/edit imagery group" ""
	        {"execute i.group"}	
	    "Target imagery group" ""
	        {"execute i.target"}
	    -separator "" ""
	    "Mosaic up to 4 adjacent images" ""
	        {"execute i.image.mosaic"}
	}
	"Manage image colors" "" {
	    "Transform HIS (Hue/Intensity/Saturation) color image to RGB (Red/Green/Blue)" ""
	        {"execute i.his.rgb"}
	    "Transform RGB (Red/Green/Blue) color image to HIS (Hue/Intensity/Saturation)" ""
	        {"execute i.rgb.his"}
	}
	"Rectify & georeference image group" "" {
	    "Set ground control points (GCP's) from raster map" ""
	        {"execute i.points"}
	    "Affine and Polynomial rectification (rubber sheet)" ""
	        {"execute i.rectify"}
	    "Ortho photo rectification" ""
	        {"execute i.ortho.photo"}
	}
	-separator "" ""
	"Brovey transformation and pan sharpening for Landsat ETM, SPOT, & Quickbird" ""
	    {"execute i.fusion.brovey"}
	"Classify image" "" {
	    "Clustering input for unsupervised classification" ""
	        {"execute i.cluster"}
	    -separator "" ""
	    "Maximum likelyhood classification (MLC)" ""
	        {"execute i.maxlik"}
	    "Sequential maximum a posteriory classification (SMAP)" ""
	        {"execute i.smap"}
	    -separator "" ""
	    "Interactive input for supervised classification" ""
	        {"execute i.class"}
	    "Non-interactive input for supervised classification (MLC)" ""
	        {"execute i.gensig"}
	    "Non-interactive input for supervised classification (SMAP)" ""
	        {"execute i.gensigset"}
	}
	"Dehaze for LandSAT 5" ""
	    {"execute i.tm.dehaze"}
	"Filter image" "" {
	    "Zero edge crossing detection" ""
	        {"execute i.zc"}
	    "User defined matrix/convolving filter" ""
	        {"execute r.mfilter"}
	}
	"Spectral response" ""
	    {"execute i.spectral"}
	"Tassled cap vegetation index" ""
	    {"execute i.tasscap"}
	"Transform image" "" {
	    "Canonical component" ""
	        {"execute i.cca"}
	    "Principal component" ""
	        {"execute i.pca"}
	    "Fast Fourier Transform" ""
	        {"execute i.fft"}
	    "Inverse Fast Fourier Transform" ""
	        {"execute i.ifft"}
	}
	-separator "" ""
	"Reports & statistics" "" {
	    "Report basic file information" ""
	        {"execute r.info"}
	    "Range of image values" ""
	        {"execute r.describe"}
	    -separator "" ""
	    "Kappa classification aaccuracy assessment" ""
	        {"execute r.kappa"}
	    "Optimum index factor for LandSat TM" ""
	        {"execute i.oif"}
	}
}
"Grid3D" "3D volume management. Use r3.showdspf or NVIZ to view." {
	"Develop grid3D volumes" "" {
	    "Manage nulls for grid3D volume" ""
	        {"execute r3.null"}
	    "Manage timestamp for grid3D volume" ""
	        {"execute r3.timestamp"}
	}
	"Create 3D mask for grid3D operations" ""
	    {"execute r3.mask"}
	"Create display file for grid3D volume" ""
		{"execute r3.mkdspf"}
	"Map calculator for grid3D operations" ""
	    {"execute r3.mapcalculator"}
	"Interpolate volume from vector points using splines" ""
	    {"execute v.vol.rst"}
	"Report & Statistics" "" {
	    "Display information about grid3D volume" ""
	        {"execute r3.info"}
	}
}
"Databases" "Database management" {
	"Manage database" "" {
	    "Connect to database" ""
	        {"execute db.connect"}
	    "Create empty database" ""
	        {"execute db.createdb"}
	    "PERMANTLY remove table" ""
	        {"execute db.droptable"}
	    "Copy table" ""
	        {"execute db.copy"}
	}
	"Database information" "" {
	    "Describe table" ""
	        {"execute db.describe"}
	    "List columns" ""
	        {"execute db.columns"}
	    "List databases" ""
	        {"execute db.databases"}
	    "List drivers" ""
	        {"execute db.drivers"}
	    "List tables" ""
	        {"execute db.tables"}
	}
	-separator "" ""
	"Query" "" {
	    "Query data (SQL select)" ""
	        {"execute db.select"}
	    "Execute SQL statement" ""
	        {"execute db.execute"}
	}
	"Manage PostGIS database" ""
	    {"execute pg.postgisdb"}
}
"Config" "Configuration of TclTkGRASS" {
	"Reset menu size" ""
	    {resize_menu}
	-separator "" ""
	"Set menu font" ""
	    {"set_menu_font"}
}
"Help" "Help" {
	"Manual pages" ""
	    {"spawn g.manual -i"}
	-separator "" ""
	"Tcltkgrass menus help" ""
	    {"source $env(TCLTKGRASSBASE)/main/help.tcl"}
	"About tcltkgrass" ""
	    {"source $env(TCLTKGRASSBASE)/main/about.tcl"}
	-separator "" ""
	"About GRASS" ""
	    {"source $env(TCLTKGRASSBASE)/main/grassabout.tcl"}
}
}]
