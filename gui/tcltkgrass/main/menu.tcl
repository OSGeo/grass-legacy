frame .main_menu
pack .main_menu -expand yes -fill both

# tcltkgrass menu.tcl v 4.0.1 for GRASS 5.7 2004/06/30 Michael Barton
# based on menu.tcl for GRASS 5.3 by Michael Barton, Jacques Bouchard, and Markus Neteler
# with scripting support by Andreas Lange

# main menu

menu_build 1 .main_menu {
    File "Files in/out" {
	"Import" "Import maps into GRASS" {
	    "Raster map" "" {
		"Multiple formats using GDAL" "r.in.gdal" {
		"exec  r.in.gdal &"}
		-separator
		"ASCII GRID (includes GRASS ASCII)" "r.in.ascii" {
		"exec r.in.ascii &"}
		"GRASS ASCII vector map" "r.in.poly" {
		"exec r.in.poly &"}
		-separator
		"Binary file (includes GTOPO30 format)" "r.in.bin" {
		"exec  r.in.bin &"}
		"ERDAS LAN" "i.in.erdas" {
		"exec  i.in.erdas &"}
		"GRIDATB.FOR map file (TOPMODEL)" "r.in.gridatb" {
		"exec r.in.gridatb &"}
		"MAT-File (v.4) map file (Matlab or Octave)" "r.in.gridatb" {
		"exec r.in.mat &"}
	    }
	    "Vector map" "" {
		"Various formats using OGR" "v.in.ogr" {
		"exec v.in.ogr &"}
		-separator
		"ASCII points file" "" {
		"exec v.in.asciipoints &"}
		"GRASS vector file" "v.in.ascii" {
		"exec v.in.ascii &"}
		"Import/export old GRASS vector format" "v.convert" {
		"exec  v.convert &"}
		-separator
		"Garmin GPS Waypoints/Routes/Tracks" "v.in.garmin" {
		"exec v.in.garmin &"}
		"MATLAB MapGen files" "v.in.mapgen.sh" {
		"exec v.in.mapgen.sh &"}
		-separator
		"ASCII points file to GRASS sites format" "s.in.ascii" {
		"exec s.in.ascii &"}
	    }
	    "Grid 3D" "" {
	     "ASCII 3D file" "r3.in.ascii" {
		"exec r3.in.ascii &"}
	     "Vis5D file" "r3.in.v5d" {
		"exec r3.in.v5d &"}
	    }
	}
	Export "Export maps from GRASS" {
	    "Raster map" "" {
		"ASCII grid (for GRASS, Surfer, Modflow, etc)" "r.out.ascii" {
		"exec r.out.ascii &"}
		-separator
		"ESRI ARC/INFO ASCII grid" "r.out.arc" {
		"exec r.out.arc &"}
		"ERDAS LAN file" "i.out.erdas" {
		"exec i.out.erdas &"}
		"GRIDATB.FOR map file (TOPMODEL)" "r.out.gridatb" {
		"exec r.out.gridatb"}
		"MAT-File (v.4) map file (Matlab or Octave)" "r.out.mat" {
		"exec r.out.mat &"}
		-separator
		"Binary file" "r.out.bin" {
		"exec r.out.bin &"}
		-separator
		"MPEG-1 animations" "r.out.mpeg" {
		"exec r.out.mpeg &"}
		"PNG image (not georeferenced)" "r.out.png" {
		"exec r.out.png &"}
		"PPM image (24bit)" "r.out.ppm" {
		"exec r.out.ppm &"}
		"PPM image from red, green, blue raster maps" "r.out.ppm3" {
		"exec r.out.ppm3 &"}
		"POVray height-field" "r.out.pov" {
		"exec r.out.pov &"}
		"TIFF image (8/24bit)" "r.out.pov" {
		"exec r.out.tiff &"}
	    }
	    "Vector map" "" {
		"Various formats using OGR (SHAPE, MapInfo etc)" "v.out.ogr" {
		"exec v.out.ogr &"}
		-separator
		"GRASS vector file" "v.out.ascii" {
		"exec v.out.ascii &"}
		"Import/export old GRASS vector format" "v.convert" {
		"exec  v.convert &"}
		"POV-Ray format" "v.out.pov" {
		"exec  v.out.pov &"}
		-separator
		"ASCII points file from GRASS sites file" "s.out.ascii" {
		"exec  s.out.ascii &"}
	    }
	    "Grid 3D" "" {
	     "ASCII 3D file" "r3.out.ascii" {
		"exec r3.out.ascii &"}
	     "Vis5D file" "r3.out.v5d" {
		"exec r3.out.v5d &"}
	    }
	}
	Scripting "" {
	    "Start scripting" "" {
	    "script_start"}
	    "Stop scripting" "" {
	    "script_stop"}
	    "Play script" "" {
	    "script_play"}
	}
	"Save display to image file" "" {
	    "XWD (Save display, selected with mouse, to map.xwd in home directory )"
		"r3.out.v5d" {
	    "exec xwd -out map.xwd"}
	    "PNG (save currently selected display to 24 bit PNG file)" "d.out.png" {
	    "exec d.out.png &"}
	}
	"Postscript map creation" "ps.map" {"exec ps.map &"}
	"Print (Use display manager)" "" {"do_nothing"}
	"Quit tcltkgrass" "" resize_menu;quit
    }
    GIS "Manage GRASS GIS files" {
	"Maps & grid3D files" "Map management (map files operations)" {
	    "Copy maps" "" {
	    "exec g.copy &"}
	    "List maps" "" {
	    "exec g.list &"}
	    "List maps using expressions & 'wildcards'" "" {
	    "exec g.mlist &"}
	    "Rename maps" "" {
	    "exec g.rename &"}
	    "Remove maps" "" {
	    "exec g.remove &"}
	    "Remove maps using expressions & 'wildcards'" "" {
	    "exec g.mremove &"}
	    -separator
	    "Modify access to current mapset" "" {
	    "exec g.access &"}
	    "Modify mapset search path" "" {
	    "exec g.mapsets &"}
	}
	Region "Region management" {
	    "Display region settings" "" {
	    "exec g.region -p &"}
	    "Manage region" "" {
	    "exec g.region &"}
	    "Select default region" "" {
	    "exec g.region -d; exec d.erase"}
	}
	"3D region" "Grid3D region management" {
	    "Create WIND3 (default 3D window) from current 2D region" "" {
	    "exec g3.createwind"}
	    "Manage 3D region" "" {
	    "exec g3.setregion"}
	}
	"Map type conversions" "raster<->vector<->sites<->grid3D" {
	    "Raster to vector map" "r.to.vect" {
	    "exec r.to.vect &"}
	    "Vector to raster" "v.to.rast" {
	    "exec v.to.rast &"}
	    "Vector to points" "v.to.points" {
	    "exec v.to.points &"}
	    "Sites to vector" "v.in.sites" {
	    "exec v.in.sites &"}
	}
	"Projections & GRASS environment" "" {
	    "Create/edit projection information for current location" "g.setproj" {
	    "exec g.setproj"}
	    "Show projection information & create projection files" "g.proj" {
	    "exec g.proj &"}
	    "Show current GRASS environment settings" "g.gisenv" {
	    "exec g.gisenv &"}
	    "Show current GRASS version" "g.version -c" {
	    "exec g.version -c &"}
	}
    }
    Display "Display maps" {
	"Start display manager" "d.m" {
	"exec d.m &"}
	"Start NVIZ (n-dimensional visualization module)" "nviz -q" {
	"exec nviz -q &"}
	-separator
	"Start displays" "" {
	    X0 "" {
	    "exec d.mon start=x0 &"}
	    X1 "" {
	    "exec d.mon start=x1 &"}
	    X2 "" {
	    "exec d.mon start=x2 &"}
	    X3 "" {
	    "exec d.mon start=x3 &"}
	    X4 "" {
	    "exec d.mon start=x4 &"}
	    X5 "" {
	    "exec d.mon start=x5 &"}
	    X6 "" {
	    "exec d.mon start=x6 &"}
	    -separator
	    "Start/restart display at specified window size" "" {
	    "exec d.monsize"}
	}
	"Stop displays" "" {
	    X0 "" {
	    "exec d.mon stop=x0 &"}
	    X1 "" {
	    "exec d.mon stop=x1 &"}
	    X2 "" {
	    "exec d.mon stop=x2 &"}
	    X3 "" {
	    "exec d.mon stop=x3 &"}
	    X4 "" {
	    "exec d.mon stop=x4 &"}
	    X5 "" {
	    "exec d.mon stop=x5 &"}
	    X6 "" {
	    "exec d.mon stop=x6 &"}
	    -separator
	    CELL "" {
	    "exec d.mon stop=CELL &"}
	}
	"Select displays" "" {
	    X0 "" {
	    "exec d.mon select=x0 &"}
	    X1 "" {
	    "exec d.mon select=x1 &"}
	    X2 "" {
	    "exec d.mon select=x2 &"}
	    X3 "" {
	    "exec d.mon select=x3 &"}
	    X4 "" {
	    "exec d.mon select=x4 &"}
	    X5 "" {
	    "exec d.mon select=x5 &"}
	    X6 "" {
	    "exec d.mon select=x6 &"}
	}
	-separator
	    Raster "Display raster maps" {
	    "Display raster maps" "" {
	    "exec d.rast &"}
	    "Display raster map, legend, & title in active display" "" {
	    "exec d.rast.leg &"}
	    -separator
	    "Display HIS overlays" "" {
	    "exec d.his &"}
	    "Display RGB overlays" "" {
	    "exec d.rgb &"}
	    "Drape raster map over shaded relief or aspect map" "" {
	    "exec d.shadedmap &"}
	    -separator
	    "Slide show of all raster maps in current mapset" "" {
	    "exec d.slide.show &"}
	}
	Vector "Display vector maps" {
	"exec d.vect &"}
	Text "Display text on maps" {
	    "Display legend for raster maps" "" {
	    "exec d.legend &"}
	    "Display category values in raster map cells" "" {
	    "exec d.rast.num &"}
	    -separator
	    "Create map title file for use by d.text" "" {
	    "exec d.title &"}
	    "Display text labels for paint output" "" {
	    "exec d.paint.labels &"}
	    -separator
	    "Select text font" "" {
	    "exec d.font &"}
	    -separator
 	    "Draw text" "" {
	    "exec d.text &"}
	    "Draw text using TrueType fonts" "" {
	    "exec d.text.freetype &"}
	    -separator
	    "Display standard GRASS fonts" "" {
	    "exec show.fonts.sh &"}
	}
	Graphics "Display graphics on maps" {
	    "Display histogram" "" {
	    "exec d.histogram &"}
	    "Display line graph" "" {
	    "exec d.linegraph &"}
	    "Display graphs at vector point localities" "" {
	    "exec d.vect.chart &"}
	    "Display geodesic line" "" {
	    "exec d.geodesic &"}
	    "Display rhumbline" "" {
	    "exec d.rhumbline &"}
	    -separator
	    "Display color table" "" {
	    "exec d.colortable &"}
	    "Display standard GRASS colors" "" {
	    "exec show.color.sh &"}
	    -separator
	    "Overlay scale and north arrow" "" {
	    "exec d.barscale &"}
	    "Overlay grid" "" {
	    "exec d.grid &"}
	    "Overlay slope arrows on aspect raster map" "" {
	    "exec d.rast.arrow &"}
	    -separator
		"Draw simple graphics in active display monitor (display coordinates)"
		"" {
	    "exec d.graph &"}
	    "Draw simple graphics in active display monitor (map coordinates)"
		"" {
	    "exec d.mapgraph &"}
	}
	-separator
	"Split active display and show maps in each half" "" {
	"exec d.split &"}
	-separator
	"Manage displays" "" {
	"exec d.mon &"}
	"Manage display frames" "" {
	"exec d.frame &"}
	-separator
	"Save file of commands to recreate active display" "" {
	"exec d.save &"}
	-separator
	    "Redraw active display (Note: some items may not be redrawn)" "" {
	"exec d.redraw &"}
	"Set active display to specified size" "" {
	"exec d.resize &"}
	-separator
	"Pan and zoom in active display" "" {
	"exec d.zoom -f &"}
	"Show geographical position" "" {
	"exec d.where &"}
	"Measure lengths and areas" "" {
	"exec d.measure &"}
	"Zoom/Unzoom in active display" "" {
	"exec d.zoom &"}
	-separator
	"Erase active display/frame" "" {
	"exec d.erase &"}
    }
    Raster "Raster map analysis" {
	"Develop map" "" {
	    "Digitize" "" {
	    "exec r.digit &"}
	    -separator
	    "Compress/decompress raster file" "" {
	    "exec r.compress &"}
	    "Manage boundary definitions" "" {
	    "exec r.region &"}
	    "Manage null values" "" {
	    "exec r.null &"}
	    "Manage timestamps for files" "" {
	    "exec r.timestamp &"}
	    "Quantization for floating-point maps" "" {
	    "exec r.quant &"}
	    "Resample (change resolution)" "" {
	    "exec r.resample &"}
	    -separator
	    "Reproject raster from other location" "" {
	    "exec r.proj &"}
	}
	"Manage map colors" "" {
	    "Modify color table" "" {
	    "exec d.colors &"}
	    "Set colors to predefined color tables" "" {
	    "exec r.colors &"}
	    "Set colors using color rules" "" {
	    "exec r.colors.rules &"}
	    -separator
	    "Blend 2 color maps to produce 3 RGB files" "" {
	    "exec r.blend &"}
	    "Create color image from RGB files" "" {
	    "exec r.composite &"}
	    "Create 3 RGB (red, green, blue) maps from 3 HIS (hue, intensity, saturation) maps"
		"" {
	    "exec r.his &"}
	}
	-separator
	"Query by coordinate(s)" "" {
	"exec r.what &"}
	"Query with mouse" "" {
	"exec d.what.rast &"}
	-separator
	"Create buffers" "" {
	"exec r.buffer &"}
	"Locate closest points between areas in 2 raster maps" "" {
	"exec r.distance &"}
	"Map calculator" "" {
	"exec r.mapcalculator &"}
	"Neighborhood analysis" "" {
	"exec r.neighbors &"}
	"Overlay maps" "" {
	    "Cross product" "" {
	    "exec r.cross &"}
	    "Function of map series" "" {
	    "exec r.series &"}
	    "Patch maps" "" {
	    "exec r.patch &"}
	    -separator
		"Statistical calculations for cover map over base map" "" {
	    "exec r.statistics &"}
	}
	"Solar radiance and shadows" "" {
	    "Solar irradiance & daily irradiation" "" {
	    "exec r.sun &"}
	    "Shadows map for sun position or date/time" "" {
	    "exec r.sunmask"}
	}
	"Terrain analysis" "" {
	    "Cost surface" "" {
	    "exec r.cost &"}
	    "Least cost route or flow" "" {
	    "exec r.drain &"}
	    "Profile analysis" "" {
	    "exec d.profile &"}
	    "Shaded relief map" "" {
	    "exec r.shaded.relief"}
	    "Slope and aspect" "" {
	    "exec r.slope.aspect &"}
	    "Terrain parameters" "" {
	    "exec r.param.scale &"}
	    "Textural features" "" {
	    "exec r.texture &"}
	    "Visibility/line of sight" "" {
	    "exec r.los &"}
	}
	"Transform features" "" {
	    "Clump small areas" "" {
	    "exec r.clump &"}
	    "Grow areas" "" {
	    "exec r.grow &"}
	    "Thin linear features" "" {
	    "exec r.thin &"}
	}
	-separator
	    "Hydrologic modeling" "" {
	    "Depressionless elevation map and flowline map" "" {
	    "exec r.fill.dir &"}
	    "Flow accumulation for massive grids (floating-point version)" "" {
	    "exec r.terraflow &"}
	    "Flow accumulation for massive grids (integer version)" "" {
	    "exec r.terraflow.short &"}
	    "Topographic index map" "" {
	    "exec r.topidx &"}
	    "TOPMODEL simulation" "" {
	    "exec r.topmodel &"}
	    "Watershed subbasins" "" {
	    "exec r.basins.fill &"}
	    "Watershed basin creation" "" {
	    "exec r.water.outlet &"}
	}
	"Landscape structure modeling" "" {
	    "Set up sampling and analysis framework" "" {
	    "run r.le.setup &"}
	    -separator
	    "Analyze landscape characteristics" "" {
	    "exec r.le.pixel"}
	    "Analyze landscape patch characteristics" "" {
	    "exec r.le.patch"}
	    "Output landscape patch information" "" {
	    "exec r.le.trace"}
	}
	"Wildfire modeling" "" {
	    "Generate rate of spread (ROS) maps" "" {
	    "exec r.ros &"}
	    "Generate least-cost spread paths" "" {
	    "exec r.spreadpath &"}
	    "Simulate anisotropic spread phenomena" "" {
	    "exec r.spread &"}
	}
	-separator
	    "Change category values and labels" "" {
	    "Edit category values of individual cells for displayed raster map"
		"" {
	    "exec d.rast.edit &"}
	    -separator
	    "Reclassify categories for areas of specified sizes" "" {
	    "exec r.reclass.area &"}
	    "Reclassify categories using rules" "" {
	    "exec r.reclass.rules &"}
	    "Reclassify categories using rules file" "" {
	    "exec r.reclass &"}
	    -separator
	    "Recode categories using rules (create new map)" "" {
	    "exec r.recode.rules &"}
	    "Recode categories using rules file (create new map)" "" {
	    "exec r.recode &"}
	    -separator
	     "Rescale categories (create new map)" "" {
	    "exec r.rescale &"}
	    "Rescale categories with equalized histogram (create new map)" "" {
	    "exec r.rescale.eq &"}
	}
	-separator
	"Generate concentric circles around points" "" {
	"exec r.circle &"}
	"Generate & interpolate surfaces" "" {
	    "Interpolate surfaces from points" "" {
		"Bilinear from points" "" {
		"exec r.bilinear &"}
		"Inverse distance weighted from raster (Lat./Long. locations)" "" {
		"exec r.surf.idw &"}
		"Inverse distance weighted from vector points" "" {
		"exec v.surf.idw &"}
		"Regularized spline tension from vector points" "" {
		"exec v.surf.rst &"}
	    }
	    "Fill NULL areas using regularized spline tension" "" {
	    "exec r.fillnulls &"}
	    "Interpolate surfaces from contours" "" {
		"Regularized spline tension from raster contours" "" {
		"exec r.surf.contour &"}
		"Regularized spline tension from vector contours" "" {
		"exec v.surf.rst &"}
	    }
	    -separator
	    "Generate density surface using moving Gausian kernal" "" {
	    "exec v.kernel &"}
	    "Generate fractal surface" "" {
	    "exec r.surf.fractal &"}
	    "Generate gaussian deviates surface" "" {
	    "exec r.surf.gauss &"}
	    "Generate plane" "" {
	    "exec r.plane &"}
	    "Generate random deviates surface" "" {
	    "exec r.surf.random &"}
	    "Generate random surface with spatial dependence" "" {
	    "exec r.random.surface &"}
	}
	"Generate points" "" {
	    "Generate random cells" "" {
	    "exec r.random.cells &"}
	    "Generate random cells & sites from raster map" "" {
	    "exec r.random &"}
	}
	"Generate vector contour lines" "" {
	    "exec r.contour &" }
	-separator
	"Reports & statistics" "" {
	    "Report basic file information" "" {
	    "exec r.info &"}
	    "Report category labels and values" "" {
	    "exec r.cats &"}
	    -separator
	    "General statistics" "" {
	    "exec r.stats &"}
	    "Range of all category values" "" {
	    "exec r.describe &"}
	    "Sum all cell category values" "" {
	    "exec r.sum &"}
	    "Sum area by map and category" "" {
	    "exec r.report &"}
	    "Total surface area, considering toppography" "" {
	    "exec r.surf.area &"}
	    "Univariate statistics" "" {
	    "exec r.univar &"}
	    -separator
	    "Sample values along transects" "" {
	    "exec r.profile &"}
	    "Sample values along transects (use azimuth, distance)" "" {
	    "exec r.transect &"}
	    -separator
	    "Covariance/correlation" "" {
	    "exec r.covar &"}
	    "Linear regression between 2 maps" "" {
	    "exec r.regression.line &"}
	    "Mutual category occurences (coincidence)" "" {
	    "exec r.coin &"}
	}
    }
    Vector "Vector map analysis" {
	"Develop map" "" {
	    "Digitize" "" {
	    "exec v.digit &"}
	    -separator
	    "Create/rebuild topology" "" {
	    "exec v.build &"}
	    "Clean vector files" "" {
	    "exec v.clean &"}
	    -separator
	    "Break lines at intersections" "" {
	    "exec v.topo.check &"}
	    "Build polylines from adjacent segments" "" {
	    "exec v.build.polylines &"}
	    "Split polylines into segments" "" {
	    "exec v.segment"}
	    -separator
	    "Convert vector feature types" "" {
	    "exec v.type &"}
	    -separator
	    "Create text label file for vector features" "" {
	    "exec v.label &"}
	    -separator
	    "Reproject vector from other location" "" {
	    "exec v.proj &"}
	}
	"Register/unregister connections" "" {
	    "Create new vector as link to external OGR layer" "" {
	    "exec  v.external &"}
	    "Register ESRI shapefile" "" {
	    "exec  v.shape.register &"}
	    "Unregister ESRI shapefile" "" {
	    "exec  v.shape.unregister &"}
	    "Register ESRI shapefiles and define PostGRASS connections" "" {
	    "exec  v.format &"}
	    -separator
	    "Set database connection for vector attributes" "" {
	    "exec v.db.connect &"}
	    "Set database & driver for vector attributes" "" {
	    "exec v.database &"}
	}
	"Rectify & georeference vector map" "" {
	"exec v.transform &"}
	-separator
	"Query by attributes" "" {
	"exec v.extract &"}
	"Query by map features" "" {
	"exec v.select &"}
	"Query with mouse" "" {
	"exec d.what.vect -xf &"}
	-separator
	"Buffer features" "" {
	"exec v.buffer &"}
	"Locate nearest features to points or centroids" "" {
	"exec v.distance &"}
	"Network analysis" "" {
	    "Allocate subnets" "" {
	    "exec  v.net.alloc &"}
	    "Network maintenance" "" {
	    "exec  v.net &"}
	    "Shortest route" "" {
	    "exec  v.net.path &"}
	    "Shortest route (visualization only)" "" {
	    "exec  d.path &"}
	    "Split net to bands between cost isolines" "" {
	    "exec  v.net.iso &"}
	    "Steiner tree" "" {
	    "exec  v.net.steiner &"}
	    "Traveling salesman analysis" "" {
	    "exec  v.net.salesman &"}
	}
	"Overlay maps" "" {
	    "Overlay/combine 2 vector maps" "" {
	    "exec v.overlay &"}
	    "Patch multiple maps (combine)" "" {
	    "exec v.patch &"}
	}
	"Sample raster map at vector points" "" {
	"exec v.what.rast &"}
	-separator
	    "Change attributes" "" {
	    "Attach, delete, or report categories" "" {
	    "exec v.category &"}
	    "Reclassify features using rules file" "" {
	    "exec v.reclass &"}
	}
	-separator
	    "Generate vector maps" "" {
	    "Generate convex hull for point set" "" {
	    "exec v.hull &"}
	    "Generate vector points from database x&y coordinates" "" {
	    "exec v.in.db &"}
	    "Generate area feature for extent of current region" "" {
	    "exec v.in.region &"}
	}
	"Generate points" "" {
	    "Generate random points" "" {
	    "exec v.random &"}
	}
	-separator
	"Reports & statistics" "" {
	    	"Basic information" "" {
	    	"exec v.info &"}
		"Load vector attributes to database or create reports" "" {
		"exec v.to.db &"}
	}
    }
    Image "Image processing" {
	"Develop images & groups" "" {
	    "Create/edit imagery group" "" {
	    "exec i.group &"}	
	    "Target imagery group" "" {
	    "exec i.target &"}
	    -separator
	    "Mosaic up to 4 adjacent images" "" {
	    "exec i.image.mosaic &"}
	}
	"Manage image colors" "" {
	    "Transform HIS (Hue/Intensity/Saturation) color image to RGB (Red/Green/Blue)"
		"" {
	    "exec i.his.rgb &"}
	    "Transform RGB (Red/Green/Blue) color image to HIS (Hue/Intensity/Saturation)"
		"" {
	    "exec i.rgb.his &"}
	}
	"Rectify & georeference image group" "" {
	    "Set ground control points (GCP's) from raster map" "" {
	    "exec i.points &"}
	    "Affine and Polynomial rectification (rubber sheet)" "" {
	    "exec i.rectify &"}
	    "Ortho photo rectification" "" {
	    "exec i.ortho.photo &"}
	}
	-separator
	"Brovey transformation and pan sharpening for Landsat ETM, SPOT, & Quickbird" "" {
	"exec i.fusion.brovey &"}
	"Classify image" "" {
		"Clustering input for unsupervised classification" "" {
	    	"exec i.cluster &"}
	    	-separator
	    	"Maximum likelyhood classification (MLC)" "" {
	    	"exec i.maxlik &"}
	    	"Sequential maximum a posteriory classification (SMAP)" "" {
	    	"exec i.smap &"}
	    	-separator
	    	"Interactive input for supervised classification" "" {
	    	"exec i.class &"}
	    	"Non-interactive input for supervised classification (MLC)" "" {
	    	"exec i.gensig &"}
	    	"Non-interactive input for supervised classification (SMAP)" "" {
	    	"exec i.gensigset &"}
	}
	"Dehaze for LandSAT 5" "" {
	"exec i.tm.dehaze &"}
	"Filter image" "" {
	    "Zero edge crossing detection" "" {
	    "exec i.zc &"}
	    "User defined matrix/convolving filter" "" {
	    "exec r.mfilter &"}
	}
	"Spectral response" "" {
	"exec i.spectral &"}
	"Tassled cap vegetation index" "" {
	"exec i.tasscap &"}
	"Transform image" "" {
	    "Canonical component" "" {
	    "exec i.cca &"}
	    "Principal component" "" {
	    "exec i.pca &"}
	    "Fast Fourier Transform" "" {
	    "exec i.fft &"}
	    "Inverse Fast Fourier Transform" "" {
	    "exec i.ifft &"}
	}
	-separator
	"Reports & statistics" "" {
	    "Report basic file information" "" {
	    "exec r.info &"}
	    "Range of image values" "" {
	    "exec r.describe &"}
	    -separator
	    "Kappa classification aaccuracy assessment" "" {
	    "exec r.kappa &"}
	    "Optimum index factor for LandSat TM" "" {
	    "exec i.oif &"}
	}
    }
    "Grid3D" "3D volume management. Use NVIZ to view." {
	"Develop grid3D volumes" "" {
		"Manage nulls for grid3D volume" "" {
		"exec r3.null &"}
		"Manage timestamp for grid3D volume" "" {
		"exec r3.timestamp &"}
	}
	"Create 3D mask for grid3D operations" "" {
	"exec r3.mask &"}
	"Map calculator for grid3D operations" "" {
	"exec r3.mapcalc &"}
	"Interpolate volume from vector points using splines" "" {
	"exec v.vol.rst &"}
	"Report & Statistics" "" {
		"Display information about grid3D volume" "" {
		"exec r3.info &"}
	}
    }
    Databases "Database management" {
	"Manage database" "" {
	    "Connect to database" "" {
	    "exec db.connect &"}
	    "Create empty database" "" {
	    "exec db.createdb &"}
	    "PERMANTLY remove table" "" {
	    "exec db.droptable &"}
	    "Copy table" "" {
	    "exec db.copy &"}
	}
	"Database information" "" {
	    "Describe table" "" {
	    "exec db.describe &"}
	    "List columns" "" {
	    "exec db.columns &"}
	    "List databases" "" {
	    "exec db.databases &"}
	    "List drivers" "" {
	    "exec db.drivers &"}
	    "List tables" "" {
	    "exec db.tables &"}
	}
	-separator
	    "Query" "" {
	    "Query data (SQL select)" "" {
	    "exec db.select &"}
	    "Execute SQL statement" "" {
	    "exec db.execute &"}
	}
	"Manage PostGIS database" "" {
	"exec pg.postgisdb &"}
    }
    Config "Configuration of TclTkGRASS" {
	"Reset menu size" "" {
	resize_menu}
	-separator
	"Set menu font" "" {
	"fontsel {Menu font} main_menu(font);\
			setfont .main_menu $main_menu(font);\
			resize_menu"}
	"Save config" "" {
	"tcltkgrass_save ."}
    }
    "Help" "Help" {
	"Manual pages" "" {
	"exec g.manual -i &"}
	-separator
	"Tcltkgrass menus help" "" {
	"source $env(TCLTKGRASSBASE)/main/help.tcl"}
	"About tcltkgrass" "" {
	"source $env(TCLTKGRASSBASE)/main/about.tcl"}
	-separator
	"About GRASS" "" {
	"source $env(TCLTKGRASSBASE)/main/grassabout.tcl"}
	-separator
	"Help on scripting" "" {
	"source $env(TCLTKGRASSBASE)/main/help-scripting.tcl"}
    }
}
