frame .main_menu
pack .main_menu -expand yes -fill both

# tcltkgrass menu.tcl v 4.0 for GRASS 5.7 2004/04/01 Michael Barton
# based on menu.tcl for GRASS 5 by Michael Barton, Jacques Bouchard, and Markus Neteler
# with scripting support by Andreas Lange

# main menu

menu_build 1 .main_menu {
    File "Files in/out" {
	"Import" "Import maps into GRASS" {
	    "Raster map" "" {
		"Multiple formats using GDAL" "" {
		"exec  r.in.gdal &"}
		-separator
		"GRASS ASCII GRID" "" {
		"exec r.in.ascii"}
		"GRASS ASCII vector map" "" {
		"exec r.in.poly"}
		-separator
		"Binary file (GTOPO30 format)" "" {
		"exec  r.in.bin &"}
		"ERDAS LAN" "" {
		"exec  i.in.erdas &"}
		"GRIDATB.FOR map file (TOPMODEL)" "" {
		"exec r.in.gridatb &"}
	    }
	    "Vector map" "" {
		"Various formats using OGR" "" {
		"exec v.in.ogr &"}
		-separator
		"ASCII GRASS vector file" "" {
		"exec v.in.ascii &"}
		"Import/export old GRASS vector format" "" {
		"exec  v.convert &"}
		-separator
		"Garmin GPS Waypoints/Routes/Tracks" "" {
		"exec v.in.garmin &"}
		"MATLAB MapGen files" "" {
		"exec v.in.mapgen.sh &"}
		"Garmin GPS Waypoints/Routes/Tracks" "" {
		"exec v.in.garmin &"}
		"XY points ascii file" "" {
		"exec s.in.ascii &"}
	    }
	}
	Export "Export maps from GRASS" {
	    "Raster map" "" {
		"GRASS ASCII" "" {
		"exec r.out.ascii &"}
		"XYZ ASCII file" "" {
		"exec r.out.xyz"}
		-separator
		"ESRI ARC/INFO ASCII-GRID" "" {
		"exec r.out.arc &"}
		"ERDAS/LAN" "" {
		"exec i.out.erdas &"}
		"GRIDATB.FOR map file (TOPMODEL)" "" {
		"exec r.out.gridatb"}
		-separator
		"Binary file" "" {
		"exec r.out.bin &"}
		-separator
		"MPEG-1 animations" "" {
		"exec r.out.mpeg &"}
		"PNG image (not georeferenced)" "" {
		"exec r.out.png &"}
		"PPM image (24bit)" "" {
		"exec r.out.ppm &"}
		"PPM image from red, green, blue raster maps" "" {
		"exec r.out.ppm3 &"}
		"POVray height-field" "" {
		"exec r.out.pov &"}
		"TIFF image (8/24bit)" "" {
		"exec r.out.tiff &"}
	    }
	    "Vector map" "" {
		"ASCII GRASS vector file" "" {
		"exec v.out.ascii &"}
		"Import/export old GRASS vector format" "" {
		"exec  v.convert &"}
		"POV-Ray format" "" {
		"exec  v.out.pov &"}
		"Various formats using OGR (SHAPE, MapInfo etc)" "" {
		"exec v.out.ogr &"}
		-separator
		"ASCII file points file" "" {
		"exec  s.out.ascii &"}
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
		"" {
	    "exec xwd -out map.xwd"}
	    "PNG (save currently selected display to 24 bit PNG file)" "" {
	    "exec d.out.png"}
	}
	"Print driver output" "" {
	    "Display label" "" {
	    "exec d.paint.labels &"}
	    "Postscript map creation" "" {"exec ps.map &"}
	}
	"Print (not yet implemented)" "" {"do_nothing"}
	"Quit" "Bye" resize_menu;quit
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
	    "Show projection information & create projection files" "" {
	    "exec g.proj &"}
	}
	"3D region" "Grid3D region management" {
	    "Create WIND3 (default 3D window) from current 2D region" "" {
	    "exec g3.createwind"}
	    "Manage 3D region" "" {
	    "exec g3.setregion"}
	}
	"Map type conversions" "raster<->vector<->sites<->grid3D" {
	    "Raster to vector map" "" {
	    "exec r.to.vect &"}
	    "Vector to raster" "" {
	    "exec v.to.rast &"}
	    "Vector to points" "" {
	    "exec v.to.points &"}
	    "Sites to vector" "" {
	    "exec v.in.sites &"}
	}
	"Other" "" {
	    "Create/edit projection information for current location" "" {
	    "exec g.setproj &"}
	    "Show current GRASS environment settings" "" {
	    "exec g.gisenv &"}
	    "Show current GRASS version" "" {
	    "exec g.version &"}
	}
    }
    Display "Display maps" {
	"Display Manager" "" {
	"exec d.m &"}
	"NVIZ visualization tool" "" {
	"exec nviz -q &"}
	-separator
	    "Start displays" "" {
	    "[not functional ] All active saved X" "" {
	    start_monitors}
	    -separator
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
	    "[not functional ]CELL" "" {
	    "exec d.mon start=CELL &"}
	    -separator
	    "Start/restart display at specified window size" "" {
	    "exec d.monsize"}
	}
	"Stop displays" "" {
	    "[not functional] All X" "" {
	    stop_monitors}
	    -separator
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
	    -separator
	    "[not functional] CELL" "" {
	    "exec d.mon select=CELL"}
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
	    "exec d.shadedmap"}
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
	    "Overlay bar scale and north arrow" "" {
	    "exec d.barscale *"}
	    "Overlay line scale and north arrow" "" {
	    "exec d.scale *"}
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
	"Pan in active display" "" {
	"exec d.pan &"}
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
	"exec mapcalc &"}
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
	"Generate surfaces" "" {
	    "Interpolate surfaces from points" "" {
		"Bilinear from points" "" {
		"exec r.bilinear &"}
		"Inverse distance weighted from raster (Lat./Long. locations)"
		    "" {
		"exec r.surf.idw &"}
	    }
	    "Interpolate surfaces from contours" "" {
		"Regularized spline tension from raster contours" "" {
		"exec r.surf.contour &"}
		"Regularized spline tension from vector contours" "" {
		"exec v.surf.rst &"}
	    }
	    -separator
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
	"exec d.what.vect &"}
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
	    "exec  v.net.salsman &"}
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
	    "exec v.in region &"}
	}
	"Generate points" "" {
	    "Generate random points" "" {
	    "exec v.random &"}
	}
	-separator
	    "Reports & statistics" "" {
	    "Basic information" "" {
	    "exec v.info &"}
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
	    "User defined matrix filter" "" {
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
	"Load vector to DB" "" {
	"exec v.to.db &"}
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
	"Configure html-browser" "" {
	"config_netscape"}
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
	"Help on html-browser" "" {
	"source $env(TCLTKGRASSBASE)/main/help-netscape.tcl"}
    }
}
