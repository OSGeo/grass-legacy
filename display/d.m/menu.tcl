# Updated 15-10-2004 by Michael Barton, Arizona State University
# menu.tcl
# produces menu bar for d.m


 set descmenu [subst -novariables {
 "&File" all file 1 {
	 {cascad "Import" {} "" 1 {			
 		{cascad "Raster map" {} "" 1 {			
 		{command "Multiple formats using GDAL" {} "r.in.gdal" {} -command { execute r.in.gdal }}
 		{separator}
 		{command "ASCII GRID (includes GRASS ASCII)" {} "r.in.ascii" {} -command { execute r.in.ascii }}
 		{command "GRASS ASCII vector map" {} "r.in.poly" {} -command { execute r.in.poly }}
 		{separator}
 		{command "Binary file (includes GTOPO30 format)" {} "r.in.bin" {} -command { execute r.in.bin }}
 		{command "ESRI Arc/Info ASCII grid" {} "r.in.arc" {} -command { execute r.in.arc }}
 		{command "GRIDATB.FOR map file (TOPMODEL)" {} "r.in.gridatb" {} -command { execute r.in.gridatb }}
 		{command "MAT-File (v.4) map file (Matlab or Octave)" {} "r.in.gridatb" {} -command { execute r.in.mat }}
 		{command "SRTM hgt files" {} "r.in.srtm" {} -command { execute r.in.srtm }}
 	 }}
 	 {cascad "Vector map" {} "" 1 {			
 		{command "Various formats using OGR" {} "v.in.ogr" {} -command { execute v.in.ogr }}
 		{separator}
 		{command "ASCII points file or GRASS ASCII vector file" {} "v.in.ascii" {} -command { execute v.in.ascii }}
 		{command "Import old GRASS vector format" {} "v.convert" {} -command { execute v.convert }}
 		 {separator}
 	 	{command "ESRI e00 format" {} "v.in.e00" {} -command { execute v.in.e00 }}
 	 	{command "Garmin GPS Waypoints/Routes/Tracks" {} "v.in.garmin" {} -command { execute v.in.garmin }}
 		{command "MATLAB MapGen files" {} "v.in.mapgen.sh" {} -command { execute v.in.mapgen.sh }}
 		{separator}
 	 }}
 	 {cascad "Grid 3D" {} "" 1 {			
 		{command "ASCII 3D file" {} "r3.in.ascii" {} -command { execute r3.in.ascii }}
 	 	{command "Vis5D file" {} "r3.in.v5d" {} -command { execute r3.in.v5d }}
 	 }}
 	}}
 	{cascad "Export" {} "" 1 {			
 	 {cascad "Raster map" {} "" 1 {			
 		{command "ASCII grid (for GRASS, Surfer, Modflow, etc)" {} "r.out.ascii" {} -command { execute r.out.ascii }}
 		{separator}
 		{command "ESRI ARC/INFO ASCII grid" {} "r.out.arc" {} -command { execute r.out.arc }}
 		 {command "GRIDATB.FOR map file (TOPMODEL)" {} "r.out.gridatb" {} -command { execute r.out.gridatb }}
 		{command "MAT-File (v.4) map file (Matlab or Octave)" {} "r.out.mat" {} -command { execute r.out.mat }}
 		{separator}
 		{command "Binary file" {} "r.out.bin" {} -command { execute r.out.bin }}
 		{separator}
 		{command "MPEG-1 animations" {} "r.out.mpeg" {} -command { execute r.out.mpeg }}
 		{command "PNG image (not georeferenced)" {} "r.out.png" {} -command { execute r.out.png }}
 		{command "PPM image (24bit)" {} "r.out.ppm" {} -command { execute r.out.ppm }}
 		{command "PPM image from red, green, blue raster maps" {} "r.out.ppm3" {} -command { execute r.out.ppm3 }}
 	{command "POVray height-field" {} "r.out.pov" {} -command { execute r.out.pov }}
 		{command "TIFF image (8/24bit)" {} "r.out.pov" {} -command { execute r.out.tiff }}
 	 }}
 	 {cascad "Vector map" {} "" 1 {			
 		{command "Various formats using OGR (SHAPE, MapInfo etc)" {} "v.out.ogr" {} -command { execute v.out.ogr }}
 		{separator}
 		{command "DXF file (ASCII)" {} "v.out.dxf" {} -command { execute v.out.dxf }}
 		{command "ASCII vector or point file/old GRASS ASCII vector file" {} "v.out.ascii" {} -command { execute v.out.ascii }}
 		{command "POV-Ray format" {} "v.out.pov" {} -command { execute v.out.pov }}
 		{separator}
 	 }}
 	 {cascad "Grid 3D" {} "" 1 {			
 		{command "ASCII 3D file" {} "r3.out.ascii" {} -command { execute r3.out.ascii }}
 		{command "Vis5D file" {} "r3.out.v5d" {} -command { execute r3.out.v5d }}
 	 }}
 	}}
 {separator}
 	{cascad "Workspace" {} "" 1 {			
 {command "New" {} "Create new workspace file" {} -accelerator Ctrl-N -command { Dm::new}}
 	 {command "Open..." {} "Open workspace file" {} -accelerator Ctrl-O -command { Dm::OpenFileBox {}}}
 	 {command "Save" {} "Save workspace file" {} -accelerator Ctrl-S -command { Dm::SaveFileBox {}}}
 	 {command "Save as..." {} "Save workspace file as name" {} -command { catch {unset ::Dm::filename} ; Dm::SaveFileBox {}}}
 	 {command "Close" {} "Close workspace" {} -accelerator Ctrl-W -command { Dm::FileClose {}}}
 }}
 {separator}
 	{cascad "Save display to image file" {} "" 1 {			
 {command "XWD (Save display, selected with mouse, to map.xwd in home directory )" {} "" {} -command { spawn xwd -out map.xwd }}
 {command "PNG (save currently selected display to 24 bit PNG file)" {} "d.out.png" {} -command { execute d.out.png }}
 }}
 	{command "Save map to Postscript file" {} "ps.map" {} -command { execute ps.map }}
 	{command "Print (requires ghostscript)" {} "print" {} -accelerator Ctrl-P -command {Dm::print} }
 	{separator}
 {command "E&xit" {} "Exit Display Manager" {} -accelerator Ctrl-Q -command { DmPrint::clean; exit } }
 }
 "&GIS" all options 1 {
 	{cascad "Manage maps and grid3D files" {} "" 1 {			
 	 {command "Copy maps" {} "g.copy" {} -command {execute g.copy }}
 	 {command "List maps" {} "g.list" {} -command {execute g.list }}
 	 {command "List maps using expressions and 'wildcards'" {} "g.mlist" {} -command {execute g.mlist }}
 	 {command "Rename maps" {} "g.rename" {} -command {execute g.rename }}
 	 {command "Remove maps" {} "g.remove" {} -command {execute g.remove }}
 	 {command "Remove maps using expressions and 'wildcards'" {} "g.mremove" {} -command {execute g.mremove }}
 	 {separator}
 	 {command "List Grid3D files" {} "g3.list" {} -command {execute g3.list }}
 	 {command "Rename Grid3D volumes" {} "g3.rename" {} -command {execute g3.rename }}
 	 {command "Remove Grid3D volumes" {} "g3.remove" {} -command {execute g3.remove }}
 }}
 	{cascad "Region" {} "" 1 {			
 	{command "Display region settings" {} "g.region -p" {} -command {run g.region -p }}
 	 {command "Manage region" {} " g.region" {} -command {execute g.region }}
 	 {command "Select default region" {} "g.region -d" {} -command {run g.region -d ; run d.redraw }}
 	 {command "Zoom to maximum extent of all displayed maps" {} "d.extend" {} -command {run d.extend }}
 	 {separator}
 	 {command "Create WIND3 (default 3D window) from current 2D region" {} "g3.createwind" {} -command {execute g3.createwind }}
 	 {command "Manage 3D region" {} "g3.setregion" {} -command {execute g3.setregion }}
 	}}
 	{cascad "GRASS working environment" {} "" 1 {			
 	 {command "Modify access by other users to current mapset" {} "g.access" {} -command {term g.access }}
 	 {command "Modify mapset search path" {} "g.mapsets.tcl" {} -command {spawn $env(GISBASE)/etc/g.mapsets.tcl}}
 	 {command "Change current working session to new mapset, location, or GISDBASE" {} "g.mapset" {} -command {execute g.mapset }}
 {command "Show current GRASS environment settings" {} "g.gisenv" {} -command {execute g.gisenv }}
 {command "Show current GRASS version" {} "g.version -c" {} -command {run g.version -c }}
 	}}
 	{cascad "Map type conversions" {} "" 1 {			
 	 {command "Raster to vector map" {} "r.to.vect" {} -command {execute r.to.vect }}
 	 {command "Vector to raster" {} "v.to.rast" {} -command {execute v.to.rast }}
 	 {command "Vector to points" {} "v.to.points" {} -command {execute v.to.points }}
 	 {command "Sites to vector" {} "v.in.sites" {} -command {execute v.in.sites }}
 	}}
 	{cascad "Manage projections" {} "" 1 {			
 	 {command "Create/edit projection information for current location" {} "g.setproj" {} -command {term g.setproj }}
 	 {command "Show projection information and create projection files" {} "g.proj" {} -command {execute g.proj }}
 	}}
 } 
 "&Display" all options 1 {
			{command "Start NVIZ (n-dimensional visualization module)" {} "nviz" {} -command {execute nviz }}
			{command "Create fly-through animation path for NVIZ" {} "d.nviz" {} -command {execute d.nviz }}
 	{separator}
 	{cascad "Start displays" {} "d.mon start" 1 [monitor_menu start]}
			{cascad "Select displays" {} "d.mon select" 1 [monitor_menu select]}
			{cascad "Stop displays" {} "d.mon stop" 1 [monitor_menu stop]}
			{command "Start/restart display at specified window size" {} "d.monsize" {} -command {execute d.monsize }}
			{command "Set active display to specified size" {} "d.resize" {} -command {execute d.resize }}
 	{separator}
	 {cascad "Display raster maps" {} "" 1 {
			 {command "Display raster map" {} "d.rast" {} -command {execute d.rast }}
			 {command "Display raster map, legend, and title in active display" {} "d.rast.leg" {} -command {execute d.rast.leg }}
 	 {separator}
			 {command "Display HIS overlays" {} "d.his" {} -command {execute d.his }}
			 {command "Display RGB overlays" {} "d.rgb" {} -command {execute d.rgb }}
			 {command "Drape raster map over shaded relief or aspect map" {} "d.shadedmap" {} -command {execute $env(GISBASE)/etc/dm/script/d.shadedmap }}
 	 {separator}
			 {command "Slide show of all raster maps in current mapset" {} "d.slide.show" {} -command {execute d.slide.show }}
	 }}
			{command "Display vector maps" {} "d.vect" {} -command {execute d.vect }}
			{cascad "Display text on maps" {} "" 1 {			
		 	{command "Display legend for raster maps" {} "d.legend" {} -command {execute d.legend }}
		 	{command "Display category values in raster map cells" {} "d.rast.num" {} -command {execute d.rast.num }}
 	 {separator}
		 	{command "Create map title file for use by d.text" {} "d.title" {} -command {execute d.title }}
		 	{command "Display text labels for paint output" {} "d.paint.labels" {} -command {execute d.paint.labels }}
 	 {separator}
		 	{command "Select text font" {} "d.font" {} -command {execute d.font }}
 	 {separator}
 	 		{command "Draw text" {} "d.text" {} -command {execute d.text }}
		 	{command "Draw text using TrueType fonts" {} "d.text.freetype" {} -command {execute d.text.freetype }}
 	 {separator}
		 	{command "Display standard GRASS fonts" {} "show.fonts.sh" {} -command {execute show.fonts.sh }}
	 }}
			{cascad "Display graphics on maps" {} "" 1 {			
	 		{command "Overlay scale and north arrow" {} "d.barscale" {} -command {execute d.barscale }}
 {separator}
	 		{command "Display graphs at vector point localities" {} "d.vect.chart" {} -command {execute d.vect.chart }}
	 		{command "Display histogram" {} "d.histogram" {} -command {execute d.histogram }}
	 		{command "Display line graph" {} "d.linegraph" {} -command {execute d.linegraph }}
 	 {separator}
	 		{command "Overlay grid" {} "d.grid" {} -command {execute d.grid }}
	 		{command "Display geodesic line" {} "d.geodesic" {} -command {execute d.geodesic }}
	 		{command "Display rhumbline" {} "d.rhumbline" {} -command {execute d.rhumbline }}
 	 {separator}
	 		{command "Display color table" {} "d.colortable" {} -command {execute d.colortable }}
 	 {separator}
	 		{command "Overlay slope arrows on aspect raster map" {} "d.rast.arrow" {} -command {execute d.rast.arrow }}
 	 {separator}
	 		{command "Draw simple graphics in active display monitor (display coordinates)" {} "d.graph" {} -command {execute d.graph }}
	 		{command "Draw simple graphics in active display monitor (map coordinates)" {} "d.mapgraph" {} -command {execute d.mapgraph }}
 	}}
			{command "Split active display and show maps in each half" {} "d.split" {} -command {execute d.split }}
 	{separator}
			{command "Manage displays" {} "d.mon" {} -command {execute d.mon }}
			{command "Manage display frames" {} "d.frame" {} -command {execute d.frame }}
			{command "Display information about active display monitor" {} "d.info" {} -command {execute d.info }}
 	{separator}
			{command "Redraw active display (Note: some items may not be redrawn)" {} "d.redraw" {} -command {execute d.redraw }}
			{command "Save file of commands to recreate active display" {} "d.save" {} -command {execute d.save }}
 	{separator}
			{command "Measure lengths and areas" {} "d.measure" {} -command {term d.measure -s}}
			{command "Show geographical position" {} "d.where" {} -command {execute d.where }}
 	{separator}
			{command "Zoom/Unzoom/Pan with options" {} "d.zoom" {} -command {execute d.zoom }}	
 	{separator}
			{command "Erase active display/frame to selected color" {} "d.erase" {} -command {execute d.erase }}
 }
 "&Raster" all options 1 {
			{cascad "Develop map" {} "" 1 {			
			 {command "Digitize raster" {} "r.digit" {} -command {term r.digit }}
			 {separator}
			 {command "Compress/decompress raster file" {} "r.compress" {} -command {execute r.compress }}
			 {command "Manage boundary definitions" {} "r.region" {} -command {execute r.region }}
			 {command "Manage null values" {} "r.null" {} -command {execute r.null }}
			 {command "Manage timestamps for files" {} "r.timestamp" {} -command {execute r.timestamp }}
			 {command "Quantization for floating-point maps" {} "r.quant" {} -command {execute r.quant }}
			 {command "Resample (change resolution) using nearest neighbor method" {} "r.resample" {} -command {execute r.resample }}
			 {command "Resample (change resolution) using regularized spline tension" {} "r.resamp.rst" {} -command {execute r.resamp.rst }}
			 {separator}
			 {command "Reproject raster from other location" {} "r.proj" {} -command {execute r.proj }}
			}}
			{cascad "Manage map colors" {} "" 1 {			
			 {command "Modify color table" {} "d.colors.sh" {} -command {execute d.colors.sh }}
			 {command "Set colors to predefined color tables" {} "r.colors" {} -command {execute r.colors }}
			 {command "Set colors using color rules" {} "r.colors.rules" {} -command {execute $env(GISBASE)/etc/dm/script/r.colors.rules }}
			 {separator}
			 {command "Blend 2 color maps to produce 3 RGB files" {} "r.blend" {} -command {execute r.blend }}
			 {command "Create color image from RGB files" {} "r.composite" {} -command {execute r.composite }}
			 {command "Create 3 RGB (red, green, blue) maps from 3 HIS (hue, intensity, saturation) maps" {} "r.his" {} -command {execute r.his }}
			}}
			{separator}
			{command "Query by coordinate(s)" {} "r.what" {} -command { execute r.what }}
			{command "Query with mouse" {} "d.what.rast" {} -command { execute d.what.rast }}
			{separator}
			{command "Create raster buffers" {} "r.buffer" {} -command { execute r.buffer }}
			{command "Locate closest points between areas in 2 raster maps" {} "r.distance" {} -command { execute r.distance }}
			{command "Map calculator" {} "r.mapcalculator" {} -command { execute r.mapcalculator }}
			{cascad "Neighborhood analysis" {} "" 1 {			
 			 {command "Moving window analysis of raster cells" {} "r.neighbors" {} -command { execute r.neighbors }}
 			 {command "Analyze vector points in neighborhood of raster cells" {} "v.neighbors" {} -command { execute v.neighbors }}
			}}
			{cascad "Overlay maps" {} "" 1 {			
			 {command "Cross product" {} "r.cross" {} -command {execute r.cross }}
			 {command "Function of map series (time series)" {} "r.series" {} -command {execute r.series }}
			 {command "Patch maps" {} "r.patch" {} -command {execute r.patch }}
			 {separator}
			 {command "Statistical calculations for cover map over base map" {} "r.statistics" {} -command {execute r.statistics }}
			}}
			{cascad "Solar radiance and shadows" {} "" 1 {			
			 {command "Solar irradiance and daily irradiation" {} "r.sun" {} -command {execute r.sun }}
			 {command "Shadows map for sun position or date/time" {} "r.sunmask" {} -command {execute r.sunmask }}
			}}
			{cascad "Terrain analysis" {} "" 1 {			
			 {command "Cost surface" {} "r.cost" {} -command {execute r.cost }}
			 {command "Least cost route or flow" {} "r.drain" {} -command {execute r.drain }}
			 {command "Profile analysis" {} "d.profile" {} -command {execute d.profile }}
			 {command "Shaded relief map" {} "r.shaded.relief" {} -command {execute r.shaded.relief }}
			 {command "Slope and aspect" {} "r.slope.aspect" {} -command {execute r.slope.aspect }}
			 {command "Terrain parameters" {} "r.param.scale" {} -command {execute r.param.scale }}
			 {command "Textural features" {} "r.texture" {} -command {execute r.texture }}
			 {command "Visibility/line of sight" {} "r.los" {} -command {execute r.los }}
			}}
			{cascad "Transform features" {} "" 1 {			
			 {command "Clump small areas" {} "r.clump" {} -command {execute r.clump }}
			 {command "Grow areas" {} "r.grow" {} -command {execute r.grow }}
			 {command "Thin linear features" {} "r.thin" {} -command {execute r.thin }}
			}}
			{separator}
			{cascad "Hydrologic modeling" {} "" 1 {			
			 {command "Depressionless elevation map and flowline map" {} "r.fill.dir" {} -command {execute r.fill.dir }}
			 {command "Flow accumulation for massive grids" {} "r.terraflow" {} -command {spawn r.terraflow }}
			 {command "Generate flow lines for raster map" {} "r.flow" {} -command {spawn r.flow }}
			 {command "Topographic index map" {} "r.topidx" {} -command {execute r.topidx }}
			 {command "TOPMODEL simulation" {} "r.topmodel" {} -command {execute r.topmodel }}
			 {command "Watershed subbasins" {} "r.basins.fill" {} -command {execute r.basins.fill }}
			 {command "Watershed analysis" {} "r.watershed" {} -command {execute r.watershed }}
			 {command "Watershed basin creation" {} "r.water.outlet" {} -command {execute r.water.outlet }}
			}}
			{cascad "Landscape structure modeling" {} "" 1 {			
			 {command "Set up sampling and analysis framework" {} "r.le.setup" {} -command {term r.le.setup }}
			 {separator}
			 {command "Analyze landscape characteristics" {} "r.le.pixel" {} -command {execute r.le.pixel }}
			 {command "Analyze landscape patch characteristics" {} " r.le.patch" {} -command {execute r.le.patch }}
			 {command "Output landscape patch information" {} "r.le.trace" {} -command {execute r.le.trace }}
			}}
			{cascad "Wildfire modeling" {} "" 1 {			
			 {command "Generate rate of spread (ROS) maps" {} "r.ros" {} -command {execute r.ros }}
			 {command "Generate least-cost spread paths" {} "r.spreadpath" {} -command {execute r.spreadpath }}
			 {command "Simulate anisotropic spread phenomena" {} "r.spread" {} -command {execute r.spread }}
			}}
			{separator}
			{cascad "Change category values and labels" {} "" 1 {			
			 {command "Edit category values of individual cells for displayed raster map" {} "d.rast.edit" {} -command {term d.rast.edit }}
			 {separator}
			 {command "Reclassify categories for areas of specified sizes" {} "r.reclass.area" {} -command {execute r.reclass.area }}
			 {command "Reclassify categories using rules" {} "r.reclass.rules" {} -command {execute $env(GISBASE)/etc/dm/script/r.reclass.rules }}
			 {command "Reclassify categories using rules file" {} "r.reclass.file" {} -command {execute $env(GISBASE)/etc/dm/script/r.reclass.file }}
			 {separator}
			 {command "Recode categories using rules (create new map)" {} "r.recode.rules" {} -command {execute $env(GISBASE)/etc/dm/script/r.recode.rules }}
			 {command "Recode categories using rules file (create new map)" {} "r.recode.file " {} -command {execute $env(GISBASE)/etc/dm/script/r.recode.file }}
			 {separator}
			 {command "Rescale categories (create new map)" {} "r.rescale" {} -command {execute r.rescale }}
			 {command "Rescale categories with equalized histogram (create new map)" {} "r.rescale.eq" {} -command {execute r.rescale.eq }}
			}}
			{separator}
			{command "Generate concentric circles around points" {} "r.circle" {} -command { execute r.circle }}
			{cascad "Generate random raster cells" {} "" 1 {			
			 {command "Generate random cells" {} "r.random.cells" {} -command {execute r.random.cells }}
			 {command "Generate random cells and vector points from raster map" {} "r.random" {} -command {execute r.random }}
			}}
			{cascad "Generate surfaces" {} "" 1 {			
			 {command "Generate density surface using moving Gausian kernal" {} "v.kernel" {} -command {execute v.kernel }}
			 {command "Generate fractal surface" {} "r.surf.fractal" {} -command {execute r.surf.fractal }}
			 {command "Generate gaussian deviates surface" {} "r.surf.gauss" {} -command {execute r.surf.gauss }}
			 {command "Generate plane" {} "r.plane" {} -command {execute r.plane }}
			 {command "Generate random deviates surface" {} "r.surf.random" {} -command {execute r.surf.random }}
			 {command "Generate random surface with spatial dependence" {} "r.random.surface" {} -command {execute r.random.surface }}
			}}
			{command "Generate vector contour lines" {} "r.contour" {} -command { execute r.contour }}
			{cascad "Interpolate surfaces" {} "" 1 {			
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
			{cascad "Reports and statistics" {} "" 1 {			
			 {command "Report basic file information" {} "r.info" {} -command {execute r.info }}
			 {command "Report category labels and values" {} "r.cats" {} -command {execute r.cats }}
			 {separator}
			 {command "General statistics" {} "r.stats" {} -command {execute r.stats }}
			 {command "Range of all category values" {} "r.describe" {} -command {execute r.describe }}
			 {command "Sum all cell category values" {} "r.sum" {} -command {execute r.sum }}
			 {command "Sum area by map and category" {} "r.report" {} -command {execute r.report }}
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
 "&Vector" all options 1 {
			{cascad "Develop map" {} "" 1 {			
			 {command "Digitize" {} "v.digit" {} -command {execute v.digit }}
			 {separator}
			 {command "Create/rebuild topology" {} "v.build" {} -command {execute v.build }}
			 {command "Clean vector files" {} "v.clean" {} -command {execute v.clean }}
			 {separator}
			 {command "Break lines at intersections" {} "v.topo.check" {} -command {execute v.topo.check }}
			 {command "Build polylines from adjacent segments" {} "v.build.polylines" {} -command {execute v.build.polylines }}
			 {command "Split polylines into segments" {} "v.segment" {} -command {execute v.segment }}
			 {separator}
			 {command "Convert vector feature types" {} "v.type" {} -command {execute v.type }}
			 {separator}
			 {command "Create text label file for vector features" {} "v.label" {} -command {execute v.label }}
			 {separator}
			 {command "Reproject vector from other location" {} "v.proj" {} -command {execute v.proj }}
			}}
			{cascad "Vector<->database connections" {} "" 1 {			
			 {command "Create new vector as link to external OGR layer" {} "v.external" {} -command {execute v.external }}
			 {command "Set database connection for vector attributes" {} "v.db.connect" {} -command {execute v.db.connect }}
			}}
			{command "Rectify and georeference vector map" {} "v.transform" {} -command {execute v.transform }}
			{separator}
			{command "Query by attributes" {} "v.extract" {} -command {execute v.extract }}
			{command "Query by map features" {} " v.select" {} -command {execute v.select }}
			{command "Query with mouse (form mode, editing enabled)" {} "d.what.vect -ef" {} -command {spawn d.what.vect -ef}}
			{separator}
			{command "Create vector buffers" {} "v.buffer" {} -command {execute v.buffer }}
			{command "Locate nearest features to points or centroids" {} "v.distance" {} -command {execute v.distance }}
			{cascad "Network analysis" {} "" 1 {			
			 {command "Allocate subnets" {} "v.net.alloc" {} -command {execute v.net.alloc }}
			 {command "Network maintenance" {} "v.net" {} -command {execute v.net }}
			 {command "Shortest route" {} "v.net.path" {} -command {execute v.net.path }}
			 {command "Shortest route (visualization only)" {} "d.path" {} -command {execute d.path }}
			 {command "Split net to bands between cost isolines" {} "v.net.iso" {} -command {execute v.net.iso }}
			 {command "Steiner tree" {} "v.net.steiner" {} -command {execute v.net.steiner }}
			 {command "Traveling salesman analysis" {} "v.net.salesman" {} -command {execute v.net.salesman }}
			}}
			{cascad "Overlay maps" {} "" 1 {			
			 {command "Overlay/combine 2 vector maps" {} "v.overlay" {} -command {execute v.overlay }}
			 {command "Patch multiple maps (combine)" {} "v.patch" {} -command {execute v.patch }}
			}}
			{command "Generate area feature for extent of current region" {} "v.in.region" {} -command {execute v.in.region }}
			{command "Generate rectangular vector grid" {} "v.mkgrid" {} -command {execute v.mkgrid }}
			{separator}
			{cascad "Change attributes" {} "" 1 {			
			 {command "Attach, delete, or report categories" {} "v.category" {} -command {execute v.category }}
			 {command "Reclassify features using rules file" {} "v.reclass" {} -command {execute v.reclass }}
			}}
			{separator}
			{cascad "Work with vector points" {} "" 1 {			
 			{cascad "Generate points" {} "" 1 {			
			 {command "Generate points from database with x/y coordinates" {} "v.in.db" {} -command {execute v.in.db }}
 			 {command "Generate random points" {} "v.random" {} -command {execute v.random }}
 			 {command "Random location perturbations of points" {} "v.perturb" {} -command {execute v.perturb }}
 			}}
 			{cascad "Generate areas from points" {} "" 1 {			
 			 {command "Generate convex hull for point set" {} "v.hull" {} -command {execute v.hull }}
 			 {command "Generate Delaunay triangles for point set" {} "v.delaunay" {} -command {execute v.delaunay }}
 			 {command "Generate Voronoi diagram/Thiessen polygons for point set" {} "v.voronoi" {} -command {execute v.voronoi }}
 			}}
 			{cascad "Sample raster maps" {} "" 1 {			
 			{command "Sample raster map at point locations" {} "v.what.rast" {} -command {execute v.what.rast }}
 			{command "Sample raster neighborhood around points" {} "v.sample" {} -command {execute v.sample }}
 			}}
 			{command "Partition points into test/training sets for k-fold cross validatation" {} "v.kcv" {} -command {execute v.kcv }}
			}}
			{separator}
			{cascad "Reports and statistics" {} "" 1 {			
			 {command "Basic information" {} "v.info" {} -command {execute v.info }}
			 {command "Load vector attributes to database or create reports" {} "v.to.db" {} -command {execute v.to.db }}
			 {command "Univariate statistics" {} "v.univar" {} -command {execute v.univar }}
 			{separator}
			 {command "Test normality of point distribution" {} "v.normal" {} -command {execute v.normal }}
			 {command "Indices of point counts in quadrats" {} "v.qcount" {} -command {execute v.qcount }}
			}}
 } 
 "&Image" all options 1 {			
			{cascad "Develop images and groups" {} "" 1 {			
			 {command "Create/edit imagery group" {} "i.group" {} -command {execute i.group }}			
			 {command "Target imagery group" {} "i.target" {} -command {execute i.target }}
			 {separator}
			 {command "Mosaic up to 4 adjacent images" {} "i.image.mosaic" {} -command {execute i.image.mosaic }}
			}}
			{cascad "Manage image colors" {} "" 1 {			
			 {command "Transform HIS (Hue/Intensity/Saturation) color image to RGB (Red/Green/Blue)" {} "i.his.rgb" {} -command {execute i.his.rgb }}
			 {command "Transform RGB (Red/Green/Blue) color image to HIS (Hue/Intensity/Saturation)" {} "i.rgb.his" {} -command {execute i.rgb.his }}
			}}
			{cascad "Rectify and georeference image group" {} "" 1 {			
			 {command "Set ground control points (GCP's) from raster map or keyboard entry" {} "i.points" {} -command {term i.points}}
			 {command "Set ground control points (GCP's) from vector map or keyboard entry" {} "i.vpoints" {} -command {term i.vpoints}}
			 {command "Affine and Polynomial rectification (rubber sheet)" {} "i.rectify" {} -command {execute i.rectify }}
			 {command "Ortho photo rectification" {} "i.ortho.photo" {} -command {term i.ortho.photo }}
			}}
			{separator}
			{command "Brovey transformation and pan sharpening for Landsat ETM, SPOT, and Quickbird" {} "i.fusion.brovey" {} -command {execute i.fusion.brovey }}
			{cascad "Classify image" {} "" 1 {			
			 {command "Clustering input for unsupervised classification" {} "i.cluster" {} -command {execute i.cluster }}
			 {separator}
			 {command "Maximum likelyhood classification (MLC)" {} "i.maxlik" {} -command {execute i.maxlik }}
			 {command "Sequential maximum a posteriory classification (SMAP)" {} "i.smap" {} -command {execute i.smap }}
			 {separator}
			 {command "Interactive input for supervised classification" {} "i.class" {} -command {term i.class }}
			 {command "Non-interactive input for supervised classification (MLC)" {} "i.gensig" {} -command {execute i.gensig }}
			 {command "Non-interactive input for supervised classification (SMAP)" {} "i.gensigset" {} -command {execute i.gensigset }}
			}}
			{cascad "Filter image" {} "" 1 {			
			 {command "Zero edge crossing detection" {} "i.zc" {} -command {execute i.zc }}
			 {command "User defined matrix/convolving filter" {} "r.mfilter" {} -command {execute r.mfilter }}
			}}
			{command "Spectral response" {} "i.spectral" {} -command {execute i.spectral }}
			{command "Tassled cap vegetation index" {} "i.tasscap" {} -command {execute i.tasscap }}
			{cascad "Transform image" {} "" 1 {			
			 {command "Canonical component" {} "i.cca" {} -command {execute i.cca }}
			 {command "Principal component" {} "i.pca" {} -command {execute i.pca }}
			 {command "Fast Fourier Transform" {} "i.fft" {} -command {execute i.fft }}
			 {command "Inverse Fast Fourier Transform" {} "i.ifft" {} -command {execute i.ifft }}
			}}
			{separator}
			{cascad "Reports and statistics" {} "" 1 {			
			 {command "Report basic file information" {} "r.info" {} -command {execute r.info }}
			 {command "Range of image values" {} "r.describe" {} -command {execute r.describe }}
			 {separator}
			 {command "Kappa classification accuracy assessment" {} "r.kappa" {} -command {execute r.kappa }}
			 {command "Optimum index factor for LandSat TM" {} "i.oif" {} -command {execute i.oif }}
			}}
 } 
 "&Grid3D" all options 1 {
			{cascad "Develop grid3D volumes" {} "" 1 {			
			 {command "Manage nulls for grid3D volume" {} "r3.null" {} -command {execute r3.null }}
			 {command "Manage timestamp for grid3D volume" {} "r3.timestamp" {} -command {execute r3.timestamp }}
			}}
			{command "Create 3D mask for grid3D operations" {} "r3.mask" {} -command {execute r3.mask }}
			{command "Create display file for grid3D volume" {} "r3.mkdspf" {} -command { execute r3.mkdspf }}
			{command "Map calculator for grid3D operations" {} "r3.mapcalculator" {} -command {execute r3.mapcalculator }}
			{command "Interpolate volume from vector points using splines" {} "v.vol.rst" {} -command {execute v.vol.rst }}
			{cascad "Report and Statistics" {} "" 1 {			
			 {command "Display information about grid3D volume" {} "r3.info" {} -command {execute r3.info }}
			}}
 } 
 "&Databases" all options 1 {
			{cascad "Manage database" {} "" 1 {			
			 {command "Connect to database" {} "db.connect" {} -command {execute db.connect }}
			 {command "PERMANTLY remove table" {} "db.droptable" {} -command {execute db.droptable }}
			 {command "Copy table" {} "db.copy" {} -command {execute db.copy }}
			 {command "Test database" {} "db.test" {} -command {execute db.test }}
			}}
			{cascad "Database information" {} "" 1 {			
			 {command "Describe table" {} "db.describe" {} -command {execute db.describe }}
			 {command "List columns" {} "db.columns" {} -command {execute db.columns }}
			 {command "List drivers" {} "db.drivers" {} -command {execute db.drivers }}
			 {command "List tables" {} "db.tables" {} -command {execute db.tables }}
			}}
			{separator}
			{cascad "Query" {} "" 1 {			
			 {command "Query data (SQL select)" {} "db.select" {} -command {execute db.select }}
			 {command "Execute SQL statement" {} "db.execute" {} -command {execute db.execute }}
			}}
 } 
 "&Help" all options 1 {
 {command "GRASS help" {} "g.manual" {} -command { exec $env(GRASS_HTML_BROWSER) $env(GISBASE)/docs/html/index.html & } }
 {command "d.m &help" {} "d.m help" {} -command { exec $env(GRASS_HTML_BROWSER) $env(GISBASE)/docs/html/d.m.html & } }
 {command "About &GRASS" {} "About GRASS" {} -command { source $env(GISBASE)/etc/dm/grassabout.tcl} }
 {command "About &System" {} "About System" {} -command { exec $env(GRASS_WISH) $env(GISBASE)/etc/dm/tksys.tcl --tcltk } }
 }

 }]

