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
            		{command "ERDAS LAN" {} "i.in.erdas" {} -command { execute i.in.erdas }}
            		{command "ESRI Arc/Info ASCII grid" {} "r.in.arc" {} -command { execute r.in.arc }}
            		{command "GRIDATB.FOR map file (TOPMODEL)" {} "r.in.gridatb" {} -command { execute r.in.gridatb }}
            		{command "MAT-File (v.4) map file (Matlab or Octave)" {} "r.in.gridatb" {} -command { execute r.in.mat }}
            		{command "STRM hgt files" {} "r.in.strm" {} -command { execute r.in.strm }}
        	    }}
        	    {cascad "Vector map" {} "" 1 {			
            		{command "Various formats using OGR" {} "v.in.ogr" {} -command { execute v.in.ogr }}
            		{separator}
            		{command "ASCII points file or GRASS ASCII vector file" {} "v.in.ascii" {} -command { execute v.in.ascii }}
            		{command "Import/export old GRASS vector format" {} "v.convert" {} -command { execute v.convert }}
        		    {separator}
        	    	{command "ESRI e00 format" {} "v.in.e00" {} -command { execute v.in.v00 }}
        	    	{command "Garmin GPS Waypoints/Routes/Tracks" {} "v.in.garmin" {} -command { execute v.in.garmin }}
            		{command "MATLAB MapGen files" {} "v.in.mapgen.sh" {} -command { execute v.in.mapgen.sh }}
            		{separator}
            		{command "ASCII points file to GRASS sites format" {} "s.in.ascii" {} -command { execute s.in.ascii }}
        	    }}
        	    {cascad "Grid 3D" {} "" 1 {			
            		{command "ASCII 3D file" {} "r3.in.ascii" {} -command { execute r3.in.ascii }}
        	    	{command "Vis5D file" {} "r3.in.v5d" {} -command { execute r3.in.v5d }}
        	    }}
        	}}
        	{cascad "Export" {} "" 1 {			
        	    {cascad "Raster map"  {} "" 1 {			
            		{command "ASCII grid (for GRASS, Surfer, Modflow, etc)" {} "r.out.ascii" {} -command { execute r.out.ascii }}
            		{separator}
            		{command "ESRI ARC/INFO ASCII grid" {} "r.out.arc" {} -command { execute r.out.arc }}
        	    	{command "ERDAS LAN file" {} "i.out.erdas" {} -command { execute i.out.erdas }}
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
            		{command "GRASS vector file" {} "v.out.ascii" {} -command { execute v.out.ascii }}
            		{command "Import/export old GRASS vector format" {} "v.convert" {} -command { execute v.convert }}
            		{command "POV-Ray format" {} "v.out.pov" {} -command { execute v.out.pov }}
            		{separator}
            		{command "ASCII points file from GRASS sites file" {} "s.out.ascii" {} -command { execute s.out.ascii }}
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
                {command "PNG (save currently selected display to 24 bit PNG file)" {} "" {} -command { execute d.out.png }}
            }}
        	{command "Save map to Postscript file" {} "" {} -command { execute ps.map }}
        	{command "Print (requires ghostscript)" {} "" {}  -accelerator Ctrl-P -command {Dm::print} }
        	{separator}
            {command "E&xit" {} "Exit Display Manager" {} -accelerator Ctrl-Q -command { DmPrint::clean;  exit } }
        }
        "&GIS" all options 1 {
        	{cascad "Manage maps and grid3D files" {} "" 1 {			
        	    {command "Copy maps" {} "" {} -command {execute g.copy }}
        	    {command "List maps" {} "" {} -command {execute g.list }}
        	    {command "List maps using expressions and 'wildcards'" {} "" {} -command  {execute g.mlist }}
        	    {command "Rename maps" {} "" {} -command {execute g.rename }}
        	    {command "Remove maps" {} "" {} -command {execute g.remove }}
        	    {command "Remove maps using expressions and 'wildcards'" {} "" {} -command {execute g.mremove }}
        	    {separator}
        	    {command "List Grid3D files" {} "" {} -command {execute g3.list }}
        	    {command "Rename Grid3D volumes" {} "" {} -command {execute g3.rename }}
        	    {command "Remove Grid3D volumes" {} "" {} -command  {execute g3.remove }}
            }}
        	{cascad "Region" {} "" 1 {			
              	{command "Display region settings" {} "" {} -command {run g.region -p }}
        	    {command "Manage region" {} "" {} -command {execute g.region }}
        	    {command "Select default region" {} "" {} -command {run g.region -d ; run d.redraw }}
        	    {separator}
        	    {command "Create WIND3 (default 3D window) from current 2D region" {} "" {} -command {execute g3.createwind }}
        	    {command "Manage 3D region" {} "" {} -command {execute g3.setregion }}
        	}}
        	{cascad "GRASS working environment" {} "" 1 {			
        	    {command "Modify access by other users to current mapset" {} "" {} -command  {term g.access }}
        	    {command "Modify mapset search path" {} "" {} -command {spawn $env(GISBASE)/etc/g.mapsets.tcl}}
        	    {command "Change current working session to new mapset, location, or GISDBASE" {} "" {} -command {execute g.mapset }}
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
        	    {command "Show projection information and create projection files" {} "g.proj" {} -command  {execute g.proj }}
        	}}
        }            
        "&Display" all options 1 {
			{command "Start NVIZ (n-dimensional visualization module)" {} "nviz" {} -command {execute nviz }}
			{command "Create fly-through animation path for NVIZ" {} "" {} -command {execute d.nviz }}
        	{separator}
        	{cascad "Start displays" {} "" 1 [monitor_menu start]}
			{cascad "Select displays" {} "" 1 [monitor_menu select]}
			{cascad "Stop displays" {} "" 1 [monitor_menu stop]}
			{command "Start/restart display at specified window size" {} "" {} -command {execute d.monsize }}
			{command "Set active display to specified size" {} "" {} -command {execute d.resize }}
        	{separator}
	        {cascad "Display raster maps" {} "" 1 {
			    {command "Display raster map" {} "" {} -command {execute d.rast }}
			    {command "Display raster map, legend, and title in active display" {} "" {} -command {execute d.rast.leg }}
        	    {separator}
			    {command "Display HIS overlays" {} "" {} -command {execute d.his }}
			    {command "Display RGB overlays" {} "" {} -command {execute d.rgb }}
			    {command "Drape raster map over shaded relief or aspect map" {} "" {} -command {execute $env(GISBASE)/etc/dm/script/d.shadedmap }}
        	    {separator}
			    {command "Slide show of all raster maps in current mapset" {} "" {} -command {execute d.slide.show }}
	        }}
			{command "Display vector maps" {} "" {} -command {execute d.vect }}
			{cascad "Display text on maps" {} "" 1 {			
		    	{command "Display legend for raster maps" {} "" {} -command {execute d.legend }}
		    	{command "Display category values in raster map cells" {} "" {} -command {execute d.rast.num }}
    	        {separator}
		    	{command "Create map title file for use by d.text" {} "" {} -command {execute d.title }}
		    	{command "Display text labels for paint output" {} "" {} -command {execute d.paint.labels }}
    	        {separator}
		    	{command "Select text font" {} "" {} -command {execute d.font }}
    	        {separator}
 	    		{command "Draw text" {} "" {} -command {execute d.text }}
		    	{command "Draw text using TrueType fonts" {} "" {} -command {execute d.text.freetype }}
    	        {separator}
		    	{command "Display standard GRASS fonts" {} "" {} -command {execute show.fonts.sh }}
	        }}
			{cascad "Display graphics on maps" {} "" 1 {			
	    		{command "Overlay scale and north arrow" {} "" {} -command {execute d.barscale }}
                {separator}
	    		{command "Display graphs at vector point localities" {} "" {} -command {execute d.vect.chart }}
	    		{command "Display histogram" {} "" {} -command {execute d.histogram }}
	    		{command "Display line graph" {} "" {} -command {execute d.linegraph }}
        	    {separator}
	    		{command "Overlay grid" {} "" {} -command {execute d.grid }}
	    		{command "Display geodesic line" {} "" {} -command {execute d.geodesic }}
	    		{command "Display rhumbline" {} "" {} -command {execute d.rhumbline }}
        	    {separator}
	    		{command "Display color table" {} "" {} -command {execute d.colortable }}
        	    {separator}
	    		{command "Overlay slope arrows on aspect raster map" {} "" {} -command {execute d.rast.arrow }}
        	    {separator}
	    		{command "Draw simple graphics in active display monitor (display coordinates)" {} "" {} -command {execute d.graph }}
	    		{command "Draw simple graphics in active display monitor (map coordinates)" {} "" {} -command {execute d.mapgraph }}
        	}}
			{command "Split active display and show maps in each half" {} "" {} -command {execute d.split }}
        	{separator}
			{command "Manage displays" {} "" {} -command {execute d.mon }}
			{command "Manage display frames" {} "" {} -command {execute d.frame }}
			{command "Display information about active display monitor" {} "" {} -command {execute d.info }}
        	{separator}
			{command "Redraw active display (Note: some items may not be redrawn)" {} "" {} -command {execute d.redraw }}
			{command "Save file of commands to recreate active display" {} "" {} -command {execute d.save }}
        	{separator}
			{command "Measure lengths and areas" {} "" {} -command {term d.measure -s}}
			{command "Show geographical position" {} "" {} -command {execute d.where }}
        	{separator}
			{command "Zoom/Unzoom/Pan with options" {} "" {} -command {execute d.zoom }}	
           	{separator}
			{command "Erase active display/frame to selected color" {} "" {} -command {execute d.erase }}
        }
        "&Raster" all options 1 {
			{cascad "Develop map" {} "" 1 {			
			    {command "Digitize raster" {} "" {} -command {term r.digit }}
			    {separator}
			    {command "Compress/decompress raster file" {} "" {} -command {execute  r.compress }}
			    {command "Manage boundary definitions" {} "" {} -command {execute  r.region }}
			    {command "Manage null values" {} "" {} -command {execute  r.null }}
			    {command "Manage timestamps for files" {} "" {} -command {execute  r.timestamp }}
			    {command "Quantization for floating-point maps" {} "" {} -command {execute  r.quant }}
			    {command "Resample (change resolution) using nearest neighbor method" {} "" {} -command {execute  r.resample }}
			    {command "Resample (change resolution) using regularized spline tension" {} "" {} -command {execute  r.resamp.rst }}
			    {separator}
			    {command "Reproject raster from other location" {} "" {} -command {execute  r.proj }}
			}}
			{cascad "Manage map colors" {} "" 1 {			
			    {command "Modify color table" {} "" {} -command {execute  d.colors }}
			    {command "Set colors to predefined color tables" {} "" {} -command {execute  r.colors }}
			    {command "Set colors using color rules" {} "" {} -command {execute  $env(GISBASE)/etc/dm/script/r.colors.rules }}
			    {separator}
			    {command "Blend 2 color maps to produce 3 RGB files" {} "" {} -command {execute  r.blend }}
			    {command "Create color image from RGB files" {} "" {} -command {execute  r.composite }}
			    {command "Create 3 RGB (red, green, blue) maps from 3 HIS (hue, intensity, saturation) maps" {} "" {} -command {execute  r.his }}
			}}
			{separator}
			{command "Query by coordinate(s)" {} "" {} -command  { execute r.what }}
			{command "Query with mouse" {} "" {} -command  { execute d.what.rast }}
			{separator}
			{command "Create raster buffers" {} "" {} -command { execute r.buffer }}
			{command "Locate closest points between areas in 2 raster maps" {} "" {} -command  { execute r.distance }}
			{command "Map calculator" {} "" {} -command { execute r.mapcalculator }}
			{cascad "Neighborhood analysis" {} "" 1 {			
    			    {command "Moving window analysis of raster cells" {} "" {} -command  { execute r.neighbors }}
    			    {command "Analyze vector points in neighborhood of raster cells" {} "" {} -command  { execute v.neighbors }}
			}}
			{cascad "Overlay maps" {} "" 1 {			
			    {command "Cross product" {} "" {} -command {execute  r.cross }}
			    {command "Function of map series (time series)" {} "" {} -command {execute  r.series }}
			    {command "Patch maps" {} "" {} -command {execute  r.patch }}
			    {separator}
			    {command "Statistical calculations for cover map over base map" {} "" {} -command {execute  r.statistics }}
			}}
			{cascad "Solar radiance and shadows" {} "" 1 {			
			    {command "Solar irradiance and daily irradiation" {} "" {} -command {execute  r.sun }}
			    {command "Shadows map for sun position or date/time" {} "" {} -command {execute  r.sunmask }}
			}}
			{cascad "Terrain analysis" {} "" 1 {			
			    {command "Cost surface" {} "" {} -command {execute  r.cost }}
			    {command "Least cost route or flow" {} "" {} -command {execute  r.drain }}
			    {command "Profile analysis" {} "" {} -command {execute  d.profile }}
			    {command "Shaded relief map" {} "" {} -command {execute  r.shaded.relief }}
			    {command "Slope and aspect" {} "" {} -command {execute  r.slope.aspect }}
			    {command "Terrain parameters" {} "" {} -command {execute  r.param.scale }}
			    {command "Textural features" {} "" {} -command {execute  r.texture }}
			    {command "Visibility/line of sight" {} "" {} -command {execute  r.los }}
			}}
			{cascad "Transform features" {} "" 1 {			
			    {command "Clump small areas" {} "" {} -command {execute  r.clump }}
			    {command "Grow areas" {} "" {} -command {execute  r.grow }}
			    {command "Thin linear features" {} "" {} -command {execute  r.thin }}
			}}
			{separator}
			{cascad "Hydrologic modeling" {} "" 1 {			
			    {command "Depressionless elevation map and flowline map" {} "" {} -command {execute  r.fill.dir }}
			    {command "Flow accumulation for massive grids" {} "" {} -command {exec r.terraflow &}}
			    {command "Generate flow lines for raster map" {} "" {} -command {execute  r.flow }}
			    {command "Topographic index map" {} "" {} -command {execute  r.topidx }}
			    {command "TOPMODEL simulation" {} "" {} -command {execute  r.topmodel }}
			    {command "Watershed subbasins" {} "" {} -command {execute  r.basins.fill }}
			    {command "Watershed basin analysis" {} "" {} -command {execute  r.watershed }}
			    {command "Watershed basin creation" {} "" {} -command {execute  r.water.outlet }}
			}}
			{cascad "Landscape structure modeling" {} "" 1 {			
			    {command "Set up sampling and analysis framework" {} "" {} -command {term r.le.setup }}
			    {separator}
			    {command "Analyze landscape characteristics" {} "" {} -command {execute  r.le.pixel }}
			    {command "Analyze landscape patch characteristics" {} "" {} -command {execute  r.le.patch }}
			    {command "Output landscape patch information" {} "" {} -command {execute  r.le.trace }}
			}}
			{cascad "Wildfire modeling" {} "" 1 {			
			    {command "Generate rate of spread (ROS) maps" {} "" {} -command {execute  r.ros }}
			    {command "Generate least-cost spread paths" {} "" {} -command {execute  r.spreadpath }}
			    {command "Simulate anisotropic spread phenomena" {} "" {} -command {execute  r.spread }}
			}}
			{separator}
			{cascad "Change category values and labels" {} "" 1 {			
			    {command "Edit category values of individual cells for displayed raster map" {} "" {} -command {term  d.rast.edit }}
			    {separator}
			    {command "Reclassify categories for areas of specified sizes" {} "" {} -command {execute  r.reclass.area }}
			    {command "Reclassify categories using rules" {} "" {} -command {execute  $env(GISBASE)/etc/dm/script/r.reclass.rules }}
			    {command "Reclassify categories using rules file" {} "" {} -command {execute  r.reclass }}
			    {separator}
			    {command "Recode categories using rules (create new map)" {} "" {} -command {execute  $env(GISBASE)/etc/dm/script/r.recode.rules }}
			    {command "Recode categories using rules file (create new map)" {} "" {} -command {execute  r.recode }}
			    {separator}
			    {command "Rescale categories (create new map)" {} "" {} -command {execute  r.rescale }}
			    {command "Rescale categories with equalized histogram (create new map)" {} "" {} -command {execute  r.rescale.eq }}
			}}
			{separator}
			{command "Generate concentric circles around points" {} ""  {} -command { execute r.circle }}
			{cascad "Generate random raster cells" {} "" 1 {			
			    {command "Generate random cells" {} "" {} -command {execute  r.random.cells }}
			    {command "Generate random cells and vector points from raster map" {} "" {} -command {execute  r.random }}
			}}
			{cascad "Generate surfaces" {} "" 1 {			
			    {command "Generate density surface using moving Gausian kernal" {} "" {} -command {execute  v.kernel }}
			    {command "Generate fractal surface" {} "" {} -command {execute  r.surf.fractal }}
			    {command "Generate gaussian deviates surface" {} "" {} -command {execute  r.surf.gauss }}
			    {command "Generate plane" {} "" {} -command {execute  r.plane }}
			    {command "Generate random deviates surface" {} "" {} -command {execute  r.surf.random }}
			    {command "Generate random surface with spatial dependence" {} "" {} -command {execute  r.random.surface }}
			}}
			{command "Generate vector contour lines" {} "" {} -command { execute r.contour }}
			{cascad "Interpolate surfaces" {} "" 1 {			
						{command "Bilinear interpolation from raster points" {} "" {} -command { execute r.bilinear }}
						{command "Inverse distance weighted interpolation from raster points" {} "" {} -command { execute r.surf.idw }}
						{command "Interpolation from raster contours" {} "" {} -command { execute r.surf.contour }}
			            {separator}
						{command "Inverse distance weighted interpolation from vector points" {} "" {} -command { execute v.surf.idw }}
						{command "Regularized spline tension interpolation from vector points or contours" {} "" {} -command { run v.surf.rst}}
			            {separator}
			            {command "Fill NULL cells by interpolation using regularized spline tension" {} "" {} -command {execute  r.fillnulls }}
			}}
			{separator}
			{cascad "Reports and statistics" {} "" 1 {			
			    {command "Report basic file information" {} "" {} -command {execute  r.info }}
			    {command "Report category labels and values" {} "" {} -command {execute  r.cats }}
			    {separator}
			    {command "General statistics" {} "" {} -command {execute  r.stats }}
			    {command "Range of all category values" {} "" {} -command {execute  r.describe }}
			    {command "Sum all cell category values" {} "" {} -command {execute  r.sum }}
			    {command "Sum area by map and category" {} "" {} -command {execute  r.report }}
			    {command "Total surface area, considering toppography" {} "" {} -command {execute  r.surf.area }}
			    {command "Univariate statistics" {} "" {} -command {execute  r.univar }}
			    {command "Univariate statistics (script version)" {} "" {} -command {execute  r.univar.sh }}
			    {separator}
			    {command "Sample values along transects" {} "" {} -command {execute  r.profile }}
			    {command "Sample values along transects (use azimuth, distance)" {} "" {} -command {execute  r.transect }}
			    {separator}
			    {command "Covariance/correlation" {} "" {} -command {execute  r.covar }}
			    {command "Linear regression between 2 maps" {} "" {} -command {execute  r.regression.line }}
			    {command "Mutual category occurences (coincidence)" {} "" {} -command {execute  r.coin }}
			}}
        }            
        "&Vector" all options 1 {
			{cascad "Develop map" {} "" 1 {			
			    {command "Digitize" {} "" {} -command {execute  v.digit }}
			    {separator}
			    {command "Create/rebuild topology" {} "" {} -command {execute  v.build }}
			    {command "Clean vector files" {} "" {} -command {execute  v.clean }}
			    {separator}
			    {command "Break lines at intersections" {} "" {} -command {execute  v.topo.check }}
			    {command "Build polylines from adjacent segments" {} "" {} -command {execute  v.build.polylines }}
			    {command "Split polylines into segments" {} "" {} -command {execute  v.segment }}
			    {separator}
			    {command "Convert vector feature types" {} "" {} -command {execute  v.type }}
			    {separator}
			    {command "Create text label file for vector features" {} "" {} -command {execute  v.label }}
			    {separator}
			    {command "Reproject vector from other location" {} "" {} -command {execute  v.proj }}
			}}
			{cascad "Vector<->database connections" {} "" 1 {			
			    {command "Create new vector as link to external OGR layer" {} "" {} -command {execute  v.external }}
			    {command "Set database connection for vector attributes" {} "" {} -command {execute  v.db.connect }}
			}}
			{command "Rectify and georeference vector map" {} "" {} -command {execute  v.transform }}
			{separator}
			{command "Query by attributes" {} "" {} -command {execute  v.extract }}
			{command "Query by map features" {} "" {} -command {execute  v.select }}
			{command "Query with mouse (form mode, editing enabled)" {} "" {} -command {spawn d.what.vect -ef}}
			{separator}
			{command "Buffer features" {} "" {} -command {execute  v.buffer }}
			{command "Locate nearest features to points or centroids" {} "" {} -command {execute  v.distance }}
			{cascad "Network analysis" {} "" 1 {			
			    {command "Allocate subnets" {} "" {} -command {execute  v.net.alloc }}
			    {command "Network maintenance" {} "" {} -command {execute  v.net }}
			    {command "Shortest route" {} "" {} -command {execute  v.net.path }}
			    {command "Shortest route (visualization only)" {} "" {} -command {execute  d.path }}
			    {command "Split net to bands between cost isolines" {} "" {} -command {execute  v.net.iso }}
			    {command "Steiner tree" {} "" {} -command {execute  v.net.steiner }}
			    {command "Traveling salesman analysis" {} "" {} -command {execute  v.net.salesman }}
			}}
			{cascad "Overlay maps" {} "" 1 {			
			    {command "Overlay/combine 2 vector maps" {} "" {} -command {execute  v.overlay }}
			    {command "Patch multiple maps (combine)" {} "" {} -command {execute  v.patch }}
			}}
			{command "Generate area feature for extent of current region" {} "" {} -command {execute  v.in.region }}
			{command "Generate rectangular vector grid" {} "" {} -command {execute  v.mkgrid }}
			{separator}
			{cascad "Change attributes" {} "" 1 {			
			    {command "Attach, delete, or report categories" {} "" {} -command {execute  v.category }}
			    {command "Reclassify features using rules file" {} "" {} -command {execute  v.reclass }}
			}}
			{separator}
			{cascad "Work with vector points" {} "" 1 {			
    			{cascad "Generate points" {} "" 1 {			
			        {command "Generate points from database with x/y coordinates" {} "" {} -command {execute  v.in.db }}
    			    {command "Generate random points" {} "" {} -command {execute  v.random }}
    			    {command "Random location perturbations of points" {} "" {} -command {execute  v.perturb }}
    			}}
    			{cascad "Generate areas from points" {} "" 1 {			
    			    {command "Generate convex hull for point set" {} "" {} -command {execute  v.hull }}
    			    {command "Generate Delaunay triangles for point set" {} "" {} -command {execute  v.delaunay }}
    			    {command "Generate Voronoi diagram/Thiessen polygons for point set" {} "" {} -command {execute  v.voronoi }}
    			}}
    			{cascad "Sample raster maps" {} "" 1 {			
        			{command "Sample raster map at point locations" {} "" {} -command {execute  v.what.rast }}
        			{command "Sample raster neighborhood around points" {} "" {} -command {execute  v.sample }}
    			}}
    			{command "Partition points into test/training sets for k-fold cross validatation" {} "" {} -command {execute  v.kcv }}
			}}
			{separator}
			{cascad "Reports and statistics" {} "" 1 {			
			    {command "Basic information" {} "" {} -command {execute  v.info }}
			    {command "Load vector attributes to database or create reports" {} "" {} -command {execute  v.to.db }}
			    {command "Univariate statistics" {} "" {} -command {execute  v.univar }}
    			{separator}
			    {command "Test normality of point distribution" {} "" {} -command {execute  v.normal }}
			    {command "Indices of point counts in quadrats" {} "" {} -command {execute  v.qcount }}
			}}
        }            
        "&Image" all options 1 {			
			{cascad "Develop images and groups" {} "" 1 {			
			    {command "Create/edit imagery group" {} "" {} -command {execute  i.group }}			
			    {command "Target imagery group" {} "" {} -command {execute  i.target }}
			    {separator}
			    {command "Mosaic up to 4 adjacent images" {} "" {} -command {execute  i.image.mosaic }}
			}}
			{cascad "Manage image colors" {} "" 1 {			
			    {command "Transform HIS (Hue/Intensity/Saturation) color image to RGB (Red/Green/Blue)" {} "" {} -command {execute  i.his.rgb }}
			    {command "Transform RGB (Red/Green/Blue) color image to HIS (Hue/Intensity/Saturation)" {} "" {} -command {execute  i.rgb.his }}
			}}
			{cascad "Rectify and georeference image group" {} "" 1 {			
			    {command "Set ground control points (GCP's) from raster map or keyboard entry" {} "" {} -command {term i.points}}
			    {command "Set ground control points (GCP's) from vector map or keyboard entry" {} "" {} -command {term i.vpoints}}
			    {command "Affine and Polynomial rectification (rubber sheet)" {} "" {} -command {execute  i.rectify }}
			    {command "Ortho photo rectification" {} "" {} -command {term  i.ortho.photo }}
			}}
			{separator}
			{command "Brovey transformation and pan sharpening for Landsat ETM, SPOT, and Quickbird" {} "" {} -command {execute  i.fusion.brovey }}
			{cascad "Classify image" {} "" 1 {			
			    {command "Clustering input for unsupervised classification" {} "" {} -command {execute  i.cluster }}
			    {separator}
			    {command "Maximum likelyhood classification (MLC)" {} "" {} -command {execute  i.maxlik }}
			    {command "Sequential maximum a posteriory classification (SMAP)" {} "" {} -command {execute  i.smap }}
			    {separator}
			    {command "Interactive input for supervised classification" {} "" {} -command {term  i.class }}
			    {command "Non-interactive input for supervised classification (MLC)" {} "" {} -command {execute  i.gensig }}
			    {command "Non-interactive input for supervised classification (SMAP)" {} "" {} -command {execute  i.gensigset }}
			}}
			{command "Dehaze for LandSAT 5" {} "" {} -command {execute  i.tm.dehaze }}
			{cascad "Filter image" {} "" 1 {			
			    {command "Zero edge crossing detection" {} "" {} -command {execute  i.zc }}
			    {command "User defined matrix/convolving filter" {} "" {} -command {execute  r.mfilter }}
			}}
			{command "Spectral response" {} "" {} -command {execute  i.spectral }}
			{command "Tassled cap vegetation index" {} "" {} -command {execute  i.tasscap }}
			{cascad "Transform image" {} "" 1 {			
			    {command "Canonical component" {} "" {} -command {execute  i.cca }}
			    {command "Principal component" {} "" {} -command {execute  i.pca }}
			    {command "Fast Fourier Transform" {} "" {} -command {execute  i.fft }}
			    {command "Inverse Fast Fourier Transform" {} "" {} -command {execute  i.ifft }}
			}}
			{separator}
			{cascad "Reports and statistics" {} "" 1 {			
			    {command "Report basic file information" {} "" {} -command {execute  r.info }}
			    {command "Range of image values" {} "" {} -command {execute  r.describe }}
			    {separator}
			    {command "Kappa classification aaccuracy assessment" {} "" {} -command {execute  r.kappa }}
			    {command "Optimum index factor for LandSat TM" {} "" {} -command {execute  i.oif }}
			}}
        }            
        "&Grid3D" all options 1 {
			{cascad "Develop grid3D volumes" {} "" 1 {			
			    {command "Manage nulls for grid3D volume" {} "" {} -command {execute  r3.null }}
			    {command "Manage timestamp for grid3D volume" {} "" {} -command {execute  r3.timestamp }}
			}}
			{command "Create 3D mask for grid3D operations" {} "" {} -command {execute  r3.mask }}
			{command "Create display file for grid3D volume" {} "" {} -command { execute r3.mkdspf }}
			{command "Map calculator for grid3D operations" {} "" {} -command {execute  r3.mapcalculator }}
			{command "Interpolate volume from vector points using splines" {} "" {} -command {execute  v.vol.rst }}
			{cascad "Report and Statistics" {} "" 1 {			
			    {command "Display information about grid3D volume" {} "" {} -command {execute  r3.info }}
			}}
        }            
        "&Databases" all options 1 {
			{cascad "Manage database" {} "" 1 {			
			    {command "Connect to database" {} "" {} -command {execute  db.connect }}
			    {command "Create empty database" {} "" {} -command {execute  db.createdb }}
			    {command "PERMANTLY remove table" {} "" {} -command {execute  db.droptable }}
			    {command "Copy table" {} "" {} -command {execute  db.copy }}
			}}
			{cascad "Database information" {} "" 1 {			
			    {command "Describe table" {} "" {} -command {execute  db.describe }}
			    {command "List columns" {} "" {} -command {execute  db.columns }}
			    {command "List databases" {} "" {} -command {execute  db.databases }}
			    {command "List drivers" {} "" {} -command {execute  db.drivers }}
			    {command "List tables" {} "" {} -command {execute  db.tables }}
			}}
			{separator}
			{cascad "Query" {} "" 1 {			
			    {command "Query data (SQL select)" {} "" {} -command {execute  db.select }}
			    {command "Execute SQL statement" {} "" {} -command {execute  db.execute }}
			}}
        }            
        "&Help" all options 1 {
            {command "GRASS help" {} "g.manual" {} -command { exec $env(GRASS_HTML_BROWSER) $env(GISBASE)/docs/html/index.html & } }
            {command "d.m &help" {} "d.m help" {} -command { exec $env(GRASS_HTML_BROWSER) $env(GISBASE)/docs/html/d.m.html & } }
            {command "About &GRASS" {} "About GRASS" {} -command { source $env(GISBASE)/etc/dm/grassabout.tcl} }
            {command "About &System" {} "About System" {} -command { exec $env(GRASS_WISH) $env(GISBASE)/etc/dm/tksys.tcl --tcltk } }
        }

    }]

