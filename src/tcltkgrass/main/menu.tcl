frame .main_menu
pack .main_menu -expand yes -fill both

# tcltkgrass menu.tcl v 4.0 for GRASS5 2004/03/25 Michael Barton 
# based on menu.tcl for GRASS5 by Jacques Bouchard and Markus Neteler
# with scripting support by Andreas Lang


# main menu

menu_build 1 .main_menu {
    File "Files in/out" {
		"Import" "Import maps into GRASS" {
			"Raster map" "" {
           	 	"Multiple formats using GDAL" "" {
           		     "source $env(TCLTKGRASSBASE)/module/r.in.gdal"
           		}
				-separator
				"GRASS ASCII GRID" "" {
				"source $env(TCLTKGRASSBASE)/module/r.in.ascii"
				}
				"GRASS ASCII vector map" "" {
				"source $env(TCLTKGRASSBASE)/module/r.in.poly"
				}
				-separator
            		"ERDAS LAN" "" {
					"source $env(TCLTKGRASSBASE)/module/i.in.erdas"
            		}
				"ESRI formats" "" {
	     	       	"ESRI ARC/INFO ASCII-GRID" "" {
     	     	  	    "source $env(TCLTKGRASSBASE)/module/r.in.arc"
          	  		}
					"ESRI e00 export file" "" {
						"source $env(TCLTKGRASSBASE)/module/m.in.e00"
					}
					"ESRI Shapefile" "" {
						"source $env(TCLTKGRASSBASE)/module/r.in.shape"
					}
				}
				"GRIDATB.FOR map file (TOPMODEL)" "" {
				"source $env(TCLTKGRASSBASE)/module/r.in.gridatb"
				}
				"Tangent raster data file" "" {
				"source $env(TCLTKGRASSBASE)/module/r.in.tang"
				}
				-separator
				"ELAS file" "" {
				"source $env(TCLTKGRASSBASE)/module/r.in.elas"
				}
				"HDF file" "" {
				"source $env(TCLTKGRASSBASE)/module/r.in.hdf"
				}
				"LandSat tape formats" "" {
					"LandSat MSS files from tape format" "" {
					"run i.tape.mss &"
					}
					"LandSat MSS header file from tape format" "" {
					"run i.tape.mss.h &"
					}
					"LandSat TM files from tape format" "" {
					"run i.tape.tm &"
					}
					"LandSat TM files from tape format (fast method)" "" {
					"run i.tape.tm.fast &"
					}
				}
				"MAT-File, ver. 4 (from MatLab or Octave)" "" {
				"source $env(TCLTKGRASSBASE)/module/r.in.mat"
				}
				"MIADS ASCII file" "" {
				"source $env(TCLTKGRASSBASE)/module/r.in.miads"
				}
				"MIADS ASCII file - replace '99' (no data) values with spaces after import" "" {
				"source $env(TCLTKGRASSBASE)/module/m.strip99s"
				}
				"Other image files from tape format" "" {
				"run i.tape.other &"
				}
           		-separator
				"Binary file (GTOPO30 format)" "" {
           		     "source $env(TCLTKGRASSBASE)/module/r.in.bin"
				}
				"Lat/long binary file (GTOPO30 format) into a UTM region" "" {
           		     "source $env(TCLTKGRASSBASE)/module/r.in.ll"
           		}
				"UTM raster map" "" {
				"source $env(TCLTKGRASSBASE)/module/r.in.utm"
				}
    				-separator
				"DEM" "" {
					"DEM examination" "" {
	    					"source $env(TCLTKGRASSBASE)/module/m.dem.examine"
					}
					"DEM extraction" "" {
	    					"source $env(TCLTKGRASSBASE)/module/m.dem.extract"
					}
				}
	            	"Digital orthophoto" "" {
     	       	    "source $env(TCLTKGRASSBASE)/module/r.in.doq"
          	  	}
				"DTED" "" {
					"DTED examination" "" {
	    					"source $env(TCLTKGRASSBASE)/module/m.dted.examine"
					}
					"DTED extraction" "" {
	    					"source $env(TCLTKGRASSBASE)/module/m.dted.extract"
					}
					"DTED I & II into lat/long location" "" {
	    					"source $env(TCLTKGRASSBASE)/module/r.in.dted"
					}
    				}
	            	"Examine tape file" "" {
     	       	    "source $env(TCLTKGRASSBASE)/module/m.examine.tape"
          	  	}
				-separator
				"Graphic image formats" "" {
		            	"PBM image" "" {
     		       	    "source $env(TCLTKGRASSBASE)/module/r.in.pbm"
          		  	}
	          	  	"PGM image" "" {
     	       		    "source $env(TCLTKGRASSBASE)/module/r.in.pgm"
	          	  	}
     	       		"PNG image (24bit)" "" {
					"source $env(TCLTKGRASSBASE)/module/r.in.png"
					}
     	      		"PPM image (24bit)" "" {
						"source $env(TCLTKGRASSBASE)/module/r.in.ppm"
					}
            			"Sun raster image" "" {
						"run r.in.sunrast &"
					}
	     	       	"TIFF image (8/24bit)" "" {
     	     	  	    "source $env(TCLTKGRASSBASE)/module/r.in.tiff"
          	  		}
				}
			}
			"Vector map" "" {
      	      	"ASCII DLG, ASCII vector, or binary vector files" "" {
       	         		"run v.import &"
       	     	}
				"ASCII GRASS vector file" "" {
					"source $env(TCLTKGRASSBASE)/module/v.in.ascii"
				}
				-separator
				"ARC/INFO ungenerate file" "" {
			  		"source $env(TCLTKGRASSBASE)/module/v.in.arc"
    	        		}
   	         		"Atlas Graphics map file" "" {
    	           		"source $env(TCLTKGRASSBASE)/module/v.in.atlas"
     	       	}
				"ESRI e00 export file" "" {
					"source $env(TCLTKGRASSBASE)/module/m.in.e00"
				}
   	         		"ESRI shapefile" "" {
    	           		"source $env(TCLTKGRASSBASE)/module/v.in.shape"
     	       	}
   	         		"MATLAB MapGen files" "" {
    	           		"source $env(TCLTKGRASSBASE)/module/v.in.mapgen.sh"
     	       	}
				-separator
     	       	"Census STF1 or PL94-171 files" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.apply.census"
     	       	}
     	       	"GSHHS shoreline data file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.in.gshhs"
     	       	}
     	       	"Tiger file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.in.tig.basic"
     	       	}
     	       	"Tiger file of census 'landmark' features" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.in.tig.lndmk"
     	       	}
   	         		"USGS DLG-3 optional format (method 1)" "" {
    	           		"source $env(TCLTKGRASSBASE)/module/v.in.dlg"
     	       	}
   	         		"USGS DLG-3 optional format (method 2)" "" {
    	           		"source $env(TCLTKGRASSBASE)/module/v.in.dlg2"
     	       	}
     	       	"USGS STDS vector data file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.in.stds"
     	       	}
     	       	"USGS STDS or ISO 8211 (FIPS 123) file data dump" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/m.stds.read"
     	       	}
				-separator
     	       	"AutoCAD DXF file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.in.dxf"
     	       	}
     	       	"AutoCAD DXF file of contour lines with z value" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.in.dxf3d.sh"
     	       	}
	            	"AutoCAD DXF labels - attach to vectors imported from DXF" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.cadlabel"
          	  	}
				-separator
     	       	"XFig FIG 3.2 format file (to ASCII vector file)" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/fig2grass"
     	       	}
				-separator
		    		"Garmin GPS Waypoints/Routes/Tracks" "" {
					"source $env(TCLTKGRASSBASE)/module/v.in.garmin"
	   	 		}
       	 	}	
       	 	"Sites list" "" {
        	    		"ATKIS-DGM elevation file" "" {
         	       		"source $env(TCLTKGRASSBASE)/module/s.in.atkisdgm"
         	   		}
        	    		"ATKIS-KTB elevation file" "" {
         	       		"source $env(TCLTKGRASSBASE)/module/s.in.atkisktb"
         	   		}
        	    		"ASCII file/spot heights" "" {
         	       		"source $env(TCLTKGRASSBASE)/module/s.in.ascii"
         	   		}
     	       	"DBF file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/s.in.dbf"
     	       	}
     	       	"ESRI points shapefile" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/s.in.shape"
     	       	}
	 	   		"Garmin GPS Waypoints/Routes/Tracks" "" {
					"source $env(TCLTKGRASSBASE)/module/s.in.garmin"
	  	  		}
       	 	}
      	  	"Grid3D file" "" {
       	     	"ASCII file" "" {
       	    		     "source $env(TCLTKGRASSBASE)/module/r3.in.ascii"
       	    		}
      	      	"VIS5D v5d file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/r3.in.v5d"
      	      	}
       	 	}
    		}
		Export "Export maps from GRASS" {
			"Raster map" "" {
        			"GRASS ASCII" "" {
					"source $env(TCLTKGRASSBASE)/module/r.out.ascii"
				}
        			"XYZ ASCII file" "" {
					"source $env(TCLTKGRASSBASE)/module/r.out.xyz"
				}
				-separator
         	   		"ESRI ARC/INFO ASCII-GRID" "" {
         	       		"source $env(TCLTKGRASSBASE)/module/r.out.arc"
         	   		}
				"ERDAS/LAN" "" {
         	       		"source $env(TCLTKGRASSBASE)/module/i.out.erdas"
          	  	}
				"GRIDATB.FOR map file (TOPMODEL)" "" {
				"source $env(TCLTKGRASSBASE)/module/r.out.gridatb"
				}
				-separator
				"ELAS file" "" {
				"source $env(TCLTKGRASSBASE)/module/r.out.elas"
				}
         	   		"HDF" "" {
         	     	  	"source $env(TCLTKGRASSBASE)/module/r.out.hdf"
         	   		}
				"MAT-File, ver. 4 (for use by MatLab or Octave)" "" {
				    "source $env(TCLTKGRASSBASE)/module/r.out.mat"
				}
				-separator
      	      	"BIL file with header (to $GISBASE/bil directory)" "" {
       	         		"run r.out.bil &"
       	     	}
      	      	"Binary file" "" {
       	         		"source $env(TCLTKGRASSBASE)/module/r.out.bin"
       	     	}
				-separator
      	      	"MPEG-1 animations" "" {
      	          	"source $env(TCLTKGRASSBASE)/module/r.out.mpeg"
      	      	}
      	      	"PNG image (not georeferenced)" "" {
      	          	"source $env(TCLTKGRASSBASE)/module/r.out.png"
      	      	}
         	   		"PPM image (24bit)" "" {
         	       		"source $env(TCLTKGRASSBASE)/module/r.out.ppm"
         	   		}
         	   		"PPM image from red, green, blue raster maps" "" {
         	       		"source $env(TCLTKGRASSBASE)/module/r.out.ppm3"
         	   		}
      	      	"POVray height-field" "" {
      	          	"source $env(TCLTKGRASSBASE)/module/r.out.pov"
      	      	}
      	      	"RLC encoded image" "" {
      	          	"source $env(TCLTKGRASSBASE)/module/r.out.rlc"
      	      	}
      	      	"Targa TGA image (24bit)" "" {
      	          	"source $env(TCLTKGRASSBASE)/module/r.out.tga"
      	      	}
      	      	"TIFF image (8/24bit)" "" {
      	          	"source $env(TCLTKGRASSBASE)/module/r.out.tiff"
      	      	}
          	  	"GRASS CELL file as TIFF image" "" {
          	      	"source $env(TCLTKGRASSBASE)/module/cell.out.tiff"
          	  	}
     	   	}
      	  	"Vector map" "" {
       	     	"ASCII GRASS vector file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.out.ascii"
     	       	}
       	     	"ASCII format files (DLG, digit, SCS-GEF, Arc/Info, DXF)" "" {
       	         		"run v.export &"
        	    		}
				-separator
      	      	"Arc/Info e00 file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.out.e00"
      	      	}
      	      	"Arc/Info ungenerate file" "" {
       	         		"source $env(TCLTKGRASSBASE)/module/v.out.arc"
       	     	}
     	       	"ESRI shapefile" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.out.shape"
     	       	}
     	       	"Atlas Graphics map file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.out.atlas"
     	       	}				
     	       	"Idrisi GIS vector file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.out.idrisi"
     	       	}
     	       	"Mapinfo MIF file" "" {
     	           	"run v.out.mapinfo &"
     	       	}
     	       	"MOSS vector file" "" {
     	           	"run v.out.moss &"
     	       	}
				-separator
     	       	"USGS DLG file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.out.dlg"
     	       	}
     	       	"USGS STDS file" "" {
	      	      	"Create STDS file" "" {
     	           		"source $env(TCLTKGRASSBASE)/module/v.out.stds"
      	      		}
	      	      	"Create STDS data quality reports for use in v.out.stds" "" {
     	           		"source $env(TCLTKGRASSBASE)/module/v.stds.dq.cp"
     	       		}
					"Create STDS Metadata for use in v.out.stds" "" {
     	           		"source $env(TCLTKGRASSBASE)/module/v.stds.meta.cp"
     	       		}
					"Create STDS Metadata interactively" "" {
     	           		"source $env(TCLTKGRASSBASE)/module/v.stds.meta.cp"
     	       		}
				}
				-separator
     	       	"AutoCAD DXF file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.out.dxf"
     	       	}
				-separator
     	       	"XFig FIG 3.2 format file (ASCII vector file)" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/grass2fig"
     	       	}
     	       	"XFig FIG 3.2 format file (binary vector map)" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/v.out.xfig"
     	       	}
       	 	}
      	  	"Sites list" "" {
       	     	"ASCII file" "" {
       	    		     "source $env(TCLTKGRASSBASE)/module/s.out.ascii"
       	    		}
      	      	"Arc/Info e00 file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/s.out.e00"
      	      	}
       	 	}
      	  	"Grid3D file" "" {
       	     	"ASCII file" "" {
       	    		     "source $env(TCLTKGRASSBASE)/module/r3.out.ascii"
       	    		}
      	      	"VIS5D v5d file" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/r3.out.v5d"
      	      	}
       	 	}
   	 	}
		Scripting "" {
	    		"Start scripting" "" {
				"script_start"
	    		}
	    		"Stop scripting"  "" {
				"script_stop"
	    		}
	    		"Play script"     "" {
				"script_play"
	    		}
		}
		"Save display to image file" "" {
			"XWD (Save display, selected with mouse, to xwd format file )" ""  {
                	"source $env(TCLTKGRASSBASE)/module/d.out.xwd"
			}
			"PNG (save currently selected display to 24 bit PNG file)" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.out.png"
			}
			"TIFF (save currently selected display to 8 bit TIFF file)" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.out.tiff"
			}
		}
		"Print driver output" "" {
        		"Paint driver" "" {
            		"Create/edit icon" "" {
                		"run p.icons &"
            		}
            		"Create/edit label" "" {
                		"run p.labels &"
            		}
            		"Display label" "" {
                		"source $env(TCLTKGRASSBASE)/module/d.paint.labels"
            		}
            		"Printer select" "" {
                		"source $env(TCLTKGRASSBASE)/module/p.select"
            		}
            		"Generate color table for paint driver" "" {
                		"source $env(TCLTKGRASSBASE)/module/r.colors.paint"
            		}
            		"Paint map creation" "" {
                		"source $env(TCLTKGRASSBASE)/module/p.map.new"
            		}
        		}
        		"Postscript driver" "" {
            		"Create/edit postscript icon" "" {
                 		"source $env(TCLTKGRASSBASE)/module/ps.icon"
             		}
             		"Printer select" "" {
                 		"source $env(TCLTKGRASSBASE)/module/ps.select"
             		}
             		"Postscript map creation" "" {
                 		"source $env(TCLTKGRASSBASE)/module/ps.map"
             		}
        		}
		}
		"Print (not yet implemented)" "" {"do_nothing"}
		"Quit" "Bye" resize_menu;quit
	}
	GIS "Manage GRASS GIS files" {
		"Maps & grid3D files" "Map management (map files operations)" {
			"Copy maps" "" {
				"source $env(TCLTKGRASSBASE)/module/g.copy"
			}
			"List maps" "" {
				"source $env(TCLTKGRASSBASE)/module/g.list"
  	     	 }
			"List maps using expressions & 'wildcards'" "" {
				"source $env(TCLTKGRASSBASE)/module/g.mlist"
  	     	 }
			"Rename maps" "" {
				"source $env(TCLTKGRASSBASE)/module/g.rename"
			}
			"Remove maps" "" {
            		"source $env(TCLTKGRASSBASE)/module/g.remove"
        		}
			"Remove maps using expressions & 'wildcards'" "" {
            		"source $env(TCLTKGRASSBASE)/module/g.mremove"
        		}
        		-separator
        		"Mapset access" "" {
            		"run g.access"
        		}
        		"Mapset search path" "" {
            		"source $env(TCLTKGRASSBASE)/module/g.mapsets"
        		}
        		"Mapset remove" "" {
            		"source $env(TCLTKGRASSBASE)/module/mapset.remove"
        		}
    		}
    		Region "Region management" {
        		"Display region settings" "" {
            		"run g.region -p &"
        		}
        		"Extend region to maximum extents of displayed maps" "" {
            		"source $env(TCLTKGRASSBASE)/module/d.extend"
        		}
         		"Manage region" "" {
            		"source $env(TCLTKGRASSBASE)/module/g.region.sh"
        		}
         		"Select default region" "" {
            		"exec g.region -d; exec d.erase"
        		}
         		"Show current projection information" "" {
            		"run g.projinfo &"
        		}
   		}
    		"3D region" "Grid3D region management" {
        		"Create WIND3 (default 3D window) from current 2D region" "" {
            		"source $env(TCLTKGRASSBASE)/module/g3.createwind"
        		}
         		"Manage 3D region" "" {
            		"source $env(TCLTKGRASSBASE)/module/g3.setregion"
        		}
         		"Manage 3D region interactively" "" {
            		"run g3.region &"
        		}
   		}
		"Map type conversions" "raster<->vector<->sites<->grid3D" {
			"Grid3D to sites" "" {
               		"source $env(TCLTKGRASSBASE)/module/r3.to.sites"
            	}
			-separator
			"Raster to sites" "" {
               		"source $env(TCLTKGRASSBASE)/module/r.to.sites"
            	}
        		"Raster to vector map" "" {
            		"Lines from thinned raster" "" {
					"source $env(TCLTKGRASSBASE)/module/r.line"
            		}
            		"Area edges" "" {
                		"source $env(TCLTKGRASSBASE)/module/r.poly"
            		}
            		"Create contour map" "" {
                		"source $env(TCLTKGRASSBASE)/module/r.contour"
            		}
			}
			-separator
			"Vector to raster" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.to.rast"
			}
			"Vector to sites" "" {
				"source $env(TCLTKGRASSBASE)/module/v.to.sites"
			}
			-separator
			"Sites to grid3D" "" {
				"source $env(TCLTKGRASSBASE)/module/s.to.rast3"
 			}
			"Sites to raster" "" {
				"source $env(TCLTKGRASSBASE)/module/s.to.rast"
 			}
			"Sites to vector" "" {
				"source $env(TCLTKGRASSBASE)/module/s.to.vect"
			}
		}
    		"Coordinate Conversions" "" {
			"Datum Shift" "" {
	  	 	 	"source $env(TCLTKGRASSBASE)/module/m.datum.shift"
			}
			-separator
			"Projection conversion for coordinates (interactive)" "" {
	    			"run m.proj &"
			}
			"Projection conversion for coordinates" "" {
	  	 	 	"source $env(TCLTKGRASSBASE)/module/m.proj2"
			}
			-separator
			"geocentric to lat/lon" "" {
	   	 		"source $env(TCLTKGRASSBASE)/module/m.gc2ll"
			}
			"lat/lon to geocentric" "" {
	  		 	"source $env(TCLTKGRASSBASE)/module/m.ll2gc"
			}
			-separator
			"UTM to lat/lon" "" {
	   		 	"source $env(TCLTKGRASSBASE)/module/m.u2ll"
			}
			"lat/lon to UTM" "" {
	    			"source $env(TCLTKGRASSBASE)/module/m.ll2u"
			}
    		}
		"Other" "" {
			"ASCII file utility" "" {
				"run m.futil &"
			}
			"Create cell/area tables & calculate unit conversions" "" {
				"source $env(TCLTKGRASSBASE)/module/m.qcalc"
			}
			"Create/edit projection information for current location" "" {
				"run g.setproj &"
			}
			"CTG data from USGS lulc file" "" {
			    "source $env(TCLTKGRASSBASE)/module/m.lulc.USGS"
			}
			"Flip elevation data" "" {
			    "source $env(TCLTKGRASSBASE)/module/m.flip"
			}
			"Information on Tiger Region" "" {
			    "source $env(TCLTKGRASSBASE)/module/m.tiger.region"
			}
			"Rotate elevation data 90 degree" "" {
			    "source $env(TCLTKGRASSBASE)/module/m.rot90"
			}
			"Show current GRASS environment settings" "" {
				"run g.gisenv &"
			}
			"Show current GRASS version" "" {
				"run g.version &"
			}
			"UTM Region to lat/lon Region" "" {
				"source $env(TCLTKGRASSBASE)/module/m.region.ll"
			}
		}
	}
    	Display "Display maps" {
		    "Display Manager" "" {
          	    "exec xterm -title {Display Manager Messages} -e d.dm &"
		    }
            "NVIZ visualization tool" "" {
                "source $env(TCLTKGRASSBASE)/module/nviz"
        	}
            "Create path for scripting a fly-through animation in NVIZ" "" {
                "source $env(TCLTKGRASSBASE)/module/d.nviz"
        	}
		-separator
    		"Start displays" "" {
        		"All active saved X" "" {start_monitors}
        		-separator
        		X0 "" {"start_monitor x0"}
        		X1 "" {"start_monitor x1"}
        		X2 "" {"start_monitor x2"}
        		X3 "" {"start_monitor x3"}
        		X4 "" {"start_monitor x4"}
        		X5 "" {"start_monitor x5"}
        		X6 "" {"start_monitor x6"}
        		-separator
        		CELL "" {"exec xterm -iconic -e d.mon start=CELL"}
			-separator
			"Start/restart display at specified window size" "" {
            		"source $env(TCLTKGRASSBASE)/module/d.monsize"
			}
    		}
    		"Stop displays" "" {
        		"All X" "" {stop_monitors}
        		-separator
        		X0 "" {"stop_monitor x0"}
        		X1 "" {"stop_monitor x1"}
        		X2 "" {"stop_monitor x2"}
        		X3 "" {"stop_monitor x3"}
    			X4 "" {"stop_monitor x4"}
    			X5 "" {"stop_monitor x5"}
        		X6 "" {"stop_monitor x6"}
        		-separator
        		CELL "" {"exec xterm -iconic -e d.mon stop=CELL"}
    		}
    		"Select displays" "" {
			X0 "" {"exec xterm -iconic -e d.mon select=x0"}
   			X1 "" {"exec xterm -iconic -e d.mon select=x1"}
     		X2 "" {"exec xterm -iconic -e d.mon select=x2"}
       		X3 "" {"exec xterm -iconic -e d.mon select=x3"}
        		X4 "" {"exec xterm -iconic -e d.mon select=x4"}
        		X5 "" {"exec xterm -iconic -e d.mon select=x5"}
        		X6 "" {"exec xterm -iconic -e d.mon select=x6"}
        		-separator
        		CELL "" {"exec xterm -iconic -e d.mon select=CELL"}
    		}
        	-separator
        	Raster "Display raster maps" {
    			"Display raster maps" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.rast"
    			}
	        	"Display raster map, legend, & title in active display" "" {
	            	"run d.rast.leg &"
	        	}
			-separator
    			"Display raster with equalized greyscale colors" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.rast.rescale"
    			}
    			"Display HIS overlays" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.his"
    			}
    			"Display RGB overlays" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.rgb"
    			}
    			"Drape raster map over shaded relief or aspect map" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.shadedmap"
    			}
			-separator
    			"Display 2.5D maps" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.3d"
    			}
        		"Display raster map and oblique 3D views from 6 directions" "" {
            		"source $env(TCLTKGRASSBASE)/module/d.3d.views"
        		}
			-separator
 	       	"Slide show of all raster maps in current mapset" "" {
 	           	"run slide.show.sh &"
	        	}
		}
		Vector "Display vector maps" {
    			"Display vector maps" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.vect"
    			}
    			"Display vector areas" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.vect.area"
    			}
    			"Display vector lines" "" {
    				"source $env(TCLTKGRASSBASE)/module/d.vect.line"
    			}
		}
        	Sites  "Display sites"  {
    			"Display sites as markers" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.sites"
    			}
    			"Display sites as icons" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.sites.icons"
    			}
    			-separator
    			"Display points as markers" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.points"
    			}
    			"Display points as icons" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.icons"
    			}
			-separator
            	"Create/edit icons" "" {
                	"run p.icons &"
            	}
		}
        	Text "Display text on maps" {
    			"Label raster maps" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.rast.labels"
    			}
    			"Label vector maps" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.vect.labels"
    			}
    			"Label sites" "" {
        			"source $env(TCLTKGRASSBASE)/module/d.site.labels"
    			}
			-separator
            	"Display legend for raster maps" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.legend"
            	}
			-separator
            	"Display category values in raster map cells" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.rast.num"
            	}
			-separator
            	"Display map title" "" {
               	"source $env(TCLTKGRASSBASE)/module/d.title.sh"
            	}
			-separator
            	"Create text labels" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.labels"
            	}
            	"Display text labels" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.label"
            	}
            	"Display text labels for paint output" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.paint.labels"
            	}
			-separator
            	"Select text font" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.font"
            	}
			-separator
            	"Draw text" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.text"
            	}
            	"Draw text using TrueType fonts" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.text.freetype"
            	}
			-separator
            	"Display standard GRASS fonts" "" {
                	"run show.fonts.sh &"
            	}
        	}
        	Graphics "Display graphics on maps" {
            	"Display histogram" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.histogram"
            	}
            	"Display geodesic line" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.geodesic"
            	}
            	"Display rhumbline" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.rhumbline"
            	}
			-separator
            	"Display color table" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.colortable"
            	}
            	"Display standard GRASS colors" "" {
                	"run show.color.sh &"
            	}
			-separator
            	"Overlay bar scale and north arrow" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.barscale"
            	}
            	"Overlay grid" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.grid"
            	}
            	"Overlay slope arrows on aspect raster map" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.rast.arrow"
            	}
			-separator
            	"Draw simple graphics in active display monitor (display coordinates)" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.graph"
            	}
            	"Draw simple graphics in active display monitor (map coordinates)" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.mapgraph"
            	}
        	}
		-separator
        	"Display multiple maps, legend, & title in active display" "" {
            	"run d.display &"
        	}
        	"Split active display and show maps in each half" "" {
                "source $env(TCLTKGRASSBASE)/module/d.split"
        	}
		-separator
    		"Manage displays" "" {
        		"source $env(TCLTKGRASSBASE)/module/d.mon"
    		}
		"Manage display frames" "" {
            	"source $env(TCLTKGRASSBASE)/module/d.frame"
        	}
		"Show information about active display monitor" "" {
            	"run d.info &"
        	}
		-separator
        	"Manage colormode" "" {
            	"source $env(TCLTKGRASSBASE)/module/d.colormode"
        	}
		-separator
        	"Save file of commands to recreate active display" "" {
            	"source $env(TCLTKGRASSBASE)/module/d.save"
        	}
		-separator
        	"Redraw active display (Note: some items may not be redrawn)" "" {
            	"run d.redraw&"
        	}
        	-separator
		"Measure lengths and areas" "" {
 			"source $env(TCLTKGRASSBASE)/module/d.measure"
		}
        	"Pan in active display" "" {
            	"source $env(TCLTKGRASSBASE)/module/d.pan"
        	}
		"Show geographical position" "" {
			"source $env(TCLTKGRASSBASE)/module/d.where"
		}
		"Zoom/Unzoom in active display" "" {
			"source $env(TCLTKGRASSBASE)/module/d.zoom"
        	}
		-separator
        	"Erase active display/frame" "" {
			"source $env(TCLTKGRASSBASE)/module/d.erase"
        	}
    	}
	Raster "Raster map analysis" {
        	"Develop map" "" {
			"Digitize" "" {
				"run r.digit &"
            	}
			-separator
            	"Create/modify support file" "" {
                	"run r.support &"
            	}
			-separator
            	"Compress/decompress raster file" "" {
				"source $env(TCLTKGRASSBASE)/module/r.compress"
            	}
            	"Manage boundary definitions" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.region"
            	}
            	"Manage null values" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.null"
            	}
            	"Manage timestamps for files" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.timestamp"
            	}
            	"Quantization for floating-point maps" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.quant"
            	}
			"Resample (change resolution)" "" {
				"source $env(TCLTKGRASSBASE)/module/r.resample"
			}
			-separator
			"Reproject raster from other location" "" {
				"source $env(TCLTKGRASSBASE)/module/r.proj"
			}
			"Shift raster map in display using mouse" "" {
				"source $env(TCLTKGRASSBASE)/module/r.fix.ortho"
			}
		}
		"Manage map colors" "" {
            	"Modify color table" "" {
                	"source $env(TCLTKGRASSBASE)/module/d.colors"
            	}
               "Set colors to color table from another raster map" "" {
                   	"source $env(TCLTKGRASSBASE)/module/r.colorsrast"
               }
               "Set colors to predefined color tables" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.colors"
               }
			-separator
            	"Blend 2 color maps to produce 3 RGB files" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.blend"
            	}
			"Create color image from RGB files" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.composite"
			}
		}
		-separator
		"Query by coordinate(s)" "" {
			"source $env(TCLTKGRASSBASE)/module/r.what"
		}
		"Query with mouse" "" {
                "source $env(TCLTKGRASSBASE)/module/d.what.rast"
		}
		-separator
		"Set/remove mask" "" {
			"run r.mask &"
        	}
		-separator
		"Create buffers" "" {
			"source $env(TCLTKGRASSBASE)/module/r.buffer"
		}
		"Map calculator" "" {
			"source $env(TCLTKGRASSBASE)/module/mapcalculator"
		}
		"Neighborhood analysis" "" {
			"source $env(TCLTKGRASSBASE)/module/r.neighbors"
		}
		"Overlay maps" "" {
            "Cross product" "" {
                "source $env(TCLTKGRASSBASE)/module/r.cross"
            }
            "Patch maps" "" {
                "source $env(TCLTKGRASSBASE)/module/r.patch"
            }
			-separator
            "Bayesian expert system" "" {
                "source $env(TCLTKGRASSBASE)/module/r.binfer"
            }
			"Boolean overlays" "" {
			 	"source $env(TCLTKGRASSBASE)/module/r.combine"
            }
            "Inference engine" "" {
                "source $env(TCLTKGRASSBASE)/module/r.infer"
            }
			"Weighted overlays" "" {
				"run r.weight &"
			}
			"Weighted overlays using rules file" "" {
                "source $env(TCLTKGRASSBASE)/module/r.weight2"
			}
			-separator
			"Create map whose cells are a function of corresponding cells in a series of maps" "" {
                "source $env(TCLTKGRASSBASE)/module/r.series"
            }
			-separator
			"Statistical calculations for cover map over base map" "" {
                "source $env(TCLTKGRASSBASE)/module/r.statistics"
            }
		}
		"Solar radiance and shadows" "" {
			"Cumulative daily solar irradiation" "" {
				"source $env(TCLTKGRASSBASE)/module/r.sun.cum"
			}
            	"Solar irradiance for specific time" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.sun.time"
			}
			-separator
			"Shadows map for sun position" "" {
				"source $env(TCLTKGRASSBASE)/module/r.sunmask.suncalc"
			}
			"Shadows map for specific time" "" {
				"source $env(TCLTKGRASSBASE)/module/r.sunmask.timecalc"
			}
		}
		"Terrain analysis" "" {
               "Cost surface" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.cost"
               }
               "Least cost route or flow" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.drain"
               }
			"Profile analysis" "" {
				"source $env(TCLTKGRASSBASE)/module/d.profile"
			}
			"Shaded relief map" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.hillshade"
			}
               "Slope and aspect" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.slope.aspect"
               }
			"Terrain parameters" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.param.scale"
			}
			"Textural features" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.texture"
			}
               "Visibility/line of sight" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.los"
               }
            }
		"Transform features" "" {
            	"Clump small areas" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.clump"
			}
			"Grow areas" "" {
				"source $env(TCLTKGRASSBASE)/module/r.grow"
			}
			"Thin linear features" "" {
				"source $env(TCLTKGRASSBASE)/module/r.thin"
			}
		}
		-separator
		"Hydrologic modeling" "" {
                "Cascaded 2D hydrologic model" "" {
                    "run r.hydro.CASC2D &"
                }
                "Depressionless elevation map and flowline map" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.fill.dir"
                }
                "Finite element analysis for hydrologic simulations" "" {
                    "run r.water.fea &"
                }
                "Flow accumulation for massive grids (floating-point version)" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.terraflow"
                }
                "Flow accumulation for massive grids (integer version)" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.terraflow.short"
                }
                "Flowline map" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.flow"
                }
                "Flowline map using MMD algorithm" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.flowmd"
                }
                "Topographic index map" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.topidx"
                }
                "TOPMODEL simulation" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.topmodel"
                }
			 "Watershed subbasins" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.basins.fill"
                }
                "Watershed basin creation" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.water.outlet"
                }
                "Watershed basin analysis" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.watershed"
                }
		}
		"Landscape structure modeling" "" {
			"Set up sampling and analysis framework" "" {
                    "run r.le.setup &"
                }
			 -separator
                "Analyze landscape characteristics" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.le.pixel"
                }
                "Analyze landscape patch characteristics" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.le.patch"
                }
                "Output landscape patch information" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.le.trace"
                }
		}
		"Wildfire modeling" "" {
			"Generate rate of spread (ROS) maps" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.ros"
                }
                "Generate least-cost spread paths" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.spreadpath"
                }
                "Simulate anisotropic spread phenomena" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.spread"
                }
		}
		-separator
		"Change category values and labels" "" {
            	"Edit category values of individual cells for displayed raster map" "" {
                	"run d.rast.edit &"
            	}
			-separator
            	"Reclassify categories for areas of specified sizes" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.reclass.area"
            	}
            	"Reclassify categories interactively" "" {
                	"run r.reclass &"
            	}
            	"Reclassify categories using rules" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.reclass"
            	}
			-separator
            	"Recode categories using rules (create new map)" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.recode.rules"
            	}
            	"Recode categories using rules file (create new map)" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.recode.file"
            	}
			-separator
            	"Rescale categories (create new map)" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.rescale"
            	}
            	"Rescale categories with equalized histogram (create new map)" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.rescale.eq"
            	}
		}
		-separator
		"Generate surfaces" "" {
			"Interpolate surfaces from points" "" {
				"Inverse distance weighted from sites" "" {
                		"source $env(TCLTKGRASSBASE)/module/s.surf.idw"
            		}
				"Inverse distance weighted from raster" "" {
                		"source $env(TCLTKGRASSBASE)/module/r.surf.idw"
            		}
            	"Inverse distance weighted from raster (alternate method)" "" {
                		"source $env(TCLTKGRASSBASE)/module/r.surf.idw2"
            		}
            	"Regularized spline tension from sites" "" {
                		"source $env(TCLTKGRASSBASE)/module/s.surf.rst"
            		}
            	"Regularized spline tension from vector points" "" {
                		"source $env(TCLTKGRASSBASE)/module/v.surf.rst"
            		}
			}
			"Interpolate surfaces from contours" "" {
            		"Interpolate from raster contours" "" {
                		"source $env(TCLTKGRASSBASE)/module/r.surf.contour"
            		}
            		"Regularized spline tension from vector contours" "" {
                		"source $env(TCLTKGRASSBASE)/module/v.surf.rst"
            		}
		  	}
			-separator
            	"Fill null cells through RST interpolation" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.fillnulls"
            	}
            	"Generate fractal surface" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.surf.fractal"
            	}
            	"Generate gaussian deviates surface" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.surf.gauss"
            	}
            	"Generate plane" "" {
                	"run r.plane &"
            	}
            	"Generate random deviates surface" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.surf.random"
            	}
            	"Generate random surface with spatial dependence" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.random.surface"
            	}
		}
		"Generate points" "" {
            	"Generate random cells" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.random.cells"
            	}
            	"Generate random cells & sites from raster map" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.random"
            	}
        	}
		-separator
        	"Reports & statistics" "" {
			"Report basic file information" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.info"
            	}
            	"Report category labels and values" "" {
             	   "source $env(TCLTKGRASSBASE)/module/r.cats"
            	}
           	 -separator
           	"General statistics" "" {
            	    "source $env(TCLTKGRASSBASE)/module/r.stats"
            	}
            	"Range of all category values" "" {
            	    "source $env(TCLTKGRASSBASE)/module/r.describe"
            	}
            	"Sum all cell category values" "" {
            	    "source $env(TCLTKGRASSBASE)/module/r.sum"
            	}
            	"Sum area by map and category" "" {
            	    "source $env(TCLTKGRASSBASE)/module/r.report"
            	}
            	"Total surface area, considering toppography" "" {
            	    "source $env(TCLTKGRASSBASE)/module/r.surf.area"
            	}
            	"Univariate statistics (faster binary version)" "" {
            	    "run r.univar &"
            	}
            	"Univariate statistics (script version with median and quartiles)" "" {
            	    "run r.univar.sh &"
            	}
            	"Volume of clumped cells" "" {
             	   "source $env(TCLTKGRASSBASE)/module/r.volume"
            	}
		  	-separator
            	"Sample values along transects" "" {
            	    "source $env(TCLTKGRASSBASE)/module/r.profile"
            	}
			"Sample values along transects (use azimuth, distance)" "" {
				"source $env(TCLTKGRASSBASE)/module/r.transect"
            	}
		  	-separator
            	"Covariance/correlation" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.covar"
            	}
            	"Linear regression between 2 maps" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.regression.line"
            	}
            	"Mutual category occurences (coincidence)" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.coin"
            	}
        	}
	}
    	Vector "Vector map analysis" {
		"Develop map" "" {
            	"Digitize" "" {
                	"run v.digit &"
            	}
			-separator
            	"Create/rebuild topology or edit vector categories" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.support"
            	}
            	-separator
            	"Add intermediate points between nodes" "" {
                	"run v.plant &"
            	}
            	"Bulk label unlabeled areas" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.alabel"
            	}
            	"Bulk label unlabeled lines" "" {
                "source $env(TCLTKGRASSBASE)/module/v.llabel"
            	}
            	"Manage timestamps for vector files" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.timestamp"
            	}
            	"Remove outer arcs from vector file" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.rmedge"
            	}
			-separator
            	"Convert areas to lines" "" {
                	"run v.area2line &"
            	}
            	"Convert closed lines/boundaries to areas" "" {
                	"run v.line2area &"
            	}
			-separator
            	"Create text label file for vector features" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.label"
            	}
		  	-separator
            	"Reproject vector from other location" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.proj"
            	}
		}
		"Clean map" "" {
            	"Clean dead lines" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.clean"
            	}
            	"Process spaghetti lines (ASCII vector file)" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.ascii.spag"
            	}
            	"Process spaghetti lines (binary vector map)" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.spag"
            	}
            	"Prune extra points from polylines and area borders" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.prune"
            	}
            	"Remove duplicate lines" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.rmdup"
            	}
            	"Trim dangling lines" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.rm.dangles"
            	}
			-separator
            	"Build polylines from adjacent segments" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.build.polylines"
            	}
            	"Split polylines into segments" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.split"
            	}
		}
		"Rectify & georeference vector map" "" {
			"source $env(TCLTKGRASSBASE)/module/v.transform"
		}
		-separator
		"Query by categories and extract features" "" {
			"source $env(TCLTKGRASSBASE)/module/v.extract"
           }
		"Query by coordinate(s)" "" {
			"source $env(TCLTKGRASSBASE)/module/v.what"
		}
		"Query with mouse" "" {
			"source $env(TCLTKGRASSBASE)/module/d.what.vect"
		}
		-separator
		"Calculate distances between points and vector features" "" {
			"source $env(TCLTKGRASSBASE)/module/v.distance"
           }
		"Display feature geometry (area and perimeter)" "" {
			"source $env(TCLTKGRASSBASE)/module/v.area"
           	}
		-separator
		"Overlay maps" "" {
			"Cut/clip features with areas" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.cutter"
            	}
			"Cut/clip features with current region window" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.cutregion"
            	}
			"Cut/clip features in multiple maps" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.cutter.attr"
            	}
            	"Patch multiple maps (combine)" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.patch"
            	}
        	}
		"Change category values and labels" "" {
            	"Reclassify features using rules" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.reclass"
            	}
            	"Reclassify features interactively" "" {
                	"run v.reclass &"
            	}
		}
		-separator
		"Generate special vector maps" "" {            
            	"Generate area features for boundaries of USGS 7.5-minute quadrangles" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.mkquads"
            	}
            	"Generate area features for extent of current region" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.region.sh"
            	}
            	"Generate grid" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.mkgrid"
            	}
            	"Generate transects (line or area)" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.in.transects"
            	}
		}
		"Generate points in areas" "" {            
            	"Generate random points in vector areas" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.random"
            	}
            	"Generate random points in vector areas (scaled)" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.scale.random"
            	}
		}
		-separator
		"Reports & statistics" "" {
			"Basic information" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.info"
            	}
			"Detailed information" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.dump"
			}				
			-separator
            	"General statistics" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.stats"
            	}
            	"Sum area and perimeter by category" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.report"
            	}
			-separator
            	"Calculate spatial autocorrelation statistics" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.autocorr"
            	}
		}
    }
	Site "Sites list analysis" {
		"Develop sites" "" {
            	"Reproject sites list from other location" "" {
                	"source $env(TCLTKGRASSBASE)/module/s.proj"
            	}
		}
		-separator
		"Query attributes to extract & display sites" "" {
			"source $env(TCLTKGRASSBASE)/module/d.sites.qual"
          }
    		"Query attributes to extract & display sites interactively" "" {
			"source $env(TCLTKGRASSBASE)/module/d.siter"
    		}
		"Query by coordinate(s)" "" {
			"source $env(TCLTKGRASSBASE)/module/s.what"
		}
		"Query & extract sites using raster mask" "" {
			"source $env(TCLTKGRASSBASE)/module/s.mask"
		}
		"Query & extract ASCII points list using raster mask" "" {
			"source $env(TCLTKGRASSBASE)/module/r.mask.points"
		}
		"Query with mouse" "" {
			"source $env(TCLTKGRASSBASE)/module/d.what.sites"
		}
		-separator
		"Average site attribute within cells" "" {
			"source $env(TCLTKGRASSBASE)/module/s.windavg"
		}
		"Univariate statistics within cells" "" {
			"source $env(TCLTKGRASSBASE)/module/s.cellstats"
		}
		-separator
		"Median polish of sites list" "" {
			"source $env(TCLTKGRASSBASE)/module/s.medp"
		}
		"Resource use within territories" "" {
			"source $env(TCLTKGRASSBASE)/module/s.territory"
		}
		"Semivariograms" "" {
            	"Create sample semivariogram for sites attribute (uses GNUplot)" "" {
                	"source $env(TCLTKGRASSBASE)/module/s.sv"
            	}
            	"Fit Semivariogram model to sites attribute (uses GNUplot)" "" {
                	"source $env(TCLTKGRASSBASE)/module/s.sv2svfit"
            	}
            	"Semivariogram model fitting (uses GNUplot)" "" {
                	"source $env(TCLTKGRASSBASE)/module/m.svfit"
            	}
		}
		-separator
		"Reclassify sites list" "" {
			"source $env(TCLTKGRASSBASE)/module/s.reclass"
		}
		-separator
		"Generate areas around sites" "" {
            	"Generate vector circles around sites (constant size)" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.circle"
            	}
            	"Generate vector circles around sites (variable sizes)" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.bubble"
            	}
            	"Generate vector convex hull around set of sites" "" {
                	"source $env(TCLTKGRASSBASE)/module/s.hull"
            	}
            	"Generate vector Delaunay triangulation around sites" "" {
                	"source $env(TCLTKGRASSBASE)/module/s.delaunay"
            	}
            	"Generate vector polygons around sites" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.in.poly"
            	}
            	"Generate vector Voronoi diagram (Thiessen polygons) around sites" "" {
                	"source $env(TCLTKGRASSBASE)/module/s.voronoi"
            	}
		}
		"Generate raster surface through interpolation" "" {
			"Inverse distance weighted" "" {
				"source $env(TCLTKGRASSBASE)/module/s.surf.idw"
			}
			"Regularized spline tension" "" {
				"source $env(TCLTKGRASSBASE)/module/s.surf.rst"
            	}
        	}
		"Create raster density map of site distributions using moving Gaussian kernel" "" {
			"source $env(TCLTKGRASSBASE)/module/s.kernel"
		}
		"Generate random sites" "" {
			"Generate randomly placed sites" "" {
				"source $env(TCLTKGRASSBASE)/module/s.random"
			}
			"Randomly sample values in raster map" "" {
				"source $env(TCLTKGRASSBASE)/module/s.sample"
            	}
			"Perturb sites randomly" "" {
				"source $env(TCLTKGRASSBASE)/module/s.perturb"
			}
		}
		"Reports & statistics" "" {
            	"Basic information" "" {
                	"source $env(TCLTKGRASSBASE)/module/s.info"
            	}
			-separator
            	"Probability plot of sites attribute (requires GNUplot)" "" {
                	"source $env(TCLTKGRASSBASE)/module/s.probplt"
            	}
            	"Quadrat count indexes for sites" "" {
                	"source $env(TCLTKGRASSBASE)/module/s.qcount"
            	}
            	"Tests of normality for sites attribute" "" {
                	"source $env(TCLTKGRASSBASE)/module/s.normal"
            	}
            	"Univariate statistics for sites attribute" "" {
                	"source $env(TCLTKGRASSBASE)/module/s.univar"
            	}
		}
	}
	Image "Image processing" {
    		"Develop images & groups" "" {
			"Create/edit imagery group" "" {
     	   		"run i.group &"
    			}
    			"Target imagery group" "" {
     	   		"run i.target &"
    			}
			-separator
         		"Mosaic up to 4 adjacent images" "" {
				"source $env(TCLTKGRASSBASE)/module/i.image.mosaic.tcl"
         		}
		}
		"Manage image colors" "" {
			"Assign a histogram contrast stretch qrey scale" "" {
				"source $env(TCLTKGRASSBASE)/module/r.colors.grey"
			}
			-separator
         		"Assign RGB channels to bands in image group" "" {
             		"run i.colors &"
         		}
			-separator		
         		"Create color composite image from user-specified RGB bands" "" {
             		"run i.composite &"
         		}
         		"Create color raster map from user-specified RGB bands in image group" "" {
             		"run i.quantize &"
         		}
			-separator
         		"Transform HIS (Hue/Intensity/Saturation) color image to RGB (Red/Green/Blue)" "" {
             		"source $env(TCLTKGRASSBASE)/module/i.his.rgb"
         		}
         		"Transform RGB (Red/Green/Blue) color image to HIS (Hue/Intensity/Saturation)" "" {
             		"source $env(TCLTKGRASSBASE)/module/i.rgb.his"
         		}
		}
    		"Rectify & georeference image group" "" {
         		"Set ground control points (GCP's) from raster map" "" {
             		"run i.points &"
         		}
         		"Set ground control points (GCP's) from vector map" "" {
             		"run i.vpoints &"
         		}
			-separator
         		"Affine and Polynomial rectification (rubber sheet)" "" {
             		"run i.rectify &"
         		}
			-separator
         		"Ortho photo rectification" "" {
             		"run i.ortho.photo &"
         		}
    		}
    		-separator
		"Set/remove raster mask" "" {
			"run r.mask &"
        	}
		-separator
    		"Classify image" "" {
         		"Clustering input for unsupervised classification" "" {
             		"source $env(TCLTKGRASSBASE)/module/i.cluster"
         		}
	         	-separator
         		"Maximum likelyhood classification (MLC)" "" {
             		"source $env(TCLTKGRASSBASE)/module/i.maxlik"
         		}
         		"Sequential maximum a posteriory classification (SMAP)" "" {
             		"source $env(TCLTKGRASSBASE)/module/i.smap"
         		}
			-separator
         		"Interactive input for supervised classification" "" {
             		"run i.class &"
         		}
         		"Non-interactive input for supervised classification (MLC)" "" {
             		"source $env(TCLTKGRASSBASE)/module/i.gensig"
         		}
         		"Non-interactive input for supervised classification (SMAP)" "" {
             		"source $env(TCLTKGRASSBASE)/module/i.gensigset"
         		}
    		}
		"Dehaze for LandSAT 5" "" {
             	"source $env(TCLTKGRASSBASE)/module/i.tm.dehaze.tcl"
        	}
    		"Filter image" "" {
         		"Zero edge crossing detection" "" {
             		"source $env(TCLTKGRASSBASE)/module/i.zc"
         		}
         		"User defined matrix filter" "" {
             		"source $env(TCLTKGRASSBASE)/module/r.mfilter"
         		}	
    		}
		"Shape dectection" "" {
             	"source $env(TCLTKGRASSBASE)/module/i.shape"
        	}
		"Spectral response (uses GNUplot)" "" {
             	"source $env(TCLTKGRASSBASE)/module/i.spectral.tcl"
        	}
		"Tassled cap vegetation index for LandSAT 5" "" {
             	"source $env(TCLTKGRASSBASE)/module/i.tasscap.tm5.tcl"
        	}
 		"Tassled cap vegetation index for LandSAT 7" "" {
             	"source $env(TCLTKGRASSBASE)/module/i.tasscap.tm7.tcl"
        	}
   		"Transform image" "" {
         		"Canonical component" "" {
             		"source $env(TCLTKGRASSBASE)/module/i.cca"
         		}
         		"Principal component" "" {
          		   "source $env(TCLTKGRASSBASE)/module/i.pca"
         		}
			"Fast Fourier Transform" "" {
				"source $env(TCLTKGRASSBASE)/module/i.fft"
         		}
         		"Inverse Fast Fourier Transform" "" {
             		"source $env(TCLTKGRASSBASE)/module/i.ifft"
         		}
    		}
    		-separator
        	"Reports & statistics" "" {
			"Report basic file information" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.info"
            	}
            	"Range of image values" "" {
            	    "source $env(TCLTKGRASSBASE)/module/r.describe"
            	}
			-separator
            	"Calculate texture statistics for image" "" {
            	    "source $env(TCLTKGRASSBASE)/module/i.texture"
            	}
            	"Iterative proportional fitting for error matrices" "" {
                	"source $env(TCLTKGRASSBASE)/module/m.ipf"
            	}
            	"Kappa classification aaccuracy assessment" "" {
                	"source $env(TCLTKGRASSBASE)/module/r.kappa"
            	}
            	"Optimum index factor for LandSat TM" "" {
                	"source $env(TCLTKGRASSBASE)/module/i.oif"
            	}
			"Regression analysis" "" {
	            	"Compute linear and non-linear multiple regression models for up to 3 image bands and feature value" "" {
	            	    "source $env(TCLTKGRASSBASE)/module/r.rational.regression"
     	       	}
	            	"Compute simple linear multiple regression model for up to 3 image bands and feature value" "" {
	            	    "source $env(TCLTKGRASSBASE)/module/r.linear.regression"
     	       	}
	            	"Use regression model to predict feature values from 3 image bands" "" {
	            	    "source $env(TCLTKGRASSBASE)/module/i.rvi.prediction"
     	       	}
			}
		}
	}
	"Grid3D" "3D volume management. Use NVIZ to view." {
		"Develop grid3D volumes" "" {
			"Manage nulls for grid3D volume" "" {
	  		   	"source $env(TCLTKGRASSBASE)/module/r3.null"
			}
			"Manage timestamp for grid3D volume" "" {
	     		"source $env(TCLTKGRASSBASE)/module/r3.timestamp"
			}
		}
		"Create 3D mask for grid3D operations" "" {
	     	"source $env(TCLTKGRASSBASE)/module/r3.mask"
		}
		"Map Calculator for grid3D volumes" "" {
	     	"run r3.mapcalc &"
		}
		"Report & Statistics" "" {
			"Display information about grid3D volume" "" {
	    		 	"source $env(TCLTKGRASSBASE)/module/r3.info"
        		}
		}
	}
	Databases "Database management" {
		"PostgreSQL" "" {
			"General" "" {
	    			"Column stats" "" {
                    	"source $env(TCLTKGRASSBASE)/module/g.stats.pg"
                	}
    	    			"List tables" "" {
                    	"source $env(TCLTKGRASSBASE)/module/g.table.pg"
                	}
            		"List columns" "" {
                    	"source $env(TCLTKGRASSBASE)/module/g.column.pg"
                	}
				"Select DB" "" {
                    	"source $env(TCLTKGRASSBASE)/module/g.select.pg"
                	}
            	}
			"Query" "" {
	        		"Raster" "" {
                    	"source $env(TCLTKGRASSBASE)/module/d.what.r.pg"
                	}
	        		"Sites" "" {
                    	"source $env(TCLTKGRASSBASE)/module/d.what.s.pg"
                	}
                	"Vector" "" {
                    	"source $env(TCLTKGRASSBASE)/module/d.what.v.pg"
                	}
            	}
	    		"Display" "" {
	        		"Raster" "" {
                    	"source $env(TCLTKGRASSBASE)/module/d.rast.pg"
                	}
	        		"Sites" "" {
                    	"source $env(TCLTKGRASSBASE)/module/d.site.pg"
                	}
                	"Vector" "" {
                    	"source $env(TCLTKGRASSBASE)/module/d.vect.pg"
                	}
			}
            	"Export raster categories to PostgreSQL table" "" {
				"source $env(TCLTKGRASSBASE)/module/r.to.pg"
               }
            	"Export vector categories to PostgreSQL table" "" {
				"source $env(TCLTKGRASSBASE)/module/v.to.pg"
               }
            	"Reclassify vector categories with PostgreSQL query" "" {
				"source $env(TCLTKGRASSBASE)/module/v.reclass.pg"
               }
			-separator
            	"Import DBF file to PostgreSQL table" "" {
				"source $env(TCLTKGRASSBASE)/module/pg.in.dbf"
               }
        	}
		-separator
		"DBMI" "" {
            	"Manage database" "" {
				"Connect" "" {
               	 	"source $env(TCLTKGRASSBASE)/module/db.connect"
          	  	}
     	      	"Create database (does not work with ODBC)" "" {
	               	"source $env(TCLTKGRASSBASE)/module/db.createdb"
            		}
          	 	"PERMANTLY remove database (does not work with ODBC)" "" {
     	          	"source $env(TCLTKGRASSBASE)/module/db.dropdb"
	            	}
           		"PERMANTLY remove table (does not work with ODBC)" "" {
          	     	"source $env(TCLTKGRASSBASE)/module/db.droptable"
     	       	}
	            	"Select driver" "" {
                		"source $env(TCLTKGRASSBASE)/module/db.connect.driver"
            		}
          	 	"View/edit database attributes" "" {
     	          	"source $env(TCLTKGRASSBASE)/module/d.what.db"
	            	}
			}
			"Database information" "" {
          	 	"Describe table" "" {
     	          	"source $env(TCLTKGRASSBASE)/module/db.describe"
	            	}
          	  	"List columns" "" {
     	           	"source $env(TCLTKGRASSBASE)/module/db.columns"
	            	}
	    			"List databases" "" {
          	      	"source $env(TCLTKGRASSBASE)/module/db.databases"
     	       	}	    	    
		    		"List drivers" "" {
               	 	"source $env(TCLTKGRASSBASE)/module/db.drivers"
          	  	}	    	    
		    		"List tables" "" {
 	               	"source $env(TCLTKGRASSBASE)/module/db.tables"
            		}	    	    
			}
			-separator
            	"Query" "" {
				"Select all" "" {
             	 	  	"source $env(TCLTKGRASSBASE)/module/db.select.all"
            		}	    
            		"Select" "" {
              	 	 	"source $env(TCLTKGRASSBASE)/module/db.select"
            		} 
            		"Execute" "" {
               	 	"source $env(TCLTKGRASSBASE)/module/db.execute"
            		}
			}
	    		-separator
            	"Load vector to DB" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.to.db"
            	}	    	    
            	"Reclass vector" "" {
                	"source $env(TCLTKGRASSBASE)/module/v.db.reclass"
            	}
        	}
   	}
    Config "Configuration of TclTkGRASS" {
        "Resize menu" "" {resize_menu}
        "Module windows" "" {
            "Automatic size for all active windows" "" {
                "resize_menu; resize $module_list"
            }
            "Reinitialize all active windows" "" {
                "resize_menu; reinit_modules"
            }
            -separator
            "Undisplay all active windows" "" {
                unmap_modules
            }
            "Restore all undisplayed windows" "" {
                map_modules
            }
            -separator
            "Iconify all active windows" "" {
                "foreach module $module_list {catch {wm iconify .$module; \
                                                     wm iconify .$module.print}}"
            }
            "Deiconify all active windows" "" {
                "foreach module $module_list {catch {wm deiconify .$module; \
                                                     wm deiconify .$module.print}}"
            }
            -separator
            "Destroy all active windows" "" {
                "foreach module $module_list {catch {destroy .$module}}"
            }
        }
        -separator
        "Set fonts" "" {
            "Menu font" "" {
                "fontsel {Menu font} main_menu(font);\
                 setfont .main_menu $main_menu(font);\
                 resize_menu"
            }
            "Module font" "" {
                "fontsel {Module font} module_font;\
                 foreach module $module_list {setfont .$module $module_font} ;\
                 resize $module_list"
            }
            "Results font" "" {
                "fontsel {Result font} result_font"
            }
            "Dialog font" "" {
                "fontsel {Dialog font} dialog_font"
            }
        }
        "Set display dimensions" "" {
            "setdisplay"
        }
        "Configure html-browser" "" {
            "config_netscape"
        }
        "Save config" "" {
            "tcltkgrass_save ."
        }
    }
	"Help" "Help" {
		"Manual pages" "" {
			"source $env(TCLTKGRASSBASE)/module/g.manual"
        	}
		"GRASS help (text interface)" "" {
	    		"run g.help &"
		}
        	-separator
        	"Tcltkgrass menus help" "" {
           	"source $env(TCLTKGRASSBASE)/main/help.tcl"
        	}
        	"About tcltkgrass" "" {
           	"source $env(TCLTKGRASSBASE)/main/about.tcl"
        	}
		-separator
        	"About GRASS" "" {
           	"source $env(TCLTKGRASSBASE)/main/grassabout.tcl"
        	}
			-separator
		"About this System" "" {
	   		"exec $env(TCLTKGRASSBASE)/main/tksys.tcl --tcltk"
		}
		-separator
		"Help on scripting" "" {
	    		"source $env(TCLTKGRASSBASE)/main/help-scripting.tcl"
		}
		"Help on html-browser" "" {
	    		"source $env(TCLTKGRASSBASE)/main/help-netscape.tcl"
		}
	}
}
