frame .main_menu
pack .main_menu -expand yes -fill both

# $Id$
#

# menus used several times in the main menu

set monitors_menu {
    "Start" "" {
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
    }
    "Stop" "" {
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
    "Select" "" {
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
    "Manage display monitors" "" {
        "source $env(TCLTKGRASSBASE)/module/d.mon"
    }
}

set display_raster {
    "Display raster maps" "" {
        "source $env(TCLTKGRASSBASE)/module/d.rast"
    }
    "Display HIS values" "" {
        "source $env(TCLTKGRASSBASE)/module/d.his"
    }
    "Display RGB overlays" "" {
        "source $env(TCLTKGRASSBASE)/module/d.rgb"
    }
    "Display 3-d images" "" {
        "source $env(TCLTKGRASSBASE)/module/d.3d"
    }
    "Display shaded raster map" "" {
        "source $env(TCLTKGRASSBASE)/module/d.shadedmap"
    }
    "Display profile" "" {
        "source $env(TCLTKGRASSBASE)/module/d.profile"
    }
}

set display_vector {
    "Display vector maps" "" {
        "source $env(TCLTKGRASSBASE)/module/d.vect"
    }
    "Display vector polygons" "" {
        "source $env(TCLTKGRASSBASE)/module/d.area"
    }
    "Display USGS DLG-3 files" "" {
        ""
    }
}
set display_sites {
    "Display site markers (symbols)" "" {
        "source $env(TCLTKGRASSBASE)/module/d.sites"
    }
    "Display site labels" "" {
        "source $env(TCLTKGRASSBASE)/module/d.site.labels"
    }
    -separator
    "Display point markers (symbols)" "" {
        "source $env(TCLTKGRASSBASE)/module/d.points"
    }
    "Display point markers (icons)" "" {
        "source $env(TCLTKGRASSBASE)/module/d.icons"
    }
}

set image_processing {
    "Create/edit imagery group" "" {
        "run i.group &"
    }
    "Target imagery group" "" {
        "run i.target &"
    }
    -separator
    Rectification "" {
         "Setting ground control points (GCPs)" "" {
             "run i.points &"
         }
         "Affine transformation" "" {
             "run i.rectify &"
         }
         "Polynomial (Helmert) transformation" "" {
             "run i.rectify2 &"
         }
         "Ortho photo rectification" "" {
             "run i.ortho.photo &"
         }
    }
    "Image filtering" "" {
         "Zero edge crossing detection" "" {
             "source $env(TCLTKGRASSBASE)/module/i.zc"
         }
         "User defined matrix filter" "" {
             "source $env(TCLTKGRASSBASE)/module/r.mfilter"
         }
	 "Assign a histrogram contrast stretch qrey scale" "" {
	     "source $env(TCLTKGRASSBASE)/module/i.grey.scale"
	 }
    }
    "Image transformation" "" {
         "Canonical component" "" {
             "source $env(TCLTKGRASSBASE)/module/i.cca"
         }
         "Principal component" "" {
             "source $env(TCLTKGRASSBASE)/module/i.pca"
         }
         "Fast Fourier Transform" "" {
             "source $env(TCLTKGRASSBASE)/module/i.fft"
         }
         "Specify and remove mask for/from FFT image" "" {
             "run r.mask &"
         }
         "Inverse Fast Fourier Transform" "" {
             "source $env(TCLTKGRASSBASE)/module/i.ifft"
         }
    }
    Classification "" {
         "Clustering input for unsupervised classification" "" {
             "source $env(TCLTKGRASSBASE)/module/i.cluster"
         }
         "Interactive input for supervised classification" "" {
             "run i.class &"
         }
         "Non-interactive input for supervised classification (MLC)" "" {
             "source $env(TCLTKGRASSBASE)/module/i.gensig"
         }
         "Non-interactive input for supervised classification (SMAP)" "" {
             "source $env(TCLTKGRASSBASE)/module/i.gensigset"
         }
         -separator
         "Maximum likelyhood classification (MLC)" "" {
             "source $env(TCLTKGRASSBASE)/module/i.maxlik"
         }
         "Sequential maximum a posteriory classification (SMAP)" "" {
             "source $env(TCLTKGRASSBASE)/module/i.smap"
         }
    }
}

set misc {
    "Coordinate Conversions" "" {
	"Projection/Coordinate conversion" "" {
	    "run m.proj &"
	}
	"Datum Shift" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.datum.shift"
	}
	"geocentric to lat/lon" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.gc2ll"
	}
	"lat/lon to geocentric" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.ll2gc"
	}
	"UTM to lat/lon" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.u2ll"
	}
	"lat/lon to UTM" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.ll2u"
	}
    }
    -separator
    "DEM/DTED" "" {
	"DEM examination" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.dem.examine"
	}
	"DEM extraction" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.dem.extract"
	}
	"DTED examination" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.dted.examine"
	}
	"DTED extraction" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.dted.extract"
	}
    }
    -separator
    "Other" "" {
	"Rotate elevation data 90 degree" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.rot90"
	}
	"Flip elevation data" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.flip"
	}
	"CTG data from USGS lulc file" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.lulc.USGS"
	}
	"Information on Tiger Region" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.tiger.region"
	}
	"UTM Region to lat/lon Region" "" {
	    "source $env(TCLTKGRASSBASE)/module/m.region.ll"
	}
    }
}

# main menu

menu_build 1 .main_menu {
    Config "Configuration of TclTkGRASS" {
        Monitors "" $monitors_menu
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
        "Resize menu" "" {resize_menu}
        -separator
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
	-separator
        Options "" {
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
            -separator
            "Display dimensions" "" {
                setdisplay
            }
	    -separator
	    "Configure html-browser" "" {
		"config_netscape"
	    }
        }
        -separator
        "Save config" "" {
            "tcltkgrass_save ."
        }
    }
    Map "Map management (map files operations)" {
        "List" "" {
            "source $env(TCLTKGRASSBASE)/module/g.list"
        }
        "Copy" "" {
            "source $env(TCLTKGRASSBASE)/module/g.copy"
        }
        "Rename" "" {
            "source $env(TCLTKGRASSBASE)/module/g.rename"
        }
        "Remove" "" {
            "source $env(TCLTKGRASSBASE)/module/g.remove"
        }
        -separator
        "Mapset access" "" {
            "run g.access &"
        }
        "Mapset search path" "" {
            "source $env(TCLTKGRASSBASE)/module/g.mapsets"
        }
        "Mapset remove" "" {
            "source $env(TCLTKGRASSBASE)/module/mapset.remove"
        }
        -separator
        "Create/edit Imagery Group" "" {
            "run i.group &"
        }
        "Target Imagery Group" "" {
            "run i.target &"
        }
    }
    Region "Region coordinates management" {
        "ZOOM/UNZOOM in the monitor" "" {
            "source $env(TCLTKGRASSBASE)/module/d.zoom"
        }
        "PAN in the monitor" "" {
            "source $env(TCLTKGRASSBASE)/module/d.pan"
        }
        "Set/Remove MASK" "" {
            "run r.mask &"
        }
        -separator
        "Display region settings" "" {
            "run g.region -p &"
        }
        "Select default region" "" {
            "exec g.region -d; exec d.erase"
        }
        "Manage region" "" {
            "source $env(TCLTKGRASSBASE)/module/g.region.sh"
        }
    }
    Display "Display maps" {
        Monitors "" $monitors_menu
        -separator
        Raster "" $display_raster
        Vector "" $display_vector
        Sites  "" $display_sites
        Text "" {
            "Display map title" "" {
                "source $env(TCLTKGRASSBASE)/module/d.title"
            }
            "Display legend" "" {
                "source $env(TCLTKGRASSBASE)/module/d.legend"
            }
            "Display text labels" "" {
                "source $env(TCLTKGRASSBASE)/module/d.label"
            }
            "Display text labels for paint output" "" {
                "source $env(TCLTKGRASSBASE)/module/d.paint.labels"
            }
            "Select text font" "" {
                "source $env(TCLTKGRASSBASE)/module/d.font"
            }
            "Draw text" "" {
                "source $env(TCLTKGRASSBASE)/module/d.text"
            }
        }
        Graphics "" {
            "Display color table" "" {
                "source $env(TCLTKGRASSBASE)/module/d.colortable"
            }
            "Display geodesic line" "" {
                "source $env(TCLTKGRASSBASE)/module/d.geodesic"
            }
            "Display rhumbline" "" {
                "source $env(TCLTKGRASSBASE)/module/d.rhumbline"
            }
            "Overlay bar scale and north arrow" "" {
                "source $env(TCLTKGRASSBASE)/module/d.barscale"
            }
            "Overlay grid" "" {
                "source $env(TCLTKGRASSBASE)/module/d.grid"
            }
            "Display histogram" "" {
                "source $env(TCLTKGRASSBASE)/module/d.histogram"
            }
            "Display legend" "" {
                "source $env(TCLTKGRASSBASE)/module/d.legend"
            }
        }
        "Geographical position" "" {
            "source $env(TCLTKGRASSBASE)/module/d.where"
        }
        -separator
        "NVIZ visualization tool" "" {
            "source $env(TCLTKGRASSBASE)/module/nviz"
        }
        "ERASE display frame" "" {
            "source $env(TCLTKGRASSBASE)/module/d.erase"
        }
        "ZOOM/UNZOOM in the monitor" "" {
        "source $env(TCLTKGRASSBASE)/module/d.zoom"
        }
        "Manage display frames" "" {
            "source $env(TCLTKGRASSBASE)/module/d.frame"
        }
        "Manage colormode" "" {
            "source $env(TCLTKGRASSBASE)/module/d.colormode"
        }
    }
    Raster "Raster map analysis" {
        Display "" $display_raster
        "Analyse map" "" {
            "Query with mouse" "" {
                "source $env(TCLTKGRASSBASE)/module/d.what.rast"
            }
            "Query with definition file" "" {
                "source $env(TCLTKGRASSBASE)/module/r.what"
            }
            "Display profile" "" {
                "source $env(TCLTKGRASSBASE)/module/d.profile"
            }
            "Overlay maps" "" {
                "Specify mask" "" {
                    "run r.mask &"
                }
                "Cross product" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.cross"
                }
                "Patch maps" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.patch"
                }
                "Inference Engine" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.infer"
                }
                "Bayesian expert system" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.binfer"
                }
                "Map calculator" "" {
                    "run r.mapcalc &"
                }
                "Logical operation" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.combine"
                }
                "Weighting" "" {
                    "run r.weight &"
                }
            }
            "Neighborhood tools" "" {
                "Neighborhood analysis" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.neighbors"
                }
                "Buffer zone" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.buffer"
                }
                "Grow areas" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.grow"
                }
                "Thin linear features" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.thin"
                }
            }
            "Terrain tools" "" {
                "Watershed subbasins" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.basins.fill"
                }
                "Watershed maps" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.watershed"
                }
                "Cost between 2 locations" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.cost"
                }
                "Trace flow" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.drain"
                }
                "Calculate flowline map" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.flow"
                }
                "Slope and aspect" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.slope.aspect"
                }
                "Line of sight" "" {
                    "source $env(TCLTKGRASSBASE)/module/r.los"
                }
                "Create shaded raster map" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.shadedmap"
                }
            }
        }
        "Extract vector map" "" {
            "Lines from thinned raster" "" {
                "source $env(TCLTKGRASSBASE)/module/r.line"
            }
            "Area edges" "" {
                "source $env(TCLTKGRASSBASE)/module/r.poly"
            }
            "Contours" "" {
                "source $env(TCLTKGRASSBASE)/module/r.contour"
            }
        }
        "Develop map" "" {
            "Create/modify support file" "" {
                "run r.support &"
            }
            "Reclassify categories" "" {
                "source $env(TCLTKGRASSBASE)/module/r.reclass"
            }
            "Rescale categories" "" {
                "source $env(TCLTKGRASSBASE)/module/r.rescale"
            }
            "Average calculation" "" {
                "source $env(TCLTKGRASSBASE)/module/r.average"
            }
            "Statistical calculations" "" {
                "source $env(TCLTKGRASSBASE)/module/r.statistics"
            }
            "Clump small areas" "" {
                "source $env(TCLTKGRASSBASE)/module/r.clump"
            }
            -separator
            "Create color table" "" {
                "source $env(TCLTKGRASSBASE)/module/r.colors"
            }
            "Modify color table" "" {
                "source $env(TCLTKGRASSBASE)/module/d.colors"
            }
            -separator
            "Digitize" "" {
                "run r.digit &"
            }
            "Map calculator" "" {
                "run r.mapcalc &"
            }
            "Randomly located sites" "" {
                "source $env(TCLTKGRASSBASE)/module/r.random"
            }
            -separator
            "Resample (change resolution)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.resample"
            }
            "Interpolate using IDW (Lat./Long. locations)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.surf.idw"
            }
            "Interpolate using IDW2 (non-Lat./Long. loc.)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.surf.idw2"
            }
            "Interpolate from rasterized contours" "" {
                "source $env(TCLTKGRASSBASE)/module/r.surf.contour"
            }
            "Interpolate from vectorized contours" "" {
                "source $env(TCLTKGRASSBASE)/module/v.surf.spline"
            }
            -separator
            "Compress/decompress raster file" "" {
                "source $env(TCLTKGRASSBASE)/module/r.compress"
            }
        }
        "Report map" "" {
            "Basic information" "" {
                "source $env(TCLTKGRASSBASE)/module/r.info"
            }
            -separator
            "General statistics" "" {
                "source $env(TCLTKGRASSBASE)/module/r.stats"
            }
            "Category labels and values" "" {
                "source $env(TCLTKGRASSBASE)/module/r.cats"
            }
            "Category range" "" {
                "source $env(TCLTKGRASSBASE)/module/r.describe"
            }
            -separator
            "Area" "" {
                "source $env(TCLTKGRASSBASE)/module/r.report"
            }
            "Volume" "" {
                "source $env(TCLTKGRASSBASE)/module/r.volume"
            }
            "Mutual category occurences (Coincidence)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.coin"
            }
            "Values on transect lines" "" {
                "source $env(TCLTKGRASSBASE)/module/r.profile"
            }
            "Values on transect lines (use azimuth, distance)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.transect"
            }
            "Covariance/correlation" "" {
                "source $env(TCLTKGRASSBASE)/module/r.covar"
            }
        }
        -separator
        "Image processing" "" $image_processing
    }
    Vector "Vector map analysis" {
        Display "" $display_vector
        "Analyse map" "" {
                "Query with mouse" "" {
                       "source $env(TCLTKGRASSBASE)/module/d.what.vect"
                }
                "Query with definition file" "" {
                       "source $env(TCLTKGRASSBASE)/module/v.what"
                }
        }
        "Convert map" "" {
            "To raster format" "" {
                "source $env(TCLTKGRASSBASE)/module/v.to.rast"
            }
            "To sites format" "" {
                "source $env(TCLTKGRASSBASE)/module/v.to.sites"
            }
        }
        "Develop map" "" {
            "Create/rebuild topology" "" {
                "source $env(TCLTKGRASSBASE)/module/v.support_option=build"
            }
            "Edit vector categories" "" {
                "source $env(TCLTKGRASSBASE)/module/v.support_option=edit"
            }
            "Prune" "" {
                "source $env(TCLTKGRASSBASE)/module/v.prune"
            }
            "Process spaghetti binary map" "" {
                "source $env(TCLTKGRASSBASE)/module/v.spag"
            }
            "Process spaghetti ASCII map" "" {
                "source $env(TCLTKGRASSBASE)/module/v.ascii.spag"
            }
            "Clean dead lines" "" {
                "source $env(TCLTKGRASSBASE)/module/v.clean"
            }
            "Trim small spurs" "" {
                "source $env(TCLTKGRASSBASE)/module/v.trim"
            }
            "Build polylines" "" {
                "source $env(TCLTKGRASSBASE)/module/v.build.polylines"
            }            
            -separator
            "Change projection on ASCII vector" "" {
                "source $env(TCLTKGRASSBASE)/module/v.proj"
            }
            "Coordinate tranformation on ASCII vector" "" {
                "source $env(TCLTKGRASSBASE)/module/v.transform"
            }
            "Import ASCII vector map into GRASS vector format" "" {
                "run v.import &"
            }
            "Formatted ASCII file" "" {
                "run v.export &"
            }
            -separator
            "Digitize" "" {
                "run v.digit &"
            }
            "Create a grid" "" {
                "source $env(TCLTKGRASSBASE)/module/v.mkgrid"
            }
            "Cut (create new polygons)" "" {
                "source $env(TCLTKGRASSBASE)/module/v.cutter"
            }
            "Patch (overlay vector maps)" "" {
                "source $env(TCLTKGRASSBASE)/module/v.patch"
            }
            "Geometrical calculations" "" {
                 "source $env(TCLTKGRASSBASE)/module/v.geom"
            }
        }
        "Report map" "" {
            "Basic information" "" {
                "source $env(TCLTKGRASSBASE)/module/v.info"
            }
            "General statistics" "" {
                "source $env(TCLTKGRASSBASE)/module/v.stats"
            }
            "Dimensions" "" {
                "source $env(TCLTKGRASSBASE)/module/v.report"
            }
        }
    }
    Site "Site map analysis" {
        Display "" $display_sites
        "Query with mouse" "" {
            "source $env(TCLTKGRASSBASE)/module/d.what.sites"
        }
        Interpolation "" {
            "Inverse distance weighted" "" {
                "source $env(TCLTKGRASSBASE)/module/s.surf.idw"
            }
            "Spline with tension" "" {
                 "source $env(TCLTKGRASSBASE)/module/s.surf.rst"
            }
        }
    }
    Image "Image processing" $image_processing
    Import "Import maps into GRASS" {
        "Raster map" "" {
            "GIF (8bit)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.gif"
            }
            "TIFF 8bit" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.tiff"
            }
            "PNG (24bit)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.png"
            }
            "PPM (24bit)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.ppm"
            }
            "HDF" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.hdf"
            }
            "ERDAS LAN" "" {
                "source $env(TCLTKGRASSBASE)/module/i.in.erdas"
            }
            -separator
            "Binary file (GTOPO30 format) in a lat-lon region" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.bin"
            }
            "Binary file (GTOPO30 format) in a UTM region" "" {
                "source $env(TCLTKGRASSBASE)/module/r.in.ll"
            }
        }
        "Vector map" "" {
            "ASCII GRASS vector file" "" {
                "source $env(TCLTKGRASSBASE)/module/v.in.ascii"
            }
            "ARC/INFO ungenerate file" "" {
                "source $env(TCLTKGRASSBASE)/module/v.in.arc"
            }
            "ESRI shapefile" "" {
                "source $env(TCLTKGRASSBASE)/module/v.in.shape"
            }
            "Various formats" "" {
                "run v.import &"
            }
	    "Garmin GPS Waypoints/Routes/Tracks" "" {
		"source $env(TCLTKGRASSBASE)/module/v.in.garmin"
	    }
        }
        "Site data" "" {
            "ASCII file/spot heights" "" {
                "source $env(TCLTKGRASSBASE)/module/s.in.ascii"
            }
	    "Garmin GPS Waypoints/Routes/Tracks" "" {
		"source $env(TCLTKGRASSBASE)/module/s.in.garmin"
	    }
        }
    }
    Export "Export maps from GRASS" {
        "Raster map" "" {
            "TIFF (8/24bit)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.out.tiff"
            }
            "PPM (24bit)" "" {
                "source $env(TCLTKGRASSBASE)/module/r.out.ppm"
            }
            "HDF" "" {
                "source $env(TCLTKGRASSBASE)/module/r.out.hdf"
            }
        }
        "Vector map" "" {
            "ASCII GRASS vector file" "" {
                "source $env(TCLTKGRASSBASE)/module/v.out.ascii"
            }
            "ARC/INFO E00 file" "" {
                "source $env(TCLTKGRASSBASE)/module/v.out.e00"
            }
            "ARC/INFO ungenerate file" "" {
                "source $env(TCLTKGRASSBASE)/module/v.out.arc"
            }
            "Various formats" "" {
                "run v.export &"
            }
        }
        "Site data" "" {
            "ASCII file" "" {
                "source $env(TCLTKGRASSBASE)/module/s.out.ascii"
            }
        }
    }
    MapCreating MapCreating {
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
            "Paint map creation" "" {
                "source $env(TCLTKGRASSBASE)/module/p.map.new"
            }
        }
        "Postscript driver" "" {
            "Create/edit icon" "" {
                 "source $env(TCLTKGRASSBASE)/module/ps.icon"
             }
             "Printer select" "" {
                 "source $env(TCLTKGRASSBASE)/module/ps.select"
             }
             "Postscript map creation" "" {
                 "source $env(TCLTKGRASSBASE)/module/ps.map"
             }
        }
        "Xfig (external)" "" {
            "run xfig &"
        }
    }
    Misc "Miscellanous Conversions" $misc
    Databases "Databases" {
        "PostgreSQL" "" {
            "General" "" {
                "Select DB" "" {
                    "source $env(TCLTKGRASSBASE)/module/g.select.pg"
                }
    	    "List tables" "" {
                    "source $env(TCLTKGRASSBASE)/module/g.table.pg"
                }
            "List columns" "" {
                    "source $env(TCLTKGRASSBASE)/module/g.column.pg"
                }
	    "Column stats" "" {
                    "source $env(TCLTKGRASSBASE)/module/g.stats.pg"
                }
            }
            "Query" "" {
                "Vector" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.what.v.pg"
                }
	        "Sites" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.what.s.pg"
                }
	        "Raster" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.what.r.pg"
                }
            }
	    "Display" "" {
                "Vector" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.vect.pg"
                }
	        "Sites" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.site.pg"
                }
	        "Raster" "" {
                    "source $env(TCLTKGRASSBASE)/module/d.rast.pg"
                }
            "Reclass vector" "" {
                    "source $env(TCLTKGRASSBASE)/module/v.reclass.pg"
                }
            }
        }
	-separator
	"DBMI" "" {
            "Select driver" "" {
                "source $env(TCLTKGRASSBASE)/module/db.connect.driver"
            }
            "Connect" "" {
                "source $env(TCLTKGRASSBASE)/module/db.connect"
            }
            -separator
	    "List tables" "" {
                "source $env(TCLTKGRASSBASE)/module/db.tables"
            }	    	    
            "List columns" "" {
                "source $env(TCLTKGRASSBASE)/module/db.columns"
            }
            "Describe table" "" {
                "source $env(TCLTKGRASSBASE)/module/db.describe"
            }
            -separator
            "Select all" "" {
                "source $env(TCLTKGRASSBASE)/module/db.select.all"
            }	    
            "Select" "" {
                "source $env(TCLTKGRASSBASE)/module/db.select"
            } 
            "Execute" "" {
                "source $env(TCLTKGRASSBASE)/module/db.execute"
            }
	    -separator
            "Reclass vector" "" {
                "source $env(TCLTKGRASSBASE)/module/v.db.reclass"
            }
            "Load vector to DB" "" {
                "source $env(TCLTKGRASSBASE)/module/v.to.db"
            }	    	    
        }
    }
    Help Help {
        "Manual pages" "" {
           "source $env(TCLTKGRASSBASE)/module/g.manual"
        }
        -separator
        "Help" "" {
           "source $env(TCLTKGRASSBASE)/main/help.tcl"
        }
        About "" {
           "source $env(TCLTKGRASSBASE)/main/about.tcl"
        }
	-separator
	"Help on scripting" "" {
	    "source $env(TCLTKGRASSBASE)/main/help-scripting.tcl"
	}
	"Help on html-browser" "" {
	    "source $env(TCLTKGRASSBASE)/main/help-netscape.tcl"
	}
    }
    Quit Bye quit
}
