

                                   APPENDIX I                    1
                              GRASS-MAPGEN User's Guide
            plot.sh                                         plot.sh

            
            
            NAME 
            plot.sh - Draws the contents of the map composition on a
            plotter.
            
            SYNOPSIS 
            plot.sh [-s value] [-r] [overlay1] [overlay2] ...
            
            DESCRIPTION
            plot.sh draws the contents of the map composition, which are 
            stored in the ovm directory, in the selected GRASS window.
            The -s option allows for scaling, where the input value is a 
            percentage of the map definition (e.g. -s .5).  When the -r 
            option is selected, the x and y axis are reversed on the
            plotting device; by default, the x axis is always the long
            side of the paper.
            
            overlay1 is a graphic overlay file to display; the default
            is ovm/*, which shows all graphic overlay files in the ovm
            directory.
            
            The user is required to provide the following:
            
                 - plotter name
                 - paper size (see paper size)
            
            EXAMPLES 
            
                 plot.sh ovm/*
                 plot.sh -s .5 ovm/*
                 plot.sh ovm/cornfld ovm/fields
                 plot.sh ovm/* ../[map2]/ovm/*
                 plot.sh -m test.def ovm/*
            
            SEE ALSO            
                 plotter(1)     User Manual for MAPGEN (UNIX 
                                Version), USGS, Open-File Report 85-706,
                                p. 117.
                 monitor docu-  GRASS User's Manual, Version 3.1,
                   mentation    CERL.
                 plotter docu-  System Manual
                   mentation
                 paper sizes    GRASS-MAPGEN User's Guide, APPENDIX I-A,
                                USDA-SCS.
            
            AUTHOR 
            Marty Holko, Computer Systems Analyst, Cartography and
            Geographic Information Systems Division, USDA, Soil
