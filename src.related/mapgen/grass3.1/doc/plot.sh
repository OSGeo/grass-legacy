

                                   APPENDIX I                    1
                              GRASS-MAPGEN User's Guide
            plot.sh                                         plot.sh            plot.sh                                         plot.sh




            
            
            NAME            NAME
            plot.sh - Draws the contents of the map composition on a
            plotter.
            
            SYNOPSIS            SYNOPSIS
            plot.sh [-s value] [-r] [overlay1] [overlay2] ...            plot.sh                                                               __ _____   __   ________   ________     
            
            DESCRIPTION            DESCRIPTION
            plot.sh draws the contents of the map composition, which are            _______                                                     
            stored in the ovm directory, in the selected GRASS window.                          ___                                         
            The -s option allows for scaling, where the input value is a                __                                                      
            percentage of the map definition (e.g. -s .5).  When the -r                                                                     __
            option is selected, the x and y axis are reversed on the
            plotting device; by default, the x axis is always the long
            side of the paper.
            
            overlay1 is a graphic overlay file to display; the default            ________                                                  
            is ovm/*, which shows all graphic overlay files in the ovm               _____                                               ___
            directory.
            
            The user is required to provide the following:
            
                 - plotter name
                 - paper size (see paper size)
            
            EXAMPLES            EXAMPLES
            
                 plot.sh ovm/*
                 plot.sh -s .5 ovm/*
                 plot.sh ovm/cornfld ovm/fields
                 plot.sh ovm/* ../[map2]/ovm/*
                 plot.sh -m test.def ovm/*
            
            SEE ALSO            SEE ALSO
                 plotter(1)     User Manual for MAPGEN (UNIX                                                                             ___________                                
                                Version), USGS, Open-File Report 85-706,
                                p. 117.
                 monitor docu-  GRASS User's Manual, Version 3.1,
                   mentation    CERL.
                 plotter docu-  System Manual
                   mentation
                 paper sizes    GRASS-MAPGEN User's Guide, APPENDIX I-A,                 _______________                                        
                                USDA-SCS.
            
            AUTHOR            AUTHOR
            Marty Holko, Computer Systems Analyst, Cartography and
            Geographic Information Systems Division, USDA, Soil
            Conservation Service, Washington, DC. (October l990).