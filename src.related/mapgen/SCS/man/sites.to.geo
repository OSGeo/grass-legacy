

                                   APPENDIX I                    1
                              GRASS-MAPGEN User's Guide
            sites.to.geo                                    sites.to.geo

            
            
            NAME
            sites.to.geo - Converts a GRASS sites file to a MAPGEN sites
            file.
            
            SYNOPSIS 
            sites.to.geo 
            
            DESCRIPTION
            sites.to.geo converts a GRASS sites file data to a MAPGEN 
            sites file in the sites_geo directory.
            
            The user is required to provide the following:
            
                 - sites_lists file name
            
            Completing the response sequence generates the required file
            to use for constructing map symbols or labels.  The default
            file name for sites.to.geo command is the GRASS sites_lists
            file name.
            
            NOTE 
            The make.symbol or make.label command will automatically
            generate the sites.to.geo file if the file does not exist.
            
            The delete key will abort the sites.to.geo command.

            SEE ALSO            
                 points         User Manual for MAPGEN (UNIX Version),
                                USGS, p. 47.
                 make.symbol    GRASS-MAPGEN User's Guide, APPENDIX I, 
                                USDA-SCS.
                 make.label     GRASS-MAPGEN User's Guide, APPENDIX I,
                                USDA-SCS.
            
            AUTHOR            AUTHOR
            Marty Holko, Computer Systems Analyst, Cartography and
            Geographic Information Systems Division, USDA, Soil
