

                                   APPENDIX I                    1
                              GRASS-MAPGEN User's Guide
            cat.to.geo                                      cat.to.geo




            
            
            NAME
            cat.to.geo - Converts a GRASS category file to MAPGEN sites
            file.
            
            SYNOPSIS
            cat.to.geo
            
            DESCRIPTION
            cat.to.geo converts GRASS UTM category (dig_cats) file to a
            MAPGEN sites file in latitude and longitude geographic
            coordinates.
            
            The user is required to provide the following:
            
                 - dig_cats file name (usually the same as the digit
                                         file name)
            
            Completing the response sequence generates the required file
            in the sites_geo directory for constructing map symbols or
            labels with the make.symbol or make.label command.  The
            default output file name for the cat.to.geo command is the
            GRASS dig_cats file name.
            
            NOTE
            The make.symbol or make.label command will automatically
            call the cat.to.geo program if the MAPGEN sites file does
            not exist.
            
            The delete key will abort the cat.to.geo command.
            
            SEE ALSO
                 points         User Manual for MAPGEN (UNIX Version),
                                USGS, p. 47.
                 make.symbol    GRASS-MAPGEN User's Guide, APPENDIX I,
                                USDA-SCS.
                 make.label     GRASS-MAPGEN User's Guide, APPENDIX I,
                                USDA-SCS.
                 dig_cats       GRASS-MAPGEN User's Guide, APPENDIX I-A,
                                USDA-SCS.
            
            AUTHOR
            Marty Holko, Computer Systems Analyst, Cartography and
            Geographic Information Systems Division, USDA, Soil
            Conservation Service, Washington, DC. (October l990).
