

                                   APPENDIX I                    1
                              GRASS-MAPGEN User's Guide
            att.to.geo                                      att.to.geo




            
            
            NAME
            att.to.geo - Converts a GRASS attribute file to MAPGEN sites
            file.
            
            SYNOPSIS
            att.to.geo
            
            DESCRIPTION
            att.to.geo converts a GRASS attribute (dig_att) file to a
            MAPGEN sites file in latitude and longitude geographic
            coordinates.
            
            The user is required to provide the following:
            
                 - dig_att file name (usually the same name as a GRASS
                                        digit file)
            
            Completing the response sequence generates the required file
            in the sites_geo directory for constructing map symbols or
            labels with the make.symbol or make.label command.  The
            default output file name for att.to.geo command is the GRASS
            dig_att file name.
            
            NOTE
            The make.symbol or make.label command will automatically
            call the att.to.geo program if the MAPGEN sites file does
            not exist.
            
            The delete key will abort the att.to.geo command.
            
            SEE ALSO
                 points         User Manual for MAPGEN (UNIX Version),
                                USGS, p. 47.
                 make.symbol    GRASS-MAPGEN User's Guide, APPENDIX I,
                                USDA-SCS.
                 make.label     GRASS-MAPGEN User's Guide, APPENDIX I,
                                USDA-SCS.
            
            
            AUTHOR
            Marty Holko, Computer Systems Analyst, Cartography and
            Geographic Information Systems Division, USDA, Soil
            Conservation Service, Washington, DC. (October l990).
