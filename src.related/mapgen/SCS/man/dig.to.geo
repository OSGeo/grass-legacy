

                                   APPENDIX I                    1
                              GRASS-MAPGEN User's Guide
            dig.to.geo                                      dig.to.geo




            
            
            NAME
            dig.to.geo - Converts a GRASS digit file to a MAPGEN line
            file.
            
            SYNOPSIS
            dig.to.geo
            
            DESCRIPTION
            dig.to.geo converts a GRASS vector (digit) file to a MAPGEN
            line file in latitude and longitude geographic coordinates.
            
            The user is required to provide the following:
            
                 - dig file name
            
            Completing the response sequence generates the required file
            in the dig_geo directory for constructing map linework with
            the make.line command.  The default output file name for
            dig.to.geo command is the GRASS vector file name.
            
            NOTE
            The make.line command will automatically call the dig.to.geo
            program if the MAPGEN line file does not exist.
            
            The delete key will abort the dig.to.geo command.
            
            SEE ALSO
                 lines          User Manual for MAPGEN (UNIX Version),
                                USGS, p. 37.
                 make.line      GRASS-MAPGEN User's Guide, APPENDIX I,
                                USDA-SCS.
            
            AUTHOR
            Marty Holko, Computer Systems Analyst, Cartography and
            Geographic Information Systems Division, USDA, Soil
            Conservation Service, Washington, DC. (October l990).
