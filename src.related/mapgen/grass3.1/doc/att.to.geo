

                                   APPENDIX I                    1
                              GRASS-MAPGEN User's Guide
            att.to.geo                                      att.to.geo            att.to.geo                                      att.to.geo




            
            
            NAME            NAME
            att.to.geo - Converts a GRASS attribute file to MAPGEN sites
            file.
            
            SYNOPSIS            SYNOPSIS
            att.to.geo            att.to.geo
            
            DESCRIPTION            DESCRIPTION
            att.to.geo converts a GRASS attribute (dig_att) file to a            __________                                               
            MAPGEN sites file in latitude and longitude geographic
            coordinates.
            
            The user is required to provide the following:
            
                 - dig_att file name (usually the same name as a GRASS
                                        digit file)
            
            Completing the response sequence generates the required file
            in the sites_geo directory for constructing map symbols or                   _________                                          
            labels with the make.symbol or make.label command.  The                            ___________    __________              
            default output file name for att.to.geo command is the GRASS                                         __________                     
            dig_att file name.
            
            NOTE            NOTE
            The make.symbol or make.label command will automatically                ___________    __________                           
            call the att.to.geo program if the MAPGEN sites file does
            not exist.
            
            The delete key will abort the att.to.geo command.                                          __________         
            
            SEE ALSO            SEE ALSO
                 points         User Manual for MAPGEN (UNIX Version),                 ______                                               
                                USGS, p. 47.
                 make.symbol    GRASS-MAPGEN User's Guide, APPENDIX I,                 ___________                                          
                                USDA-SCS.
                 make.label     GRASS-MAPGEN User's Guide, APPENDIX I,                 __________                                           
                                USDA-SCS.
            
            
            AUTHOR            AUTHOR
            Marty Holko, Computer Systems Analyst, Cartography and
            Geographic Information Systems Division, USDA, Soil
            Conservation Service, Washington, DC. (October l990).