

                                   APPENDIX I                    1
                              GRASS-MAPGEN User's Guide
            dig.to.geo                                      dig.to.geo            dig.to.geo                                      dig.to.geo




            
            
            NAME            NAME
            dig.to.geo - Converts a GRASS digit file to a MAPGEN line
            file.
            
            SYNOPSIS            SYNOPSIS
            dig.to.geo            dig.to.geo
            
            DESCRIPTION            DESCRIPTION
            dig.to.geo converts a GRASS vector (digit) file to a MAPGEN            __________                                                 
            line file in latitude and longitude geographic coordinates.
            
            The user is required to provide the following:
            
                 - dig file name
            
            Completing the response sequence generates the required file
            in the dig_geo directory for constructing map linework with                   _______                                             
            the make.line command.  The default output file name for                _________                                           
            dig.to.geo command is the GRASS vector file name.            __________                                       
            
            NOTE            NOTE
            The make.line command will automatically call the dig.to.geo                _________                                               
            program if the MAPGEN line file does not exist.
            
            The delete key will abort the dig.to.geo command.                                          __________         
            
            SEE ALSO            SEE ALSO
                 lines          User Manual for MAPGEN (UNIX Version),                 _____                                                
                                USGS, p. 37.
                 make.line      GRASS-MAPGEN User's Guide, APPENDIX I,                 _________                                            
                                USDA-SCS.
            
            AUTHOR            AUTHOR
            Marty Holko, Computer Systems Analyst, Cartography and
            Geographic Information Systems Division, USDA, Soil
            Conservation Service, Washington, DC. (October l990).