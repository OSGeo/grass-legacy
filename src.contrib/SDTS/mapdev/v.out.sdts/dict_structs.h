struct dddf  Dddf[] = {
   {0,NULL,NULL,NULL,NULL,NULL,NULL},
   {1, "ATT", "ORGANIZATION","GRASS vector header","Source responsible for digitizing map.","USACERL","US Army Construction Engineering Labs"},
   {2, "ATT", "DIGIT_DATE","GRASS vector header","Date digitized version of map created.", "USACERL","US Army Construction Engineering Labs"},
   {3, "ATT", "DIGIT_NAME","GRASS vector header","Name of individual responsible for digitizing map.","USACERL","US Army Construction Engineering Labs"},
   {4, "ATT", "MAP_NAME","GRASS vector header","Name of map.","USACERL","US Army Construction Engineering Labs"},
   {5, "ATT", "MAP_DATE","GRASS vector header","Date of original paper map. Format of item not standardized.","USACERL","US Army Construction Engineering Labs"},
   {6, "ATT", "OTHER_INFO","GRASS vector header","Comment supplied by digitizer of map.","USACERL","US Army Construction Engineering Labs"},
   {7, "ATT", "ZONE","GRASS vector header","If UTM, UPS, or State Plane, number of Zone.","USACERL","US Army Construction Engineering Labs"},
   {8, "ATT", "WEST_EDGE","GRASS vector header","Western edge of map.","USACERL","US Army Construction Engineering Labs"},
   {9, "ATT", "EAST_EDGE","GRASS vector header","Eastern edge of map.","USACERL","US Army Construction Engineering Labs"},
   {10, "ATT", "SOUTH_EDGE","GRASS vector header","Southern edge of map.","USACERL","US Army Construction Engineering Labs"},
   {11, "ATT", "NORTH_EDGE","GRASS vector header","Northern edge of map.","USACERL","US Army Construction Engineering Labs"},
   {12, "ATT", "DIGIT_THRESH","GRASS vector header","Digitizing threshold, expressed in inches. This item is currently unreliable.","USACERL","US Army Construction Engineering Labs"},
   {13, "ATT", "MAP_THRESH","GRASS vector header","Map resolution, expressed in meters. This item is currently unreliable.","USACERL","US Army Construction Engineering Labs"},
   {14, "ATT", "VERSION","GRASS vector header","Version of GRASS under which this dataset was processed.","USACERL","US Army Construction Engineering Labs"},
   {15, "ATT", "BACK_VERSION","GRASS vector header","Earliest version of GRASS compatible with the version of GRASS under which this dataset was processed.","USACERL","US Army Construction Engineering Labs"},
   {16, "ATT", "ELLIPSOID","GRASS PERMANENT mapset","Name of ellipsoid contained in GRASS PROJ_INFO file associated with this dataset.","USACERL","US Army Construction Engineering Labs"},
   {17, "ENT", "GRASS_ENT","","Exported GRASS map layer (default definition).","USACERL","US Army Construction Engineering Labs"},
   {18, "ATT", "ATTR_NUM","GRASS dig_att file","Integer number in GRASS dig_att file assigned to one or more GRASS spatial objects (default definition).","USACERL","US Army Construction Engineering Labs"},
   {19, "ATT", "ATTR_LABEL","GRASS dig_cats file","Label or description in GRASS dig_cats file associated with a particular value in the GRASS dig_atts file (default definition).","USACERL", "US Army Construction Engineering Labs"},
   { 0, NULL, NULL, NULL, NULL, NULL, NULL }

};

struct ddom Ddom[] = {
   {0,   NULL,         NULL,  NULL,     NULL,  NULL,  NULL,   NULL, NULL},
   {1, "ZONE",     "", "INTEGER",  "I", "", "MIN",  "1", "UTM Zone Number"},
   {2, "ZONE",     "", "INTEGER",  "I", "", "MAX",  "63", "UTM Zone number"},
   {3, "DIGIT_THRESH", "", "REAL", "R", "INCHES", "MIN", "0.0", "Digitizing Threshold"},
   {4, "DIGIT_THRESH", "", "REAL", "R", "INCHES", "MAX", "99.99", "Digitizing Threshold"},
   {5, "MAP_THRESH", "", "REAL", "R", "METERS", "MIN", "0.0", "Map Threshold"},
   {6, "MAP_THRESH", "", "REAL", "R", "METERS", "MAX", "99.99", "Map Threshold"},
   {7, "VERSION", "", "REAL", "R", "", "MIN", "1.0", "GRASS version under which data was processed"},
   {8, "VERSION", "", "REAL", "R", "", "MAX", "10.0", "GRASS version under which data was processed"},
   {9, "BACK_VERSION", "", "REAL", "R", "", "MIN", "1.0", "Earliest data-compatible GRASS version"},
   {10, "BACK_VERSION", "", "REAL", "R", "", "MAX", "10.0", "Earliest data-compatible GRASS version"},
   {11, "ATTR_NUM","","INTEGER", "I", "", "MIN", "[generated from data]", "Integer ID for attribute "},
   {12, "ATTR_NUM","","INTEGER",  "I", "", "MAX", "[generated from data]", "Integer ID for attribute"},
   { 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL}
};

struct ddsh Ddsh[] = {
   { 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, NULL},
   {1, "AP00", "ATPR", "", "", "ORGANIZATION", "USACERL", "A",  "", "29", "NOKEY"},
   {2, "AP00", "ATPR", "", "", "DIGIT_DATE",   "USACERL", "A",  "", "19", "NOKEY"},
   {3, "AP00", "ATPR", "", "", "DIGIT_NAME",   "USACERL", "A",  "", "19", "NOKEY"},
   {4, "AP00", "ATPR", "", "", "MAP_NAME",   "USACERL", "A",  "", "40", "NOKEY"},
   {5, "AP00", "ATPR", "", "", "MAP_DATE",   "USACERL", "A",  "", "10", "NOKEY"},
   {6, "AP00", "ATPR", "", "", "OTHER_INFO",   "USACERL", "A",  "", "[generated from data]", "NOKEY"},
   {7, "AP00", "ATPR", "", "", "ZONE",         "USACERL", "I",  "", "2",  "NOKEY"},
   {8, "AP00", "ATPR", "", "", "WEST_EDGE",    "USACERL", "R",  "", "12", "NOKEY"},
   {9, "AP00", "ATPR", "", "", "EAST_EDGE",    "USACERL", "R",  "", "12", "NOKEY"},
   {10, "AP00", "ATPR", "", "", "SOUTH_EDGE",   "USACERL", "R",  "", "12", "NOKEY"},
   {11, "AP00", "ATPR", "", "", "NORTH_EDGE",   "USACERL", "R",  "", "12", "NOKEY"},
   {12, "AP00", "ATPR", "", "", "DIGIT_THRESH", "USACERL", "R",  "INCHES", "5", "NOKEY"},
   {13, "AP00", "ATPR", "", "", "MAP_THRESH",  "USACERL", "R",  "METERS", "5", "NOKEY"},
   {14, "AP00", "ATPR", "", "", "VERSION",  "USACERL", "R",  "", "5", "NOKEY"},
   {15, "AP00", "ATPR", "", "", "BACK_VERSION",  "USACERL", "R",  "", "5", "NOKEY"},
   {16, "AP00", "ATPR", "", "", "ELLIPSOID",  "USACERL", "A",  "", "20", "NOKEY"},
   {17, "AP01", "ATPR", "[generated or user-supplied]", "USACERL", "ATTR_NUM",  "USACERL", "I", "", "", "NOKEY"},
   {18, "AP01", "ATPR", "[generated or user-supplied]", "USACERL", "ATTR_LABEL","USACERL", "A", "",  "", "NOKEY"},
   { 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, NULL}
};
