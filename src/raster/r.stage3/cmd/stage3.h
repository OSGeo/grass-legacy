/*
Permission to use, copy, modify, and distribute this software and its
documentation for any purpose without fee is hereby granted. ABRFC or the
National Weather Service (NWS) makes no representations about the suitability
of this software for any purpose. It is provided "as is" without express or
implied warranty. It is provided with no support and without obligation on the
part of ABRFC or the NWS, to assist in its use, correction, modification, or
enhancement. However, if this software is used, it is requested that proper
acknowledgement be given to the ABRFC or the NWS.

SOFTWARE MODULE TO CONVERT NetCDF FILES TO GRASS RASTER MAPS
------------------------------------------------------------

r.stage3 is an initial attempt to convert NetCDF files containing rainfall 
precipitation data into GRASS raster maps.  The NetCDF files containing the 
precipitation values in the units of 1/100 of mm can be obtained from ABRFC's
gopher server. r.stage3 program generates the GRASS raster map layers only
in the Lat/Lon coordinate system. UTM is not utilized because the Area being
refrenced is too large for a UTM zone. The region settings for the ABRFC's
coverage area is:
	______________________________________________________________
	projection: 3 (Latitude-Longitude)
	zone:       0
	north:      41:16:06N
	south:      32:48:28N
	east:       90:53:28W
	west:       107:08:03W
	______________________________________________________________

The r.stage3 module can be obtained from the ABRFC's gopher server. r.stage3 
can be run only within GRASS. 

*/

typedef struct HRAP {
	int x, y;
} HRAP;

typedef struct LL {
	float lat, lon;
} LL;

extern char mapname[512];
extern char title[512];
extern double true_lat, true_lon;
