/*
****************************************************************************
*
* MODULE:       d.auto.viewproj
* AUTHOR(S):    Bev Wallace, beverly.t.wallace@lmco.com
* PURPOSE:      Automatically set the region and projection parameters.
* COPYRIGHT:    (C) 2003 by Lockheed Martin Missiles & Space, Sunnyvale, CA, USA
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>	/* for rint and floor */

#include "auto.h"
#include "proj_api.h"    /* For PJ_VERSION */

/* #define DEBUG */
/* #define TEST_ALL */


/******************************************************************************
Test notes by Bev Wallace 2/18/2003
The projections were each tested with the following:
  g.region n=90 s=-90 w=90 e=450; d.erase; d.auto.viewproj
  d.mon.viewproj; d.rast.viewproj nations; 
  d.vect.viewproj coastlines color=brown; d.grid.viewproj size=10
The polar aspects were tested with region:
  g.region n=90 s=0 w=90 e=450
The oblique aspects were tested with region:
  g.region n=90 s=0 w=180 e=360
******************************************************************************/

/******************************************************************************
Test notes by Bev Wallace 6/18/2003
Modified to work with the GRASS version of PROJ.
Must set ellps for all projections because proj_def.dat is not available.
Also needed to set lat_1 and lat_2 for some projections.
******************************************************************************/


/******************************************************************************
proj_def_struct defines the proj paramters to be calculated and the region 
limitations for each projections. 

	proj_name 	Abbreviated projection name used by proj
	proj_args 	Arguments to calculate for proj
	proj_default 	Default argument for proj (one only)
	max_lat_span 	Maximum latitude span in degrees
			Centered on the equator (170 allows +85 to -85)
	max_lon_span 	Maximum longitude span in degrees
	polar_lat_limit For polar projections, use 0 if not polar
			Maximum latitude distance (0 to 90) from the pole
	full_name 	Full projection name
			Don't use spaces because of the GRASS parser
 
NOTES:
The projections must have both forward and inverse forms.
Order is not important, currently projections are in the same order as the 
PROJ manual.
If there are cart_PROJ mapping failures, reduce the region span.

The projections and regions that would properly display grid, raster, and 
vector were selected for auto_proj_def.
******************************************************************************/

proj_def_struct auto_proj_def[] = 
{
  { "merc", "lon_0", "", 		170., 359.,  0.0, CYL, "Mercator" },
  { "tmerc", "lon_0 lat_0", "", 	160.,  20.,  0.0, CYL, "TransverseMercator" },
  { "utm", "lon_0 lat_0 south", "", 	170.,   8.,  0.0, CYL, "UniversalTransverseMercator" },
  { "cc", "lon_0", "", 			170., 359.,  0.0, CYL, "CentralCylindrical" },
  { "mill", "lon_0", "", 		180., 359.,  0.0, CYL, "Miller" },
  { "cea", "lon_0", "", 		180., 359.,  0.0, CYL, "LambertCylindricalEqualArea" },
  { "cea", "lon_0", "lat_ts=30", 	180., 359.,  0.0, CYL, "Behrmann" },
  { "cea", "lon_0", "lat_ts=45", 	180., 359.,  0.0, CYL, "GallsOrthographic" },
  { "gall", "lon_0", "", 		180., 359.,  0.0, CYL, "GallStereographic" },
  { "tcea", "lon_0 lat_0", "", 		180., 179.,  0.0, CYL, "TransverseCylindricalEqualArea" },
  { "eqc",  "lon_0", "", 		180., 359.,  0.0, CYL, "EquidistantCylindrical" },
  { "eqc",  "lon_0", "lat_ts=30", 	180., 359.,  0.0, CYL, "PlateCaree" },
  { "cass", "lon_0 lat_0", "", 		170.,  20.,  0.0, CYL, "Cassini" },
  { "sinu", "lon_0", "", 		150., 120.,  0.0, PCYL, "Sinusoidal" },
  { "moll", "lon_0", "", 		150., 120.,  0.0, PCYL, "Mollweide" },
  { "robin", "lon_0", "", 		150., 120.,  0.0, PCYL, "Robinson" },
  { "eck1", "lon_0", "", 		150., 120.,  0.0, PCYL, "EckertI" },
  { "eck2", "lon_0", "", 		150., 120.,  0.0, PCYL, "EckertII" },
  { "eck3", "lon_0", "", 		150., 120.,  0.0, PCYL, "EckertIII" },
  { "eck4", "lon_0", "", 		150., 120.,  0.0, PCYL, "EckertIV" },
  { "eck5", "lon_0", "", 		150., 120.,  0.0, PCYL, "EckertV" },
  { "eck6", "lon_0", "", 		150., 120.,  0.0, PCYL, "EckertVI" },
  { "goode", "lon_0", "", 		150., 120.,  0.0, PCYL, "GoodeHomolosine" },
  { "hatano", "lon_0", "", 		170., 180.,  0.0, PCYL, "HatanoAsymmetricalEqualArea" },
  { "loxim", "lon_0 lat_1", "", 	150., 120.,  0.0, PCYL, "Loximuthal" },
  { "mbtfpp", "lon_0", "", 		170., 150.,  0.0, PCYL, "McBrydeThomasFlatPolarParabolic" },
  { "mbtfpq", "lon_0", "", 		170., 150.,  0.0, PCYL, "McBrydeThomasFlatPolarQuartic" },
  { "mbtfps", "lon_0", "", 		170., 150.,  0.0, PCYL, "McBrydeThomasFlatPolarSinusoidal" },
  { "putp2", "lon_0", "", 		150., 120.,  0.0, PCYL, "PutninsP2" },
  { "putp5", "lon_0", "", 		150., 120.,  0.0, PCYL, "PutninsP5" },
  { "wink1", "lon_0", "", 		170., 150.,  0.0, PCYL, "WinkelI" },
  { "collg", "lon_0", "", 		150.,  60.,  0.0, PCYL, "Collingnon" },
  { "crast", "lon_0", "", 		150., 120.,  0.0, PCYL, "CrasterParabolic" },
  { "lcc", "lon_0 lat_0 lat_1 lat_2", "", 160., 180.,  0.0, CONIC, "LambertConformalConic" },
  { "eqdc", "lon_0 lat_0 lat_1 lat_2", "", 160., 180.,  0.0, CONIC, "EquidistantConic" },
  { "aea", "lon_0 lat_0 lat_1 lat_2", "", 170.,  90.,  0.0, CONIC, "AlbersEqualArea" },
  { "leac", "lon_0 lat_0", "", 		180., 359.,  0.0, CONIC, "LambertEqualArea" },
  { "leac", "lon_0 lat_0", "south", 	180.,359.,  0.0, CONIC, "LambertEqualAreaSouth" },
  { "poly", "lon_0 lat_0", "", 		170., 160.,  0.0, CONIC, "PolyconicAmerican" },
  { "gnom", "lon_0", "", 		150., 150.,  0.0, AZIM, "Gnomonic" },
  { "ortho", "lon_0 lat_0", "", 	180., 360., 90.0, AZIM, "Orthographic" },
  { "nsper", "lon_0 lat_0 h=", "", 	170., 120., 59.0, AZIM, "NearSidedPerspective" },
  { "laea", "lon_0 lat_0", "", 		179.9,359.9, 0.0, AZIM, "LambertAzimuthalEqualArea" },
  { "vandg", "lon_0", "", 		180., 180.,  0.0, MISC, "VanDerGrintenI" },

#ifdef TEST_ALL

/******************************************************************************
The projections and regions that would properly display grid and vector, but 
not raster, are labeled "Big" below (for test purposes only). 
******************************************************************************/

/* Raster won't draw outide of 90 degrees from center */
  { "poly", "lon_0 lat_0", "", 	180., 359.,  0.0, CONIC, "BigPolyconicAmerican" },
/* Raster draws outside of the projection: */
  { "tmerc", "lon_0 lat_0", "", 	170., 180.,  0.0, CYL, "BigTransverseMercator" },
  { "cass", "lon_0 lat_0", "", 		160., 180.,  0.0, CYL, "BigCassini" },
  { "sinu", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigSinusoidal" },
  { "moll", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigMollweide" },
  { "robin", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigRobinson" },
  { "eck1", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigEckertI" },
  { "eck2", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigEckertII" },
  { "eck3", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigEckertIII" },
  { "eck4", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigEckertIV" },
  { "eck5", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigEckertV" },
  { "eck6", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigEckertVI" },
  { "goode", "lon_0", "", 		170., 359.,  0.0, PCYL, "BigGoodeHomolosine" },
  { "hatano", "lon_0", "", 		170., 359.,  0.0, PCYL, "BigHatanoAsymmetricalEqualArea" },
  { "loxim", "lon_0 lat_1", "", 	170., 359.,  0.0, PCYL, "BigLoximuthal" },
  { "mbtfpp", "lon_0", "", 		170., 359.,  0.0, PCYL, "BigMcBrydeThomasFlatPolarParabolic" },
  { "mbtfpq", "lon_0", "", 		170., 359.,  0.0, PCYL, "BigMcBrydeThomasFlatPolarQuartic" },
  { "mbtfps", "lon_0", "", 		170., 359.,  0.0, PCYL, "BigMcBrydeThomasFlatPolarSinusoidal" },
  { "putp2", "lon_0", "", 		170., 359.,  0.0, PCYL, "BigPutninsP2" },
  { "putp5", "lon_0", "", 		170., 359.,  0.0, PCYL, "BigPutninsP5" },
  { "qua_aut", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigQuarticAuthalic" },
  { "wink1", "lon_0", "", 		170., 359.,  0.0, PCYL, "BigWinkelI" },
  { "collg", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigCollingnon" },
  { "crast", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigCrasterParabolic" },
  { "lcc", "lon_0 lat_0 lat_1 lat_2", "", 160., 359.,  0.0, CONIC, "BigLambertConformalConic" },
  { "aea", "lon_0 lat_0 lat_1 lat_2", "", 170., 359.,  0.0, CONIC, "BigAlbersEqualArea" },
  { "eqdc", "lon_0 lat_0 lat_1 lat_2", "", 180., 359.,  0.0, CONIC, "BigEquidistantConic" },
  { "vandg", "lon_0", "", 		180., 359.,  0.0, MISC, "BigVanDerGrintenI" }, 
/* Raster draws outside of the projection when oblique (lat_0=45): */
  { "aeqd", "lon_0 lat_0", "", 		180., 360., 90.0, AZIM, "BigAzimuthalEquidistant" }, 
/* Raster did not draw anything: */
  { "boggs", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigBoggsEumorphic" },
  { "rpoly", "lon_0", "", 		180., 359.,  0.0, CONIC, "BigRectangularPolyconic" },
  { "stere", "lon_0 lat_0", "", 	180., 360., 90.0, AZIM, "BigStereographic" },
  { "nsper", "lon_0 lat_0 h=", "", 	180., 360., 59.0, AZIM, "BigNearSidedPerspective" },
  { "hammer", "lon_0", "", 		180., 359.,  0.0, AZIM, "BigHammer" },
  { "wag7", "lon_0", "", 		170., 359.,  0.0, AZIM, "BigWagnerVII" },
  { "aitoff", "lon_0", "", 		180., 359.,  0.0, AZIM, "BigAitoff" },
  { "august", "lon_0", "", 		180., 359.,  0.0, MISC, "BigAugustEpicycloidal" },
  { "bacon", "lon_0", "", 		180., 359.,  0.0, MISC, "BigBacon" },
  { "nicol", "lon_0", "", 		180., 359.,  0.0, MISC, "BigNicolosiGlobular" },
  { "apian", "lon_0", "", 		180., 359.,  0.0, MISC, "BigApianGlobularI" },
  { "ortel", "lon_0", "", 		180., 359.,  0.0, MISC, "BigOrteliusOval" },
  { "vandg2", "lon_0", "", 		180., 359.,  0.0, MISC, "BigVanDerGrintenII" },
  { "vandg3", "lon_0", "", 		180., 359.,  0.0, MISC, "BigVanDerGrintenIII" },
  { "vandg4", "lon_0", "", 		180., 359.,  0.0, MISC, "BigVanDerGrintenIV" },
  { "lagrng", "lon_0", "W=2", 		180., 359.,  0.0, MISC, "BigLagrange" },
/* Raster is different from vector */
  { "qua_aut", "lon_0", "", 		160.,  90.,  0.0, PCYL, "BigQuarticAuthalic" },
/* Raster crashes: */
  { "tcc", "lon_0", "", 		180., 160.,  0.0, CYL, "BigTransverseCentralCylindrical" }, 
  { "denoy", "lon_0", "", 		180., 359.,  0.0, PCYL, "BigDenoyerSemiElliptical" },
  { "lagrng", "lon_0", "W=2", 		180., 359.,  0.0, MISC, "BigLagrange" },
  { "airy", "lon_0", "", 		180., 180., 90.0, AZIM, "BigAiry" },

/******************************************************************************
The projections that did not work are labeled "Bad" below (for test purposes only). 
******************************************************************************/

/* Projection was not accepted by proj: */
  { "pconic", "lon_0 lat_0", "", 	180., 359.,  0.0, CONIC, "BadPerspectiveConic" },
  { "eqdc", "lon_0 lat_0", "", 	160., 180.,  0.0, CONIC, "BadEquidistantConic" },
  { "bonne", "lon_0", "", 		180., 359.,  0.0, CONIC, "BadBonne" },
/* Only drew a horizontal line: */
  { "wintri", "lon_0", "", 		170., 359.,  0.0, AZIM, "BadWinkelTripel" },

#endif /* TEST_ALL */

  { "" }
};



int auto_proj_list (char *proj_list)
{
	/* Make a comma separated list of projection names */
	/* Spaces are not allowed */
	int i=0;
	strcpy (proj_list, auto_proj_def[i].full_name);
	i++;
	while (auto_proj_def[i].proj_name[0] != '\0') {
		strcat (proj_list, ",");
		strcat (proj_list, auto_proj_def[i].full_name);
		i++;
	}

	/* Check that the string length has not been exceeded */
	if (strlen (proj_list) >= MAX_PROJ_LIST) {
		fprintf (stdout, "PROGRAMMER ERROR\n"); 
		fprintf (stdout, "increase MAX_PROJ_LIST beyond %d\n", 
			strlen (proj_list)); 
	}
	return i;
}


proj_def_struct *auto_translate_proj (char *proj_str)
{
	int i = 0;
	while (auto_proj_def[i].proj_name[0] != '\0') {
		if (! strcmp (proj_str, auto_proj_def[i].full_name))		
			return (& auto_proj_def[i]);
		i++;
	}
	return NULL;
}


int auto_proj_region (proj_def_struct *map_proj, zoom_struct *region)
{
    /* Returns 1 if the region has been adjusted */
    int adjust_flag = 0;
    double mid;
    double lat_span, lon_span;
    double lat_limit, lon_limit;
    double grass_min_span = .0001;	/* To ensure correct drawing */

    if (!map_proj || !region)
	return 0;

    /* Must round longitudes when using full span */
    lon_span = region->east - region->west;
    if (lon_span >= 360.) {
	region->west = rint (region->west);
	region->east = region->west + 360.;
    }

    /* Force East-West between +180/-180, then force east > west */
    while (region->west >= 180.) region->west -= 360.;
    while (region->west < -180.) region->west += 360.;
    while (region->east >  180.) region->east -= 360.;
    while (region->east <= region->west) 
	region->east += 360.;

#ifdef DEBUG
    fprintf (stdout, "map_proj full_name=%s proj_name=%s proj_args=%s\n", 
	map_proj->full_name, map_proj->proj_name, map_proj->proj_args); 
    fprintf (stdout, "  max_lat_span= %f max_lon_span=%f  polar_lat_limit=%f\n", 
	map_proj->max_lat_span, map_proj->max_lon_span, 
	map_proj->polar_lat_limit);
    fprintf (stdout, "requested n=%f s=%f e=%f w=%f\n", 
	region->north, region->south, 
	region->east, region->west); 
#endif /* DEBUG */

    /*** Adjust Latitudes ***/

    /* Keep North-South between latitude limits (half of the span). */
    lat_limit = map_proj->max_lat_span / 2.;
    if (region->north > lat_limit) {
	region->north = lat_limit;
	adjust_flag = 1;
    }
    if (region->south < -lat_limit) {
	region->south = -lat_limit;
	adjust_flag = 1;
    }

    /* Adjust North-South for latitude span limitations */
    lat_span = region->north - region->south;
    if (lat_span > map_proj->max_lat_span) {
	mid = (region->north + region->south) / 2.;
	region->north = mid + lat_limit;
	region->south = mid - lat_limit;
	adjust_flag = 1;
    }
    else if (lat_span < grass_min_span) {
	mid = (region->north + region->south) / 2.;
	region->north = mid + grass_min_span/2.;
	region->south = mid - grass_min_span/2.;
	adjust_flag = 1;
    }

    /*** Adjust Longitudes ***/

    /* Adjust East-West for longitude span limitations */
    lon_span = region->east - region->west;
    lon_limit = map_proj->max_lon_span / 2.;
    if (map_proj->max_lon_span <= 8.0) {
	/* Center within one zone */
	mid = (region->east + region->west) / 2.;
	mid = floor (mid / 6.0) * 6.0  + 3.0;
	if (region->west < mid - lon_limit) 
		region->west = mid - lon_limit;
	if (region->east > mid + lon_limit) 
		region->east = mid + lon_limit;
	adjust_flag = 1;
    }
    else if (lon_span > map_proj->max_lon_span) {
	mid = (region->east + region->west) / 2.;
	region->east = mid + lon_limit;
	region->west = mid - lon_limit;
	adjust_flag = 1;
    }
    else if (lon_span < grass_min_span) {
	mid = (region->east + region->west) / 2.;
	region->east = mid + grass_min_span/2.;
	region->west = mid - grass_min_span/2.;
	adjust_flag = 1;
    }

    /*** Adjust Hemisphere for Polar Projections ***/

    /* If cross the equator, limit the longitude for an equitorial view */
    /* Otherwise view the North or South pole */

    lon_span = region->east - region->west;
    if (map_proj->polar_lat_limit > 0.0 && lon_span >= 120) {
	/* Must round full projections for PROJ */
	/* Can round to to .5 degree but rounding to 1 degree */
	region->north = rint (region->north);
	region->south = rint (region->south);
	region->east = rint (region->east);
	region->west = rint (region->west);
	if (region->north > 0.0 && region->south < 0.0) { 
		/* Crosses the equator */
		mid = (region->east + region->west) / 2.;
		if (region->north != -region->south) {
			/* Limit lon_span to 120 */
			region->east = mid + 60.;
			region->west = mid - 60.;
		}
		else if (lon_span > 180.) {
			/* Limit lon_span to 180 */
			region->east = mid + 90.;
			region->west = mid - 90.;
		}
		adjust_flag = 1;
	}
	else if (lon_span >= 180. && region->north <= 0.0 &&
	    region->north > (map_proj->polar_lat_limit - 90.0)) { 
		/* Southern Hemisphere */
		region->north = map_proj->polar_lat_limit - 90.0;
		adjust_flag = 1;
	}
	else if (lon_span >= 180. && region->south >= 0.0 && 
	    region->south < (90.0 - map_proj->polar_lat_limit)) { 
		/* Northern Hemisphere */
		region->south = 90.0 - map_proj->polar_lat_limit;
		adjust_flag = 1;
	}
    }

    if (adjust_flag) {
	while (region->west >= 180.) region->west -= 360.;
	while (region->west < -180.) region->west += 360.;
	while (region->east >  180.) region->east -= 360.;
	while (region->east <= region->west) 
		region->east += 360.;
    }
#ifdef DEBUG
    fprintf (stdout, "auto_proj_region:  done\n");
    fprintf (stdout, "  n=%f s=%f e=%f w=%f\n", 
		region->north, region->south, 
		region->east, region->west); 
#endif /* DEBUG */
    return adjust_flag;
}


int auto_arg_value (char *param_ptr, int np, char *arg, double value)
{
    if (np < MAX_PROJ_PARAMS - 1) {
	sprintf (param_ptr, "%s=%f", arg, value);
	np++;
    }
    return np;
}


int auto_arg_string (char *param_ptr, int np, char *arg)
{
    if (np < MAX_PROJ_PARAMS - 1) {
	sprintf (param_ptr, "%s", arg);
	np++;
    }
    return np;
}


int auto_proj_param (proj_def_struct *map_proj, zoom_struct region,
    char param_def[MAX_PROJ_PARAMS][MAX_PROJ_PARAM_LEN])
{
    int 	np;
    double      lat_span, lon_span;
    double      lat_0, lon_0;
    double earth_radius  =  6378135.; /* earth equatorial radius (m) */

    if (!map_proj)
	return 0;

    /* Create proj argument list, start with the projection name */
    /* MAX_PROJ_PARAMS = number of possible args + 1 for NULL string */
    strcpy (param_def [0], "proj=");
    strcat (param_def [0], map_proj->proj_name);
    np = 1;

    /* Must set the ellipsoid in case proj_def.dat is not available */
    if (PJ_VERSION == 445)
 	strcpy (param_def [np], "ellps=wgs84");	// GRASS version of PROJ
    else 
	strcpy (param_def [np], "ellps=WGS84");	// External PROJ
    np++;

    lat_span = region.north - region.south;
    lon_span = region.east - region.west;

    /* Calculate lat_0, modify for polar plots so all points are visible */
    lat_0 = (region.north + region.south) / 2.0;
    if (map_proj->polar_lat_limit > 0.0 && lon_span > 180.) {
	if (region.south >= 0.0 && region.south < 30.0)
		lat_0 =  90. - region.south;
	else if (region.north <= 0.0 && region.north > -30.0)
		lat_0 = -90. - region.north;
    }

    /* Calculate lon_0 */
    lon_0 = (region.east + region.west) / 2.0;
    while (lon_0 > 180.) lon_0 -= 360.;

    /* Then add the requested parameters */

    if (strstr (map_proj->proj_args, "lon_0"))
	np = auto_arg_value (param_def[np], np, "lon_0", lon_0);

    if (strstr (map_proj->proj_args, "lat_0"))
	np = auto_arg_value (param_def[np], np, "lat_0", lat_0);

    if (strstr (map_proj->proj_args, "lat_1"))
	np = auto_arg_value (param_def[np], np, "lat_1", region.north - lat_span / 3.);

    if (strstr (map_proj->proj_args, "lat_2"))
	np = auto_arg_value (param_def[np], np, "lat_2", region.north - lat_span / 4.);

    if (strstr (map_proj->proj_args, "h="))
	np = auto_arg_value (param_def[np], np, "h", (lat_span + lon_span) * earth_radius);

    /* If south is in proj_args, only use when in the southern hemisphere */
    if (strstr (map_proj->proj_args, "south") && region.north <= 0.0)
	np = auto_arg_string (param_def[np], np, "south");

    /* Add the default arguments for this projection */
    if (map_proj->proj_default)
	np = auto_arg_string (param_def[np], np, map_proj->proj_default);

#ifdef DEBUG
    {
	int i;
	fprintf (stdout, "auto_proj_param:  ");
	for (i = 0; i < np; i++)
		fprintf (stdout, " %s", param_def[i]);
	fprintf (stdout, "\n");
    }
#endif /* DEBUG */

    return np;
}

