/*======================================================================
Filename:   control_pt.c
Module:	    i.landsat.tm
Author:	    
======================================================================*/

#include "config.h"

#include <math.h>

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#include "landsat.h"
#include "ortho_image2.h"  /* same as ortho_imagery w/o including */ 
			     /* imagery.h  */	

#define X_MAG 28.5
#define Y_MAG 28.5

#define DEG_PER_RAD   57.2978
#define ROTATE         8.2           /* degrees */


/* globals */
double a, e;      /* spheroid pararmeters */
int    z;         /* UTM zone */
int    center_row, center_col;  /* quad scene center in pixs */
double center_east, center_north;
double display_rotation;
double display_skew;
double x_mag, y_mag;

/*======================================================================
Landsat Quad Scene Control Points;

======================================================================*/
void 
put_landsat_control (
    Landsat *landsat,
    TMinfo *tminfo,
    unsigned char sc_header[4321],    /* scene header record */
    unsigned char map_proj[4321]     /* map projection record */
)
{
     Control_Points_LL  cpll;
     double lat,  lon;
     double temp_east, temp_north;


     /** print some info **/
     PrintTMinfo (tminfo);

     /** zero the control points **/
     cpll.count = 0;
     cpll.e1   = NULL;
     cpll.n1   = NULL;
     cpll.lat2 = NULL;
     cpll.lon2 = NULL;
     cpll.status = NULL;


     /* get display rotation and skew from map proj record */
     display_rotation = - ( atof ( Field (map_proj, 205, 220))); 

     /** display_rotation = - ( atof ( Field (map_proj, 317, 332))); **/

     display_skew     =  - ( atof ( Field (map_proj, 77,  92)));

     /* inter pixel and interline distances from map proj record */
     x_mag  = atof ( Field (map_proj, 365, 380))  * 1000.0;
     y_mag  = atof ( Field (map_proj, 381, 396))  * 1000.0;


     /* get full scene center  UTM */
     lat = tminfo->quad_scene_geo.lat;
     lon = tminfo->quad_scene_geo.lon;
     

     center_north = atof ( Field (map_proj, 141, 156)) * 1000.0;
     center_east  = atof ( Field (map_proj, 157, 172)) * 1000.0;

 
     /* full scene center rlative to quad */
     center_row = tminfo->quad_scene_pixel.row;
     center_col = tminfo->quad_scene_pixel.col;

     
     center_row = atoi ( Field (map_proj, 413, 428)) + 
        tminfo->full_scene_pixel.row ;
     center_col = atoi ( Field (map_proj, 429, 444));
     

     /* allocate an lat/lon control point */
     /*** I_new_con_point_ll (&cpll, (double) center_col, (double) center_row,
     /***			 lon, lat, 0);
     ***/

     /* write the file */
     /** I_put_con_points_ll(landsat->group, &cpll); **/


     /** TODO -- get ellipsoid in target location */
          /* get the ellipsoid parameters */
     if (!G_get_ellipsoid_by_name ("clark66", &a, &e)) {
       /** TODO -- warning message **/
     }
     
     CC_u2ll_spheroid_parameters(a,e);
     
     /** zone **/
     z = 0;
     
     /** all this is to calc the UTM zone **/
     /** lat = tminfo->quad_scene_geo.lat;
     /** lon = tminfo->quad_scene_geo.lon;
     **/  

     lat *= 3600.0; /* convert to arc seconds for CC library */
     lon *= 3600.0; /* convert to arc seconds for CC library */
     lon = -lon;    /* CC lib expects negative in the west, which is
		       reverse from what G_scan_easting() returns */
     
     /** error = NULL; **/
     switch (CC_ll2u (lat, lon, &temp_east, &temp_north, &z))
       {
       case -1:
	 /** error = lat < 0 ? "too far south" : "too far north"; **/
	 ;
       case -2:
	 /** error = "too far from center of utm zone";**/
	 ;
       }


     /* conver the corners to lat lon */
     /* top left */
     convert_pix_to_ll  (landsat->window.west  +1,
			 landsat->window.north -1,
			 &lat,
			 &lon);

     I_new_con_point_ll (&cpll, 
			 landsat->window.west  +1, 
			 landsat->window.north -1,
			 lon, lat, 1);


     /* top right */
     convert_pix_to_ll  (landsat->window.east  -1,
			 landsat->window.north -1,
			 &lat,
			 &lon);

     I_new_con_point_ll (&cpll, 
			 landsat->window.east  -1, 
			 landsat->window.north -1,
			 lon, lat, 1);


     /* bottom left */
     convert_pix_to_ll  (landsat->window.west  +1,
			 landsat->window.south +1,
			 &lat,
			 &lon);

     I_new_con_point_ll (&cpll, 
			 landsat->window.west  +1, 
			 landsat->window.south +1,
			 lon, lat, 1);


     /* bottom right */
     convert_pix_to_ll  (landsat->window.east  -1,
			 landsat->window.south +1,
			 &lat,
			 &lon);

     I_new_con_point_ll (&cpll, 
			 landsat->window.east  -1, 
			 landsat->window.south +1,
			 lon, lat, 1);


     /* write the file */
     I_put_con_points_ll(landsat->group, &cpll);




     /* put into group control points */
}


/*======================================================================
convert quad corner pixel coords to lat lon

======================================================================*/
int 
convert_pix_to_ll (double col, double row, double *lat, double *lon)
{
double east, north;
double x_off, y_off;
double x, y;
double rot_rads;
double skew_rads;

    x_off = col - center_col;
    y_off = row - center_row;

    rot_rads  = (double) (display_rotation/DEG_PER_RAD);
    skew_rads = (double) (display_skew/DEG_PER_RAD);

    x_off = x_off + (y_off * sin(skew_rads));  

    x  =  ( x_off * cos(rot_rads)) + (y_off * sin(rot_rads));
    y  =  (-x_off * sin(rot_rads)) + (y_off * cos(rot_rads));


    east  = center_east  + (x * x_mag);
    north = center_north + (y * y_mag);


    CC_u2ll_spheroid_parameters (a,e);
    CC_u2ll_zone (z);


	if(CC_u2ll_north (north) < 0)
	  {
	    /** error = zone<0 ? "too far south" : "too far north"; **/
	    ;
	  }
	else if(CC_u2ll (east, lat, lon) < 0)
	  {
	    /** error = "too far from center of zone"; **/
	    ;
	  }

	*lat /= 3600.0; /* convert arc seconds to degrees */
	*lon /= 3600.0; /* convert arc seconds to degrees */
	*lon = - *lon;  /* CC lib expects negative in the west, which is
			  reverse from what G_scan_easting() returns */


	return 0;
}

