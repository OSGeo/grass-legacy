/*======================================================================
Filename:   control_pt.c
Module:	    i.landsat.tm
Author:	    
======================================================================*/

#include <stdlib.h>
#include <math.h>
#include "CC.h"
#include "landsat.h"
#include "ortho_image2.h"  /* same as ortho_imagery w/o including */ 
			     /* imagery.h  */	
#include "imagelib.h"
#include "local_proto.h"

#define DEG_PER_RAD   57.2978
#define PI             3.14159
#define LINES         16.0           /* lines per Landsat TM scan */


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
     Satellite         sat_info;
     Satellite_Expose  sat_exp;

     double lat,  lon;


     /** print some info.  Commented out, clesher 21 Oct 93 **/
     /** PrintTMinfo (tminfo); **/

     /** zero the control points **/
     cpll.count = 0;
     cpll.e1   = NULL;
     cpll.n1   = NULL;
     cpll.lat2 = NULL;
     cpll.lon2 = NULL;
     cpll.status = NULL;


     /* get display rotation and skew from map proj record */
     display_rotation = ( atof ( Field (map_proj, 205, 220)));
     /** display_rotation = ( 90.0 - atof ( Field (map_proj, 525, 540))); ***/

     display_skew     =  - ( atof ( Field (map_proj, 77,  92)));

     /* inter pixel and interline distances from map proj record */
     x_mag  = atof ( Field (map_proj, 365, 380))  * 1000.0;
     y_mag  = atof ( Field (map_proj, 381, 396))  * 1000.0;


/** USE WRS easting and full scene northing **/

     /* full scene lat and lon */
     lat = tminfo->full_scene_geo.lat;
     lon = tminfo->full_scene_geo.lon;

     /* quad scene lat and lon */
/**     lat = tminfo->quad_scene_geo.lat;
/**     lon = tminfo->quad_scene_geo.lon;
**/ 

     /* full scene center rlative to quad */
     /** TODO -- what if full scene ? */
     /** TODO -- what if windowed */
     switch (tminfo->quadrant) {
     case 1:
       center_col = tminfo->SEpix.col;
       center_row = tminfo->SEpix.row;
       break;
     case 2:
       center_col = tminfo->SWpix.col - 1;
       center_row = tminfo->SWpix.row;
       break;
     case 3:
       center_col = tminfo->NEpix.col;
       center_row = tminfo->NEpix.row + 1;
       break;
     case 4:
       center_col = tminfo->NWpix.col - 1;
       center_row = tminfo->NWpix.row + 8;
       break;
     default:
       break;
     }



/**     center_col = tminfo->quad_scene_pixel.col;
/**     center_row = tminfo->quad_scene_pixel.row;
/**
/**     center_col = tminfo->SWpix.col + 
/**                  ((tminfo->NWpix.col - tminfo->SWpix.col) / 2.0) + 
/**		  ((tminfo->SEpix.col - tminfo->SWpix.col) / 2.0);  
/**
/**     center_row = 2982.0 - tminfo->quad_scene_pixel.row;
**/

     /** all this is to calc the UTM zone **/
     /** TODO -- get ellipsoid in target location */
     /* get the ellipsoid parameters */
     if (!G_get_ellipsoid_by_name ("clark66", &a, &e)) {
       /** TODO -- warning message **/
     }
     
     CC_u2ll_spheroid_parameters(a,e);
     
     /** zone **/
     z = 0;
     
     lat *= 3600.0; /* convert to arc seconds for CC library */
     lon *= 3600.0; /* convert to arc seconds for CC library */
     lon = -lon;    /* CC lib expects negative in the west, which is
		       reverse from what G_scan_easting() returns */
     
     /** error = NULL; **/
     switch (CC_ll2u (lat, lon, &center_east, &center_north, &z))
       {
       case -1:
	 /** error = lat < 0 ? "too far south" : "too far north"; **/
	 ;
       case -2:
	 /** error = "too far from center of utm zone";**/
	 ;
       }

     /* WRS scene easting */
     center_east  = atof(Field (map_proj, 125, 140)) * 1000.0;
     center_north = atof(Field (map_proj, 109, 124)) * 1000.0;


     /* conver the corners to lat lon */
     /* top left */
     convert_pix_to_ll  (landsat->window.west  +0.5,
			 landsat->window.north -0.5,
			 &lat,
			 &lon);
     I_new_con_point_ll (&cpll, 
			 landsat->window.west  +0.5, 
			 landsat->window.north -0.5,
			 lon, lat, 1);

     /* top right */
     convert_pix_to_ll  (landsat->window.east  -0.5,
			 landsat->window.north -0.5,
			 &lat,
			 &lon);
     I_new_con_point_ll (&cpll, 
			 landsat->window.east  -0.5, 
			 landsat->window.north -0.5,
			 lon, lat, 1);

     /* bottom left */
     convert_pix_to_ll  (landsat->window.west  +0.5,
			 landsat->window.south +0.5,
			 &lat,
			 &lon);
     I_new_con_point_ll (&cpll, 
			 landsat->window.west  +0.5, 
			 landsat->window.south +0.5,
			 lon, lat, 1);

     /* bottom right */
     convert_pix_to_ll  (landsat->window.east  -0.5,
			 landsat->window.south +0.5,
			 &lat,
			 &lon);
     I_new_con_point_ll (&cpll, 
			 landsat->window.east  -0.5, 
			 landsat->window.south +0.5,
			 lon, lat, 1);

     /* write the file */
     I_put_con_points_ll(landsat->group, &cpll);



     /* Satellite Info */
     
     /** TODO -- take quad into effect */
     sat_info.Xpix = tminfo->SWpix.col;
     sat_info.Ypix = tminfo->SWpix.row;


     /* top left */
     sat_info.corners[0].Xf = tminfo->NWpix.col + 0.5;
     sat_info.corners[0].Yf = tminfo->NWpix.row + 0.5;
     convert_pix_to_UTM (tminfo->NWpix.col  +0.5,
			 tminfo->NWpix.row  -0.5,
			 &sat_info.corners[0].Ef,
			 &sat_info.corners[0].Nf);

     /* top right */
     sat_info.corners[1].Xf = tminfo->NEpix.col - 0.5;
     sat_info.corners[1].Yf = tminfo->NEpix.row - 0.5;
     convert_pix_to_UTM  (tminfo->NEpix.col  -0.5,
			 tminfo->NEpix.row  -0.5,
			 &sat_info.corners[1].Ef,
			 &sat_info.corners[1].Nf);

     /* bottom left */
     sat_info.corners[2].Xf = tminfo->SWpix.col + 0.5;
     sat_info.corners[2].Yf = tminfo->SWpix.row + 0.5;
     convert_pix_to_UTM  (tminfo->SWpix.col  +0.5,
			 tminfo->SWpix.row  +0.5,
			 &sat_info.corners[2].Ef,
			 &sat_info.corners[2].Nf);

     /* bottom right */
     sat_info.corners[3].Xf = tminfo->SEpix.col + 0.5;
     sat_info.corners[3].Yf = tminfo->SEpix.row + 0.5;
     convert_pix_to_UTM  (tminfo->SEpix.col  -0.5,
			 tminfo->SEpix.row  +0.5,
			 &sat_info.corners[3].Ef,
			 &sat_info.corners[3].Nf);

     /* write the file */
     I_put_group_sat (landsat->group, &sat_info);


     /** TODO take in quad,  aslo ZC */
     /* satellite exposure */
     sat_exp.XC_init = sat_info.corners[2].Ef;
     sat_exp.YC_init = sat_info.corners[2].Nf;
     sat_exp.ZC_init = 705000.0;

     sat_exp.XC_dot = ((sat_info.corners[0].Ef - sat_info.corners[2].Ef) /
		       (sat_info.corners[0].Yf - sat_info.corners[2].Yf)) * LINES;

     sat_exp.YC_dot = ((sat_info.corners[0].Nf - sat_info.corners[2].Nf) /
		       (sat_info.corners[0].Yf - sat_info.corners[2].Yf)) * LINES ;

     sat_exp.ZC_dot = 0.0;

     sat_exp.omega_init = 0.0;
     sat_exp.phi_init   = 0.0;
     sat_exp.kappa_init = (PI / 2.0) - (display_rotation/DEG_PER_RAD);

     sat_exp.omega_dot = 0.0;
     sat_exp.phi_dot   = 0.0;
     sat_exp.kappa_dot = 0.0;
     sat_exp.status = 1;

     /* put ltm auxillary data **/
     I_put_group_satexp (landsat->group, &sat_exp);

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


    x  =  ( x_off * cos(rot_rads)) + (-y_off * sin(rot_rads));
    y  =  ( x_off * sin(rot_rads)) + ( y_off * cos(rot_rads));


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

/*======================================================================
convert quad corner pixel coords to UTM

======================================================================*/
int 
convert_pix_to_UTM (double col, double row, double *east, double *north)
{
double x_off, y_off;
double x, y;
double rot_rads;
double skew_rads;

    x_off = col - center_col;
    y_off = row - center_row;

    rot_rads  = (double) (display_rotation/DEG_PER_RAD);
    skew_rads = (double) (display_skew/DEG_PER_RAD);

    /** x_off = x_off + (y_off * sin(skew_rads));  
    **/

    x  =  ( x_off * cos(rot_rads)) + (-y_off * sin(rot_rads));
    y  =  ( x_off * sin(rot_rads)) + ( y_off * cos(rot_rads));


    *east  = center_east  + (x * x_mag);
    *north = center_north + (y * y_mag);

	return 0;
}



