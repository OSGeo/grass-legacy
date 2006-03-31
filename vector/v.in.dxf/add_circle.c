/* Benjamin Horner-Johnson, 10/06/1998
 * J Moorman, 07/23/1990 adapted from undxf.c code written by:
 * Tom Howard, National Park Service GIS division
 */

#include <stdlib.h>
#include "global.h"

int add_circle(struct dxf_file *dxf, struct Map_info *Map)
{
    int code;
    int layer_flag = 0;		/* indicates if a layer name has been found */
    int xflag = 0;		/* indicates if a x value has been found */
    int yflag = 0;		/* indicates if a y value has been found */
    int rflag = 0;		/* indicates if a radius has been found */
    double centerx = 0;		/* read in from dxf file */
    double centery = 0;		/* read in from dxf file */
    double radius = 0;		/* read in from dxf file */
    double zcoor = 0;		/* read in from dxf file */
    int arr_size = 0;
    char layer_name[256];

    strcpy(layer_name, UNIDENTIFIED_LAYER);

    /* READS IN LINES AND PROCESSES INFORMATION UNTIL A 0 IS READ IN */
    while ((code = dxf_get_code(dxf)) != 0) {
	if (code == -2)
	    return -1;

	switch (code) {
	case 8:		/* layer name */
	    if (!layer_flag && *dxf_buf) {
		strcpy(layer_name, dxf_buf);
		layer_flag = 1;
	    }
	    break;
	case 10:		/* x coordinate */
	    centerx = atof(dxf_buf);
	    xflag = 1;
	    break;
	case 20:		/* y coordinate */
	    centery = atof(dxf_buf);
	    yflag = 1;
	    break;
	case 30:		/* z coordinate */
	    zcoor = atof(dxf_buf);
	    break;
	case 40:		/* radius */
	    radius = atof(dxf_buf);
	    rflag = 1;
	    break;

	    /* THE FOLLOWING GROUPS USED ONLY IF DIFFERENT THAN DEFAULTS */
	case 6:		/* linetype name */
	case 38:		/* elevation if nonzero */
	case 39:		/* thickness if nonzero */
	case 62:		/* color number (if not "BYLAYER") */
	case 210:		/* x extrusion if not parallel to the world z axis */
	case 220:		/* y extrusion if not parallel to the world z axis */
	case 230:		/* z extrusion if not parallel to the world z axis */
	    break;
	}
    }

    if (xflag && yflag && rflag) {
	arr_size = make_arc(0, centerx, centery, radius, 0.0, 360.0, zcoor, 0);
	write_polyline(Map, layer_name, arr_size);
    }
    return 1;
}
