/* Benjamin Horner-Johnson, 10/06/1998
 * J Moorman, 07/23/1990 adapted from undxf.c code written by:
 * Tom Howard, National Park Service GIS division
 */

#include <stdlib.h>
#include "global.h"

int add_arc(struct dxf_file *dxf, struct Map_info *Map)
{
    int code;			/* VARIABLE THAT HOLDS VALUE RETURNED BY get_code() */
    int layer_flag = 0;		/* INDICATES IF A LAYER NAME HAS BEEN FOUND */
    int xflag = 0;		/* INDICATES IF A x VALUE HAS BEEN FOUND */
    int yflag = 0;		/* INDICATES IF A y VALUE HAS BEEN FOUND */
    int rflag = 0;		/* INDICATES IF A radius HAS BEEN FOUND */
    int sflag = 0;		/* INDICATES IF A start_angle HAS BEEN FOUND */
    int fflag = 0;		/* INDICATES IF A finish_angle HAS BEEN FOUND */
    double centerx = 0;		/* READ IN FROM DXF FILE */
    double centery = 0;		/* READ IN FROM DXF FILE */
    double radius = 0;		/* READ IN FROM DXF FILE */
    double zcoor = 0;		/* READ IN FROM DXF FILE */
    float start_angle = 0;	/* READ IN FROM DXF FILE */
    float finish_angle = 0;	/* READ IN FROM DXF FILE */
    int arr_size = 0;
    char layername[256];

    strcpy(layername, UNIDENTIFIED_LAYER);

    /* READS IN LINES AND PROCESSES INFORMATION UNTIL A 0 IS READ IN */
    while ((code = dxf_get_code(dxf)) != 0) {
	if (code == -2)
	    return -1;

	switch (code) {
	case 8:
	    if (!layer_flag && *dxf_buf) {
		strcpy(layername, dxf_buf);
		layer_flag = 1;
	    }
	    break;
	case 10:		/* x COORDINATE */
	    centerx = atof(dxf_buf);
	    xflag = 1;
	    break;
	case 20:		/* y COORDINATE */
	    centery = atof(dxf_buf);
	    yflag = 1;
	    break;
	case 30:		/* Z COORDINATE */
	    zcoor = atof(dxf_buf);
	    break;
	case 40:		/* RADIUS */
	    radius = atof(dxf_buf);
	    rflag = 1;
	    break;
	case 50:
	    start_angle = atof(dxf_buf);
	    sflag = 1;
	    break;
	case 51:
	    finish_angle = atof(dxf_buf);
	    fflag = 1;
	    break;

	    /* THE FOLLOWING GROUPS USED ONLY IF DIFFERENT THAN DEFAULTS */
	case 38:		/* ELEVATION IF NONZERO */
	case 39:		/* THICKNESS IF NONZERO */
	case 62:		/* COLOR NUMBER (IF NOT "BYLAYER") */
	case 210:		/* X EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
	case 220:		/* Y EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
	case 230:		/* Z EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
	default:
	    break;
	}
    }

    if (xflag && yflag && rflag && sflag && fflag) {
	arr_size =
	    make_arc(0, centerx, centery, radius, start_angle, finish_angle,
		     zcoor, 1);
	write_polylines(Map, layername, arr_size);
    }
    return 1;
}
