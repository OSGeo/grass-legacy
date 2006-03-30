/* Benjamin Horner-Johnson, 10/06/1998
 * J Moorman, 07/23/1990
 */

#include <stdlib.h>
#include "global.h"

int add_line(struct dxf_file *dxf, struct Map_info *Map)
{
    int code;			/* VARIABLE THAT HOLDS VALUE RETURNED BY get_code() */
    int layer_flag = 0;		/* INDICATES IF A LAYER NAME HAS BEEN FOUND */
    int xflag = 0;		/* INDICATES IF A x VALUE HAS BEEN FOUND */
    int yflag = 0;		/* INDICATES IF A y VALUE HAS BEEN FOUND */
    int zflag = 0;		/* INDICATES IF A z VALUE HAS BEEN FOUND */
    int arr_size = 0;
    char layername[256];

    strcpy(layername, "UNIDENTIFIED");

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
	case 10:		/* START POINT x COORDINATE */
	    xpnts[arr_size] = atof(dxf_buf);
	    xflag = 1;
	    break;
	case 20:		/* START POINT y COORDINATE */
	    ypnts[arr_size] = atof(dxf_buf);
	    yflag = 1;
	    break;
	case 30:		/* START POINT z COORDINATE */
	    zpnts[arr_size] = atof(dxf_buf);
	    zflag = 1;
	    break;
	case 11:		/* END POINT x COORDINATE */
	    xpnts[arr_size] = atof(dxf_buf);
	    xflag = 1;
	    break;
	case 21:		/* END POINT y COORDINATE */
	    ypnts[arr_size] = atof(dxf_buf);
	    yflag = 1;
	    break;
	case 31:		/* END POINT z COORDINATE */
	    zpnts[arr_size] = atof(dxf_buf);
	    zflag = 1;
	    break;

	    /* THE FOLLOWING GROUPS USED ONLY IF DIFFERENT THAN DEFAULTS */
	case 6:		/* LINETYPE NAME */
	case 38:		/* ELEVATION IF NONZERO */
	case 39:		/* THICKNESS IF NONZERO */
	case 62:		/* COLOR NUMBER (IF NOT "BYLAYER") */
	case 210:		/* X EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
	case 220:		/* Y EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
	case 230:		/* Z EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */

	default:
	    break;
	}
	if (xflag == 1 && yflag == 1) {
	    check_ext(xpnts[arr_size], ypnts[arr_size]);
	    if (arr_size == ARR_MAX) {
		ARR_MAX += ARR_INCR;
		xpnts = (double *)G_realloc(xpnts, ARR_MAX * sizeof(double));
		ypnts = (double *)G_realloc(ypnts, ARR_MAX * sizeof(double));
		zpnts = (double *)G_realloc(zpnts, ARR_MAX * sizeof(double));
	    }
	    arr_size++;
	    xflag = 0;
	    yflag = 0;
	}
    }

    if (arr_size == 2) {	/* had both starts and stops */
	if (!zflag)
	    zpnts[0] = zpnts[1] = 0.0;
	write_polylines(Map, layername, arr_size);
    }

    return 0;
}
