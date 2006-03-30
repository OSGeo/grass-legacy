/* Benjamin Horner-Johnson, 10/06/1998
 * J Moorman, 07/23/1990
 */

#include <stdlib.h>
#include "global.h"

int add_point(struct dxf_file *dxf, struct Map_info *Map)
{
    int code;			/* VARIABLE THAT HOLDS VALUE RETURNED BY get_code() */
    int layer_flag = 0;		/* INDICATES IF A LAYER NAME HAS BEEN FOUND */
    int xflag = 0;		/* INDICATES IF A x VALUE HAS BEEN FOUND */
    int yflag = 0;		/* INDICATES IF A y value has been found */
    char layername[256];

    /* READS IN LINES AND PROCESSES INFORMATION UNTIL A 0 IS READ IN */

    strcpy(layername, UNIDENTIFIED_LAYER);
    zpnts[0] = 0.0;

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
	    xpnts[0] = atof(dxf_buf);
	    xflag = 1;
	    break;
	case 20:		/* y COORDINATE */
	    ypnts[0] = atof(dxf_buf);
	    yflag = 1;
	    break;
	case 30:		/* Z COORDINATE */
	    zpnts[0] = atof(dxf_buf);
	    break;
	case 50:		/* ANGLE OF x AXIS FOR THE UCS IN EFFECT */

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
	if (xflag == 1 && yflag == 1) {
	    check_ext(xpnts[0], ypnts[0]);
	    xflag = 0;
	    yflag = 0;
	    xpnts[1] = xpnts[0];
	    ypnts[1] = ypnts[0];
	    zpnts[1] = zpnts[0];

	    write_point(Map, layername);

	    zpnts[0] = 0.0;
	}
    }
    return 1;
}
