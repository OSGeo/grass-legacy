/* Benjamin Horner-Johnson, 10/06/1998
 * J Moorman, 07/23/1990
 */

#include <stdlib.h>
#include "global.h"

int add_point(struct dxf_file *dxf, struct Map_info *Map)
{
    int code;
    int layer_flag = 0;		/* indicates if a layer name has been found */
    int xflag = 0;		/* indicates if a x value has been found */
    int yflag = 0;		/* indicates if a y value has been found */
    char layer_name[256];

    /* READS IN LINES AND PROCESSES INFORMATION UNTIL A 0 IS READ IN */
    strcpy(layer_name, UNIDENTIFIED_LAYER);
    zpnts[0] = 0.0;

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
	    xpnts[0] = atof(dxf_buf);
	    xflag = 1;
	    break;
	case 20:		/* y coordinate */
	    ypnts[0] = atof(dxf_buf);
	    yflag = 1;
	    break;
	case 30:		/* Z coordinate */
	    zpnts[0] = atof(dxf_buf);
	    break;
	case 50:		/* angle of x axis for the UCS in effect */

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

    if (xflag == 1 && yflag == 1) {
	check_ext(xpnts[0], ypnts[0]);
	xpnts[1] = xpnts[0];
	ypnts[1] = ypnts[0];
	zpnts[1] = zpnts[0];

	write_point(Map, layer_name);
    }

    return 1;
}
