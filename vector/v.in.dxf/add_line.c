/* Benjamin Horner-Johnson, 10/06/1998
 * J Moorman, 07/23/1990
 */

#include <stdlib.h>
#include "global.h"

int add_line(struct dxf_file *dxf, struct Map_info *Map)
{
    int code;
    int layer_flag = 0;		/* indicates if a layer name has been found */
    int xflag = 0;		/* indicates if a x value has been found */
    int yflag = 0;		/* indicates if a y value has been found */
    int zflag = 0;		/* indicates if a z value has been found */
    int arr_size = 0;
    char layer_name[DXF_BUF_SIZE];

    strcpy(layer_name, UNIDENTIFIED_LAYER);

    /* READS IN LINES AND PROCESSES INFORMATION UNTIL A 0 IS READ IN */
    while ((code = dxf_get_code(dxf)) != 0) {
	if (code == -2)
	    return -1;

	switch (code) {
	case 8:		/* layer name */
	    if (!layer_flag && *dxf_buf) {
		if (flag_list) {
		    if (!is_layer_in_list(dxf_buf)) {
			add_layer_to_list(dxf_buf);
			fprintf(stdout, _("Layer %d %s\n"), num_layers,
				dxf_buf);
		    }
		    return 0;
		}
		if (layers && !is_layer_in_list(dxf_buf))
		    return 0;
		strcpy(layer_name, dxf_buf);
		layer_flag = 1;
	    }
	    break;
	case 10:		/* start point x coordinate */
	    xpnts[arr_size] = atof(dxf_buf);
	    xflag = 1;
	    break;
	case 11:		/* end point x coordinate */
	    xpnts[arr_size] = atof(dxf_buf);
	    xflag = 1;
	    break;
	case 20:		/* start point y coordinate */
	    ypnts[arr_size] = atof(dxf_buf);
	    yflag = 1;
	    break;
	case 21:		/* end point y coordinate */
	    ypnts[arr_size] = atof(dxf_buf);
	    yflag = 1;
	    break;
	case 30:		/* start point z coordinate */
	    zpnts[arr_size] = atof(dxf_buf);
	    zflag = 1;
	    break;
	case 31:		/* end point z coordinate */
	    zpnts[arr_size] = atof(dxf_buf);
	    zflag = 1;
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

	if (xflag && yflag) {
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
	write_polyline(Map, layer_name, arr_size);
    }

    return 0;
}
