/* Benjamin Horner-Johnson, 10/06/1998
 * J Moorman, 07/23/1990
 */

#include <stdlib.h>
#include <math.h>
#include "global.h"

#define POLYFLAG1 1
#define DEG_TO_RAD (M_PI/180.0)

int add_polyline(struct dxf_file *dxf, struct Map_info *Map)
{
    int code;			/* VARIABLE THAT HOLDS VALUE RETURNED BY get_code() */
    int layer_flag = 0;		/* INDICATES IF A LAYER NAME HAS BEEN FOUND */
    int polyline_flag = 0;	/* INDICATES THE TYPE OF POLYLINE */
    int nu_layer_flag = 1;	/* INDICATES IF A nu_layer WAS FOUND */
    int warn_flag66 = 1;	/* INDICATES IF ERROR MESSAGE PRINTED ONCE */
    int warn_flag70 = 1;	/* INDICATES IF ERROR MESSAGE PRINTED ONCE */
    int vert_flag;		/* INDICATES THAT VERTICES ARE FOLLOWING */
    int xflag = 0;		/* INDICATES IF A x VALUE HAS BEEN FOUND */
    int yflag = 0;		/* INDICATES IF A y VALUE HAS BEEN FOUND */
    int zflag = 0;		/* INDICATES IF A z VALUE HAS BEEN FOUND */
    int arr_size = 0;
    int arc_arr_size = 0;
    char layername[256];
    double x1, x2, y1, y2, cent_y, cent_x, rad, beta, half_alpha;
    float ang1, ang2;
    /* variables to create arcs */
    double bulge = 0.0;		/* for arc curves */
    double prev_bulge = 0.0;	/* for arc curves */
    double arc_tan = 0.0;	/* for arc curves */

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
	case 66:		/* FLAG BIT VALUE MEANING VERTICES FOLLOW FLAG */
	    vert_flag = atoi(dxf_buf);
	    if (vert_flag != 1)	/* flag must always be 1 */
		if (warn_flag66) {
		    G_warning(_("TEXT: vertices following flag missing"));
		    warn_flag66 = 0;
		}
	    /* NOTE: WARNING PRINTED ONLY */
	    break;
	case 70:		/* POLYLINE FLAGS */
	    polyline_flag = atoi(dxf_buf);
	    /* polyline flag is 1 for closed polyline
	     * 2 curve fit vertices have been added
	     * 4 spline fit vertices have been added
	     */
	    /* NOTE: CODE ONLY EXISTS FOR FLAG = 1 (CLOSED POLYLINE) or 0 */
	    if (polyline_flag & 8 || polyline_flag & 16 || polyline_flag & 32)
		if (warn_flag70) {
		    G_warning(_("WARNING: 3-d data in dxf file"));
		    warn_flag70 = 0;
		}
	    break;

	    /* THE FOLLOWING GROUPS USED ONLY IF DIFFERENT THAN DEFAULTS */
	case 6:		/* LINETYPE NAME */
	case 38:		/* ELEVATION IF NONZERO */
	case 39:		/* THICKNESS IF NONZERO */
	case 62:		/* COLOR NUMBER (IF NOT "BYLAYER") */
	case 210:		/* X EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
	case 220:		/* Y EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */
	case 230:		/* Z EXTRUSION IF NOT PARALLEL TO THE WORLD Z AXIS */

	    /* THE FOLLOWING GROUPS ARE SPECIFIC TO POLYLINE ENTITY */
	case 41:		/* DEFAULT ENDING WIDTH */
	case 71:		/* POLYGON MESH */
	case 72:		/* POLYGON MESH */
	case 75:		/* SMOOTH SURFACE TYPE -OPTIONAL */
	    /* not used */
	default:
	    break;
	}
    }

    zpnts[0] = 0.0;
    while (strcmp(dxf_buf, "SEQEND") != 0) {	/* LOOP UNTIL SEQEND IN THE DXF FILE */
	if (feof(dxf->fp))	/* EOF */
	    return -1;
	if (strcmp(dxf_buf, "VERTEX") == 0) {
	    xflag = 0;
	    yflag = 0;
	    zflag = 0;
	    while ((code = dxf_get_code(dxf)) != 0) {
		if (code == -2)	/* EOF */
		    return -1;
		switch (code) {
		case 8:	/* LAYER NAMES ARE INCLUDED IN VERTEX ENTITY */
		    if (!layer_flag && *dxf_buf) {	/* IF NO LAYER PREVIOUSLY ASSIGNED */
			strcpy(layername, dxf_buf);
			layer_flag = 1;
		    }
		    else if (strcmp(dxf_buf, layername) != 0 &&
			     nu_layer_flag == 1) {
			G_warning(_("ERROR: layer name %s listed but not used"),
				  dxf_buf);
			nu_layer_flag = 0;	/* so ERROR only printed once */
		    }
		    break;
		case 10:	/* x COORDINATE */
		    xpnts[arr_size] = atof(dxf_buf);
		    xflag = 1;
		    break;
		case 20:	/* y COORDINATE */
		    ypnts[arr_size] = atof(dxf_buf);
		    yflag = 1;
		    break;
		case 30:	/* Z COORDINATE */
		    zpnts[arr_size] = atof(dxf_buf);
		    zflag = 1;
		    break;
		case 42:	/* bulge */
		    bulge = atof(dxf_buf);
		    break;
		case 50:	/* curve fit tangent */
		case 70:	/* vertex flags */
		    if (atoi(dxf_buf) == 16) {
			/* spline frame control point: don't draw it! */
			xflag = 0;
			yflag = 0;
			zflag = 0;
		    }
		    break;
		    /* NOTE: THERE ARE MORE CASES POSSIBLE */
		default:
		    break;
		}
	    }
	}

	if (xflag == 1 && yflag == 1) {
	    /* if prev segment is an arc  (prev_bulge != 0) prepare to make arc */
	    if (prev_bulge > 0.0)
		arc_tan = prev_bulge;
	    else if (prev_bulge < 0.0)
		arc_tan = (-1.0) * prev_bulge;

	    if (arc_tan == 0.0) {	/* straight line segment */
		check_ext(xpnts[arr_size], ypnts[arr_size]);
		if (arr_size >= ARR_MAX - 1) {
		    ARR_MAX += ARR_INCR;
		    xpnts =
			(double *)G_realloc(xpnts, ARR_MAX * sizeof(double));
		    ypnts =
			(double *)G_realloc(ypnts, ARR_MAX * sizeof(double));
		    zpnts =
			(double *)G_realloc(zpnts, ARR_MAX * sizeof(double));
		}
		arr_size++;
	    }
	    else if (!(xpnts[arr_size - 1] == xpnts[arr_size] &&
		       ypnts[arr_size - 1] == ypnts[arr_size]))
		/* make an arc */
	    {
		/* compute cent_x, cent_y, ang1, ang2 */
		if (prev_bulge > 0.0) {
		    x1 = xpnts[arr_size - 1];
		    x2 = xpnts[arr_size];
		    y1 = ypnts[arr_size - 1];
		    y2 = ypnts[arr_size];
		}
		else {
		    /* figure out how to compute the opposite center */
		    x2 = xpnts[arr_size - 1];
		    x1 = xpnts[arr_size];
		    y2 = ypnts[arr_size - 1];
		    y1 = ypnts[arr_size];
		}
		half_alpha = (double)atan(arc_tan) * 2.;
		rad = hypot(x1 - x2, y1 - y2) * .5 / sin(half_alpha);
		beta = atan2(x1 - x2, y1 - y2);
		/* now bring it into range 0 to 360 */
		beta = 90.0 * DEG_TO_RAD - beta;
		if (beta <= 0.0)
		    beta = 360.0 * DEG_TO_RAD + beta;
		/* now beta is counter clock wise from 0 (direction of (1,0)) to 360 */
		if (beta >= 0.0 && beta < 90.0) {
		    cent_x = x2 + rad * sin(half_alpha + beta);
		    cent_y = y2 - rad * cos(half_alpha + beta);
		    ang2 = (half_alpha + beta) / DEG_TO_RAD + 90.0;
		    ang1 = (beta - half_alpha) / DEG_TO_RAD + 90.0;
		}
		else if (beta >= 90.0 && beta < 180.0) {
		    beta -= 90.0;
		    cent_y = y2 + rad * sin(half_alpha + beta);
		    cent_x = x2 + rad * cos(half_alpha + beta);
		    ang2 = (half_alpha + beta) / DEG_TO_RAD + 180.0;
		    ang1 = (beta - half_alpha) / DEG_TO_RAD + 180.0;
		}
		else if (beta >= 180.0 && beta < 270.0) {
		    beta -= 180.0;
		    cent_x = x2 - rad * sin(half_alpha + beta);
		    cent_y = y2 + rad * cos(half_alpha + beta);
		    ang2 = (half_alpha + beta) / DEG_TO_RAD + 270.0;
		    ang1 = (beta - half_alpha) / DEG_TO_RAD + 270.0;
		}
		else {		/* 270 <= beta < 360 */

		    beta -= 270.0;
		    cent_y = y2 - rad * sin(half_alpha + beta);
		    cent_x = x2 - rad * cos(half_alpha + beta);
		    ang2 = (half_alpha + beta) / DEG_TO_RAD;
		    ang1 = (beta - half_alpha) / DEG_TO_RAD;
		}

		arr_size--;	/* disregard last 2 points */
		if (prev_bulge < 0.0)
		    arc_arr_size = make_arc(arr_size, cent_x, cent_y,
					    -rad, ang2, ang1, zpnts[0], 1);
		/* arc is going in clockwise direction from x2 to x1 */
		else

		    arc_arr_size = make_arc(arr_size, cent_x, cent_y,
					    rad, ang1, ang2, zpnts[0], 1);
		arr_size += arc_arr_size;
		while (arr_size >= ARR_MAX) {
		    ARR_MAX += ARR_INCR;
		    xpnts =
			(double *)G_realloc(xpnts, ARR_MAX * sizeof(double));
		    ypnts =
			(double *)G_realloc(ypnts, ARR_MAX * sizeof(double));
		    zpnts =
			(double *)G_realloc(zpnts, ARR_MAX * sizeof(double));
		}
	    }			/* arc */
	    prev_bulge = bulge;
	    arc_tan = 0.0;
	    bulge = 0.0;
	}			/* processing polyline vertex */
    }				/* vertex loop */
    /* done reading vertices */
    if (polyline_flag & POLYFLAG1) {	/* ONLY DEALING WITH polyline_flag = 1 */
	/* CHECK TO MAKE SURE VERTEX POINTS DESCRIBE A CLOSED POLYLINE */
	if (xpnts[0] != xpnts[arr_size - 1] || ypnts[0] != ypnts[arr_size - 1]) {
	    /* ADD ON THE VERTEX POINT TO COMPLETE CLOSED POLYLINE */
	    xpnts[arr_size] = xpnts[0];
	    ypnts[arr_size] = ypnts[0];
	    zpnts[arr_size] = zpnts[0];

	    /* arr_size INCREMENTED TO BE CONSISTENT WITH POLYLINE_FLAG != 1 */
	    if (arr_size >= ARR_MAX - 1) {
		ARR_MAX += ARR_INCR;
		xpnts = (double *)G_realloc(xpnts, ARR_MAX * sizeof(double));
		ypnts = (double *)G_realloc(ypnts, ARR_MAX * sizeof(double));
		zpnts = (double *)G_realloc(zpnts, ARR_MAX * sizeof(double));
	    }
	    arr_size++;
	}
    }

    if (!zflag) {
	int i;

	for (i = 0; i < arr_size; i++)
	    zpnts[i] = 0.0;
    }

    write_polylines(Map, layername, arr_size);

    return 0;
}
