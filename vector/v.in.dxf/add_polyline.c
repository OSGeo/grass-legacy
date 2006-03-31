/* Benjamin Horner-Johnson, 10/06/1998
 * J Moorman, 07/23/1990
 */

#include <stdlib.h>
#include <math.h>
#include "global.h"

int add_polyline(struct dxf_file *dxf, struct Map_info *Map)
{
    int code;
    int layer_flag = 0;		/* indicates if a layer name has been found */
    int polyline_flag = 0;	/* indicates the type of polyline */
    int nu_layer_flag = 1;	/* indicates if a nu_layer was found */
    int warn_flag66 = 1;	/* indicates if error message printed once */
    int warn_flag70 = 1;	/* indicates if error message printed once */
    int vert_flag;		/* indicates that vertices are following */
    int xflag = 0;		/* indicates if a x value has been found */
    int yflag = 0;		/* indicates if a y value has been found */
    int zflag = 0;		/* indicates if a z value has been found */
    int arr_size = 0;
    char layer_name[DXF_BUF_SIZE];
    /* variables to create arcs */
    double bulge = 0.0;		/* for arc curves */
    double prev_bulge = 0.0;	/* for arc curves */

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
	case 66:		/* vertices follow flag */
	    vert_flag = atoi(dxf_buf);
	    if (vert_flag != 1)	/* flag must always be 1 */
		if (warn_flag66) {
		    G_warning(_("TEXT: vertices following flag missing"));
		    warn_flag66 = 0;
		}
	    break;
	case 70:		/* polyline flag */
	    polyline_flag = atoi(dxf_buf);

	    /*******************************************************************
             Flag bit value                    Meaning
                    1        This is a closed Polyline (or a polygon
                             mesh closed in the M direction)
                    2        Curve-fit vertices have been added
                    4        Spline-fit vertices have been added
                    8        This is a 3D Polyline
                   16        This is a 3D polygon mesh.  Group 75 indi-
                             cates the smooth surface type, as follows:

                               0 = no smooth surface fitted
                               5 = quadratic B-spline surface
                               6 = cubic B-spline surface
                               8 = Bezier surface

                   32        The polygon mesh is closed in the N direc-
                             tion
	     ******************************************************************/
	    /* NOTE: CODE ONLY EXISTS FOR FLAG = 1 (CLOSED POLYLINE) or 0 */
	    if (polyline_flag & 8 || polyline_flag & 16 || polyline_flag & 32)
		if (warn_flag70) {
		    G_warning(_("WARNING: 3-d data in dxf file"));
		    warn_flag70 = 0;
		}
	    break;

	case 41:		/* default ending width */
	case 71:		/* polygon mesh m */
	case 72:		/* polygon mesh n */
	case 75:		/* smooth surface type */

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

    zpnts[0] = 0.0;
    /* LOOP UNTIL SEQEND IN THE DXF FILE */
    while (strcmp(dxf_buf, "SEQEND") != 0) {
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
		case 8:	/* layer name */
		    /* if no layer previously assigned */
		    if (!layer_flag && *dxf_buf) {
			if (flag_list) {
			    if (!is_layer_in_list(dxf_buf)) {
				add_layer_to_list(dxf_buf);
				fprintf(stdout, _("Layer %d %s\n"), num_layers,
					dxf_buf);
			    }
			    return 0;
			}
			/* skip if layers != NULL && (
			 * (flag_invert == 0 && is_layer_in_list == 0) ||
			 * (flag_invert == 1 && is_layer_in_list == 1)
			 * )
			 */
			if (layers && flag_invert == is_layer_in_list(dxf_buf))
			    return 0;
			strcpy(layer_name, dxf_buf);
			layer_flag = 1;
		    }
		    else if (strcmp(dxf_buf, layer_name) != 0 &&
			     nu_layer_flag == 1) {
			G_warning(_("ERROR: layer name %s listed but not used"),
				  dxf_buf);
			nu_layer_flag = 0;	/* so ERROR only printed once */
		    }
		    break;
		case 10:	/* x coordinate */
		    xpnts[arr_size] = atof(dxf_buf);
		    xflag = 1;
		    break;
		case 20:	/* y coordinate */
		    ypnts[arr_size] = atof(dxf_buf);
		    yflag = 1;
		    break;
		case 30:	/* Z coordinate */
		    zpnts[arr_size] = atof(dxf_buf);
		    zflag = 1;
		    break;
		case 40:	/* starting width */
		case 41:	/* ending width */
		case 42:	/* bulge */
		    bulge = atof(dxf_buf);
		    break;
		case 50:	/* curve fit tangent direction */
		case 70:	/* vertex flag */

	    /*******************************************************************
             Flag bit value                    Meaning
                   1         Extra vertex created by curve fitting
                   2         Curve fit tangent defined for this vertex.
                             A curve fit tangent direction of 0 may be
                             omitted from the DXF output, but is signif-
                             icant if this bit is set.
                   4         Unused (never set in DXF files)
                   8         Spline vertex created by spline fitting
                   16        Spline frame control point
                   32        3D Polyline vertex
                   64        3D polygon mesh vertex
	     ******************************************************************/
		    if (atoi(dxf_buf) == 16) {
			/* spline frame control point: don't draw it! */
			xflag = 0;
			yflag = 0;
			zflag = 0;
		    }
		    break;
		    /* NOTE: there are more cases possible */
		}
	    }
	}

	if (xflag && yflag) {
	    arr_size = make_arc_from_polyline(arr_size, bulge, prev_bulge);

	    prev_bulge = bulge;
	    bulge = 0.0;
	}			/* processing polyline vertex */
    }				/* vertex loop */
    /* done reading vertices */
    if (polyline_flag & 1) {	/* only dealing with polyline_flag = 1 */
	/* check to make sure vertex points describe a closed polyline */
	if (xpnts[0] != xpnts[arr_size - 1] || ypnts[0] != ypnts[arr_size - 1]) {
	    /* add on the vertex point to complete closed polyline */
	    xpnts[arr_size] = xpnts[0];
	    ypnts[arr_size] = ypnts[0];
	    zpnts[arr_size] = zpnts[0];

	    /* arr_size incremented to be consistent with polyline_flag != 1 */
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

    write_polyline(Map, layer_name, arr_size);

    return 0;
}
