/* Dave Gerdes, US Army Construction Engineering Research Lab, 11/1989
 * Benjamin Horner-Johnson, 09/1998, 10/1998
 */

#include <stdlib.h>
#include "global.h"

int add_text(struct dxf_file *dxf, struct Map_info *Map)
{
    int code;
    int layer_flag = 0;
    int label_len = 0;
    char label[DXF_BUF_SIZE];
    int xflag = 0;
    int yflag = 0;
    double startx = 0.0;
    double starty = 0.0;
    double startz = 0.0;
    double height = 1.0;
    double angle = 0.0;
    char layer_name[DXF_BUF_SIZE];

    /*  initialize defaults */
    strcpy(layer_name, UNIDENTIFIED_LAYER);

    while ((code = dxf_get_code(dxf)) != 0) {
	if (code == -2)
	    return -1;

	switch (code) {
	case 1:		/* label value */
	    /* allow 1 character labels, label_len - 1 didn't   BCH-J */
	    label_len = strlen(dxf_buf);
	    strcpy(label, dxf_buf);
	    break;
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
	    break;
	case 10:		/* x coordinate */
	    startx = atof(dxf_buf);
	    xflag = 1;
	    break;
	case 20:		/* y coordinate */
	    starty = atof(dxf_buf);
	    yflag = 1;
	    break;
	case 30:		/* z coordinate */
	    startz = atof(dxf_buf);
	    break;
	case 40:		/* text height */
	    height = atof(dxf_buf);
	    break;
	case 50:		/* text angle */
	    angle = atof(dxf_buf);
	    break;

	case 7:		/* text style name */
	case 11:		/* alignment point */
	case 21:		/* alignment point */
	case 31:		/* alignment point */
	case 41:		/* relative x scale factor */
	case 51:		/* oblique angle */
	case 71:		/* text generation flag */
	case 72:		/* horizontal text justification type */

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

    if (label_len == 0)
	return -2;

    if (xflag && yflag) {
	/* TODO */
#if 0
	double theta, length, diag, base1, base2;

	/* now build the points of the box */
	if (ARR_MAX <= 5) {
	    ARR_MAX += ARR_INCR;
	    xpnts = (double *)G_realloc(xpnts, ARR_MAX * sizeof(double));
	    ypnts = (double *)G_realloc(ypnts, ARR_MAX * sizeof(double));
	    zpnts = (double *)G_realloc(zpnts, ARR_MAX * sizeof(double));
	}

	theta = angle * M_PI / 180.;
	length = (label_len - 1) * height;

	/* base angles for polar description of rectangle */
	base1 = M_PI / 2.;
	base2 = atan2(1., (double)(label_len - 1));
	diag = hypot(length, height);

	xpnts[0] = xpnts[4] = startx;
	ypnts[0] = ypnts[4] = starty;
	zpnts[0] = zpnts[4] = startz;

	xpnts[1] = xpnts[0] + (height * cos(theta + base1));
	ypnts[1] = ypnts[0] + (height * sin(theta + base1));
	zpnts[1] = startz;

	xpnts[2] = xpnts[0] + (diag * cos(theta + base2));
	ypnts[2] = ypnts[0] + (diag * sin(theta + base2));
	zpnts[2] = startz;

	xpnts[3] = xpnts[0] + (length * cos(theta));
	ypnts[3] = ypnts[0] + (length * sin(theta));
	zpnts[3] = startz;

	write_polyline(Map, layer_name, 5);
#endif
	xpnts[0] = startx;
	ypnts[0] = starty;
	zpnts[0] = startz;

	write_text(Map, layer_name, label);
    }

    return 0;
}
