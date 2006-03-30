/* Dave Gerdes, US Army Construction Engineering Research Lab, 11/1989
 * Benjamin Horner-Johnson, 09/1998, 10/1998
 */

#include <stdlib.h>
#include "global.h"

#ifdef LABEL
int add_text(struct dxf_file *dxf, struct Map_info *Map)
{
    int code;
    int char_cnt;
    char label[256];		/* same size as dxf_buf */
    double start_x, start_y, angle, theta, height, length, diag;
    double base1, base2;
    int arr_size = 0;
    char layername[256];

    /*  initialize defaults */
    strcpy(layername, UNIDENTIFIED_LAYER);

    char_cnt = 0;
    layer_fd = NULL;
    label_fd = NULL;
    start_x = 0.0;
    start_y = 0.0;
    height = 1.0;
    angle = 0.0;

    while ((code = dxf_get_code(dxf)) > 0) {
	if (code == -2)
	    return -1;

	switch (code) {
	case 1:		/* label value */
	    /* allow 1 character labels, char_cnt - 1 didn't   BCH-J */
	    char_cnt = strlen(dxf_buf);
	    strcpy(label, dxf_buf);
	    break;
	case 8:		/* layer name */
	    layer_fd = which_layer(dxf_buf, DXF_LABEL_LINE);
	    if (layer_fd == NULL)
		return 0;
	    label_fd = which_layer(dxf_buf, DXF_LABEL);
	    if (label_fd == NULL)
		return 0;
	    break;
	case 10:		/* X */
	    start_x = atof(dxf_buf);
	    break;
	case 20:		/* Y */
	    start_y = atof(dxf_buf);
	    break;
	case 40:		/* Text height */
	    height = atof(dxf_buf);
	    break;
	case 50:		/* Text angle */
	    angle = atof(dxf_buf);
	    break;

	case 30:		/* Z */
	case 41:		/* relative X scale factor */
	case 51:		/* Obliquing angle */
	case 71:		/* text generation flags */

	case 72:		/* Justification */
	case 11:		/* alignment point */
	case 21:		/* alignment point */
	case 31:		/* alignment point */
	    debug_msg("TEXT got unused code %d\n", code);
	case 62:		/* Color */
	case 7:		/* Text Style */
	case 6:		/* Line type */
	default:
	    break;
	}
    }
    if (code < 0) {
	debug_msg("TEXT: Error in DXF file\n");
	return -1;
    }


    /* else ZERO */
    /* test for error */
    if (char_cnt <= 0)
	return 2;
    if (layer_fd == NULL) {
	/* this if valid now */
	/*
	 * debug_msg("TEXT: No layer specified\n");
	 * return -1;
	 */
	return 0;
    }
    /*      if (label == 0)         Don't see why label can't be 0  BCH-J
     *      {
     *          debug_msg("TEXT: No label specified\n");
     *          return -1;
     *      }               */
    if (start_x == 0.0 || start_y == 0.0) {
	debug_msg("TEXT: No x/y position specified\n");
	return -1;
    }

    /* now build the points of the box */
    if (5 >= ARR_MAX) {
	ARR_MAX += ARR_INCR;
	xpnts = (double *)G_realloc(xpnts, ARR_MAX * sizeof(double));
	ypnts = (double *)G_realloc(ypnts, ARR_MAX * sizeof(double));
    }
    arr_size = 5;

    theta = angle * M_PI / 180.;
    length = (char_cnt - 1) * height;

    /* base angles for polar description of rectangle */
    base1 = M_PI / 2.;
    base2 = atan2(1., (double)(char_cnt - 1));	/* == atan2 (height, length) */
    diag = hypot(length, height);

    xpnts[0] = xpnts[4] = start_x;
    ypnts[0] = ypnts[4] = start_y;

    xpnts[1] = xpnts[0] + (height * cos(theta + base1));
    ypnts[1] = ypnts[0] + (height * sin(theta + base1));

    xpnts[2] = xpnts[0] + (diag * cos(theta + base2));
    ypnts[2] = ypnts[0] + (diag * sin(theta + base2));

    xpnts[3] = xpnts[0] + (length * cos(theta));
    ypnts[3] = ypnts[0] + (length * sin(theta));


    /* and finally print it out in digit format */
    /* if "-n" flag not set  [ 1998-SEP-30 BCH-J ] */

    if (!txtbox)
	write_polylines(layer_fd, arr_size);
    /* And add pnts to the label file */
    /*
     * Changed from using 3rd point to 1st point.
     * 3rd would be safer, and more in line w/ the way vect lines
     * are labelled, but this is often used for PNT labelling
     * of elevations, and the 1st point is the real location
     * fprintf (label_fd->fd, "L  %lf %lf %d\n",pt_array[2].x,pt_array[2].y,label);
     * Labels now characters, not integers  BCH-J
     */
    fprintf(label_fd->fd, "L  %f %f %s\n", xpnts[0], ypnts[0], label);
    return 0;
}
#endif
