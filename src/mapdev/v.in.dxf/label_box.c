/*
**  Written by Dave Gerdes  11/1989
**  US Army Construction Engineering Research Lab
*/
#include "dxf2vect.h"
#include <math.h>
#include <stdlib.h>
#include "Vect.h"

#ifndef PI
#define PI  3.141592
#endif

int dxf_add_labelbox (FILE *dxf_file)
{
	DXF_DIG	*layer_fd, *label_fd = NULL;
	int	count, char_cnt, label, code;
	double  start_x, start_y, angle, theta, height, length, diag;
	double base1, base2;
	int arr_size = 0;

	/*  initialize defaults */
	label = 0;
	char_cnt = 0;
	layer_fd = NULL;
	start_x = 0.0;
	start_y = 0.0;
	height = 1.0;
	angle = 0.0;

	while ((code = dxf_readcode (dxf_file)) > 0)
	{
	    dxf_fgets (dxf_line, 80, dxf_file);

	    switch (code) {
		case  1: 	/* label value */
		    char_cnt = strlen (dxf_line) - 1;
		    label = atoi (dxf_line);
		    break;
		case  8:	/* layer name */
		    layer_fd = dxf_which_layer (dxf_line, DXF_LABEL_LINE);
		    if (layer_fd == NULL)
			return(0);
		    label_fd = dxf_which_layer (dxf_line, DXF_LABEL);
		    if (label_fd == NULL)
			return(0);
		    break;
		case 10:	/* X */
		    start_x = atof (dxf_line);
		    break;
		case 20:	/* Y */
		    start_y = atof (dxf_line);
		    break;
		case 40:	/* Text height */
		    height = atof (dxf_line);
		    break;
		case 50:	/* Text angle */
		    angle = atof (dxf_line);
		    break;

		case 30:	/* Z */
		case 41:	/* relative X scale factor */
		case 51:	/* Obliquing angle */
		case 71:	/* text generation flags */

		case 72:	/* Justification */
		case 11:	/* alignment point */
		case 21:	/* alignment point */
		case 31:	/* alignment point */

		debugf("TEXT got unused code %d\n", code );
		case 62:	/* Color */
		case  7:	/* Text Style */
		case  6:	/* Line type */
		default:
		    break;
	    }
	}
	if (code < 0)
	{
	    debugf("TEXT: Error in DXF file\n");

	    return (-1);
	}


	/* else ZERO */
	/* test for error */
	if (char_cnt <= 0)
	    return 2;
	if (layer_fd == NULL)
	{
	    /* this if valid now */
	    /*
	    debugf("TEXT: No layer specified\n");
	    return (-1);
	    */
	    return (0);
	}
	if (label == 0)
	{
	    debugf("TEXT: No label specified\n");
	    return (-1);
	}
	if (start_x == 0.0 || start_y == 0.0)
	{
	    debugf("TEXT: No x/y position specified\n");
	    return (-1);
	}

	/* now build the points of the box */
	if (5 >= ARR_MAX)
	{
		ARR_MAX+= ARR_INCR;
		xinfo = (double *) G_realloc (xinfo, ARR_MAX * sizeof (double));
		yinfo = (double *) G_realloc (yinfo, ARR_MAX * sizeof (double));
	}
	arr_size = 5;

	theta = angle * PI / 180.;
	length = char_cnt * height;

	/* base angles for polar description of rectangle */
	base1 = PI/2.;
	base2 = atan2 (1., (double)char_cnt); /* == atan2 (height, length) */
	diag  = hypot (length, height);

	xinfo[0] = xinfo[4] = start_x;
	yinfo[0] = yinfo[4] = start_y;

	xinfo[1] = xinfo[0] + (height * cos (theta+base1));
	yinfo[1] = yinfo[0] + (height * sin (theta+base1));

	xinfo[2] = xinfo[0] + (diag * cos (theta+base2));
	yinfo[2] = yinfo[0] + (diag * sin (theta+base2));

	xinfo[3] = xinfo[0] + (length * cos (theta));
	yinfo[3] = yinfo[0] + (length * sin (theta));


	/* and finally print it out in digit format */
	
	/* FOR BINARY FILES */
	if(!ascii_flag->answer)
	{
	    Vect_copy_xy_to_pnts (Points, xinfo, yinfo, arr_size);
	    Vect_write_line(layer_fd->Map,LINE,Points);
	}

	/* OR FOR ASCII FILE */
	else
	{
		fprintf (layer_fd->fd, "L  %d\n", arr_size);
		for (count = 0; count < arr_size; count++)
			fprintf (layer_fd->fd," %12.2f %12.2f\n", yinfo[count], xinfo[count]);
	}
	/* And add info to the label file */
	/*
	** Changed from using 3rd point to 1st point.
	**  3rd would be safer, and more in line w/ the way vect lines
	** are labelled, but this is often used for PNT labelling
	** of elevations, and the 1st point is the real location
	fprintf (label_fd->fd, "L  %f %f %d\n",pt_array[2].x,pt_array[2].y,label);
	*/
    fprintf (label_fd->fd, "L  %f %f %d\n",xinfo[0],yinfo[0],label);
    return (0);
}

/*
**  Reads next line of input file
** returns atoi of line, or  -1 if NON-numeric  or -2 on EOF
*/

int 
dxf_readcode (FILE *dxf_file)
{
    char buf[100], *p;
    int ready = 0;

    if (NULL == dxf_fgets (buf, 80, dxf_file))
	return (-2);
    for (p = buf ; *p ; p++)
    {
	if (*p != ' ' && *p != '\t')
	    ready = 1;
	if (ready)
	    if ('0' <= *p && *p <= '9')
		return (atoi (buf));
	    else
		return (-1);	/* NOT NUMERIC */
    }
    return (-1);	/* NOT NUMERIC */
}
