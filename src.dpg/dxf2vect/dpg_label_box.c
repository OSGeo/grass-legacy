/*
**  Written by Dave Gerdes  11/1989
**  US Army Construction Engineering Research Lab
*/
#include "dxf2vect.h"
#include "math.h"

#ifndef PI
#define PI  3.141592
#endif

double atof ();

dxf_add_labelbox (dxf_file)
FILE	*dxf_file;
{
	FILE	*layer_fd, *label_fd, *dxf_which_layer();
	int	count, char_cnt, label, code;
	double  start_x, start_y, angle, theta, height, length, diag;
	double base1, base2, base3;
	char pbuf[20];

	/*  initialize defaults */
	label = -1;
	char_cnt = 0;
	layer_fd = NULL;
	start_x = 0.0;
	start_y = 0.0;
	height = 1.0;
	angle = 0.0;

	while ((code = dxf_readcode (dxf_file)) > 0)
	{
	    fgets (dxf_line, 80, dxf_file);

	    switch (code) {
		case  1: 	/* label value */
		    char_cnt = strlen (dxf_line) - 1;
		    label = atoi (dxf_line);
		    break;
		case  8:	/* layer name */
		    layer_fd = dxf_which_layer (dxf_line, DXF_ASCII);
		    label_fd = dxf_which_layer (dxf_line, DXF_LABEL);
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

/*DEBUG*/ fprintf (stderr, "TEXT got unused code %d\n", code );
		case 62:	/* Color */
		case  7:	/* Text Style */
		case  6:	/* Line type */
		default:
		    break;
	    }
	}
	if (code < 0)
	{
	    fprintf (stderr, "TEXT: Error in DXF file\n");
	    return (-1);
	}
	/* test for error */
	if (layer_fd == NULL)
	{
	    fprintf (stderr, "TEXT: No layer specified\n");
	    return (-1);
	}
	if (label == -1)
	{
	    fprintf (stderr, "TEXT: No label specified\n");
	    return (-1);
	}
	if (start_x == 0.0 || start_y == 0.0)
	{
	    fprintf (stderr, "TEXT: No x/y position specified\n");
	    return (-1);
	}

	/* now build the points of the box */
	if (5 >= arr_max)
	{
		arr_max += ARRAY_INCR;
		pt_array = (POINT *) realloc
			(pt_array, arr_max * sizeof (POINT));
	}
	arr_size = 5;

	theta = angle * PI / 180.;
	length = char_cnt * height;

	/* base angles for polar description of rectangle */
	base1 = PI/2.;
	base2 = atan2 (1., (double)char_cnt); /* == atan2 (height, length) */
	diag  = hypot (length, height);
	base3 = 0.;

	pt_array[0].x = pt_array[4].x = start_x;
	pt_array[0].y = pt_array[4].y = start_y;

	pt_array[1].x = pt_array[0].x + (height * cos (theta+base1));
	pt_array[1].y = pt_array[0].y + (height * sin (theta+base1));

	pt_array[2].x = pt_array[0].x + (diag * cos (theta+base2));
	pt_array[2].y = pt_array[0].y + (diag * sin (theta+base2));

	pt_array[3].x = pt_array[0].x + (length * cos (theta));
	pt_array[3].y = pt_array[0].y + (length * sin (theta));


	/* and finally print it out in digit format */
	fprintf (layer_fd, "L  %d\n", arr_size);
	for (count = 0; count < arr_size; count++)
	{
		/*
		fprintf (layer_fd, " %s", dig_float_point (pbuf, 12, pt_array[count].y));
		fprintf (layer_fd, " %s\n", dig_float_point (pbuf, 12, pt_array[count].x));
		*/
		fprintf (layer_fd, " %12.2lf %12.2lf\n", 
			pt_array[count].y, pt_array[count].x);
	}

	/* And add info to the label file */
	/*
	** Changed from using 3rd point to 1st point.
	**  3rd would be safer, and more in line w/ the way vect lines
	** are labelled, but this is often used for POINT labelling
	** of elevations, and the 1st point is the real location
	fprintf (label_fd, "L  %lf %lf %d\n",pt_array[2].x,pt_array[2].y,label);
	*/
	fprintf (label_fd, "L  %lf %lf %d\n",pt_array[0].x,pt_array[0].y,label);
}

/*
**  Reads next line of input file
** returns atoi of line, or  -1 if NON-numeric  or -2 on EOF
*/

dxf_readcode (dxf_file)
    FILE *dxf_file;
{
    char buf[100], *p;
    int ready = 0;

    if (NULL == fgets (buf, 80, dxf_file))
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
