/*
   do_zoom.c --

   This file contains routines to dump series of enlarged 
   images to ppm 
*/

/* gsf includes */

/* Nvision includes */
#include "interface.h"

/* Standard includes*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/******************************************************** */
int
Nstart_zoom_cmd (
    Nv_data *data,         /* Local data */
    Tcl_Interp *interp,    /* Current interpreter */
    int argc,              /* Number of arguments */
    char **argv           /* Argument strings */
)
{
int a, b, c, d;
int maxx, maxy;
int width, height;
int img_width, img_height;
int row, col, factor;
int XX, YY, var_i;
float aspect, varx, vary, var;
char pref[64], filename[64];


/* Parse arguments */
if (argc != 2) {
interp->result="Error: should be Nstart_zoom file_name";
return (TCL_ERROR);
}

/* Get Filename*/
strcpy(pref, argv[1]);


/* Get Current and Max Viewport coordinates */
GS_zoom_setup(&a, &b, &c, &d, &maxx, &maxy) ;

/* Calculate Maximum possible output size */
varx = ((float)maxx / (float)c);
vary = ((float)maxy / (float)d);

if (varx < vary) {
var = varx;
width = maxx;
height = (int)(d*var) + 1;
} else {
var = vary;
height = maxy;
width = (int)(c*var) + 1;
}
var_i = (int)var + 1;

fprintf(stderr, "MAX Viewport Size %d x %d\n", maxx, maxy);
fprintf(stderr, "Final Assembled Image will be %d x %d\n", width, height);
fprintf(stderr, "Assemled Image will be %f times larger\n\n", var);

/* Set X & Y to zero for lower left corner */
XX = YY = 0;
/* Set output image width & height */
img_width = c;
img_height = d;

/* Cycle through tiles according to Zoom factor */

for (row = 1; row <= var_i; row++) {
	for (col = 1; col <= var_i; col++) {
	GS_set_viewport(XX, width, YY, height);
	Ndraw_all_cmd(data, interp, argc, argv);
	sprintf(filename, "%s_%d_%d.ppm", pref, row, col);
	/* Re-set image width or height if required */
	if ( (width + XX) < c) img_width = width + XX;
	if ( (height + YY) < d) img_height = height + YY;
	/* Save tile to file */
	GS_write_zoom(filename, img_width, img_height);

	XX -= c;
	/* Done Row */
	}
	/* Reset XX and img_width */
	XX = 0;
	img_width = c;
	YY -= d;
        }

/* Done */
/* Reset viewport and draw orinanl view */
GS_set_viewport(a, c, b, d);
Ndraw_all_cmd(data, interp, argc, argv);


return (TCL_OK);
}
