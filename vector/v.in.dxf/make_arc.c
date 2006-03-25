#include <math.h>
#include "global.h"

#define RSTEP 5.0

int make_arc(int offset,	/* offset into array of points */
	     double centerx, double centery,
	     double radius, double start_angle, double finish_angle,
	     double zcoor, int flag)
{
    float theta;		/* the angle used for calculating a given point */
    float alpha;		/* theta converted into radians for use in math */
    double extcirclx[4], extcircly[4];	/*to check_extents of circle */
    int i;			/* looping variable */
    int arr_size;

    arr_size = offset;
    printf
	("making arc: offset %d  x %.1f y %.1f rad %.1f a1 %.1f a2 %.1f  %d\n",
	 offset, centerx, centery, radius, start_angle, finish_angle, flag);
    if (start_angle > finish_angle)
	finish_angle = 360. + finish_angle;

    /* negative radius indicates that arc is to be drawn in a clockwise
     * direction from start_angle to finish_angle
     */
    if (radius < 0) {
	start_angle = 360. + start_angle;
	theta = start_angle;
	radius = -radius;
	while (theta > finish_angle) {
	    alpha = theta * M_PI / 180.0;	/* converting to radians */
	    xinfo[arr_size] = radius * cos(alpha) + centerx;
	    yinfo[arr_size] = radius * sin(alpha) + centery;
	    zinfo[arr_size] = zcoor;
	    /*check_ext(pt_array[arr_size].x,pt_array[arr_size].y); */
	    theta -= RSTEP;
	    if (arr_size == ARR_MAX) {
		ARR_MAX += ARR_INCR;
		xinfo = (double *)G_realloc(xinfo, ARR_MAX * sizeof(double));
		yinfo = (double *)G_realloc(yinfo, ARR_MAX * sizeof(double));
		zinfo = (double *)G_realloc(zinfo, ARR_MAX * sizeof(double));
	    }
	    arr_size++;
	}
    }
    else {
	theta = start_angle;
	while (theta < finish_angle) {	/*draw arc counterclockwise */
	    alpha = theta * M_PI / 180.0;	/* converting to radians */
	    xinfo[arr_size] = radius * cos(alpha) + centerx;
	    yinfo[arr_size] = radius * sin(alpha) + centery;
	    zinfo[arr_size] = zcoor;
	    /*check_ext(pt_array[arr_size].x,pt_array[arr_size].y); */
	    theta += RSTEP;
	    if (arr_size == ARR_MAX) {
		ARR_MAX += ARR_INCR;
		xinfo = (double *)G_realloc(xinfo, ARR_MAX * sizeof(double));
		yinfo = (double *)G_realloc(yinfo, ARR_MAX * sizeof(double));
		zinfo = (double *)G_realloc(zinfo, ARR_MAX * sizeof(double));
	    }
	    arr_size++;
	}
    }
    /* this insures that the last point will be correct */
    alpha = finish_angle * M_PI / 180.0;	/* converting to radians */
    xinfo[arr_size] = radius * cos(alpha) + centerx;
    yinfo[arr_size] = radius * sin(alpha) + centery;
    zinfo[arr_size] = zcoor;
    /*check_ext(pt_array[arr_size].x,pt_array[arr_size].y); */
    if (arr_size == ARR_MAX) {
	ARR_MAX += ARR_INCR;
	xinfo = (double *)G_realloc(xinfo, ARR_MAX * sizeof(double));
	yinfo = (double *)G_realloc(yinfo, ARR_MAX * sizeof(double));
	zinfo = (double *)G_realloc(zinfo, ARR_MAX * sizeof(double));
    }
    arr_size++;

    /* if (BOUNDARIES != 4) dpg */
    {
	/*need to check extent of plotted arcs and circles */
	if (flag)		/*for an arc */
	    for (i = offset; i < arr_size; i++)
		check_ext(xinfo[i], yinfo[i]);

	else {			/*for a circle */

	    extcirclx[0] = centerx + radius;
	    extcircly[0] = extcircly[2] = centery;

	    extcirclx[1] = extcirclx[3] = centerx;
	    extcircly[1] = centery - radius;

	    extcirclx[2] = centerx - radius;

	    extcircly[3] = centery + radius;

	    for (i = 0; i < 4; i++)
		check_ext(extcirclx[i], extcircly[i]);
	}
    }
    return arr_size - offset;
}
