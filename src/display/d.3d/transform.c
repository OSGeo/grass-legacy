#include <math.h>
#include "display.h"
#include "D.h"
#include "gis.h"
#include "options.h"
#include "l_proto.h"
# define RpD ((2 * 3.141593) / 360.)	/* radians/degree */

static double  rotate00, rotate01, rotate02 ;
static double  rotate10, rotate11, rotate12 ;
static double  rotate20, rotate21, rotate22 ;
static double x_orig, y_orig, z_orig ;
static double y_adj ;
static double x_adj ;
static int y_add ;
static int x_add ;

int 
establish_view (double fx, double fy, double fz, double tx, double ty, double tz, double view_angle)
{
	register double x, y, z ;
	double rx ;
	double ry ;
	double rz ;
	int t, b, l, r ;

/* Get the screen coordinates for the current window */
        D_get_screen_window(&t, &b, &l, &r) ;
	save_edges(t, b, l, r) ;

/* the functions for drawing faces use D_ functions to draw.
   it is inconsistent since the functions in clip.c use R_ calls
   instead of D_ calls. This is because the original version of
   d.3d was written before display (D_) libraries.
   So the functions in clip.c need t, b, l, r for clipping.
   That is why we call D_get_screen_window() even though
   D_set_clip_window() calls it internally to set static vars
   for D_ routines to draw and clip */

        D_set_clip_window( t, b, l, r);
/* adjust coordinates of the window to draw with D_ routines */

/* Calculate real to screen conversion factors */
	x_adj =  ((double)(r - l)/2.) / tan(view_angle * RpD / 2.0);
	y_adj =  x_adj ;
	x_add = (r + l) / 2 ;
	y_add = (t + b) / 2 ;

	x_orig = fx ;
	y_orig = fy ;
	z_orig = fz ;

/* Assume old coordinate system:
 *		x  increases up
 *		y  increases right
 *		z  increases back
 */
	y = tx - x_orig ;
	z = ty - y_orig ;
	x = tz - z_orig ;

/* Calculate the x, y, z rotation angles  */
/* Coordinate system rotated such that new y will be screen x,
 *                                     new x will be screen y,
 *                                 and new z will be screen depth.
 */
	rx = atan2(y,z) ;
	ry = -atan2(x,hypot(z,y)) ;
	rz = 0.0 ;

/* Calculate factors in the rotation matrix */
	rotate00 = cos(ry) * cos(rz);
	rotate01 = cos(rx) * -sin(rz) + sin(rx) * sin(ry) * cos(rz);
	rotate02 = -sin(rx) * -sin(rz) + cos(rx) * sin(ry) * cos(rz);
	rotate10 = cos(ry) * sin(rz);
	rotate11 = cos(rx) * cos(rz) + sin(rx) * sin(ry) * sin(rz);
	/*
	rotate12 = -sin(rx) * cos(rz) + cos(rz) * sin(ry) * sin(rz);
	*/
	rotate12 = -sin(rx) * cos(rz) + cos(rx) * sin(ry) * sin(rz);
	rotate20 = -sin(ry);
	rotate21 = sin(rx) * cos(ry);
	rotate22 = cos(rx) * cos(ry);

/*
fprintf (stdout,"orig: %10.2lf,%10.2lf,%10.2lf\n", x_orig, y_orig, z_orig) ;
fprintf (stdout,"fr: %10.1f,%10.1f,%10.1f\n", fx, fy, fz) ;
fprintf (stdout,"to: %10.1f,%10.1f,%10.1f\n", tx, ty, tz) ;
fprintf (stdout,"n_look: %10.1lf,%10.1lf,%10.1lf\n", x, y, z) ;
fprintf (stdout,"rx: %lf   ry: %lf   rz: %lf\n", rx, ry, rz) ;
*/

	return 0;
}

int 
Screen_calc (DCELL carray[], double z_adj, int scr_x[], int scr_y[], double atrow, struct Cell_head *window, int do_zero)
{
	int i ;
	DCELL *arr_ptr ;
	int *xptr ;
	int *yptr ;
	register double x, y, z ;
	double nx, ny, nz ;

	arr_ptr = carray ;
	xptr = scr_x ;
	yptr = scr_y ;

	z = window->north - atrow * window->ns_res - y_orig ;

	for(i=0; i<window->cols; i++, arr_ptr++, xptr++, yptr++)
	{
		if (!do_null && G_is_d_null_value(arr_ptr))
		{
 	 	    *xptr = 0.0 ;
		    *yptr = 0.0 ;
		}
		else
		{

		    y = window->west + (i+1) * window->ew_res - x_orig ;
                    if(G_is_d_null_value(arr_ptr)) 
		       x = - z_orig ;
                    else
		       x = *arr_ptr * z_adj - z_orig ;

	    /* calc new z (which is used for screen x) */
		    nz = x * rotate20 + y * rotate21 + z * rotate22;
	    /* calc new y (which is used for screen depth) */
		    ny = x * rotate10 + y * rotate11 + z * rotate12;
	    /* calc new x (which is used for screen y) */
		    nx = x * rotate00 + y * rotate01 + z * rotate02;

		    *xptr = x_add + (int)(x_adj * ny / nz) ;
		    *yptr = y_add - (int)(y_adj * nx / nz) ;
		}
	}

	return 0;
}

int 
point_calc (double y, double z, double x, int *scr_x, int *scr_y)
{
	double nx, ny, nz ;

	z -= y_orig ;
	y -= x_orig ;
	x -= z_orig ;

/* calc new z (which is used for screen x) */
	nz = x * rotate20 + y * rotate21 + z * rotate22;
/* calc new y (which is used for screen depth) */
	ny = x * rotate10 + y * rotate11 + z * rotate12;
/* calc new x (which is used for screen y) */
	nx = x * rotate00 + y * rotate01 + z * rotate02;

	*scr_x = x_add + (int)(x_adj * ny / nz) ;
	*scr_y = y_add - (int)(y_adj * nx / nz) ;

	return 0;
}
