/*  %W%  %G%  */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <math.h>
#include "gis.h"
# define RpD ((2 * 3.141593) / 360.)	/* radians/degree */

static double  rotate[3][3];
static double x_orig, y_orig, z_orig ;
static double y_adj ;
static double x_adj ;
static int y_add ;
static int x_add ;
static int y_size ;
static int Y_size ;

establish_view(fx,fy,fz,tx,ty,tz, view_angle)
	double fx,fy,fz,tx,ty,tz ;
	double view_angle ;
{
	register double x, y, z ;
	double atan2() ;
	double tan() ;
	double hypot() ;
	double sin() ;
	double cos() ;
	double rx ;
	double ry ;
	double rz ;
	int x1, y1;
	Window root_return;
	unsigned int width, height, border_width, depth;
	extern Window the_window;
	extern Display *the_display;

	XGetGeometry(the_display, the_window, &root_return,
			&x1, &y1, &width, &height,
			&border_width, &depth);

/* Calculate real to screen conversion factors */
	x_adj =  (width/2.) /
			tan(view_angle * RpD / 2.0);
	y_adj =  x_adj ;
	x_add = width / 2 ;
	y_add = height / 2 ;

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
	rotate[0] [0] = cos(ry) * cos(rz);
	rotate[0] [1] = cos(rx) * -sin(rz) + sin(rx) * sin(ry) * cos(rz);
	rotate[0] [2] = -sin(rx) * -sin(rz) + cos(rx) * sin(ry) * cos(rz);
	rotate[1] [0] = cos(ry) * sin(rz);
	rotate[1] [1] = cos(rx) * cos(rz) + sin(rx) * sin(ry) * sin(rz);
	rotate[1] [2] = -sin(rx) * cos(rz) + cos(rz) * sin(ry) * sin(rz);
	rotate[2] [0] = -sin(ry);
	rotate[2] [1] = sin(rx) * cos(ry);
	rotate[2] [2] = cos(rx) * cos(ry);

/*
printf("orig: %10.2lf,%10.2lf,%10.2lf\n", x_orig, y_orig, z_orig) ;
printf("fr: %10.1f,%10.1f,%10.1f\n", fx, fy, fz) ;
printf("to: %10.1f,%10.1f,%10.1f\n", tx, ty, tz) ;
printf("n_look: %10.1lf,%10.1lf,%10.1lf\n", x, y, z) ;
printf("rx: %lf   ry: %lf   rz: %lf\n", rx, ry, rz) ;
*/
}

Screen_calc(carray, z_adj, scr_x, scr_y, atrow, window)
	CELL carray[] ;
	double z_adj ;
	int scr_x[] ;
	int scr_y[] ;
	int atrow ;
	struct Cell_head *window ;
{
	int i ;
	CELL *arr_ptr ;
	int *xptr ;
	int *yptr ;
	int d ;
	double x, y, z ;
	double nx, ny, nz ;

	arr_ptr = carray ;
	xptr = scr_x ;
	yptr = scr_y ;

	z = window->north - atrow * window->ns_res - y_orig ;

	for(i=0; i<window->cols; i++, arr_ptr++, xptr++, yptr++)
	{
		y = window->west + i * window->ew_res - x_orig ;
		x = *arr_ptr * z_adj - z_orig ;

	/* calc new z (which is used for screen x) */
		nz = x * rotate[2][0] + y * rotate[2][1] + z * rotate[2][2];
	/* calc new y (which is used for screen depth) */
		ny = x * rotate[1][0] + y * rotate[1][1] + z * rotate[1][2];
	/* calc new x (which is used for screen y) */
		nx = x * rotate[0][0] + y * rotate[0][1] + z * rotate[0][2];

		*xptr = x_add + (int)(x_adj * ny / nz) ;
		*yptr = y_add - (int)(y_adj * nx / nz) ;
	}
}
