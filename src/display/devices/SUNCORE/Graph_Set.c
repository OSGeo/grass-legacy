/*
 * Start up graphics processing.  Anything that needs to be assigned, set up,
 * started-up, or otherwise initialized happens here.  This is called only at
 * the startup of the graphics driver.
 *
 * The external variables define the pixle limits of the graphics surface.  The
 * coordinate system used by the applications programs has the (0,0) origin
 * in the upper left-hand corner.  Hence,
 *    SCREEN_LEFT < SCREEN_RIGHT
 *    SCREEN_TOP  < SCREEN_BOTTOM 
 *
 * NCOLORS is set to the total number of colors available on the device.  This
 * most certainly needs to be more than 100 (or so).  If you are writing a
 * driver with fewer colors you probably need to provide your own Color(),
 * Color_table_float(), Color_table_fixed(), and
 * Reset_color() routines (see ../lib/{Color.c,Reset_clr.c,Clr_table.c}).
 */

int SCREEN_LEFT	  ;
int SCREEN_RIGHT  ;
int SCREEN_BOTTOM ;
int SCREEN_TOP    ;
int NCOLORS = 256 ;

#define	MAIN
#include <usercore.h>
#include <signal.h>
#include "sun.h"

static int been_here_before = 0 ;
struct vwsurf *suncolor ;
int cgpixwindd() ;
struct vwsurf vwsurf_cgpix = DEFAULT_VWSURF(cgpixwindd) ; 

/* The following two lines for SUN 3/110 machines */
int cg4dd() ;
struct vwsurf vwsurf_cg   = DEFAULT_VWSURF(cg4dd) ; 

/* The following two lines for SUN 3/160 machines */
/*
int cg2dd() ;
struct vwsurf vwsurf_cg   = DEFAULT_VWSURF(cg2dd) ; 
*/

Graph_Set() 
{
	char *getenv() ;
	struct {int w,h,d; short *bits;} raster ;
	int sigint() ;

	if (getenv("WINDOW_ME"))
		suncolor = &vwsurf_cgpix ; 
	else
		suncolor = &vwsurf_cg ; 

	suncolor->cmapsize = 256 ;

	if (initialize_core(DYNAMICB,NOINPUT,TWOD))
	{
		return(1) ;
	}
	if (initialize_view_surface(suncolor,FALSE))
	{
		return(2) ;
	}
	if (select_view_surface(suncolor))
	{
		return(3) ;
	}
	set_viewport_2(0., 1., 0., .75) ;

/* Determine pixle coordinates within window */
	/* set_window() defaults to 0,1,0,.75 */
	/* allocate a raster for entire window */
	size_raster(suncolor, 0., 1., 0., .75, &raster) ;
	/* Save returned width and height information */
	SCREEN_LEFT   = 1 ;
	SCREEN_RIGHT  = raster.w ;
	SCREEN_BOTTOM = raster.h ;
	SCREEN_TOP    = 1 ;
	free(raster.bits) ;

	/* Set Normalized Device Coordinates to this window */
	set_window(
		(float)SCREEN_LEFT, 
		(float)SCREEN_RIGHT, 
		(float)SCREEN_TOP, 
		(float)SCREEN_BOTTOM ) ;
	set_output_clipping(TRUE) ;
	set_window_clipping(FALSE) ;
	set_charprecision (CHARACTER) ;

	initialize_device(LOCATOR, 1) ;
	initialize_device(BUTTON, 1) ;
	initialize_device(BUTTON, 2) ;
	initialize_device(BUTTON, 3) ;
	set_echo_surface(LOCATOR, 1, suncolor) ;

/* Set text size land rotation value */
	Text_size(25, 25) ;
	Text_rotation(0.0) ;

/* set font */
	init_font("romans") ;

/* Prepare to catch signals */
	signal (SIGHUP, sigint);
	signal (SIGINT, sigint);
	signal (SIGQUIT, sigint);
	signal (SIGILL, sigint);
	signal (SIGTSTP, SIG_IGN);

/* Note that x,y not yet known */
	sun_x = 99999999, sun_y = 99999999 ;
/* Start up first segment */
	create_temporary_segment() ;

	return(0) ;
}

static
sigint()
{
	Graph_Close() ;
	exit(-1) ;
}
