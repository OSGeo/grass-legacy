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
 *
 */

 /*  Intergraph
 *  Origin point is in upper left corner.
 *  Max screens are defined in <tools.h>
 *  Function unique to IGRAPH.  Allows setting window from .grassrc
 *  variables.  Set_screen_parameters() ;
 *
 *  There are three types of virtual screens:
 *   1) has only one screen which is both text and graphics 
 *   2) has two virtual screens:
 *       virtual screen: 1 graphics and text
 *       virtual screen: 2 text only
 *   3) has two virtual screens:
 *       virtual screen: both 1 and 2 are graphics and text
 *
 *  The graphics monitor can be started from a window or an
 *  ascii terminal.
 *
 *  Functions in file:
 *   Graph_Set() -  main graphics function, sets up everything
 *
 *   Reset_events() - Resets the Environ V events to our defaults.
 *   This called by the mouse routines, who have to enable certain
 *   mouse events.
 *    
 *
 *  Written by the GRASS Team in the Winter of 88, -mh.
 *  Updated  March of 89, -mh:  to include choosing of virtual screen.
 *  Updated  June of 90, -mh:  to include choosing of virtual screen
 *    using the 'Digraph' program.
 *   Had to bring over SWITCHER.c .
 *
 */

#include	<stdio.h>
#include	<tools.h>
#include        "colors.h"
#include	"igraphics.h"


int SCREEN_LEFT	  ;
int SCREEN_RIGHT  ;
int SCREEN_BOTTOM ;
int SCREEN_TOP    ;
int SCREEN_VS    ;
int NCOLORS    ;

int I_COLOR_OFFSET ;  /*offset to skip over igraph fixed colors--dks*/

unsigned long  VSI_PLANE_MASK ;

/*  window we are working in  */
int WNO ;
int VSNO ;

/* DEBUG */
FILE *fp, *fopen() ;

char *malloc ();

static int size_changed = -1;
static short *Parray = NULL;
static long x_size, y_size;


Graph_Set() 
{

	int  vsno ;	/*  virtual screen number  */
	int  wno ;	/*  window number  */

/* DEBUG */
/**
set_debug() ;
**/


/*  Set process logo user will see */
	Set_logo ("GRASS") ;

/*  set the window from evironment variables if the env's are set  */
	Set_screen_parameters() ;

/*   perhaps set color offset here:
*     if NCOLORS is set to more than 32, then offset grass-made colors
*	   if (NCOLORS > 32)
*		  I_COLOR_OFFSET = 14;
*       else
*		  I_COLOR_OFFSET = 0;
*/

/*  Store the info to .grassrc  */
	Put_to_grass_env() ;

/*  assign to a more descriptive name  */
	VSNO = SCREEN_VS ;
/*
* Turn on the events we want, including mouse.
*/
	Reset_events() ;

/*  Create window  */
/***********
	Create_win ( VSNO, "GRASS GRAPHICS", SCREEN_LEFT, SCREEN_TOP,
		SCREEN_RIGHT, SCREEN_BOTTOM, &wno) ;
	Set_win_repaint_icon (wno, 0) ;
	Set_win_collapse_icon (wno, 0) ;

***********/

	Create_levwin_no_border ( VSNO, (int)LEVEL_NORMAL_PRIORITY, SCREEN_LEFT,
		SCREEN_TOP, SCREEN_RIGHT, SCREEN_BOTTOM, &wno) ;

/*  assign to the external variable  */
	WNO = wno ;

/*  Automatic activation is disabled - 
*   the user doesn't have to worry about the first
*   button hit on the window going to activating the window.
*/

	Set_win_auto_act (wno, 0) ;

/*  Show the window to the user  */
	Display_win (wno) ;



/*  Creates a table of color intensity */
	Init_color_lookup() ;


/*  design cursor  */
	make_cross_cursor() ;

/*  hide the cursor,  only show it when needed.  */
	/*do we really wnat to do this?"
	Hide_cursor() ;
	*/

/*  Set text size and land rotation value  */
	Text_size(25, 25) ;
	Text_rotation(0.0) ;
/*  Set font */
	init_font ("romant") ;

/*  Clear the window with a color that will contrast with the screen bkgrnd*/
		/*color(BLUE)*/
        color (get_standard_color (BLUE));
        Erase (); 

	_setsize(); /*set the size of window and Parray*/
				/*(Raster_Buffer set in Set_env.c*/

}

Reset_events()
{
	Enable_events( (int) (OUR_EVENTS) ) ;
}


/** set_debug has been commented out above in Graph_set()  */

static  int  debug_on = 0 ;
set_debug()
{
 /*
 fp = fopen("/dev/tty02", "w") ;
 fp = fopen("/tmp/graph.errors", "w") ;
 */
 fp = stderr ;
 debug_on = 1 ;
}

write_debug( s) 
	char *s ;
{
	if ( ! debug_on) return;
	fprintf( fp, "%s\n", s) ;
}

/*lifted from IRIS--at present, this is only called once, since we
   can't resize windows in the igraph driver--yet  dks*/

_setsize ()
{
    int jj;


    if (Parray == NULL)	/* first time */
    {
		x_size = SCREEN_RIGHT - SCREEN_LEFT + 1;
		y_size = SCREEN_BOTTOM - SCREEN_TOP + 1;

/*
*		reshapeviewport ();
*		getsize (&x_size, &y_size);
*		ortho2 (-.5, x_size-.5, -.5, y_size-.5);
*/
		size_changed = 1;
    }
    else 
    {
/*
*	reshapeviewport ();
*	getsize (&tmp_x, &tmp_y);
*	if (tmp_x != x_size || tmp_y != y_size)
*	{
**/
/*DEBUG*//*  fprintf (stderr, "SIZE CHANGE  %d,%d    %d,%d\n", x_size, y_size, tmp_x, tmp_y);*/
/*
*	    size_changed = 1;
*	    x_size = tmp_x;
*	    y_size = tmp_y;
*	}
*	ortho2 (-.5, tmp_x-.5, -.5, tmp_y-.5);
*/
	}

	if (size_changed)
	{
		if (Parray != NULL)
			free (Parray);
		Parray = (short *) malloc (x_size * y_size * sizeof (short));

       /*Raster_Buffer already alloted in Set_env.c*/

		if (Parray == NULL)
			fprintf (stderr, "Driver Malloc out of memory\n"), exit (0);
       /*Raster_Buffer already alloted in Set_env.c*/

/*
*   clear_all_pads ();
*	SCREEN_RIGHT = x_size - 1;
*	SCREEN_BOTTOM = y_size - 1;
*	SCREEN_LEFT	  	= 0;
*	SCREEN_TOP    	= 0;
*/
		/*clear ();*/

		size_changed = 0;
	
    }
    else
    {
		long x, y;
/*
*		getorigin (&x, &y);
*

* rect stuff MUST be inside screen *
* IRIS
*       if (x < 0 || y < 0 || 
*			(x+x_size-1) > XMAXSCREEN || (y+y_size-1) > YMAXSCREEN)
*		{
*			color (MAGENTA);
*			clear ();
*		}
*		else
*			lrectwrite (0, 0, x_size-1, y_size-1, Parray);
*/


/*DEBUG*//*      fprintf (stderr, "WINDOW %d (%d %d)  (%d %d)\n", WNO, SCREEN_LEFT, 
		SCREEN_TOP, SCREEN_RIGHT, SCREEN_BOTTOM);
		*/

		if (0 != putpixelblock16 (WNO, (int)VSI_PLANE_MASK, (short) 0, (short) 0, (short) (SCREEN_RIGHT-SCREEN_LEFT), (short) (SCREEN_BOTTOM-SCREEN_TOP), Parray))
        flushbuffer(WNO);
    }

}

_save_screen ()
{
     getpixelblock16 (WNO, (int)VSI_PLANE_MASK, 0, 0, (short) (SCREEN_RIGHT-SCREEN_LEFT), (short) (SCREEN_BOTTOM-SCREEN_TOP), Parray);
}

