
/*
*   NOTES on Intergraph Driver.
*   This is called only at the startup of the graphics driver.
*
*  The IGRAPH driver allows setting window values from environment
*  variables.
*
*  Functions in file:
*     Set_screen_parameters() - reads the env values from .grassrc file.
*     find_primary_screen()  -  determines primary screen.
*
*  The values set in this function should match the values set
*  for the program 'Digraph'
*  ( src/D/devices/IGRAPH/igraph_progs/digraph/get_env.c )
*
*
*        VIRTUAL SCREENS (VS)
*  There are three types of virtual screen setups a machine can have:
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
*  Written by the GRASS Team in the Spring of 90, -mh.
*
*/

/*
*
*  Get window parameters from GRASS environment variables (.grassrc).
*
*  env variable IGRAPH_TOP =  SCREEN_TOP
*  env variable IGRAPH_LEFT =  SCREEN_LEFT
*  env variable IGRAPH_BOT =  SCREEN_BOTTOM
*  env variable IGRAPH_RIGHT =  SCREEN_RIGHT
*  env variable IGRAPH_VS =  SCREEN_VS
*  env variable IGRAPH_COLORS =  NCOLORS
*
*  Written by the GRASS Team in the Spring of 90, -mh.
*
*/

#include	<stdio.h>
#include	<tools.h>
#include	"igraphics.h"
#include	"gis.h"

int     IGRAPH_CLRS_SAV; /*save color range before offset subtraction */
                         /*for .grassrc file (IGRAPH_COLORS)*/
extern  int SCREEN_LEFT ;
extern  int SCREEN_RIGHT ;
extern  int SCREEN_BOTTOM ;
extern  int SCREEN_TOP ;
extern  int SCREEN_VS ;
extern  int NCOLORS ;
extern  int I_COLOR_OFFSET ;

extern  unsigned long VSI_PLANE_MASK ;

static  struct  scr_info  info[MAX_SCREENS] ;

/*Raster_Buffer is new in 91, used for Raster draw routine: 
 in Raster_def.c adopted from dpg's IRIS driver--dks*/

short *Raster_Buffer = NULL;

Set_screen_parameters() 
{

	int  max_colors ;
	int  value ;
	char  *env_value ;
	char  *getenv() ;

/*moved this over to SWITCHER.c--dks
	G_gisinit(argv[0]) ;
*/

/*  loads all of the window's info  */
	Inq_screen_info (info) ;

/*
*  Load the external variables.
*/

/*
*find_primary_screen calls Inq_screen_vs to get SCREEN_VS. to use
* this as an index to the info struct to derive scrfeen parameters
* at this point is a little dangerous,  since
* SCREEN_VS may change when IGRAPH_VS is read from .grassrc below--dks
* this is apparently benign, since colors, which can change, are assigned
* later, and other parameters are constant across vs's, but still . . .
*/

	find_primary_screen(&SCREEN_VS) ;

	SCREEN_LEFT =  1 ; /*adust_x_border*/
	SCREEN_TOP =  29 ; /*adjust y border*/

	SCREEN_BOTTOM = info[SCREEN_VS].vsi_y - 29  ; /*adjust y border*/
	SCREEN_RIGHT = info[SCREEN_VS].vsi_x - 1 ; /*adjust x border*/

	NCOLORS = info[SCREEN_VS].vsi_vlt_size ;
/*DEBUG*/
/*
*      fprintf (stderr,"virtual screen = %d\n", SCREEN_VS);
*      fprintf (stderr, "vsi_flags = %x vsi_size = %d\n", info[SCREEN_VS].vsi_flags, NCOLORS);
*      fprintf (stderr, "vsi_fixed_vlt_size = %d\n", info[SCREEN_VS].vsi_fixed_vlt_size);
*      fprintf (stderr, "vsi_fixed_vlt_start = %d\n", info[SCREEN_VS].vsi_fixed_vlt_start);
*
*      fprintf (stderr,"virtual screen = %d\n", SCREEN_VS+1);
*      fprintf (stderr, "vsi_flags = %x vsi_size = %d\n", info[SCREEN_VS+1].vsi_flags, NCOLORS);
*      fprintf (stderr, "vsi_fixed_vlt_size = %d\n", info[SCREEN_VS+1].vsi_fixed_vlt_size);
*      fprintf (stderr, "vsi_fixed_vlt_start = %d\n", info[SCREEN_VS+1].vsi_fixed_vlt_start);
*/
    if (Raster_Buffer != NULL)
        free (Raster_Buffer);
    Raster_Buffer = (short *) malloc (info[SCREEN_VS].vsi_x * sizeof (short));
    if (Raster_Buffer == NULL)
        fprintf (stderr, "Driver Malloc out of memory\n"), exit (0);


/*
*  If the user gives us reasonable values we use them else
*  we use the screen values that were already set before
*  this function was called.
*/
	if (( env_value = G__getenv ("IGRAPH_TOP")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value >= SCREEN_TOP && value <= (SCREEN_BOTTOM - 50) )
			SCREEN_TOP = value ;
	}

	if (( env_value = G__getenv ("IGRAPH_BOT")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value > (SCREEN_TOP + 50) && value <= SCREEN_BOTTOM
		&&   value > 5)
			SCREEN_BOTTOM = value ;
	}
	else
	{
		/*if IGRAPH_BOT not set, set small screen size:
		   with bottom at slightly above mid-screen*/

		SCREEN_BOTTOM = (SCREEN_BOTTOM - SCREEN_TOP)/2 - 10;
    }

	if (( env_value = G__getenv ("IGRAPH_LEFT")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value >= SCREEN_LEFT && value <= (SCREEN_RIGHT - 50) )
			SCREEN_LEFT = value ;
	}
	else
	{
		/*if IGRAPH_LEFT not set, set small screen size:
		   with left edge at slightly right of mid-screen*/
		   SCREEN_LEFT = (SCREEN_RIGHT - SCREEN_LEFT)/2 + 10;
	}

	if (( env_value = G__getenv ("IGRAPH_RIGHT")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value > (SCREEN_LEFT + 50) && value <= SCREEN_RIGHT
		&&   value > 5)
			SCREEN_RIGHT = value ;
	}

	if (( env_value = G__getenv ("IGRAPH_COLORS")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value >= MIN_IGRAPH_COLORS)
			NCOLORS = value ;
	}


/*
* Primary screen is SCREEN_VS,
* secondary screen is SCREEN_VS+1.
*/

	if (( env_value = G__getenv ("IGRAPH_VS")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value == SCREEN_VS  ||  value == (SCREEN_VS+1) )
			SCREEN_VS = value ;
	}


/*
*  SCREEN_VS is set to the vs screen they want or our default
*  which is the primary screen.
*  Maximum number of colors should be the number of possible
*  colors on the particular VS they have already chosen.
*/
	max_colors = info[SCREEN_VS].vsi_vlt_size ;

	if (max_colors > MAX_IGRAPH_COLORS)
		max_colors = MAX_IGRAPH_COLORS ;

	if (NCOLORS > max_colors)
		NCOLORS =   max_colors ;
		

/*   unless hardware with limited color capacity, set offset to skip over
**   igraph fixed colors -- dks    note that offset = 15.
**   this is because Environ V leaves a "free" slot at index 0 before
**   assigned reserved colors to slots 1-14. contiguous grass assignments must
**   start at the 16th slot (i.e, slot #15)
*/
     if (NCOLORS > FEW_COLORS)
         I_COLOR_OFFSET = 15;
     else
	 I_COLOR_OFFSET = 0;

     IGRAPH_CLRS_SAV = NCOLORS; /*save colors before offset subtraction */
				/*for writing to .grassrc */
     NCOLORS -= I_COLOR_OFFSET;

/*  This is used by Panel.c  */
	VSI_PLANE_MASK =  info[SCREEN_VS].vsi_plane_mask ;
}


/*
*
*  This function determines what the virtual screen numbers are
*  for this machine (virtual screen configuration).
*
*  An Interpro 32 has two virtual screens: 1 and 2.
*  Primary virtual screen is 1.
*
*  An InterAct 32 has two virtual screens: 2 and 3.
*  Primary virtual screen is 2.
*
*  The primary virtual screen is also the graphics screen.
*  We assume that the secondary screen virtual number will
*  one more then the primary screen virtual number.
*
*  Alogrithm:
*    Creates a window, but doesn't display it on the screen.
*    Loop through the possible virtual screen numbers 0-3,
*    try swapping the window we created to each virtual screen,
*    the first virtual screen number that the window can swap
*    to is the primary virtual screen.
*/

find_primary_screen(primary_vs_no)
	int  *primary_vs_no ;
{

	int  w_no ;
	int  vs_no ;
	int  current_vs_no ;

	Inq_displayed_vs(&current_vs_no) ;
/*DEBUG fprintf (stderr, "current vs_no = %d\n", current_vs_no);*/

	/*                             xlo,  ylo, xhi, yhi */
	Create_win( current_vs_no, "VS win ", 10, 10, 30, 30, &w_no) ;

	*primary_vs_no = -1 ;

	for ( vs_no = MIN_VS_NO; vs_no <= MAX_VS_NO; ++vs_no)
	{

		if ( ! Set_win_vs( w_no, vs_no) )
		{
			*primary_vs_no = vs_no ;
			break ;
		}

	}

	if (*primary_vs_no == -1)
		*primary_vs_no = DEFAULT_VS_NO ;

	Delete_win(w_no) ;

}

