
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
*  env variable IGRAPH_TOP =  SCREEN_BOTTOM
*  env variable IGRAPH_LEFT =  SCREEN_RIGHT
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

extern  int SCREEN_LEFT ;
extern  int SCREEN_RIGHT ;
extern  int SCREEN_BOTTOM ;
extern  int SCREEN_TOP ;
extern  int SCREEN_VS ;
extern  int NCOLORS ;

extern  unsigned long VSI_PLANE_MASK ;

static  struct  scr_info  info[MAX_SCREENS] ;

Set_screen_parameters() 
{

	int  max_colors ;
	int  value ;
	char  *env_value ;
	char  *getenv() ;

	G_gisinit(argv[0]) ;

/*  loads all of the window's info  */
	Inq_screen_info (info) ;

/*
*  Load the external variables.
*/
	find_primary_screen(&SCREEN_VS) ;

	SCREEN_LEFT =  0 ;
	SCREEN_TOP =  0 ;

	SCREEN_BOTTOM = info[SCREEN_VS].vsi_y - 1  ;
	SCREEN_RIGHT = info[SCREEN_VS].vsi_x - 1 ;

	NCOLORS = info[SCREEN_VS].vsi_vlt_size ;

/*
*  If the user gives us reasonable values we use them else
*  we use the screen values that were already set before
*  this function was called.
*/
	if (( env_value = G__getenv ("IGRAPH_TOP")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value >= SCREEN_TOP && value <= SCREEN_BOTTOM )
			SCREEN_TOP = value ;
	}

	if (( env_value = G__getenv ("IGRAPH_BOT")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value > SCREEN_TOP && value <= SCREEN_BOTTOM
		&&   value > 5)
			SCREEN_BOTTOM = value ;
		else
			SCREEN_BOTTOM = info[SCREEN_VS].vsi_y - ADJUST_Y_BORDER ;
	}
	else
		SCREEN_BOTTOM = info[SCREEN_VS].vsi_y - ADJUST_Y_BORDER ;

	if (( env_value = G__getenv ("IGRAPH_LEFT")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value >= SCREEN_LEFT && value <= SCREEN_RIGHT )
			SCREEN_LEFT = value ;
	}

	if (( env_value = G__getenv ("IGRAPH_RIGHT")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value > SCREEN_LEFT && value <= SCREEN_RIGHT
		&&   value > 5)
			SCREEN_RIGHT = value ;
		else
			SCREEN_RIGHT = info[SCREEN_VS].vsi_x - ADJUST_X_BORDER ;
	}
	else
		SCREEN_RIGHT = info[SCREEN_VS].vsi_x - ADJUST_X_BORDER ;

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

