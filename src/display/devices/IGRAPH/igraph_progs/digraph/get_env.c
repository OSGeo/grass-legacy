
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
 *  The defaults being set in this function must match the defaults
 *  in ../../Set_env.c .
 *
 *  Written by the GRASS Team in the Spring of 90, -mh.
 *
 */

#include	<stdio.h>
#include	<tools.h>
#include	"gis.h"
#include	"env.h"
#include	"igraphics.h"

static  struct  scr_info  info[MAX_SCREENS] ;

get_screen_parameters( desc) 
	struct  screen_description *desc ;
{

	int  value ;
	char  *env_value ;

	int SCREEN_LEFT ;
	int SCREEN_RIGHT ;
	int SCREEN_BOTTOM ;
	int SCREEN_TOP ;
	int SCREEN_VS ;
	int NCOLORS ;

/*  loads all of the window's info  */
	Inq_screen_info (info) ;

	find_primary_screen(desc) ;
	SCREEN_VS  =  desc->primary_vs_no  ;

	SCREEN_LEFT =  1	;
	SCREEN_TOP =  29 ;

	SCREEN_BOTTOM = info[SCREEN_VS].vsi_y - 29 ;

	SCREEN_RIGHT = info[SCREEN_VS].vsi_x - 1 ;

/*
*  Maximum number of colors should be the number of possible
*  colors on the primary screen.
*/
	NCOLORS = info[SCREEN_VS].vsi_vlt_size ;
	desc->max_num_colors = NCOLORS ;

/*
*  If the user gives us reasonable values we use them else
*  we use the default values.
*/
	if (( env_value = G__getenv ("IGRAPH_TOP")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value >= SCREEN_TOP && value <= SCREEN_BOTTOM - 50 )
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
		SCREEN_BOTTOM = (SCREEN_BOTTOM - SCREEN_TOP)/2 - 10;

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
* Screen_vs is set to the vs screen they want or our default
* which is the primary screen.
* Maximum number of colors should be the number of possible
* colors on the particular VS they have already chosen.
*/
	if( NCOLORS > info[SCREEN_VS].vsi_vlt_size )
		NCOLORS = info[SCREEN_VS].vsi_vlt_size ;

	desc->left = SCREEN_LEFT ;
	desc->right = SCREEN_RIGHT ;
	desc->bottom = SCREEN_BOTTOM ;
	desc->top = SCREEN_TOP ;
	desc->vs_no = SCREEN_VS ;
	desc->num_colors = NCOLORS ;

	return(0) ;

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

find_primary_screen(desc)
	struct  screen_description *desc ;
{

	int  w_no ;
	int  vs_no ;

	/*                             xlo,  ylo, xhi, yhi */
	Create_win( desc->current_vs_no, "VS win ", 10, 10, 30, 30, &w_no) ;

	desc->primary_vs_no = -1 ;

	for ( vs_no = MIN_VS_NO; vs_no <= MAX_VS_NO; ++vs_no)
	{
	
		if ( ! Set_win_vs( w_no, vs_no) )
		{
			desc->primary_vs_no = vs_no ;
			break ;
		}

	}

	if (desc->primary_vs_no == -1)
		desc->primary_vs_no = DEFAULT_VS_NO ;

	Delete_win(w_no) ;

}
