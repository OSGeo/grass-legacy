
 /*
 *   NOTES on Intergraph Driver.
 *   This is called only at the startup of the graphics driver.
 *
 *  Origin point is in upper left corner.
 *  Max screens is defined in <tools.h>
 *
 *  Function unique to IGRAPH:   Set_from_env() ;
 *  Allows setting window from environment
 *  variables.
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
 *  The graphics screen will always start in VS screen 1.
 *  It can be started in screen 2 by setting the IGRAPH_VS environmental
 *  variable or setting the default to 2 in this code before the driver
 *  is compiled.
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

	int  vsno ;	/*  virtual screen number  */
	int  wno ;	/*  window number  */

	G_gisinit("START_IGRAPH") ;

/*  loads all of the window's info  */
	Inq_screen_info (info) ;

/*
*  Load the external variables.
*/
/*  This is the default for virtual screen */
	SCREEN_VS = 1 ;

	SCREEN_VS = find_graphics_vs(SCREEN_VS) ;

	SCREEN_LEFT =  0	;
	SCREEN_TOP =  0 ;

	SCREEN_BOTTOM = info[SCREEN_VS].vsi_y - 60 ;

	SCREEN_RIGHT = info[SCREEN_VS].vsi_x - 10 ;
	NCOLORS = 0 ;

/*  set the window from evironment variables if the env's are set  */
	Set_from_env() ;

/*  The number of colors they can use is limited by the vlt size on
*   the particular VS they have already chosen.
*/
	if (NCOLORS > Num_of_colors_for_vs(SCREEN_VS) ||
	    NCOLORS < MIN_IGRAPH_COLORS)
		NCOLORS =   Num_of_colors_for_vs(SCREEN_VS) ;

/*  This is used by Panel.c  */
	VSI_PLANE_MASK =  info[SCREEN_VS].vsi_plane_mask ;
}

/*
*  lets user set window from environment variables.
*  env variable IGRAPH_TOP =  SCREEN_BOTTOM
*  env variable IGRAPH_LEFT =  SCREEN_RIGHT
*  env variable IGRAPH_BOT =  SCREEN_BOTTOM
*  env variable IGRAPH_RIGHT =  SCREEN_RIGHT
*  env variable IGRAPH_VS =  SCREEN_VS
*  env variable IGRAPH_COLORS =  NCOLORS
*/

Set_from_env()
{
	int  value ;
	char  *env_value ;
	char  *getenv() ;

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
	}

	if (( env_value = G__getenv ("IGRAPH_LEFT")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value >= SCREEN_LEFT && value < SCREEN_RIGHT )
			SCREEN_LEFT = value ;
	}

	if (( env_value = G__getenv ("IGRAPH_RIGHT")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value > SCREEN_LEFT && value <= SCREEN_RIGHT
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
}

/*
*  If the environmental variable isn't set for the VSNO, then:
*
*  Loop through all the screen info to find the vsno with the most
*  active colors.  This can be determined by looking at the number of
*  planes or by the vsi_vlt_size.
*/


find_graphics_vs (default_vsno)
int  default_vsno ;
{
	int  i ;
	int  current_vlt_size ;
	int  value ;
	char  *env_value ;

	if (( env_value = G__getenv ("IGRAPH_VS")) != NULL )
	{
		value = -1 ;
		value = atoi(env_value) ;

		if ( value >= 0 || value <= 3)
			return(value) ;
	}

	current_vlt_size = info[default_vsno].vsi_vlt_size ;

/****
	return(default_vsno) ;
****/

/*
*  ENVIRON_V BUG.
*  I have no way of determining which of the virtual screens is the 
*  graphics screen.  For now return the default VS.
*
*  This loop doesn't work properly, because ENVIRON_V isn't initiliazing the
*  screen info correctly.  The information in the screen info says that
*  the information is the same for all three virtual screens.
*  Another words all the VS screens have 9 planes which they don't.
*  Only VS 0 and 1 have 9 planes,  VS 2 has 5 planes on IP240 and 3050.
*/

	for ( i=0; i<MAX_SCREENS ; ++i)
	{
		if ( info[i].vsi_screen_num == -1)
		continue ;
		
		if (info[i].vsi_vlt_size > current_vlt_size)
		{ 
			current_vlt_size = Num_of_colors_for_vs(i) ;
			default_vsno = i ;
		}
	}

	return(default_vsno) ;

} 

Num_of_colors_for_vs( i)
  int i ;
{
	return (info[i].vsi_vlt_size) ;
}
