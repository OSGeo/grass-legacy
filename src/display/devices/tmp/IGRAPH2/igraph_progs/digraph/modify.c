
/*
*  Written by the GRASS Team in the Spring of 90, -mh.
*/

#include	<stdio.h>
#include	<tools.h>
#include	"gis.h"
#include	"env.h"

#define  DISABLE_ICON  0

static  int   left, right ;
static  int   top, bot ;


modify_window (desc)
	struct  screen_description *desc ;
{

	int	w_no ;	/* Current window */
	int	which_w_no ;
	int	which_vs_no ;
	int	cur_event_mask ;
	int	opmask ;

/*
*  Adjust the window borders when getting the window size; for
*  display purposes.  The borders are added back in before storing.
*/
	top = desc->top + WINDOW_TOP_BORDER ;
	bot = desc->bottom - WINDOW_BOTTOM_BORDER ;

	left = desc->left + WINDOW_LEFT_BORDER ;
	right = desc->right -  WINDOW_RIGHT_BORDER ;
/**

fprintf( stderr, "l: %d, r: %d,   t: %d, b: %d\n",
	left, right, top, bot ) ;
**/

/*	Set up Environ V window for graphics.  */
	Set_logo("GRASS Graphics Window to Modify") ;
	Create_win( desc->current_vs_no, "Resize win ", left, top, right, bot, &w_no) ;

	Refresh_on_move(w_no, 1) ;

	Display_win(w_no) ;

/*
*  Disable some of the icons on the window strip.
*/
	Set_win_top_icon(w_no, DISABLE_ICON ) ;
	Set_win_bottom_icon(w_no, DISABLE_ICON ) ;
	Set_win_collapse_icon(w_no, DISABLE_ICON ) ;

	Color_window(w_no) ;
	Enable_events( DELETE_EVENT | REFRESH_EVENT ) ;

/*	Start the idle loop - look for window events.
*/

while (1)
{

	Wait_for_events( DELETE_EVENT | REFRESH_EVENT, &cur_event_mask ) ;

	if ( cur_event_mask & DELETE_EVENT)
	{

	/*  Using Get_delete_data() clears the mask  */
		if ( 0 < Get_delete_data(&which_w_no) )
			continue ;

	/*  Was our window deleted  */
		if( which_w_no == w_no)
			return(0) ;
	}


	if ( cur_event_mask & REFRESH_EVENT)
	{
		if ( 0 < Get_refresh_data( &which_w_no, &which_vs_no,
			&left, &top, &right, &bot, &opmask ) )
			continue ;

		if( which_w_no != w_no)
			continue ;

		/*  Our window was affected  */

		Color_window (w_no) ;
/*
*  Adjust the window borders when getting the window size; for
*  display purposes.  The borders are added back in before storing.
*/
		desc->top = top - WINDOW_TOP_BORDER ;
		desc->bottom = bot + WINDOW_BOTTOM_BORDER ;

		desc->left = left - WINDOW_LEFT_BORDER ;
		desc->right = right + WINDOW_RIGHT_BORDER ;

		handle_refresh ( opmask, desc) ;
		print_current_parameters (desc) ;

	}

}  /* main while loop  */

}
/*  end of modify_window()  */

/*
*  The variable opmask may have more than one event in it.
*  Loop through opmask if the event is in opmask perform the action
*  then use one's complement (~) of the event's #define to clear the
*  event from the opmask.
*/

handle_refresh( op_mask, desc)
	int  op_mask ;
	struct  screen_description *desc ;
{

	int  opmask ;

	opmask = op_mask ;

	while (opmask)
	{
		if (WN_WAS_COVERED & opmask)
			opmask &= ( ~ WN_WAS_COVERED) ;

		if (WN_IS_COVERED & opmask)
			opmask &= ( ~ WN_IS_COVERED) ;

		if (WN_WAS_OFF_SCREEN & opmask)
			opmask &= ( ~ WN_WAS_OFF_SCREEN) ;

		if (WN_IS_OFF_SCREEN & opmask)
			opmask &= ( ~ WN_IS_OFF_SCREEN) ;

		if (WN_CHANGED_POSITION & opmask)
		{
			store_dimensions( desc) ;

			opmask &= ( ~ WN_CHANGED_POSITION) ;
		}


		if (WN_CHANGED_SIZE & opmask)
		{
			store_dimensions( desc) ;
			opmask &= ( ~ WN_CHANGED_SIZE) ;
		}


		if (WN_CHANGED_VS & opmask)
			opmask &= ( ~ WN_CHANGED_VS) ;

		if (WN_ICON_REFRESH & opmask)
			opmask &= ( ~ WN_ICON_REFRESH) ;

		if (WN_UNCOLLAPSED & opmask)
			opmask &= ( ~ WN_UNCOLLAPSED) ;

	} /*  while  */


}

Color_window(wno)
	int  wno ;
{
	/*  color our window gray  */
	fgcolor( wno, (unsigned long) 5) ;

	rectf( wno, 0, 0, right-left, bot-top ) ;

}
