

/*
*  
*/

#include	<stdio.h>
#include	<tools.h>
#include	"gis.h"

#define  DISABLE_ICON  0

static  int   left, right ;
static  int   top, bot ;

modify_window( vs_no)
	int  vs_no ;
{

	int	w_no ;	/* Current window */
	int	which_w_no ;
	int	which_vs_no ;
	int	cur_event_mask ;
	int	opmask ;


/*
 Print current window size.
 Create window to resize or move. 
 Tell them to delete the window when their done.
 Black out window.
 When they delete the window save the window size to .grassrc.
*/

	init_igraph_parameters() ;

	get_screen_parameters( &left, &top, &right, &bot) ;

	fprintf(stderr, "\nCurrent GRASS Graphics window size:\n") ;

	Set_logo("GRASS Graphics Window to Modify") ;
/*	Set up Environ V window for graphics.  */

	/*                             xlo,  ylo, xhi, yhi */
	Create_win( vs_no, "Resize win ", left, top, right, bot, &w_no) ;

	Refresh_on_move(w_no, 1) ;

	Display_win(w_no) ;

/*
*  Disable some of the icons on the window strip.
*/
	Set_win_top_icon(w_no, DISABLE_ICON ) ;
	Set_win_bottom_icon(w_no, DISABLE_ICON ) ;
	Set_win_collapse_icon(w_no, DISABLE_ICON ) ;

	Color_window(w_no) ;
	fprintf( stderr, "\n  Current window: %d\n" , w_no);

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
		{
fprintf(stderr, " Final size: l: %d, t: %d, r: %d, b: %d\n",
	left, top, right, bot ) ;

			return(0) ;
		}
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
		handle_refresh ( w_no, opmask) ;

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

handle_refresh( w_no, op_mask)
	int  w_no, op_mask ;
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
			store_dimensions( left, top, right, bot) ;

fprintf(stderr, " Position change: l: %d, t: %d, r: %d, b: %d\n",
	left, top, right, bot ) ;

			opmask &= ( ~ WN_CHANGED_POSITION) ;
		}


		if (WN_CHANGED_SIZE & opmask)
		{
			store_dimensions( left, top, right, bot) ;
			opmask &= ( ~ WN_CHANGED_SIZE) ;

fprintf(stderr, " Size change: l: %d, t: %d, r: %d, b: %d\n",
	left, top, right, bot ) ;

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

	/*
	rectf( wno, left, top, right, bot ) ;
	*/

	rectf( wno, 0, 0, right-left, bot-top ) ;

}
