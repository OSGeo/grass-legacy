
 /*
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

extern  int WNO ;


Resize_igraph() 
{

int  looping ;
int  cur_events ;

	looping = 1 ;

		Inq_enabled_events( &cur_events ) ;
fprintf(stderr, "Enabled: %x  Our: %x\n", cur_events, OUR_EVENTS) ;
fprintf(stderr, "Resize the  window now .\n") ;

	while (looping)
	{
		Inq_events( &cur_events ) ;

		/*  No events  */
/*****
		if( ! cur_events )
			break ;
*****/

		if ( r_handle_events( cur_events) )
			looping = 0 ;

	}  /*  while(looping)  */

return(0) ;

}

r_handle_events (cur_events)
	int  cur_events ;
{


int  value ;
int  opmask ;
int  xlo, ylo ;
int  xhi, yhi ;
int  which_w_no ;
int  which_vs_no ;


value = 0 ;
	if ( cur_events & DELETE_EVENT)
	{
fprintf(stderr, "Delete  \n") ;
		if ( 0 == Get_delete_data(&which_w_no) )
			if( which_w_no == WNO)
				Graph_Close() ;
	}


	if ( cur_events & REFRESH_EVENT)
	{
fprintf(stderr, "Refresh  \n") ;
		if ( 0 < Get_refresh_data( &which_w_no, &which_vs_no,
			&xlo, &ylo, &xhi, &yhi, &opmask ) )
			return(0) ;

	}

/*
*  The variable cur_events may have more than one event in it.
*  That is why a case statement is not used.
*
*  These refresh events may happen but we don't care:
*   WN_WAS_COVERED, WN_IS_COVERED, WN_WAS_OFF_SCREEN, WN_IS_OFF_SCREEN,
*   WN_ICON_REFRESH, WN_UNCOLLAPSED
*
*   They may do this one, but I have never found a way to switch
*   a window to another VS.  "WN_CHANGED_VS"
*
*/

	if (  (WN_CHANGED_POSITION & opmask)  ||
		(WN_CHANGED_SIZE & opmask) )
	{
fprintf(stderr, "Window changed.\n") ;
		SCREEN_LEFT = xlo ;
		SCREEN_RIGHT = xhi ;
		SCREEN_BOTTOM = yhi ;
		SCREEN_TOP = ylo ;
		++value ;
	}

return(value) ;

}

