/*
*  These are special functions for the GRASS Driver for Environ V.
*
*  Functions:
*    Clear_button_hits() - Clears mouse button hits.
*    Hide_cursor() - Hides mouse cursor from user.
*    Shows_cursor() - Shows mouse cursor to user.
*    make_cross_cursor() - makes the cursor into cross hairs.
*
*    NOTE: that the mouse is ALWAYS active even if it isn't an active event.
*
*  Written by the GRASS Team in the Winter of 88.
*
*/

#include	<tools.h>

extern int WNO ;



Clear_button_hits()
{

	int  area_no ;
	int  x, y ;
	int  button ;
	int  transition ;
	int  timetag ;

	int  curevents ;


/*	Clear_button_data() doesn't clear out buttons hits in the queue.
	???????

	BEWARE:  There are some subtleties to the function below.

	Making a call to Get_button_data without first making the call to 
	to Inq_events() will result in Get_button_data() returning
	a no data condition.  No button hits will be cleared.

	Making the call to Inq_events() will allow Get_button_data() to
	clear as many button hits as in the queue.
	Don't ask me why.  I had to figure the damn thing out by trial and
	error.  -mh

	Inq_button_data() does not seem to work either?????
	I tried this first.
*/


		Inq_events ( &curevents);		

	while (  !  Get_button_data( &area_no, &x, &y, &button, &transition, &timetag) )
		;   /*  null body  */


}


Hide_cursor()
{
	hidecursor(WNO) ;
}

Show_cursor()
{
	showcursor(WNO) ;
}

make_cross_cursor()
{

/*  makes a crosshair pattern one pixels wide  */
	int   i ;
	long  enable_pattern[32] ;  /* 32x32,  unmodified pixels  */
	long  data_pattern[32] ;    /* 32x32, cursor pattern  */

/*  two pixel wide cursor
	enable_pattern[i] = 0x00018000 ;
*/

/*  turn on pixels for the vertical lines  */
	for(i = 0; i < 32; i++ )
	{
		enable_pattern[i] = 0x00010000 ;
		data_pattern[i] = 0x00010000 ;
		
	}

/*  turn on pixels for the horizontal lines  */
	enable_pattern[14] |= 0xffffffff ;
	data_pattern[14] |= 0xffffffff ;

/*  The 15, 15  is for the x,y of the "hot spot", which is the actual
active point of the cursor.
*/

	defcursor( WNO, enable_pattern, data_pattern, 15, 15) ;


}
