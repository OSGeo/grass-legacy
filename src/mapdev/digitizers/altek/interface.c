

#include	<stdio.h>


/*  file includes functions that are digitizer depend, but don't directly
*   call the digitizer 
*
*  The following functions answer questions the main digit program will
*  ask to setup menus, prompting user,  etc...
*
*  D_cursor_buttons() - tells the calling program if this digitizer has 
*   at least five digitizer cursor keys to use (the reverse is to use NO
*   cursor buttons).  
*
*  D_start_button()  - tells the calling program what number the digitizer
*  cursor buttons starts with.
*
*  D_foot_switch() - tells the calling program if there is a useable 
*  footswitch.  This accomodates the geographics digitizer.
*
*-------------------------------------------------------------------------
*  Functions that interface between the digit program and the device driver.
*
*  D_ask_driver_raw()  -  is the heart of the effort to using the digitizer
*   cursor keys for input.
*     return which key or button was hit on the digitizer.
*
*   D_setup_origin() - sets digitizer origin (if needed) and calls the device
*   driver to set anything else that is needed at startup.
*
*   D_read_raw() - reads raw coor. from the digitizer, exits on read error.
*
*-------------------------------------------------------------------------
*   General purpose function:
*
*   delay() - used by the driver to delay for a split second.
*
*         GRASS 3.0  Spring 88
*/


/*  This is to tell the calling program that it can ask for responses from
*   the digitizer, because this particular digitizer has multiple buttons.
*   true - yes,  false - no 
*/

D_cursor_buttons()
{
	return(1) ;	/*  yes  */
}


/*  This is to tell the calling program how the digitizer cursor buttons
*   are numbered.  An ALTEK's buttons are 0-F, a KURTA is 1-16,  it doesn't 
*   matter what this returns if you don't have a cursor with buttons..
*/

D_start_button()
{
	return(0) ;	/*  buttons start with 0  */
}


/*  This is to tell the calling program if there is a foot switch.
*   Accomodating the geographics.
*/

D_foot_switch()
{
	return(0) ;	/*  no foot switch  */
}



D_setup_origin()
{
	/*  this is not needed in the altek  */
}

/*  ask until any key is hit, loads in digitizer (raw) coordinates  */
/*  fine tune the delays  for your system and baud rate  */

D_ask_driver_raw( x, y)
	double  *x, *y ;
{
	int  X ;
	int  Y ;
	int  button ;

    while (  ! (button = D_readhit( &X, &Y )) )
    {
/*
	delay(10) ;
*/
    }

    D_clear_driver() ;

    *x = (double)X ;
    *y = (double)Y ;

    return(button) ;
}




D_clear_driver ()
{
	int  X ;
	int  Y ;

/*  clear out all button hits  */
    D_flush() ;
    while ( D_read_raw( &X, &Y ) > 0)
   	 continue ;
/*
     D_read_raw( &X, &Y ) ;
*/

}



D_read_raw (Xraw, Yraw)
	int     *Xraw;
	int     *Yraw;
{

	int  X, Y ;
	int  Key_Hit ;

	if ( (Key_Hit = D_readall(&X, &Y)) < 0)
	{
		fprintf(stderr, "\n Digitizer read error:   exiting\n\n") ;
		close_down(-1) ;
	}

	*Xraw = X ;
	*Yraw = Y ;

	return(Key_Hit) ;
}


delay(n) /* delay n milliseconds */
	int  n ;
{
	char zero;
	int i;

	zero = 0;

/* this assumes 9600 baud to stderr */
	while (n-- > 0)
		for (i = 0; i < 10; i++)
			write (2, &zero, 1);
}

