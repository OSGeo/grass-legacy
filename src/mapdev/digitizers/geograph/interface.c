


#include	<stdio.h>


/*  file includes functions that are digitizer depend, but don't directly
*   call the digitizer 
*
*  D_cursor_buttons() - tells the calling program if this digitizer has 
*   at least five digitizer cursor keys to use.
*
*  D_start_button()  - tells the calling program what number the digitizer
*  cursor starts with.
*
*  D_ask_driver_raw()  -  is the heart of the effort to using the digitizer
*   cursor keys for input.
*     return which key or button was hit on the digitizer.
*   NOT USEABLE FOR THIS DIGITIZER.  IT HAS NO KEYS.
*
*
*   D_setup_origin() - sets digitizer origin (if needed) and calls the device
*   driver to set anything else that is needed at startup.
*
*   D_read_raw() - reads raw coor. from the digitizer, exits on read error.
*
*  delay() - used by the driver to delay for a split second.
*
*         GRASS 3.0  Spring 88
*/



D_setup_origin()
{

	char  buff[81] ;

	Clear_base() ;
	Clear_info() ;
	Write_info(2, "Identify digitizer 0,0 point") ;
	Write_info(3, "  Please move cursor to the far lower left.") ;
	Write_info(4, "  Then hit <RETURN>  ") ;
	Get_curses_text(buff) ;
	D_set_origin() ;
	Clear_info() ;
}


/*  This is to tell the calling program that it can ask for responses from
*   the digitizer, because this particular digitizer has multiple buttons.
*   true - yes,  false - no 
*/

D_cursor_buttons()
{
	return(0) ;	/*  NO cursor buttons  */
}

/*  This is to tell the calling program how the digitizer cursor buttons
*   are numbered.   ALTEK buttons are 0-F, a KURTA is 1-16,  it doesn't 
*   matter what this returns if you don't have a cursor with buttons..
*/

D_start_button()
{
	return(0) ;	/*  buttons start with 0  */
}



/*  This is to tell the calling program if there is a foot switch.
*/

D_foot_switch()
{
	return(1) ;	/*  YES, there is a foot switch  */
}



D_read_raw (Xraw, Yraw)
	int     *Xraw;
	int     *Yraw;
{

	int  X, Y ;
	int  KpdChar ;
	int  KpdStat ;
	int  FtswStat ;
	int  stat ;
	int	i ;

	if ( (stat = D_readall(&X, &Y, &FtswStat, &KpdStat, &KpdChar)) < 0)
	{
		fprintf(stderr, "\n Digitizer read error:   exiting\n\n") ;
		for(i=0; i<9999999; i++)
			;
		close_down(-1) ;
	}

	*Xraw = X ;
	*Yraw = Y ;

	return( FtswStat) ;
}


/*  returns digitizer (raw) coordinates  */

D_ask_driver_raw( x, y)
	double  *x, *y ;
{
	int  X ;
	int  Y ;
	int  FtswStat ;

/*  this is used by digitizers with buttons,  not applicable  to this
*   digitizer with the setup we have now.  yes, we have no buttons.
*/

	FtswStat = D_read_raw (&X, &Y) ;
	*x = (double)X ;
	*y = (double)Y ;


    return(FtswStat) ;
}


/*  D_clear_driver() in this driver is just read the digitizer to make sure
*  there are no points left.
*/

D_clear_driver()
{
	int  X ;
	int  Y ;

	D_read_raw (&X, &Y) ;

    return(0) ;
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
