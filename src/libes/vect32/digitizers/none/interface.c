
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
*
*********************    WARNING ******************
*    In ask_driver_raw():
*   if your digitizer seems to be hanging up and the left light
*   digitizer cursor is SOLID red it may mean that the computer is outrunning
*   the digitizer.    you can up the time in the delay() calls to give your
*   digitizer some extra time.
*
*   Set_origin() - sets digitizer origin (if needed) and calls the device
*   driver to set anything else that is needed at startup.
*
*   Read_raw() - reads raw coor. from the digitizer, exits on read error.
*
*  delay() - used by the driver to delay for a split second.
*
*         GRASS 3.0  Spring 88
*/






D_setup_origin()
{
 return(0) ;
}


/*  This is to tell the calling program that it can ask for responses from
*   the digitizer, because this particular digitizer has multiple buttons.
*   true - yes,  false - no 
*/

D_cursor_buttons()
{
	return(0) ;	/*  NO  */
}

/*  This is to tell the calling program how the digitizer cursor buttons
*   are numbered.  An ALTEK buttons are 0-F, a KURTA is 1-16,  it doesn't 
*   matter what this returns if you don't have a cursor with buttons..
*/

D_start_button()
{
	return(0) ;	/*  buttons start with 0  */
}

D_foot_switch()
{
	return(0) ;	/*  buttons start with 0  */
}



/*  ask until any key is hit, loads in digitizer (raw) coordinates  */
D_ask_driver_raw( x, y)
	double  *x, *y ;
{
    return(-1) ;
}



D_read_raw (Xraw, Yraw)
	int     *Xraw;
	int     *Yraw;
{

	return(-1) ;
}

D_clear_driver()
{
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
