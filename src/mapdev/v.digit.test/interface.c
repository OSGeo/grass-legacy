
#include <stdio.h>
#include "debug.h"
#include "D.h"
#include <unistd.h>
#include "ginput.h"
#include "local_proto.h"


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
*
*   D_read_raw() - reads raw coor. from the digitizer, exits on read error.
*
*  delay() - used by the driver to delay for a split second.
*
*         GRASS 3.0  Spring 88
*/



/*  This is to tell the calling program that it can ask for responses from
*   the digitizer, because this particular digitizer has multiple buttons.
*/

int 
D_cursor_buttons (void)
{
	return (digdevice.buttons);
}

/*  This is to tell the calling program how the digitizer cursor buttons
*   are numbered.  An ALTEK buttons are 0-F, a KURTA is 1-16,  it doesn't 
*   matter what this returns if you don't have a cursor with buttons..
*/

int 
D_start_button (void)
{
	 return (digdevice.buttonstart); /* do buttons start with 0 or 1 ?*/
}

/*  This is to tell the calling program if there is a foot switch.
*/

int 
D_foot_switch (void)
{
    return (digdevice.footswitch);
}


int 
D_clear_driver (void)
{
   int x, y;

/*  clear out all button hits */  
    while (get_diginput( &x, &y))
    ;

    return 0;
}

/*  ask until any key is hit, loads in digitizer (raw) coordinates  */
int 
D_ask_driver_raw (double *x, double *y)
{
    int button = 0;
    int ix, iy;

    D_clear_driver() ; 

/*DEBUG*/ debugf ("in D_ask_driver_raw\n");

/*  keep reading until a button is hit  */
    
    while ( 0 >= button)
    { 
	button = get_diginput(&ix, &iy);
    } 

    *x = (double)ix ;
    *y = (double)iy ;

/*DEBUG*/ debugf ("D_ask_driver_raw: button = %d   x = %lf y = %lf\n", ginfo.button, *x, *y);

    D_clear_driver() ;
    return(button) ;
}



int 
D_read_raw (int *Xraw, int *Yraw)
{

	int  X, Y ;
	int  Key_Hit ;


	Key_Hit = D_readall(&X, &Y);

	*Xraw = X ;
	*Yraw = Y ;

	return(Key_Hit) ;
}



int 
delay ( /* delay n milliseconds */
    int n
)
{
	char zero;
	int i;

	zero = 0;

/* this assumes 9600 baud to stderr */
	while (n-- > 0)
		for (i = 0; i < 10; i++)
			write (2, &zero, 1);
    return 0;
}
