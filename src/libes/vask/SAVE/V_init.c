/***********************************************************************

NAME:		V_init()

FUNCTION:	Initialize curses and prepare screen

USAGE:		V_init()

PARAMETERS:

RETURNS:	nothing

ALGORITHM:	
		|	Ignore interrupt and quit signals
		|	Initialize curses
		|	set no echo and no new line capabilities of curses
		|       set raw mode for input

CALLS:		
		initscr ()      curses
		raw()           curses
		noecho()        curses
		nonl()          curses

***********************************************************************/
#include "vask.h"

/*-----------------------------------------------------------------*/
/* Prepare to use Viewform library                                 */
V_init()	
{
    static int first = 1;

    if (first)
    {
	V_get_old_tty();	/* get current tty settings */

	initscr () ;		/* initialize curses and tty */
	raw();
	noecho() ;
	nonl()   ;

	V_get_new_tty();	/* get new tty settings */
	first = 0;
    }
    V_set_new_tty();
    clear()  ;

    return(0) ;
}
