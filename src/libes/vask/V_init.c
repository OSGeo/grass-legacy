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
#include <stdlib.h>
#include "vask.h"

/*-----------------------------------------------------------------*/
/* Prepare to use Viewform library                                 */
int V_init()	
{
    static int first = 1;

    system("clear");	/* this is a kludge - xterm has problems
			 * it shows what was on the screen after
			 * endwin is called in V_exit()
			 */
    if (first)
    {
	initscr () ;		/* initialize curses and tty */
	first = 0;
    }

/* the order of these 3 calls is important for
 * Mips' braindead implementation of curses
 */
    noecho() ;
    nonl()   ;
    raw();

    clear()  ;
    refresh();
#ifdef HAVE_KEYPAD
    keypad(stdscr, 1);
#endif

    return(0) ;
}
