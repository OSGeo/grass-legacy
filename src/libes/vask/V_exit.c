/***********************************************************************

NAME:		V_exit()

FUNCTION:	Erases the current screen and flushes the current curses setup 

USAGE:		V_exit()

PARAMETERS:

RETURNS:	nothing

ALGORITHM:	
		|	Clears screen
		|	Puts cursor on top left of the screen
		|	exits curses

CALLS:		
			clear ()       curses
			refresh ()     curses
			endwin ()     curses

***********************************************************************/

#include "config.h"
#include "vask.h"
int V_exit()
{
#ifdef HAVE_KEYPAD
	keypad(stdscr, 0);
#endif
	clear() ;
	refresh() ;
    /* added for Mips' braindead implementation of curses 
     * and the ordering is important
     */
	    echo();
	    nl();
	    noraw();

	endwin();
	fflush(stdout);
	fflush(stderr);
	fflush(stdin);

/* Added 17 Sep 1990  dpg.  is a hack we have been using on Sys V machines
**  it is not the correct way, but it seems to do the job.
**  Fixes the problem with prompts not being displayed after exitting curses
*/
/**************************************************************************/
#ifdef SYSV
        setvbuf (stderr, NULL, _IONBF, 0);
        setvbuf (stdout, NULL, _IONBF, 0);
#endif
/**************************************************************************/

	return(0) ;
}
