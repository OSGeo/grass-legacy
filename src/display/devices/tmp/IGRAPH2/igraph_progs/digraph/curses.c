
#include <curses.h>
#include "gis.h"

WINDOW *newwin() ;
WINDOW *MESG_WIN ;

static  int  curses_on = 0 ;


Initialize_curses()
{
	char buffer[128]  ;

	Get_old_tty() ;

	initscr () ;
    /*
	raw() ;
    */
	crmode() ;
	noecho() ;
	nonl()   ;

	Get_new_tty() ;

	/*         newwin(NROWS, NCOLS, BEGROW, BEGCOL) ; */
	MESG_WIN = newwin( 16, 70, 2, 3) ;

	wclear (MESG_WIN) ;

	box (MESG_WIN, '|', '-') ;
	touchwin(MESG_WIN) ;

	curses_on = 1 ;
}

Close_curses()
{
	if (curses_on == 0)
		return(0) ;

	curses_on = 0 ;

	clear() ;
	refresh() ;
	mvcur(0, COLS-1, LINES-1, 0) ;
	endwin() ;
}


Write_message(line, message)
	int line ;
	char *message ;
{
	wmove(MESG_WIN,line,1); 
	waddstr(MESG_WIN, "                            ") ;
	wmove(MESG_WIN,line,1); 
	waddstr(MESG_WIN, message) ;
	wmove(MESG_WIN,0,0); 
	wrefresh(MESG_WIN) ;
}

Clear_message_win()
{
	wclear (MESG_WIN) ;
	wrefresh(MESG_WIN) ;
}


Touch_win() 
{
	refresh () ;
}

Replot_screen() 
{
	wrefresh(curscr) ;
}

