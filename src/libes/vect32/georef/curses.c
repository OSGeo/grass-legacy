/*  @(#)curses.c    2.1  6/26/87  */

/*  GRASS, Summer of 88, -mh */


#include <curses.h>
#include <unistd.h>
#include "gis.h"
#include "georef.h"


WINDOW *BASE_WIN;
WINDOW *INFO_WIN;

static int _curses_state = 0 ;

int 
Init_curses (void)
{
    Get_old_tty ();
    initscr ();
    raw();
    crmode();
    noecho();
    nonl()  ;

    Get_new_tty ();


setbuf(stderr, NULL);


    /*	 newwin(NROWS, NCOLS, BEGROW, BEGCOL) ; */
    /*
    BASE_WIN = newwin (18, 79,  0, 0);
    INFO_WIN = newwin ( 6, 79, 17, 0);
    */
    BASE_WIN = newwin (19, 79,  0, 0);
    INFO_WIN = newwin ( 6, 79, 18, 0);

    scrollok (stdscr, 0);
    scrollok (INFO_WIN, 0);

    _Curses_on() ;

/*
    Clear_base();
    Clear_info();
*/
     return 0;
}


int 
Close_curses (void)
{
    if (Curses_state()  ==  0)
	return(0) ;

    clear();
    refresh();
    mvcur(0, COLS-1, LINES-1, 0);
    endwin();
    return(0) ;
}



int 
_Write_base (int line, char *message)
{
    wmove(BASE_WIN,line,1); 
/*
    wclrtoeol(BASE_WIN);
*/
    wmove(BASE_WIN,line,1); 
    waddstr(BASE_WIN, message);
    wmove(BASE_WIN,0,0); 

    return 0;
}

int 
_Base_string (int y, int x, char *message)
{
    wmove(BASE_WIN,y,x); 
    waddstr(BASE_WIN, message);
    wmove(BASE_WIN,0,0); 

    return 0;
}

int 
Base_string (int y, int x, char *message)
{
    _Base_string (y, x, message);
    wrefresh(BASE_WIN);

    return 0;
}

int 
Write_base (int line, char *message)
{
    _Write_base (line, message);
    wrefresh(BASE_WIN);

    return 0;
}


int 
_Write_info (int line, char *message)
{
    wmove(INFO_WIN,line,1); 
    wclrtoeol(INFO_WIN);
    wmove(INFO_WIN,line,1); 
    waddstr(INFO_WIN, message);
    box (INFO_WIN, '|', '-');

    return 0;
}

int 
Write_info (int line, char *message)
{
    _Write_info(line, message);
    wrefresh(INFO_WIN);

    return 0;
}


int 
Clear_base (void)
{
    _Clear_base ();
    /*
    touchwin(BASE_WIN);
    */
    wrefresh(BASE_WIN);

    return 0;
}

int 
_Clear_base (void)
{
    werase(BASE_WIN);
    wclear(BASE_WIN);
    box (BASE_WIN, '|', '-');

    return 0;
}

int 
Clear_info (void)
{

    _Clear_info();
/*
    touchwin(INFO_WIN);
*/
    wrefresh (INFO_WIN);

    return 0;
}

int 
_Clear_info (void)
{

    werase(INFO_WIN);
    wclear(INFO_WIN);
    box (INFO_WIN, '|', '-');

    return 0;
}

int 
Replot_screen (void) 
{
    wrefresh(curscr);

    return 0;
}

int 
Get_curses_char (char *answer)
{
    *answer = wgetch(INFO_WIN) & 0177;

    return 0;
}

int 
Get_curses_text (char answer[])
{
    char newchar;
    char *pointer;
    int curx, cury;

    pointer = answer;

    *answer = 0;
    for(;;)
    {
	newchar = wgetch(INFO_WIN) & 0177;

	if ((newchar > 037) && (newchar < 0177))
	{
	    *(pointer++) = newchar;
	    *pointer = 000;
	    waddch(INFO_WIN,newchar);
	    wrefresh(INFO_WIN);
	}
	else if (newchar == 010)
	{
	    if (pointer > answer)
	    {
		*(pointer--) = 000;
		getyx(INFO_WIN,cury,curx);
		wmove(INFO_WIN,cury,curx-1);
		waddch(INFO_WIN,' ');
		wmove(INFO_WIN,cury,curx-1);
		wrefresh(INFO_WIN);
	    }
	}
	else
	    break;
    }
    G_squeeze (answer);

    return 0;
}


int 
_Base_refresh (void)
{
    wrefresh (BASE_WIN);

    return 0;
}

int 
curses_yes_no (int n, char *s)
{
    char buff[64];
    while (1)
    {
	Write_info(n, s);
	Get_curses_text(buff);
	switch (*buff)
	{
	    case 'Y': case 'y':
		return(1);
	    case 'N': case 'n':
		return(0);
	    default:
		Write_info(n, "Please answer yes or no");
		sleep(2);
	}
    }

    return 0;
}

int mysuspend (void)
{
    move (LINES-1, 0);
    refresh ();
    Old_tty ();

    return 0;
}

int myrespend (void)
{
    New_tty();
    move (0, 0);
    clear ();
    touchwin (curscr);
    refresh ();
    touchwin (BASE_WIN);
    touchwin (INFO_WIN);
    wrefresh (BASE_WIN);
    wrefresh (INFO_WIN);

    return 0;
}


int _Info_refresh (void)
{
    wrefresh (INFO_WIN);

    return 0;
}

int _Curses_on (void)
{
	_curses_state = 1 ;

    return 0;
}

int _Curses_off (void)
{
	_curses_state = 0 ;

    return 0;
}

int Curses_state (void)
{
	return( _curses_state) ;
}

void close_down (int ret)
{
    Close_curses() ;
    exit (ret!=0);
}
