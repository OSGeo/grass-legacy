/*  @(#)curses.c    2.1  6/26/87  */

/*  GRASS, Summer of 88, -mh */


#include <curses.h>


WINDOW *BASE_WIN;
WINDOW *INFO_WIN;

static int _curses_state = 0 ;

Init_curses()
{
    char buffer[128] ;

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
}


Close_curses()
{
    if (Curses_state()  ==  0)
	return(0) ;

    clear();
    refresh();
    mvcur(0, COLS-1, LINES-1, 0);
    endwin();
    return(0) ;
}



_Write_base(line, message)
    int line;
    char *message;
{
    wmove(BASE_WIN,line,1); 
/*
    wclrtoeol(BASE_WIN);
*/
    wmove(BASE_WIN,line,1); 
    waddstr(BASE_WIN, message);
    wmove(BASE_WIN,0,0); 
}

_Base_string (y, x, message)
    int y, x;
    char *message;
{
    wmove(BASE_WIN,y,x); 
    waddstr(BASE_WIN, message);
    wmove(BASE_WIN,0,0); 
}

Base_string (y, x, message)
    int y, x;
    char *message;
{
    _Base_string (y, x, message);
    wrefresh(BASE_WIN);
}

Write_base(line, message)
    int line;
    char *message;
{
    _Write_base (line, message);
    wrefresh(BASE_WIN);
}


_Write_info(line, message)
    int line;
    char *message;
{
    wmove(INFO_WIN,line,1); 
    wclrtoeol(INFO_WIN);
    wmove(INFO_WIN,line,1); 
    waddstr(INFO_WIN, message);
    box (INFO_WIN, '|', '-');
}

Write_info(line, message)
    int line;
    char *message;
{
    _Write_info(line, message);
    wrefresh(INFO_WIN);
}


Clear_base()
{
    _Clear_base ();
    /*
    touchwin(BASE_WIN);
    */
    wrefresh(BASE_WIN);
}

_Clear_base()
{
    int i;
    char  buf[85];

    werase(BASE_WIN);
    wclear(BASE_WIN);
    box (BASE_WIN, '|', '-');
}

Clear_info()
{

    _Clear_info();
/*
    touchwin(INFO_WIN);
*/
    wrefresh (INFO_WIN);
}

_Clear_info()
{

    werase(INFO_WIN);
    wclear(INFO_WIN);
    box (INFO_WIN, '|', '-');
}

Replot_screen() 
{
    wrefresh(curscr);
}

Get_curses_char(answer)
    char *answer;
{
    *answer = wgetch(INFO_WIN) & 0177;
}

Get_curses_text(answer)
    char answer[];
{
    char newchar;
    char *pointer;
    int curx, cury;
    int size;

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
}


_Base_refresh()
{
    wrefresh (BASE_WIN);
}

curses_yes_no(n, s)
    int n;
    char *s;
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
}

suspend ()
{
    move (LINES-1, 0);
    refresh ();
    Old_tty ();
}

respend ()
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
}


_Info_refresh()
{
    wrefresh (INFO_WIN);
}


_Curses_on()
{
	_curses_state = 1 ;
}


_Curses_off()
{
	_curses_state = 0 ;
}

Curses_state()
{
	return( _curses_state) ;
}


close_down(ret)
   int  ret ;
{

    Close_curses() ;
    exit (ret!=0);
}

