/*  @(#)curses.c    2.1  6/26/87  */

#ifdef SCS_MODS
#define PSU             (0x010000 | AREA)
#endif

static int _curses_state = 0 ;

Init_curses()
{

    /*    this all got replaced by a call to V_init() 
    Get_old_tty ();
    initscr ();
    raw();
    crmode();
    noecho();
    nonl()  ;

    Get_new_tty ();
    */

    /*	This is done by the V_call in get_head_binary () called from main
    **  but doesn't hurt to leave this here in case things change later
    */
    V_init ();

    V_set_old_tty ();  /* get old and set it in our structs */
    Get_old_tty ();

    V_set_new_tty ();  /* get new and set it in our structs */
    cbreak ();		/* but set crmode in ours */
    noecho();
    Get_new_tty ();

    _Curses_on() ;

}


Close_curses()
{
    if (Curses_state()  ==  0)
	return(0) ;

/*  clear();
    refresh();
    mvcur(0, COLS-1, LINES-1, 0);
*/
    endwin();
    return(0) ;
}


Get_curses_char(answer)
    char *answer;
{
    *answer = getch() & 0177;
}

/*
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
	else if (newchar == 010 || pointer >= answer + 70)
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

curses_yes_no(n, s)
    int n;
    char *s;
{
    char buff[200];
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

curses_yes_no_default (n, str, def)
    int n;
    char *str;
    int def;
{
    char buff[120];
    while (1)
    {
	sprintf (buff, "%s[%c] ", str, def ? 'y' : 'n');
	Write_info(n, buff);
	Get_curses_text(buff);
	switch (*buff)
	{
	    case 'Y': case 'y':
		return(1);
	    case 'N': case 'n':
		return(0);
	    default:
		return (def);
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
*/

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

/*
Curses_error (str)
    char *str;
{
    char buf[100];
    _Clear_info ();
    BEEP;
    Write_info (2, str);
    Write_info (4, "     Press <Return> to continue");
    Get_curses_text (buf);

    return (-1);
}
*/
