/*  @(#)curses.c    2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

/*
*  This code is compiled by each digitizer driver and stored in its directory.
*  Even though there is no curses.o it is used.
*/


WINDOW *BASE_WIN;
WINDOW *INFO_WIN;
WINDOW *HELP_WIN;

static int _curses_state = 0 ;


Init_curses()
{
    char buffer[128] ;

/*setbuf(stdout, NULL) ;*/    /* ?? -dpg */
/* setbuf(stderr, NULL); */
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
#ifdef STRANGE
    /* this is extinct in sys V */
    /* and causes all sorts of trouble w/ the Star Gate async */
    crmode ();		/* but set crmode in ours */
#endif
    Get_new_tty ();

    _Curses_on() ;


    /*	 newwin(NROWS, NCOLS, BEGROW, BEGCOL) ; */
    /*
    BASE_WIN = newwin (18, 79,  0, 0);
    INFO_WIN = newwin ( 6, 79, 17, 0);
    HELP_WIN = newwin (16, 49, 1, 28);
    */
    BASE_WIN = newwin (19, 79,  0, 0);
    INFO_WIN = newwin ( 6, 79, 18, 0);
    HELP_WIN = newwin (17, 49, 1, 28);

    scrollok (stdscr, 0);
    scrollok (INFO_WIN, 0);

/*
     _Clear_info();
    wnoutrefresh (INFO_WIN);
     Init_help ();
    wnoutrefresh (HELP_WIN);
*/
     _Clear_base();
/*
     wnoutrefresh (HELP_WIN);
*/
     wnoutrefresh (BASE_WIN);
     doupdate ();
   /*DEBUG*/  refresh ();
}


Close_curses()
{
    if (Curses_state()  ==  0)
	return(0) ;

    clear();
    refresh();
    mvcur(0, COLS-1, LINES-1, 0);
    endwin();
    _Curses_off() ;
    return(0) ;
}

_Write_base_win()
{
    int i;

    _Clear_base();
    for(i=0; i<menu1_lines; i++)
    {
	wmove(BASE_WIN,i+1,1);
	waddstr(BASE_WIN, menu1[i]);
    }
    box (BASE_WIN, '|', '-');
    wnoutrefresh (BASE_WIN);
}

Write_base_win()
{
    _Write_base_win ();
    /* wrefresh(BASE_WIN); */
    doupdate ();
}

Write_edit_win()
{
    Write_edit_win (&M_edit);
/*
    int i;

    _Clear_base();
    for(i=0; i<menu2_lines; i++)
    {
	wmove(BASE_WIN,i+1,1);
	waddstr(BASE_WIN, menu2[i]);
    }
    update_menu (&M_edit);
    wrefresh(BASE_WIN);
*/
}

_Write_generic_win(menu)
    struct Menu_head *menu;
{
    int i;

    _Clear_base();
    for(i=0; i<menu2_lines; i++)
    {
	wmove(BASE_WIN,i+1,1);
	waddstr(BASE_WIN, menu2[i]);
    }
    _write_generic_title (menu->name);
    update_menu (menu);
}

Write_generic_win(menu)
    struct Menu_head *menu;
{
    _Write_generic_win (menu);
    /* wrefresh(BASE_WIN); */
    doupdate ();
}

_write_generic_title (name)
    char *name;
{
    char buf[1024];

    sprintf (buf, "%s Menu", name);
    _Base_string (1, 77 - strlen (buf), buf);

}


/* 
*  _Write_dig_win()  contains information that is digitizer specific
*/

_Write_dig_win()
{
    int i;

    _Clear_base();
    for(i=0; i<dig_menu_lines; i++)
    {
	wmove(BASE_WIN,i+1,1);
	waddstr(BASE_WIN, dig_menu[i]);
    }
    update_menu (&M_digit);
    wnoutrefresh (BASE_WIN);
}


Write_dig_win()
{
    _Write_dig_win ();
    /* wrefresh(BASE_WIN); */
    doupdate ();
}


_Write_header_info()
{
    char buf[128];
    sprintf(buf, "%-.30s", head.map_name) ;	wmove(BASE_WIN,4,18) ;  waddstr(BASE_WIN, buf);
    sprintf(buf, "%-d", head.orig_scale) ;      wmove(BASE_WIN,5,18) ;  waddstr(BASE_WIN, buf);
    sprintf(buf, "%-.30s", head.your_name) ;       wmove(BASE_WIN,6,18) ;  waddstr(BASE_WIN, buf);
    sprintf(buf,"%-6.5lf in.", head.digit_thresh) ; wmove(BASE_WIN,7,18) ;  waddstr(BASE_WIN, buf);
    sprintf(buf,"%-3.2lf %s", head.map_thresh, G_unit_name (G_projection_units (G_projection()), 1));
    wmove(BASE_WIN,8,18) ;  waddstr(BASE_WIN, buf);

    box (BASE_WIN, '|', '-');
    wnoutrefresh (BASE_WIN);
}

Write_header_info()
{
    _Write_header_info ();
    /* wrefresh(BASE_WIN); */
    doupdate ();
}

_Write_type_info()
{
    char buf[128];
    sprintf(buf,"%d", get_type_cnt (LINE)) ;  wmove(BASE_WIN,4,70) ;  waddstr(BASE_WIN, buf);
    sprintf(buf,"%d", get_type_cnt (AREA)) ;  wmove(BASE_WIN,5,70) ;  waddstr(BASE_WIN, buf);
    sprintf(buf,"%d", get_type_cnt (POINTS)) ;  wmove(BASE_WIN,8,70) ;  waddstr(BASE_WIN, buf);
    wnoutrefresh (BASE_WIN);
}

Write_type_info()
{
    _Write_type_info ();
    /* wrefresh(BASE_WIN); */
    doupdate ();
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
    wnoutrefresh (BASE_WIN);
}

_Base_string (y, x, message)
    int y, x;
    char *message;
{
    wmove(BASE_WIN,y,x); 
    waddstr(BASE_WIN, message);
    wmove(BASE_WIN,0,0); 
    wnoutrefresh (BASE_WIN);
}

Base_string (y, x, message)
    int y, x;
    char *message;
{
    _Base_string (y, x, message);
    /* wrefresh(BASE_WIN); */
    doupdate ();
}

Write_base(line, message)
    int line;
    char *message;
{
    _Write_base (line, message);
    /* wrefresh(BASE_WIN); */
    doupdate ();
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
    wnoutrefresh (INFO_WIN);
}

Write_info(line, message)
    int line;
    char *message;
{
    _Write_info(line, message);
    /* wrefresh(INFO_WIN); */
    doupdate ();
}


Init_help ()
{
    werase (HELP_WIN);
}

Clear_help()
{
    _Clear_help ();
    /* wrefresh(HELP_WIN); */
    doupdate ();
}

_Clear_help()
{
    int i;

    werase(HELP_WIN);
    /*
    wclear(HELP_WIN);
    */
    box (HELP_WIN, '|', '-');
    wnoutrefresh (HELP_WIN);
}

Clear_base()
{
    _Clear_base ();
    /*
    touchwin(BASE_WIN);
    */
    /* wrefresh(BASE_WIN); */
    doupdate ();
}

_Clear_base()
{
    int i;

    werase(BASE_WIN);
    /*
    wclear(BASE_WIN);
    */
    box (BASE_WIN, '|', '-');
    wnoutrefresh (BASE_WIN);
}

Clear_info()
{

    _Clear_info();
/*
    touchwin(INFO_WIN);
*/
    /* wrefresh (INFO_WIN); */
    doupdate ();
}

_Clear_info()
{

    werase(INFO_WIN);
    /*
    wclear(INFO_WIN);
    */
    box (INFO_WIN, '|', '-');
    wnoutrefresh (INFO_WIN);
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

show_mode(mode, type)
    int mode;
    char type;
{
    wmove(BASE_WIN,14,51) ; waddstr(BASE_WIN, "   point      line      ");
    wmove(BASE_WIN,15,51) ; waddstr(BASE_WIN, "   stream     area edge ");
#ifdef SITES_DEFINED
    wmove(BASE_WIN,16,51) ; waddstr(BASE_WIN, "              site      ");
#endif
    wmove(BASE_WIN,16,51) ; waddstr(BASE_WIN, "              site      ");
    switch(mode)
    {
    case POINT:
	wmove(BASE_WIN,14,53);
	waddstr(BASE_WIN, ">POINT<");
	break;
    case STREAM:
	wmove(BASE_WIN,15,53);
	waddstr(BASE_WIN, ">STREAM<");
	break;
    default:
	break;
    }
    switch(type)
    {
    case LINE:
	wmove(BASE_WIN,14,64);
	waddstr(BASE_WIN, ">LINE<");
	break;
    case AREA:
	wmove(BASE_WIN,15,64);
	waddstr(BASE_WIN, ">AREA EDGE<");
	break;
    case DOT:
	wmove(BASE_WIN,16,64);
	waddstr(BASE_WIN, ">SITE<");
	break;
    default:
	break;
    }
    wrefresh(BASE_WIN);
}

_Base_refresh()
{
    wrefresh (BASE_WIN);
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
/***********************************************************************/
/*
** Interpro computers barfed when going to text mode w/out an endwin()
**  and also barfed when calling V_call() w/out first calling Old_tty()
**  Thus the old method of setting up for both those cases [suspend() and respend()]
**  was not sufficient, so we had to modify suspend(), and make a new vask_suspend()
**  for resetting everything before calling  V_call().
*/
#ifdef INTERPRO
suspend ()
{
    move (LINES-1, 0);
    refresh ();
	endwin ();
}

respend ()
{
    move (0, 0);
    clear ();
    touchwin (curscr);
    touchwin (BASE_WIN);
    touchwin (INFO_WIN);
    wnoutrefresh (BASE_WIN);
    wnoutrefresh (INFO_WIN);
    doupdate ();

}

vask_suspend ()
{
    move (LINES-1, 0);
    refresh ();
    Old_tty ();
}

vask_respend ()
{
    New_tty();
	respend ();
}
#else

vask_suspend () { return suspend (); }
vask_respend () { return respend (); }

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
    touchwin (BASE_WIN);
    touchwin (INFO_WIN);
    wnoutrefresh (BASE_WIN);
    wnoutrefresh (INFO_WIN);
    doupdate ();

}
#endif
/***********************************************************************/


get_type_cnt (type)
    char type;
{
    switch (type)
     {
	 case AREA:
	     return ((int)(CM->n_alines));
	     break;
	 case LINE:
	     return ((int)(CM->n_llines));
	     break;
	 case DOT:
	     return ((int)(CM->n_plines));
	     break;
	 case POINTS:
	     return ((int)(CM->n_points));
	     break;
	 default:
	     return (-1);
	     break;
     }
}	     /*  get_type_cnt ()  */

_Write_help(line, message)
    int line;
    char *message;
{
    wmove(HELP_WIN,line,1); 
    wclrtoeol(HELP_WIN);
    wmove(HELP_WIN,line,1); 
    waddstr(HELP_WIN, message);
    box(HELP_WIN, '#', '#');
    wmove(HELP_WIN,0,0); 
    wnoutrefresh (HELP_WIN);
}

_Help_string (y, x, message)
    int y, x;
    char *message;
{
    wmove(HELP_WIN,y,x); 
    waddstr(HELP_WIN, message);
    wmove(HELP_WIN,0,0); 
    wnoutrefresh (HELP_WIN);
}

Help_string (y, x, message)
    int y, x;
    char *message;
{
    _Help_string (y, x, message);
    /* wrefresh(HELP_WIN); */
    doupdate ();
}

Write_help(line, message)
    int line;
    char *message;
{
    _Write_help (line, message);
    /* wrefresh(HELP_WIN); */
    doupdate ();
}

Show_help ()
{
    wrefresh (HELP_WIN);
}

Hide_help ()
{
    wrefresh (BASE_WIN);
}

_Info_refresh()
{
    wrefresh (INFO_WIN);
}

_Help_refresh()
{
    wrefresh (HELP_WIN);
}

help_get_key ()
{
    int key;
    return (wgetch (HELP_WIN) & 0177);
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

curses_getchar ()
{
    return getch() & 0177;
}
