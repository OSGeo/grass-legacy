#include <string.h>
#include <unistd.h>
#include <curses.h>
#include "gis.h"
#include "vask.h"
#include "digit.h"
#include "menu.h"
#include "ginput.h"
#include "dig_curses.h"
#include "local_proto.h"

/*  @(#)curses.c    2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

WINDOW *BASE_WIN;
WINDOW *INFO_WIN;
WINDOW *HELP_WIN;
WINDOW *COVR_WIN;
#define dig_menu_lines 16

static int _curses_state = 0 ;

#define G_CLEAR_WIN(WIN)    { werase(WIN);}
/*
#ifdef SYSV
#define G_CLEAR_WIN(WIN)    { werase(WIN);}
#else
#define G_CLEAR_WIN(WIN)    { werase(WIN); wclear(WIN); }
#endif
*/


int Init_curses (void)
{

/*setbuf(stdout, NULL) ;*/    /* ?? -dpg */
/* setbuf(stderr, NULL); */

    /*	This is done by the V_call in get_head_binary () called from main
    **  but doesn't hurt to leave this here in case things change later
    */
    V_init ();
    crmode ();		/* but set crmode in ours */
    noecho();

    _Curses_on() ;


    /*	 newwin(NROWS, NCOLS, BEGROW, BEGCOL) ; */
    BASE_WIN = newwin (19, 79,  0, 0);
    INFO_WIN = newwin ( 6, 79, 18, 0);
    HELP_WIN = newwin (17, 49, 1, 28);
    COVR_WIN = newwin (18, 49, 0,  0);

    scrollok (stdscr, 0);
    scrollok (INFO_WIN, 0);

    Clear_base();
    Clear_info();
    _Clear_help();

    return 0;
}


int Close_curses (void)
{
    if (Curses_state()  ==  0)
	return(0) ;

    clear();
    refresh();
    mvcur(0, COLS-1, LINES-1, 0);
    endwin();
    return(0) ;
}

int _Write_base_win (void)
{
    int i;

    _Clear_base();
    for(i=0; i<menu1_lines; i++)
    {
	wmove(BASE_WIN,i+1,1);
	waddstr(BASE_WIN, menu1[i]);
    }
    _Show_version ();
    box (BASE_WIN, '|', '-');

    return 0;
}

int Write_base_win (void)
{
    _Write_base_win ();
    wrefresh(BASE_WIN);

    return 0;
}

/*
Write_edit_win()
{
    Write_edit_win (&M_edit);
    int i;

    _Clear_base();
    for(i=0; i<menu2_lines; i++)
    {
	wmove(BASE_WIN,i+1,1);
	waddstr(BASE_WIN, menu2[i]);
    }
    _Show_version ();

    update_menu (&M_edit);
    wrefresh(BASE_WIN);

    return 0;
}
*/

int _Write_generic_win (struct Menu_head *menu)
{
    int i;

    _Clear_base();
    for(i=0; i<menu2_lines; i++)
    {
	wmove(BASE_WIN,i+1,1);
	waddstr(BASE_WIN, menu2[i]);
    }
    _Show_version ();

    _write_generic_title (menu->name);
    update_menu (menu);

    return 0;
}

int Write_generic_win (struct Menu_head *menu)
{
    _Write_generic_win (menu);
    wrefresh(BASE_WIN);

    return 0;
}

int _write_generic_title (char *name)
{
    char buf[200];

    sprintf (buf, "%s Menu", name);
    _Base_string (1, 77 - strlen (buf), buf);

    return 0;

}


/* 
*  _Write_dig_win()  contains information that is digitizer specific
*/

#define WHITE_OUT "                                                "

/* display options that are available only while digitizing */
int Dig_menu_opts (void)
{
    wrefresh (COVR_WIN);

    return 0;
}

int _Write_mouse_covr (void)
{
    register int i;
    wmove (BASE_WIN, 3, 1);
    wmove (COVR_WIN, 3, 1);
    waddstr (BASE_WIN, "  Mouse Digitizer                               ");
    waddstr (COVR_WIN, "  Mouse Digitizer                               ");
    for(i=4; i < 9 ; i++)
    {
	wmove (BASE_WIN, i, 1);
	waddstr (BASE_WIN, WHITE_OUT);
	waddstr (COVR_WIN, WHITE_OUT);
    }

    return 0;
}

/* put up digitizer specific menu */
/*  but remove the actual digitizing commands until needed */
int _Write_dig_win (void)
{
    static int first=0;
    int i;
    char dig_menu[dig_menu_lines][80];
    
    if (!first)
        make_dig_win(dig_menu);

    _Clear_base();
    for(i=0; i<dig_menu_lines; i++)
    {
	wmove(BASE_WIN,i+1,1);
	waddstr(BASE_WIN, dig_menu[i]);
    }

    _Show_version ();
    /* copy digitizer menu to COVR_WIN */
    overwrite (BASE_WIN, COVR_WIN);

    /* clean out the top of BASE_WIN */
    for(i=4; i < 9 ; i++)
    {
	wmove(BASE_WIN,i,1);
	waddstr(BASE_WIN, WHITE_OUT);
    }

    /* clean out the bottom of COVR_WIN */
    for(i=11; i < 18 ; i++)
    {
	wmove(COVR_WIN,i,1);
	waddstr(COVR_WIN, WHITE_OUT);
    }

    /* adjust digitizer info for mouse if that is enabled */
    if (Digtiz_Device == MOUSE)
	_Write_mouse_covr ();

    update_menu (&M_digit);

    return 0;
}


int Write_dig_win (void)
{
    _Write_dig_win ();
    wrefresh(BASE_WIN);

    return 0;
}


int _Write_header_info (void)
{
    char buf[128];
    char tbuf[128];
    sprintf(buf, "%-.30s", CMap->head.map_name) ;	wmove(BASE_WIN,4,18) ;  waddstr(BASE_WIN, buf);
    sprintf(buf, "%-ld", CMap->head.orig_scale) ;      wmove(BASE_WIN,5,18) ;  waddstr(BASE_WIN, buf);
    sprintf(buf, "%-.30s", CMap->head.your_name) ;       wmove(BASE_WIN,6,18) ;  waddstr(BASE_WIN, buf);
    sprintf(buf,"%s in.", dig_float_point (tbuf, 6, CMap->head.digit_thresh)) ; wmove(BASE_WIN,7,18) ;  waddstr(BASE_WIN, buf);
    sprintf(buf,"%s %s", dig_float_point (tbuf, 6, CMap->head.map_thresh), G_database_unit_name (1));
    wmove(BASE_WIN,8,18) ;  waddstr(BASE_WIN, buf);
    /*
    sprintf(buf, "%20.20s", CMap->head.map_name) ;	wmove(BASE_WIN,4,16) ;  waddstr(BASE_WIN, buf);
    sprintf(buf, "%20ld", CMap->head.orig_scale) ;      wmove(BASE_WIN,5,16) ;  waddstr(BASE_WIN, buf);
    sprintf(buf, "%20.20s", CMap->head.your_name) ;       wmove(BASE_WIN,6,16) ;  waddstr(BASE_WIN, buf);
    sprintf(buf,"%8.5lf", CMap->head.digit_thresh) ; wmove(BASE_WIN,7,16) ;  waddstr(BASE_WIN, buf);
    sprintf(buf,"%10.2lf", CMap->head.map_thresh) ;  wmove(BASE_WIN,8,16) ;  waddstr(BASE_WIN, buf);
    */

    box (BASE_WIN, '|', '-');

    return 0;
}

int Write_header_info (void)
{
    _Write_header_info ();
    wrefresh(BASE_WIN);

    return 0;
}

int _Write_type_info (void)
{
    char buf[128];
    sprintf(buf,"%d", get_type_cnt (LINE)) ;  wmove(BASE_WIN,4,70) ;  waddstr(BASE_WIN, buf);
    sprintf(buf,"%d", get_type_cnt (AREA)) ;  wmove(BASE_WIN,5,70) ;  waddstr(BASE_WIN, buf);
    sprintf(buf,"%d", get_type_cnt (DOT)) ;  wmove(BASE_WIN,6,70) ;  waddstr(BASE_WIN, buf);
    sprintf(buf,"%d", get_type_cnt (POINTS)) ;  wmove(BASE_WIN,8,70) ;  waddstr(BASE_WIN, buf);

    return 0;
}

int Write_type_info (void)
{
    _Write_type_info ();
    wrefresh(BASE_WIN);

    return 0;
}


int _Write_base (int line, char *message)
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

int _Base_string (int y, int x, char *message)
{
    wmove(BASE_WIN,y,x); 
    waddstr(BASE_WIN, message);
    wmove(BASE_WIN,0,0); 

    return 0;
}

int Base_string (int y, int x, char *message)
{
    _Base_string (y, x, message);
    wrefresh(BASE_WIN);

    return 0;
}

int Write_base (int line, char *message)
{
    _Write_base (line, message);
    wrefresh(BASE_WIN);

    return 0;
}


int _Write_info (int line, char *message)
{
    wmove(INFO_WIN,line,1); 
    wclrtoeol(INFO_WIN);
    wmove(INFO_WIN,line,1); 
    waddstr(INFO_WIN, message);
    box (INFO_WIN, '|', '-');

    return 0;
}

int Write_info (int line, char *message)
{
    _Write_info(line, message);
    wrefresh(INFO_WIN);

    return 0;
}


int Clear_help (void)
{
    _Clear_help ();
    wrefresh(HELP_WIN);

    return 0;
}

int _Clear_help (void)
{
    int i;

    G_CLEAR_WIN (HELP_WIN);
    box (HELP_WIN, '|', '-');

    return 0;
}

int Clear_base (void)
{
    _Clear_base ();
    /*
    touchwin(BASE_WIN);
    */
    wrefresh(BASE_WIN);

    return 0;
}

int _Clear_base (void)
{
    int i;

    G_CLEAR_WIN (BASE_WIN);
    box (BASE_WIN, '|', '-');

    return 0;
}

int Clear_info (void)
{

    _Clear_info();
/*
    touchwin(INFO_WIN);
*/
    wrefresh (INFO_WIN);

    return 0;
}

int _Clear_info (void)
{

    G_CLEAR_WIN (INFO_WIN);
    box (INFO_WIN, '|', '-');

    return 0;
}

int Replot_screen (void) 
{
    wrefresh(curscr);

    return 0;
}

int Get_curses_char (char *answer)
{
#ifdef ASIAN_CHARS
    *answer = wgetch(INFO_WIN);
#else
    *answer = wgetch(INFO_WIN) & 0177;
#endif

    return 0;
}

int Get_curses_text (char answer[])
{
    char newchar;
    char *pointer;
    int curx, cury;
    int size;

    pointer = answer;

    *answer = 0;
    for(;;)
    {
#ifdef ASIAN_CHARS
	newchar = wgetch(INFO_WIN);

	if (((newchar > 037) && (newchar < 0177)) || newchar < 0)
#else
	newchar = wgetch(INFO_WIN) & 0177;

	if ((newchar > 037) && (newchar < 0177))
#endif
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

    return 0;
}

int _show_mode (int mode, int type, int label)
{
    char buffer[128] ;

    wmove(BASE_WIN,12,51) ; waddstr(BASE_WIN, " MODE       TYPE        ");
    wmove(BASE_WIN,13,51) ; waddstr(BASE_WIN, "   point      line      ");
    wmove(BASE_WIN,14,51) ; waddstr(BASE_WIN, "   stream     area edge ");
    wmove(BASE_WIN,15,51) ; waddstr(BASE_WIN, "              site      ");
#ifndef SCS_MODS 
    wmove(BASE_WIN,16,51) ; waddstr(BASE_WIN, "                        ");
#else
    wmove(BASE_WIN,16,51) ; waddstr(BASE_WIN, "              psu       ");
#endif
    wmove(BASE_WIN,17,51) ; waddstr(BASE_WIN, " AutoLabel:             ");

    if (!label)
    {
	wmove (BASE_WIN, 17, 64); waddstr (BASE_WIN,  "DISABLED");
    }
    else
    {
#ifndef SCS_MODS
	if(Cat_name)
	{
	    char *p;

	    p = G_store(Cat_name);
	    if(strlen(p) > 15)
	        *(p+14) = 0;

	    sprintf (buffer, " Category: %s%c", p,
			    (strlen(Cat_name) > 15 ? '~' : ' '));
	    wmove (BASE_WIN, 16, 51); waddstr (BASE_WIN,  buffer);
	}
#endif
	sprintf (buffer, "%4d      ", label);
	wmove (BASE_WIN, 17, 64); waddstr (BASE_WIN,  buffer);
    }
    
    switch(mode)
    {
    case POINT:
	wmove(BASE_WIN,13,53);
	waddstr(BASE_WIN, ">POINT<");
	break;
    case STREAM:
	wmove(BASE_WIN,14,53);
	waddstr(BASE_WIN, ">STREAM<");
	break;
    default:
	break;
    }
    switch(type)
    {
    case LINE:
	wmove(BASE_WIN,13,64);
	waddstr(BASE_WIN, ">LINE<");
	break;
    case AREA:
	wmove(BASE_WIN,14,64);
	waddstr(BASE_WIN, ">AREA EDGE<");
	break;
    case DOT:
	wmove(BASE_WIN,15,64);
	waddstr(BASE_WIN, ">SITE<");
	break;
#ifdef SCS_MODS
    case PSU:
	wmove(BASE_WIN,16,64);
	waddstr(BASE_WIN, ">PSU<");
	break;
#endif /* SCS_MODS */
    default:
	break;
    }

    return 0;
}

int show_mode (int mode, int type, int label)
{
    _show_mode (mode, type, label);
    wrefresh(BASE_WIN);

    return 0;
}

int _Base_refresh (void)
{
    wrefresh (BASE_WIN);

    return 0;
}

int curses_yes_no (int n, char *s)
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

    return 0;
}

int curses_yes_no_default (int n, char *str, int def)
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

    return 0;
}

int mysuspend (void)
{
    move (LINES-1, 0);
    refresh ();
    endwin ();

    return 0;
}

int myrespend (void)
{
    crmode ();
    noecho();

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

int vask_suspend (void)
{
    /*
    move (LINES-1, 0);
    refresh ();
    */
    endwin ();
    fflush (stdout);

    return 0;
}

int vask_respend (void)
{
    myrespend ();

    return 0;
}

int get_type_cnt (int type)
{
    switch (type)
     {
	 case AREA:
	     return ((int)(CMap->n_alines));
	     break;
	 case LINE:
	     return ((int)(CMap->n_llines));
	     break;
	 case DOT:
	     return ((int)(CMap->n_plines));
	     break;
	 case POINTS:
	     return ((int)(CMap->n_points));
	     break;
	 default:
	     return (-1);
	     break;
     }

    return 0;
}	     /*  get_type_cnt ()  */

int _Write_help (int line, char *message)
{
    wmove(HELP_WIN,line,1); 
    wclrtoeol(HELP_WIN);
    wmove(HELP_WIN,line,1); 
    waddstr(HELP_WIN, message);
    box(HELP_WIN, '#', '#');
    wmove(HELP_WIN,0,0); 

    return 0;
}

int _Help_string (int y, int x, char *message)
{
    wmove(HELP_WIN,y,x); 
    waddstr(HELP_WIN, message);
    wmove(HELP_WIN,0,0); 

    return 0;
}

int Help_string (int y, int x, char *message)
{
    _Help_string (y, x, message);
    wrefresh(HELP_WIN);

    return 0;
}

int Write_help (int line, char *message)
{
    _Write_help (line, message);
    wrefresh(HELP_WIN);

    return 0;
}

int Show_help (void)
{
    /*
    overwrite (HELP_WIN, BASE_WIN);
    */
    wrefresh (HELP_WIN);

    return 0;
}

int Hide_help (void)
{
    /* 
    overwrite (BASE_WIN, HELP_WIN);
    overwrite (INFO_WIN, HELP_WIN);
    */
    wrefresh (BASE_WIN);
    wrefresh (INFO_WIN);

    return 0;
}

int _Info_refresh (void)
{
    wrefresh (INFO_WIN);

    return 0;
}

int _Help_refresh (void)
{
    wrefresh (HELP_WIN);

    return 0;
}

int help_get_key (void)
{
    int key;
    return (wgetch (HELP_WIN));

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

int curses_getchar (void)
{
    /* TESTING 1/91  dpg */
    /* return ((getch ()) & 0177); */
#ifdef ASIAN_CHARS
    return (wgetch (BASE_WIN));
#else
    return ((wgetch (BASE_WIN)) & 0177);
#endif
}

int _Write_covr (int line, char *message)
{
    wmove(COVR_WIN,line,1); 
    wclrtoeol(COVR_WIN);
    wmove(COVR_WIN,line,1); 
    waddstr(COVR_WIN, message);
    wmove(COVR_WIN,0,0); 

    return 0;
}

int _Covr_string (int y, int x, char *message)
{
    wmove(COVR_WIN,y,x); 
    waddstr(COVR_WIN, message);
    wmove(COVR_WIN,0,0); 

    return 0;
}

int Covr_string (int y, int x, char *message)
{
    _Covr_string (y, x, message);
    wrefresh(COVR_WIN);

    return 0;
}

int Write_covr (int line, char *message)
{
    _Write_covr (line, message);
    wrefresh(COVR_WIN);

    return 0;
}


int Clear_covr (void)
{
    _Clear_covr ();
    wrefresh(COVR_WIN);

    return 0;
}

int _Clear_covr (void)
{
    G_CLEAR_WIN (COVR_WIN);
    box (COVR_WIN, '|', '-');

    return 0;
}


int _Covr_refresh (void)
{
    wrefresh (COVR_WIN);

    return 0;
}


int _Show_version (void)
{
    char buf[100];
    /* write out title and version number */
    wmove (BASE_WIN, 1, 2);
    sprintf(buf,"GRASS-DIGIT Modified %2d.%02d  ", VERSION_MAJOR, VERSION_MINOR);
    waddstr (BASE_WIN, buf);

    return 0;
}

int Curses_error (char *str)
{
    char buf[100];
    _Clear_info ();
    BEEP;
    Write_info (2, str);
    Write_info (4, "     Press <Return> to continue");
    Get_curses_text (buf);

    return (-1);
}

int make_dig_win (char buf[16][80])
{
    int i;
    
    i = digdevice.buttonstart;
    sprintf (buf[0], 
" GRASS-DIGIT Version %2d.%02d                               Digitizing menu", 
	    VERSION_MAJOR, VERSION_MINOR);
    sprintf (buf[1],
"----------------------------------------------------------------------------");
    sprintf (buf[2], 
"  %20s digitizer                 | AMOUNT DIGITIZED",digdevice.digname);
    sprintf (buf[3],
"                  Cursor keys:                  |   # Lines:");
    sprintf (buf[4],
"                 <%d>  digitize point            |   # Area edges:", i++);
    sprintf (buf[5],
"                 <%d>  quit digitizing           |   # Sites:", i++);
    sprintf (buf[6],
"                 <%d>  update monitor            | - - - - - - - - - - - - - -", i++);
    sprintf (buf[7],
"                 <%d>  toggle point/stream mode  |   Total points:", i++);
    sprintf (buf[8],
"-----------------------------------------------------------------------------");
    sprintf (buf[9],
"                                                | CURRENT DIGITIZER PARAMS.");
    sprintf (buf[10],
"                                                |");
    sprintf (buf[11],
"                                                |");
    sprintf (buf[12],
"                                                | MODE       TYPE");
    sprintf (buf[13],
"                                                |   point      line");
    sprintf (buf[14],
"                                                |   stream     area edge");
    sprintf (buf[15],
"                                                |");

    return 0;
} 

