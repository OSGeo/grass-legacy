/*  @(#)curses.c	2.2  8/3/87  */
#include <curses.h>

WINDOW *BASE_WIN ;
WINDOW *INFO_WIN ;



Init_curses()
{

	Get_old_tty() ;

	initscr () ;
	raw() ;
	crmode() ;
	noecho() ;
	nonl()   ;

	Get_new_tty() ;

	/*         newwin(NROWS, NCOLS, BEGROW, BEGCOL) ; */
	BASE_WIN = newwin(18, 79,  0, 0) ;
	INFO_WIN = newwin( 6, 79, 17, 0) ;

	Clear_base() ;
	Clear_info() ;
}


Close_curses()
{
	clear() ;
	refresh() ;
	mvcur(0, COLS-1, LINES-1, 0) ;
	endwin() ;
}

/*
Write_base_win()
{
	int i ;
	char buf[128] ;

	Clear_base() ;
	for(i=0; i<menu1_lines; i++)
	{
		wmove(BASE_WIN,i+1,1) ;
		waddstr(BASE_WIN, menu1[i]) ;
	}

	sprintf(buf, "%s", head.map_name) ;
	wmove(BASE_WIN,4,16) ;  waddstr(BASE_WIN, buf) ;
	sprintf(buf, "%d", head.orig_scale) ;
	wmove(BASE_WIN,5,16) ;  waddstr(BASE_WIN, buf) ;
	sprintf(buf, "%s", head.your_name) ;
	wmove(BASE_WIN,6,16) ;  waddstr(BASE_WIN, buf) ;
	sprintf(buf,"%8.4lf", head.digit_thresh) ;
	wmove(BASE_WIN,7,16) ;  waddstr(BASE_WIN, buf) ;
	sprintf(buf,"%10.2lf", head.map_thresh) ;
	wmove(BASE_WIN,8,16) ;  waddstr(BASE_WIN, buf) ;

	box (BASE_WIN, '|', '-') ;
	wrefresh(BASE_WIN) ;
}
*/

Write_base(line, message)
	int line ;
	char *message ;
{
	wmove(BASE_WIN,line,1); 
	wclrtoeol(BASE_WIN) ;
	wmove(BASE_WIN,line,1); 
	waddstr(BASE_WIN, message) ;
	wmove(BASE_WIN,0,0); 
	wrefresh(BASE_WIN) ;
}

Write_info(line, message)
	int line ;
	char *message ;
{
	wmove(INFO_WIN,line,1); 
	wclrtoeol(INFO_WIN) ;
	wmove(INFO_WIN,line,1); 
	waddstr(INFO_WIN, message) ;
	wrefresh(INFO_WIN) ;
}

Clear_base()
{
	wclear (BASE_WIN) ;
	werase (BASE_WIN) ;
	touchwin (BASE_WIN) ;
	box (BASE_WIN, '|', '-') ;
	wrefresh(BASE_WIN) ;
}

Clear_info()
{
	wclear (INFO_WIN) ;
	werase (INFO_WIN) ;
	touchwin (INFO_WIN) ;
	box (INFO_WIN, '|', '-') ;
	wrefresh(INFO_WIN) ;
}

Replot_screen() 
{
	wrefresh(curscr) ;
}

Get_curses_char(answer)
	char *answer ;
{
	*answer = wgetch(INFO_WIN) & 0177 ;
}

Get_curses_text(answer)
	char answer[] ;
{
	char newchar ;
	char *pointer ;
	int curx, cury ;

	pointer = answer ;

	for(;;)
	{
		newchar = wgetch(INFO_WIN) & 0177 ;

		if ((newchar > 037) && (newchar < 0177))
		{
			*(pointer++) = newchar ;
			*pointer = 000 ;
			waddch(INFO_WIN,newchar) ;
			wrefresh(INFO_WIN) ;
		}
		else if (newchar == 010)
		{
			if (pointer > answer)
			{
				*(pointer--) = 000 ;
				getyx(INFO_WIN,cury,curx) ;
				wmove(INFO_WIN,cury,curx-1) ;
				waddch(INFO_WIN,' ') ;
				wmove(INFO_WIN,cury,curx-1) ;
				wrefresh(INFO_WIN) ;
			}
		}
		else
			break ;
	}
}

/*
show_mode(mode, type)
	int mode, type ;
{
	wmove(BASE_WIN,14,51) ; waddstr(BASE_WIN, "   point      line      ") ;
	wmove(BASE_WIN,15,51) ; waddstr(BASE_WIN, "   stream     area edge ") ;
	wmove(BASE_WIN,16,51) ; waddstr(BASE_WIN, "              dot       ") ;
	switch(mode)
	{
	case POINT:
		wmove(BASE_WIN,14,53) ;
		waddstr(BASE_WIN, ">POINT<") ;
		break ;
	case STREAM:
		wmove(BASE_WIN,15,53) ;
		waddstr(BASE_WIN, ">STREAM<") ;
		break ;
	default:
		break ;
	}
	switch(type)
	{
	case LINE:
		wmove(BASE_WIN,14,64) ;
		waddstr(BASE_WIN, ">LINE<") ;
		break ;
	case AREA:
		wmove(BASE_WIN,15,64) ;
		waddstr(BASE_WIN, ">AREA EDGE<") ;
		break ;
	case DOT:
		wmove(BASE_WIN,16,64) ;
		waddstr(BASE_WIN, ">DOT<") ;
		break ;
	default:
		break ;
	}
	wrefresh(BASE_WIN) ;
}
*/

curses_yes_no(n, s, ans)
	int n ;
	char *s ;
	char *ans ;
{
	char buff ;
	char *cptr ;

	while (1)
	{
		Write_info(n, s) ;
		buff = wgetch(INFO_WIN) & 0177 ;
		switch (buff)
		{
			case 'Y': case 'y':
				return('y') ;
			case 'N': case 'n':
				return('n') ;
			default:
				if (ans)
					for(cptr=ans; *cptr != 0; cptr++)
						if (buff == *cptr)
							return(*cptr) ;
				Write_info(n, "Please try again") ;
				sleep(1) ;
		}
	}
}

