/*  @(#)curses.c	2.2  8/3/87  */
#include <curses.h>
#include "dlg.h"
#include "menus.h"

WINDOW *newwin() ;
WINDOW *NODE_WIN ;
WINDOW *AREA_WIN ;
WINDOW *LINE_WIN ;
WINDOW *MENU_WIN ;
WINDOW *MESG_WIN ;

Initialize_curses()
{

	Get_old_tty() ;

	initscr () ;
	raw() ;
	/*crmode() ;*/
	noecho() ;
	nonl()   ;

	Get_new_tty() ;

	/*         newwin(NROWS, NCOLS, BEGROW, BEGCOL) ; */
	NODE_WIN = newwin(  8, 50,  0, 30) ;
	AREA_WIN = newwin(  8, 50,  7, 30) ;
	LINE_WIN = newwin(  8, 50, 14, 30) ;
	MENU_WIN = newwin( 16, 31,  0,  0) ;
	MESG_WIN = newwin(  7, 31, 15,  0) ;
	werase (NODE_WIN) ; box (NODE_WIN, '|', '-') ; wrefresh(NODE_WIN) ;
	werase (AREA_WIN) ; box (AREA_WIN, '|', '-') ; wrefresh(AREA_WIN) ;
	werase (LINE_WIN) ; box (LINE_WIN, '|', '-') ; wrefresh(LINE_WIN) ;
	werase (MENU_WIN) ; box (LINE_WIN, '|', '-') ; wrefresh(LINE_WIN) ;
	werase (MESG_WIN) ; box (MESG_WIN, '|', '-') ; wrefresh(MESG_WIN) ;
}


Close_curses()
{
	clear() ;
	refresh() ;
	mvcur(0, COLS-1, LINES-1, 0) ;
	endwin() ;
}

Write_node(num, node)
	int num ;
	struct node *node ;
{
	int i ;
	char buffer[128] ;
	char buff[64]  ;

	werase(NODE_WIN) ;
	wmove(NODE_WIN, 1, 1) ;
	sprintf(buffer,"NODE #:%4d", num) ;
	waddstr(NODE_WIN,buffer) ;

	wmove(NODE_WIN, 2, 1) ;
	sprintf(buffer,"   Easting:  %12.2lf", node->x) ;
	waddstr(NODE_WIN,buffer) ;

	wmove(NODE_WIN, 3, 1) ;
	sprintf(buffer,"   Northing: %12.2lf", node->y) ;
	waddstr(NODE_WIN,buffer) ;

	sprintf(buffer,"   Lines:") ;
	i=0 ;
	while (i<node->n_lines)
	{
		sprintf(buff," %d,",abs(node->lines[i++]) ) ;
		strcat(buffer,buff) ;
	}

	/*
	sprintf(buffer,"   Lines: %d", node->n_lines) ;
	*/
	wmove(NODE_WIN, 4, 1) ;
	waddstr(NODE_WIN,buffer) ;

	sprintf(buffer,"   Atts:") ;
	i=0 ;
	while (i<node->n_atts)
	{
		sprintf(buff," %d",node->atts[i++]) ;
		strcat(buffer,buff) ;
	}
	wmove(NODE_WIN, 5, 1) ;
	waddstr(NODE_WIN,buffer) ;

	box (NODE_WIN, '|', '-') ;
	wrefresh(NODE_WIN) ;
	return(0) ;
}

Write_area(num, area)
	int num ;
	struct area *area ;
{
	int i ;
	int j ;
	char buffer[128] ;
	char buff[64]  ;

	werase(AREA_WIN) ;
	wmove(AREA_WIN, 1, 1) ;
	sprintf(buffer,"AREA #:%4d", num) ;
	waddstr(AREA_WIN,buffer) ;

	wmove(AREA_WIN, 2, 1) ;
	sprintf(buffer,"   Easting:  %12.2lf", area->x) ;
	waddstr(AREA_WIN,buffer) ;

	wmove(AREA_WIN, 3, 1) ;
	sprintf(buffer,"   Northing: %12.2lf", area->y) ;
	waddstr(AREA_WIN,buffer) ;

	wmove(AREA_WIN, 4, 1) ;
/*
	sprintf(buffer,"   Lines:") ;
	i=0 ;
	while (i<area->n_lines)
	{
		sprintf(buff," %d",area->lines[i++]) ;
		strcat(buffer,buff) ;
	}
*/
	sprintf(buffer,"   Lines: %d  Islands: %d", area->n_lines, area->n_isles) ;
	waddstr(AREA_WIN,buffer) ;

	wmove(AREA_WIN, 5, 1) ;
	sprintf(buffer,"   Atts:") ;
	j = area->n_atts ;
	if (j > 3) j = 3 ;
	for(i=0; i<j; i++)
	{
		sprintf(buff," %d-%d",area->atts[i*2], area->atts[i*2+1]) ;
		strcat(buffer,buff) ;
	}
	waddstr(AREA_WIN,buffer) ;

	box (AREA_WIN, '|', '-') ;
	wrefresh(AREA_WIN) ;
	return(0) ;
}

Write_line(num, line)
	int num ;
	struct line *line ;
{
	int i ;
	char buffer[128] ;
	char buff[64]  ;

	werase(LINE_WIN) ;
	wmove(LINE_WIN, 1, 1) ;
	sprintf(buffer,"LINE #:%4d", num) ;
	waddstr(LINE_WIN,buffer) ;

	wmove(LINE_WIN, 2, 1) ;
	sprintf(buffer,"   Nodes: Start:%4d  End:%4d",
		line->start_node, line->end_node) ;
	waddstr(LINE_WIN,buffer) ;

	wmove(LINE_WIN, 3, 1) ;
	sprintf(buffer,"   Areas: Left:%4d  Right:%4d",
		line->left_area, line->right_area) ;
	waddstr(LINE_WIN,buffer) ;

	wmove(LINE_WIN, 4, 1) ;
	sprintf(buffer,"   Number of Coordinates: %d", line->n_coors) ;
	waddstr(LINE_WIN,buffer) ;

	wmove(LINE_WIN, 5, 1) ;
	sprintf(buffer,"   Atts:") ;
	i=0 ;
	for ( i=0; i<line->n_atts; i++)
	{								/*  major,   minor  */
		sprintf(buff," %d-%d ",line->atts[i*2], line->atts[i*2+1]) ;
		strcat(buffer,buff) ;
	}
	waddstr(LINE_WIN,buffer) ;

	box (LINE_WIN, '|', '-') ;
	wrefresh(LINE_WIN) ;
	return(0) ;
}

Write_menu(menu)
	int menu ;
{
	int i ;

	werase (MENU_WIN) ;
	for(i=0; i<N_MENU_LINES; i++)
	{
		wmove(MENU_WIN,i+1,1) ; 
		waddstr(MENU_WIN, menus[menu][i]) ;
	}
	box (MENU_WIN, '|', '-') ;
	wrefresh(MENU_WIN) ;
}

Write_message(line, message)
	int line ;
	char *message ;
{
	wmove(MESG_WIN,line,1); 
	wclrtoeol(MESG_WIN) ;
	wmove(MESG_WIN,line,1); 
	waddstr(MESG_WIN, message) ;
	wrefresh(MESG_WIN) ;
}

Clear_area()
{
	werase (AREA_WIN) ;
	wclear (AREA_WIN) ;
	touchwin (AREA_WIN) ;
	box (AREA_WIN, '|', '-') ;
	wrefresh(AREA_WIN) ;
}

Clear_line()
{
	werase (LINE_WIN) ;
	wclear (LINE_WIN) ;
	touchwin (LINE_WIN) ;
	box (LINE_WIN, '|', '-') ;
	wrefresh(LINE_WIN) ;
}

Clear_node()
{
	werase (NODE_WIN) ;
	wclear (NODE_WIN) ;
	touchwin (NODE_WIN) ;
	box (NODE_WIN, '|', '-') ;
	wrefresh(NODE_WIN) ;
}

Clear_message()
{
	werase (MESG_WIN) ;
	wclear (MESG_WIN) ;
	touchwin (MESG_WIN) ;
	box (MESG_WIN, '|', '-') ;
	wrefresh(MESG_WIN) ;
}

Clear_menu()
{
	werase (MENU_WIN) ;
	wclear (MENU_WIN) ;
	touchwin (MENU_WIN) ;
	box (MENU_WIN, '|', '-') ;
}

Write_menu_line(line, message)
	int line ;
	char *message ;
{
	wmove(MENU_WIN,line,1); 
	wclrtoeol(MENU_WIN) ;
	wmove(MENU_WIN,line,1); 
	waddstr(MENU_WIN, message) ;
	wrefresh(MENU_WIN) ;
}

Replot_screen() 
{
	wrefresh(curscr) ;
}

Get_curses_text(answer)
	char answer[] ;
{
	char newchar ;
	char *pointer ;
	int curx, cury ;
	int size ;

	pointer = answer ;

	for(;;)
	{
		newchar = wgetch(MESG_WIN) & 0177 ;

		if ((newchar > 037) && (newchar < 0177))
		{
			*(pointer++) = newchar ;
			*pointer = 000 ;
			waddch(MESG_WIN,newchar) ;
			wrefresh(MESG_WIN) ;
		}
		else if (newchar == 010)
		{
			if (pointer > answer)
			{
				*(pointer--) = 000 ;
				getyx(MESG_WIN,cury,curx) ;
				wmove(MESG_WIN,cury,curx-1) ;
				waddch(MESG_WIN,' ') ;
				wmove(MESG_WIN,cury,curx-1) ;
				wrefresh(MESG_WIN) ;
			}
		}
		else
			break ;
	}
}

curses_yes_no(n, s)
	int n ;
	char *s ;
{
	char newchar ;
	while (1)
	{
		Write_message(n, s) ;
		newchar = wgetch(MESG_WIN) & 0177 ;
		switch (newchar)
		{
			case 'Y': case 'y':
				return(1) ;
			case 'N': case 'n':
				return(0) ;
			default:
				Write_message(n, "Please answer yes or no") ;
				sleep(2) ;
		}
	}
}

