/*  @(#)inspect_a.c	2.1  6/26/87  */
#include "dlg.h"

inspect_area(cur_area)
	int cur_area ;
{
	int i ;
	char buffer[64] ;
	int cur_line ;

	for(i=0; i<area[cur_area].n_lines; i++)
	{
		cur_line = abs(area[cur_area].lines[i]) ;
		Write_line(cur_line, &line[cur_line] ) ;
		full_plot_line(cur_line, "red", "yellow", "red", "yellow") ;
		R_flush() ;

		Write_message(5," Continue? [y] ") ;
		Get_curses_text(buffer) ;
		Clear_message() ;
		if (*buffer == 'n')
		{
			re_full_plot_line("black", "black", "blue", "blue") ;
			break ;
		}

		re_full_plot_line("black", "black", "blue", "blue") ;
		R_flush() ;
	}
}
