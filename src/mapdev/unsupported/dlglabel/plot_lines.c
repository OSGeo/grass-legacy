/*  @(#)plot_lines.c	2.1  6/26/87  */
#include "dlg.h"
#include "convert.h"
#include "externs.h"
#include <signal.h>

#define		ALL			0
#define		LABELED		1
#define		UNLABELED	2
#define		SPECIFIC	3

/*
*  file contains functions to highlight lines.  we don't show a lines label,
*  we just highlight it.
*  functions:  plot_all_lines(), plot_l_labels(), plot_lines(),
*/

plot_all_lines()
{
	return (plot_lines (ALL, 0)) ;
}


plot_l_labels()
{
	return (plot_lines ( LABELED, 0)) ;
}


plot_lines( type, att)
	int		type ;
	int		att ;
{
	int i ;
	int n ;
	char buff[64] ;

	switch (type)
	 {
		case ALL:
			sprintf(buff, " PLOTTING ALL LINES") ;
			break ;
		case LABELED:
			sprintf(buff, " PLOTTING LABELED LINES") ;
			break ;
		case UNLABELED:
			sprintf(buff, " PLOTTING UNLABELED LINES") ;
			break ;
		case SPECIFIC:
			sprintf(buff, " PLOTTING LINES WITH ATT: %d", att) ;
			break ;
		default:
			return (-1) ;
			break ;
	 }

	Write_message(2, buff) ;
	Write_message(3, "Hit DEL/RUB to abort") ;

	Old_tty() ;
	set_signals() ;

	for (i=1; i<=tot_lines; i++)
	{
		if (signalflag.interrupt)
			break ;
		if (! line[i].start_node)
		    continue ;
		if (line[i].S > U_north)
		    continue ;
		if (line[i].N < U_south)
		    continue ;
		if (line[i].W > U_east)
		    continue ;
		if (line[i].E < U_west)
		    continue ;

		switch (type)
		 {
			case ALL:
				break ;

			case LABELED:
				if ( ! line[i].n_atts)
		    			continue ;
				break ;

			case UNLABELED:
				if ( line[i].n_atts)
		    			continue ;
				break ;

			case SPECIFIC:
				if ( ! line[i].n_atts)
		    			continue ;
				if ( line[i].atts[1] != att)
		    			continue ;
				break ;

			default:
				break ;
		 }

		if ( (n = plot_line(i, SOLID)) < 0)
		{
			sprintf(buff, " plot_lines:  Error on read (%d)", i) ;
			Write_message(5, buff) ;
			getchar() ;
		}
	}

	New_tty() ;
	Clear_message() ;
	R_flush() ;

	return (1) ;
}
						/*  plot_lines()  */


plot_specific_labels()
{
	int cat_num ;
	char buff[80] ;
	int n ;
	
	Clear_menu() ;
	Clear_message() ;

	for(;;)
	{
		Clear_message() ;
		Write_message(1, "Enter category number ") ;
		Write_message(2, "  (or 0 to quit): ") ;

		Get_curses_text(buff) ;
		sscanf(buff,"%d",&cat_num) ;
		if (cat_num == 0)
		{
			Clear_message() ;
			return(0) ;
		}

		R_standard_color( D_translate_color("blue") ) ;
		plot_lines(SPECIFIC, cat_num) ;

	}
}
					/*  plot_specific_labels()  */

