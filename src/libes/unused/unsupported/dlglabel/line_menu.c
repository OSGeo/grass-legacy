/*  @(#)line_menu.c	2.1  6/26/87  */
#include "dlg.h"
#include "menus.h"

line_menu(choice)
	int choice ;
{
	char cur_char ;
	char buffer[128] ;

	Write_menu(LINE_MENU) ;
	Write_line(choice, &line[choice]) ;
	full_plot_line(choice, "red", "blue", "red", "blue") ;

	while(1) 
	{
		R_flush() ;
		cur_char = getchar() & 0177 ;
		sprintf(buffer,"  %c",cur_char) ;
		Write_message(2, buffer) ;
		switch(cur_char)
		{
			case 'n': /* Change nodes  */
				Clear_menu() ;
				line_nodes(choice) ;
				Clear_message() ;
				Write_menu(LINE_MENU) ;
				Write_line(choice, &line[choice]) ;
				re_full_plot_line("black", "black", "black", "black") ;
				full_plot_line(choice, "red", "blue", "red", "blue") ;
				break ;
			case 'a': /* Change areas  */
				Clear_menu() ;
				line_areas(choice) ;
				Clear_message() ;
				Write_menu(LINE_MENU) ;
				Write_line(choice, &line[choice]) ;
				break ;
			case 's': /* Split line  */
				Write_message(2, "Split not yet available") ;
				break ;
			case 'r': /* Re-Draw line  */
				re_full_plot_line("black", "black", "black", "black") ;
				R_flush() ;
				make_line(choice, 1) ;
				full_plot_line(choice, "red", "blue", "red", "blue") ;
				Clear_message() ;
				Write_menu(LINE_MENU) ;
				break ;
			case 'x': /* Remove line  */
				re_full_plot_line("black", "black", "black", "black") ;
				remove_line(choice) ;
				return(0) ;
				break ;
			case 'e': /* Edit category codes */
				edit_l_atts(choice) ;
				Write_menu(LINE_MENU) ;
				Write_line(choice, &line[choice]) ;
				break ;
			case '*':
				Replot_screen();
				break;
			case 'q':
				re_full_plot_line("black", "black", "gray", "gray") ;
				return(0) ;
			default:
				sprintf(buffer,"  %c - Unknown Command",cur_char) ;
				Write_message(2, buffer) ;
				break ;
		}
	}
}
