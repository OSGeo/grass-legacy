/*  @(#)plot_menu.c	2.1  6/26/87  */
#include "menus.h"
#include "dlg.h"

static long cur_area = 0 ;
static long cur_line = 0 ;
static long cur_node = 0 ;

plot_menu()
{
	char cur_char ;
	char buffer[128] ;
	char answer[64] ;
	long atoi() ;
	int choice ;

	Write_menu(PLOT_MENU) ;

	while(1) 
	{
		R_flush() ;
		cur_char = getchar() & 0177 ;
		sprintf(buffer,"  %c",cur_char) ;
		Write_message(2, buffer) ;
		switch(cur_char)
		{
			case 'a':
				R_standard_color(D_translate_color("orange") ) ;
				plot_a_cents() ;
				break;
			case 'n':
				R_standard_color(D_translate_color("green") ) ;
				plot_all_nodes() ;
				break;
			case 'l':
				R_standard_color(D_translate_color("gray") ) ;
				plot_all_lines() ;
				break;
			case 'A':
				R_standard_color(D_translate_color("orange") ) ;
				plot_a_labels() ;
				break;

			case 'L':
				R_standard_color(D_translate_color("violet") ) ;
				plot_l_labels() ;
				break;
/*
				Clear_menu() ;
				if (choice = choose_area() )
				{
					R_standard_color(D_translate_color("gray") ) ;
					plot_alines(choice, SOLID) ;
				}
				Write_menu(PLOT_MENU) ;
				break;
			case 'L':
				Clear_menu() ;
				if (choice = choose_line() )
				{
					Write_line(choice, &line[choice]) ;
					full_plot_line(choice, "gray", "gray", "gray", "gray") ;
				}
				Write_menu(PLOT_MENU) ;
				break ;
*/
			case 'W':
				push_window() ;
				Write_menu(PLOT_MENU) ;
				Clear_message() ;
				break ;
			case 'P':
				pop_window() ;
				break ;
			case 'C':
				outline_map() ;
				break ;
			case 'w':
				Clear_menu() ;
				where_am_i() ;
				Write_menu(PLOT_MENU) ;
				Clear_message() ;
				break ;
			case 's':
				add_scale();
				break;
			case '*':
				Replot_screen();
				break;
			case 'q':
				return(0) ;
			default:
				sprintf(buffer,"  %c - Unknown Command",cur_char) ;
				Write_message(2, buffer) ;
				break ;
		}
	}
}
