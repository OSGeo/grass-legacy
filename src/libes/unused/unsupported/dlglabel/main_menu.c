/*  @(#)main_menu.c	2.1  6/26/87  */
#include "menus.h"
#include "convert.h"
#include <stdio.h>

main_menu()
{
	char cur_char ;
	char buffer[128] ;
	int choice ;

	c_wind = 0 ;

	_west[0] = U_west ;
	_east[0] = U_east ;
	_south[0] = U_south ;
	_north[0] = U_north ;

	Initialize_curses() ;

	Write_menu(MAIN_MENU) ;

	while(1) 
	{
		R_flush() ;
		cur_char = getchar() & 0177 ;
		sprintf(buffer,"  %c",cur_char) ;
		Write_message(2, buffer) ;
		switch(cur_char)
		{
			case 'P':
				plot_menu() ;
				Write_menu(MAIN_MENU) ;
				break;

			case 'L':
				label_menu() ;
				Write_menu(MAIN_MENU) ;
				break;

			case 'R':
				report() ;
				Replot_screen() ;
				Write_menu(MAIN_MENU) ;
				break ;

			case 'n':
				Clear_menu() ;
				choice = choose_node() ;
				Clear_message() ;
				if (choice)
					node_menu(choice) ;
				Write_menu(MAIN_MENU) ;
				break;

			case 'l':
				Clear_menu() ;
				choice = choose_line() ;
				Clear_message() ;
				if (choice)
				{
					line_menu(choice) ;
				}
				Write_menu(MAIN_MENU) ;
				break;

			case 'a':
				Clear_menu() ;
				choice = choose_area() ;
				Clear_message() ;
				if (choice)
					area_menu(choice) ;
				Write_menu(MAIN_MENU) ;
				break;

			case 'W':
				Old_tty() ;
				clear_screen() ;
				write_dlg() ;
				New_tty() ;
				Replot_screen();
				break ;

			case '*':
				Replot_screen();
				break;

			case 'Q':
				if (curses_yes_no ( 2,
					"  Write this session before    quitting (y/n):  ") )
				{
					Old_tty() ;
					clear_screen() ;
					write_dlg() ;
				}

				Close_curses() ;
				return(0) ;

				/*  not on menu  */
			case 'z':
				debug_menu();
				Write_menu(MAIN_MENU) ;
				break;

			default:
				sprintf(buffer,"  %c - Unknown Command",cur_char) ;
				Write_message(2, buffer) ;
				break ;
		}
	}
}
