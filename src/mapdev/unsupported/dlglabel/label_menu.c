/*  @(#)label_menu.c	2.1  6/26/87  */
#include "menus.h"

label_menu()
{
	int cur_char ;
	char buffer[64] ;

	Write_menu(LABEL_MENU) ;

	while(1) 
	{
		R_flush() ;
		cur_char = getchar() & 0177 ;
		sprintf(buffer,"  %c",cur_char) ;
		Write_message(2, buffer) ;
		switch(cur_char)
		{
			case 'a':
				label_area() ;
				Write_menu(LABEL_MENU) ;
				break;

			case 'l':
				label_lines() ;
				Write_menu(LABEL_MENU) ;
				break;

			case 'L':
				unlabel_lines() ;
				Write_menu(LABEL_MENU) ;
				break;

			case 'A':
				unlabel_areas() ;
				Write_menu(LABEL_MENU) ;
				break;

			case 's':
				plot_specific_labels() ;
				Write_menu(LABEL_MENU) ;
				break;

			case 'w':
				Clear_menu() ;
				where_am_i() ;
				Write_menu(LABEL_MENU) ;
				Clear_message() ;
				break ;

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
