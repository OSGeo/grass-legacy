/*  @(#)area_menu.c	2.1  6/26/87  */
#include "dlg.h"
#include "menus.h"

area_menu(choice)
	int choice ;
{
	char cur_char ;
	char buffer[128] ;
	double x, y ;
	char do_win ;

/* Show area info on screen */
	Write_area(choice, &area[choice]) ;

/* Ask if auto-zoom desired */
	Write_message(2, "Auto zoom? y/n [n]> ") ;
	Get_curses_text(buffer) ;
	switch(*buffer)
	{
		case 'y': case 'Y':
			Write_message(3, "  ZOOMING") ;
			auto_area_zoom(choice) ;
			do_win = 1 ;
			break ;
		default:
			do_win = 0 ;
			break ;
	}
	Clear_message() ;

/* Draw lines that claim to be attached to area */
	Write_message(2, "Orange: Lines claiming area") ;
	Write_message(5, "Hit DEL/RUB to abort") ;
	R_standard_color( D_translate_color("orange")) ;
	plot_alines(choice, SOLID) ;

/* Draw lines that area claims bound it */
	Write_message(3, "Red: Lines claimed by area") ;
	R_standard_color( D_translate_color("red")) ;
	plot_area(choice, SOLID) ; 
	R_standard_color( D_translate_color("violet")) ;
	plot_isles(choice, SOLID) ; 
	Clear_message() ;

	Write_menu(AREA_MENU) ;

	while(1) 
	{
		R_flush() ;
		cur_char = getchar() & 0177 ;
		sprintf(buffer,"  %c",cur_char) ;
		Write_message(2, buffer) ;
		switch(cur_char)
		{
			case 'l': /* Link lines around area */
				Write_message(3, " no link yet.") ;
				break ;
			case 'm': /* Move area center */
				if (get_new_coor(area[choice].x, area[choice].y, &x,&y))
				{
					R_standard_color( D_translate_color("gray")) ;
					Blot(area[choice].x, area[choice].y) ;
					area[choice].x = x ;
					area[choice].y = y ;
					R_standard_color( D_translate_color("orange") ) ;
					Blot(area[choice].x, area[choice].y) ;
				}
				Write_menu(AREA_MENU) ;
				Write_area(choice, &area[choice]) ;
				break ;
			case 'e': /* Edit category codes */
				edit_a_atts (choice) ;
				Write_menu(AREA_MENU) ;
				Write_area(choice, &area[choice]) ;
				break ;
			case 'i': /* Inspect border */
				inspect_area(choice) ;
				Clear_message() ;
				break ;
			case '*':
				Replot_screen();
				break;
			case 'q':
				if (do_win)
				{
					pop_window() ;
					return(0) ;
				}
				if (choice)
				{
					sprintf(buffer,"ERASING area # %d", choice) ;
					Write_message(2, buffer) ;
					R_standard_color( D_translate_color("gray") ) ;
				/* erase lines that claim to be attached to area */
					plot_alines(choice, SOLID) ;  
				/* erase lines that area claims bound it */
					plot_area(choice, SOLID) ;
					plot_isles(choice, SOLID) ; 
					Clear_message() ;
				}
				return(0) ;
			default:
				sprintf(buffer,"  %c - Unknown Command",cur_char) ;
				Write_message(2, buffer) ;
				break ;
		}
	}
}
