/*  @(#)line_areas.c	2.1  6/26/87  */
#include "dlg.h"

line_areas(choice)
	int choice ;
{
	char buffer[128] ;
	int new_area ;

/* Assign new right area */
	Clear_message() ;
	Write_message(1, " Right side of line") ;
	sprintf(buffer," to area %d", line[choice].right_area) ;
	Write_message(2, buffer) ;
	Write_message(3, " Attach to new area? ") ;
	Get_curses_text(buffer) ;
	switch(*buffer)
	{
		case 'y':
			new_area = choose_area() ;
			if (new_area)
			{
				line[choice].right_area = new_area ;
			}
			break ;
		default:
			break ;
	}

/* Assign new left area */
	Clear_message() ;
	Write_message(1, " Left side of line") ;
	sprintf(buffer," to area %d", line[choice].left_area) ;
	Write_message(2, buffer) ;
	Write_message(3, " Attach to new area? ") ;
	Get_curses_text(buffer) ;
	switch(*buffer)
	{
		case 'y':
			new_area = choose_area() ;
			if (new_area)
			{
				line[choice].left_area = new_area ;
			}
			break ;
		default:
			break ;
	}
}
