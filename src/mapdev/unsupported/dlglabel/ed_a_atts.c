/*  @(#)edit_a_atts.c	1.1  5/4/87  */
#include "dlg.h"

edit_a_atts(choice)
	int choice ;
{
	int atts[100] ;
	int i ;
	int ptr ;
	char buffer[80] ;
	int incorrect ;
	int not_done ;
	int number ;

	ptr = 0 ;

/* Check for acceptance of existing attributes */
	for (i=0; i<area[choice].n_atts; i++)
	{
		Clear_message() ;
		sprintf(buffer," Retain: %d %d\n",
			area[choice].atts[i*2],
			area[choice].atts[i*2+1]) ; 
		Write_message(1, buffer) ;
		Write_message(2, " (y,n) > ") ;
		incorrect = 1 ;
		while(incorrect)
		{
			Get_curses_text(buffer) ;
			switch(*buffer)
			{
				case 'y':
					atts[ptr++] = area[choice].atts[i*2] ;
					atts[ptr++] = area[choice].atts[i*2+1] ;
					incorrect = 0 ;
					break ;
				case 'n':
					incorrect = 0 ;
					break ;
				default:
					Write_message(2, "Answer yes/no >") ;
					break ;
			}
		}
	}

/* Check to add more categories */
	not_done = 1 ;
	while(not_done)
	{
		Clear_message() ;
		Write_message(1," Next category, ") ;
		Write_message(2," or 0 to quit") ;

		Write_message(3,"  Major att> ") ;
		Get_curses_text(buffer) ;

		if ( sscanf(buffer,"%d",&number) <= 0)
			break ;
		if ( ! number)
			break ;

		atts[ptr++] = number ;

		incorrect = 1 ;
		while(incorrect)
		{
			Write_message(4,"  Minor att> ") ;
			Get_curses_text(buffer) ;
			if (sscanf(buffer,"%d",&number) > 0)
			{
				incorrect = 0 ;
				atts[ptr++] = number ;
			}
		}
	}

/* Could ask an "Are you sure?" here */

/* Free old atts list and allocate space for new */
	free((char*)area[choice].atts) ;
	area[choice].atts = (int *)(calloc(ptr,sizeof(int))) ;

/* update atts and n_atts */
	area[choice].n_atts = ptr/2 ;
	for (i=0; i<ptr; i++)
		area[choice].atts[i] = atts[i] ;
}
