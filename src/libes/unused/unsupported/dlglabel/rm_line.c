/*  @(#)remove_line.c	1.1  5/4/87  */
#include "dlg.h"

remove_line(choice)
	int choice ;
{
	int atnode ;
	int atarea ;
	int j ;
	int k ;
	char buffer[80] ;
	char *gets() ;

	Clear_message() ;
	Write_message(2, " Modifying nodes") ;

/* Remove line from each node's list */
	if (atnode = line[choice].start_node)
	{
		if (sub_int(choice, &(node[atnode].n_lines), &(node[atnode].lines)) )
			sprintf(buffer," Node %d: line removed", atnode) ;
		else
			sprintf(buffer," Node %d: line not found", atnode) ;
		Write_message(3, buffer) ;
	}

	if (atnode = line[choice].end_node)
	{
		if (sub_int(-choice, &(node[atnode].n_lines), &(node[atnode].lines)) )
			sprintf(buffer," Node %d: line removed", atnode) ;
		else
			sprintf(buffer," Node %d: line not found", atnode) ;
		Write_message(4, buffer) ;
	}

	sleep(2) ;     /* Pause for reading */

/* Remove line from each area's list */
	Clear_message() ;
	Write_message(2, " Modifying areas") ;

	if (atarea = line[choice].left_area)
	{
		if (! sub_int(choice, &(area[atarea].n_lines), &(area[atarea].lines)) )
		{
			if (! sub_int(-choice, &(area[atarea].n_lines), &(area[atarea].lines)) )
				sprintf(buffer," Area %d: line removed", atarea) ;
			else
				sprintf(buffer," Area %d: line not found", atarea) ;
		}
		else
			sprintf(buffer," Area %d: line removed", atarea) ;
		Write_message(3, buffer) ;
	}

	if (atarea = line[choice].right_area)
	{
		if (! sub_int(choice, &(area[atarea].n_lines), &(area[atarea].lines)) )
		{
			if (! sub_int(-choice, &(area[atarea].n_lines), &(area[atarea].lines)) )
				sprintf(buffer," Area %d: line removed", atarea) ;
			else
				sprintf(buffer," Area %d: line not found", atarea) ;
		}
		else
			sprintf(buffer," Area %d: line removed", atarea) ;
		Write_message(4, buffer) ;
	}

	sleep(2) ;

/* Zero line entries */
	line[choice].start_node = 0 ;
	line[choice].end_node = 0 ;
	line[choice].left_area = 0 ;
	line[choice].right_area = 0 ;
	line[choice].n_coors = 0 ;
	line[choice].n_atts = 0 ;
	free((char*)line[choice].atts) ;
	Write_message(5, "Hit return to continue") ;
	Get_curses_text(buffer) ;
	Clear_message() ;
	return(1) ;
}
