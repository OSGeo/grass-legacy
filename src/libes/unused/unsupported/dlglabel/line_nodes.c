/*  @(#)line_nodes.c	2.1  6/26/87  */
#include "dlg.h"

line_nodes(choice)
	int choice ;
{
	char buffer[128] ;
	int new_node ;
	int old_node ;

/* Assign new beginning node */
	Clear_message() ;
	Write_message(1, " Line beginning attached") ;
	sprintf(buffer," to node %d", line[choice].start_node) ;
	Write_message(2, buffer) ;
	Write_message(3, " Attach to new node? ") ;
	Get_curses_text(buffer) ;
	switch(*buffer)
	{
		case 'y':
			new_node = choose_node() ;
			if (new_node)
			{
				old_node = line[choice].start_node ;
				line[choice].start_node = new_node ;
				sub_int(choice, &(node[old_node].n_lines), &(node[old_node].lines)) ;
				add_int(choice, &(node[new_node].n_lines), &(node[new_node].lines)) ;
			}
			break ;
		default:
			break ;
	}

/* Assign new ending node */
	Clear_message() ;
	Write_message(1, " Line ending attached") ;
	sprintf(buffer," to node %d", line[choice].end_node) ;
	Write_message(2, buffer) ;
	Write_message(3, " Attach to new node? ") ;
	Get_curses_text(buffer) ;
	switch(*buffer)
	{
		case 'y':
			new_node = choose_node() ;
			if (new_node)
			{
				old_node = line[choice].end_node ;
				line[choice].end_node = new_node ;
				sub_int(-choice, &(node[old_node].n_lines), &(node[old_node].lines)) ;
				add_int(-choice, &(node[new_node].n_lines), &(node[new_node].lines)) ;
			}
			break ;
		default:
			break ;
	}
}
