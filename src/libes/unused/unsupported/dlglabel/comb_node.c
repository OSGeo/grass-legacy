/*  @(#)comb_node.c	2.1  6/26/87  */
#include "dlg.h"

/* comb_node assigns the lines associated with a user choosen node
 * to the current node and then deletes the chosen node
 */

comb_node(choice)
	int *choice ;
{
	int new_node ;
	double x, y ;
	int comb_choice ;
	int aline ;
	int new_n_lines ;
	int *new_lines ;
	int i, j ;
	char buffer[64] ;

/* Get node to combine */
	if ( (comb_choice = choose_node()) == 0)
		return(0) ;

/* Assign lines to nodes */
	new_n_lines = node[*choice].n_lines + node[comb_choice].n_lines ;
	new_lines = (int *)
		calloc(node[*choice].n_lines + node[comb_choice].n_lines ,sizeof(int)) ;
	if (!new_lines)
	{
		fprintf(stderr,"ERROR: insufficient memory\n") ;
		return(0) ;
	}

	j = 0 ;
	for(i=0; i<node[*choice].n_lines; i++)
	{
		new_lines[j++] = node[*choice].lines[i] ;
	}
	for(i=0; i<node[comb_choice].n_lines; i++)
	{
		new_lines[j++] = node[comb_choice].lines[i] ;
	}

	Clear_message() ;
	sprintf(buffer,"Lines attached to node %d", comb_choice) ;
	Write_message(1,buffer) ;
	sprintf(buffer,"about to be attached to %d", *choice) ;
	Write_message(2,buffer) ;
	Write_message(3,"Are you sure? (y/n) > ") ;
	Get_curses_text(buffer) ;
	switch(*buffer)
	{
		case 'y':
		/* Redefine old node's lines based on update info */
			free((char*)node[*choice].lines) ;
			node[*choice].n_lines = new_n_lines ;
			node[*choice].lines = new_lines ;
		/* Change each line's end node info */
			for(i=0; i<node[comb_choice].n_lines; i++)
			{
				aline = node[comb_choice].lines[i] ;
				if (aline > 0)
					line[abs(aline)].start_node = *choice ;
				else
					line[abs(aline)].end_node   = *choice ;
			}
		/* Delete node */
			node[comb_choice].x = 0.0 ;
			node[comb_choice].y = 0.0 ;
			node[comb_choice].n_lines = 0 ;
			free((char*)node[comb_choice].lines) ;
			break ;
		case 'n':
			break ;
		default:
			Write_message(2,"Taking that as NO") ;
			sleep(2) ;
	}
	Clear_message() ;

	return(1) ;
}
