/*  @(#)split_node.c	2.1  6/26/87  */
#include "dlg.h"

split_node(choice)
	int *choice ;
{
	int new_node ;
	int old_line_list[10] ;
	int new_line_list[10] ;
	int cur_line ;
	struct node old ;
	struct node new ;
	double x, y ;
	int i ;
	char incorrect ;
	char buffer[128] ;

	new_node = tot_nodes ;

/* Assign new node */
	Write_message(1, "Splitting node") ;
	sprintf(buffer,"    OLD node: %d", *choice) ;
	Write_message(2, buffer) ;
	sprintf(buffer,"    NEW node: %d", new_node) ;
	Write_message(3, buffer) ;
	Write_message(4, "Hit RETURN to continue") ;
	getchar() ;
	Clear_message() ;

/* Locate new node */
	if (! get_new_coor(node[*choice].x,node[*choice].y,&x, &y) )
	{
		return(0) ;
	}
	new.x = x ;
	new.y = y ;
	R_standard_color( D_translate_color("green") ) ;
	Blot(x,y) ;

/* Assign lines to nodes */
	old.n_lines = 0 ;
	new.n_lines = 0 ;

	for(i=0; i<node[*choice].n_lines; i++)
	{
		cur_line = abs(node[*choice].lines[i]) ;
		Clear_message() ;
		R_standard_color( D_translate_color("blue") ) ;
		plot_line(cur_line, SOLID) ;
		R_flush() ;

		sprintf(buffer,"Attach line %d to", cur_line) ;
		Write_message(2, buffer) ;
		Write_message(3, " old or new node > ") ;
		incorrect = 1 ;
		while(incorrect)
		{
			Get_curses_text(buffer) ;
			switch(*buffer)
			{
				case 'o':
					old_line_list[old.n_lines++] = cur_line ;
					incorrect = 0 ;
					break ;
				case 'n':
					new_line_list[new.n_lines++] = cur_line ;
					incorrect = 0 ;
					break ;
				default:
					break ;
			}
			if(incorrect)
				Write_message(3, " Enter old or new > ") ;
		}
		R_standard_color( D_translate_color("gray") ) ;
		replot_line() ;
		R_flush() ;
	}

	Clear_message() ;
	Write_message(2,"Are you sure? (y/n) > ") ;
	Get_curses_text(buffer) ;
	switch(*buffer)
	{
		case 'y':
		/* Redefine old node's lines based on update info */
			free((char*)node[*choice].lines) ;
			node[*choice].n_lines = old.n_lines ;
			node[*choice].lines = 
				(int *)(calloc(old.n_lines,sizeof(int))) ;
			for(i=0; i<old.n_lines; i++)
				node[*choice].lines[i] = old_line_list[i] ;

		/* Define new node's location */
			node[new_node].x = new.x ;
			node[new_node].y = new.y ;

		/* Define new node's lines based on update info */
			node[new_node].n_lines = new.n_lines ;
			node[new_node].lines = 
				(int *)(calloc(new.n_lines,sizeof(int))) ;

			for(i=0; i<new.n_lines; i++)
			{
				node[new_node].lines[i] = new_line_list[i] ;
				cur_line = node[new_node].lines[i] ;
				if (cur_line > 0)
					line[abs(cur_line)].start_node = new_node ;
				else
					line[abs(cur_line)].end_node   = new_node ;
			}
		/* Define new node's atts based on old node's atts */
			node[new_node].n_atts = node[*choice].n_atts ;
			node[new_node].atts = 
				(int *)(calloc(node[new_node].n_atts,sizeof(int))) ;
			for(i=0; i<node[*choice].n_atts; i++)
				node[new_node].atts[i] = node[*choice].atts[i] ;
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
