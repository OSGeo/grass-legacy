/*  @(#)node_menu.c	2.1  6/26/87  */
#include "dlg.h"
#include "menus.h"

node_menu(choice)
	int choice ;
{
	char cur_char ;
	char buffer[128] ;
	double x, y ;

	Write_menu(NODE_MENU) ;
	Write_node(choice, &node[choice]) ;
	R_standard_color( D_translate_color("red") ) ;
	Blot(node[choice].x, node[choice].y) ;

	while(1) 
	{
		R_flush() ;
		cur_char = getchar() & 0177 ;
		sprintf(buffer,"  %c",cur_char) ;
		Write_message(2, buffer) ;
		switch(cur_char)
		{
			case 'm': /* Move node */
				if (get_new_coor(node[choice].x,node[choice].y,&x,&y))
				{
					R_standard_color( D_translate_color("black") ) ;
					Blot(node[choice].x, node[choice].y) ;
					node[choice].x = x ;
					node[choice].y = y ;
					R_standard_color( D_translate_color("green") ) ;
					Blot(node[choice].x, node[choice].y) ;
				}
				Write_menu(NODE_MENU) ;
				Write_node(choice, &node[choice]) ;
				break ;
			case 's': /* Split node */
				split_node(&choice) ;
				Write_node(choice, &node[choice]) ;
				Write_menu(NODE_MENU) ;
				break ;
			case 'c': /* Combine 2 nodes */
				comb_node(&choice) ;
				Write_node(choice, &node[choice]) ;
				Write_menu(NODE_MENU) ;
				break ;
			case 'r': /* Remove node */
				R_standard_color( D_translate_color("black") ) ;
				Blot(node[choice].x, node[choice].y) ;
				if (remove_node(&choice))
					return(0) ;
				else
				{
					R_standard_color( D_translate_color("green") ) ;
					Blot(node[choice].x, node[choice].y) ;
				}
				break ;
			case 'e': /* Edit category codes */
				edit_n_atts(choice) ;
				Write_node(choice, &node[choice]) ;
				Write_menu(NODE_MENU) ;
				break ;
			case '*':
				Replot_screen();
				break;
			case 'q':
				R_standard_color( D_translate_color("green") ) ;
				Blot(node[choice].x, node[choice].y) ;
				return(0) ;
			default:
				sprintf(buffer,"  %c - Unknown Command",cur_char) ;
				Write_message(2, buffer) ;
				break ;
		}
	}
}
