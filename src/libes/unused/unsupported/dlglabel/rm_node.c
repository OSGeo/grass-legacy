/*  @(#)remove_node.c	1.1  5/4/87  */
#include "dlg.h"

/* remove_node deletes a node only if no lines are attached to it
 */

remove_node(choice)
	int *choice ;
{
	char buffer[80] ;
	Clear_message() ;

/* Check to make sure no lines are assigned to node */
	if (node[*choice].n_lines)
	{
		Write_message(2,"Sorry, lines still attached") ;
		Write_message(3,"Not deleting node.") ;
		sleep(3) ;
		return(0) ;
	}

/* Delete node */
	Write_message(3,"Deleting node.") ;
	sleep(3) ;
	node[*choice].x = 0.0 ;
	node[*choice].y = 0.0 ;
	node[*choice].n_lines = 0 ;
	free((char*)node[*choice].lines) ;
	return(1) ;
}
