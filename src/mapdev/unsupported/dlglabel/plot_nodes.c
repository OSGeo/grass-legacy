/*  @(#)plot_nodes.c	2.1  6/26/87  */
#include "dlg.h"
#include "externs.h"
#include <signal.h>

plot_all_nodes()
{
	int i ;

	Write_message(2, "PLOTTING ALL NODES") ;
	Write_message(3, "Hit DEL/RUB to abort") ;

	Old_tty() ;
	set_signals() ;

	for (i=1; i<=tot_nodes; i++)
	{
		if (signalflag.interrupt)
			break ;
		Blot(node[i].x, node[i].y) ;
	}

	New_tty() ;
	Clear_message() ;
}
