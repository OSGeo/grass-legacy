/*  @(#)plot_alines.c	1.1  5/4/87  */
#include "dlg.h"
#include "externs.h"
#include <signal.h>

/* plot_alines plots lines that claim to be 
 * associated with the area "area".
 */
plot_alines(area, plot_type)
	long area ;
	int plot_type ;
{
	int i ;

	Old_tty() ;
	set_signals() ;

	for (i=1; i<=tot_lines; i++)
	{
		if (signalflag.interrupt)
			break ;
		if ( (line[i].right_area == area) || (line[i].left_area == area) )  
			plot_line(i, plot_type) ;
	}

	New_tty() ;
}
