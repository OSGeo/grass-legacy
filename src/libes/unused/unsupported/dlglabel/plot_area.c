/*  @(#)plot_area.c	2.1  6/26/87  */
#include "dlg.h"
#include "externs.h"
#include <signal.h>

/* plot_area plots lines that area "a" claims
 * are associated with it.
 */
plot_area(a, plottype)
	long a ;
	int plottype ;
{
	int i ;
	int l ;

	Old_tty() ;
	set_signals() ;

	for (i=0; i<area[a].n_lines; i++)
	{
		if (signalflag.interrupt)
			break ;
		l = abs(area[a].lines[i]) ;
		if (! l)
			break ;
		plot_line(l, plottype) ;
	}

	New_tty() ;
}
