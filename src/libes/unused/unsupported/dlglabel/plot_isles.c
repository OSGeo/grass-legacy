/*  @(#)plot_isles.c	2.1  6/26/87  */
#include "dlg.h"
#include "externs.h"
#include <signal.h>

/* plot_isles plots lines that area "a" claims
 * as its islands.
 */
plot_isles(a, plottype)
	long a ;
	int plottype ;
{
	int i ;
	int l ;

	Old_tty() ;
	set_signals() ;

/* Skip the boundary data */
	for (i=0; i<area[a].n_lines; i++)
	{
		l = abs(area[a].lines[i]) ;
		if (! l)
			break ;
	}

/* Draw each island */
	for (++i; i<area[a].n_lines; i++)
	{
		if (signalflag.interrupt)
			break ;
		l = abs(area[a].lines[i]) ;
		if (! l)
			continue ;
		plot_line(l, plottype) ;
	}

	New_tty() ;
}
