/*  @(#)plot_line.c	1.1  5/4/87  */
#include "dlg.h"

static last_type ;
static last_n ;

plot_line(choice, plot_type)
	int choice ;
	int plot_type ;
{
	int n ;

	last_n = line[choice].n_coors ;
	last_type = plot_type ;

	if (n = read_coors(choice) < 0)
		return(n) ;

	switch(plot_type)
	{
		case SOLID:
			plot_all_coors(line[choice].n_coors, coors) ;
			break ;
		case DOTTED:
			plot_dots(line[choice].n_coors, coors) ;
			break ;
	}

	return(0) ;
}

replot_line()
{
	switch(last_type)
	{
		case SOLID:
			plot_all_coors(last_n, coors) ;
			break ;
		case DOTTED:
			plot_dots(last_n, coors) ;
			break ;
	}
}
