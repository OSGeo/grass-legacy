#include <stdio.h>
#include "dlg.h"
#include "bmif.h"

do_lines(fp, dlg)
	FILE *fp ;
	struct dlg *dlg ;
{
	int at_line ;
	int cat ;
	double *arr ;
	int n_vert ;
	int pkgs ;
	int er ;
	int  status ;

	n_vert = 0 ;
	pkgs = 0 ;
	status = 0 ;

	for(at_line=1; at_line <= dlg->max_lines; at_line++)
	{
		er = dlg_read_line ( fp, dlg, at_line) ;
		if (er < 0)
			return(-1) ;
		if (er ==  1)
			continue ;

	/* If there are no attributes attached to line, skip it */
		if (! dlg->line.n_atts)
			continue ;
		cat = dlg->line.atts[1] ;  /* First minor att num */

		if ( ! cat)
			continue ;
		pkgs++ ;

		arr = dlg->line.coors ;
		n_vert = dlg->line.n_coors ;

	    /*  convert utm's to row & col  */
		line_convert (arr, n_vert) ;
/*
{
int i ;
fprintf(stderr, "GOT:\n") ;
for(i=0; i<n_vert; i++)
	fprintf(stderr, "%.2lf  %.2lf\n", xarr[i], yarr[i]) ;
}
*/

		find_line (arr, n_vert) ;

		status += save_line (cat) ;
		free(xy) ;
	}

	if ( status)
		fprintf (stderr, "Lines may not have been converted \
correctly on map edge.\n Make sure GIS window is large enough \
to encompass map\n") ;

	return(pkgs) ;
}
