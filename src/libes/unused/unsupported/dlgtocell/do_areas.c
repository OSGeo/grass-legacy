/* @(#)do_areas.c	2.2  9/21/87 */
#include <stdio.h>
#include "dlg.h"
#include "bmif.h"

do_areas(fp, dlg)
	FILE *fp ;
	struct dlg *dlg ;
{
	int at_area ;
	int cat ;
	double *xarr, *yarr ;
	int n_vert ;
	int n_alloc ;
	int no_cats ;
	int convert_failed ;
	int pkgs ;
	int er ;
	int last_area ;
	int i ;

	n_vert = 0 ;
	n_alloc = 0 ;
	no_cats = 0 ;
	pkgs = 0 ;
	last_area = 0 ;
	convert_failed = 0 ;

	for(at_area=1; at_area <= dlg->max_areas; at_area++)
	{

		er = dlg_read_whole_area (fp, dlg, at_area, &xarr, &yarr, &n_vert, &n_alloc) ;
		if (er < 0)
			return(-1) ;

		if (er == 1)
			continue ;
/*
fprintf(stderr, "GOT: area %d,  n_vert: %d\n", at_area, n_vert) ;
for(i=0; i<n_vert; i++)
	fprintf(stderr, "%10.2lf  %10.2lf\n", xarr[i], yarr[i]) ;
*/


	/* If there are no attributes attached to area, skip it */
		if (! dlg->area.n_atts)
		{
			++no_cats ;
			continue ;
		}

		cat = dlg->area.atts[1] ;  /* First minor att num */

		if (! cat)
		{
			++no_cats ;
			continue ;
		}

		pkgs++ ;
		area_convert (xarr, yarr, n_vert) ;
/*
{
int i ;
fprintf(stderr, "GOT: area %d,  n_vert: %d\n", at_area, n_vert) ;
for(i=0; i<n_vert; i++)
	fprintf(stderr, "%.2lf  %.2lf\n", xarr[i], yarr[i]) ;
}
*/


		find_area (xarr, yarr, n_vert) ;

		if ( save_area (cat) < 0 )
		{
			fprintf(stderr, " ERROR, couldn't convert area: %d \n",
				at_area) ;
			++convert_failed ;
		}

		free(xy) ;
	last_area = at_area ;
	}

/*  area outside (A1) and inside (A2) map won't be labeled  */
	if (no_cats > 2)
		fprintf ( stderr, "Number of areas with no labels: %d\n", no_cats) ;


	if (convert_failed)
	{
		fprintf ( stderr, "There was a problem converting %d areas.\n",
			convert_failed) ;
		fprintf ( stderr, "Those areas will have to be fixed in the dlg file and\n then reconverted to have a proper cell file.\n") ;
	}

	return(pkgs) ;
}
