/*  @(#)plot_cents.c	2.1  6/26/87  */
#include "dlg.h"

plot_a_cents()
{
	int i ;

	for (i=1; i<=tot_areas; i++)
		Blot(area[i].x, area[i].y) ;
}

plot_a_labels()
{
	int i ;
	char buff[64] ;

	for (i=1; i<=tot_areas; i++)
		if (area[i].n_atts)
		{
			sprintf(buff, "%d", area[i].atts[1]) ; /* First min. att. code */
			Adot(&area[i].x, &area[i].y, buff) ;
		}
}
		/*  plot_a_labels ()  */

