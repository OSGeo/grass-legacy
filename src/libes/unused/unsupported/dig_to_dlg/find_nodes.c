/*  @(#)find_nodes.c	2.1  6/26/87  */
#include <math.h>
#include "structures.h"

#define  MAX_ENDPTS	40
#define  DEGENERATE_LINE	999

find_nodes(thresh)
	double thresh ;
{
	char message[128] ;
	int nodes[MAX_ENDPTS] ;
	int num ;
	int i ;
	int j ;
	double x, y ;
	double fabs() ;
#ifdef DEBUG
	double p_x, p_y;
	char buff[80] ;
				sprintf(buff, " Thresh: %8.2lf  N_endpts: %d",
					thresh, n_endpts) ;
				Write_info(4, buff) ;
				getchar() ;
#endif DEBUG

	n_nodes = 0 ;

	R_standard_color( D_translate_color("white")) ;

	for (i=0; i<n_endpts; i++)
		endpoints[i].node = 0 ;

	for (i=0; i<n_endpts; i++)
	{
		if (endpoints[i].node)
			continue ;
		n_nodes++ ;
		num = 0 ;
		nodes[num++] = i ;

		if (i == n_endpts)
		{   
			/* dispatch with last endpoint quickly */
			endpoints[i].node = n_nodes ;
			x = endpoints[i].x ;
			y = endpoints[i].y ;
			if (endpoints[i].angle == DEGENERATE_LINE)
				R_standard_color( D_translate_color("green")) ;
			Blot(&x,&y) ;
			continue ;
		}
		else
		{

#ifdef DEBUG
				sprintf(buff, " e_x: %10.2lf  e_y: %10.2lf   i: %d num: %d",
					endpoints[nodes[0]].x, endpoints[nodes[0]].y,
					i, num) ;
				Write_info(2, buff) ;
#endif DEBUG

			/* find all other endpoints within thresh distance of first */
				x = endpoints[nodes[0]].x ;
				y = endpoints[nodes[0]].y ;
			for (j=i+1; j<n_endpts; j++)
				if ((fabs(x - endpoints[j].x) < thresh) &&
				    (fabs(y - endpoints[j].y) < thresh))
				{
					nodes[num++] = j ;
					if (num >= MAX_ENDPTS)
					{
						R_standard_color( D_translate_color("purple")) ;
						Blot(&x,&y) ;
						R_flush() ;
						sprintf(message, "ERROR: More than %d endpoints assigned to one node.",
							MAX_ENDPTS) ;
						Write_info(2, message) ;
						Write_info(3, "Likely that the threshold value is too high") ;
						sprintf(message, " x: %lf,  y: %lf.", x, y ) ;
						Write_info(4, message) ;
						sleep(5) ;
						Clear_info() ;
						return(0) ;
					}
				}

			/* Calculate average coordinates of set */
			x = 0.0 ; y = 0.0 ;
			for (j=0; j<num; j++)
			{
				x =  x  +  endpoints[nodes[j]].x ;
				y =  y  +  endpoints[nodes[j]].y ;
#ifdef DEBUG
				sprintf(buff, " e_x: %10.2lf  e_y: %10.2lf  x:%10.2lf y:%10.2lf  j: %d  node.j: %d",
				endpoints[nodes[j]].x, endpoints[nodes[j]].y,
					x, y,
					j, nodes[j]) ;
				Write_info(3, buff) ;
#endif DEBUG
			}


#ifdef DEBUG
p_x = x ;
p_y = y ;
#endif DEBUG
			x /= (double)num ;
			y /= (double)num ;

			/*  color degenerate node green instead of white */
			if (endpoints[nodes[0]].angle == DEGENERATE_LINE)
			{
				R_standard_color( D_translate_color("green")) ;
				Blot(&x,&y) ;
				R_standard_color( D_translate_color("white")) ;
			}
			else
				Blot(&x,&y) ;
#ifdef DEBUG
				sprintf(buff, " x: %10.2lf  y: %10.2lf   p_x:%10.2lf  p_y:%10.2lf  num: %d",
					x, y, p_x, p_y, num) ;
				Write_info(4, buff) ;
				getchar() ;
#endif DEBUG

			/* Reassign coordinates */
			for (j=0; j<num; j++)
			{
				endpoints[nodes[j]].x = x ;
				endpoints[nodes[j]].y = y ;
				endpoints[nodes[j]].node = n_nodes ;
			}
		}
	}

	R_flush() ;
	return(1) ;
}
