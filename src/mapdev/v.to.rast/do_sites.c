#include <stdio.h>
#include "digit.h"

extern int cur_category;

do_sites (Map, header, Pnts)
	struct Map_info *Map ;
	struct Cell_head *header ;
	struct  line_pnts  *Pnts ;  
{
    int at_line ;
    int cat ;
    double *yarr ;
    double *xarr ;
    int n_vert ;
    int max_lines ;
    int pkgs ;
    int er ;
    int  status ;


    pkgs = 0 ;
    status = 0 ;

    max_lines = V2_num_lines(Map) ;

    for(at_line=1; at_line <= max_lines; at_line++)
    {
	if (Map->Line[at_line].type != DOT)
	    continue;

	cur_category = V2_line_att( Map, at_line) ;

	if ( ! cur_category)
		continue ;

	if (0 > V2_read_line ( Map, Pnts, at_line)) 
		return(-1) ;

	pkgs++ ;

/*  break out x and y  */
	n_vert = Pnts->n_points ;
	xarr = Pnts->x ;
	yarr = Pnts->y ;

    /*  convert utm's to row & col  */
	translate (xarr, yarr, n_vert) ;

	find_line (n_vert, xarr, yarr) ;

    }

    return(pkgs) ;
}
