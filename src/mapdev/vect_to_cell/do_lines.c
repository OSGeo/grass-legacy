/* %W%  %G% */
#include <stdio.h>
#include "digit.h"
#include "bmif.h"

extern int cur_category;

do_lines(Map, header)
	struct Map_info *Map ;
	struct Cell_head *header ;
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

    struct  line_pnts  *Pnts ;  

    pkgs = 0 ;
    status = 0 ;

    max_lines = dig_P_num_lines(Map) ;

    printf( "Step 3: Creating Line information...   ") ;
    for(at_line=1; at_line <= max_lines; at_line++)
    {
	if (Map->Line[at_line].type == DOT)
	    continue;

	cur_category = dig_P_line_att( Map, at_line) ;

	if ( ! cur_category)
		continue ;

	if (0 > dig_P_read_line ( Map, at_line, &Pnts)) 
		return(-1) ;

	pkgs++ ;

/*  break out x and y  */
	n_vert = Pnts->n_points ;
	xarr = Pnts->x ;
	yarr = Pnts->y ;

    /*  convert utm's to row & col  */
	/*
	line_convert (n_vert, xarr, yarr) ;
	*/
	translate (xarr, yarr, n_vert) ;

	find_line (n_vert, xarr, yarr) ;

	/*
	status += save_line (cat) ;
	*/
	/*
	free(xy) ;
	*/
    }

    return(pkgs) ;
}
