/* @(#) 1.11 6/27/90 /usr/grass3.1/src.scs/scspsu/s.do_sites.c */
/* @(#) 1.20 4/23/91 /usr/grass4.0/src.contrib/SCS/psu/s.do_sites.c */
#include <stdio.h>
#include "digit.h"

extern int cur_category;
extern struct line_pnts *Points;

do_sites (Map, header)
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
	pkgs++ ;
    }

    return(pkgs) ;
}
