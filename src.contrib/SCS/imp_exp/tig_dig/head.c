static char rcsid[] = "$Header$";
/*
$Log$
Revision 1.1  1999-12-29 15:12:12  markus
Initial revision

 * Revision 1.1  1991/06/25  03:31:35  paul
 * Initial revision
 *
 * Revision 1.1  1991/05/29  03:50:43  paul
 * Initial revision
 *
*/

#include "gis.h"
#include "Vect.h"

do_head(Map,verbose)
	struct Map_info *Map;
	int verbose;
{
        Map->head.orig_scale = 100000.0;
	Map->head.N = Map->head.S = Map->head.W = Map->head.E = 1.0;
	Map->head.plani_zone = G_zone();
	Map->head.date[0] = '\0';
	Map->head.line_3[0]= Map->head.map_name[0] = Map->head.source_date[0] = '\0';
	Map->head.digit_thresh = Map->head.map_thresh = 0.0;
	G_strcpy(Map->head.organization,"USDC, BUREAU OF THE CENSUS");
	G_strcpy(Map->head.your_name,"SCS TIGER Import");
	if(verbose) Vect_print_header(Map);
}
