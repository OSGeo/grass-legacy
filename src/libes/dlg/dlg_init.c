/*  ./src/libes/dlg/dlg_init.c
 *******************************************************************
 *  #include "dlg.h"
 *
 *  dlg_init (fd,dlg)
 *      FILE *fd ;
 *      struct dlg *dlg;
 *
 * This routine reads the dlg header information from 'fd'.  Information
 * is stored in the structure 'dlg'.
 *
 * returns:  -1 on error
 *            0 on completion
 */

#include <stdio.h>
#include "dlg.h"

#define GET(x,y,z) fread(x,y,z,fd)

dlg_init (fd,dlg)
	FILE *fd ;
	struct dlg *dlg;
{
	char buffer[81] ;
	int i ;
	int j ;

	dlg->head.nlines = 0 ;

/* Read first (banner) line */
	GET(dlg->head.banner, sizeof(*dlg->head.banner), 72) ;

/* Read second line 
 *   Contains: cart_unit, source_date, orig_scale
 */
	GET(dlg->head.cart_unit,   sizeof(*dlg->head.cart_unit),   40) ;
	GET(dlg->head.source_date, sizeof(*dlg->head.source_date), 10) ;
	GET(dlg->head.orig_scale,  sizeof(*dlg->head.orig_scale),   8) ;

/* Read third line 
 *   Used, but meaning undefined
 */
	GET(dlg->head.line_3, sizeof(*dlg->head.line_3), 72) ;

/* Read fourth line */
	GET (&dlg->head.level_code,  sizeof(dlg->head.level_code),  1) ;
	GET (&dlg->head.plani_code,  sizeof(dlg->head.plani_code),  1) ;
	GET (&dlg->head.plani_zone,  sizeof(dlg->head.plani_zone),  1) ;
	GET (&dlg->head.plani_units, sizeof(dlg->head.plani_units), 1) ;
	GET (&dlg->head.resolution,  sizeof(dlg->head.resolution),  1) ;
	GET (&dlg->head.trans_param, sizeof(dlg->head.trans_param), 1) ;
	GET (&dlg->head.misc_records,sizeof(dlg->head.misc_records),1) ;
	GET (&dlg->head.num_sides,   sizeof(dlg->head.num_sides),   1) ;
	GET (&dlg->head.num_cats,    sizeof(dlg->head.num_cats),    1) ;

/* Read fifth through ninth lines */
	GET (dlg->proj.params, sizeof(*dlg->proj.params), 15) ;
	
/* Read tenth line */
	GET (dlg->proj.int_params, sizeof(*dlg->proj.int_params), 4) ;

/* Read eleventh through forteenth lines */
	for(i=0;i<4;i++)
	{
		GET (buffer, sizeof(*buffer), 2) ;
		if (! strncmp("SW",buffer,2))
			j = SW ;
		else if (! strncmp("NW",buffer,2))
			j = NW ;
		else if (! strncmp("NE",buffer,2))
			j = NE ;
		else if (! strncmp("SE",buffer,2))
			j = SE ;
		else
			return(-1) ;   /* good place to see if binary dlg file is in sync */


		GET (&dlg->coors.lat[j],   sizeof(dlg->coors.lat[j]),    1) ;
		GET (&dlg->coors.lon[j],   sizeof(dlg->coors.lon[j]),    1) ;
		GET (&dlg->coors.utm_e[j], sizeof(dlg->coors.utm_e[j]),  1) ;
		GET (&dlg->coors.utm_n[j], sizeof(dlg->coors.utm_n[j]),  1) ;
	}

	dlg->head.nlines = 14 ;

/* Read one more line for each category */

	for (i=0; i<32; i++)
		dlg->cats.read = 0 ;

	for (i=0; i<dlg->head.num_cats; i++)
	{
		dlg->cats.read = 1 ;
		GET (dlg->cats.name,       sizeof(*dlg->cats.name),    20) ;
		GET (&dlg->cats.form_code, sizeof(dlg->cats.form_code), 1) ;
		GET (&dlg->cats.num_nodes, sizeof(dlg->cats.num_nodes), 1) ;
		GET (&dlg->cats.act_nodes, sizeof(dlg->cats.act_nodes), 1) ;
		GET (&dlg->cats.nta_link,  sizeof(dlg->cats.nta_link),  1) ;
		GET (&dlg->cats.ntl_link,  sizeof(dlg->cats.ntl_link),  1) ;
		GET (&dlg->cats.num_areas, sizeof(dlg->cats.num_areas), 1) ;
		GET (&dlg->cats.act_areas, sizeof(dlg->cats.act_areas), 1) ;
		GET (&dlg->cats.atn_link,  sizeof(dlg->cats.atn_link),  1) ;
		GET (&dlg->cats.atl_link,  sizeof(dlg->cats.atl_link),  1) ;
		GET (&dlg->cats.area_list, sizeof(dlg->cats.area_list), 1) ;
		GET (&dlg->cats.num_lines, sizeof(dlg->cats.num_lines), 1) ;
		GET (&dlg->cats.act_lines, sizeof(dlg->cats.act_lines), 1) ;
		GET (&dlg->cats.line_list, sizeof(dlg->cats.line_list), 1) ;
	}
	return(0) ;
}
