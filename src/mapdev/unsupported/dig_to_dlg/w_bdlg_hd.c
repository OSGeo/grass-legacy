/*  @(#)w_bdlg_head.c	1.2  6/24/87  */
/*
 * Writes a dlg file header out in binary format.
 *
 * returns:  -1 on error
 *            0 on completion
 */

#include <stdio.h>
#include "dlg.h"
static char *compass[4] = {"SW","NW","NE","SE"} ;
 
write_bdlg_head(bin)
	FILE *bin ;
{
	char corner[3] ;
	int i ;
	int j ;

/* Write first line */
	fwrite(dlg_head.banner, sizeof(*dlg_head.banner), 72, bin) ;

/* Write second line */
	fwrite(dlg_head.cart_unit,   sizeof(*dlg_head.cart_unit),   40, bin) ;
	fwrite(dlg_head.source_date, sizeof(*dlg_head.source_date), 10, bin) ;
	fwrite(dlg_head.orig_scale,  sizeof(*dlg_head.orig_scale),   8, bin) ;

/* Write third line 
 *   Used, but meaning undefined
 */
	fwrite(dlg_head.line_3, sizeof(*dlg_head.line_3), 72, bin) ;

/* Read fourth line */
	fwrite (&dlg_head.level_code,  sizeof(dlg_head.level_code),  1, bin) ;
	fwrite (&dlg_head.plani_code,  sizeof(dlg_head.plani_code),  1, bin) ;
	fwrite (&dlg_head.plani_zone,  sizeof(dlg_head.plani_zone),  1, bin) ;
	fwrite (&dlg_head.plani_units, sizeof(dlg_head.plani_units), 1, bin) ;
	fwrite (&dlg_head.resolution,  sizeof(dlg_head.resolution),  1, bin) ;
	fwrite (&dlg_head.trans_param, sizeof(dlg_head.trans_param), 1, bin) ;
	fwrite (&dlg_head.misc_records,sizeof(dlg_head.misc_records),1, bin) ;
	fwrite (&dlg_head.num_sides,   sizeof(dlg_head.num_sides),   1, bin) ;
	i = 1 ;
	fwrite (&i,           sizeof(i),           1, bin) ;

/* Write fifth through ninth lines */
	fwrite (dlg_proj.params, sizeof(*dlg_proj.params), 15, bin) ;

/* Write tenth line */
	fwrite (dlg_proj.int_params, sizeof(*dlg_proj.int_params), 4, bin) ;

/* Write eleventh through forteenth lines */
	for(i=0;i<4;i++)
	{
		fwrite (compass[i],          sizeof(*compass[i]),         2, bin) ;
		fwrite (&dlg_coors[i].lat,   sizeof(dlg_coors[i].lat),    1, bin) ;
		fwrite (&dlg_coors[i].lon,   sizeof(dlg_coors[i].lon),    1, bin) ;
		fwrite (&dlg_coors[i].utm_e, sizeof(dlg_coors[i].utm_e),  1, bin) ;
		fwrite (&dlg_coors[i].utm_n, sizeof(dlg_coors[i].utm_n),  1, bin) ;
	}

	dlg_head.nlines = 14 ;

/* Write one more line for each category */

	fwrite (dlg_cats.name,      sizeof(*dlg_cats.name),   20, bin) ;
	fwrite (&dlg_cats.form_code,sizeof(dlg_cats.form_code),1, bin) ;
	fwrite (&dlg_cats.num_nodes,sizeof(dlg_cats.num_nodes),1, bin) ;
	fwrite (&dlg_cats.act_nodes,sizeof(dlg_cats.act_nodes),1, bin) ;
	fwrite (&dlg_cats.nta_link, sizeof(dlg_cats.nta_link), 1, bin) ;
	fwrite (&dlg_cats.ntl_link, sizeof(dlg_cats.ntl_link), 1, bin) ;
	fwrite (&dlg_cats.num_areas,sizeof(dlg_cats.num_areas),1, bin) ;
	fwrite (&dlg_cats.act_areas,sizeof(dlg_cats.act_areas),1, bin) ;
	fwrite (&dlg_cats.atn_link, sizeof(dlg_cats.atn_link), 1, bin) ;
	fwrite (&dlg_cats.atl_link, sizeof(dlg_cats.atl_link), 1, bin) ;
	fwrite (&dlg_cats.area_list,sizeof(dlg_cats.area_list),1, bin) ;
	fwrite (&dlg_cats.num_lines,sizeof(dlg_cats.num_lines),1, bin) ;
	fwrite (&dlg_cats.act_lines,sizeof(dlg_cats.act_lines),1, bin) ;
	fwrite (&dlg_cats.line_list,sizeof(dlg_cats.line_list),1, bin) ;

	return(0) ;
}
