/*  @(#)w_bdlg_head.c	1.2  6/24/87  */
/*
 * Writes a dlg file header out in binary format.
 *
 * returns:  -1 on error
 *            0 on completion
 */

#include <stdio.h>
#include "dlghead.h"
 
write_bdlg_head(bin, cnum)
	FILE *bin ;
	int cnum ;
{
	char corner[3] ;
	int i ;
	int j ;

	static  double  intparms[4] = { 1.0, 0.0, 0.0, 0.0} ;

/* Write first line */
	fwrite(banner, sizeof(*banner), 72, bin) ;

/* Write second line */
	fwrite(cart_unit,   sizeof(*cart_unit),   40, bin) ;
	fwrite(source_date, sizeof(*source_date), 10, bin) ;
	fwrite(orig_scale,  sizeof(*orig_scale),   8, bin) ;

/* Write third line 
 *   Used, but meaning undefined
 */
	fwrite(line_3, sizeof(*line_3), 72, bin) ;

/* Read fourth line */
	fwrite (&level_code,  sizeof(level_code),  1, bin) ;
	fwrite (&plani_code,  sizeof(plani_code),  1, bin) ;
	fwrite (&plani_zone,  sizeof(plani_zone),  1, bin) ;
	fwrite (&plani_units, sizeof(plani_units), 1, bin) ;
	fwrite (&resolution,  sizeof(resolution),  1, bin) ;
	fwrite (&trans_param, sizeof(trans_param), 1, bin) ;
	fwrite (&misc_records,sizeof(misc_records),1, bin) ;
	fwrite (&num_sides,   sizeof(num_sides),   1, bin) ;
	i = 1 ;
	fwrite (&i,           sizeof(i),           1, bin) ;

/* Write fifth through ninth lines */
	fwrite (params, sizeof(*params), 15, bin) ;

/* Write tenth line */
	fwrite (intparms, sizeof(*int_params), 4, bin) ;

/* Write eleventh through forteenth lines */
	for(i=0;i<4;i++)
	{
		fwrite (coors[i].corner, sizeof(*coors[i].corner),2, bin) ;
		fwrite (&coors[i].lat,   sizeof(coors[i].lat),    1, bin) ;
		fwrite (&coors[i].lon,   sizeof(coors[i].lon),    1, bin) ;
		fwrite (&coors[i].utm_n, sizeof(coors[i].utm_n),  1, bin) ;
		fwrite (&coors[i].utm_e, sizeof(coors[i].utm_e),  1, bin) ;
	}

	nlines = 14 ;

/* Read one more line for each category */

	fwrite (cats[cnum].name,       sizeof(*cats[cnum].name),    20, bin) ;
	fwrite (&cats[cnum].form_code, sizeof(cats[cnum].form_code), 1, bin) ;
	fwrite (&cats[cnum].num_nodes, sizeof(cats[cnum].num_nodes), 1, bin) ;
	fwrite (&cats[cnum].act_nodes, sizeof(cats[cnum].act_nodes), 1, bin) ;
	fwrite (&cats[cnum].nta_link,  sizeof(cats[cnum].nta_link),  1, bin) ;
	fwrite (&cats[cnum].ntl_link,  sizeof(cats[cnum].ntl_link),  1, bin) ;
	fwrite (&cats[cnum].num_areas, sizeof(cats[cnum].num_areas), 1, bin) ;
	fwrite (&cats[cnum].act_areas, sizeof(cats[cnum].act_areas), 1, bin) ;
	fwrite (&cats[cnum].atn_link,  sizeof(cats[cnum].atn_link),  1, bin) ;
	fwrite (&cats[cnum].atl_link,  sizeof(cats[cnum].atl_link),  1, bin) ;
	fwrite (&cats[cnum].area_list, sizeof(cats[cnum].area_list), 1, bin) ;
	fwrite (&cats[cnum].num_lines, sizeof(cats[cnum].num_lines), 1, bin) ;
	fwrite (&cats[cnum].act_lines, sizeof(cats[cnum].act_lines), 1, bin) ;
	fwrite (&cats[cnum].line_list, sizeof(cats[cnum].line_list), 1, bin) ;

	return(0) ;
}
