/*  @(#)w_dlg_head.c	2.1  6/26/87  */
#include <stdio.h>
#include "dlghead.h"
#include "dlg.h"

static char *compass[4] = {"SW","NW","NE","SE"} ;

write_dlg_header()
{
	char buff1[64] ;
	char buff2[64] ;
	char buff3[64] ;
	char buff4[64] ;
	int i ;
	int j ;

/* Write banner (first) line */
	fwrite(dlg_head.banner,      sizeof(*dlg_head.banner),      72, dlg_new) ;

/* Write second line */
	fwrite(dlg_head.cart_unit,   sizeof(*dlg_head.cart_unit),   40, dlg_new) ;
	fwrite(dlg_head.source_date, sizeof(*dlg_head.source_date), 10, dlg_new) ;
	fwrite(dlg_head.orig_scale,  sizeof(*dlg_head.orig_scale),   8, dlg_new) ;

/* Write third line */
	fwrite(dlg_head.line_3,      sizeof(*dlg_head.line_3),      72, dlg_new) ;

/* Write fourth line */
	fwrite (&dlg_head.level_code,  sizeof(dlg_head.level_code),  1, dlg_new) ;
	fwrite (&dlg_head.plani_code,  sizeof(dlg_head.plani_code),  1, dlg_new) ;
	fwrite (&dlg_head.plani_zone,  sizeof(dlg_head.plani_zone),  1, dlg_new) ;
	fwrite (&dlg_head.plani_units, sizeof(dlg_head.plani_units), 1, dlg_new) ;
	fwrite (&dlg_head.resolution,  sizeof(dlg_head.resolution),  1, dlg_new) ;
	fwrite (&dlg_head.trans_param, sizeof(dlg_head.trans_param), 1, dlg_new) ;
	fwrite (&dlg_head.misc_records,sizeof(dlg_head.misc_records),1, dlg_new) ;
	fwrite (&dlg_head.num_sides,   sizeof(dlg_head.num_sides),   1, dlg_new) ;
	fwrite (&dlg_head.num_cats,    sizeof(dlg_head.num_cats),    1, dlg_new) ;

/* Write fifth through ninth lines */
	fwrite (dlg_proj.params, sizeof(*dlg_proj.params), 15, dlg_new) ;

/* Write tenth line */
	fwrite (dlg_proj.int_params, sizeof(*dlg_proj.int_params), 4, dlg_new) ;

/* Write eleventh through forteenth lines */
	for(i=0;i<4;i++)
	{
		fwrite (compass[i], sizeof(*compass[i]),2, dlg_new) ;
		fwrite (&dlg_coors.lat[i],   sizeof(dlg_coors.lat[i]),    1, dlg_new) ;
		fwrite (&dlg_coors.lon[i],   sizeof(dlg_coors.lon[i]),    1, dlg_new) ;
		fwrite (&dlg_coors.utm_e[i], sizeof(dlg_coors.utm_e[i]),  1, dlg_new) ;
		fwrite (&dlg_coors.utm_n[i], sizeof(dlg_coors.utm_n[i]),  1, dlg_new) ;
	}

/* Write one more line for each category */

	for (i=0; i<dlg_head.num_cats; i++)
	{
		if (! dlg_cats[i].read)
		{
			printf("ERROR: category number %d not available for dlg header\n") ;
			exit(-1) ;
		}

		check_cats(i) ;

		fwrite (dlg_cats[i].name,       sizeof(*dlg_cats[i].name),    20, dlg_new) ;
		fwrite (&dlg_cats[i].form_code, sizeof(dlg_cats[i].form_code), 1, dlg_new) ;
		fwrite (&dlg_cats[i].num_nodes, sizeof(dlg_cats[i].num_nodes), 1, dlg_new) ;
		fwrite (&dlg_cats[i].act_nodes, sizeof(dlg_cats[i].act_nodes), 1, dlg_new) ;
		fwrite (&dlg_cats[i].nta_link,  sizeof(dlg_cats[i].nta_link),  1, dlg_new) ;
		fwrite (&dlg_cats[i].ntl_link,  sizeof(dlg_cats[i].ntl_link),  1, dlg_new) ;
		fwrite (&dlg_cats[i].num_areas, sizeof(dlg_cats[i].num_areas), 1, dlg_new) ;
		fwrite (&dlg_cats[i].act_areas, sizeof(dlg_cats[i].act_areas), 1, dlg_new) ;
		fwrite (&dlg_cats[i].atn_link,  sizeof(dlg_cats[i].atn_link),  1, dlg_new) ;
		fwrite (&dlg_cats[i].atl_link,  sizeof(dlg_cats[i].atl_link),  1, dlg_new) ;
		fwrite (&dlg_cats[i].area_list, sizeof(dlg_cats[i].area_list), 1, dlg_new) ;
		fwrite (&dlg_cats[i].num_lines, sizeof(dlg_cats[i].num_lines), 1, dlg_new) ;
		fwrite (&dlg_cats[i].act_lines, sizeof(dlg_cats[i].act_lines), 1, dlg_new) ;
		fwrite (&dlg_cats[i].line_list, sizeof(dlg_cats[i].line_list), 1, dlg_new) ;
	}
	return(0) ;
}
