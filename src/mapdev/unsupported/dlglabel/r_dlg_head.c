/*  @(#)r_dlg_head.c	2.1  6/26/87  */
/*
 * This routine reads the dlg header in binary format 
 * storing pertinent information in the dlghead structures.
 *
 * returns:  -1 on error
 *            0 on completion
 */

#include <stdio.h>
#include "dlghead.h"
#include "dlg.h"

read_dlg_header()
{
	char buffer[81] ;
	int i ;
	int j ;

	dlg_head.nlines = 0 ;

/* Read first (banner) line */
	fread(dlg_head.banner, sizeof(*dlg_head.banner), 72, dlg) ;

/* Read second line 
 *   Contains: cart_unit, source_date, orig_scale
 */
	fread(dlg_head.cart_unit,   sizeof(*dlg_head.cart_unit),   40, dlg) ;
	fread(dlg_head.source_date, sizeof(*dlg_head.source_date), 10, dlg) ;
	fread(dlg_head.orig_scale,  sizeof(*dlg_head.orig_scale),   8, dlg) ;

/* Read third line 
 *   Used, but meaning undefined
 */
	fread(dlg_head.line_3, sizeof(*dlg_head.line_3), 72, dlg) ;

/* Read fourth line */
	fread (&dlg_head.level_code,  sizeof(dlg_head.level_code),  1, dlg) ;
	fread (&dlg_head.plani_code,  sizeof(dlg_head.plani_code),  1, dlg) ;
	fread (&dlg_head.plani_zone,  sizeof(dlg_head.plani_zone),  1, dlg) ;
	fread (&dlg_head.plani_units, sizeof(dlg_head.plani_units), 1, dlg) ;
	fread (&dlg_head.resolution,  sizeof(dlg_head.resolution),  1, dlg) ;
	fread (&dlg_head.trans_param, sizeof(dlg_head.trans_param), 1, dlg) ;
	fread (&dlg_head.misc_records,sizeof(dlg_head.misc_records),1, dlg) ;
	fread (&dlg_head.num_sides,   sizeof(dlg_head.num_sides),   1, dlg) ;
	fread (&dlg_head.num_cats,    sizeof(dlg_head.num_cats),    1, dlg) ;

/* Read fifth through ninth lines */
	fread (dlg_proj.params, sizeof(*dlg_proj.params), 15, dlg) ;
	
/* Read tenth line */
	fread (dlg_proj.int_params, sizeof(*dlg_proj.int_params), 4, dlg) ;

/* Read eleventh through forteenth lines */
	for(i=0;i<4;i++)
	{
		fread (buffer, sizeof(*buffer), 2, dlg) ;
		if (! strncmp("SW",buffer,2))
			j = 0 ;
		else if (! strncmp("NW",buffer,2))
			j = 1 ;
		else if (! strncmp("NE",buffer,2))
			j = 2 ;
		else if (! strncmp("SE",buffer,2))
			j = 3 ;
		else
			return(-1) ;   /* Good place to see if binary dlg file is in sync */

		fread (&dlg_coors.lat[j],   sizeof(dlg_coors.lat[j]),    1, dlg) ;
		fread (&dlg_coors.lon[j],   sizeof(dlg_coors.lon[j]),    1, dlg) ;
		fread (&dlg_coors.utm_e[j], sizeof(dlg_coors.utm_e[j]),  1, dlg) ;
		fread (&dlg_coors.utm_n[j], sizeof(dlg_coors.utm_n[j]),  1, dlg) ;
	}

	dlg_head.nlines = 14 ;

/* Read one more line for each category */

	for (i=0; i<32; i++)
		dlg_cats[i].read = 0 ;

	for (i=0; i<dlg_head.num_cats; i++)
	{
		dlg_cats[i].read = 1 ;
		fread (dlg_cats[i].name,       sizeof(*dlg_cats[i].name),    20, dlg) ;
		fread (&dlg_cats[i].form_code, sizeof(dlg_cats[i].form_code), 1, dlg) ;
		fread (&dlg_cats[i].num_nodes, sizeof(dlg_cats[i].num_nodes), 1, dlg) ;
		fread (&dlg_cats[i].act_nodes, sizeof(dlg_cats[i].act_nodes), 1, dlg) ;
		fread (&dlg_cats[i].nta_link,  sizeof(dlg_cats[i].nta_link),  1, dlg) ;
		fread (&dlg_cats[i].ntl_link,  sizeof(dlg_cats[i].ntl_link),  1, dlg) ;
		fread (&dlg_cats[i].num_areas, sizeof(dlg_cats[i].num_areas), 1, dlg) ;
		fread (&dlg_cats[i].act_areas, sizeof(dlg_cats[i].act_areas), 1, dlg) ;
		fread (&dlg_cats[i].atn_link,  sizeof(dlg_cats[i].atn_link),  1, dlg) ;
		fread (&dlg_cats[i].atl_link,  sizeof(dlg_cats[i].atl_link),  1, dlg) ;
		fread (&dlg_cats[i].area_list, sizeof(dlg_cats[i].area_list), 1, dlg) ;
		fread (&dlg_cats[i].num_lines, sizeof(dlg_cats[i].num_lines), 1, dlg) ;
		fread (&dlg_cats[i].act_lines, sizeof(dlg_cats[i].act_lines), 1, dlg) ;
		fread (&dlg_cats[i].line_list, sizeof(dlg_cats[i].line_list), 1, dlg) ;
	}
	return(0) ;
}
