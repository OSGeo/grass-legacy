/*  @(#)r_bdlg_head.c	1.2  6/24/87  */
#include <stdio.h>
#include "dlg.h"

read_bdlg_head(dlgin)
	FILE *dlgin ;
{
	char buffer[81] ;
	int i ;
	int j ;

	dlg_head.nlines = 0 ;

/* Read banner (first) line */
	fread(buffer, sizeof(*buffer), 72, dlgin) ;
	/* Check for node, area or line record */
	if (! strncmp(buffer,"N ",2)) return(-1) ;
	if (! strncmp(buffer,"A ",2)) return(-1) ;
	if (! strncmp(buffer,"L ",2)) return(-1) ;
	strncpy(dlg_head.banner,buffer,72) ;
	*(dlg_head.banner+strlen(buffer)-1) = NULL ;

/* Read second line */
	fread(dlg_head.cart_unit,   sizeof(*dlg_head.cart_unit),   40, dlgin) ;
	fread(dlg_head.source_date, sizeof(*dlg_head.source_date), 10, dlgin) ;
	fread(dlg_head.orig_scale,  sizeof(*dlg_head.orig_scale),   8, dlgin) ;

/* Read third line */
	fread(dlg_head.line_3, sizeof(*dlg_head.line_3), 72, dlgin) ;

/* Read fourth line */
	fread (&dlg_head.level_code,  sizeof(dlg_head.level_code),  1, dlgin) ;
	fread (&dlg_head.plani_code,  sizeof(dlg_head.plani_code),  1, dlgin) ;
	fread (&dlg_head.plani_zone,  sizeof(dlg_head.plani_zone),  1, dlgin) ;
	fread (&dlg_head.plani_units, sizeof(dlg_head.plani_units), 1, dlgin) ;
	fread (&dlg_head.resolution,  sizeof(dlg_head.resolution),  1, dlgin) ;
	fread (&dlg_head.trans_param, sizeof(dlg_head.trans_param), 1, dlgin) ;
	fread (&dlg_head.misc_records,sizeof(dlg_head.misc_records),1, dlgin) ;
	fread (&dlg_head.num_sides,   sizeof(dlg_head.num_sides),   1, dlgin) ;
	fread (&dlg_head.num_cats,    sizeof(dlg_head.num_cats),    1, dlgin) ;
	if(dlg_head.num_sides != 4)
		return(-1) ;

/* Read fifth through ninth lines */
	fread (dlg_proj.params, sizeof(*dlg_proj.params), 15, dlgin) ;

/* Read tenth line */
	fread (dlg_proj.int_params, sizeof(*dlg_proj.int_params), 4, dlgin) ;

/* Read eleventh through forteenth lines */
	for(i=0;i<4;i++)
	{
		fread (buffer, sizeof(*buffer),2, dlgin) ;

		if (! strncmp("SW",buffer,2))
			j = 0 ;
		else if (! strncmp("NW",buffer,2))
			j = 1 ;
		else if (! strncmp("NE",buffer,2))
			j = 2 ;
		else if (! strncmp("SE",buffer,2))
			j = 3 ;
		else
		{
			Write_info(2, "Error in reading corner coors in dlg header") ;
			sleep(5) ;
			return(-1) ;
		}
		fread (&dlg_coors[j].lat,   sizeof(dlg_coors[j].lat),    1, dlgin) ;
		fread (&dlg_coors[j].lon,   sizeof(dlg_coors[j].lon),    1, dlgin) ;
		fread (&dlg_coors[j].utm_n, sizeof(dlg_coors[j].utm_n),  1, dlgin) ;
		fread (&dlg_coors[j].utm_e, sizeof(dlg_coors[j].utm_e),  1, dlgin) ;
	}

	dlg_head.nlines = 14 ;

/* Read one more line for each category */

	for (i=0; i<dlg_head.num_cats; i++)
	{
		fread (dlg_cats.name,       sizeof(*dlg_cats.name),    20, dlgin) ;
		fread (&dlg_cats.form_code, sizeof(dlg_cats.form_code), 1, dlgin) ;
		fread (&dlg_cats.num_nodes, sizeof(dlg_cats.num_nodes), 1, dlgin) ;
		fread (&dlg_cats.act_nodes, sizeof(dlg_cats.act_nodes), 1, dlgin) ;
		fread (&dlg_cats.nta_link,  sizeof(dlg_cats.nta_link),  1, dlgin) ;
		fread (&dlg_cats.ntl_link,  sizeof(dlg_cats.ntl_link),  1, dlgin) ;
		fread (&dlg_cats.num_areas, sizeof(dlg_cats.num_areas), 1, dlgin) ;
		fread (&dlg_cats.act_areas, sizeof(dlg_cats.act_areas), 1, dlgin) ;
		fread (&dlg_cats.atn_link,  sizeof(dlg_cats.atn_link),  1, dlgin) ;
		fread (&dlg_cats.atl_link,  sizeof(dlg_cats.atl_link),  1, dlgin) ;
		fread (&dlg_cats.area_list, sizeof(dlg_cats.area_list), 1, dlgin) ;
		fread (&dlg_cats.num_lines, sizeof(dlg_cats.num_lines), 1, dlgin) ;
		fread (&dlg_cats.act_lines, sizeof(dlg_cats.act_lines), 1, dlgin) ;
		fread (&dlg_cats.line_list, sizeof(dlg_cats.line_list), 1, dlgin) ;
	}
	return(0) ;
}
