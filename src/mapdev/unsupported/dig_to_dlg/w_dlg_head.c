/*  @(#)w_dlg_head.c	2.1  6/26/87  */
#include <stdio.h>
#include "dlg.h"
static char *compass[4] = {"SW","NW","NE","SE"} ;

write_dlg_header(dlgout)
	FILE *dlgout ;
{
	char *fgets() ;
	char buff1[64] ;
	char buff2[64] ;
	char buff3[64] ;
	char buff4[64] ;
	int i ;
	int j ;

/* Write banner (first) line */
	fprintf(dlgout,"%s\n",dlg_head.banner) ;

/* Write second line */
	fprintf(dlgout,"%-40s %-10s %-8s\n",
		dlg_head.cart_unit,
		dlg_head.source_date,
		dlg_head.orig_scale) ;

/* Write third line */
	/* Third line is used, but its meaning is not documented */
	fprintf(dlgout,"\n") ;

/* Write fourth line */
	_put_dtype(buff1, &dlg_head.resolution, 18, 11) ;
	fprintf(dlgout,"%6d%6d%6d%6d%-18s%6d%6d%6d%6d\n",
		dlg_head.level_code,
		dlg_head.plani_code,
		dlg_head.plani_zone,
		dlg_head.plani_units,
		buff1,
		dlg_head.trans_param,
		dlg_head.misc_records,
		dlg_head.num_sides,
		dlg_head.num_cats) ;

/* Write fifth through ninth lines */
	_put_dtype(buff1, &dlg_proj.params[0], 24, 15) ;
	_put_dtype(buff2, &dlg_proj.params[1], 24, 15) ;
	_put_dtype(buff3, &dlg_proj.params[2], 24, 15) ;
	fprintf(dlgout,"%-24s%-24s%-24s\n", buff1, buff2, buff3) ;
	_put_dtype(buff1, &dlg_proj.params[3], 24, 15) ;
	_put_dtype(buff2, &dlg_proj.params[4], 24, 15) ;
	_put_dtype(buff3, &dlg_proj.params[5], 24, 15) ;
	fprintf(dlgout,"%-24s%-24s%-24s\n", buff1, buff2, buff3) ;
	_put_dtype(buff1, &dlg_proj.params[6], 24, 15) ;
	_put_dtype(buff2, &dlg_proj.params[7], 24, 15) ;
	_put_dtype(buff3, &dlg_proj.params[8], 24, 15) ;
	fprintf(dlgout,"%-24s%-24s%-24s\n", buff1, buff2, buff3) ;
	_put_dtype(buff1, &dlg_proj.params[9], 24, 15) ;
	_put_dtype(buff2, &dlg_proj.params[10], 24, 15) ;
	_put_dtype(buff3, &dlg_proj.params[11], 24, 15) ;
	fprintf(dlgout,"%-24s%-24s%-24s\n", buff1, buff2, buff3) ;
	_put_dtype(buff1, &dlg_proj.params[12], 24, 15) ;
	_put_dtype(buff2, &dlg_proj.params[13], 24, 15) ;
	_put_dtype(buff3, &dlg_proj.params[14], 24, 15) ;
	fprintf(dlgout,"%-24s%-24s%-24s\n", buff1, buff2, buff3) ;

/* Write tenth line */
	_put_dtype(buff1, &dlg_proj.int_params[0], 18, 11) ;
	_put_dtype(buff2, &dlg_proj.int_params[1], 18, 11) ;
	_put_dtype(buff3, &dlg_proj.int_params[2], 18, 11) ;
	_put_dtype(buff4, &dlg_proj.int_params[3], 18, 11) ;
	fprintf(dlgout,"%-18s%-18s%-18s%-18s\n", buff1, buff2, buff3, buff4) ;

/* Write eleventh through forteenth lines */
	for(i=0;i<4;i++)
	{
		fprintf(dlgout,"%-2s    %12.6f%12.6f      %12.2f%12.2f\n",
			compass[i],
			dlg_coors.lat[i],
			dlg_coors.lon[i],
			dlg_coors.utm_e[i],
			dlg_coors.utm_n[i]) ;
	}

/* Write one more line for each category */

	for (i=0; i<dlg_head.num_cats; i++)
	{
		if (! dlg_cats[i].read)
		{
			printf("ERROR: category number %d not available for dlg header\n") ;
			exit(-1) ;
		}
		fprintf(dlgout,"%-20s%4d%6d%6d %1d%1d %6d%6d %1d%1d%1d%6d%6d   %1d\n",
			dlg_cats[i].name,
			dlg_cats[i].form_code,
			dlg_cats[i].num_nodes,
			dlg_cats[i].act_nodes,
			dlg_cats[i].nta_link,
			dlg_cats[i].ntl_link,
			dlg_cats[i].num_areas,
			dlg_cats[i].act_areas,
			dlg_cats[i].atn_link,
			dlg_cats[i].atl_link,
			dlg_cats[i].area_list,
			dlg_cats[i].num_lines,
			dlg_cats[i].act_lines,
			dlg_cats[i].line_list) ;
	}
	return(0) ;
}

_put_dtype(buff, value, width, deci)
	char *buff ;
	double *value ;
	int width ;
	int deci ;
{
	char form[64] ;
	char *strchr() ;

	if (*value == 0.0)
	{
		sprintf(buff,"%6.1lf",*value) ;
	}
	else
	{
		sprintf(form,"%%%d.%dle",width, deci) ;
		sprintf(buff,form,*value) ;
		*(strchr(buff,'e')) = 'D' ;
		return ;
	}
}
