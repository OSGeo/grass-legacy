/*  @(#)read_head.c	2.1  6/26/87  */
#include <stdio.h>
#include "dlg.h"
#include "dig.h"

read_digit_head(digit, thresh)
	FILE *digit ;
	double *thresh ;
{
	struct dig_head *dig_head ;
	int i ;
	char message[128] ;

	dig_head = (struct dig_head *) falloc(1, sizeof(struct dig_head)) ;

	fseek(digit, 0L, 0) ;

	fread(dig_head->organization, sizeof(dig_head->organization), 1,digit) ;
	fread(dig_head->date,         sizeof(dig_head->date),         1,digit) ;
	fread(dig_head->your_name,    sizeof(dig_head->your_name),    1,digit) ;
	fread(dig_head->map_name,     sizeof(dig_head->map_name),     1,digit) ;
	fread(dig_head->source_date,  sizeof(dig_head->source_date),  1,digit) ;
	fread(dig_head->line_3,       sizeof(dig_head->line_3),       1,digit) ;
	fread(&dig_head->orig_scale,  sizeof(dig_head->orig_scale),   1,digit) ;
	fread(&dig_head->plani_zone,  sizeof(dig_head->plani_zone),   1,digit) ;
	fread(&dig_head->W,           sizeof(dig_head->W),            1,digit) ;
	fread(&dig_head->E,           sizeof(dig_head->E),            1,digit) ;
	fread(&dig_head->S,           sizeof(dig_head->S),            1,digit) ;
	fread(&dig_head->N,           sizeof(dig_head->N),            1,digit) ;
	fread(&dig_head->map_thresh,  sizeof(dig_head->map_thresh),   1,digit) ;

/* set thresh value for return to MAIN */
	*thresh = dig_head->map_thresh ;

/* set dlg header information */
	sprintf(dlg_head.banner, "%-.30s %-.20s %-.20s", 
		dig_head->organization, dig_head->date, dig_head->your_name) ;
	strcpy (dlg_head.cart_unit, dig_head->map_name) ;
	strcpy (dlg_head.source_date, dig_head->source_date) ;
	sprintf(dlg_head.orig_scale, "%d", dig_head->orig_scale) ;
	strcpy (dlg_head.line_3, dig_head->line_3) ;
	dlg_head.level_code   = 3 ;
	dlg_head.plani_code   = 1 ;    /* DLG level 3 */
	dlg_head.plani_zone   = dig_head->plani_zone ;
	dlg_head.plani_units  = 2 ;    /* meters */
	dlg_head.resolution   = dig_head->map_thresh ;
	dlg_head.trans_param  = 4 ;
	dlg_head.misc_records = 0 ;
	dlg_head.num_sides    = 4 ;
	dlg_head.num_cats     = 1 ;

	dlg_coors[SE].utm_n = dlg_coors[SW].utm_n = dig_head->S ;
	dlg_coors[NE].utm_n = dlg_coors[NW].utm_n = dig_head->N ;
	dlg_coors[SE].utm_e = dlg_coors[NE].utm_e = dig_head->E ;
	dlg_coors[SW].utm_e = dlg_coors[NW].utm_e = dig_head->W ;
	dlg_coors[SW].lat = 0.0 ;
	dlg_coors[SW].lon = 0.0 ;
	dlg_coors[NW].lat = 0.0 ;
	dlg_coors[NW].lon = 0.0 ;
	dlg_coors[NE].lat = 0.0 ;
	dlg_coors[NE].lon = 0.0 ;
	dlg_coors[SE].lat = 0.0 ;
	dlg_coors[SE].lon = 0.0 ;
	dlg_proj.params[0] = (dlg_coors[SW].lon+dlg_coors[NW].lon)/2.0;
	dlg_proj.params[1] = (dlg_coors[NW].lat+dlg_coors[NE].lat)/2.0;
	for(i=2; i<15; i++)
		dlg_proj.params[i] = 0.0 ;
	dlg_proj.int_params[0] = 1.0 ;
	dlg_proj.int_params[1] = 0.0 ;
	dlg_proj.int_params[2] = 0.0 ;
	dlg_proj.int_params[3] = 0.0 ;

/* Print some info to screen */
	sprintf(message, " %s", dlg_head.banner) ;
		Write_base(2, message) ;
	sprintf(message, " Date:           %s", dlg_head.source_date) ;
		Write_base(3, message) ;
	sprintf(message, " Scale:          %s", dlg_head.orig_scale) ;
		Write_base(4, message) ;
	sprintf(message, " Digitizing res: %12.2lf", dig_head->map_thresh ) ;
		Write_base(5, message) ;
	sprintf(message, " %s", dlg_head.line_3) ;
		Write_base(6, message) ;
	sprintf(message, "      Digitizing window") ;
		Write_base(7, message) ;
	sprintf(message, "        N:%12.2lf  S:%12.2lf\n",
		dlg_coors[NE].utm_n, dlg_coors[SW].utm_n) ;
		Write_base(8, message) ;
	sprintf(message, "        E:%12.2lf  W:%12.2lf\n",
		dlg_coors[NE].utm_e, dlg_coors[SW].utm_e) ;
		Write_base(9, message) ;

	 if(  ! (dig_head->S * dig_head->N * dig_head->E * dig_head->W) )
	  {
		Write_info(2, "  Need to have all the area edges.") ;
		sleep (3) ;
		return (0) ;
	  }
	
	free(dig_head) ;
	return (1) ;
}
