/*  @(#)b_a_head.c	2.4  8/28/87  */
/*
 * This routine reads the dlg header in binary format and writes
 * it out in dlg "optional" ascii format.
 *
 * returns:  -1 on error
 *            0 on completion
 */

#include <stdio.h>
 
b_d_head(bin, dlg)
	FILE *bin ;
	FILE *dlg ;
{
	int nlines ;
	char banner[73] ;
	char cart_unit[42] ;
	char source_date[12] ;
	char orig_scale[10] ;
	char line_3[73] ;
	int level_code ;
	int plani_code ;
	int plani_zone ;
	int plani_units ;
	double resolution ;
	int trans_param ;
	int misc_records ;
	int num_sides ;
	int num_cats ;

	double lat ;
	double lon ;
	double utm_n ;
	double utm_e ;

	double params[15] ;
	double int_params[4] ;

	int read ;
	char name[21] ;
	int form_code ;
	int num_nodes ;
	int act_nodes ;
	int nta_link ;
	int ntl_link ;
	int num_areas ;
	int act_areas ;
	int atn_link ;
	int atl_link ;
	int area_list ;
	int num_lines ;
	int act_lines ;
	int line_list ;
	int i ;
	int j ;

	char buffer[81] ;
	char corner[3] ;
	char buff1[64] ;
	char buff2[64] ;
	char buff3[64] ;
	char buff4[64] ;

/* Read first (banner) line */
	fread(buffer, sizeof(*buffer), 72, bin) ;
	pad(buffer, 72) ;

/* Write first line */
	fprintf(dlg, "%s\n", buffer) ;

/* Read second line 
 *   Contains: cart_unit, source_date, orig_scale
 */
	fread(cart_unit,   sizeof(*cart_unit),   40, bin) ;   /* cart_unit    */
	fread(source_date, sizeof(*source_date), 10, bin) ;   /* source_date  */
	fread(orig_scale,  sizeof(*orig_scale),   8, bin) ;   /* orig_scale   */
	pad(cart_unit, 40) ;
	pad(source_date, 10) ;
	pad(orig_scale, 8) ;

	fprintf(dlg, "%40s %10s %8s\n", cart_unit, source_date, orig_scale) ;

/* Read third line 
 *   Used, but meaning undefined
 */
	fread(line_3, sizeof(*line_3), 72, bin) ;
	fprintf(dlg, "%s\n", line_3) ;

/* Read fourth line */
	fread (&level_code,  sizeof(level_code),  1, bin) ;
	fread (&plani_code,  sizeof(plani_code),  1, bin) ;
	fread (&plani_zone,  sizeof(plani_zone),  1, bin) ;
	fread (&plani_units, sizeof(plani_units), 1, bin) ;
	fread (&resolution,  sizeof(resolution),  1, bin) ;
	fread (&trans_param, sizeof(trans_param), 1, bin) ;
	fread (&misc_records,sizeof(misc_records),1, bin) ;
	fread (&num_sides,   sizeof(num_sides),   1, bin) ;
	fread (&num_cats,    sizeof(num_cats),    1, bin) ;

	_put_dtype(buff1, &resolution, 18, 11) ;
	fprintf(dlg, "%6d%6d%6d%6d%-18s%6d%6d%6d%6d\n",
		level_code, plani_code, plani_zone, plani_units, buff1,
		trans_param, misc_records, num_sides, num_cats) ;

/* Read fifth through ninth lines */
	fread (params, sizeof(*params), 15, bin) ;
	
	_put_dtype(buff1, &params[0], 24, 15) ;
	_put_dtype(buff2, &params[1], 24, 15) ;
	_put_dtype(buff3, &params[2], 24, 15) ;
	fprintf(dlg,"%-24s%-24s%-24s\n", buff1, buff2, buff3) ;
	
	_put_dtype(buff1, &params[3], 24, 15) ;
	_put_dtype(buff2, &params[4], 24, 15) ;
	_put_dtype(buff3, &params[5], 24, 15) ;
	fprintf(dlg,"%-24s%-24s%-24s\n", buff1, buff2, buff3) ;
	
	_put_dtype(buff1, &params[6], 24, 15) ;
	_put_dtype(buff2, &params[7], 24, 15) ;
	_put_dtype(buff3, &params[8], 24, 15) ;
	fprintf(dlg,"%-24s%-24s%-24s\n", buff1, buff2, buff3) ;
	
	_put_dtype(buff1, &params[ 9], 24, 15) ;
	_put_dtype(buff2, &params[10], 24, 15) ;
	_put_dtype(buff3, &params[11], 24, 15) ;
	fprintf(dlg,"%-24s%-24s%-24s\n", buff1, buff2, buff3) ;
	
	_put_dtype(buff1, &params[12], 24, 15) ;
	_put_dtype(buff2, &params[13], 24, 15) ;
	_put_dtype(buff3, &params[14], 24, 15) ;
	fprintf(dlg,"%-24s%-24s%-24s\n", buff1, buff2, buff3) ;

/* Read tenth line */
	fread (int_params, sizeof(*int_params), 4, bin) ;

	_put_dtype(buff1, &int_params[0], 18, 11) ;
	_put_dtype(buff2, &int_params[1], 18, 11) ;
	_put_dtype(buff3, &int_params[2], 18, 11) ;
	_put_dtype(buff4, &int_params[3], 18, 11) ;
	fprintf(dlg,"%-18s%-18s%-18s%-18s\n", buff1, buff2, buff3, buff4) ;

/* Read eleventh through forteenth lines */
	for(i=0;i<4;i++)
	{
		fread (corner, sizeof(*corner),2, bin) ;
		fread (&lat,   sizeof(lat),    1, bin) ;
		fread (&lon,   sizeof(lon),    1, bin) ;
		fread (&utm_n, sizeof(utm_n),  1, bin) ;
		fread (&utm_e, sizeof(utm_e),  1, bin) ;

		fprintf(dlg, "%1c%1c    %12.6lf%12.6lf      %12.2lf%12.2lf\n",
			*corner, *(corner+1), lat, lon, utm_n, utm_e) ;
	}
	nlines = 14 ;

/* Read one more line for each category */

	for (i=0; i<num_cats; i++)
	{
		fread (name,       sizeof(*name),    20, bin) ;
		fread (&form_code, sizeof(form_code), 1, bin) ;
		fread (&num_nodes, sizeof(num_nodes), 1, bin) ;
		fread (&act_nodes, sizeof(act_nodes), 1, bin) ;
		fread (&nta_link,  sizeof(nta_link),  1, bin) ;
		fread (&ntl_link,  sizeof(ntl_link),  1, bin) ;
		fread (&num_areas, sizeof(num_areas), 1, bin) ;
		fread (&act_areas, sizeof(act_areas), 1, bin) ;
		fread (&atn_link,  sizeof(atn_link),  1, bin) ;
		fread (&atl_link,  sizeof(atl_link),  1, bin) ;
		fread (&area_list, sizeof(area_list), 1, bin) ;
		fread (&num_lines, sizeof(num_lines), 1, bin) ;
		fread (&act_lines, sizeof(act_lines), 1, bin) ;
		fread (&line_list, sizeof(line_list), 1, bin) ;
		fprintf(dlg,"%20s%4d%6d%6d %1d%1d %6d%6d %1d%1d%1d%6d%6d   %1d\n",
			name, form_code, num_nodes, act_nodes, nta_link, ntl_link,
			num_areas, act_areas, atn_link, atl_link,
			area_list, num_lines, act_lines, line_list) ;
	}
	return(0) ;
}
