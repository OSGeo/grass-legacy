/*  @(#)r_dlg_head.c	2.1  6/26/87 
*  Created by: CERL, original in mapdev/dlg_to_bdlg
*  modified:   To ignore additional registration coord., GRASS
*              dlg_to_cell doesn't know what to do with them.
*                                 R.L.Glenn, SCS,  12-1-87 
*/

#include <stdio.h>
#include "dlghead.h" 
 
/* This routine reads the dlg header in "optional" format 
 *
 * returns:  -1 on error
 *            0 on completion
 */

read_dlg_head(dlg)
	FILE *dlg ;
{
	int i ;
	int j ;
	int skip_reg=0 ;
	char buffer[128] ;
	char *bufptr ;


/* Read first (banner) line */
	if (! fgets(buffer, 90, dlg)) return(-1) ;

	pad(buffer, 72) ;
	strncpy(banner, buffer, 80) ;
	/* Remove any trailing newline */
	bufptr = banner ;
	for (; *bufptr && *bufptr != '\n'; bufptr++) ;
	*bufptr = 0;

/* Read second line 
 *   Contains: cart_unit, source_date, orig_scale
 */
	if (! fgets(buffer, 90, dlg)) return(-1) ;

	pad(buffer, 80) ;
	strncpy(cart_unit,   buffer     , 40) ;
	strncpy(source_date, buffer + 41, 10) ;
	strncpy(orig_scale,  buffer + 52,  8) ;

/* Read third line 
 *   Used, but meaning undefined
 */
	if (! fgets(buffer, 90,dlg)) return(-1) ;
	pad(buffer, 80) ;
	strncpy(line_3, buffer, 72) ;
	bufptr = line_3 ;
	for (; *bufptr && *bufptr != '\n'; bufptr++) ;
	*bufptr = 0;

/* Read fourth line */
	if (! fgets(buffer, 90,dlg)) return(-1) ;

	sscanf(buffer,    "%d",  &level_code) ;
	sscanf(buffer+6,  "%d",  &plani_code) ;
	sscanf(buffer+12, "%d",  &plani_zone) ;
	sscanf(buffer+18, "%d",  &plani_units) ;
	_get_dtype(buffer+24,    &resolution) ;
	sscanf(buffer+42, "%d",  &trans_param) ;
	sscanf(buffer+48, "%d",  &misc_records) ;
	sscanf(buffer+54, "%d",  &num_sides) ;
	sscanf(buffer+60, "%6d", &num_cats) ;
/*----------------------------------------------------------RLG---*/
	if(num_sides != 4)
	{
		fprintf (stdout,"\nWARNING: Number of registration points defining the coverage\n");
		fprintf (stdout,"         window is greater than 4. The additional\n") ;
		fprintf (stdout,"         registration points will be ignored.\n") ;
		skip_reg = num_sides - 4 ;
		num_sides = 4 ;
	}
/*----------------------------------------------------------RLG---*/

	/* Read fifth through ninth lines */
	if (! fgets(buffer, 90,dlg)) return(-1) ;
	_get_dtype(buffer+ 0, &params[0]) ;
	_get_dtype(buffer+24, &params[1]) ;
	_get_dtype(buffer+48, &params[2]) ;

	if (! fgets(buffer, 90,dlg)) return(-1) ;
	_get_dtype(buffer+ 0, &params[3]) ;
	_get_dtype(buffer+24, &params[4]) ;
	_get_dtype(buffer+48, &params[5]) ;

	if (! fgets(buffer, 90,dlg)) return(-1) ;
	_get_dtype(buffer+ 0, &params[6]) ;
	_get_dtype(buffer+24, &params[7]) ;
	_get_dtype(buffer+48, &params[8]) ;

	if (! fgets(buffer, 90,dlg)) return(-1) ;
	_get_dtype(buffer+ 0, &params[9]) ;
	_get_dtype(buffer+24, &params[10]) ;
	_get_dtype(buffer+48, &params[11]) ;

	if (! fgets(buffer, 90,dlg)) return(-1) ;
	_get_dtype(buffer+ 0, &params[12]) ;
	_get_dtype(buffer+24, &params[13]) ;
	_get_dtype(buffer+48, &params[14]) ;

/* Read tenth line */
	if (! fgets(buffer, 90,dlg)) return(-1) ;
	_get_dtype(buffer+ 0, &coeff_params[0]) ;
	_get_dtype(buffer+18, &coeff_params[1]) ;
	_get_dtype(buffer+36, &coeff_params[2]) ;
	_get_dtype(buffer+54, &coeff_params[3]) ;

/* Read eleventh through fourteenth lines 
      these hold the SW, NW, NE, SE corner points */
	for(i=0;i<4;i++)
	{
		if (! fgets(buffer, 90,dlg)) return(-1) ;

		pad(buffer,80) ;
		strncpy(coors[i].corner, buffer, 2) ;
		sscanf(buffer+6 , "%lf", &coors[i].lat) ;
		sscanf(buffer+18, "%lf", &coors[i].lon) ;
		sscanf(buffer+36, "%lf", &coors[i].utm_n) ;
		sscanf(buffer+48, "%lf", &coors[i].utm_e) ;
	}

	nlines = 14 ;

/*----------------------------------------------------------RLG---*/
/* Skip additional registration points, if they are present */
	if (skip_reg != 0)
	   {
 	   for(i=0;i<skip_reg;i++)
	      {
		if (! fgets(buffer, 90,dlg)) return(-1) ;
	      
	      }
	   }
/*----------------------------------------------------------RLG---*/

/* Read one more line for each category */

	for (i=0; i<num_cats; i++)
	{
		if (! fgets(buffer, 90,dlg)) return(-2) ;
		nlines++ ;
		pad(buffer,80) ;
		strncpy(cats[i].name,buffer,20) ;
		cats[i].name[20] = NULL ;
		sscanf(buffer+20, "%4d", &cats[i].form_code) ;
		sscanf(buffer+24, "%6d", &cats[i].num_nodes) ;
		sscanf(buffer+30, "%6d", &cats[i].act_nodes) ;
		sscanf(buffer+37, "%1d", &cats[i].nta_link ) ;
		sscanf(buffer+38, "%1d", &cats[i].ntl_link ) ;
		sscanf(buffer+40, "%6d", &cats[i].num_areas) ;
		sscanf(buffer+46, "%6d", &cats[i].act_areas) ;
		sscanf(buffer+53, "%1d", &cats[i].atn_link ) ;
		sscanf(buffer+54, "%1d", &cats[i].atl_link ) ;
		sscanf(buffer+55, "%1d", &cats[i].area_list) ;
		sscanf(buffer+56, "%6d", &cats[i].num_lines) ;
		sscanf(buffer+62, "%6d", &cats[i].act_lines) ;
		sscanf(buffer+71, "%1d", &cats[i].line_list) ;
	}
	return(0) ;
}

static
_get_dtype(buf, doub)
	char *buf ;
	double *doub ;
{
	char *strchr(), *position ;
	double raise ;
	double pow() ;
	int pwr ;

	raise = 10.0 ;
	pwr = 0 ;
	sscanf(buf,"%lf", doub) ;
	if (*doub != 0.0)
           {
           position = buf;
           if ((position = strchr(buf,'D')) == 0)
                position = strchr(buf,'E');
           position++;
           sscanf(position,"%d", &pwr) ;
           }
	if (pwr)
		*doub = *doub * pow(raise, (double)pwr) ;
}

pad(s,n)
	char *s ;
	int n ;
{
	int fill ;

	fill = 0 ;

	while(n--)
	{
		if(*s == NULL  ||  *s == '\n')
			fill = 1 ;
		if(fill)
			*s = ' ' ;
		s++ ;
	}
	*s = NULL ;
}
