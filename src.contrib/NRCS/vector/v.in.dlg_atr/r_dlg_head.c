/*  @(#)r_head.c	2.2  8/26/87  */
/*
 * This routine reads the dlg header in "optional" format 
 *
 * returns:  -1 on error
 *            0 on completion
 */

#include <stdio.h>
#include "gis.h"
#include "dlg.h"

 
read_dlg_head(dlg,dlgstr)
        FILE   *dlg ;
	struct dlg *dlgstr;
{
	int i ;
	int j ;
	char buffer[128] ;
	char *bufptr ;


/* Read first (banner) line */
	if (! fgets(buffer, 90, dlg)) return(-1) ;

	pad(buffer, 72) ;
	strncpy(dlgstr->head.banner, buffer, 80) ;
	/* Remove any trailing newline */
	bufptr = dlgstr->head.banner ;
	for (; *bufptr && *bufptr != '\n'; bufptr++) ;
	*bufptr = 0;

/* Read second line 
 *   Contains: cart_unit, source_date, orig_scale
 */
	if (! fgets(buffer, 90, dlg)) return(-1) ;

	pad(buffer, 80) ;
	strncpy(dlgstr->head.cart_unit,   buffer     , 40) ;
	strncpy(dlgstr->head.source_date, buffer + 41, 10) ;
	strncpy(dlgstr->head.orig_scale,  buffer + 52,  8) ;

/* Read third line 
 *   Used, but meaning undefined
 */
	if (! fgets(buffer, 90,dlg)) return(-1) ;
	pad(buffer, 80) ;
	strncpy(dlgstr->head.line_3, buffer, 72) ;
	bufptr = dlgstr->head.line_3 ;
	for (; *bufptr && *bufptr != '\n'; bufptr++) ;
	*bufptr = 0;

/* Read fourth line */
	if (! fgets(buffer, 90,dlg)) return(-1) ;

	sscanf(buffer,    "%d",  &(dlgstr->head.level_code)) ;
	sscanf(buffer+6,  "%d",  &(dlgstr->head.plani_code)) ;
	sscanf(buffer+12, "%d",  &(dlgstr->head.plani_zone)) ;
	sscanf(buffer+18, "%d",  &(dlgstr->head.plani_units)) ;
	_get_dtype(buffer+24,    &(dlgstr->head.resolution)) ;
	sscanf(buffer+42, "%d",  &(dlgstr->head.trans_param)) ;
	sscanf(buffer+48, "%d",  &(dlgstr->head.misc_records)) ;
	sscanf(buffer+54, "%d",  &(dlgstr->head.num_sides)) ;
	sscanf(buffer+60, "%6d", &(dlgstr->head.num_cats)) ;

	if(dlgstr->head.num_sides < 4)
	{
		printf("ERROR: Number of sides in the polygon defining the coverage\n");
		printf("         of the map less than 4.\n") ;
		printf("%s\n", buffer) ;
		printf("Another words the Dlg header is incorrect.\n", buffer) ;
		printf("Dlg header line 4, data element 8.\n", buffer) ;
		exit(-1) ;
	}


	/* Read fifth through ninth lines */
	if (! fgets(buffer, 90,dlg)) return(-1) ;
	_get_dtype(buffer+ 0, &(dlgstr->proj.params[0])) ;
	_get_dtype(buffer+24, &(dlgstr->proj.params[1])) ;
	_get_dtype(buffer+48, &(dlgstr->proj.params[2])) ;

	if (! fgets(buffer, 90,dlg)) return(-1) ;
	_get_dtype(buffer+ 0, &(dlgstr->proj.params[3])) ;
	_get_dtype(buffer+24, &(dlgstr->proj.params[4])) ;
	_get_dtype(buffer+48, &(dlgstr->proj.params[5])) ;

	if (! fgets(buffer, 90,dlg)) return(-1) ;
	_get_dtype(buffer+ 0, &(dlgstr->proj.params[6])) ;
	_get_dtype(buffer+24, &(dlgstr->proj.params[7])) ;
	_get_dtype(buffer+48, &(dlgstr->proj.params[8])) ;

	if (! fgets(buffer, 90,dlg)) return(-1) ;
	_get_dtype(buffer+ 0, &(dlgstr->proj.params[9])) ;
	_get_dtype(buffer+24, &(dlgstr->proj.params[10])) ;
	_get_dtype(buffer+48, &(dlgstr->proj.params[11])) ;

	if (! fgets(buffer, 90,dlg)) return(-1) ;
	_get_dtype(buffer+ 0, &(dlgstr->proj.params[12])) ;
	_get_dtype(buffer+24, &(dlgstr->proj.params[13])) ;
	_get_dtype(buffer+48, &(dlgstr->proj.params[14])) ;

/* Read tenth line */

	if (! fgets(buffer, 90,dlg)) return(-1) ;

    /*  check for complete string  */
	if(strlen(buffer) < 54)
	{
		dlgstr->proj.int_params[0] = 1.0 ;
		dlgstr->proj.int_params[1] = 0.0 ;
		dlgstr->proj.int_params[2] = 0.0 ;
		dlgstr->proj.int_params[3] = 0.0 ;
	}
	else
	{
		_get_dtype(buffer+ 0, &(dlgstr->proj.int_params[0])) ;
		_get_dtype(buffer+18, &(dlgstr->proj.int_params[1])) ;
		_get_dtype(buffer+36, &(dlgstr->proj.int_params[2])) ;
		_get_dtype(buffer+54, &(dlgstr->proj.int_params[3])) ;
	}


/* Read eleventh through forteenth lines */

/*  read the four quadrangle corners  */
	for(i=0;i<4;i++)
	{
		if (! fgets(buffer, 90,dlg)) return(-1) ;

		pad(buffer,80) ;
		strncpy(dlgstr->coors.corner+i, buffer, 2) ;
		sscanf(buffer+6, "%lf", dlgstr->coors.lat+i) ;
		sscanf(buffer+18 , "%lf", dlgstr->coors.lon+i) ;
		sscanf(buffer+36, "%lf", dlgstr->coors.utm_e+i) ;
		sscanf(buffer+48, "%lf", dlgstr->coors.utm_n+i) ;
	}

    /*  our dlg files cannot store anymore control points then the four
    *   quadrangle corners.  skip any extra control points and set the sides
    *   to four.  Spring 88.  -mh
    */
	for( ; i<dlgstr->head.num_sides; i++)
	{
		if (! fgets(buffer, 90,dlg)) return(-1) ;
	}
	dlgstr->head.num_sides = 4 ;


	dlgstr->head.nlines = 14 ;

/* Read one more line for each category */

	for (i=0; i<dlgstr->head.num_cats; i++)
	{
		if (! fgets(buffer, 90,dlg)) return(-2) ;
		dlgstr->head.nlines++ ;
		pad(buffer,80) ;
		strncpy(dlgstr->cats[i].name,buffer,20) ;
		dlgstr->cats[i].name[20] = NULL ;
		sscanf(buffer+20, "%4d", &(dlgstr->cats[i].form_code)) ;
		sscanf(buffer+24, "%6d", &(dlgstr->cats[i].num_nodes)) ;
		sscanf(buffer+30, "%6d", &(dlgstr->cats[i].act_nodes)) ;
		sscanf(buffer+37, "%1d", &(dlgstr->cats[i].nta_link )) ;
		sscanf(buffer+38, "%1d", &(dlgstr->cats[i].ntl_link )) ;
		sscanf(buffer+40, "%6d", &(dlgstr->cats[i].num_areas)) ;
		sscanf(buffer+46, "%6d", &(dlgstr->cats[i].act_areas)) ;
		sscanf(buffer+53, "%1d", &(dlgstr->cats[i].atn_link )) ;
		sscanf(buffer+54, "%1d", &(dlgstr->cats[i].atl_link )) ;
		sscanf(buffer+55, "%1d", &(dlgstr->cats[i].area_list)) ;
/*
* We don't support the lat/lon area-line list.
* We skip them. Make area_list a zero to reflect this.
*/
		dlgstr->cats[i].area_list = 0 ;
		sscanf(buffer+56, "%6d", &(dlgstr->cats[i].num_lines)) ;
		sscanf(buffer+62, "%6d", &(dlgstr->cats[i].act_lines)) ;
		sscanf(buffer+71, "%1d", &(dlgstr->cats[i].line_list)) ;
	}
	return(0) ;
}

static
_get_dtype(buf, doub)
	char *buf ;
	double *doub ;
{
	char *strchr() ;
	double raise ;
	double pow() ;
	int pwr ;

	*doub = 0.0 ;
	raise = 10.0 ;
	pwr = 0 ;
	sscanf(buf,"%lf", doub) ;
	if (*doub != 0.0)
	{
	    /* sscanf(strchr(buf,'D')+1,"%d", &pwr) ; */
	    char *p;
	    if ((p = G_index (buf, 'D')) != NULL)
		sscanf(p+1, "%d", &pwr);
		    
	}
	if (pwr)
	{
		*doub = *doub * pow(raise, (double)pwr) ;
	}
}
