/* @(#)convert.c	2.2  9/21/87 */
#include "gis.h"
#include "dlg.h"

static int num_rows, num_cols ;
static double south, west ;
static double ew_res, ns_res ;
static double dnrows ;

prime_convert(header)
	struct Cell_head *header ;
{
	num_rows = header->rows ;
	num_cols = header->cols ;
	south    = header->south ;
	west     = header->west ;
	ew_res   = header->ew_res ;
	ns_res   = header->ns_res ;
	dnrows   = (double)num_rows ;

	set_limits(num_rows, num_cols)  ;
}

area_convert(xarr, yarr, n)
	double *xarr, *yarr ;
	int n ;
{
	int i ;
	double *ptr ;

	for(i=n, ptr=xarr; i; i--, ptr++)
		if (*ptr != ISLAND_MARKER)
			*ptr = ( *ptr - west ) / ew_res ;

	for(i=n, ptr=yarr; i; i--, ptr++)
		if (*ptr != ISLAND_MARKER)
			*ptr = dnrows - ( *ptr - south ) / ns_res ;
}

line_convert(arr, n)
	double *arr ;
	int n ;
{
	int i ;
	double *xptr ;
	double *yptr ;

	xptr = arr ;
	yptr = arr + 1 ;

	for(i=n; i; i--)
	{
		*xptr = ( *xptr - west ) / ew_res ;

		*yptr = dnrows - ( *yptr - south ) / ns_res ;
		
		xptr+=2 ;
		yptr+=2 ;
	}
}

wrapup()
{
	write_end_record(num_rows+1, num_rows, num_cols, 0) ;
	write_end_record(	0, num_rows, num_cols, 0) ;
}
