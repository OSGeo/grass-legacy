#include <stdio.h>
static int rec_num = 1 ;
static int maxrow, minrow ;
static float maxcol, mincol ;

set_limits(numrows, numcols) 
	int numrows, numcols ;
{
	maxrow = numrows ;
	maxcol = (float)(numcols - 1) ;
	minrow = 1 ;
	mincol = 0.0 ;
}

check_limits(row, first_cell, last_cell)
	int *row ;
	float *first_cell, *last_cell ;
{
	if (*row < minrow)
		return(0) ;
	if (*row > maxrow)
		return(0) ;
	if (*first_cell > maxcol)
		return(0) ;
	if (*last_cell < mincol)
		return(0) ;
	if (*first_cell < mincol)
		*first_cell = mincol ;
	if (*last_cell > maxcol)
		*last_cell = maxcol ;
	*last_cell = maxcol - *last_cell ;
	return(1) ;
}

write_record(row, first_cell, last_cell, category ) 
	int row, category ;
	float first_cell, last_cell ;
{
	float fc, lc ;

	fc = first_cell ;
	lc = last_cell ;

	if (check_limits(&row, &fc, &lc))
		printf ("%d %8d:%d:%d:%d\n", 
			row,
			rec_num++,
			(int)(100. * fc),
			(int)(100. * lc),
			category) ;
}

write_end_record(row, first_cell, last_cell, category ) 
	int row, first_cell, last_cell, category ;
{
		printf ("%d %8d:%d:%d:%d\n", 
			row, rec_num++, first_cell, last_cell, category) ;
}
