/* %W% %G% */
#include <stdio.h>
static int rec_num = 1 ;
static int maxrow, minrow ;
static int maxcol, mincol ;

set_limits(numrows, numcols) 
    int numrows, numcols ;
{
    maxrow = numrows ;
    maxcol = numcols - 1 ;
    minrow = 1 ;
    mincol = 0 ;
}

check_limits(row, first_cell, last_cell)
    int *row, *first_cell, *last_cell ;
{
    if (*row < minrow)
	return(0) ;
    if (*row > maxrow)
	return(0) ;

    *first_cell = (*first_cell > mincol) ? *first_cell : mincol ;
    *last_cell  = (*last_cell  < maxcol) ? *last_cell  : maxcol ;
    return(1) ;
}

write_record(row, first_cell, last_cell, file) 
    int row, first_cell, last_cell;
    FILE *file;
{
    if (check_limits(&row, &first_cell, &last_cell))
	fprintf (file,"%6d%6d%6d\n", row, first_cell, last_cell) ;
}
