/* %W% %G% */

#include "gis.h"
#include <stdio.h>

makenewcell(oldcell, newcell, table, len, search_cell, all_cell)
    int oldcell, newcell ;
    int *table ;
    int *search_cell ;
    int *all_cell ;
{
    CELL *buff[3] ;
    CELL *newbuff ;
    CELL *pointer[3] ;
    CELL *ptr ;
    int percent ;
    int oldpercent ;
    int incr ;
    int atrow, atcol ;
    int nrows, ncols;

    *search_cell  = 0 ;
    *all_cell     = 0 ;

    nrows = G_window_rows();
    ncols = G_window_cols();

    buff[0] = G_allocate_cell_buf() ;
    buff[1] = G_allocate_cell_buf() ;
    buff[2] = G_allocate_cell_buf() ;
    newbuff = G_allocate_cell_buf() ;


    fprintf(stderr, "\nPreparing intermediate map") ;

/* Prepare for first row */
    ptr = buff[0] ;
    for (incr=0; incr < ncols; incr++)
	*ptr++ = 1 ;

    G_get_map_row(oldcell, buff[1], 0) ;
    finger_cells(buff[1], table, len, ncols) ;

    oldpercent = 0 ;
    fprintf(stderr, "\nPercent done: %2d0%% ", 0) ;

    for (atrow=0; atrow<nrows-1; atrow++)
    {
	percent = (atrow * 10) / nrows ;
	if (percent != oldpercent)
	    fprintf(stderr, "\b\b\b\b\b%2d0%% ", oldpercent = percent) ;

	pointer[0] = buff[(atrow  )%3] ;
	pointer[1] = buff[(atrow+1)%3] ;
	pointer[2] = buff[(atrow+2)%3] ;
	G_get_map_row(oldcell, pointer[2], atrow+1) ;
	finger_cells(pointer[2], table, len, ncols) ;

#ifdef DEBUG
    printbuf(pointer[0]) ;
    printbuf(pointer[1]) ;
    printbuf(pointer[2]) ;
#endif

	findedges(pointer[0], pointer[1], pointer[2],
		newbuff, ncols, search_cell, all_cell) ;

#ifdef DEBUG
    printbuf(newbuff) ;
#endif

	G_put_map_row(newcell, newbuff) ;
    }

    fprintf(stderr, "\b\b\b\b\b%2d0%%\n", 10) ;

    pointer[0] = buff[(atrow  )%3] ;
    pointer[1] = buff[(atrow+1)%3] ;
    pointer[2] = buff[(atrow+2)%3] ;

    ptr = pointer[2] ;
    for (incr=0; incr < ncols; incr++)
	*ptr++ = 1 ;

    findedges(pointer[0], pointer[1], pointer[2],
		newbuff, ncols, search_cell, all_cell) ;
    G_put_map_row(newcell, newbuff) ;
}

findedges(pntr0, pntr1, pntr2, newbuff, ncols, search_cell, all_cell)
    register CELL *pntr0;
    register CELL *pntr1;
    register CELL *pntr2;
    CELL *newbuff ;
    int ncols ;
    int *search_cell ;
    int *all_cell ;
{
    int atcol ;
    register CELL *newptr ;

    newptr = newbuff ;

/* Deal with first col */
    *newptr = *pntr1 ;
    if (*pntr1)
	if (*(pntr1+1) && *pntr0 && *pntr2)
	    *newptr = 2 ;
    newptr++; pntr0++; pntr1++; pntr2++ ;

/* Deal with internal cols */
    for (atcol = 1; atcol < ncols-1; atcol++)
    {
	*newptr = *pntr1 ;
	if (*pntr1)
	    if (*(pntr1-1) && *(pntr1+1) && *pntr0 && *pntr2)
		*newptr = 2 ;
	newptr++; pntr0++; pntr1++; pntr2++ ;
    }

/* Deal with last col */
    *newptr = *pntr1 ;
    if (*pntr1)
	if (*(pntr1-1) && *pntr0 && *pntr2)
	    *newptr = 2 ;

    newptr = newbuff ;
    for (atcol = 1; atcol < ncols-1; atcol++)
    {
	if (*newptr == 1)
	    *search_cell++ ;

	if (*newptr)
	    *all_cell++ ;
    }
}

printbuf(buff)
    CELL *buff ;
{
    int incr ;

    for (incr=1; incr<15; incr++)
    {
	fprintf(stderr," %2ld", (long) buff[incr]) ;
    }
    fprintf(stderr,"\n") ;
}

finger_cells(buff, table, len, ncols)
    register CELL *buff ;
    int *table ;
    register int ncols ;
{
    register CELL value ;

    while(ncols-- > 0)
    {
	value = *buff;
	if (value >= 0 && value < len && table[value] != 0)
	    *buff++ = 1 ;
	else
	    *buff++ = 0 ;
    }
}
