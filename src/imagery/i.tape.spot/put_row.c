/****************************************************************/
/* NAME: 	put_row						*/
/*								*/
/* FUNCTION:	read a row from tape and put in band file	*/
/*								*/
/* USAGE:	put_row(fd,buf,row)				*/
/*								*/
/* INPUT:	fd -- signature of file				*/
/*		buf -- buffer for the row			*/
/*		row -- number of current row			*/
/*								*/
/* OUTPUT:	none						*/
/****************************************************************/
#include "tape.h"

int 
put_row (int fd, unsigned char *buf, int row)
{
    CELL *c;
    int ncols;

    ncols = G_window_cols();
    c = tape.cellbuf;
    while(ncols-- > 0)
	*c++ = *buf++;
    G_put_raster_row (fd, tape.cellbuf, CELL_TYPE);

    return 0;
}
