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

put_row (fd, buf, row)
    unsigned char *buf;
{
    CELL *c;
    int ncols;

    ncols = G_window_cols();
    c = tape.cellbuf;
    while(ncols-- > 0)
	*c++ = *buf++;
    G_put_map_row (fd, tape.cellbuf);
}
