/*
 ****************************************************************
 *  G_zero_cell_buf (buf)
 *     char *buf           cell buffer to be zeroed
 *
 *  Zeros out a cell buffer 
 ****************************************************************/

#include "gis.h"

G_zero_cell_buf (buf)
    register CELL *buf ;
{
    register int i ;
    i = G_window_cols() ;

    while(i--)
	*buf++ = 0 ;
}
