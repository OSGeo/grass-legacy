/*
 ****************************************************************
 *  G_zero_cell_buf (buf)
 *     char *buf           cell buffer to be zeroed
 *
 *  G_zero_raster_buf (void *rast, RASTER_MAP_TYPE data_type)
 *
 *  Zeros out a cell buffer 
 ****************************************************************/

#include "gis.h"

int G_zero_cell_buf (register CELL *buf)
{
    register int i ;
    i = G_window_cols() ;

    while(i--)
	*buf++ = 0 ;

    return 0;
}

int G_zero_raster_buf ( register void *rast,
    RASTER_MAP_TYPE data_type)
{
    register int i ;
    register unsigned char *ptr;
    /* assuming that the size of unsigned char is 1 byte */
    i = G_window_cols() * G_raster_size(data_type) ;
    ptr = (unsigned char *) rast;

    while(i--)
	*ptr++ = 0 ;

    return 0;
}
