/*
 * $Id$
 */
 
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "segment.h"

/*  int segment_get_row (SEGMENT *SEG, CELL *buf,int row) */
int segment_get_row (SEGMENT *SEG, void *buf,int row)
{
    int size;
    int ncols;
    int scols;
    int n, index, col;

    ncols = SEG->ncols - SEG->spill ;
    scols = SEG->scols ;
    size = scols * SEG->len;

    for (col = 0; col < ncols; col += scols)
    {
	segment_address (SEG, row, col, &n, &index) ;
	if(segment_seek (SEG, n, index) < 0)
	    return -1;
	if(read (SEG->fd, buf, size) != size)
	{
	    G_warning ("segment_get_row: %s\n",strerror(errno));
	    return -1;
	}
	
	/* The buf variable is a void pointer and thus points to anything. */
	/* Therefore, it's size is unknown and thus, it cannot be used for */
	/* pointer arithmetic (some compilers treat this as an error - SGI */
	/* MIPSPro compiler for one). Since the read command is reading in */
	/* "size" bytes, cast the buf variable to char * before incrementing */
	buf = ((char *) buf) + size;
    }
    if ((size = SEG->spill * SEG->len))
    {
	segment_address (SEG, row, col, &n, &index) ;
	if(segment_seek (SEG, n, index) < 0)
	    return -1;
	if(read (SEG->fd, buf, size) != size)
	{
	    G_warning ("segment_get_row: %s\n",strerror(errno));
	    return -1;
	}
    }

    return 1;
}
