#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "segment.h"

/* bugfix:
 * int segment_get_row (SEGMENT *SEG, CELL *buf,int row) */

/*!
 * \brief read row from segment file
 *
 * Transfers data from a segment file, row by row, into memory
 * (which can then be written to a regular matrix file). <b>Seg</b> is the
 * segment structure that was configured from a call to <i>segment_init.</i>
 * <b>Buf</b> will be filled with <i>ncols*len</i> bytes of data
 * corresponding to the <b>row</b> in the data matrix.
 * Return codes are: 1 if ok; else -1 could not seek or read segment file.
 *
 *  \param seg
 *  \param buf
 *  \param row
 *  \return int
 */

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
