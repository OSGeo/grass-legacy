#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include "segment.h"
#include "gis.h"

/*	buf is CELL *   WRAT code	*/
/*  
 * int segment_put_row (SEGMENT *SEG, CELL *buf,int row) */

/*!
 * \brief write row to segment file
 *
 * Transfers nonsegmented matrix data, row by row, into a segment
 * file.  <b>Seg</b> is the segment structure that was configured from a call
 * to <i>segment_init.</i> <b>Buf</b> should contain <i>ncols*len</i>
 * bytes of data to be transferred to the segment file. <b>Row</b> specifies
 * the row from the data matrix being transferred.
 * Return codes are: 1 if ok;   else -1 could not seek or write segment file.
 *
 *  \param seg
 *  \param buf
 *  \param row
 *  \return int
 */

int segment_put_row (SEGMENT *SEG, void *buf,int row)
{
    int size;
    int ncols;
    int scols;
    int n, index, col;
	int result;

    ncols = SEG->ncols - SEG->spill ;
    scols = SEG->scols ;
    size = scols * SEG->len;
/*  	printf("segment_put_row ncols: %d, scols %d, size: %d, col %d, row: %d,  SEG->fd: %d\n",ncols,scols,size,col,row, SEG->fd); */

    for (col = 0; col < ncols; col += scols)
    {
	segment_address (SEG, row, col, &n, &index) ;
	if(segment_seek (SEG, n, index) < 0) {
	    G_warning (
	        "Failed seek in segment file for index = %d n = %d at col:row %d:%d\n",
	        index,n,col,row);
	    return -1;
	}
	if((result = write (SEG->fd, buf, size)) != size)
	{
	    G_warning ("segment_put_row write error %s\n",strerror(errno));
/*  	printf("segment_put_row result = %d. ncols: %d, scols %d, size: %d, col %d, row: %d,  SEG->fd: %d\n",result,ncols,scols,size,col,row, SEG->fd); */
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
	if(segment_seek (SEG, n, index) < 0) {
	    G_warning (
	        "Failed seek in segment file for index = %d n = %d at col:row %d:%d\n",
	        index,n,col,row);
	    return -1;
	}
	if(write (SEG->fd, buf, size) != size)
	{
	    G_warning ("segment_put_row final write error: %s\n",strerror(errno));
	    return -1;
	}
    }
    return 1;
}
