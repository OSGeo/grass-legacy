/********************************************************************
 * this file has debug versions of segment_get() and segment_put()
 * which check the row,col and print error messages to stderr upon
 * violations
 *
 * No functional change, just additional overhead to check the row,col.
 *
 * load the debug.o file before the SEGMENTLIB and the debug versions
 * will supercede the original.
 ******************************************************************/
#include <stdio.h>
#include "segment.h"

static int check(SEGMENT *,int,int,char *);


/*!
 * \brief get value  from segment file
 *
 * Provides random read access to the segmented data. It gets
 * <i>len</i> bytes of data into <b>value</b> from the segment file
 * <b>seg</b> for the corresponding <b>row</b> and <b>col</b> in the
 * original data matrix.
 * Return codes are:  1 if ok;  else -1 could not seek or read segment file.
 *
 *  \param seg
 *  \param value
 *  \param row
 *  \param col
 *  \return int
 */

int segment_get (SEGMENT *SEG,void *buf,int row,int col)
{
    int n;
    int index;
    int i;
    char *b, *p=buf;

    if (!check(SEG, row, col, "segment_get"))
	return -1;

    segment_address (SEG, row, col, &n, &index);
    if((i = segment_pagein (SEG, n)) < 0)
	return -1;
    b = &SEG->scb[i].buf[index];

    n = SEG->len;
    while (n-- > 0)
	*p++ = *b++;
    
    return 1;
}


/*!
 * \brief put value to segment file
 *
 * Provides random write access to the segmented data. It
 * copies <i>len</i> bytes of data from <b>value</b> into the segment
 * structure <b>seg</b> for the corresponding <b>row</b> and <b>col</b> in
 * the original data matrix.
 * The data is not written to disk immediately. It is stored in a memory segment
 * until the segment routines decide to page the segment to disk.
 * Return codes are: 1 if ok; else -1 could not seek or write segment file.
 *
 *  \param seg
 *  \param value
 *  \param row
 *  \param col
 *  \return int
 */

int segment_put (SEGMENT *SEG,void *buf,int row,int col)
{
    int n;
    int index;
    int i;
    char *b, *p = buf;

    if (!check(SEG, row, col, "segment_put"))
	return -1;

    segment_address (SEG, row, col, &n, &index);
    if((i = segment_pagein (SEG, n)) < 0)
	return -1;
    b = &SEG->scb[i].buf[index];
    SEG->scb[i].dirty = 1;

    n = SEG->len;
    while (n-- > 0)
	*b++ = *p++;
    return 1;
}

static int check(SEGMENT *SEG,int row,int col,char *me)
{
    int r,c;
    r = row >= 0 && row < SEG->nrows;
    c = col >= 0 && col < SEG->ncols;
    if (r && c) return 1;

    fprintf (stderr, "%s(SEG=%lx,fd=%d,row=%d,col=%d) ",
	me, (long int)SEG, SEG->fd, row, col);
    if (!r)
    {
	fprintf (stderr, "bad row ");
	if (row >= SEG->nrows)
	    fprintf (stderr, "(max %d) ", SEG->nrows-1);
    }
    if (!c)
    {
	fprintf (stderr, "bad col ");
	if (col >= SEG->ncols)
	    fprintf (stderr, "(max %d) ", SEG->ncols-1);
    }
    fprintf (stderr, "\n");
    return 0;
}
