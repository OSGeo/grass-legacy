#include "segment.h"
/*bugfix: buf: char* vs int* -> wrong pointer arithmetics!!!. Pierre de Mouveaux - 09 april 2000 */
/*  
 * int segment_put (SEGMENT *SEG,int *buf,int row,int col) */

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

int segment_put (SEGMENT *SEG, void *buf,int row,int col)
{
    int n;
    int index;
    int i;
    char *b, *p=buf;

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
