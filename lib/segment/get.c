#include "segment.h"

/*bugfix: buf: char* vs int* -> wrong pointer arithmetics!!!. Pierre de Mouveaux - 09 april 2000 */
/*  
 * int segment_get (SEGMENT *SEG, register int *buf,int row,int col) */

/*!
 * \brief get value
 *       from segment file
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
    register char *b, *p=buf;

    segment_address (SEG, row, col, &n, &index);
    if((i = segment_pagein (SEG, n)) < 0)
	return -1;
    b = &SEG->scb[i].buf[index];

    n = SEG->len;
    while (n-- > 0)
	*p++ = *b++;
    
    return 1;
}
