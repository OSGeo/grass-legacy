#include "segment.h"
/*bugfix: buf: char* vs int* -> wrong pointer arithmetics!!!. Pierre de Mouveaux - 09 april 2000 */
/*  int segment_put (SEGMENT *SEG,int *buf,int row,int col) */
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
