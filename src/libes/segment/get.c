#include "segment.h"

int segment_get (SEGMENT *SEG, register int *buf,int row,int col)
{
    int n;
    int index;
    int i;
    register char *b;

    segment_address (SEG, row, col, &n, &index);
    if((i = segment_pagein (SEG, n)) < 0)
	return -1;
    b = &SEG->scb[i].buf[index];

    n = SEG->len;
    while (n-- > 0)
	*buf++ = *b++;
    
    return 1;
}
