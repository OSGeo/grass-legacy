/* %W% %G% */

#include <stdio.h>
#include "segment.h"

segment_get (SEG, buf, row, col)
    SEGMENT *SEG;
    char *buf;
{
    int n;
    int index;
    int i;
    char *b;

    segment_address (SEG, row, col, &n, &index);

    if((i = segment_pagein (SEG, n)) < 0)
	return -1;
    b = &SEG->scb[i].buf[index];

    n = SEG->len;
    while (n-- > 0)
	*buf++ = *b++;
    
    return 1;
}
