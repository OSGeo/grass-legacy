/* %W% %G% */

#include <stdio.h>
#include "segment.h"

/* JNK 07/17/91 -- new external routine removed from pagein.c file
                   where it was local                              */

segment_select (SEG, n)
    SEGMENT *SEG;
{
    int i;

    SEG->scb[n].age = 0;
    for (i = 0; i < SEG->nseg; i++)
	SEG->scb[i].age++ ;
    return SEG->cur = n;
}
