/* %W% %G% */

#include "segment.h"

segment_address (SEG, row, col, n, index)
    SEGMENT *SEG;
    int *n;
    int *index;
{
    *n     = row/SEG->srows * SEG->spr   + col/SEG->scols ;
    *index = (row%SEG->srows * SEG->scols + col%SEG->scols) * SEG->len;
}
