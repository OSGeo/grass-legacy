#include <grass/segment.h>

int segment_address (
    SEGMENT *SEG,int row,int col,
    int *n,
    int *index)
{
    *n     = row/SEG->srows * SEG->spr   + col/SEG->scols ;
    *index = (row%SEG->srows * SEG->scols + col%SEG->scols) * SEG->len;

	return 0;
}
