#include <stdlib.h>
#include "segment.h"

int segment_release ( SEGMENT *SEG)
{
    int i;

    if (SEG->open != 1)
	return -1;

    for (i = 0; i < SEG->nseg; i++)
	free(SEG->scb[i].buf);
    free(SEG->scb);

    SEG->open = 0;

    return 1;
}
