/* %W% %G% */

#include <stdio.h>
#include "segment.h"

/* SEG must have the following parms set
 *  fd (open for read and write)
 *  nrows, ncols, srows, scols, len
 *  nseg
 */
segment_setup (SEG)
    SEGMENT *SEG;
{
    int i;
    char *malloc();

    SEG->open = 0;

    if (SEG->nrows <= 0 || SEG->ncols <= 0
    ||  SEG->srows <= 0 || SEG->scols <= 0
    ||  SEG->len   <= 0 || SEG->nseg  <= 0)
    {
	fprintf (stderr, "illegal segment file parameters\n");
	return -1;
    }

    SEG->offset = lseek (SEG->fd, 0L, 1);

    SEG->spr = SEG->ncols / SEG->scols ;
    SEG->spill = SEG->ncols % SEG->scols ;
    if(SEG->spill)
	SEG->spr++ ;

    SEG->scb = (struct SEGMENT_SCB *) malloc (SEG->nseg * sizeof(struct SEGMENT_SCB));
    if (SEG->scb == NULL)
    {
	fprintf (stderr, "Out of Memory\n");
	return -2;
    }

    SEG->size = SEG->srows * SEG->scols * SEG->len;

    for (i = 0; i < SEG->nseg; i++)
    {
	SEG->scb[i].buf = malloc (SEG->size) ;
	if (SEG->scb[i].buf == NULL)
	{
	    fprintf (stderr, "Out of Memory\n");
	    return -2;
	}
	SEG->scb[i].n = -1;	/* mark free */
	SEG->scb[i].dirty = 0;
    }
    SEG->cur = 0;
    SEG->open = 1;

    return 1;
}
