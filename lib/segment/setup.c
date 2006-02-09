#include <stdio.h>
#include <grass/segment.h>

/* SEG must have the following parms set
 *  fd (open for read and write)
 *  nrows, ncols, srows, scols, len
 *  nseg
 */
int segment_setup (SEGMENT *SEG)
{
    int i;
    char *malloc();

    SEG->open = 0;

    if (SEG->nrows <= 0 || SEG->ncols <= 0
    ||  SEG->srows <= 0 || SEG->scols <= 0
    ||  SEG->len   <= 0 || SEG->nseg  <= 0)
    {
	G_warning ("segment_setup: illegal segment file parameters\n");
	return -1;
    }

    SEG->offset = lseek (SEG->fd, 0L, 1);

    SEG->spr = SEG->ncols / SEG->scols ;
    SEG->spill = SEG->ncols % SEG->scols ;
    if(SEG->spill)
	SEG->spr++ ;

    if((SEG->scb =
        (struct SEGMENT_SCB *) G_malloc (SEG->nseg * sizeof(struct SEGMENT_SCB))) == NULL)
	return -2;

    SEG->size = SEG->srows * SEG->scols * SEG->len;

    for (i = 0; i < SEG->nseg; i++)
    {
	if((SEG->scb[i].buf = G_malloc (SEG->size)) == NULL)
	    return -2;
	SEG->scb[i].n = -1;	/* mark free */
	SEG->scb[i].dirty = 0;
	SEG->scb[i].age = 0;
    }
    SEG->cur = 0;
    SEG->open = 1;

    return 1;
}
