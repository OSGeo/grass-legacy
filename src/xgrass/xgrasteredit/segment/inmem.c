/* %W% %G% */

#include <stdio.h>
#include "segment.h"

/* JNK 07/17/91 -- new function made by extracting first part of pagein.c
                which finds and/or frees a RAM segment slot, given a file
                segment number.  New functionality: finds a free slot if
                input slot number is (-1). Select() function is now external
                instead of local to pagein.c.
    RETURNS: if successful, the RAM segment slot number of the slot containing
                the requested file segment if it is in RAM (indicated by the
                slot's segment number "SEG->scb[SEG->cur].n" being the one
                requested) -- or of an available slot if the requested one
                is not in RAM (in which case the slot's segment number will be
                returned unequal to the requested number).  Also, the returned
                slot is made the current one (SEG->cur is set to the returned
                slot number);
             else SEG->nseg (out-of-range-too-large value) if unsucessful.     */

segment_inmem (SEG,n)
    SEGMENT *SEG;
{
    int age;
    int cur;
    int i;

/* is n the current segment? */
    if (n == SEG->scb[SEG->cur].n)
	return SEG->cur;

/* search the in memory segments */
    for (i = 0; i < SEG->nseg; i++)
	if (n == SEG->scb[i].n)
	    return segment_select (SEG,i);/* uses external segment_select() */

/* find a slot to use to hold segment */
    age = 0;
    cur = 0;
    for (i = 0; i < SEG->nseg; i++)
	if (SEG->scb[i].n < 0)	/* free slot */
	{
	    cur = i;
	    break;
	}
	else if (age < SEG->scb[i].age)	/* find oldest segment */
	{
	    cur = i;
	    age = SEG->scb[i].age;
	}

/* if slot is used, write it out, if dirty */
    if (SEG->scb[cur].n >= 0 && SEG->scb[cur].dirty)
	if(segment_pageout (SEG, cur) < 0)
	    return SEG->nseg;/* ERROR when return value >= SEG->nseg */

    return segment_select (SEG,cur);/* uses external segment_select() */
}
