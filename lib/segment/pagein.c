#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <grass/segment.h>

static int segment_select(SEGMENT *,int);

int segment_pagein ( SEGMENT *SEG,int n)
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
	    return segment_select (SEG,i);

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
	    return -1;

/* read in the segment */
    SEG->scb[cur].n = n;
    SEG->scb[cur].dirty = 0;
    segment_seek (SEG, SEG->scb[cur].n, 0);
    if (read (SEG->fd, SEG->scb[cur].buf, SEG->size) != SEG->size)
    {
	G_warning ("segment_pagein: %s\n", strerror(errno));
	return -1;
    }

    return segment_select (SEG,cur);
}

static int segment_select(SEGMENT *SEG,int n)
{
    int i;

    SEG->scb[n].age = 0;
    for (i = 0; i < SEG->nseg; i++)
	SEG->scb[i].age++ ;
    return SEG->cur = n;
}
