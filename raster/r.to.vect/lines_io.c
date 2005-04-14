/* Cell-file line extraction */
/*   Input/output and line tracing routines */

/* Mike Baba */
/* DBA Systems*/
/* Farfax, VA */
/* Jan 1990 */

/* Jean Ezell */
/* US Army Corps of Engineers */
/* Construction Engineering Research Lab */
/* Modelling and Simulation Team */
/* Champaign, IL  61820 */
/* March 1988 */

/* input is a GRASS cell file */
/* output is a binary or ascii digit file */

/*
 * Modified for the new Grass 5.0 floating point and
 * null values raster file format.
 * Pierre de Mouveaux - 20 april 2000.
 */

#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "global.h"
#include "lines.h"

static int write_ln(struct COOR *, struct COOR *, int);

/* write_line - attempt to write a line to output */
/* just returns if line is not completed yet */

int write_line(struct COOR *seed)
{
    struct COOR *point, *begin, *end, *find_end(), *move();
    int dir, line_type, n, n1;

    point = seed;
    if ( (dir = at_end(point)) ) {	/* already have one end of line */
	begin = point;
	end = find_end(point, dir, &line_type, &n);
	if (line_type == OPEN) {
	    return (-1);	/* unfinished line */
	}
	direction = dir;
    }
    else {			/* in middle of a line */

	end = find_end(point, FORWARD, &line_type, &n);
	if (line_type == OPEN) {	/* line not finished */
	    return (-1);
	}
	if (line_type == END) {	/* found one end at least *//* look for other one */
	    begin = find_end(point, BACKWARD, &line_type, &n1);
	    if (line_type == OPEN) {	/* line not finished */
		return (-1);
	    }
	    if (line_type == LOOP) {	/* this should NEVER be the case */
		fprintf(stderr, "write_line:  found half a loop!\n");
		return (-1);
	    }
	    direction = at_end(begin);	/* found both ends now; total length */
	    n += n1;		/*   is sum of distances to each end */
	}
	else {			/* line_type = LOOP by default */
	    /* already have correct length */
	    begin = end;	/* end and beginning are the same */
	    direction = FORWARD;	/* direction is arbitrary */
	}
    }
    /* if (n > 2) */
    write_ln(begin, end, n);
    return (0);
}

/* write_ln - actual writing part of write_line */
/* writes binary and supplemental file */

static int write_ln(struct COOR *begin, struct COOR *end,	/* start and end point of line */
		    int n)
{				/* number of points to write */
    double x, y;
    struct COOR *p, *last;
    int i;

    ++n;

    p = begin;
    y = cell_head.north - ((double) p->row + 0.5) * cell_head.ns_res;
    x = cell_head.west + ((double) p->col + 0.5) * cell_head.ew_res;


/****************************************************************
 * shapiro 27 feb 1992.
 * bug fixed by:  by Jinn-Guey Lay: jinn@uhunix.uhcc.Hawaii.edu
 ***************************************************************/
    Vect_reset_line(Points);
/***************************************************************/

    Vect_append_point(Points, x, y, 0.0);

    for (i = 1; i < n; i++) {
	last = p;
	if ((p = move(p)) == NULL) {	/* this should NEVER happen */
	    fprintf(stderr, "write_line:  line terminated unexpectedly\n");
	    fprintf(stderr, "  previous (%d) point %p (%d,%d,%d) %p %p\n",
		    direction, last, last->row, last->col, last->node,
		    last->fptr, last->bptr);
	    exit(-1);
	}
	y = cell_head.north - ((double) p->row + 0.5) * cell_head.ns_res;
	x = cell_head.west + ((double) p->col + 0.5) * cell_head.ew_res;

	Vect_append_point(Points, x, y, 0.0);
    }


    /* now free all the pointers */
    p = begin;

    for (i = 1; i < n; i++) {
	last = p;
	if ((p = move(p)) == NULL)
	    break;
	if (last == p)
	    break;
	if (last->fptr != NULL)
	    if (last->fptr->fptr == last)
		last->fptr->fptr = NULL;
	/* now it can already ne NULL */
	if (last->fptr != NULL)
	    if (last->fptr->bptr == last)
		last->fptr->bptr = NULL;
	if (last->bptr != NULL)
	    if (last->bptr->fptr == last)
		last->bptr->fptr = NULL;
	if (last->bptr != NULL)
	    if (last->bptr->bptr == last)
		last->bptr->bptr = NULL;
	G_free(last);
    }				/* end of for i */
    if (p != NULL)
	G_free(p);

    Vect_write_line(&Map, GV_LINE, Points, Cats);

    return 0;
}
