#include <stdlib.h>
#include "gis.h"
#include "local_proto.h"

#define MEDIAN 1
#define AVERAGE 2
#define RAW 0

static int type = RAW;

static struct point
{
    unsigned short row, col;
    CELL value;
} *points = NULL;
static int cmp_row_col(struct point *, struct point *);
static int cmp_value(struct point *, struct point *);

static int nalloc = 0;
static int npoints = 0;

static int width = 1;
static int nrows, ncols;

int init_profiles (int n, char *t)
{
    nrows = G_window_rows();
    ncols = G_window_cols();
    width = n/2;
    if (strcmp(t,"raw") == 0)
	type = RAW;
    else if (strcmp(t,"median") == 0)
	type = MEDIAN;
    else if (strcmp(t,"average") == 0)
	type = AVERAGE;

    return 0;
}

int graph_point (int col, int row)
{
    int x,y;

    for (y = -width; y <= width; y++)
	for (x = -width; x <= width; x++)
	    add_point (col+x,row+y);

    return 0;
}

int add_point (int col, int row)
{
    int n;

    if (col < 0 || row < 0) return 0;
    if (col >= ncols || row >= nrows) return 0;

    n = npoints++;
    if (npoints > nalloc)
	points = (struct point *) G_realloc (points, (nalloc += 64)*sizeof(struct point));

    points[n].col = col;
    points[n].row = row;

    return 0;
}

static int cmp_row_col (struct point *a, struct point *b)
{
    int n;

    if (n = (a->row - b->row))
	return n;
    return (a->col - b->col);
}

static int cmp_value (struct point *a, struct point *b)
{
    return (a->value - b->value);
}

int profile (int fd, CELL *cell, int col1, int row1, int col2, int row2)
{
    int i,j;
    double x;

    npoints = 0;
    G_bresenham_line(col1,row1,col2,row2,graph_point);
    if (npoints == 0)
    {
	fprintf (stdout,"0\n");
	return 0;
    }

    qsort (points, npoints, sizeof(*points), cmp_row_col);
/* remove duplicates */
    i = 0;
    for (j = 1; j < npoints; j++)
    {
	if (points[i].row != points[j].row
	||  points[i].col != points[j].col)
	{
	    i++;
	    if (i != j)
	    {
		points[i].row = points[j].row;
		points[i].col = points[j].col;
	    }
	}
    }
    npoints = i+1;

/* read the cell file data */
    j = -1;
    for (i=0;i<npoints;i++)
    {
	if (j != points[i].row)
	{
	    if (G_get_map_row (fd, cell, j = points[i].row) < 0)
		exit(1);
	}
	points[i].value = cell[points[i].col];
    }

/* print results */
    fprintf (stdout,"%d", npoints);
    switch(type)
    {
    case RAW:
	for (i=0;i<npoints;i++)
	    fprintf (stdout," %ld", (long) points[i].value);
	break;
    case MEDIAN:
	qsort (points, npoints, sizeof(*points), cmp_value);
	x = points[(npoints-1)/2].value + points[npoints/2].value;
	fprintf (stdout," %f", x/2.0);
	break;
    case AVERAGE:
	x = 0.0;
	for (i=0;i<npoints;i++)
	    x += points[i].value;
	fprintf (stdout," %f", x/npoints);
	break;
    }
    fprintf (stdout,"\n");

    return 0;
}
