#include "gis.h"
#define MEDIAN 1
#define AVERAGE 2
#define RAW 0

static int type = RAW;

static
struct point
{
    unsigned short row, col;
    CELL value;
} *points = NULL;

static int nalloc = 0;
static int npoints = 0;

static int width = 1;
static int nrows, ncols;

init_profiles (n, t)
    char *t;
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
}

graph_point(col,row)
{
    int x,y;

    for (y = -width; y <= width; y++)
	for (x = -width; x <= width; x++)
	    add_point (col+x,row+y);
}

add_point(col,row)
{
    int n;

    if (col < 0 || row < 0) return;
    if (col >= ncols || row >= nrows) return;

    n = npoints++;
    if (npoints > nalloc)
	points = (struct point *) G_realloc (points, (nalloc += 64)*sizeof(struct point));

    points[n].col = col;
    points[n].row = row;
}

cmp_row_col (a, b)
    struct point *a, *b;
{
    int n;

    if (n = (a->row - b->row))
	return n;
    return (a->col - b->col);
}

cmp_value (a, b)
    struct point *a, *b;
{
    return (a->value - b->value);
}

profile(fd,cell,col1,row1,col2,row2)
    CELL *cell;
{
    int i,j;
    double x;

    npoints = 0;
    G_bresenham_line(col1,row1,col2,row2,graph_point);
    if (npoints == 0)
    {
	printf ("0\n");
	return;
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
    printf ("%d", npoints);
    switch(type)
    {
    case RAW:
	for (i=0;i<npoints;i++)
	    printf (" %ld", (long) points[i].value);
	break;
    case MEDIAN:
	qsort (points, npoints, sizeof(*points), cmp_value);
	x = points[(npoints-1)/2].value + points[npoints/2].value;
	printf (" %lf", x/2.0);
	break;
    case AVERAGE:
	x = 0.0;
	for (i=0;i<npoints;i++)
	    x += points[i].value;
	printf (" %lf", x/npoints);
	break;
    }
    printf ("\n");
}
