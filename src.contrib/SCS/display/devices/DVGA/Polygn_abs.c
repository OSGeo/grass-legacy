/* Function: Polygon_abs	P.W. Carlson		12/88	*/

#include <stdio.h>
#include "vio_driver.h"

struct point {				/* edge table element */
    int x;
    int y;
};

static struct point *edge_table;	/* pointer to edge table */
static int np;				/* no. of points in edge table */
static int npmax;			/* max no. of points allocated */

Polygon_abs(x, y, n)
int *x, *y, n;
{
    register int i;
    register unsigned char *ptr;
    int edge_order();
    char *calloc();
    int x0, x1, xN, y0, y1, yN;

    /* set the current coordinates */
    cur_x = x[n-1];
    cur_y = y[n-1];

    /* need at least three vertices */
    if (n < 3) return 0;

    /* fill the raster buffer with current color */
    ptr = raster_buff;
    for (i = 0; i < H_RES; i++) *ptr++ = cur_color;

    /* initialize no. of points in edge table and make inital
       memory allocation for edge table */
    np = 0;
    edge_table = (struct point *)calloc(npmax = 32, sizeof(struct point));

    /* loop thru edges, building the edge table */
    xN = x0 = *x++;
    yN = y0 = *y++;
    while (--n)
    {	x1 = *x++;
        y1 = *y++;
        edge(x0, y0, x1, y1);
        x0 = x1;
        y0 = y1;
    }
    edge(x0, y0, xN, yN);

    /* check if perimeter has odd number of points */
    if (np % 2)
    {	if (edge_table) free(edge_table);
        return -99;
    }

    /* sort the edge table points by y and then by x */
    qsort(edge_table, np, sizeof(struct point), edge_order);

    /* loop thru pairs of endpoints */
    for (n = 1; n < np; n++)
    {	
	/* endpoints must have equal y values */
	if (edge_table[n].y != edge_table[n-1].y)
        {   fprintf(stderr,"Polygon_abs: row %d out of sync\n", 
		edge_table[n-1].y);
            continue;
        }

   	/* pass coords. and color to device driver to draw horiz. line */
   	args.arg1 = edge_table[n-1].x;
	args.arg2 = edge_table[n].x;
	args.arg3 = edge_table[n].y;
	args.ptr  = raster_buff;
	ioctl(viofd, VIO_HORLINE, &args);
        n++;
    }

    /* free edge table memory */
    free (edge_table);
}

static edge(x0, y0, x1, y1)
register int x0, y0 ;
int x1, y1 ;
{
    register float m;
    register float x;

    /* skip horizontal line segments */
    if (y0 == y1) return;

    /* compute slope */
    m = (float) (x0 - x1) / (float) (y0 - y1) ;

    /* if negative slope */
    if (y0 < y1)
    {	x = x0;

	/* compute x for each y and put x,y pair in edge table */
        while (++y0 <= y1)
        {   x0 = (x += m) + .5;
            edge_point(x0, y0);
        }
    }
    else

    /* if negative slope */
    {	x = x1;

	/* compute x for each y and put x,y pair in edge table */
        while (++y1 <= y0)
        {   x1 = (x += m) + .5;
            edge_point(x1, y1);
        }
    }
}


static edge_point(x, y)
register int x, y;
{
    char *realloc();

    /* allocate more memory for edge table, if necessary */
    if (np >= npmax) 
	edge_table = (struct point *)realloc(edge_table, 
	    (npmax += 32) * sizeof(struct point));

    /* put coordinates in edge table */
    edge_table[np].x = x;
    edge_table[np++].y = y;
}

static edge_order(a, b)
struct point *a, *b;
{
    /* sort first by y */
    if (a->y < b->y) return (-1);
    if (a->y > b->y) return (1);

    /* then sort by x */
    if (a->x < b->x) return (-1);
    if (a->x > b->x) return (1);
    return (0);
}
