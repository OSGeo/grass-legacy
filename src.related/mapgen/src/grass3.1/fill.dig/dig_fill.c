
/* Functions: dig, edge, edge_point, edge_order
 *
 * These functions are direct adaptations of the functions modified by
 * P. Carlson, SCS.
 * M.L.H
 * These functions are adapted from polyhash.c (mass1).  The edge function 
 * was modified to correct a bug that gave an odd number of edge points in 
 * rare circumstances.
 *
 * P.W.C.       5/88
 */
#include <stdio.h>
#include <math.h>
#include "pattern.h"

#define x_rotate(x,y)           (x) * rot_cos - (y) * rot_sin
#define y_rotate(x,y)           (y) * rot_cos + (x) * rot_sin
#define x_rotate_rev(x,y)       (x) * rot_cos + (y) * rot_sin
#define y_rotate_rev(x,y)       (y) * rot_cos - (x) * rot_sin
#define POINT   struct point

POINT
{       double x;
        int y;
};
static POINT *P;        /* edge table element */
static int np;          /* number of edge table elements */
static int npmax;       /* number of edge table elements allocated */
static double MARGIN;

/* Function: poly_fill
 *
 * This function fills a polygon with a pattern.
 */
dig_fill(n, x, y, pattern, islands, ix, iy, in, scale)
int n;                          /* number of points in x and y arrays */
double *x, *y;                  /* arrays of polygon vertex coordinates */
struct pattern *pattern;        /* structure containing pattern info */
int islands;                    /* number of islands */
double *ix, *iy;                /* arrays of islands vertex coordinates */
int in[];                       /* array of number of points per island */
double scale;			/* scale of the map being filled */
{
        double angle;
        int space, ni, i;
        int width, edge_order();
        double x0, x1, xN;
        double y0, y1, yN;
        double rot_sin, rot_cos, radians, sin(), cos();
	double slp, intsp;

        if (n < 3) return(0);
/*DEBUG fprintf(stderr,"\n%d %lf %lf %lf %lf\n",n,*x,*y,pattern->spacing,pattern->angle);*/
/*DEBUG fprintf(stderr,"\n%d %lf %lf %d\n",islands,*ix,*iy,in[0]);*/

        /* Single line width */
        width = 1;


	/* set MARGIN in polygon */
	MARGIN = .02 * scale/100;

        /* Get spacing between lines */
        space = (int)pattern->spacing;

        /* Get and adjust line angle */
        angle = pattern->angle;
        if (angle > 90.0) angle -= 180.0;
        radians = 0.017453292 * angle;

        /* Compute line angle functions */
        rot_sin = sin(radians);
        rot_cos = cos(radians);

        
        /* traverse the perimeter */
        np = 0;
        P = (POINT *)calloc(npmax = 32, sizeof(POINT));
        xN = x0 = x_rotate_rev(*x, *y);
        yN = y0 = y_rotate_rev(*x, *y);
        x++;
        y++;
        while (--n)
        {       x1 = x_rotate_rev(*x, *y);
                y1 = y_rotate_rev(*x, *y);
                x++;
                y++;
                edge(x0, y0, x1, y1, width, space);
                x0 = x1;
                y0 = y1;
        }
        edge(x0, y0, xN, yN, width, space);
        if (np % 2) edge(xN-1,yN-1,xN,yN,0,1);


        /* Add island edges */
        for (i = 0; i < islands; i++)
        {       xN = x0 = x_rotate_rev(*ix, *iy);
                yN = y0 = y_rotate_rev(*ix, *iy);
                ix++;
                iy++;
                ni = in[i];
                while (--ni)
                {       
                        /* Check for another island */
                        if (*ix == 0.0 && *iy == 0.0) 
                        {       ix++;
                                iy++;
                                ni--;
                                break;
                        }
                        x1 = x_rotate_rev(*ix, *iy);
                        y1 = y_rotate_rev(*ix, *iy);
                        ix++;
                        iy++;
                        edge(x0, y0, x1, y1, width, space);
                        x0 = x1;
                        y0 = y1;
                }
                edge(x0, y0, xN, yN, width, space);
                ni--;
        }
        if (np % 2) edge(xN-1,yN-1,xN,yN,0,1);


        /* Every pattern line should have two endpoints */
        /* Ignore and warn the user that the pattern may look bad*/
        /*if (np % 2)
        {       if (P) free(P);
                return(-99);
        }*/

        /* Sort the edge points by y and then by x */
        qsort((char *)P, np, sizeof(*P), edge_order);

        /* Plot the lines */
        printf("#-p %d\n",pattern->color);
        for (n = 1; n < np; n++)
        {
                int x0, y0, x1, y1, ii, j;
		double tmp;

                if (P[n].y != P[n-1].y)
                {       fprintf(stderr,"\npoly_fill: Warning row out of sync.\n");
/*			for (j=-4;j<4;j++)
				fprintf(stderr,"%lf %d\n", P[n+j].x, P[n+j].y);
			if (n > 1 && n < np - 1) {
				P[n-1].y = P[n].y; P[n-1].x = P[n+2].x + P[n].x - P[n+1].x;

			if (n > 3 && P[n-4].y == P[n-1].y) {
				P[n-2].y = P[n-1].y; tmp = P[n-1].x;
				P[n-1].x = P[n-2].x -( P[n-4].x - P[n-2].x);
				P[n-2].x = tmp;
				n--;
				} 
			else  
				continue;
			for (j=-4;j<4;j++)
				fprintf(stderr,"%lf %d\n", P[n+j].x, P[n+j].y);
*/
printf("ERROR HERE ############\n");
continue;
		}


                x0 = x_rotate(P[n-1].x, P[n-1].y);
                y0 = y_rotate(P[n-1].x, P[n-1].y);
                x1 = x_rotate(P[n].x, P[n].y);
                y1 = y_rotate(P[n].x, P[n].y);
		
/*		if (x0 == x1){
                        if (y0 > y1) {y0 -= MARGIN; y1 += MARGIN;}
			else {y1 -= MARGIN; y0 += MARGIN;}
			}

		else if (y0 == y1){
                        if (x0 > x1) {x0 -= MARGIN; x1 += MARGIN;}
			else {x1 -= MARGIN; x0 += MARGIN;}
			}

		else {
			slp = (y1-y0)/(x1-x0);
			intsp = y0 - (slp * x0);
			if (x0 > x1) {x0 -= MARGIN; x1 += MARGIN;}
			else {x1 -= MARGIN; x0 += MARGIN;}
			y0 = (int) slp * x0 + intsp;
			y1 = (int) slp * x1 + intsp;
			}
*/
			x0 = x0 + (MARGIN * rot_cos);
			y0 = y0 + (MARGIN * rot_sin);
			x1 = x1 - (MARGIN * rot_cos);
			y1 = y1 - (MARGIN * rot_sin);

		if (ii) {
                        printf("%ld     %ld\n",x0, y0);
                        printf("%ld     %ld\n",x1, y1);
                        printf("#-b\n");
			ii--;
			}

		else {
                        printf("%ld     %ld\n",x1, y1);
                        printf("%ld     %ld\n",x0, y0);
                        printf("#-b\n");
			ii++;
			}


                
                n++;
        }
        free(P);
        return(0);
}
                

/* Function: edge
 *
 * This function finds the intersection coordinates of the pattern lines
 * and an edge line.
 */
static edge( x0, y0, x1, y1, width, space)
double x0, y0, x1, y1;
int width, space;
{
        double slope, x;
        int y, w, total;

        if (y0 == y1) return;
        slope = (x0 - x1) / (y0 - y1);
        if (y0 < y1)
        {       y = (int)y0 + 1;
                x = x0 + slope * ((double)y - y0);
        }
        else
        {       y = (int)y1 + 1;
                x = x1 + slope * ((double)y - y1);
                y1 = y0;
        }
        total = width + space;
        if (total == 1) w = 0;
        else if (y < 0) w = total -(-y % total);
        else w = y % total;
        for ( ; y <= (int)y1; y++, x += slope, w++)
        {       if (w >= width)
                {       if (w < total) continue;
                        w = 0;
                }
                edge_point(x, y);
        }
}


/* Function: edge_point
 *
 * This function adds the intersection point to the edge table, allocating
 * space in the edge table as necessary.
 */
static edge_point(x, y)
double x;
int y;
{
        char *realloc();
        
        if (np >= npmax) P = (POINT *)realloc(P, (npmax += 32) * sizeof(POINT));
        P[np].x = x;
        P[np++].y = y;
}


/* Function: edge_order
 * 
 * This function does the comparison of edge table elements as required
 * by the qsort function.
 */
static edge_order(a, b)
struct point *a, *b;
{
        if (a->y < b->y) return(-1);
        if (a->y > b->y) return(1);
        if (a->x < b->x) return(-1);
        if (a->x > b->x) return(1);
        return(0);
}
