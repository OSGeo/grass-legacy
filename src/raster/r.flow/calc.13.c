/*
**  Original Algorithm:    H. Mitasova, L. Mitas, J. Hofierka, M. Zlocha 
**  New GRASS Implementation:  J. Caplan, M. Ruesink  1995
**
**  US Army Construction Engineering Research Lab, University of Illinois 
**
**  Copyright  J. Caplan, H. Mitasova, L. Mitas, M.Ruesink, J. Hofierka, 
**	M. Zlocha  1995
**
**This program is free software; you can redistribute it and/or
**modify it under the terms of the GNU General Public License
**as published by the Free Software Foundation; either version 2
**of the License, or (at your option) any later version.
**
**This program is distributed in the hope that it will be useful,
**but WITHOUT ANY WARRANTY; without even the implied warranty of
**MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**GNU General Public License for more details.
**
**You should have received a copy of the GNU General Public License
**along with this program; if not, write to the Free Software
**Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
**
**  version 13 for GRASS5.0
**  FP related bugs in slope length output fixed by Helena oct. 1999)
**  Update MN: commented line 387
*/


#include "r.flow.13.h"
#include "mem.13.h"
#include "io.13.h"
#include "aspect.13.h"
#include "precomp.13.h"
#include <stdlib.h>      /* for the random number generation */
#include <time.h>

#define HORIZ	1		/* \		*/
#define VERT	0		/* |		*/
#define EAST	1		/* |		*/
#define WEST	0		/* |_ magic	*/
#define NORTH	1		/* |  numbers	*/
#define SOUTH	0		/* |		*/
#define ROW	1		/* |		*/
#define COL	0		/* /		*/

#define DY(dx, angle)   ((dx) / tang[angle])
#define DX(dy, angle)   ((dy) * tang[angle])

typedef struct
{
    int     row, col;		/* current matrix address	*/
}
addr;

typedef int bbox[2][2];		/* current bounding box		*/

typedef struct
{
    double  x, y, z;		/* exact earth coordinates	*/
    int theta;			/* aspect			*/
    double  r, c;		/* cell matrix coordinates	*/
}
point;

typedef struct
{
    double *px, *py;		/* x/y point arrays		*/
    int     index;		/* index of next point		*/
}
flowline;

/***************************** CALCULATION ******************************/

/*
 * height_angle_bounding_box: averages matrix values at sub between
 *	floor(cut) and ceil(cut), based on proximity; adjusts bbox
 * globals r: o, z, parm
 * params  w: p, b
 */
void
height_angle_bounding_box(sub, cut, horiz, p, b)
    int		 sub, horiz;
    double	 cut;
    point	*p;
    bbox	 b;
{
    int      c, f = (int) cut;
    double   r = cut - (double) f;
    double   a1, a2, a, d;

    b[horiz][horiz] = sub - 1;
    b[horiz][!horiz] = sub + 1;
    b[!horiz][horiz] = f + 1;
    c = (b[!horiz][!horiz] = f - (r == 0.)) + 1;

    if (horiz)
    {
	a1   = (double) aspect(sub, f);
	a2   = (double) aspect(sub, c);
	p->z = (double) get(el, sub, f) * (1. - r) + 
	       (double) get(el, sub, c) * r;
    }
    else
    {
	a1   = (double) aspect(f, sub);
	a2   = (double) aspect(c, sub);
	p->z = (double) get(el, f, sub) * (1. - r) +
	       (double) get(el, c, sub) * r;
    }

    if (!(a1 == UNDEF || a2 == UNDEF) &&
        !(G_is_d_null_value(&a1) || G_is_d_null_value(&a2)))
/*    if (!(G_is_d_null_value(&a1) || G_is_d_null_value(&a2)))*/
    {
	if ((d = a1 - a2) >= D_PI || d <= -D_PI)
	    if (a2 > D_PI)
		a2 -= D2_PI;
	    else
		a1 -= D2_PI;
	a = r * a2 + (1. - r) * a1;
	p->theta = ROUND(a + (a < 0.) * D2_PI);
    }
    else
	p->theta = UNDEF;

    return;
}

/*
 * sloping: returns elevation condition for continuing current line
 */
#define sloping(z1, z2) (z1 > z2)

/*
 * on_map: returns map boundary condition for continuing current line
 * globals r:  region
 */
int on_map(int sub,double cut,int horiz)
{
    return
	(sub >= 0 && cut >= 0.0 &&
	 ((horiz && sub < region.rows && cut <= (double) (region.cols - 1)) ||
	  (!horiz && sub < region.cols && cut <= (double) (region.rows - 1))));
}

/*
 * add_to_line: puts a new point on the end of the current flowline
 * globals r:  parm
 * params  w:  f
 */
void add_to_line(point	*p, flowline *f)
{
    if (parm.flout)
    {
	f->px[f->index] = (double) p->x;
	f->py[f->index] = (double) p->y;
    }
    ++f->index;
}

/*
 * rectify: correct quantization problems (designed for speed, not elegance)
 */
double rectify(double delta,double bd[2],double e)
{
    if (delta > 0.)
    {
	if (delta > bd[1] + e)
	    return delta;
    }
    else
    {
	if (delta < bd[0] - e)
	    return delta;
    }
    if (delta < bd[1] - e)
	if (delta > bd[0] + e)
	    return delta;
	else
	    return bd[0];
    else
	return bd[1];
}

/*
 * next_point: computes next point based on current point, z, and o
 *	returns continuation condition
 * globals r:  region, bitbar, parm
 * params  w:  p, a, l
 * globals w:  density
 */
int next_point(
    point	*p, /* current/next point		*/
    addr	*a, /* current/next matrix address	*/
    bbox	 b, /* current/next bounding box	*/
    double	*l  /* current/eventual length		*/
)
{
    int      sub;
    double   cut;
    int      horiz;
    int      semi;
    double   length, delta;

    double   oldz	= p->z;
    int	     oldtheta	= p->theta;
    double   oldr	= p->r;
    double   oldc	= p->c;

    addr     ads;
    double   bdy[2], bdx[2];

    ads = *a;
    bdy[SOUTH]	= (double) (oldr - b[ROW][SOUTH]) * region.ns_res;
    bdy[NORTH]	= (double) (oldr - b[ROW][NORTH]) * region.ns_res;
    bdx[WEST]	= (double) (b[COL][WEST] - oldc) * ew_dist[ads.row];
    bdx[EAST]	= (double) (b[COL][EAST] - oldc) * ew_dist[ads.row];

    semi = oldtheta < 90 || oldtheta >= 270;
    if (oldtheta != 90 && oldtheta != 270 &&          /* north/south */
	(delta = DX(bdy[semi],oldtheta)) < bdx[EAST] &&
	delta > bdx[WEST])
    {
	delta	 = rectify(delta, bdx, epsilon[HORIZ][ads.row]);
	p->x	+= delta;
	p->y	+= bdy[semi];
	p->r	 = (double) b[ROW][semi];
	p->c	+= delta / ew_dist[ads.row];
	a->row	 = b[ROW][semi];
	a->col	 = ROUND(p->c);
	sub	 = b[ROW][semi];
	cut	 = p->c;
	horiz	 = HORIZ;
	if (parm.lgout) 
	    length = hypot(delta, bdy[semi]);
    }
    else                                                /*  east/west  */
    {
	semi = oldtheta < 180;
	if (oldtheta == 90 || oldtheta == 270)
	    delta = 0;
	else
	{
	    /* I don't know if this is right case.
	     * Anyway, DY() should be avoid from dividing by zero.
	     * Any hydrologic idea?
	     */
	    if(tang[oldtheta] == 0.0)
	        tang[oldtheta] = 0.000001;

	    delta = DY(bdx[semi], oldtheta);
	}

	delta	 = rectify(delta, bdy, epsilon[VERT][ads.row]);
	p->y	+= delta;
	p->x	+= bdx[semi];
	p->r	-= delta / region.ns_res;
	p->c	 = (double) b[COL][semi];
	a->row	 = ROUND(p->r);
	a->col	 = b[COL][semi];
	sub	 = b[COL][semi];
	cut	 = p->r;
	horiz	 = VERT;
	if (parm.lgout)
	    length = hypot(bdx[semi], delta);
    }

    if (on_map(sub, cut, horiz) &&
	(height_angle_bounding_box(sub, cut, horiz, p, b),
	 sloping(oldz, p->z)) &&
	!(parm.barin && BM_get(bitbar, a->col, a->row)))
    {
	if (parm.dsout && (ads.row != a->row || ads.col != a->col))
	    put(ds, a->row, a->col, get(ds, a->row, a->col) + 1);
	if (parm.lgout)
	    *l += parm.l3d ? hypot(length, oldz - p->z) : length;
	return 1;
    }

    return 0;
}

/*
 * calculate: create a flowline for each cell
 * globals r: region, bitbar, parm, lgfd
 */
void calculate()
{
    point	 pts;
    addr	 ads;
    bbox	 bbs;
    flowline	 fls;
    int		 row, col;
    double	 x, y, length, xstep, ystep, roffset, coffset;
    FCELL	*lg = G_allocate_f_raster_buf();
    struct	 line_pnts *points = Vect_new_line_struct();
    int		 loopstep = (!parm.dsout && !parm.lgout && parm.flout) ?
			    parm.skip : 1;

    diag("Working...");

    fls.px = (double *) G_calloc(parm.bound, sizeof (double));
    fls.py = (double *) G_calloc(parm.bound, sizeof (double));

    ystep = region.ns_res * (double) loopstep;

    srand( time(0));

    for (row = 0, y = (double) region.north - (region.ns_res * .5);
	 row < region.rows;
	 row += loopstep, y -= ystep)
    {
	xstep = ew_dist[row] * (double) loopstep;

	for (col = 0, x = (double) region.west + (ew_dist[row] * .5);
	     col < region.cols;
	     col += loopstep, x += xstep)
	{
	    G_percent(row, region.rows, 1);

	    length		= 0.0;
	    fls.index		= 0;

	    if (!(parm.barin && BM_get(bitbar, col, row)))
	    {
	        roffset         = parm.offset * (double) region.ew_res \
		  * ((2. * (double) rand()/ (double) RAND_MAX) - 1.); 
	        coffset         = parm.offset * (double) region.ns_res \
		  * ((2. * (double) rand()/ (double) RAND_MAX) - 1.); 

		pts.x		= x;
		pts.y		= y;
		pts.z		= (double) get(el, row, col);
		pts.theta	= (double) aspect(row, col);
		pts.r		= (double) row + roffset;
		pts.c		= (double) col + coffset;

		ads.row		= row;
		ads.col 	= col;

	/*	fprintf(stderr, "dx: %f  x: %f %f  row: %f %f\n",\
			roffset, x, pts.x, (double) row, pts.r);
		fprintf(stderr, "dy: %f  y: %f %f  col: %f %f\n",\
			roffset, y, pts.y, (double) col, pts.c);   */

		bbs[ROW][SOUTH]	= row + 1;
		bbs[ROW][NORTH]	= row - 1;
		bbs[COL][WEST]	= col - 1;
		bbs[COL][EAST]	= col + 1;

		do
		    add_to_line(&pts, &fls);
		while (fls.index <= parm.bound &&
		       (pts.z != UNDEFZ && pts.theta != UNDEF) && 
		 /*  (!G_is_d_null_value(&pts.z) && pts.theta != UNDEF) &&*/
		       next_point(&pts, &ads, bbs, &length));
	    }

	    if (fls.index > 1 && parm.flout &&
		(loopstep == parm.skip ||
		 !(row % parm.skip || col % parm.skip)))
	    {
		Vect_copy_xy_to_pnts(points, fls.px, fls.py, fls.index);
		Vect_write_line(&fl, LINE, points);
	    }
/*debugg helena
            if (parm.lgout)
            {   lg[col] = length;
                 if(lg[col] > 1800  ||  lg[col] < 0)
                    fprintf(stderr, "length= %d \n", lg[col]);
            }
debugg helena */
	    if (parm.lgout)
	    	lg[col] = (float) length;
	}

	if (parm.lgout)
	    G_put_f_raster_row(lgfd, lg);
    }

    free(fls.px);
    free(fls.py);
/*    free(fls); */ /* commented 19/10/99 MN */
    free(lg);
    Vect_destroy_line_struct(points);

    if (parm.lgout)
        G_close_cell(lgfd);
    diag("done.\n");
}

int main(int argc, char	*argv[])
{
    initialize_globals(argc, argv);

    if (parm.flout || parm.dsout || parm.lgout)
    {
	open_output_files();
	allocate_heap();
	read_input_files();
	precompute();
	calculate();
	if (parm.dsout)
	    write_density_file();
	close_files();
	deallocate_heap();
    }

    exit(0);
}
