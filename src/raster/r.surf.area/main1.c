
/* main.c - r.surf.area */
/* Written by Bill Brown, brown@zorro.cecer.army.mil
 * All disclaimers apply - this was a quickie
 * December 21, 1994
*/

/* Calculates area of regular 3D triangulated points in current region by
 * adding areas of triangles.  Therefore, area of a flat surface 
 * will be reported as (rows + cols -1)*(area of cell) less than area of 
 * flat region due to a half row and half column missing around the 
 * perimeter.
*/


#include "gis.h"
#include "math.h"

typedef int FILEDESC;
double atof();

#define X 0
#define Y 1
#define Z 2

main(argc, argv)
    int argc;
    char *argv[];
{

    struct Option 	*surf, *vscale;
    struct Flag 	*tdir; 
    char 		*cellmap, errbuf[100];
    CELL		*cell_buf[2];
    int 		row;
    struct Cell_head	w;
    FILEDESC    	cellfile = NULL;
    double 		xarea=0.0, sz;

    G_gisinit (argv[0]);

    surf = G_define_option();
    surf->key                    = "input";
    surf->type                   = TYPE_STRING;
    surf->required               = YES;
    surf->multiple               = NO;
    surf->gisprompt              = "old,cell,Raster";
    surf->description            = "Raster file for surface.";

    vscale = G_define_option();
    vscale->key                    = "vscale";
    vscale->type                   = TYPE_DOUBLE;
    vscale->required               = NO;
    vscale->multiple               = NO;
    vscale->description            = "Vertical scale";

    tdir = G_define_flag ();
    tdir->key = 't';
    tdir->description = "Reverse triangulation direction";

    if (G_parser (argc, argv))
	exit (-1);

    if(vscale->answer)
	sz = atof(vscale->answer);
    else sz = 1.0;


    G_get_set_window (&w); 

    /* open cell file for reading */
    {  
	cellmap = G_find_file2 ("cell", surf->answer, "");
	if(!cellmap){
	    sprintf(errbuf,"Couldn't find raster file %s", surf->answer);
	    G_fatal_error(errbuf);
	}

	if ((cellfile = G_open_cell_old(surf->answer, cellmap)) == -1) 
	{
	    sprintf(errbuf,"Not able to open cellfile for [%s]", surf->answer);
	    G_fatal_error(errbuf);
	}
    }

    cell_buf[0] = (CELL *)G_malloc (w.cols * sizeof (CELL));
    cell_buf[1] = (CELL *)G_malloc (w.cols * sizeof (CELL));
    
    {
    CELL *top, *bottom;
	for (row=0; row < w.rows-1; row++){
	    if(!row){
		G_get_map_row(cellfile, cell_buf[1], 0); 
		top=cell_buf[1];
	    }
	    G_get_map_row(cellfile, cell_buf[row%2], row+1); 
	    bottom=cell_buf[row%2];
	    add_row_area(top, bottom, sz, &w, tdir->answer, &xarea);
	    top=bottom;
	}
    }


    free(cell_buf[0]);
    free(cell_buf[1]);
    G_close_cell(cellfile);

    { /* report */
    double reg_area, flat_area;

	flat_area = (w.cols-1) * (w.rows-1) * w.ns_res * w.ew_res;
	reg_area = w.cols * w.rows * w.ns_res * w.ew_res;

	fprintf(stderr,"Surface Area: %.12lf\n", xarea);
/*
	fprintf(stderr,"Flat Area: %.12lf\n", flat_area);
	fprintf(stderr,"Region Flat Area: %.12lf\n", reg_area);
	fprintf(stderr,"Estimated Region Surface Area: %.12lf\n", 
		reg_area * xarea/flat_area );
*/
	fprintf(stderr,"\nDone.\n"); 
    }
    
    return(1);
}

/************************************************************************/

add_row_area(top, bottom, sz, w, dir, xarea)
CELL *top, *bottom;
double sz;
struct Cell_head	*w;
int dir;
double *xarea;
{
double mag, tedge1[3], tedge2[3], crossp[3];
int col;


	for(col = 0; col < w->cols-1; col++) {
	    
	    if(dir){ /* ul to lr diag */
		tedge1[X] = w->ew_res;
		tedge1[Y] = -w->ns_res;
		tedge1[Z] = sz * (bottom[col+1] - top[col]);

		/* upper */
		tedge2[X] = 0.0;
		tedge2[Y] = w->ns_res;
		tedge2[Z] = sz * (top[col+1] - bottom[col+1]);

		v3cross(tedge1, tedge2, crossp);
		v3mag(crossp, &mag);
		*xarea += .5 * mag;

		/* lower */
		tedge2[X] = -w->ew_res; 
		tedge2[Y] = 0.0;
		tedge2[Z] = sz * (bottom[col] - bottom[col+1]);

		v3cross(tedge1, tedge2, crossp);
		v3mag(crossp, &mag);
		*xarea += .5 * mag;
	    }

	    else{ /* ll to ur diag */
		tedge1[X] = w->ew_res;
		tedge1[Y] = w->ns_res;
		tedge1[Z] = sz * (top[col+1] - bottom[col]);

		/* upper */
		tedge2[X] = -w->ew_res;
		tedge2[Y] = 0.0;
		tedge2[Z] = sz * (top[col+1] - top[col+1]);

		v3cross(tedge1, tedge2, crossp);
		v3mag(crossp, &mag);
		*xarea += .5 * mag;

		/* lower */
		tedge2[X] = 0.0; 
		tedge2[Y] = -w->ns_res;
		tedge2[Z] = sz * (bottom[col+1] - top[col+1]);

		v3cross(tedge1, tedge2, crossp);
		v3mag(crossp, &mag);
		*xarea += .5 * mag;
	    }

	} /* ea col */

    fprintf(stderr," %.12lf\r", *xarea);
    
}

/************************************************************************/
/* return the cross product v3 = v1 cross v2 */
v3cross(v1, v2, v3)
double v1[3], v2[3], v3[3];
{
        v3[X] = (v1[Y]*v2[Z]) - (v1[Z]*v2[Y]);
        v3[Y] = (v1[Z]*v2[X]) - (v1[X]*v2[Z]);
        v3[Z] = (v1[X]*v2[Y]) - (v1[Y]*v2[X]);
}

/************************************************************************/
/* magnitude of vector */
v3mag(v1, mag)
double v1[3], *mag;
{
    *mag = sqrt(v1[X] * v1[X] + v1[Y] * v1[Y] +v1[Z] * v1[Z]);
}
/************************************************************************/
/************************************************************************/


