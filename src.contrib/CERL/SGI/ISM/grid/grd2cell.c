#include <stdio.h>
#include <math.h>
#include "gis.h"


#define XRAYMAX 512
#define YRAYMAX 512
#define USAGE  "ISMgrd2cell input=ISM_grid_file output=GRASS_raster_file"

float elvarr[XRAYMAX][YRAYMAX];         /* column major order */

struct Option *old, *new, *mult;


main (argc, argv)
    char *argv[];
{
    int idmxcl = XRAYMAX;	/* allocated size of array */
    int idmyrw = YRAYMAX;
    int inmxcl, inmyrw;		/* real X/Y sizes */
    float xgdmin, xgdmax;	/* X Min/Max */
    float ygdmin, ygdmax;	/* Y Min/Max */
    float znlval;		/* NO-DATA value */
    float multiplier = 1.0;

    int prjflg;			/* projection info flag */
    int iproj;			/* GCTP projection number */
    int izone;			/* Projection zone number */
    int iunits;			/* GCTP units code  */
    double gctpar[8];		/* GCTP projection params */
    int istat;			/* status flag */

    char desc  [40];		/* file desc */
    char namsdf[20];		/* Scattered Data File */
    char sdffld[ 8];		/* Field used in namsdf */
    char namnvf[20];		/* Non-vert fault file */
    char namvfl[20];		/* Vert fault file */
    char *in_name, *out_name;


    int i;
    struct Cell_head cellhd;
    CELL *cell;
    int cf;
    int row, col;
    int nrows, ncols;
    float tmp;


    G_gisinit (argv[0]);

        
/************************** Command Parser ************************************/

        old = G_define_option();
        old->key                = "input";
        old->type               = TYPE_STRING;
        old->required           = YES;
        old->multiple           = NO;
        old->description        = "ISM grid input file";

        new = G_define_option();
        new->key                = "output";
        new->type               = TYPE_STRING;
        new->required           = YES;
        new->multiple           = NO;
        new->gisprompt          = "new,cell,raster";
        new->description        = "name of resulting raster file";

        mult = G_define_option();
        mult->key                = "mult";
        mult->type               = TYPE_DOUBLE;
        mult->required           = NO;
        mult->multiple           = NO;
	mult->answer		 = "1";
        mult->description        = "multiplier of cell data";

        if (G_parser (argc, argv))
                exit(-1);

    in_name  = old->answer;
    out_name = new->answer;
    multiplier = atof (mult->answer);

    if (in_name == NULL  ||   out_name == NULL)
    {
        fprintf (stderr, "%s: Command line error.\n\n Usage: %s\n",
                argv[0], USAGE);
        exit (-1);
    }

/******************************************************************************/


    gdread_ (   in_name, desc, 
    		elvarr, 
   		&idmxcl, &idmyrw,
	        &inmxcl, &xgdmin, &xgdmax, 
		&inmyrw, &ygdmin, &ygdmax, 
		&znlval, 
		namsdf, sdffld, namvfl, namnvf,
		&prjflg, &iproj, &izone, &iunits, gctpar, 
		&istat,  strlen(in_name),  40, 20,   8, 20,   20
		);
	    
    cellhd.zone = izone;
    /* 
    ** dunno what iproj flags  are, but -1 seems to be no grid 
    **  1 might be utm?
    */
    cellhd.proj = iproj == 1 ? 1 : 0;
    cellhd.north = ygdmax;
    cellhd.south = ygdmin;
    cellhd.east = xgdmax;
    cellhd.west = xgdmin;
    cellhd.rows = inmyrw;
    cellhd.cols = inmxcl;

    cellhd.ns_res = (cellhd.north - cellhd.south)/cellhd.rows;
    cellhd.ew_res = (cellhd.east - cellhd.west)/cellhd.cols;
    nrows = cellhd.rows;
    ncols = cellhd.cols;

    if(G_set_window (&cellhd) < 0)
        exit(3);

    if (nrows != G_window_rows())
    {
        fprintf (stderr, "OOPS: rows changed from %d to %d\n", nrows, G_window_rows());
        exit(1);
    }
    if (ncols != G_window_cols())
    {
        fprintf (stderr, "OOPS: cols changed from %d to %d\n", ncols, G_window_cols());
        exit(1);
    }


    cell = G_allocate_cell_buf();
    cf = G_open_cell_new (out_name);
    if (!cf)
    {
        char msg[100];
        sprintf (msg, "unable to create layer %s", out_name);
        G_fatal_error (msg);
        exit(1);
    }

    for (row = 0 ; row < nrows ; row++)
    {
	for (col = 0 ; col < ncols ; col++)
	{
	    tmp = elvarr[nrows - row][col];
	    cell[col] = tmp == znlval ? 0 : tmp * multiplier;
	}
	G_put_map_row (cf, cell);
    }
    fprintf (stderr, "CREATING SUPPORT FILES FOR %s\n", out_name);

    G_close_cell (cf);

    desc[39] = 0;	/* terminate Fortran string */
    G_put_cell_title (out_name, desc);


    printf ("Size (%d, %d) \n", inmxcl, inmyrw);
    printf ("No-Data = %f\n", znlval);
    printf ("prjflag %d  Proj %d  Zone %d  Units %d\n", prjflg, iproj, izone, iunits);
    /*
    printf ("GCT Params:\n");
    for (i = 0 ; i < 8 ; i++)
	printf ("   %lf\n", gctpar[i]);
    */
}
