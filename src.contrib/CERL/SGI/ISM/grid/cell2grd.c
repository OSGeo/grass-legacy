#include <stdio.h>
#include "gis.h"


#define XRAYMAX 512
#define YRAYMAX 512


#define NODATA 100000000000000000000.0

float elvarr[XRAYMAX][YRAYMAX];         /* column major order */

main (argc, argv)
    char *argv[];
{
    int idmxcl = XRAYMAX;	/* allocated size of array */
    int idmyrw = YRAYMAX;
    int inmxcl, inmyrw;		/* real X/Y sizes */
    float xgdmin, xgdmax;	/* X Min/Max */
    float ygdmin, ygdmax;	/* Y Min/Max */
    float znlval;		/* NO-DATA value */

    int prjflg;			/* projection info flag */
    int iproj;			/* GCTP projection number */
    int izone;			/* Projection zone number */
    int iunits;			/* GCTP units code  */
    double gctpar[8];		/* GCTP projection params */
    int istat;			/* status flag */

    char *desc  ;		/* file desc */
    char namsdf[20];		/* Scattered Data File */
    char sdffld[ 8];		/* Field used in namsdf */
    char namnvf[20];		/* Non-vert fault file */
    char namvfl[20];		/* Vert fault file */

    int i;
    struct Cell_head cellhd;
    CELL *cell;
    int cf;
    int row, col;
    int nrows, ncols;
    float tmp;
    char *name, *mapset;
    int flag;

    struct Option *old, *new;

    static  char  *cell_name = NULL ;
    static  char  *ism_name = NULL ;



    G_gisinit (argv[0]);



/************************** Command Parser ************************************/
        old = G_define_option();
        old->key                        = "input";
        old->type                       =  TYPE_STRING;
        old->required           =  YES;
        old->multiple           =  NO;
        old->gisprompt          = "old,cell,raster";
        old->description        = "Raster file to be converted to ISM grid file";

        new = G_define_option();
        new->key                        = "output";
        new->type                       =  TYPE_STRING;
        new->required           =  YES;
        new->multiple           =  NO;
        new->description        = "Name of resulting grid file";



        if (G_parser (argc, argv))
                exit(-1);

        cell_name = old->answer;
        ism_name = new->answer;

/******************************************************************************/

    if(G_get_window (&cellhd) < 0)
        exit(3);


    flag = 0;
    if (cellhd.rows > YRAYMAX)
    {
	cellhd.rows = YRAYMAX;
	flag = 1;
    }
    if (cellhd.cols > XRAYMAX)
    {
	cellhd.cols = XRAYMAX;
	flag = 1;
    }
    if (flag)
    {
	fprintf (stderr, "Warning: Changing window to %d rows x %d cols\n", cellhd.rows, cellhd.cols);
	fprintf (stderr, "ISM maximum grid size is %d x %d\n\n", YRAYMAX, XRAYMAX);
	G_adjust_Cell_head (&cellhd, 1, 1);
	G_set_window (&cellhd);
    }
    /* 
    ** dunno what iproj flags  are, but -1 seems to be no grid 
    **  1 might be utm?
    */
    if (cellhd.proj == 0) 
	iproj = -1;
    else 
	iproj = 1;
    izone = cellhd.zone;
    ygdmax = cellhd.north;
    ygdmin = cellhd.south;
    xgdmax = cellhd.east;
    xgdmin = cellhd.west;
    inmyrw = cellhd.rows;
    inmxcl = cellhd.cols;

    znlval = NODATA;
    prjflg = 1;	/* assuming UTM */
    iunits = 8;
    for (i = 0 ; i < 8 ; i++)
	gctpar[i] = 0;

    nrows = cellhd.rows;
    ncols = cellhd.cols;

    name = G_store (cell_name);
    mapset = G_find_file2 ("cell", name, "");

    cell = G_allocate_cell_buf();
    cf = G_open_cell_old (name, mapset);
    if (!cf)
    {
        char msg[100];
        sprintf (msg, "unable to create layer %s", argv[1]);
        G_fatal_error (msg);
    }

    for (row = 0 ; row < nrows ; row++)
    {
	if (0 > G_get_map_row (cf, cell, row))
	    G_fatal_error ("Error reading cell file");
	for (col = 0 ; col < ncols ; col++)
	{
	    tmp = cell[col];
	    /*elvarr[nrows - row][col] = tmp == 0 ? znlval : tmp;*/
	    /*elvarr[(nrows-1) - row][col] = tmp;;*/
	    elvarr[(nrows-1) - row][col] = tmp == 0 ? NODATA : tmp;;
	}
    }
    G_close_cell (cf);

    desc = G_get_cell_title (name, mapset);

    strcpy (namsdf, "");
    strcpy (sdffld, "");
    strcpy (namvfl, "");
    strcpy (namnvf, "");

    gdwrit_ (   ism_name, desc, 
    		elvarr,
   		&idmxcl, &idmyrw,
	        &inmxcl, &xgdmin, &xgdmax,
		&inmyrw, &ygdmin, &ygdmax,
		&znlval,
		namsdf, sdffld, namvfl, namnvf,
		&prjflg, &iproj, &izone, &iunits, gctpar,
		&istat,  strlen(ism_name),  strlen (desc), strlen (namsdf),
		strlen (sdffld), strlen (namvfl),   strlen (namnvf)
		);


    printf ("Size (%d, %d) \n", inmxcl, inmyrw);
    printf ("No-Data = %f\n", znlval);
    printf ("prjflag %d  Proj %d  Zone %d  Units %d\n", prjflg, iproj, izone, iunits);
    /*
    printf ("GCT Params:\n");
    for (i = 0 ; i < 8 ; i++)
	printf ("   %lf\n", gctpar[i]);
    */
}
