#include "gis.h"
#include <stdio.h>

int **imatrix();
int  *ivector();

/*****************************************************************************/
/*  Program to determing the topology of a stream network.  A file is        */
/*  generated that reports the tributaries that are joined at the head       */
/*  of each stream segement.                                                 */
/*****************************************************************************/

main(argc,argv) 
int   argc;
char *argv[];
{
/*
 *  Matricies
 */
    int **accum;
    int **chann;
    int **aspect;

    CELL *cell;

    char *chann_name; 
    char *accum_name; 
    char *aspect_name; 
    char *mapset;

    int col;
    int fd_accum;
    int fd_chann;
    int fd_aspect;
    int ncols;
    int nrows;
    int row;

    struct {
	struct Option *accum ;
	struct Option *chann ;
	struct Option *aspect ;
    } parm;

/*****************************************************************************/
/*  Allocate memory for the Option structure and return a pointer to this    */
/*  memeory.  Do this for the structured variables parm.accum and            */
/*  parm.chann.                                                              */
/*****************************************************************************/
/*
 *  Set values for parm.accum
 */
    parm.accum = G_define_option() ;
    parm.accum->key        = "accumulation";
    parm.accum->type       = TYPE_STRING;
    parm.accum->required   = YES;
    parm.accum->gisprompt  = "old,cell,raster" ;
    parm.accum->description= "Name of the ACCUMULATION map" ;
/*
 *  Set values for parm.chann
 */
    parm.chann = G_define_option() ;
    parm.chann->key        = "stream";
    parm.chann->type       = TYPE_STRING;
    parm.chann->required   = YES;
    parm.chann->gisprompt  = "old,cell,raster" ;
    parm.chann->description= "Name of the STREAM map" ;
/*
 *  Set values for parm.aspect
 */
    parm.aspect = G_define_option() ;
    parm.aspect->key        = "drainage";
    parm.aspect->type       = TYPE_STRING;
    parm.aspect->required   = YES;
    parm.aspect->gisprompt  = "old,cell,raster" ;
    parm.aspect->description= "Name of the DRAINAGE DIRECTION map" ;

/*****************************************************************************/
/*  Initailize GIS library for this program.                                 */
/*****************************************************************************/

    G_gisinit(argv[0]);

/*****************************************************************************/
/*  Parse values from the command line.  If this is not successful, then     */
/*  display a usage statement and exit.                                      */
/*****************************************************************************/

    if (G_parser(argc, argv))
       	exit (-1);
    accum_name = parm.accum->answer;
    chann_name = parm.chann->answer;
    aspect_name = parm.aspect->answer;

/*****************************************************************************/
/*  Find the name of mapset that we are going to use.                        */
/*****************************************************************************/

    mapset = G_find_cell2 (accum_name, "");

    if (mapset == NULL) {
        char msg[100];	
	sprintf (msg, "%s: <%s> cellfile not found\n", G_program_name(), accum_name);
		G_fatal_error (msg);
        exit(1);
    }

/*****************************************************************************/
/*  Open the cell file "name" in "mapset".                                   */
/*****************************************************************************/

    fd_accum = G_open_cell_old (accum_name, mapset);
    if (fd_accum < 0)
    	exit(1);

    fd_chann = G_open_cell_old (chann_name, mapset);
    if (fd_chann < 0)
    	exit(1);

    fd_aspect = G_open_cell_old (aspect_name, mapset);
    if (fd_aspect  < 0)
    	exit(1);

/*****************************************************************************/
/*  Open up a vector that is just long enough to hold one row of data.       */
/*****************************************************************************/

    cell = G_allocate_cell_buf();

/*****************************************************************************/
/*  Determine the number of rows and columns.                                */
/*****************************************************************************/

    nrows = G_window_rows();
    ncols = G_window_cols();
    printf ("\n", nrows);
    printf ("nrows: %d\n", nrows);
    printf ("ncols: %d\n", ncols);

/*****************************************************************************/
/*  Allocate memory for matricies.                                           */
/*****************************************************************************/

    accum  = imatrix(0,nrows,0,ncols);
    chann  = imatrix(0,nrows,0,ncols);
    aspect = imatrix(0,nrows,0,ncols);

/*****************************************************************************/
/*  Process DEM and Channel files.                                           */
/*****************************************************************************/

    for (row=(nrows-1); row>=0; row--) {
	if(G_get_map_row (fd_accum, cell, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++) {
	    accum[row][col] = (int)cell[col];
	}
	if(G_get_map_row (fd_chann, cell, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++) {
	    chann[row][col] = (int)cell[col];
	}
	if(G_get_map_row (fd_aspect, cell, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++) {
	    aspect[row][col]  = (int)cell[col];
	}
    }

/*****************************************************************************/
/*  Compute the topology of the network.                                     */
/*****************************************************************************/

    (void)find_tribs(nrows, ncols, accum, chann, aspect);

    exit(0);
}
