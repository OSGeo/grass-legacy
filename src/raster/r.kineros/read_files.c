#include <stdio.h>
#include <math.h>
#include "define.h"
#include "global.h"

/*****************************************************************************/
/*  Read in input file names.                                                */
/*****************************************************************************/

read_files(argc,argv)
int   argc;
char *argv[];
{

    char *map;

/*****************************************************************************/
/*  Set GRASS input parameters (Option structure).                           */
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
 *  Set values for parm.dem
 */
    parm.dem = G_define_option() ;
    parm.dem->key        = "dem";
    parm.dem->type       = TYPE_STRING;
    parm.dem->required   = YES;
    parm.dem->gisprompt  = "old,cell,raster" ;
    parm.dem->description= "Name of the DEM map" ;
/*
 *  Set values for parm.drain
 */
    parm.drain = G_define_option() ;
    parm.drain->key        = "drainage";
    parm.drain->type       = TYPE_STRING;
    parm.drain->required   = YES;
    parm.drain->gisprompt  = "old,cell,raster" ;
    parm.drain->description= "Name of the DRAINAGE DIRECTION map" ;
/*
 *  Set values for parm.half_basin
 */
    parm.half_basin = G_define_option() ;
    parm.half_basin->key        = "half_basin";
    parm.half_basin->type       = TYPE_STRING;
    parm.half_basin->required   = YES;
    parm.half_basin->gisprompt  = "old,cell,raster" ;
    parm.half_basin->description= "Name of the HALF BASIN map" ;
/*
 *  Set values for parm.rock
 */
    parm.rock = G_define_option() ;
    parm.rock->key        = "rock";
    parm.rock->type       = TYPE_STRING;
    parm.rock->required   = YES;
    parm.rock->gisprompt  = "old,cell,raster" ;
    parm.rock->description= "Name of the ROCK map" ;
/*
 *  Set values for parm.slopes
 */
    parm.slopes = G_define_option() ;
    parm.slopes->key        = "slopes";
    parm.slopes->type       = TYPE_STRING;
    parm.slopes->required   = YES;
    parm.slopes->gisprompt  = "old,cell,raster" ;
    parm.slopes->description= "Name of the SLOPES map" ;
/*
 *  Set values for parm.soil
 */
    parm.soil = G_define_option() ;
    parm.soil->key        = "soil";
    parm.soil->type       = TYPE_STRING;
    parm.soil->required   = YES;
    parm.soil->gisprompt  = "old,cell,raster" ;
    parm.soil->description= "Name of the SOIL map" ;
/*
 *  Set values for parm.stream
 */
    parm.stream = G_define_option() ;
    parm.stream->key        = "stream";
    parm.stream->type       = TYPE_STRING;
    parm.stream->required   = YES;
    parm.stream->gisprompt  = "old,cell,raster" ;
    parm.stream->description= "Name of the STREAM map" ;
/*
 *  Set values for parm.width
 */
    parm.width = G_define_option() ;
    parm.width->key        = "width";
    parm.width->type       = TYPE_STRING;
    parm.width->required   = YES;
    parm.width->gisprompt  = "old,cell,raster" ;
    parm.width->description= "Name of the STREAM WIDTH map" ;

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

/*****************************************************************************/
/*  Find the name of mapset that we are going to use.                        */
/*****************************************************************************/

    map = parm.dem->answer;
    mapset = G_find_cell2(map, "");

    if (mapset == NULL) {
        char msg[100];	
	sprintf (msg, "%s: <%s> cellfile not found\n", G_program_name(), map);
		G_fatal_error (msg);
        exit(1);
    }

/*****************************************************************************/
/*  Determine the number of rows and columns.                                */
/*****************************************************************************/

    nrows = G_window_rows();
    ncols = G_window_cols();
    printf ("\n", nrows);
    printf ("nrows: %d\n", nrows);
    printf ("ncols: %d\n", ncols);

    G_get_window(&region);
    if(region.ew_res != region.ns_res) {
	printf("\n ERROR: ns and ew resolutions not equal.");
        printf("\n ew_res = %d",region.ew_res);
        printf("\n ns_res = %d",region.ns_res);
	exit(0);
    }
    res = (float)region.ns_res;
    res_diag = res*sqrt(2.0);
    printf ("resolution:   %f\n", res);
    printf ("along diag:   %f\n", res_diag);
}
