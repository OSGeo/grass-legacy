#include <stdio.h>
#include <math.h>
#include "define.h"
#include "global.h"

/*****************************************************************************/
/*  Program to generate a KINEROS input file from GRASS mapsets.  This       */
/*  program also determines the topology of a stream network.  Given this    */
/*  topology, elements (streams and hillslopes) are printed in their proper  */
/*  computational order.                                                     */
/*****************************************************************************/

main(argc,argv) 
int   argc;
char *argv[];
{
    char *map;

    float **slopes;

    int **accum;
    int **dem;
    int **drain;
    int **half_basin;
    int **stream;

    CELL *cell;

    int col;
    int row;

/*****************************************************************************/
/*  Read in names of GRASS files.                                            */
/*****************************************************************************/

    (void)read_files(argc, argv);

/*****************************************************************************/
/*  Prompt user for KINEROS parameters (time, space units, temperature, ...) */
/*****************************************************************************/

    (void)read_par();

/*****************************************************************************/
/*  Process DEM and Channel files.                                           */
/*****************************************************************************/

    accum = imatrix(0,nrows,0,ncols);
    map = parm.accum->answer;
    (void)read_int(map, mapset, accum);

    dem = imatrix(0,nrows,0,ncols);
    map = parm.dem->answer;
    (void)read_int(map, mapset, dem);

    drain = imatrix(0,nrows,0,ncols);
    map = parm.drain->answer;
    (void)read_int(map, mapset, drain);

    stream = imatrix(0,nrows,0,ncols);
    map = parm.stream->answer;
    (void)read_int(map, mapset, stream);

/*****************************************************************************/
/*  Find range of element numbers.                                           */
/*****************************************************************************/

    min_ele = nrows*ncols;
    num_ele = 0;

    for(row=0; row<nrows; row++) {
	for(col=0; col<ncols; col++) {
            if(stream[row][col] > num_ele) {
		printf("%d ",stream[row][col]);
		num_ele = stream[row][col];
	    }
            if((stream[row][col] >  0) && (stream[row][col] < min_ele))
		min_ele = stream[row][col];
	}
    }
    num_ele = index(num_ele,STREAM);

    printf("\n Total number of elements = %d",num_ele);

/*****************************************************************************/
/*  Allocate space for element data.                                         */
/*****************************************************************************/

    element = (struct data *)malloc((unsigned) (num_ele+1)*sizeof(struct data));
    if (!element) {
	printf("\n ERROR: allocation failure for data struct for element");
	exit(0);
    }

/*****************************************************************************/
/*  Allocate space for element data.                                         */
/*****************************************************************************/

    (void)initialize(stream);

/*****************************************************************************/
/*  Find the head and mouth of each stream.                                  */
/*****************************************************************************/
 
    (void)find_stream(accum, dem, stream, drain);

/*****************************************************************************/
/*  Fing the tributaries for each stream.                                    */
/*****************************************************************************/

    (void)find_tribs(accum, stream, drain, dem);

/*****************************************************************************/
/*  Find the computational order for each element.                           */
/*****************************************************************************/

    (void)stream_order();

/*****************************************************************************/
/*  Read in channel widths.                                                  */
/*****************************************************************************/

    map = parm.width->answer;
    (void)stream_width(stream, map, mapset);

/*****************************************************************************/
/*  Free up memory.                                                          */
/*****************************************************************************/

    (void)free_imatrix(accum,0,nrows,0,ncols);
    (void)free_imatrix(dem,0,nrows,0,ncols);
    (void)free_imatrix(drain,0,nrows,0,ncols);
    (void)free_imatrix(stream,0,nrows,0,ncols);

/*****************************************************************************/
/*  Assign planes for each stream.                                           */
/*****************************************************************************/

    (void)contrib_planes();

/*****************************************************************************/
/*  Compute slopes and width, lengths of all plane elements.                 */
/*****************************************************************************/

    half_basin = imatrix(0,nrows,0,ncols);
    map = parm.half_basin->answer;
    (void)read_int(map, mapset, half_basin);

    slopes = fmatrix(0,nrows,0,ncols);
    map = parm.slopes->answer;
    (void)read_float(map, mapset, slopes);
/*
 *  Convert slope from percent to fractional value.
 */
    for(row=0; row<nrows; row++) {
	for(col=0; col<ncols; col++) {
	    slopes[row][col] = slopes[row][col]/100.0;
	}
    }

    (void)plane_geom(slopes, half_basin);

/*****************************************************************************/
/*  Compute soil parameters based on texture.                                */
/*****************************************************************************/

    (void)soil_param(half_basin, parm.soil->answer, parm.rock->answer, mapset);

/*****************************************************************************/
/*  Print KINEROS element file.                                              */
/*****************************************************************************/

    (void)print_ele();
}
