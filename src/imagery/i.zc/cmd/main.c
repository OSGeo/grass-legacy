/*
Zero Crossings for GRASS
 Central Washington University GIS Laboratory
 Programmer: David B. Satnik

 Based on code provided by Bill Hoff at University of Illinois.

*/

#define MAIN

#include <stdlib.h>
#include "local_proto.h"
#include <math.h>
#include "gis.h"
#include "globals.h"


int main( int argc, char *argv[])
{
        /* Global variable & function declarations */

        int inputfd, zcfd; /* the input and output file descriptors */
        char *inmapset; /* the input mapset name */
        struct Cell_head window;
        CELL *cell_row;
        float Width ;

        long max_pow2(); /* Used to find the smallest power of 2 >= to a number
   */
        int i,j;         /* Loop control variables */
        int or,oc;      /* Original dimensions of image */
        int rows,cols;  /* Smallest powers of 2 >= number of rows & columns */
        int size;      /* the length of one side */
        long totsize; /* the Total number of data points */
        double *data[2]; /* Data structure containing real & complex values of FFT */
        int save_args(); /* function to stash the command line arguments */
		struct GModule *module;
        struct Option *input_map, *output_map, *width, *threshold, *orientations ;
        char *me ;

        G_gisinit(argv[0]);
        me = G_program_name();

		module = G_define_module();
		module->description =
			"Zero-crossing \"edge detection\" raster "
			"function for image processing.";

        /* define options */
        input_map = G_define_option();
        input_map->key          = "input";
        input_map->type         = TYPE_STRING;
        input_map->required             = YES;
        input_map->multiple             = NO;
        input_map->gisprompt  = "old,cell,raster" ;
        input_map->description  = "input raster map";
#define INPUT_MAP input_map->answer

        output_map = G_define_option();
        output_map->key                 = "output";
        output_map->type                = TYPE_STRING;
        output_map->required            = YES;
        output_map->multiple            = NO;
        output_map->gisprompt           = "new,cell,raster" ;
        output_map->description         = "zero crossing raster map";
#define OUTPUT_MAP output_map->answer

        width = G_define_option();
        width->key              = "width";
        width->type             = TYPE_INTEGER;
        width->required         = NO;
        width->multiple         = NO;
        width->description = "x-y extent of the Gaussian filter";
        width->answer           = "9";

        threshold = G_define_option();
        threshold->key          = "threshold";
        threshold->type         = TYPE_DOUBLE;
        threshold->required     = NO;
        threshold->multiple     = NO;
        threshold->description = "sensitivity of Gaussian filter";
        threshold->answer      = "10";

        orientations = G_define_option();
        orientations->key               = "orientations";
        orientations->type              = TYPE_INTEGER;
        orientations->required  = NO;
        orientations->multiple  = NO;
        orientations->description = "number of azimuth directions categorized";
        orientations->answer    = "1";

        /* call parser */
        if(G_parser(argc, argv))
                exit(-1);

        /* open input cell map */
        if ((inmapset = G_find_cell(INPUT_MAP,"")) == NULL )
        {
                fprintf (stderr, "%s: %s - raster map not found\n", me, INPUT_MAP);
                exit(1);
        }
        inputfd = G_open_cell_old(INPUT_MAP, inmapset );
        if (inputfd < 0)
                exit(1);

        /* check command line args for validity */
        if(G_legal_filename(OUTPUT_MAP) < 0)
        {
                fprintf (stderr, "%s: %s - illegal name\n", me, OUTPUT_MAP);
                exit(1);
        }

        sscanf(threshold -> answer, "%1lf", &Thresh);
        if (Thresh<=0.0)
                G_fatal_error("Threshold less than or equal to zero not allowed. ");
        Thresh /= 100.0;

        sscanf(width -> answer, "%f", &Width);

        if (Width<=0.0)
                G_fatal_error("Width less than or equal to zero not allowed.");

        sscanf(orientations -> answer, "%d", &NumOrients);
        if(NumOrients<1)
                G_fatal_error("Fewer than 1 orientation classes not allowed.");


        /* get the current window for later */
        G_get_set_window(&window);

        /* get the rows and columns in the current window */
        or = G_window_rows();
        oc = G_window_cols();
        rows = max_pow2(or);
        cols = max_pow2(oc);
        size = (rows>cols) ? rows : cols;
        totsize = size * size;

        fprintf(stderr,"Power 2 values : %d rows %d columns\n",rows,cols);

        /* Allocate appropriate memory for the structure containing
     the real and complex components of the FFT.  DATA[0] will
     contain the real, and DATA[1] the complex component.
     */
        data[0] = (double *) G_malloc(totsize*sizeof(double));
        data[1] = (double *) G_malloc(totsize*sizeof(double));
        if (data[0] == NULL || data[1] == NULL)
                G_fatal_error("Insufficent memory for allocation of data structure");

        /* Initialize real & complex components to zero */
        fprintf(stderr, "Initializing data...\n");
        for (i=0; i<(totsize); i++) {
                *(data[0]+i) = 0.0;
                *(data[1]+i) = 0.0;
        }

        /* allocate the space for one row of cell map data */
        cell_row = G_allocate_cell_buf();

        /* Read in cell map values */
        fprintf(stderr, "Reading the raster map...\n");
        for (i=0; i<or; i++) {
                if (G_get_map_row(inputfd, cell_row, i)<0)
                        G_fatal_error("Error while reading input raster map.");
                for (j=0; j<oc; j++)
                        *(data[0]+(i*size)+j) = (double) cell_row[j];
        }
        /* close input cell map and release the row buffer */
        G_close_cell(inputfd);
        free(cell_row);

        /* take the del**2g of image */
        del2g(data, size, Width);

        /* find the zero crossings:  Here are several notes -
     1) this routine only uses the real values
     2) it places the zero crossings in the imaginary array */
        findzc(data[0], size, data[1], Thresh);

        /* open the output cell maps and allocate cell row buffers */
        fprintf(stderr, "Writing transformed data to file...\n");
        if ((zcfd = G_open_cell_new(OUTPUT_MAP)) < 0)
                exit(1);

        cell_row = G_allocate_cell_buf();

        /* Write out result to a new cell map */
        for (i=0; i<or; i++) {
                for (j=0; j<oc; j++) {
                        *(cell_row+j) = (CELL) (*(data[1]+i*cols+j));
                }
                G_put_map_row(zcfd, cell_row);
        }
        G_close_cell(zcfd);

        free(cell_row);

        /* Release memory resources */
        for (i=0 ; i<2 ; i++) free(data[i]);
        fprintf(stderr, "Transform successful\n");
        exit(0);
}


/*****************************************************************************/
/* MAX_POW2 : finds least power of 2 greater than or equal to number         */
/*                                                                           */
/* Input arguments: n - unsigned integer, the number                         */
/*                                                                           */
/* Output is an integer power of 2                                           */
/*                                                                           */
/*****************************************************************************/

long 
max_pow2 (long n)

{
        long p2, n1;

        n1 = n >> 1;
        p2 = 1;
        while (n1 > 0)
        {
                n1 >>= 1;
                p2 <<= 1;
        }
        if (p2 < n) p2 <<=1;
        return(p2);
}   /* end max_pow2 */

/*************************************************************************/
