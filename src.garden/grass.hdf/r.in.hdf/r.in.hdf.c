/* Written by Bill Brown, USA-CERL, NCSA.
 * December 28, 1992
 */

/* Use to convert HDF scientific data set to grass raster file.
 * Currently, creates a raster file with southwest origin at 
 * 0 East and 0 North, resolution 1.  So to view GRASS raster file
 * after creation, need to set g.region rast=newfile.  If the 
 * SDS is geo-referenced, you need to manually change the new GRASS
 * cellhd file to the correct region.  - hope to fix later
*/

#include <strings.h>

#include "gis.h"
#include <df.h>

typedef int FILEDESC;
double atof();

#define MAXDIMS 8

main(argc, argv)
    int argc;
    char *argv[];
{

    struct Option 	*hdf_file, *rast, *mult;
    struct Flag         *bequiet;
    char 		errbuf[100];
    float 		*data;
    double 		mply;
    int 		rank, row, col, size, dims[MAXDIMS];
    struct Cell_head	w;
    FILEDESC    	cellfile = NULL;
    CELL		*cellbuf; 


    G_gisinit (argv[0]);

    hdf_file = G_define_option();
    hdf_file->key                    = "input";
    hdf_file->type                   = TYPE_STRING;
    hdf_file->required               = YES;
    hdf_file->multiple               = NO;
    hdf_file->description            = "HDF file to be converted.";

    rast = G_define_option();
    rast->key            	   = "output";
    rast->type           	   = TYPE_STRING;
    rast->required     	           = YES;
    rast->gisprompt    		   = "new,cell,raster";
    rast->description  		   = "Name of new raster file.";

    mult = G_define_option();
    mult->key                    = "mult";
    mult->type                   = TYPE_DOUBLE;
    mult->required               = NO;
    mult->multiple               = NO;
    mult->description = 
	"Floating point multiplier. (rastfile = (int)(file.hdf * multiplier))";

    bequiet = G_define_flag ();
    bequiet->key = 'q';
    bequiet->description = "Run quietly";

    if (G_parser (argc, argv))
	exit (-1);
    
    if(mult->answer)
	mply = atof(mult->answer);
    else mply = 1.0;

    if(!bequiet->answer)
	fprintf(stderr,"\nmultiplier = %.4lf\n", mply);

    { /* get data from HDF file */

    	if(-1 == DFishdf(hdf_file->answer)){
	    sprintf(errbuf,"[%s] not a valid HDF file!\n",hdf_file->answer);
	    G_fatal_error(errbuf);
	}
	if(!bequiet->answer)
	    fprintf(stderr,"Loading HDF file %s\n",hdf_file->answer);

	if(-1 == DFSDgetdims(hdf_file->answer, &rank, dims, MAXDIMS)){
	    fprintf(stderr, "error in DFSDgetdims\n");
	    fprintf(stderr, "HDF raster image files not yet supported\n");
	    fprintf(stderr, "HDF file must be a Scientific Data Set (SDS)\n");
	    exit(1);
	}
	if (2 != rank){
	    fprintf(stderr,"%s has %d dimensions, must be 2d for conversion.\n",
				    hdf_file->answer, rank);
	    exit(1);
	}
    
	if(!bequiet->answer)
	    fprintf(stderr,"rows = %d, cols = %d\n", dims[0], dims[1]);

	size = dims[0] * dims[1];
	if((data = (float *)malloc(size  * sizeof(float)))==NULL){
	    fprintf(stderr,"malloc failed\n");
	    exit(1);
	}

	if(-1 == DFSDgetdata(hdf_file->answer, 2, dims, data)){
	    fprintf(stderr, "error in DFSDgetdata\n");
	    exit(1);
	}

    }

    { /* write to new GRASS raster file */

	if(!bequiet->answer)
	    fprintf(stderr,"Writing GRASS output file: %s", rast->answer); 

	w.zone = G_zone();
	w.proj = G_projection();
	w.rows = w.north = dims[0];
	w.cols = w.east = dims[1];
	w.south = w.west = 0.0;
	w.ns_res = w.ew_res = 1;
	
	if(G_set_window (&w) < 0)
	    exit(2);
	

	if ((cellfile = G_open_cell_new(rast->answer)) == -1) 
	{
	    sprintf(errbuf,"Unable to create cellfile for [%s]",rast->answer);
	    G_fatal_error(errbuf);
	}

	cellbuf = G_allocate_cell_buf();
	{	
	    int row_off;
	    float *tf;

	    for (row = 0; row < dims[0]; row++) {

		if(!bequiet->answer)
		    G_percent(row, dims[0] - 1, 10);

		row_off = row * dims[1];
		tf = &(data[row_off]);
		for(col=0; col < w.cols; col++)
		    cellbuf[col] = (int)(*tf++ * mply);
		G_put_map_row(cellfile, cellbuf); 
	    }
	}
	G_close_cell(cellfile);
    }

    if(!bequiet->answer)
	fprintf(stderr,"\nDone.\n"); 
    
    return(1);
}



