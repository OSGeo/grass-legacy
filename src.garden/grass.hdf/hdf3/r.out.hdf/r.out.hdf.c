/* Written by Bill Brown, USA-CERL, NCSA.
 * December 28, 1992
*/

/* Use to convert grass raster file to HDF scientific data set.
 * Only creates a 2d SDS, no color information is transferred.
 * I hope to write a GRASS raster to HDF raster conversion program
 * soon.
*/

#include <strings.h>

#include "gis.h"
#include <df.h>

typedef int FILEDESC;
double atof();

main(argc, argv)
    int argc;
    char *argv[];
{

    struct Option 	*rast, *hdf_file, *mult;
    struct Flag 	*bequiet;
    char 		*cellmap, *p, errbuf[100], ofile[1000];
    float 		*data, Max, Min;
    int			*int_buf;
    double 		mply;
    int 		row, col, size, dims[2], i, ret;
    struct Cell_head	w;
    FILEDESC    	cellfile = NULL;


    G_gisinit (argv[0]);

    rast = G_define_option();
    rast->key                    = "input";
    rast->type                   = TYPE_STRING;
    rast->required               = YES;
    rast->multiple               = NO;
    rast->gisprompt              = "old,cell,Raster";
    rast->description            = "Raster file to be converted.";

    hdf_file = G_define_option();
    hdf_file->key                    = "output";
    hdf_file->type                   = TYPE_STRING;
    hdf_file->required               = NO;
    hdf_file->multiple               = NO;
    hdf_file->answer	       	     = "<rasterfilename>.hdf";
    hdf_file->description            = "Name for new HDF file.";

    mult = G_define_option();
    mult->key                    = "mult";
    mult->type                   = TYPE_DOUBLE;
    mult->required               = NO;
    mult->multiple               = NO;
    mult->description     =
	    "Floating point multiplier. (file.hdf = cellfile * multiplier)";

    bequiet = G_define_flag ();
    bequiet->key = 'q';
    bequiet->description = "Run quietly";

    if (G_parser (argc, argv))
	exit (-1);
    
    if(mult->answer)
	mply = atof(mult->answer);
    else mply = 1.0;

    if(!bequiet->answer)
	fprintf(stderr, "multiplier = %.4lf\n", mply);
    
    G_get_set_window (&w); 
    dims[0] = w.rows;
    dims[1] = w.cols;

    if(!bequiet->answer)
	fprintf(stderr,"rows = %d, cols = %d\n", dims[0], dims[1]);

    {
	cellmap = G_find_file2 ("cell", rast->answer, "");
	if(!cellmap){
	    sprintf(errbuf,"Couldn't find raster file %s", rast->answer);
	    G_fatal_error(errbuf);
	}

	if ((cellfile = G_open_cell_old(rast->answer, cellmap)) == -1) 
	{
	    sprintf(errbuf,"Not able to open cellfile for [%s]", rast->answer);
	    G_fatal_error(errbuf);
	}

	size = dims[0] * dims[1];
	if((data = (float *)malloc(size  * sizeof(float)))==NULL){
	    fprintf(stderr,"malloc failed\n");
	    exit(1);
	}

	if(!bequiet->answer)
	    fprintf(stderr,"Loading %s...\r",rast->answer);

	int_buf = (int *)G_malloc (w.cols * sizeof (int));
	{	
	    int row_off;
	    int *ti;
	    float *tf;
	    for (row = 0; row < w.rows; row++) {
		
		row_off = row * w.cols;
		G_get_map_row(cellfile, int_buf, row); 
		tf = &(data[row_off]);
		ti = int_buf;
		for(col=0; col < w.cols; col++)
		    *tf++ = *ti++ * mply;

	    }
	}
	free(int_buf);
	G_close_cell(cellfile);
    }
    if(!bequiet->answer)
	fprintf(stderr,"GRASS file loaded              \n");
    
/* get range */
    Min = Max = data[0];
    for (i=1; i< size; i++){
	if (data[i] > Max) Max = data[i];
	else if (data[i] < Min) Min = data[i];
    }

    {
	if(strcmp(hdf_file->answer,"<rasterfilename>.hdf"))
	    strcpy (ofile, hdf_file->answer);
	else{
	    p = rast->answer;
	    strcpy (ofile, p);

	    /* knock off any suffix
	    if ((char*)NULL != (p = strrchr (ofile, '.'))) {
		    if (p != ofile)
			*p = '\0'; 
	    }
	    */

	    strcat(ofile,".hdf");
	}

	ret = DFSDsetdims(2,dims);
	if(ret == -1){
	    fprintf(stderr,"errror in DFSDsetdims\n");
	    exit(1);
	}
	
	ret = DFSDsetrange(&Max,&Min);
	if(ret == -1){
	    fprintf(stderr,"errror in DFSDsetrange\n");
	    exit(1);
	}
	if(!bequiet->answer)
	    fprintf(stderr,"new min = %f, new max = %f\n", Min, Max); 
    }


/* 
    DFSDsettype(0,0,0,DFO_C);
*/

    if(!bequiet->answer)
	fprintf(stderr,"Writing HDF output file: %s\n", ofile); 

    ret = DFSDputdata(ofile, 2, dims, data);
    if(ret == -1){
	fprintf(stderr,"errror in DFSDputdata\n");
	exit(1);
    }

    if(!bequiet->answer)
	fprintf(stderr,"Done.\n"); 
    
    return(1);
}



