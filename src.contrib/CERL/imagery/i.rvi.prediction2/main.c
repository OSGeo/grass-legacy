#define MAIN

#include "gis.h"
#include <string.h>
#include "globals.h"

/******************************************
This programs calculate ground feature, e.g. plant cover, by remote sensing
	data using a given regression model
The input imagery is assumed with three optical bands: visible band1 and band2
	another infrared band3
			************************************************/

int 
main (int argc, char *argv[])
{
	long i;
	int band, rows, cols;
	int result;
	double amodel[7];   /* rvi model coefficients */
	FILE *fdmodel;
	char buf[512];
	char *mapset, tempstr[100];
	CELL *rowbuffer[NBANDS];
	struct Option *opt1, *opt3, *opt4 ;
	struct Option *opt2, *opt5 ;
	struct GModule *module;

	G_gisinit(argv[0]);
	
	/* Set description */
	module              = G_define_module();
	module->description = ""\
	"Calculates ground features, e.g. plant "
	"cover, by remote sensing data using a given regression model";

	/* Define the different options */

	opt1 = G_define_option() ;
	opt1->key        = "model";
	opt1->type       = TYPE_STRING;
	opt1->required   = YES;
	opt1->description= "rvi regression model input file" ;

	opt2 = G_define_option() ;
	opt2->key        = "band1";
	opt2->type       = TYPE_STRING;
	opt2->required   = YES;
	opt2->description= "visible band1 input file" ;

	opt3 = G_define_option() ;
	opt3->key        = "band2";
	opt3->type       = TYPE_STRING;
	opt3->required   = YES;
	opt3->description= "visible band2 input file";

	opt4 = G_define_option() ;
	opt4->key        = "band3";
	opt4->type       = TYPE_STRING;
	opt4->required   = YES;
	opt4->description= "infrared band3 input file";

	opt5 = G_define_option() ;
	opt5->key        = "output";
	opt5->type       = TYPE_STRING;
	opt5->required   = YES;
	opt5->description= "output ground feature calculated" ;

	if (G_parser(argc, argv) < 0)
		exit(-1);

	strcpy(modelfile, opt1->answer);
	strcpy(inputfiles[0], opt2->answer);
	strcpy(inputfiles[1], opt3->answer);
	strcpy(inputfiles[2], opt4->answer);
	strcpy(outputfiles, opt5->answer);
						/* get model vector */
	fdmodel = fopen (modelfile, "w");
	if (fdmodel == NULL)
	{
                sprintf (buf, "%s - not found\n", modelfile);
                G_fatal_error (buf);
                exit(1);
        }
	for (i=0; i<7; i++)
	{
		fscanf (fdmodel, "%1lf", amodel+i);
	}
						/* get dimension of the image */
	rows = G_window_rows();
	cols = G_window_cols();
	fprintf (stdout,"total rows=%d\n",rows);
	fprintf (stdout,"total columns=%d\n",cols);
						/* get imagery files */

    for (band=0; band<NBANDS; band++) {
    if (G_legal_filename(inputfiles[band]) < 0) {
      fprintf (stderr, "\nERROR: <%s> -- illegal result signature file name\n",
		inputfiles[band]);
      G_usage();
      exit(1);
    }

   if ((mapset = G_find_cell(inputfiles[band], "")) == NULL) {
      sprintf(tempstr, "Unable to find input cell map <%s>.",
              inputfiles[band]);
      G_fatal_error(tempstr);
    }
    if ((fd_input[band] = G_open_cell_old(inputfiles[band],mapset)) < 0)
      G_fatal_error("Error in opening input file");
  }

 						/*  open output files 
						*/
    if ((fd_output = G_open_cell_new(outputfiles)) < 0)
      G_fatal_error("Error in opening output file");

						/* allocate the cell row buffer 
						*/
  for (band=0; band<NBANDS; band++) {
    if ((rowbuffer[band]=G_allocate_cell_buf()) == NULL)
      G_fatal_error("Unable to allocate the input row buffer");
  }


	for (i=0; i<rows; i++) {
					/* read in a row from each cell map */
		for (band=0; band<NBANDS; band++) {
		if(G_get_map_row(fd_input[band], rowbuffer[band], i) < 0)
				G_fatal_error("Error while reading cell map.");
		}
			/* process this row of the map */
		fprintf (stdout,"processing row: %ld/%d\n", i+1, rows);
		model(amodel, rowbuffer, cols);
			/* write out the new row for each cell map */
		if(G_put_raster_row(fd_output, rowbuffer[0], CELL_TYPE) < 0)
			G_fatal_error("Error while writing new cell map.");
	}

	closefiles(rowbuffer);
	exit(0);
}
