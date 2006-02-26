#define MAIN

#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "globals.h"

/****************************************************************************/

int main( int argc, char **argv)
{
	long i;
	int band, rows, cols;
	CELL *rowbuffer[NBANDS];
	struct Option *opt1, *opt4 ;
	struct Option *opt2, *opt5 ;
	struct Option *opt3, *opt6 ;
	struct GModule *module;

	G_gisinit(argv[0]);
	/* Set description */
	module              = G_define_module();
	module->description = ""\
	"Hue-intensity-saturation (his) to red-green-blue (rgb) raster map color transformation function.";
	
	/* Define the different options */

	opt1 = G_define_option() ;
	opt1->key        = "hue_input";
	opt1->type       = TYPE_STRING;
	opt1->required   = YES;
	opt1->description= "hue map name" ;
	opt1->gisprompt  = "old,cell,raster";


	opt2 = G_define_option() ;
	opt2->key        = "intensity_input";
	opt2->type       = TYPE_STRING;
	opt2->required   = YES;
	opt2->description= "intensity map name";
	opt2->gisprompt  = "old,cell,raster";


	opt3 = G_define_option() ;
	opt3->key        = "saturation_input";
	opt3->type       = TYPE_STRING;
	opt3->required   = YES;
	opt3->description= "saturation map name";
	opt3->gisprompt  = "old,cell,raster";

	opt4 = G_define_option() ;
	opt4->key        = "red_output";
	opt4->type       = TYPE_STRING;
	opt4->required   = YES;
	opt4->description= "output map representing the red" ;
	opt4->gisprompt  = "new,cell,raster";

	opt5 = G_define_option() ;
	opt5->key        = "green_output";
	opt5->type       = TYPE_STRING;
	opt5->required   = YES;
	opt5->description= "output map representing the green" ;
	opt5->gisprompt  = "new,cell,raster";

	opt6 = G_define_option() ;
	opt6->key        = "blue_output";
	opt6->type       = TYPE_STRING;
	opt6->required   = YES;
	opt6->description= "output map representing the blue" ;
	opt6->gisprompt  = "new,cell,raster";


	if (G_parser(argc, argv) < 0)
		exit(-1);

	strcpy(inputfiles[0], opt1->answer);
	strcpy(inputfiles[1], opt2->answer);
	strcpy(inputfiles[2], opt3->answer);
	strcpy(outputfiles[0], opt4->answer);
	strcpy(outputfiles[1], opt5->answer);
	strcpy(outputfiles[2], opt6->answer);


	/* get dimension of the image */
	rows = G_window_rows();
	cols = G_window_cols();

	openfiles(rowbuffer);
	for (i=0; i<rows; i++) {
		G_percent(i, rows, 2);
		/* read in a row from each cell map */
		for (band=0; band<NBANDS; band++) {
			if(G_get_map_row(fd_input[band], rowbuffer[band], i) < 0)
				G_fatal_error("Error while reading cell map.");
		}
		/* process this row of the map */
		his2rgb(rowbuffer, cols);
		/* write out the new row for each cell map */
		for (band=0; band<NBANDS; band++) {
			if(G_put_raster_row(fd_output[band], rowbuffer[band], CELL_TYPE) < 0)
				G_fatal_error("Error while writing new cell map.");
		}
	}

	closefiles(rowbuffer);
	exit(0);
}
