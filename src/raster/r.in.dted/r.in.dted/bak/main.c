#include <stdio.h>
#include "gis.h"
#include <ctype.h>
#include "dtedgis.h"

/* For importing DTED I & DTED II into GRASS Lat-Lon location */


main(argc,argv) 
int argc;
char *argv[];
{
	FILE *infp;
	int outfd;
	struct Cell_head cellhd;
	struct Option *inopt, *outopt;
	struct Flag *qflag;
	char *name, *mapset;
	char ebuf[256];
	int nrows, ncols;
	int quiet=0;

	G_gisinit (argv[0]);
	inopt = G_define_option();
	outopt = G_define_option();

	inopt->key              = "input";
	inopt->type             = TYPE_STRING;
	inopt->required 	= YES;
	inopt->description      = "Name of existing DTED file.";

	outopt->key             = "output";
	outopt->type   		= TYPE_STRING;
	outopt->required        = YES;
	outopt->gisprompt	= "new,cell,raster";
	outopt->description     = "Name of new raster file.";

	qflag = G_define_flag();
	qflag->key              = 'q';
	qflag->description      = "Quiet mode on.";

	if(G_parser(argc, argv))
		exit(-1);

	/* check to make sure we're in latlon location */

	quiet = qflag->answer;

	if((infp = fopen(inopt->answer, "r")) == NULL)
		G_fatal_error("Can't open DTED file for read.");
	name = outopt->answer;

	if(0 > get_header(infp, &cellhd)){
	    sprintf(ebuf,"Unable to read header for %s\n", inopt->answer);
	    G_fatal_error(ebuf);
	}
	G_set_window(&cellhd);
	nrows = cellhd.rows;
	ncols = cellhd.cols;

	/* also use header info to write a title & history */

	if((outfd = G_open_cell_new (name)) < 0)
		G_fatal_error("Can't open new raster file.");

        if(0 > do_read_write(infp, outfd, nrows, ncols, quiet, ebuf))
	    G_fatal_error(ebuf);

	G_close_cell(outfd);
	exit(0);
}

