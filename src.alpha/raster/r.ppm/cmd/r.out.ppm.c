#include <stdio.h>
#include "gis.h"
main(argc,argv) char *argv[];
{
	CELL *cell;
	char *name, *mapset;
	int cf;
	int row,col;
	int nrows, ncols;
	int i;
	int have_cell;
	int n;
	char fmt[20];
	struct Colors colors, *pcolr;
	int red,green,blue;
	struct Option *inopt, *outopt;
	struct Flag *vflag;
	FILE *outfp;
	int Verbose = 0;


	pcolr = &colors;
	have_cell = 0;
	strcpy (fmt, "%ld ");

	G_gisinit (argv[0]);
	inopt = G_define_option();
	outopt = G_define_option();

	inopt->key              = "input";
	inopt->type             = TYPE_STRING;
	inopt->required 	= YES;
	inopt->gisprompt	= "old,cell,raster";
	inopt->description      = "Name of existing raster file.";

	outopt->key             = "output";
	outopt->type   		= TYPE_STRING;
	outopt->required        = YES;
	outopt->description     = "Name of new PPM file.";


	vflag = G_define_flag();
	vflag->key              = 'v';
	vflag->description      = "Verbose mode on.";

	if(G_parser(argc, argv))
		exit(-1);

	Verbose = vflag->answer; 
	name = inopt->answer;
	mapset = G_find_cell (name,"");
	if (mapset == NULL)
	{
		fprintf (stderr, "%s: %s: cellfile not found\n", argv[0], name);
		exit(1);
	}

	cf = G_open_cell_old (name, mapset);
	if (cf < 0)
		exit(1);
	if ((outfp = fopen(outopt->answer,"w")) == NULL)
		G_fatal_error("Can't open output file.");

	cell = G_allocate_cell_buf();
	G_init_colors(pcolr);
	G_read_colors(name, mapset, pcolr);

	nrows = G_window_rows();
	ncols = G_window_cols();

	fprintf (outfp, "P6\n# created from r.out.ppm\n");
	fprintf(outfp, "%d %d\n%d\n", ncols, nrows, 255);
	if (Verbose) fprintf(stderr, "Percent Complete: ");
	for (row = 0; row < nrows; row++)
	{
		if (G_get_map_row (cf, cell, row) < 0)
			exit(1);
		if(Verbose) G_percent(row, nrows, 5);
		for (col = 0; col < ncols; col++){
			/*	    printf (fmt, (long)cell[col]);*/
			G_get_color(cell[col], &red, &green, &blue, pcolr);
			/*	    printf("%d %d %d ", (long)cell[col],red, green, blue);*/
			fputc(red, outfp);
			fputc(green, outfp);
			fputc(blue, outfp);
		}
	}
	if(Verbose) G_percent(nrows,nrows, 5);
	exit(0);
}
