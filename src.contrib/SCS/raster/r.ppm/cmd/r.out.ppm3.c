#include "gis.h"
main(argc,argv) char *argv[];
{
	CELL *cellred,*cellgrn,*cellblu;
	char *namered,*namegrn,*nameblu, *mapset;
	int cfred,cfgrn,cfblu;
	int row,col;
	int nrows, ncols;
	int i;
	int n;
	struct Colors colorsred,colorsgrn,colorsblu,
		*pcolrred,*pcolrgrn,*pcolrblu;
	int red,green,blue,dummy;
	struct Option *redopt, *grnopt, *bluopt, *outopt;
	struct Flag *vflag;
	int verbose = 0;
	FILE *outfp;

	pcolrred = &colorsred;
	pcolrgrn = &colorsgrn;
	pcolrblu = &colorsblu;

	G_gisinit (argv[0]);

	redopt = G_define_option();
	grnopt = G_define_option();
	bluopt = G_define_option();
	outopt = G_define_option();

	redopt->key              = "red";
	redopt->type             = TYPE_STRING;
	redopt->required 	= YES;
	redopt->gisprompt	= "old,cell,raster";
	redopt->description      = "Name of existing RED raster file.";

	grnopt->key              = "grn";
	grnopt->type             = TYPE_STRING;
	grnopt->required 	= YES;
	grnopt->gisprompt	= "old,cell,raster";
	grnopt->description      = "Name of existing GREEN raster file.";

	bluopt->key              = "blu";
	bluopt->type             = TYPE_STRING;
	bluopt->required 	= YES;
	bluopt->gisprompt	= "old,cell,raster";
	bluopt->description      = "Name of existing BLUE raster file.";

	outopt->key             = "output";
	outopt->type   		= TYPE_STRING;
	outopt->required        = YES;
	outopt->description     = "Name of new PPM file.";

	vflag = G_define_flag();
	vflag->key              = 'v';
	vflag->description      = "Verbose mode on.";

	if(G_parser(argc, argv))
		exit(-1);

	verbose = vflag->answer; 
	namered = redopt->answer;
	namegrn = grnopt->answer;
	nameblu = bluopt->answer;

	if((mapset = G_find_cell (namered,"")) == NULL){
		fprintf (stderr, "%s: %s: cellfile not found\n", argv[0], namered);
		exit(1);
	}

	cfred = G_open_cell_old (namered, mapset);
	if (cfred < 0)
		exit(1);
	cfgrn = G_open_cell_old (namegrn, mapset);
	if (cfgrn < 0)
		exit(1);
	cfblu = G_open_cell_old (nameblu, mapset);
	if (cfblu < 0)
		exit(1);
	if((outfp=fopen(outopt->answer,"w")) == NULL)
		G_fatal_error("Can't open new PPM file.");

	cellred = G_allocate_cell_buf();
	cellgrn = G_allocate_cell_buf();
	cellblu = G_allocate_cell_buf();
	G_init_colors(pcolrred);
	G_init_colors(pcolrgrn);
	G_init_colors(pcolrblu);
	G_read_colors(namered, mapset, pcolrred);
	G_read_colors(namegrn, mapset, pcolrgrn);
	G_read_colors(nameblu, mapset, pcolrblu);

	nrows = G_window_rows();
	ncols = G_window_cols();

	fprintf (outfp,"P6\n# created from celltoppm\n");
	fprintf(outfp,"%d\n%d\n%d\n", ncols, nrows, 255);
	if (verbose) printf("Complete ....");
	for (row = 0; row < nrows; row++)
	{
		if (verbose) G_percent(row, nrows, 5);
		if (G_get_map_row (cfred, cellred, row) < 0)
			exit(1);
		if (G_get_map_row (cfgrn, cellgrn, row) < 0)
			exit(1);
		if (G_get_map_row (cfblu, cellblu, row) < 0)
			exit(1);
		for (col = 0; col < ncols; col++){
			G_get_color(cellred[col], &red, &dummy, &dummy, pcolrred);
			G_get_color(cellgrn[col], &green, &dummy,&dummy, pcolrgrn);
			G_get_color(cellblu[col], &blue,&dummy, &dummy, pcolrblu);
			/*	    printf("%d %d %d ", (long)cell[col],red, green, blue);*/
			fputc(red, outfp);
			fputc(green, outfp);
			fputc(blue, outfp);
		}
	}
	if (verbose) G_percent(nrows, nrows, 5);
	exit(0);
}
