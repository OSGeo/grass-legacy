
#include <stdio.h>
#include "gis.h"

static int
read_line(char *buf, int size, FILE *fp)
{
	for (;;)
	{
		if (!fgets(buf, size, fp))
			G_fatal_error("Error reading PGM file.");

		if (buf[0] != '#')
			return 0;
	}
}

int 
main (int argc, char *argv[])
{
	static CELL i0 = 0, i1 = 255;
	static FCELL f0 = 0.0f, f1 = 1.0f;

	struct GModule *module;
	struct Option *inopt, *outopt;
	struct Flag *vflag, *fflag;
	int verbose, isfp;
	char *outname;
	struct Colors colors;
	FILE *infp;
	char buf[80];
	int magic;
	int maxval;
	int nrows, ncols;
	struct Cell_head cellhd;
	int outfd;
	unsigned char *line;
	void *xcell;
	int row, col;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Converts a PGM image file to a GRASS raster map.";

	inopt = G_define_option();
	inopt->key              = "input";
	inopt->type             = TYPE_STRING;
	inopt->required 	= YES;
	inopt->description      = "Name of existing PGM file.";

	outopt = G_define_option();
	outopt->key             = "output";
	outopt->type   		= TYPE_STRING;
	outopt->required        = YES;
	outopt->gisprompt	= "new,cell,raster";
	outopt->description     = "Name of new raster map.";
	
	vflag = G_define_flag();
	vflag->key              = 'v';
	vflag->description      = "Verbose.";
	
	fflag = G_define_flag();
	fflag->key              = 'f';
	fflag->description      = "Create floating-point map (0.0 - 1.0).";

	if(G_parser(argc, argv))
		return 1;

	verbose = vflag->answer;
	isfp = fflag->answer;

	infp = fopen(inopt->answer, "r");
	if (!infp)
		G_fatal_error("Unable to open PGM file.");

	outname = outopt->answer;

	G_init_colors(&colors);
	if (isfp)
		G_add_f_raster_color_rule(&f0,   0,   0,   0,
					  &f1, 255, 255, 255,
					  &colors);
	else
		G_add_c_raster_color_rule(&i0,   0,   0,   0,
					  &i1, 255, 255, 255,
					  &colors);

	read_line(buf, sizeof(buf), infp);

	if (sscanf(buf, "P%c", &magic) != 1)
		G_fatal_error("Invalid PGM file.");

	read_line(buf, sizeof(buf), infp);

	if (sscanf(buf, "%d %d", &ncols, &nrows) != 2)
		G_fatal_error("Invalid PGM file.");

	read_line(buf, sizeof(buf), infp);

	if (sscanf(buf, "%d", &maxval) != 1)
		G_fatal_error("Invalid PGM file.");

	switch (magic)
	{
	case '2':
	case '5':
		break;
	default:
		G_fatal_error("Invalid magic number: 'P%c'.", magic);
		break;
	}

	cellhd.rows = nrows;
	cellhd.cols = ncols;
	cellhd.proj = G_projection();
	cellhd.zone = G_zone();
	cellhd.ew_res = 1.0;
	cellhd.ns_res = 1.0;
	cellhd.south = 0.0;
	cellhd.north = (double) nrows;
	cellhd.west = 0.0;
	cellhd.east = (double) ncols;

	G_set_window(&cellhd);
	G_set_cell_format(0);

	outfd = isfp
		? G_open_fp_cell_new(outname)
		: G_open_cell_new(outname);
	if(outfd < 0)
		G_fatal_error("Unable to open output map.");

	line = G_malloc(ncols);

	xcell = isfp
		? (void *) G_allocate_f_raster_buf()
		: (void *) G_allocate_c_raster_buf();

	for (row = 0; row < nrows; row++)
	{
		switch (magic)
		{
		case '2':
			for (col = 0; col < ncols; col++)
			{
				int v;
				if (fscanf(infp, "%d", &v) != 1)
					G_fatal_error("Invalid PGM file.");
				line[col] = (unsigned char) v;
			}
			break;
		case '5':
			if (fread(line, 1, ncols, infp) != ncols)
				G_fatal_error("Invalid PGM file.");
			break;
		}

		if (isfp)
			for (col = 0; col < ncols; col++)
				((FCELL *) xcell)[col] = (FCELL) line[col] / maxval;
		else
			for (col = 0; col < ncols; col++)
				((CELL *) xcell)[col] = line[col] * 255 / maxval;

		if (G_put_raster_row(outfd, xcell, isfp ? FCELL_TYPE : CELL_TYPE) < 0)
			G_fatal_error("Error writing output map.");

		if (verbose)
			G_percent(row, nrows, 2);
	}

	G_close_cell(outfd);

	fclose(infp);

	if (verbose)
		fprintf(stderr, "Writing color table\n");

	if (G_write_colors(outname, G_mapset(), &colors) < 0)
		G_fatal_error("Unable to write color table.");

	return 0;
}

