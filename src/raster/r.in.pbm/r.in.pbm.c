
#include <stdio.h>
#include "gis.h"

static int
read_line(char *buf, int size, FILE *fp)
{
	for (;;)
	{
		if (!fgets(buf, size, fp))
			G_fatal_error("Error reading PBM file.");

		if (buf[0] != '#')
			return 0;
	}
}

int 
main (int argc, char *argv[])
{
	struct GModule *module;
	struct Option *inopt, *outopt;
	struct Flag *vflag;
	int verbose = 0;
	char *outname;
	struct Colors colors;
	FILE *infp;
	char buf[80];
	unsigned char magic;
	int nrows, ncols;
	struct Cell_head cellhd;
	int outfd;
	int bytes;
	unsigned char *line;
	CELL *cell;
	int row, col;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Converts a PBM image file to a GRASS raster map.";

	inopt = G_define_option();
	inopt->key              = "input";
	inopt->type             = TYPE_STRING;
	inopt->required 	= YES;
	inopt->description      = "Name of existing PBM file.";

	outopt = G_define_option();
	outopt->key             = "output";
	outopt->type   		= TYPE_STRING;
	outopt->required        = YES;
	outopt->gisprompt	= "new,cell,raster";
	outopt->description     = "Name of new raster map.";
	
	vflag = G_define_flag();
	vflag->key              = 'v';
	vflag->description      = "verbose.";

	if(G_parser(argc, argv))
		return 1;

	verbose = vflag->answer;

	infp = fopen(inopt->answer, "r");
	if (!infp)
		G_fatal_error("Unable to open PBM file.");

	outname = outopt->answer;

	G_init_colors(&colors);
	G_set_color(0,   0,   0,   0, &colors);
	G_set_color(1, 255, 255, 255, &colors);

	read_line(buf, sizeof(buf), infp);

	if (sscanf(buf, "P%c", &magic) != 1)
		G_fatal_error("Invalid PBM file.");

	read_line(buf, sizeof(buf), infp);

	if (sscanf(buf, "%d %d", &ncols, &nrows) != 2)
		G_fatal_error("Invalid PBM file.");

	switch (magic)
	{
	case '1':
	case '4':
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

	outfd = G_open_cell_new(outname);
	if(outfd < 0)
		G_fatal_error("Unable to open output map.");

	bytes = (ncols + 7) / 8;
	line = G_malloc(bytes);
	cell = G_allocate_cell_buf();

	for (row = 0; row < nrows; row++)
	{
		switch (magic)
		{
		case '1':
			for (col = 0; col < ncols; col++)
			{
				switch (fgetc(infp))
				{
				case '0':	cell[col] = 0;	break;
				case '1':	cell[col] = 1;	break;
				case '\r':	col--;		break;
				case '\n':	col--;		break;
				case EOF:
					G_fatal_error("Error reading PBM file.");
					break;
				default:
					G_fatal_error("Invalid PBM file.");
					break;
				}
			}
			break;
		case '4':
			if (fread(line, 1, bytes , infp) != bytes)
				G_fatal_error("Invalid PBM file.");
			for (col = 0; col < ncols; col++)
				cell[col] = (line[col/8] >> (7 - (col % 8))) & 1;
			break;
		}

		if (G_put_raster_row(outfd, cell, CELL_TYPE) < 0)
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

