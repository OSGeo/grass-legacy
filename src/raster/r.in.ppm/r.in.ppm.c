
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "gis.h"

static int
read_line(char *buf, int size, FILE *fp)
{
	for (;;)
	{
		if (!fgets(buf, size, fp))
			G_fatal_error("Error reading PPM file.");

		if (buf[0] != '#')
			return 0;
	}
}

static int
do_command(char *fmt, ...)
{
	char buff[1024];
	va_list va;

	va_start(va, fmt);
	vsprintf(buff, fmt, va);
	va_end(va);

	return system(buff);
}

int 
main(int argc, char *argv[])
{
	static const char color_codes[] = {'r', 'g', 'b'};
	static CELL i0 = 0, i1 = 255;
	static FCELL f0 = 0.0f, f1 = 1.0f;

	struct GModule *module;
	struct Option *inopt, *outopt;
	struct Flag *vflag, *fflag, *bflag, *cflag;
	int verbose, isfp, bands, composite;
	char *outname;
	struct Colors colors;
	FILE *infp;
	char buf[80];
	unsigned char magic;
	int maxval;
	int nrows, ncols;
	struct Cell_head cellhd, save;
	struct band {
		char outname[100];
		int outfd;
		void *xcell;
	} B[3];
	unsigned char *line;
	int row, col;
	int i;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Converts a PPM image file to GRASS raster map(s).";

	inopt = G_define_option();
	inopt->key              = "input";
	inopt->type             = TYPE_STRING;
	inopt->required 	= YES;
	inopt->description      = "Name of existing PPM file.";

	outopt = G_define_option();
	outopt->key             = "output";
	outopt->type   		= TYPE_STRING;
	outopt->required        = YES;
	outopt->gisprompt	= "new,cell,raster";
	outopt->description     = "Name of new raster map(s).";

	vflag = G_define_flag();
	vflag->key              = 'v';
	vflag->description      = "Verbose.";

	fflag = G_define_flag();
	fflag->key              = 'f';
	fflag->description      = "Create floating-point maps (0.0 - 1.0).";

	bflag = G_define_flag();
	bflag->key              = 'b';
	bflag->description      = "Create separate red/green/blue maps.";

	cflag = G_define_flag();
	cflag->key              = 'c';
	cflag->description      = "Create composite color map.";

	if(G_parser(argc, argv))
		return 1;

	verbose   = vflag->answer;
	isfp      = fflag->answer;
	bands     = bflag->answer;
	composite = cflag->answer;

	if (!bands && !composite)
		G_fatal_error("Either -b or -c must be specified");

	infp = fopen(inopt->answer, "r");
	if (!infp)
		G_fatal_error("Unable to open PPM file.");

	outname = outopt->answer;

	for (i = 0; i < 3; i++)
	{
		sprintf(B[i].outname, "%s.%c", outname, color_codes[i]);
		if (!bands && G_find_file("cell", B[i].outname, ""))
			G_fatal_error("Refusing to overwrite <%s.[rgb]> maps\n"
				      "Delete or rename them, or use '-b'",
				      outname);
	}

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
		G_fatal_error("Invalid PPM file.");

	read_line(buf, sizeof(buf), infp);

	if (sscanf(buf, "%d %d", &ncols, &nrows) != 2)
		G_fatal_error("Invalid PPM file.");

	read_line(buf, sizeof(buf), infp);

	if (sscanf(buf, "%d", &maxval) != 1)
		G_fatal_error("Invalid PPM file.");

	switch (magic)
	{
	case '3':
	case '6':
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

	G_get_window(&save);

	G_set_window(&cellhd);
	G_set_cell_format(0);

	line = G_malloc(ncols * 3);

	for (i = 0; i < 3; i++)
	{
		struct band *b = &B[i];

		b->outfd = isfp
			? G_open_fp_cell_new(b->outname)
			: G_open_cell_new(b->outname);
		if(b->outfd < 0)
			G_fatal_error("Unable to open output map.");

		b->xcell = isfp
			? (void *) G_allocate_f_raster_buf()
			: (void *) G_allocate_c_raster_buf();
	}

	for (row = 0; row < nrows; row++)
	{
		switch (magic)
		{
		case '3':
			for (col = 0; col < ncols; col++)
			{
				int r, g, b;
				if (fscanf(infp, "%d %d %d", &r, &g, &b) != 3)
					G_fatal_error("Invalid PPM file.");
				line[col*3+0] = (unsigned char) r;
				line[col*3+1] = (unsigned char) g;
				line[col*3+2] = (unsigned char) b;
			}
			break;
		case '6':
			if (fread(line, 3, ncols, infp) != ncols)
				G_fatal_error("Invalid PPM file.");
			break;
		}

		if (isfp)
			for (col = 0; col < ncols; col++)
				for (i = 0; i < 3; i++)
					((FCELL *) B[i].xcell)[col] =
						(FCELL) line[col*3+i] / maxval;
		else
			for (col = 0; col < ncols; col++)
				for (i = 0; i < 3; i++)
					((CELL *) B[i].xcell)[col] =
						line[col*3+i] * 255 / maxval;

		for (i = 0; i < 3; i++)
			if (G_put_raster_row(B[i].outfd, B[i].xcell,
					     isfp ? FCELL_TYPE : CELL_TYPE) < 0)
				G_fatal_error("Error writing output map.");

		if (verbose)
			G_percent(row, nrows, 2);
	}

	for (i = 0; i < 3; i++)
		G_close_cell(B[i].outfd);

	fclose(infp);

	if (verbose)
		fprintf(stderr, "Writing color tables\n");

	for (i = 0; i < 3; i++)
		if (G_write_colors(B[i].outname, G_mapset(), &colors) < 0)
			G_fatal_error("Unable to write color table.");

	if (!composite)
		return 0;

	if (verbose)
		fprintf(stderr, "Generating composite layer\n");

	G_put_window(&cellhd);
	do_command("r.composite -o 'r=%s' 'g=%s' 'b=%s' 'out=%s'",
		   B[0].outname, B[1].outname, B[2].outname, outname);
	G_put_window(&save);

	if (bands)
		return 0;

	do_command("g.remove 'rast=%s,%s,%s' >/dev/null",
		   B[0].outname, B[1].outname, B[2].outname);

	return 0;
}

