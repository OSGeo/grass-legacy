#include <stdio.h>
#include "gis.h"

#define COLORMAX 255
#define COLORMIN 0

typedef unsigned short u_short;


main(argc, argv)
int         argc;
char       *argv[];
{
	FILE       *inpfp;
	char *name, *mapset;
	int Verbose;
	struct Cell_head cellhd;
	struct Colors colors;
	typedef struct{
		long 	stuff_size;
		char	lut_file[256];
		char	filter_file[256];
		u_short	y_start;
		u_short	y_end;
		u_short	x_start;
		u_short	x_end;
		u_short data_packing;
		u_short filter_range;
		u_short scan_density;
		u_short output_density;
		u_short total_lines;
		u_short swath;
		char	data_file1[256];
		char	data_file2[256];
		char	data_file3[256];
		char	data_file4[256];
		char	span_file[256];
	} HDR;
	HDR hdr;
	u_short dummy;
	float factor;

	struct Option *inopt, *outopt;
	struct Flag *vflag;

	G_gisinit(argv[0]);
	inopt = G_define_option();
	inopt->key		= "cmd";
	inopt->type		= TYPE_STRING;
	inopt->required		= YES;
	inopt->gisprompt	= "mapset,cell,CMD";
	inopt->description	= "Name of Tangent CMD file.";

	outopt = G_define_option();
	outopt->key		= "data";
	outopt->type		= TYPE_STRING;
	outopt->required	= YES;
	outopt->gisprompt	= "mapset,cell,DATA";
	outopt->description	= "Name of Tangent DATA file.";

	vflag = G_define_flag();
	vflag->key		= 'v';
	vflag->description	= "Verbose mode on.";

	if(G_parser(argc, argv))
		exit(-1);

	Verbose = (vflag->answer);

	name = inopt->answer;
	mapset = G_mapset();
	if ((inpfp = G_fopen_old("cell", name, mapset)) == NULL)
		G_fatal_error("Can't open input file.");
	G_init_colors(&colors);
	G_make_grey_scale_colors(&colors, COLORMIN, COLORMAX);
	if (Verbose)
		fprintf(stderr, "Reading %s...\n", name);
	fread( &hdr, sizeof(hdr), 1, inpfp);
#ifndef ATT_386
	swab(&hdr.y_start, &dummy, 2);
	hdr.y_start = dummy;
	swab(&hdr.y_end, &dummy, 2);
	hdr.y_end = dummy;
	swab(&hdr.x_start, &dummy, 2);
	hdr.x_start = dummy;
	swab(&hdr.x_end, &dummy, 2);
	hdr.x_end = dummy;
	swab(&hdr.output_density, &dummy, 2);
	hdr.output_density = dummy;
#endif
	if (Verbose)
		fprintf(stderr,"y_start=%d y_end=%d x_start=%d x_end=%d \n",
		    hdr.y_start, hdr.y_end, hdr.x_start, hdr.x_end);
	factor = (float)hdr.output_density / 1000.0;
	if (Verbose)
		fprintf(stderr,"output_density=%d, calculated factor=%f\n",hdr.output_density, factor);

	cellhd.zone = G_zone();
	cellhd.proj = G_projection();
	cellhd.rows = cellhd.north = (int)((float)(hdr.y_end - hdr.y_start) * factor);
	cellhd.cols = cellhd.east = (int)((float)(hdr.x_end - hdr.x_start) * factor);
	cellhd.south = cellhd.west = 0.0;
	cellhd.ns_res = cellhd.ew_res = 1;
	cellhd.format = 0;
	cellhd.compressed = 0;
	if (Verbose)
		fprintf(stderr, "Writing cellhd.\n");
	if(G_put_cellhd(outopt->answer, cellhd) < 0 )
		G_fatal_error("Could not write cellhd file.");
	if (Verbose)
		fprintf(stderr, "Writing greyscale color table.\n");
	if(G_write_colors(outopt->answer, mapset, &colors) < 0 )
		G_fatal_error("Could not write color file.");
	if (Verbose)
		fprintf(stderr, "done.\n");

	exit(0);
}
