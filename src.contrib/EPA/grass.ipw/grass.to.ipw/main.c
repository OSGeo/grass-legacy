
#ifndef lint
static char *SCCSid=	"SCCS version: @(#)   main.c   1.12   9/5/91";
#endif

/*
** NAME
**	grass.to.ipw -- Converts GRASS raster map layer to IPW image file
**
** SYNOPSIS
**	grass.to.ipw
**	grass.to.ipw help
**	grass.to.ipw [-r] [-o] map=name [header=name] [units=name] \
**	                 [divisor=value] [offset=value]	
**
** DESCRIPTION
**	grass.to.ipw converts a GRASS raster file to an IPW image file.
**
**	A linear quantization header may be provided with -h option or stdin;
**	otherwise the number of bits per pixel and floating point map
**	are derived from the min and max cell values (the floating point
**	map maximum may be increased to preserve the precision of the data).
**
**	If both the -r (raw mode) flag and a header file are specified,
**	the given linear quantization header will be written to the output
**	image, but the data values will be copied as raw pixel values
**	bypassing the floating point mapping (see examples below).
**
**	In GRASS, the geographic region (window) specifies the north,
**	south, east and west edges of the raster map.  The data values are
**	assumed to be located at the cell centers so that the data value
**	for the north-westernmost pixel (1,1) is actually located at
**	northing NORTH-NSRES/2 and easting WEST+EWRES/2	(where NORTH
**	is the northern edge of the file, WEST is the western edge of
**	the file and NSRES and EWRES are the resolutions in the
**	N/S and E/W directions, respectively).  In IPW, the image origin
**	(beginning line and sample coordinates) are assumed to be at the
**	center of pixel (0,0).  Unless the -o (override) option is set,
**	the output image GEO header will be set so that:
**		bline = NORTH+NSRES/2
**		bsamp = WEST+EWRES/2.
**		dline = -NSRES
**		dsamp = EWRES
**	If the -o option is specified, bline will be set to NORTH and
**	bsamp will be set to west.
**
**	The output image is written to standard output.
**
** OPTIONS
**
**	Flags:
**	-r                  raw values (don't convert to linear quantization)
**	-o		    use the GRASS NORTH and WEST values for the
**			    GEO origin
**			    default: origin is offset by half a pixel in
**			    each direction (see discussion above)
**
**	Parameters:
**	map=name            input GRASS raster file
**	header=name         input linear quantization header file
**	units=name          data units for annotation
**	divisor=value       divide each map data by this value
**			    default: 1.0
**	offset=value	    add to each map data (prior to division)
**			    default: 0.0
** 	
**
** EXAMPLES
**
**	The following command,
** 
**		grass.to.ipw map=grasslayer divisor=10.0 offset=-100.0 \
**		       > ipwfile
** 
**	will convert a GRASS map layer called  grasslayer to an IPW
**	image file called ipwfile, subtracting 100.0 from all input
**	values then dividing the result by 10.0 before storing in the
**	IPW output file.
**
**	The following pipeline of commands,
**
**		mkbih -l 200 -s 500 -f | mklqh -m 0,0,255,1.275 -f \
**		  | grass.to.ipw -r map=rawlayer > image
**
**	creates an IPW image with 200 lines and 500 samples with linear
**	floating point mapping defined by the pairs (0,0) and (255,1.275).
**	The data values in GRASS raster file "rawlayer" are copied as raw
**	pixel values, so that a value of 200 in input map layer grasslayer
**	is copied as raw value 200 in the IPW image, and will be mapped to
**	floating point value of 1.0.
**
**	To convert several GRASS raster files into a multi-band IPW image:
**		grass.to.ipw map=raster.1 > ipw.1
**		    ...
**		grass.to.ipw map=raster.N > ipw.N
**		mux ipw.1 ... ipw.N > image
** 
**
** FILES
**
** DIAGNOSTICS
**
** RESTRICTIONS
**
** FUTURE DIRECTIONS
**
** HISTORY
**	1/5/90	Written by Kelly Longley, Oregon State University, USEPA ERL-C
**	4/9/91	Modified to handle all GRASS geographic projections, by
**		K. Longley, OSU, EPA ERL-C
**	4/16/91 Added multiplier (-c) and offset (-s) options, by K. Longley
**		OSU, EPA ERL-C
**	4/22/91 Substituted GRASS command line parsing, by G. Koerper, OSU, EPA
**		ERL-C
**	4/29/91 A header file and raw (-r) can now both be specified - the
**	        header will be writted, and the pixels will be copied as raw
**		pixels, by K. Longley, OSU, EPA ERL-C
**	9/4/91  Changed so that raster file need not be in current mapset and
**		current window need not be the same as the raster file region;
**		fixed bug (BIH write) when header specified;
**		added half-pixel offset for GEO origin and -o option to
**		override, by K. Longley, OSU, EPA ERL-C
*/

#include "gis.h"
#include "ipw.h"

#define MAXN 50		/* max length of pathname for GRASS files */

extern char	**_argv; /* not IPW kosher, but required to initialize _argv */

main (argc, argv)
	int argc;
	char *argv[];
{
	int		ipw_fd;		/* IPW image file descriptor  */
	int		cf_fd;		/* GRASS raster file desc     */
	int		lqh_fd;		/* LQH file descriptor        */
	int		got_lqh;	/* LQH specified flag	      */
	int		verbose;	/* verbose flag               */
	double		divisor;	/* data divisor		      */
	double		offset;		/* data offset		      */
	char	       *mapset;		/* -> name of mapset          */
	struct Cell_head cellhd;	/* GRASS cell header          */
	struct Range 	range;		/* GRASS range of data values */

	struct Option	*grass_file, *lqh, *units, *divisor_opt, *offset_opt;
	struct Flag	*raw;
	struct Flag	*override;

   /* Initialize GRASS library */

	G_gisinit (argv[0]);

	raw = G_define_flag();
   	raw->key = 'r';
   	raw->description = "raw values (don't convert to linear quantization)";

	override = G_define_flag();
   	override->key = 'o';
   	override->description = "don't offset GEO origin by half-pixel - use GRASS raster file values";

	grass_file = G_define_option();
	grass_file->key = "map";
	grass_file->type = TYPE_STRING;
	grass_file->required = YES;
	grass_file->description = "input GRASS raster file";
	grass_file->gisprompt = "old,cell,raster";

        lqh = G_define_option();
        lqh->key = "header";
        lqh->type = TYPE_STRING;
        lqh->required = NO;
        lqh->description = "input linear quantization header file";

        units = G_define_option();
        units->key = "units";
        units->type = TYPE_STRING;
        units->required = NO;
        units->description = "data units for annotation";

        divisor_opt = G_define_option();
        divisor_opt->key = "divisor";
        divisor_opt->type = TYPE_DOUBLE;
        divisor_opt->required = NO;
        divisor_opt->description = "divide each map data by this value";
	divisor_opt->answer = "1.0";

        offset_opt = G_define_option();
        offset_opt->key = "offset";
        offset_opt->type = TYPE_DOUBLE;
        offset_opt->required = NO;
        offset_opt->description = "add to each map data (prior to division)";
	offset_opt->answer = "0.0";


   /* get options, flags */

	if (G_parser(argc,argv))
            exit(1);

   /* set IPW global var used by cmdline() (called by bihwrite in headers.c) */

	_argv = argv;

   /* access input LQH, if there is one, and set flag */

	if (lqh->answer) {
		got_lqh = YES;
		lqh_fd = uropen (lqh->answer);
		if (lqh_fd == ERROR) {
			error("can't open \"%s\"", lqh->answer);
		}
	} else { /* check stdin - if not tty assume it's an LQH */
		lqh_fd = ustdin();
		got_lqh = !isatty(lqh_fd);
	}

   /* get data divisor and/or offset */

	if (sscanf (divisor_opt->answer, "%lf", &divisor) != 1 ||
	 divisor == 0) {
		fprintf (stderr, "Illegal divisor: %s\n", divisor_opt->answer);
                G_usage();
                exit(1);
	}

	if (sscanf (offset_opt->answer, "%lf", &offset) != 1) {
		fprintf (stderr, "Illegal offset: %s\n", offset_opt->answer);
                G_usage();
                exit(1);
	}

   /* access output file */

	ipw_fd = ustdout();
	no_tty(ipw_fd);


   /* open GRASS raster file */

	mapset = G_find_file ("cell", grass_file->answer, "");
	if (!mapset) {
		error ("<%s> not found", grass_file->answer);
	}

	cf_fd = G_open_cell_old (grass_file->answer, mapset);
	if (cf_fd < 0) {
		error ("Error opening GRASS raster file\n");
	}

   /* get cell header and set active program window */

	if (G_get_cellhd (grass_file->answer, mapset, &cellhd) == -1) {
		error ("Error reading cell header");
	}
	G_set_window (&cellhd);

   /* get cell header and range of values in raster file */
	if (G_read_range (grass_file->answer, mapset, &range) < 0)
		error ("error reading GRASS range file");

   /* read raster file header/write IPW headers */

	headers (ipw_fd, (int) raw->answer, (int) override->answer, got_lqh,
		 lqh_fd, cellhd, range, units->answer, divisor, offset);

   /* convert GRASS raster file to IPW file */

	grass2ipw (ipw_fd, cf_fd, cellhd.rows, cellhd.cols, divisor, offset,
		   (int) raw->answer);

   /* finished */

	ipwexit (EX_OK);
}
