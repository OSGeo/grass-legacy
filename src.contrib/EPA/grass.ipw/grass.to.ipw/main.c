
#ifndef lint
static char *SCCSid=	"SCCS version: @(#)   main.c   1.10   5/9/91";
#endif

/*
** NAME
**	grass.to.ipw -- Converts GRASS raster map layer to IPW image file
**
** SYNOPSIS
**	grass.to.ipw [-r] map=name [header=name] [units=name] \
**	                 [divisor=value] [offset=value]	
**
** DESCRIPTION
**	grass.to.ipw converts a GRASS cell file to an IPW image file.
**	It runs under GRASS and assumes that the GRASS active program window is
**	set with number of rows and columns equal to that of the target IPW
**	image file.  The output image is written to standard output.
**	A linear quantization header may be provided with -h option or stdin;
**	otherwise the linear quantization values are derived from the data
**	(the floating point maximum may be increased to preserve the precision
**	of the data).  If both the -r flag and a header file are specified,
**	the given linear quantization header will be written to the output
**	image, but the data values will be copied as raw pixel values
**	bypassing the floating point mapping.
**
** OPTIONS
**
**	Flags:
**	-r                  raw values (don't convert to linear quantization)
**
**	Parameters:
**      map=name            input GRASS cell file
**      header=name         input linear quantization header file
**      units=name          data units for annotation
**      divisor=value       divide each map data by this value
**                          default: 1.0
**      offset=value        add to each map data (prior to division)
**                          default: 0.0
** 	
**
** EXAMPLES
**
**      The following command,
** 
**               grass.to.ipw map=grasslayer divisor=10.0 offset=-100.0 \
**                         > ipwfile
** 
**      will convert a GRASS map layer called  grasslayer to an IPW
**      image file called ipwfile, subtracting 100.0 from all input
**      values then dividing the result by 10.0 before storing in the
**      IPW output file.
**
**	The following pipeline of commands,
**
**		mkbih -l 200 -s 500 -f | mklqh -m 0,0,255,1.275 -f \
**		  | grass.to.ipw -r map=rawlayer > image
**
**	creates an IPW image with 200 lines and 500 samples with linear
**	floating point mapping defined by the pairs (0,0) and (255,1.275).
**	The data values in GRASS cell file "rawlayer" are copied as raw
**	pixel values, so that a value of 200 in input map layer grasslayer
**	is copied as raw value 200 in the IPW image, and will be mapped to
**	floating point value of 1.0.
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
	int		cf_fd;		/* GRASS cell file descriptor */
	int		lqh_fd;		/* LQH file descriptor        */
	int		got_lqh;	/* LQH specified flag	      */
	int		verbose;	/* verbose flag               */
	double		divisor;	/* data divisor		      */
	double		offset;		/* data offset		      */
	char	       *mapset;		/* -> name of current mapset  */
	char		r_mapset[MAXN];	/* name of reclass mapset     */
	char		r_name[MAXN];	/* name of reclass cell file  */
	struct Cell_head window;	/* GRASS active window        */
	struct Range 	range;		/* GRASS range of data values */

	struct Option	*grass_file, *lqh, *units, *divisor_opt, *offset_opt;
	struct Flag	*raw;

	raw = G_define_flag();
   	raw->key = 'r';
   	raw->description = "raw values (don't convert to linear quantization)";

	grass_file = G_define_option();
	grass_file->key = "map";
	grass_file->type = TYPE_STRING;
	grass_file->required = YES;
	grass_file->description = "input GRASS cell file";
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

   /* Initialize GRASS library */

	G_gisinit (argv[0]);

   /* open GRASS cell file */

	mapset = G_mapset();
	cf_fd = G_open_cell_old (grass_file->answer, mapset);
	if (cf_fd < 0) {
		error ("Error opening GRASS cell file\n");
	}

   /* get window and range of values in cell file */

	G_get_set_window (&window);
	if (G_read_range (grass_file->answer, mapset, &range) < 0)
		error ("error reading GRASS range file");

   /* read cell file header/write IPW headers */

	headers (ipw_fd, (int) raw->answer, got_lqh, lqh_fd, window, range,
		units->answer, divisor, offset);

   /* convert GRASS cell file to IPW file */

	grass2ipw (ipw_fd, cf_fd, window.rows, window.cols, divisor, offset,
		   (int) raw->answer);

   /* finished */

	ipwexit (EX_OK);
}
