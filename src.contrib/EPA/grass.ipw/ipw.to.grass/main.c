
#ifndef lint
static char *SCCSid=	"SCCS version: @(#)   main.c   1.12   9/5/91";
#endif

/*
** NAME
**	ipw.to.grass - Converts IPW image file to GRASS raster map layer
** 
** SYNOPSIS
**	ipw.to.grass
**	ipw.to.grass help
**	ipw.to.grass [-o] [ipw=name] map=name [mask=name] [multiplier=value] \
**		[offset=value]
** 
** DESCRIPTION
** 
**	ipw.to.grass converts a single-band image file in IPW (Image
**	Processing Workbench) format to a GRASS raster map layer.
**	If an IPW image is not specified, standard input will be
**	used.	If an IPW image is specified as a mask, all cells in the 
**	GRASS map layer corresponding to the zero ("no data") values
**	in the mask will be set to zero, regardless of the value
**	in the IPW input image.
**
**	Since GRASS cell values are integers, IPW image floating
**	point values will be truncated to integer values, i.e.
**	29.2 and 29.9 will both be set to 29 in the resulting
**	GRASS cell file.  A multiplier and/or offset may be
**	specified to convert the input IPW floating point pixel
**	values to the GRASS integer values.  If both a multiplier
**	and offset are specified, the data are first multiplied
**	by the multiplier, then the offset will be added.
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
**	the geographic region in the GRASS cell header will be set so that:
**		NORTH = bline + dline/2
**		SOUTH = NORTH - nlines * dline
**		WEST = bsamp - dsamp/2
**		EAST = WEST + nsamps * dsamp
**		NSRES = -dline
**		EWRES = dsamp
**	where bline and bsamp are the beginning line, sample coordinates
**	and dline and dsamp are the line, sample increments from the
**	IPW geodetic header, and nlines and nsamps are the number of lines
**	and samples in the IPW file.
**	If the -o option is specified, NORTH will be set to bline and
**	WEST will be set to bsamp.
**	Note: if the IPW image file contains no GEO header, the GRASS
**	active window will be used to define the geographic region.
**
**	Flags:
**	-o		    use the IPW GEO header origin (bline and bsamp)
**			    as the north and west edge of the region
**			    default: origin is offset by half a pixel in
**			    each direction (see discussion above)
** 
**	Parameters:
** 
**	ipw=name            IPW input image (defaults to standard input)
**	map=name            output GRASS cell file
**	mask=name           IPW mask image
**	multiplier=value    multiply IPW datum by value (before offset)
**			    default: 1.0
**	offset=value        add to map datum (after multiplication)
**			    default: 0.0
**
** EXAMPLES
**	The following command,
**
**		ipw.to.grass ipw=ipwfile map=grasslayer multiplier=10.0 \
**			offset=100.0
**
**	will convert an IPW image called ipwfile to a GRASS map
**	layer called  grasslayer, multiplying all input values by 10
**	then adding 100 before storing in the GRASS output file.
**
**	In the following command,
**
**		demux -b 0 ipw.img | ipw.to.grass map=grass.out \
**			mask=ipwmask offset=0.5
**
**	the IPW command demux is used to extract band 0 from a mult-band
**	IPW image file and feed it as input to ipw.to.grass.  The data are
**	masked by the IPW mask image ipwmask and 0.5 is added to non-masked
**	data to cause rounding to the nearest integer, rather than
**	truncating.
**
**	To convert an N-band IPW image into N GRASS cell files:
**
**		demux -b 1 ipwfile | ipw.to.grass o=cell.1
**			...
**		demux -b N ipwfile | ipw.to.grass o=cell.N
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
**	4/27/90   Written by Kelly Longley, Oregon State University, USEPA ERL-C
**	7/16/90   Added -f option to force GRASS range to LQH map range, by
**		  K. Longley, OSU, USEPA ERL-C
**	4/16/91	  Added -s offset option, by K. Longley OSU, USEPA ERL-C
**	4/22/91	  Substituted GRASS command line parsing, removed -f option,
**		  by G. Koerper, OSU, USEPA ERL-C
**	9/4/91	  Added half-pixel offset to North/west and -o option to
**		  override; No longer requires active program window to be
**		  set if IPW image has GEO header, by K. Longley, OSU,
**		  USEPA ERL-C
*/

#include "gis.h"
#include "ipw.h"
#include "getargs.h"

extern char     **_argv; /* not IPW kosher, but required to initialize _argv */

main (argc, argv)
	int argc;
	char *argv[];
{
	char		       *mapset;		/* name of GRASS mapset       */
	int			ipw_fd;		/* IPW image file descriptor  */
	int			cf_fd;		/* GRASS cell file descriptor */
	int			m_fd;		/* mask image file descriptor */
	double			mult;		/* IPW data multiplier        */
	double			offset;		/* IPW data offset	      */
	fpixel_t		min;		/* F.P. min of IPW image      */
	fpixel_t		max;		/* F.P. max of IPW image      */
	struct Cell_head	cellhd;		/* GRASS cell file header     */
	struct Range		range;		/* GRASS range structure      */

	struct Flag	*override;
        struct Option   *grass_file, *ipw_image, *ipw_mask, *mult_opt,
	 *offset_opt;


   /* Initialize GRASS library */

	G_gisinit (argv[0]);

	override = G_define_flag();
   	override->key = 'o';
   	override->description = "don't offset N/W edges by half-pixel - use IPW file values";

	ipw_image = G_define_option();
        ipw_image->key = "ipw";
        ipw_image->type = TYPE_STRING;
        ipw_image->required = NO;
        ipw_image->description = "IPW input image (defaults to stdin)";

        grass_file = G_define_option();
        grass_file->key = "map";
        grass_file->type = TYPE_STRING;
        grass_file->required = YES;
        grass_file->description = "output GRASS cell file";
        grass_file->gisprompt = "new,cell,raster";

        ipw_mask = G_define_option();
        ipw_mask->key = "mask";
        ipw_mask->type = TYPE_STRING;
        ipw_mask->required = NO;
        ipw_mask->description = "IPW mask image"; 

        mult_opt = G_define_option();
        mult_opt->key = "multiplier";
        mult_opt->type = TYPE_DOUBLE;
        mult_opt->required = NO;
        mult_opt->description = "multiply IPW datum by value (before offset)";
        mult_opt->answer = "1.0";

        offset_opt = G_define_option();
        offset_opt->key = "offset";
        offset_opt->type = TYPE_DOUBLE;
        offset_opt->required = NO;
        offset_opt->description = "add to map datum (after multiplication)";
        offset_opt->answer = "0.0";


   /* get args */

        if (G_parser(argc,argv))
            exit(1);

   /* set IPW global var used by error() */

        _argv = argv;

   /* process args */

        if (sscanf (mult_opt->answer, "%lf", &mult) != 1 || mult == 0) {
                fprintf (stderr, "Illegal multiplier: %s\n", mult_opt->answer);
                G_usage();
                exit(1);
        }

        if (sscanf (offset_opt->answer, "%lf", &offset) != 1) {
                fprintf (stderr, "Illegal offset: %s\n", offset_opt->answer);
                G_usage();
                exit(1);
        }

   /* access input image */

	if (ipw_image->answer) {
		ipw_fd = uropen (ipw_image->answer);
		if (ipw_fd == ERROR)
			error ("Can't open \"%s\"", ipw_image->answer);
	} else {
		ipw_fd = ustdin();
	}

    /* can't read tty */

	no_tty (ipw_fd);

   /* access mask image */

	if (ipw_mask->answer) {
		m_fd = uropen (ipw_mask->answer);
		if (m_fd == ERROR)
			error ("Can't open \"%s\"", ipw_mask->answer);
	} else {
		m_fd = ERROR;
	}

   /* read/check IPW headers */

	headers (ipw_fd, m_fd, &cellhd, &min, &max, override->answer);

   /* create GRASS cell file */

	mapset = G_mapset();
	cf_fd = G_open_cell_new (grass_file->answer);
	if (!cf_fd) {
		char msg[100];
		sprintf (msg, "unable to create layer %s", grass_file->answer);
		G_fatal_error (msg);
		exit(1);
	}

   /* convert IPW file to GRASS cell file */

	ipw2grass (ipw_fd, cf_fd, m_fd, cellhd, mult, offset);

   /* close GRASS cell file - all support files are automatically created */

	G_close_cell (cf_fd);

   /* all done */

	ipwexit (EX_OK);
}
