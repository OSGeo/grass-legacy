
#ifndef lint
static char *SCCSid=	"SCCS version: %Z%   %M%   %I%   %G%";
#endif

/*
** NAME
**      ipw.to.grass - Converts IPW image file to GRASS raster map layer
**      (GRASS Data Import/Processing Program)
** 
** SYNOPSIS
**      ipw.to.grass
**      ipw.to.grass help
**      ipw.to.grass [ipw=name] map=name [mask=name] [multiplier=value] \
**                [offset=value]
** 
** DESCRIPTION
** 
**      ipw.to.grass converts a single-band image file in IPW (Image
**      Processing Workbench) format to a GRASS raster map layer.
**      If an IPW image is not specified, standard input will be
**      used.  It runs under GRASS and assumes that the active program
**      window is set with number of rows and columns equal to that of
**      the IPW image file.
** 
**      Parameters:
** 
**      ipw=name            IPW input image (defaults to standard input)
**      map=name            output GRASS cell file
**      mask=name           IPW mask image
**      multiplier=value    multiply IPW datum by value (before offset)
**                          default: 1.0
**      offset=value        add to map datum (after multiplication)
**                          default: 0.0
**
** EXAMPLE
**
**	To convert an N-band IPW image into N GRASS cell files:
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

        struct Option   *grass_file, *ipw_image, *ipw_mask, *mult_opt,
	 *offset_opt;

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

   /* Initialize GRASS library */

	G_gisinit (argv[0]);

   /* read/check IPW headers */

	headers (ipw_fd, m_fd, &cellhd, &min, &max);

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
