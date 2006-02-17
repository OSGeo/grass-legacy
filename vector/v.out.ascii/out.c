/*
 *
 ****************************************************************************
 *
 * MODULE:     r.out.ascii
 * AUTHOR(S):  Michael Higgins, U.S. Army Construction Engineering Research Laboratory
 *             James Westervelt, U.S. Army Construction Engineering Research Laboratory
 *             Radim Blazek, ITC-Irst, Trento, Italy
 *
 * PURPOSE:    r.out.ascii: writes ASCII GRID file
 * COPYRIGHT:  (C) 2000 by the GRASS Development Team
 *
 *             This program is free software under the GNU General Public
 *              License (>=v2). Read the file COPYING that comes with GRASS
 *              for details.
 *
 ****************************************************************************
 */
/*  @(#)b_a_dig.c       2.1  6/26/87  */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>
#include "local_proto.h"

int main(int argc, char *argv[])
{
    FILE *ascii, *att;
    struct Option *input, *output, *format_opt, *dp_opt;
    struct Flag *verf;
    int format, dp;
    struct Map_info Map;
    int ver = 5, pnt = 0;
    struct GModule *module;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
	_("Convert a GRASS binary vector map to a GRASS ASCII vector map");

    input = G_define_standard_option(G_OPT_V_INPUT);

    output = G_define_option();
    output->key = "output";
    output->type = TYPE_STRING;
    output->required = NO;
    output->multiple = NO;
    output->gisprompt = "file,file,file";
    output->description =
	_("Path to resulting ASCII file or ASCII vector name if '-o' is defined");

    format_opt = G_define_option();
    format_opt->key = "format";
    format_opt->type = TYPE_STRING;
    format_opt->required = NO;
    format_opt->multiple = NO;
    format_opt->options = "point,standard";
    format_opt->answer = "point";
    format_opt->description = _("Output format");

    dp_opt = G_define_option();
    dp_opt->key = "dp";
    dp_opt->type = TYPE_INTEGER;
    dp_opt->required = NO;
    dp_opt->description =
	_("Number of significant digits (floating point only)");

    verf = G_define_flag();
    verf->key = 'o';
    verf->description = _("Create old (version 4) ASCII file");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    if (format_opt->answer[0] == 'p')
	format = FORMAT_POINT;
    else
	format = FORMAT_ALL;

    if (verf->answer)
	ver = 4;

    if (ver == 4 && format == FORMAT_POINT) {
	G_fatal_error(_("format 'point' is not supported for old version"));
    }

    if (ver == 4 && output->answer == NULL) {
	G_fatal_error(_("'output' must be given for old version"));
    }

    Vect_set_open_level(1);	/* only need level I */
    Vect_open_old(&Map, input->answer, "");


    if (output->answer) {
	if (ver == 4) {
	    ascii = G_fopen_new("dig_ascii", output->answer);
	}
	else {
	    ascii = fopen(output->answer, "w");
	}

	if (ascii == NULL) {
	    G_fatal_error(_("Cannot open file [%s]"), output->answer);
	}
    }
    else {
	ascii = stdout;
    }

    if (format == FORMAT_ALL) {
	write_head(ascii, &Map);
	fprintf(ascii, "VERTI:\n");
    }

    /*The precision of the output */
    if (dp_opt->answer) {
	if (sscanf(dp_opt->answer, "%d", &dp) != 1)
	    G_fatal_error(_("failed to interpret dp as an integer"));
	if (dp > 8 || dp < 0)
	    G_fatal_error(_("dp has to be from 0 to 8"));
    }
    else {
	dp = 8;			/*This value is taken from the lib settings in G_format_easting */
    }

    /* Open dig_att */
    att = NULL;
    if (ver == 4 && !pnt) {
	if (G_find_file("dig_att", output->answer, G_mapset()) != NULL)
	    G_fatal_error(_("dig_att file already exist"));

	if ((att = G_fopen_new("dig_att", output->answer)) == NULL)
	    G_fatal_error(_("Not able to open dig_att file <%s>\n"),
			  output->answer);
    }

    bin_to_asc(ascii, att, &Map, ver, format, dp);

    if (ascii != NULL)
	fclose(ascii);
    if (att != NULL)
	fclose(att);

    Vect_close(&Map);

    exit(EXIT_SUCCESS);
}
