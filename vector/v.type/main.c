
/***************************************************************
 *
 * MODULE:       v.type
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      Category manipulations
 *               
 * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/

#include <stdlib.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>

int main(int argc, char *argv[])
{
    struct Map_info In, Out;
    static struct line_pnts *Points;
    struct line_cats *Cats;
    int type;
    char *mapset;
    struct GModule *module;
    int i, j;
    struct Option *in_opt, *out_opt, *type_opt;
    int types[100];		/* array of input,output types */
    int ntypes;			/* number of types (number of pairs * 2) */

    module = G_define_module();
    module->keywords = _("vector, geometry");
    module->description = _("Change the type of geometry elements.");

    in_opt = G_define_standard_option(G_OPT_V_INPUT);
    out_opt = G_define_standard_option(G_OPT_V_OUTPUT);

    type_opt = G_define_standard_option(G_OPT_V_TYPE);
    type_opt->options = "point,line,boundary,centroid,face,kernel";
    type_opt->answer = "line,boundary,point,centroid";
    type_opt->label = _("Pairs for input and output type separated by comma");
    type_opt->description =
	_("<input_type1>,<output_type1>,<input_type2>,<output_type2>,...\n"
	  "\t\tExample1: line,boundary\n"
	  "\t\tExample2: line,boundary,point,centroid");


    G_gisinit(argv[0]);

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    i = 0;
    j = 0;
    while (type_opt->answers[i]) {
	switch (type_opt->answers[i][0]) {
	case 'p':
	    types[i] = GV_POINT;
	    break;
	case 'l':
	    types[i] = GV_LINE;
	    break;
	case 'b':
	    types[i] = GV_BOUNDARY;
	    break;
	case 'c':
	    types[i] = GV_CENTROID;
	    break;
	case 'f':
	    types[i] = GV_FACE;
	    break;
	case 'k':
	    types[i] = GV_KERNEL;
	    break;
	}
	if (j == 1) {		/* check type compatibility */
	    if (((types[i - 1] & (GV_POINT | GV_CENTROID | GV_KERNEL)) &&
		 (types[i] & (GV_LINE | GV_BOUNDARY | GV_FACE))
		) || ((types[i - 1] & (GV_LINE | GV_BOUNDARY | GV_FACE)) &&
		      (types[i] & (GV_POINT | GV_CENTROID | GV_KERNEL))
		)
		) {
		G_fatal_error(_("Incompatible types"));
	    }
	    j = 0;
	}
	else {
	    j++;
	}

	i++;
    }

    if (i < 2)
	G_fatal_error(_("Not enough types"));

    if (j == 1)
	G_fatal_error(_("Odd number of types"));

    ntypes = i;

    Vect_check_input_output_name(in_opt->answer, out_opt->answer,
				 GV_FATAL_EXIT);

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    /* open input vector */
    if ((mapset = G_find_vector2(in_opt->answer, "")) == NULL) {
	G_fatal_error(_("Vector map <%s> not found"), in_opt->answer);
    }

    Vect_set_open_level(1);
    Vect_open_old(&In, in_opt->answer, mapset);

    Vect_set_fatal_error(GV_FATAL_PRINT);
    if (0 > Vect_open_new(&Out, out_opt->answer, Vect_is_3d(&In))) {
	Vect_close(&In);
	exit(EXIT_FAILURE);
    }

    Vect_copy_head_data(&In, &Out);
    Vect_hist_copy(&In, &Out);
    Vect_hist_command(&Out);

    while ((type = Vect_read_next_line(&In, Points, Cats)) > 0) {
	for (i = 0; i < ntypes; i += 2) {
	    if (type == types[i]) {
		type = types[i + 1];
		break;
	    }
	}
	Vect_write_line(&Out, type, Points, Cats);
    }

    if (Vect_copy_tables(&In, &Out, 0))
        G_warning(_("Failed to copy attribute table to output map"));
    Vect_build(&Out);
    Vect_close(&Out);
    Vect_close(&In);

    exit(EXIT_SUCCESS);
}
