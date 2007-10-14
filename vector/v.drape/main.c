
/**********************************************************
 *
 * MODULE:       v.drape
 * 
 * AUTHOR(S):    Radim Blazek, Dylan Beaudette
 *               
 * PURPOSE:      Convert 2D vector to 3D vector by sampling of elevation raster.
 *               
 * COPYRIGHT:    (C) 2005 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 * 
 **********************************************************/


 /** Doxygen Style Comments
\file main.c
\brief v.drape module for converting 2D vectors into 3D vectors by means of sampling an elevation raster.
 
\author Radim Blazek
\author Dylan Beaudette
\date 2005.09.20
 
\todo add support for areas
\todo Enhanced error checking such as
	\li does the elevation raster cover the entire are of the vector map?
	\li does the current region include the entire input vector map ?
\todo Make a description.html for documentation

 */



#include <stdlib.h>
#include <math.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>


int main(int argc, char *argv[])
{
    struct GModule *module;
    struct Option *in_opt, *out_opt, *type_opt, *rast_opt, *method_opt;	
    struct Map_info In, Out;
    struct line_pnts *Points;
    struct line_cats *Cats;
    /* int    layer; */
    int line, nlines, otype, ltype;

    /* Raster stuff from v.sample::main.c */
    char *mapset;
    int j;
    double scale, estimated_elevation;
    INTERP_TYPE method = UNKNOWN;
    int fdrast;			/* file descriptor for raster map is int */
    struct Cell_head window;
    /* end raster stuff */

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("vector");
    module->description =
      _("Converts 2D vector to 3D vector by sampling of elevation raster. Default sampling by nearest neighbor.");

    in_opt = G_define_standard_option(G_OPT_V_INPUT);

    type_opt = G_define_standard_option(G_OPT_V_TYPE);
    type_opt->options = "point,centroid,line,boundary,face,kernel";
    type_opt->answer = "point,centroid,line,boundary,face,kernel";

    /* raster sampling */
    rast_opt = G_define_option();
    rast_opt->key = "rast";
    rast_opt->type = TYPE_STRING;
    rast_opt->required = NO;
    rast_opt->description = _("Elevation raster for height extraction");

    method_opt = G_define_option();
    method_opt->key = "method";
    method_opt->type = TYPE_STRING;
    method_opt->required = NO;
    method_opt->multiple = NO;
    method_opt->options = "nearest,bilinear,cubic";
    method_opt->answer = "nearest";
    method_opt->descriptions = "nearest;nearest neighbor;"
			"bilinear;bilinear interpolation;"
			"cubic;cubic convolution interpolation;";
    method_opt->description = _("Sampling method");

    out_opt = G_define_standard_option(G_OPT_V_OUTPUT);

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    /* which interpolation method should we use */
    if ( method_opt->answer[0] == 'b' )
	method = BILINEAR;
    else if ( method_opt->answer[0] == 'c' )
	method = CUBIC;
    else
    {
        G_message(_("defaulting to nearest neighbor sampling"));
        method = NEAREST;
    }

    /* setup the raster for sampling */

    /* setup the region */
    G_get_window(&window);

    /* check for the elev raster, and check for error condition */
    if ((mapset = G_find_cell(rast_opt->answer, "")) == NULL) {
	G_fatal_error(_("Raster map <%s> not found"), rast_opt->answer);
    }

    /* open the elev raster, and check for error condition */
    if ((fdrast = G_open_cell_old(rast_opt->answer, mapset)) < 0) {
	G_fatal_error(_("Unable to open raster map <%s>"), rast_opt->answer);
    }

    /* used to scale sampled raster values: will need to add an option to modify this later */
    scale = 1;


    /* Check output type */
    otype = Vect_option_to_types(type_opt);

    Vect_set_open_level(2);
    Vect_open_old(&In, in_opt->answer, "");

    /* setup the new vector map */
    /* remember to open the new vector map as 3D:  Vect_open_new(,,1) */
    Vect_open_new(&Out, out_opt->answer, 1);
    Vect_copy_head_data(&In, &Out);
    Vect_hist_copy(&In, &Out);
    Vect_hist_command(&Out);
    /* copy the input vector's attribute table to the new vector */
    /* This works for both level 1 and 2 */
    Vect_copy_tables(&In, &Out, 0);

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    /* line types */
    if ((otype &
	 (GV_POINTS | GV_LINES | GV_BOUNDARY | GV_CENTROID | GV_FACE |
	  GV_KERNEL))) {

	/* loop through each line in the dataset */
	nlines = Vect_get_num_lines(&In);

	for (line = 1; line <= nlines; line++) {

	    /* progress feedback */
	    G_percent(line, nlines, 1);

	    /* get the line type */
	    ltype = Vect_read_line(&In, Points, Cats, line);

	    /* adjust flow based on specific type of line */
	    switch (ltype) {
		/* points (at least 1 vertex) */
	    case GV_POINT:
	    case GV_CENTROID:
	    case GV_KERNEL:
                /* sample raster at this point, and update the z-coordinate
                 * (note that input vector should not be 3D!)
                 */
                estimated_elevation = scale * G_get_raster_sample(fdrast,
                            &window, NULL, Points->y[0], Points->x[0], 0, method);

		/* update the elevation value for each data point */
		Points->z[0] = estimated_elevation;
		break;
		/* standard lines (at least 2 vertexes) */
	    case GV_LINE:
	    case GV_BOUNDARY:
		if (Points->n_points < 2)
		    break;	/* At least 2 points */

		/* loop through each point in a line */
		for (j = 0; j < Points->n_points; j++) {
		    /* sample raster at this point, and update the z-coordinate (note that input vector should not be 3D!) */
                    estimated_elevation = scale * G_get_raster_sample(fdrast,
                                &window, NULL, Points->y[j], Points->x[j], 0, method);

		    /* update the elevation value for each data point */
		    Points->z[j] = estimated_elevation;
		}		/* end looping through point in a line */
		break;

		/* lines with at least 3 vertexes */
	    case GV_FACE:
		if (Points->n_points < 3)
		    break;	/* At least 3 points */

		/* loop through each point in a line */
		for (j = 0; j < Points->n_points; j++) {
		    /* sample raster at this point, and update the z-coordinate (note that input vector should not be 3D!) */
                    estimated_elevation = scale * G_get_raster_sample(fdrast,
                                &window, NULL, Points->y[j], Points->x[j], 0, method);

		    /* update the elevation value for each data point */
		    Points->z[j] = estimated_elevation;
		}
		break;
	    }			/* end line type switch */

	    /* write the new line file, with the updated Points struct*/
	    Vect_write_line(&Out, ltype, Points, Cats);
	}			/* end looping thru lines */

    }				/* end working on type=lines */

    /* close elevation raster: */
    G_close_cell(fdrast);

    /* close input vector */
    Vect_close(&In);
    /* build topology for output vector */
    Vect_build(&Out, stderr);
    /* close output vector */
    Vect_close(&Out);

    exit(EXIT_SUCCESS);
}
