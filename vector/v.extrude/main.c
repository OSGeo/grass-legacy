/****************************************************************
 *
 * MODULE:     v.extrude
 *
 * AUTHOR(S):  Jachym Cepicky jachym.cepicky _at_ centrum _dot_ cz
 *             Based on v.example by Radim Blazek
 *             Inspired by d.vect and v.drape
 *             Coding help and code cleaning by Markus Neteler
 *
 * PURPOSE:    "Extrudes" flat polygons and lines to 3D with defined height
 *              Useful for creating buildings for displaying with NVIZ
 *
 * COPYRIGHT:  (C) 2005 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 * TODO:       - points to lines would be fine
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>
#include <grass/dbmi.h>


static long extrude(struct Map_info Out, struct line_cats *Cats,
	    struct line_pnts *Points, struct line_pnts *NewPoints, int fdrast,
	    int trace, double objheight, double voffset,
	    struct Cell_head window, int area);


int main(int argc, char *argv[])
{
    struct Map_info In, Out;
    static struct line_pnts *Points;
    static struct line_pnts *NewPoints;
    struct GModule *module;
    struct line_cats *Cats;
    int i, type, cat, ctype, fdrast = 0, areanum = 0;
    char *mapset;
    struct Option *old, *new, *zshift, *height, *elevation, *hcolumn, *type_opt,
	*field_opt;
    struct Flag *t_flag;
    double objheight, voffset;
    struct Cell_head window;
    struct cat_list *Clist;
    int nelements;
    int line, area = 0, trace = 0, centroid = 0;

    /* dbmi */
    struct field_info *Fi;
    dbDriver *driver = NULL;
    char query[1024];
    dbString sql;
    dbCursor cursor;
    dbTable *table;
    dbColumn *column;
    dbValue *value;
    int more;


    module = G_define_module();
    module->keywords = _("vector");
    module->description =
	_("Extrudes flat vector object to 3D with defined height.");

    t_flag = G_define_flag();
    t_flag->key = 't';
    t_flag->description = _("Trace elevation");

    old = G_define_standard_option(G_OPT_V_INPUT);
    old->description = _("Name of input 2D vector map");

    new = G_define_standard_option(G_OPT_V_OUTPUT);
    new->description = _("Name of resulting 3D vector map");

    zshift = G_define_option();
    zshift->key = "zshift";
    zshift->description = _("Shifting value for z coordinates");
    zshift->type = TYPE_DOUBLE;
    zshift->required = NO;
    zshift->answer = "0";

    /* raster sampling */
    elevation = G_define_option();
    elevation->key = "elevation";
    elevation->type = TYPE_STRING;
    elevation->required = NO;
    elevation->answer = FALSE;
    elevation->gisprompt = "old,cell,raster";
    elevation->description = _("Elevation raster for height extraction");

    height = G_define_option();
    height->key = "height";
    height->type = TYPE_DOUBLE;
    height->required = NO;
    height->multiple = NO;
    height->description = _("Fixed height for 3D vector objects");

    hcolumn = G_define_option();
    hcolumn->key = "hcolumn";
    hcolumn->type = TYPE_STRING;
    hcolumn->required = NO;
    hcolumn->multiple = NO;
    hcolumn->description =
	_("Column of attribute table with object heights");

    type_opt = G_define_standard_option(G_OPT_V_TYPE);
    type_opt->answer = "line,boundary,area,face";
    type_opt->options = "line,boundary,area,face";

    field_opt = G_define_standard_option(G_OPT_V_FIELD);

    G_gisinit(argv[0]);
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    if (!height->answer && !hcolumn->answer) {
	G_fatal_error(_("One of [%s] or [%s] parameters must be set"), height->key,
		      hcolumn->key);
    }
    sscanf(zshift->answer, "%lf", &voffset);
    if (height->answer)
	sscanf(height->answer, "%lf", &objheight);
    else
	objheight = 0.;

    if (t_flag->answer)
	trace = 1;

    i = 0;
    type = 0;
    area = FALSE;
    while (type_opt->answers[i]) {
	switch (type_opt->answers[i][0]) {
	case 'l':
	    type |= GV_LINE;
	    break;
	case 'b':
	    type |= GV_BOUNDARY;
	    break;
	case 'f':
	    type |= GV_FACE;
	    break;
	case 'a':
	    area = TRUE;
	    break;
	}
	i++;
    }
    /* set input vector map name and mapset */
    Vect_check_input_output_name(old->answer, new->answer, GV_FATAL_EXIT);
    if ((mapset = G_find_vector2(old->answer, "")) == NULL)
	G_fatal_error(_("Vector map <%s> not found"), old->answer);

    /* vector setup */
    Points = Vect_new_line_struct();
    NewPoints = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    Vect_set_open_level(2);
    Vect_open_new(&Out, new->answer, WITH_Z);

    /* opening old vector */
    Vect_open_old(&In, old->answer, mapset);
    Vect_hist_copy (&In, &Out);
    Vect_hist_command (&Out);

    /* opening database connection, if required */
    if (hcolumn->answer) {
	Clist = Vect_new_cat_list();
	Clist->field = atoi(field_opt->answer);
	if ((Fi = Vect_get_field(&In, Clist->field)) == NULL)
	    G_fatal_error(_("Database connection not defined"));

	if ((driver =
	     db_start_driver_open_database(Fi->driver, Fi->database)) == NULL)
	    G_fatal_error(_("Unable to open driver <%s>"), Fi->driver);

    }

    /* do we work with elevation raster? */
    if (elevation->answer) {

	/* raster setup */
	G_get_window(&window);

	/* check for the elev raster, and check for error condition */
	if ((mapset = G_find_cell2(elevation->answer, "")) == NULL)
	    G_fatal_error(_("Raster map <%s> not found"), elevation->answer);

	/* open the elev raster, and check for error condition */
	if ((fdrast = G_open_cell_old(elevation->answer, mapset)) < 0)
	    G_fatal_error(_("Unable to open raster map <%s>"), elevation->answer);
    }

    /* if area */
    if (area) {
	G_debug(1, "drawing areas");
	nelements = Vect_get_num_areas(&In);
	G_debug(2, "n_areas = %d", nelements);
	for (areanum = 1; areanum <= nelements; areanum++) {

	    G_percent(areanum, nelements, 1);

	    G_debug(3, "area = %d", areanum);

	    if (!Vect_area_alive(&In, areanum))
		continue;

	    centroid = Vect_get_area_centroid(&In, areanum);

	    /* height attribute */
	    if (hcolumn->answer) {
		cat = Vect_get_area_cat(&In, areanum, Clist->field);
		db_init_string(&sql);
		sprintf(query, "SELECT %s FROM %s WHERE %s = %d",
			hcolumn->answer, Fi->table, Fi->key, cat);
		G_debug(3, "SQL: %s", query);
		db_append_string(&sql, query);
		if (db_open_select_cursor(driver, &sql, &cursor, DB_SEQUENTIAL)
		    != DB_OK)
		    G_fatal_error(_("Cannot select attributes for area #%d"),
				  areanum);
		table = db_get_cursor_table(&cursor);
		column = db_get_table_column(table, 0);	/* first column */

		if (db_fetch(&cursor, DB_NEXT, &more) != DB_OK)
		    continue;
		value = db_get_column_value(column);
		objheight =
		    db_get_value_as_double(value,
					   db_get_column_host_type(column));

		/* only draw if hcolumn was defined */
		if (objheight != 0) {
		    G_debug(3, "area centroid %d: object height: %f", centroid,
			    objheight);
		}

	    } /* if hcolumn->answer */

	    Vect_get_area_points(&In, areanum, Points);
	    extrude(Out, Cats, Points, NewPoints, fdrast, trace, objheight,
		    voffset, window, area);

	}			/* /foreach area */

    }

    if (type > 0) {
	G_debug(1, "drawing other than areas");
	i = 1;
	/* loop through each line in the dataset */
	nelements = Vect_get_num_lines(&In);
	for (line = 1; line <= nelements; line++) {

	    /* progress feedback */
	    G_percent(line, nelements, 1);

	    /* read line */
	    type = Vect_read_line(&In, Points, Cats, line);


	    if (type == GV_LINE) {
		if (Vect_cat_get(Cats, 1, &cat) == 0) {
		    Vect_cat_set(Cats, 1, i);
		    i++;
		}
	    }
	    else if (type == GV_AREA) {
		if (Vect_cat_get(Cats, 1, &cat) == 0) {
		}
	    }

	    /* height attribute */
	    if (hcolumn->answer) {
		cat = Vect_get_line_cat(&In, line, Clist->field);

                /* sql init */
		db_init_string(&sql);
		sprintf(query, "SELECT %s FROM %s WHERE %s = %d",
			hcolumn->answer, Fi->table, Fi->key, cat);
		G_debug(3, "SQL: %s", query);
		db_append_string(&sql, query);

                /* cursor init */
		if (db_open_select_cursor(driver, &sql, &cursor, DB_SEQUENTIAL)
		    != DB_OK)
		    G_fatal_error(_("Cannot select attributes for area #%d"),
				  areanum);
		table = db_get_cursor_table(&cursor);
		column = db_get_table_column(table, 0);	/* first column */

		if (db_fetch(&cursor, DB_NEXT, &more) != DB_OK)
		    continue;

                /* objheight value */
		value = db_get_column_value(column);
		/* host_type -> ctype ? 
		objheight = 
		    db_get_value_as_double(value,
					   db_get_column_host_type(column));
		*/
		ctype = db_sqltype_to_Ctype (db_get_column_sqltype(column));
		if (ctype != DB_C_TYPE_INT && ctype != DB_C_TYPE_STRING &&
		    ctype != DB_C_TYPE_DOUBLE) {
		  G_fatal_error (_("Column <%s>: invalid data type."),
				   db_get_column_name (column));
		}
		objheight = 
		  db_get_value_as_double(value, ctype);
					   
		/* only draw if hcolumn was defined */
		if (objheight != 0) {
		    G_debug(3, "area centroid %d: object height: %f", centroid,
			    objheight);
		}

	    }
	    extrude(Out, Cats, Points, NewPoints, fdrast, trace, objheight,
		    voffset, window, area);

	}			/* for each line */
    }				/* /else if area */

    if (driver) {
	db_close_database(driver);
	db_shutdown_driver(driver);
    }

    Vect_build(&Out, stderr);
    Vect_close(&In);
    Vect_close(&Out);

    exit(EXIT_SUCCESS);
}


/* for each point int struct line_pnts *Poins calculates "roof" and "walls", 
 * result is stored to struct line_pnts *NewPoints 
 */
static long extrude(struct Map_info Out, struct line_cats *Cats,
	    struct line_pnts *Points, struct line_pnts *NewPoints, int fdrast,
	    int trace, double objheight, double voffset,
	    struct Cell_head window, int area)
{
    int k;			/* Points->n_points */
    float estimated_elevation = 0.;
    long result = 0;
    double voffset_dem = 0.0;

    /* base */
    /*Vect_write_line ( &Out, GV_FACE, Points, Cats ); 
       -- has no reason to display too -- smaller files */

    /* don't generate 1 point faces (eg from degenerated input maps) */
    if (Points->n_points > 1) {

	if (area) {
	    /* roof of building */
	    Vect_reset_line(NewPoints);
	    for (k = 0; k < Points->n_points; k++) {

		/* should the voffset be set from the elevation? */
		if (fdrast) {
		    estimated_elevation =
			G_get_raster_sample(fdrast, &window, NULL, 
                                Points->y[k], Points->x[k], 0, NEAREST);
		    if (trace) {
		        voffset_dem = estimated_elevation;
		    }
		    else {

			if (k == 0) {
			    voffset_dem = estimated_elevation;
			}
			else {
			    voffset_dem =
			        estimated_elevation <
			        voffset_dem ? estimated_elevation : voffset_dem;
			}
		    }
		}		/* /if(fdrast) */

		Vect_append_point(NewPoints, Points->x[k], Points->y[k],
				  Points->z[k] + objheight + voffset + voffset_dem);

	    }
	    Vect_write_line(&Out, GV_FACE, NewPoints, Cats);
	}			/* if area */

	/* walls */
	for (k = 0; k < Points->n_points - 1; k++) {
	    float voffset_curr = 0.;	/* offset of current point */
	    float voffset_next = 0.;	/* offset of next point */
	    Vect_reset_line(NewPoints);

            /* should the voffset be set from the elevation? */
            if (trace && fdrast) {
                voffset_curr =
                    G_get_raster_sample(fdrast, &window, NULL, 
                            Points->y[k], Points->x[k], 0, NEAREST);
                voffset_next =
                    G_get_raster_sample(fdrast, &window, NULL, 
                            Points->y[k + 1], Points->x[k + 1], 0, NEAREST);
            }
            else {
                voffset_curr = voffset + voffset_dem;
                voffset_next = voffset + voffset_dem;
            }

	    Vect_append_point(NewPoints, Points->x[k], Points->y[k],
			      Points->z[k] + voffset_curr);
	    Vect_append_point(NewPoints, Points->x[k + 1], Points->y[k + 1],
			      Points->z[k + 1] + voffset_next);
	    Vect_append_point(NewPoints, Points->x[k + 1], Points->y[k + 1],
			      Points->z[k + 1] + objheight + voffset_next);
	    Vect_append_point(NewPoints, Points->x[k], Points->y[k],
			      Points->z[k] + objheight + voffset_curr);
	    Vect_append_point(NewPoints, Points->x[k], Points->y[k],
			      Points->z[k] + voffset_curr);
	    result = Vect_write_line(&Out, GV_FACE, NewPoints, Cats);
	}			/* /walls */

    }				/* npoints check */

    return result;
}

