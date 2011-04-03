
/****************************************************************
 *
 * MODULE:       v.buffer
 * 
 * AUTHOR(S):    Radim Blazek
 *               Markus Metz
 *               
 * PURPOSE:      Vector buffering
 *               
 * COPYRIGHT:    (C) 2001-2011 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/dbmi.h>
#include <grass/glocale.h>

#define DEBUG_NONE   0
#define DEBUG_BUFFER 1
#define DEBUG_CLEAN  2

#define PI M_PI

/* TODO: look at RET value and use, is it OK? */
#define RET 0.000000001		/* Representation error tolerance */

struct buf_contours
{
    int inner_count;
    int outer;
    int *inner;
};

/* 
 * Test if area in Out is in buffer, using the x,y coords of the area centroid.
 * Return: 1 in buffer
 *         0 outside buffer
 */
int point_in_buffer(struct buf_contours *arr_bc, SPATIAL_INDEX *si,
                    struct Map_info *Buf, double x, double y)
{
    int i, j, ret, flag = 0;
    BOUND_BOX bbox;
    static struct ilist *List = NULL;
    static struct line_pnts *Points = NULL;
    
    if (List == NULL)
	List = Vect_new_list();
    if (Points == NULL)
	Points = Vect_new_line_struct();

    /* select outer contours overlapping with centroid (x, y) */
    bbox.W = bbox.E = x;
    bbox.N = bbox.S = y;
    bbox.T = PORT_DOUBLE_MAX;
    bbox.B = -PORT_DOUBLE_MAX;

    Vect_spatial_index_select(si, &bbox, List);

    for (i = 0; i < List->n_values; i++) {
	Vect_read_line(Buf, Points, NULL, arr_bc[List->value[i]].outer);
	ret = Vect_point_in_poly(x, y, Points);
	if (ret == 0)
	    continue;

	flag = 1;	/* inside outer contour */
	for (j = 0; j < arr_bc[List->value[i]].inner_count; j++) {
	    if (arr_bc[List->value[i]].inner[j] < 1)
		continue;

	    Vect_read_line(Buf, Points, NULL, arr_bc[List->value[i]].inner[j]);
	    ret = Vect_point_in_poly(x, y, Points);
	    if (ret != 0) {	/* inside inner contour */
		flag = 0;
		break;
	    }
	}

	if (flag) {
	    /* (x,y) is inside outer contour and outside inner contours of arr_bc[List->value[i]] */
	    return 1;
	}
    }

    return 0;
}

void stop(struct Map_info *In, struct Map_info *Out, struct Map_info *Buf,
          char *bufname, SPATIAL_INDEX *si, int debug)
{
    Vect_spatial_index_destroy(si);

    if (debug != DEBUG_NONE) {
	Vect_build(Buf);
	Vect_close(Buf);
    }
    else {
	Vect_close(Buf);
	Vect_delete(bufname);
    }
    
    Vect_close(In);

    G_message(_("Rebuilding topology..."));
    Vect_build_partial(Out, GV_BUILD_NONE);
    Vect_build(Out);
    Vect_close(Out);
}

int main(int argc, char *argv[])
{
    struct Map_info In, Out, Buf;
    struct line_pnts *Points, *BPoints;
    struct line_cats *Cats, *BCats;
    char *mapset, bufname[GNAME_MAX];
    struct GModule *module;
    struct Option *in_opt, *out_opt, *type_opt, *buffer_opt, *dist_opt,
	*tolerance_opt, *bufcol_opt, *scale_opt, *debug_opt, *field_opt;
    double buffer, tolerance, dtmp;
    int type, debug;
    int ret, nareas, area, nlines, line;
    char *Areas, *Lines;
    int field;
    int dist_answer;
    struct buf_contours *arr_bc;
    int buffers_count = 0, line_id;
    SPATIAL_INDEX si;
    BOUND_BOX bbox;

    /* Attributes if bufcol is used */
    int i, nrec, ctype;
    struct field_info *Fi;
    dbDriver *Driver;
    dbCatValArray cvarr;
    int size_val_int;
    double size_val, scale, orig_tolerance;


    module = G_define_module();
    module->keywords = _("vector");
    module->description =
	_("Creates a buffer around features of given type (areas must contain centroid).");

    in_opt = G_define_standard_option(G_OPT_V_INPUT);
    out_opt = G_define_standard_option(G_OPT_V_OUTPUT);

    type_opt = G_define_standard_option(G_OPT_V_TYPE);
    type_opt->options = "point,line,boundary,centroid,area";
    type_opt->answer = "point,line,area";

    field_opt = G_define_standard_option(G_OPT_V_FIELD);
    field_opt->answer = "-1";

    buffer_opt = G_define_option();
    buffer_opt->key = "buffer";
    buffer_opt->type = TYPE_DOUBLE;
    buffer_opt->required = NO;
    buffer_opt->description = _("DEPRECATED Buffer distance in map units");

    dist_opt = G_define_option();
    dist_opt->key = "distance";
    dist_opt->type = TYPE_DOUBLE;
    dist_opt->required = NO;
    dist_opt->description = _("Buffer distance in map units");

    bufcol_opt = G_define_option();
    bufcol_opt->key = "bufcol";
    bufcol_opt->type = TYPE_STRING;
    bufcol_opt->required = NO;
    bufcol_opt->description =
	_("Attribute column to use for buffer distances");
    bufcol_opt->guisection = _("Advanced");

    scale_opt = G_define_option();
    scale_opt->key = "scale";
    scale_opt->type = TYPE_DOUBLE;
    scale_opt->required = NO;
    scale_opt->answer = "1.0";
    scale_opt->description = _("Scaling factor for attribute column values");
    scale_opt->guisection = _("Advanced");

    tolerance_opt = G_define_option();
    tolerance_opt->key = "tolerance";
    tolerance_opt->type = TYPE_DOUBLE;
    tolerance_opt->required = NO;
    tolerance_opt->answer = "0.01";
    tolerance_opt->guisection = _("Advanced");
    tolerance_opt->description =
	_("Maximum distance between theoretical arc and polygon segments "
	  "as multiple of buffer");

    debug_opt = G_define_option();
    debug_opt->key = "debug";
    debug_opt->type = TYPE_STRING;
    debug_opt->required = NO;
    debug_opt->options = "buffer,clean";
    debug_opt->guisection = _("Advanced");
    debug_opt->description = _("Stop the process at a certain stage");

    G_gisinit(argv[0]);
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    type = Vect_option_to_types(type_opt);
    field = atoi(field_opt->answer);

    dist_answer = (buffer_opt->answer != NULL || dist_opt->answer != NULL);

    if ((dist_answer && bufcol_opt->answer) ||
	(!(dist_answer || bufcol_opt->answer)))
	G_fatal_error("Select a buffer distance or column, but not both.");

    /* fixed
    if (bufcol_opt->answer)
	G_warning(_("The bufcol option may contain bugs during the cleaning "
		    "step. If you encounter problems, use the debug "
		    "option or clean manually with v.clean tool=break; "
		    "v.category step=0; v.extract -d type=area"));
    */

    orig_tolerance = atof(tolerance_opt->answer);
    tolerance = orig_tolerance;

    scale = atof(scale_opt->answer);
    if (scale <= 0.0)
	G_fatal_error("Illegal scale value");

    if (dist_opt->answer)
	buffer = fabs(atof(dist_opt->answer));
    else if (buffer_opt->answer)
	buffer = fabs(atof(buffer_opt->answer));
	

    if (dist_answer) {

	tolerance *= buffer;
	G_verbose_message(_("The tolerance in map units: %g"), tolerance);

	/* At least 8 points for circle. */
	dtmp = 0.999 * buffer * (1 - cos(2 * PI / 8 / 2));
	G_debug(3, "Minimum tolerance = %f", dtmp);
	if (tolerance > dtmp) {
	    tolerance = dtmp;
	    G_warning(_("The tolerance was reset to %g (map units)"),
		      tolerance);
	}
    }

    debug = DEBUG_NONE;
    if (debug_opt->answer) {
	if (debug_opt->answer[0] == 'b')
	    debug = DEBUG_BUFFER;
	else if (debug_opt->answer[0] == 'c')
	    debug = DEBUG_CLEAN;
    }

    Vect_check_input_output_name(in_opt->answer, out_opt->answer,
				 GV_FATAL_EXIT);

    Points = Vect_new_line_struct();
    BPoints = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();
    BCats = Vect_new_cats_struct();

    /* open input vector */
    if ((mapset = G_find_vector2(in_opt->answer, "")) == NULL) {
	G_fatal_error(_("Vector map <%s> not found"), in_opt->answer);
    }

    Vect_set_open_level(2);
    Vect_open_old(&In, in_opt->answer, mapset);

    /* allocate space for buffer ids */
    nlines = nareas = 0;
    if ((type & GV_POINTS) || (type & GV_LINES))
	nlines += Vect_get_num_primitives(&In, type);
    if (type & GV_AREA)
	nareas = Vect_get_num_areas(&In);
    
    if (nlines + nareas == 0) {
	G_warning(_("No features available for buffering. "
	            "Check type option and features available in the input vector."));
	exit(EXIT_SUCCESS);
    }
	
    buffers_count = 1;
    arr_bc = G_malloc((nlines + nareas + 1) * sizeof(struct buf_contours));

    Vect_spatial_index_init(&si);

    Vect_set_fatal_error(GV_FATAL_PRINT);
    if (0 > Vect_open_new(&Out, out_opt->answer, 0)) {
	Vect_close(&In);
	exit(EXIT_FAILURE);
    }
    
    /* open tmp vector for buffers, needed for cleaning */
    sprintf(bufname, "%s_buffers", out_opt->answer);
    if (0 > Vect_open_new(&Buf, bufname, 0)) {
	Vect_close(&In);
	Vect_close(&Out);
	Vect_delete(out_opt->answer);
	exit(EXIT_FAILURE);
    }
    Vect_build_partial(&Buf, GV_BUILD_BASE);

    /* check and load attribute column data */
    if (bufcol_opt->answer) {
	db_CatValArray_init(&cvarr);

	Fi = Vect_get_field(&In, field);
	if (Fi == NULL)
	    G_fatal_error(_("Unable to get layer info for vector map"));

	Driver = db_start_driver_open_database(Fi->driver, Fi->database);
	if (Driver == NULL)
	    G_fatal_error(_("Unable to open database <%s> by driver <%s>"),
			  Fi->database, Fi->driver);

	/* Note do not check if the column exists in the table because it may be expression */

	/* TODO: only select values we need instead of all in column */
	nrec = db_select_CatValArray(Driver, Fi->table, Fi->key,
				     bufcol_opt->answer, NULL, &cvarr);

	if (nrec < 0)
	    G_fatal_error(_("Unable to select data from table"));
	G_debug(2, "%d records selected from table", nrec);

	ctype = cvarr.ctype;
	if (ctype != DB_C_TYPE_INT && ctype != DB_C_TYPE_DOUBLE)
	    G_fatal_error(_("Column type not supported"));

	db_close_database_shutdown_driver(Driver);

	for (i = 0; i < cvarr.n_values; i++) {
	    if (ctype == DB_C_TYPE_INT) {
		G_debug(4, "cat = %d val = %d", cvarr.value[i].cat,
			cvarr.value[i].val.i);
	    }
	    else if (ctype == DB_C_TYPE_DOUBLE) {
		G_debug(4, "cat = %d val = %f", cvarr.value[i].cat,
			cvarr.value[i].val.d);
	    }
	}

    }


    Vect_copy_head_data(&In, &Out);
    Vect_hist_copy(&In, &Out);
    Vect_hist_command(&Out);

    /* Create buffers' boundaries */

    /* Lines (and Points) */
    if ((type & GV_POINTS) || (type & GV_LINES)) {
	int line, ltype, looped;
	double pbuffer;

	G_message(_("Line buffers... "));
	for (line = 1; line <= nlines; line++) {
	    int cat;
	    double area_size = 0, inner_size = 0;

	    G_debug(3, "line = %d", line);
	    G_percent(line, nlines, 2);

	    ltype = Vect_read_line(&In, Points, Cats, line);
	    if (!(ltype & type))
		continue;

	    if (field > 0 && !Vect_cat_get(Cats, field, &cat))
		continue;

	    if (bufcol_opt->answer) {
		/* get value from sizecol column */
		/* should probably be put in a function */
		if (ctype == DB_C_TYPE_INT) {
		    ret =
			db_CatValArray_get_value_int(&cvarr, cat,
						     &size_val_int);
		    if (ret != DB_OK) {
			G_warning(_("No record for category %d in table <%s>"),
				  cat, Fi->table);
			continue;
		    }
		    size_val = (double)size_val_int;
		}

		if (ctype == DB_C_TYPE_DOUBLE) {
		    ret =
			db_CatValArray_get_value_double(&cvarr, cat,
							&size_val);
		    if (ret != DB_OK) {
			G_warning(_("No record for category %d in table <%s>"),
				  cat, Fi->table);
			continue;
		    }
		}

		if (size_val < 0.0) {
		    G_warning(_("Attribute is of invalid size (%.3f) for category %d"),
			      size_val, cat);
		    continue;
		}

		if (size_val == 0.0)
		    continue;

		buffer = size_val * scale;
		G_debug(2, "    dynamic buffer size = %.2f", buffer);

		tolerance = orig_tolerance * buffer;
		G_debug(2, _("The tolerance in map units: %g"), tolerance);

		/* At least 8 points for circle. */
		dtmp = 0.999 * buffer * (1 - cos(2 * PI / 8 / 2));
		G_debug(2, "Minimum tolerance = %f", dtmp);
		if (tolerance > dtmp) {
		    tolerance = dtmp;
		    G_warning(_("The tolerance was reset to %g (map units). [category %d]"),
			      tolerance, cat);
		}
	    }

	    Vect_line_prune(Points);
	    /* looped line ? */
	    looped = 0;
	    if (Points->n_points > 3) {
		if (Points->x[0] == Points->x[Points->n_points - 1] &&
		    Points->y[0] == Points->y[Points->n_points - 1]) {

		    G_debug(2, "looped line");

		    /* determine correct sides for outer and inner contours */
		    dig_find_area_poly(Points, &area_size);
		    if (area_size == 0) {
			G_warning("zero area size");
			looped = 0;
		    }
		    else if (area_size > 0)
			pbuffer = -buffer;
		    else
			pbuffer = buffer;

		    looped = 1;
		}
		if (!looped) {
		    double dx = Points->x[0] - Points->x[Points->n_points - 1];
		    double dy = Points->y[0] - Points->y[Points->n_points - 1];
		    double dist = sqrt(dx * dx + dy * dy);
		    
		    /* if the distance between endpoints is < 2 * buffer
		     * and at least one point is > 2 * buffer away from the endpoints, 
		     * problems may occur
		     * break up the line if possible
		     * find point farthest away from end point
		     * if this point is > 2 * buffer away from end point, break
		     * first line from start point to this point
		     * second line from this point to end point */

		    looped = 0;
		}
	    }

	    if (looped) {
		Vect_line_parallel(Points, pbuffer, tolerance, 1, BPoints);
		if (BPoints->n_points > 3) {
		    if (BPoints->x[0] != BPoints->x[BPoints->n_points - 1] ||
			BPoints->x[0] != BPoints->x[BPoints->n_points - 1]) {
			Vect_append_point(BPoints, BPoints->x[0], BPoints->y[0], 0);
		    }
		    Vect_write_line(&Out, GV_BOUNDARY, BPoints, BCats);
		    line_id = Vect_write_line(&Buf, GV_BOUNDARY, BPoints, Cats);
		    /* add buffer to spatial index */
		    Vect_get_line_box(&Buf, line_id, &bbox);
		    Vect_spatial_index_add_item(&si, buffers_count, &bbox);
		    arr_bc[buffers_count].outer = line_id;
		    arr_bc[buffers_count].inner_count = 0;
		}
		else
		    G_fatal_error(_("Could not get outside buffer for line id %d"), line);

		Vect_line_parallel(Points, -pbuffer, tolerance, 1, BPoints);
		if (BPoints->n_points > 3) {
		    if (BPoints->x[0] != BPoints->x[BPoints->n_points - 1] ||
			BPoints->x[0] != BPoints->x[BPoints->n_points - 1]) {
			Vect_append_point(BPoints, BPoints->x[0], BPoints->y[0], 0);
		    }
		    dig_find_area_poly(BPoints, &inner_size);
		    /* area size of inner contour must be smaller than area size of original points */
		    if (fabs(inner_size) < fabs(area_size)) {
			Vect_write_line(&Out, GV_BOUNDARY, BPoints, BCats);
			line_id = Vect_write_line(&Buf, GV_BOUNDARY, BPoints, Cats);
			arr_bc[buffers_count].inner = G_malloc(sizeof(int));
			arr_bc[buffers_count].inner_count = 1;
			arr_bc[buffers_count].inner[0] = line_id;
		    }
		}
		buffers_count++;
	    }
	    else {
		Vect_line_buffer(Points, buffer, tolerance, BPoints);
		Vect_write_line(&Out, GV_BOUNDARY, BPoints, BCats);
		line_id = Vect_write_line(&Buf, GV_BOUNDARY, BPoints, Cats);

		/* add buffer to spatial index */
		Vect_get_line_box(&Buf, line_id, &bbox);
		Vect_spatial_index_add_item(&si, buffers_count, &bbox);
		arr_bc[buffers_count].outer = line_id;
		arr_bc[buffers_count].inner_count = 0;
		buffers_count++;
	    }
	}
    }

    /* Areas */
    if (type & GV_AREA) {
	int i, area, centroid, nisles, isle, line_id;

	G_message(_("Area buffers... "));
	for (area = 1; area <= nareas; area++) {
	    int cat;

	    G_percent(area, nareas, 2);
	    
	    if (!(Vect_area_alive(&In, area)))
		continue;

	    centroid = Vect_get_area_centroid(&In, area);
	    if (centroid == 0)
		continue;

	    Vect_read_line(&In, NULL, Cats, centroid);
	    if (field > 0 && !Vect_cat_get(Cats, field, &cat))
		continue;

	    if (bufcol_opt->answer) {
		/* get value from sizecol column */

		if (ctype == DB_C_TYPE_INT) {
		    ret =
			db_CatValArray_get_value_int(&cvarr, cat,
						     &size_val_int);
		    if (ret != DB_OK) {
			G_warning(_("No record for category %d in table <%s>"),
				  cat, Fi->table);
			continue;
		    }
		    size_val = (double)size_val_int;
		}

		if (ctype == DB_C_TYPE_DOUBLE) {
		    ret =
			db_CatValArray_get_value_double(&cvarr, cat,
							&size_val);
		    if (ret != DB_OK) {
			G_warning(_("No record for category %d in table <%s>"),
				  cat, Fi->table);
			continue;
		    }
		}

		if (size_val < 0.0) {
		    G_warning(_("Attribute is of invalid size (%.3f) for category %d"),
			      size_val, cat);
		    continue;
		}

		if (size_val == 0.0)
		    continue;

		buffer = size_val * scale;
		G_debug(2, "    dynamic buffer size = %.2f", buffer);

		tolerance = orig_tolerance * buffer;
		G_debug(2, _("The tolerance in map units: %g"), tolerance);

		/* At least 8 points for circle. */
		dtmp = 0.999 * buffer * (1 - cos(2 * PI / 8 / 2));
		G_debug(2, "Minimum tolerance = %f", dtmp);
		if (tolerance > dtmp) {
		    tolerance = dtmp;
		    G_warning(_("The tolerance was reset to %g (map units). [category %d]"),
			      tolerance, cat);
		}
	    }


	    /* outer ring */
	    Vect_get_area_points(&In, area, Points);
	    Vect_line_prune(Points);
	    Vect_line_parallel(Points, -buffer, tolerance, 1, BPoints);
	    Vect_write_line(&Out, GV_BOUNDARY, BPoints, BCats);
	    line_id = Vect_write_line(&Buf, GV_BOUNDARY, BPoints, Cats);

	    /* add outer ring to spatial index */
	    Vect_get_line_box(&Buf, line_id, &bbox);
	    Vect_spatial_index_add_item(&si, buffers_count, &bbox);
	    arr_bc[buffers_count].outer = line_id;

	    /* islands */
	    nisles = Vect_get_area_num_isles(&In, area);
	    arr_bc[buffers_count].inner = G_malloc(nisles * sizeof(int));
	    arr_bc[buffers_count].inner_count = nisles;
	    for (i = 0; i < nisles; i++) {
		double l, isle_size, inner_size;

		isle = Vect_get_area_isle(&In, area, i);
		Vect_get_isle_points(&In, isle, Points);
		Vect_line_prune(Points);

		/* Check if the isle is big enough */
		l = Vect_line_length(Points);
		if (l / 2 < 2 * buffer)
		    continue;

		Vect_line_parallel(Points, -buffer, tolerance, 1, BPoints);
		if (BPoints->n_points > 3) {
		    dig_find_area_poly(Points, &isle_size);
		    dig_find_area_poly(BPoints, &inner_size);
		    /* area size of inner contour must be smaller than isle size */
		    if (fabs(inner_size) < fabs(isle_size)) {
			Vect_write_line(&Out, GV_BOUNDARY, BPoints, BCats);
			line_id = Vect_write_line(&Buf, GV_BOUNDARY, BPoints, Cats);
			arr_bc[buffers_count].inner[i] = line_id;
		    }
		    else {
			arr_bc[buffers_count].inner[i] = -1;
		    }
		}
		else {
		    arr_bc[buffers_count].inner[i] = -1;
		}
	    }
	    buffers_count++;
	}
    }

    if (debug == DEBUG_BUFFER) {
	stop(&In, &Out, &Buf, bufname, &si, debug);
	exit(EXIT_SUCCESS);
    }

    /* Create areas */

    /* Break lines */
    G_message(_("Building parts of topology..."));
    Vect_build_partial(&Out, GV_BUILD_BASE);

    /* Warning: snapping must be done, otherwise colinear boundaries are not broken and 
     * topology cannot be built (the same angle). But snapping distance must be very, very 
     * small, otherwise counterclockwise boundaries can appear in areas outside the buffer.
     * I have done some tests on real data (projected) and threshold 1e-8 was not enough,
     * Snapping threshold 1e-7 seems to work. Don't increase until we find example 
     * where it is not sufficient. */
    /* TODO: look at snapping threshold better, calculate some theoretical value to avoid
     * the same angles of lines at nodes, don't forget about LongLat data, probably
     * calculate different threshold for each map, depending on map's bounding box */
    G_message(_("Snapping boundaries..."));
    Vect_snap_lines(&Out, GV_BOUNDARY, 1e-7, NULL);

    G_message(_("Breaking polygons..."));
    Vect_break_polygons(&Out, GV_BOUNDARY, NULL);

    G_message(_("Removing duplicates..."));
    Vect_remove_duplicates(&Out, GV_BOUNDARY, NULL);

    G_message(_("Breaking boundaries..."));
    Vect_break_lines(&Out, GV_BOUNDARY, NULL);

    G_message(_("Removing duplicates..."));
    Vect_remove_duplicates(&Out, GV_BOUNDARY, NULL);

    /* Dangles and bridges don't seem to be necessary if snapping is small enough. */
    /*
       G_message (  "Removing dangles..." );
       Vect_remove_dangles ( &Out, GV_BOUNDARY, -1, NULL, stderr );

       G_message (  "Removing bridges..." );
       Vect_remove_bridges ( &Out, NULL, stderr );
     */

    G_message(_("Attaching islands..."));
    Vect_build_partial(&Out, GV_BUILD_ATTACH_ISLES);

    /* Calculate new centroids for all areas */
    nareas = Vect_get_num_areas(&Out);
    Areas = (char *)G_calloc(nareas + 1, sizeof(char));

    G_message(_("Selecting areas..."));
    for (area = 1; area <= nareas; area++) {
	double x, y;

	G_percent(area, nareas, 2);
	Areas[area] = 0;

	G_debug(3, "area = %d", area);

	if (!Vect_area_alive(&Out, area))
	    continue;

	ret = Vect_get_point_in_area(&Out, area, &x, &y);
	if (ret < 0) {
	    G_warning(_("Cannot calculate area centroid"));
	    continue;
	}

	ret = point_in_buffer(arr_bc, &si, &Buf, x, y);

	if (ret) {
	    G_debug(3, "  -> in buffer");
	    Areas[area] = 1;
	}
	else
	    G_debug(3, "  -> not in buffer");

	/* Write out centroid (all centroids, so that it is visible for debug) */
	if (debug == DEBUG_CLEAN) {

	    Vect_reset_cats(Cats);
	    if (Areas[area])
		Vect_cat_set(Cats, 1, 1);

	    Vect_reset_line(Points);
	    Vect_append_point(Points, x, y, 0.);
	    Vect_write_line(&Out, GV_CENTROID, Points, Cats);
	}
    }

    if (debug == DEBUG_CLEAN) {
	stop(&In, &Out, &Buf, bufname, &si, debug);
	exit(EXIT_SUCCESS);
    }

    /* Make a list of boundaries to be deleted (both sides inside) */
    nlines = Vect_get_num_lines(&Out);
    G_debug(3, "nlines = %d", nlines);
    Lines = (char *)G_calloc(nlines + 1, sizeof(char));

    G_message(_("Generating list of boundaries to be deleted..."));
    for (line = 1; line <= nlines; line++) {
	int j, side[2], areas[2];

	G_percent(line, nlines, 2);
	Lines[line] = 0;

	G_debug(3, "line = %d", line);

	if (!Vect_line_alive(&Out, line))
	    continue;

	Vect_get_line_areas(&Out, line, &side[0], &side[1]);

	for (j = 0; j < 2; j++) {
	    if (side[j] == 0) {	/* area/isle not build */
		areas[j] = 0;
	    }
	    else if (side[j] > 0) {	/* area */
		areas[j] = side[j];
	    }
	    else {		/* < 0 -> island */
		areas[j] = Vect_get_isle_area(&Out, abs(side[j]));
	    }
	}

	G_debug(3, " areas = %d , %d -> Areas = %d, %d", areas[0], areas[1],
		Areas[areas[0]], Areas[areas[1]]);
	if (Areas[areas[0]] && Areas[areas[1]])
	    Lines[line] = 1;
    }
    G_free(Areas);

    /* Delete boundaries */
    G_message(_("Deleting boundaries..."));
    for (line = 1; line <= nlines; line++) {
	G_percent(line, nlines, 2);
	if (Lines[line]) {
	    G_debug(3, " delete line %d", line);
	    Vect_delete_line(&Out, line);
	}
    }

    G_free(Lines);

    /* Create new centroids */
    Vect_reset_cats(Cats);
    Vect_cat_set(Cats, 1, 1);
    nareas = Vect_get_num_areas(&Out);

    G_message(_("Calculating centroids for areas..."));    
    for (area = 1; area <= nareas; area++) {
	double x, y;

	G_percent(area, nareas, 2);
	G_debug(3, "area = %d", area);

	if (!Vect_area_alive(&Out, area))
	    continue;

	ret = Vect_get_point_in_area(&Out, area, &x, &y);
	if (ret < 0) {
	    G_warning(_("Cannot calculate area centroid"));
	    continue;
	}

	ret = point_in_buffer(arr_bc, &si, &Buf, x, y);

	if (ret) {
	    Vect_reset_line(Points);
	    Vect_append_point(Points, x, y, 0.);
	    Vect_write_line(&Out, GV_CENTROID, Points, Cats);
	}
    }

    G_message(_("Attaching centroids..."));
    Vect_build_partial(&Out, GV_BUILD_CENTROIDS);

    stop(&In, &Out, &Buf, bufname, &si, debug);
    exit(EXIT_SUCCESS);
}
