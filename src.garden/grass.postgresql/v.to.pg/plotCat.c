#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "dbvect.h"
#include "glocale.h"

extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_get_d_east();
extern double D_d_to_u_row(), D_d_to_u_col();

extern int D_move_abs();
extern int D_cont_abs();


int plotCat(name, mapset, points, vect_cat, Map, fillcolr, pg_conn)
     char *name, *mapset;
     struct line_pnts *points;
     int vect_cat, fillcolr;
     struct Map_info *Map;
     PGconn *pg_conn;
{
    double *x, *y;
    int *list, count, idx, i, j, jk;
    int ret, n, np, a_index, k_index, v_att;
    double N, S, E, W;
    struct Cell_head window;
    int *find_area(), *find_line();
    P_AREA *pa;
    double **xs, **ys;
    int rings;
    int *rpnts;
    struct line_pnts *points_i;

    char u_str[128] = "";
    char *tmp_string;
    char *tmp_string_i;


    PGresult *res1;



    fflush(stdout);

    G_get_set_window(&window);
#ifndef X_DISPLAY_MISSING
    G_setup_plot(D_get_d_north(), D_get_d_south(), D_get_d_west(),
		 D_get_d_east(), D_move_abs, D_cont_abs);
#endif

    /* list is the list of cat numbers (ie. dig_att vals) */


    if (!strncmp(vtype_string, "area", 4)
	&& (list = find_area(vect_cat, &count, Map)))
    {
	for (i = 0; i < count; i++) {

	    idx = list[i];

	    v_att = V2_area_att(Map, idx);

	    V2_get_area_bbox(Map, idx, &N, &S, &E, &W);
	    if (S > window.north || N < window.south ||
		W > window.east || E < window.west)
		continue;
	    k_index = Map->Area[idx].att;

	    if (!k_index)
		continue;

	    a_index = Map->Att[k_index].index;

	    V2_get_area(Map, a_index, &pa);

	    rings = pa->n_isles + 1;
	    xs = (double **) G_malloc(sizeof(double *) * rings);
	    ys = (double **) G_malloc(sizeof(double *) * rings);
	    rpnts = (int *) G_malloc(sizeof(int) * rings);

	    Vect_get_area_points(Map, a_index, points);

	    rpnts[0] = points->n_points;
	    xs[0] = (double *) G_malloc(sizeof(double) * rpnts[0]);
	    ys[0] = (double *) G_malloc(sizeof(double) * rpnts[0]);
	    Vect_copy_pnts_to_xy(points, xs[0], ys[0], &rpnts[0]);

	    tmp_string =
		(char *) G_malloc(points->n_points * (2 * MAXFLSIZE + 4)
				  + MAXFLDNAMESZ);

	    if (!to_postgis) {

		snprintf(tmp_string, MAXFLDNAMESZ,
			 "INSERT into %s_bnd (%s, num, ex, boundary) values ('%d','%d','t','(",
			 table_string, key_string, v_att, i + 1);

		for (j = 0; j < points->n_points - 1; j++) {

#ifndef X_DISPLAY_MISSING
		    G_plot_line(points->x[j], points->y[j],
				points->x[j + 1], points->y[j + 1]);
#endif

		    if (j != points->n_points - 2)
			snprintf(u_str, 128, "(%f,%f),", points->x[j],
				 points->y[j]);
		    else
			snprintf(u_str, 128, "(%f,%f)", points->x[j + 1],
				 points->y[j + 1]);
		    strcat(tmp_string, u_str);
		    if (verbose)
			total_vertices++;

		}

		snprintf(u_str, 4, ")')");

		strcat(tmp_string, u_str);

		if (verbose) {
		    printf("n_points is %d, v_att is %d, count is %d\n",
			   points->n_points, v_att, count);
		    printf("Executing\n%s;\n\n", tmp_string);
		    total_vects++;
		}

		res1 = PQexec(pg_conn, tmp_string);

		if (!res1 || PQresultStatus(res1) != PGRES_COMMAND_OK) {
		    fprintf(stderr, _("Error executing command:%sExiting.\n"),
			   PQerrorMessage(pg_conn));
		    PQclear(res1);
		    PQfinish(pg_conn);
		    G_free(tmp_string);
		    exit(-1);
		}

		PQclear(res1);

		G_free(tmp_string);

		points_i = Vect_new_line_struct();

		for (j = 0; j < pa->n_isles; j++) {
		    Vect_get_isle_points(Map, pa->isles[j], points_i);
		    rpnts[j + 1] = points_i->n_points;
		    xs[j + 1] =
			(double *) G_malloc(sizeof(double) * rpnts[j + 1]);
		    ys[j + 1] =
			(double *) G_malloc(sizeof(double) * rpnts[j + 1]);
		    Vect_copy_pnts_to_xy(points_i, xs[j + 1], ys[j + 1],
					 &rpnts[j + 1]);

		    tmp_string =
			(char *) G_malloc(points_i->n_points *
					  (2 * MAXFLSIZE + 4)
					  + MAXFLDNAMESZ);
		    snprintf(tmp_string, MAXFLDNAMESZ,
			     "INSERT into %s_bnd (%s, num, ex, boundary) values ('%d','%d','f','(",
			     table_string, key_string, v_att, i + 1);

		    for (jk = 0; jk < points_i->n_points - 1; jk++) {

#ifndef X_DISPLAY_MISSING
			G_plot_line(points_i->x[jk], points_i->y[jk],
				    points_i->x[jk + 1], points_i->y[jk + 1]);
#endif

			if (jk != points_i->n_points - 2)
			    snprintf(u_str, 128, "(%f,%f),", points_i->x[jk],
				     points_i->y[jk]);
			else
			    snprintf(u_str, 128, "(%f,%f)",
				     points_i->x[jk + 1],
				     points_i->y[jk + 1]);
			strcat(tmp_string, u_str);

			if (verbose)
			    total_vertices++;

		    }
		    snprintf(u_str, 4, ")')");

		    strcat(tmp_string, u_str);

		    if (verbose) {
			printf("n_points_i is %d, v_att is %d, count is %d\n",
			       points_i->n_points, v_att, count);
			printf("Executing\n%s;\n\n", tmp_string);

			total_vects++;
		    }
		    res1 = PQexec(pg_conn, tmp_string);
		    if (!res1 || PQresultStatus(res1) != PGRES_COMMAND_OK) {
			fprintf(stderr, _("Error executing command:%sExiting.\n"),
			       PQerrorMessage(pg_conn));
			PQclear(res1);
			PQfinish(pg_conn);
			G_free(tmp_string);
			exit(-1);
		    }

		    PQclear(res1);


		    G_free(tmp_string);

		}
	    }
	    else {		/*if to_postgis */

		snprintf(tmp_string, MAXFLDNAMESZ,
			 "INSERT into %s_mpoly (%s, num, grass_poly) values ('%d','%d',GeometryFromText('POLYGON((",
			 table_string, key_string, v_att, i + 1);

		for (j = 0; j < points->n_points - 1; j++) {

#ifndef X_DISPLAY_MISSING
		    G_plot_line(points->x[j], points->y[j],
				points->x[j + 1], points->y[j + 1]);
#endif

		    if (j != points->n_points - 2)
			snprintf(u_str, 128, "%f %f,", points->x[j],
				 points->y[j]);
		    else
			snprintf(u_str, 128, "%f %f", points->x[j + 1],
				 points->y[j + 1]);
		    strcat(tmp_string, u_str);
		    if (verbose)
			total_vertices++;

		}

		if (!pa->n_isles) {
		    snprintf(u_str, 9, "))',-1))");
		}
		else {
		    snprintf(u_str, 3, "),");
		}

		strcat(tmp_string, u_str);

		if (verbose) {
		    printf("n_points is %d, v_att is %d, count is %d\n",
			   points->n_points, v_att, count);

		    total_vects++;
		}

		points_i = Vect_new_line_struct();

		for (j = 0; j < pa->n_isles; j++) {

		    Vect_get_isle_points(Map, pa->isles[j], points_i);
		    rpnts[j + 1] = points_i->n_points;
		    xs[j + 1] =
			(double *) G_malloc(sizeof(double) * rpnts[j + 1]);
		    ys[j + 1] =
			(double *) G_malloc(sizeof(double) * rpnts[j + 1]);
		    Vect_copy_pnts_to_xy(points_i, xs[j + 1], ys[j + 1],
					 &rpnts[j + 1]);

		    tmp_string_i =
			(char *) G_malloc(points_i->n_points *
					  (2 * MAXFLSIZE + 4)
					  + MAXFLDNAMESZ);
		    snprintf(tmp_string_i, MAXFLDNAMESZ, "(");

		    for (jk = 0; jk < points_i->n_points - 1; jk++) {

#ifndef X_DISPLAY_MISSING
			G_plot_line(points_i->x[jk], points_i->y[jk],
				    points_i->x[jk + 1], points_i->y[jk + 1]);
#endif

			if (jk != points_i->n_points - 2)
			    snprintf(u_str, 128, "%f %f,", points_i->x[jk],
				     points_i->y[jk]);
			else
			    snprintf(u_str, 128, "%f %f", points_i->x[jk + 1],
				     points_i->y[jk + 1]);
			strcat(tmp_string_i, u_str);

			if (verbose)
			    total_vertices++;

		    }

		    if (j == pa->n_isles - 1) {
			snprintf(u_str, 9, "))',-1))");
		    }
		    else {
			snprintf(u_str, 3, "),");
		    }

		    strcat(tmp_string_i, u_str);

		    if (verbose) {
			printf("n_points_i is %d, v_att is %d, count is %d\n",
			       points_i->n_points, v_att, count);


			total_vects++;
		    }

		    tmp_string =
			(char *) G_realloc(tmp_string,
					   strlen(tmp_string) +
					   strlen(tmp_string_i) + 1);
		    strcat(tmp_string, tmp_string_i);

		    G_free(tmp_string_i);

		}
		if (verbose)
		    printf("Executing\n%s;\n\n", tmp_string);

		res1 = PQexec(pg_conn, tmp_string);
		if (!res1 || PQresultStatus(res1) != PGRES_COMMAND_OK) {
		    fprintf(stderr, _("Error executing command:%sExiting.\n"),
			   PQerrorMessage(pg_conn));
		    PQclear(res1);
		    PQfinish(pg_conn);
		    G_free(tmp_string);
		    exit(-1);
		}

		PQclear(res1);

		G_free(tmp_string);


	    }			/*end if !to_postgis */
#ifndef X_DISPLAY_MISSING
	    if (fillcolr)
		G_plot_area(xs, ys, rpnts, rings);
#endif

	    Vect_destroy_line_struct(points_i);

	    for (j = 0; j < rings; j++) {
		free(xs[j]);
		free(ys[j]);
	    }
	    free(xs);
	    free(ys);
	    free(rpnts);
	}

	Vect_rewind(Map);


	return 0;

    }				/* end if find_area > 0  */

    if (!strncmp(vtype_string, "line", 4)
	&& (list = find_line(vect_cat, &count, Map))) {
	for (n = 0; n < count; n++) {
	    idx = list[n];

	    v_att = V2_line_att(Map, idx);

	    if (V2_get_line_bbox(Map, idx, &N, &S, &E, &W) < 0) {
		fprintf(stderr, _("\nWARNING: vector file [%s]-read error\n"),
			name);
		return -1;
	    }

	    if (!G_window_overlap(&window, N, S, E, W))
		continue;

	    if (0 > (ret = V2_read_line(Map, points, idx))) {
		if (ret == -2) {
		    G_warning(_("Read error - EOF\n"));
		    return -1;
		}
		else {
		    G_warning(_("Read error\n"));
		    return -1;
		}
	    }


	    tmp_string =
		(char *) G_malloc(points->n_points * (2 * MAXFLSIZE + 4)
				  + MAXFLDNAMESZ);
	    if (!to_postgis)
		snprintf(tmp_string, MAXFLDNAMESZ,
			 "INSERT into %s_arc (%s, num, segment) values ('%d','%d','[",
			 table_string, key_string, v_att, n + 1);
	    else
		snprintf(tmp_string, MAXFLDNAMESZ,
			 "INSERT into %s_mstring (%s, num, grass_line) values ('%d','%d', GeometryFromText('LINESTRING(",
			 table_string, key_string, v_att, n + 1);

	    np = points->n_points;
	    x = points->x;
	    y = points->y;
	    for (i = 1; i < np; i++) {

#ifndef X_DISPLAY_MISSING
		G_plot_line(x[0], y[0], x[1], y[1]);
#endif
		if (i != points->n_points - 1)
		    if (!to_postgis)
			snprintf(u_str, 128, "(%f,%f),", x[0], y[0]);
		    else
			snprintf(u_str, 128, "%f %f,", x[0], y[0]);
		else if (!to_postgis)
		    snprintf(u_str, 128, "(%f,%f)", x[1], y[1]);
		else
		    snprintf(u_str, 128, "%f %f", x[0], y[0]);
		strcat(tmp_string, u_str);

		if (verbose)
		    total_vertices++;

		x++;
		y++;
	    }

	    if (!to_postgis)
		snprintf(u_str, 4, "]')");
	    else
		snprintf(u_str, 8, ")',-1))");

	    strcat(tmp_string, u_str);

	    if (verbose) {
		printf("n_points is %d, v_att is %d, count is %d\n",
		       points->n_points, v_att, count);
		printf("Executing\n%s;\n\n", tmp_string);
		total_vects++;
	    }

	    res1 = PQexec(pg_conn, tmp_string);
	    if (!res1 || PQresultStatus(res1) != PGRES_COMMAND_OK) {
		fprintf(stderr, _("Error executing command:%sExiting.\n"),
		       PQerrorMessage(pg_conn));
		PQclear(res1);
		PQfinish(pg_conn);
		G_free(tmp_string);
		exit(-1);
	    }

	    PQclear(res1);

	    G_free(tmp_string);

	}			/* end for count */
    }				/* end for lines  */

    return 0;
}
