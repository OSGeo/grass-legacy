#include <string.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "colors.h"
#include "Vect.h"
#include "form.h"
#include "dbmi.h"
#include "what.h"

static int nlines = 50;

#define WDTH 5

int what(int once, int txt, int terse, int width, int mwidth )
{
    int type, edit_mode;
    int row, col;
    int nrows, ncols;
    struct Cell_head window;
    int screen_x, screen_y;
    double east, north;
    int button;
    char east_buf[40], north_buf[40];
    double sq_meters;
    double x1, y1, x2, y2, z = 0, l = 0;
    int notty;
    double maxdist;
    int getz = 0;
    struct field_info *Fi;
    int flash_basecolr, flash_colr;

    plus_t line, area = 0, centroid;
    int i, j;
    struct line_pnts *Points;
    struct line_cats *Cats;
    char buf[1000], *str, title[500];
    dbString html; 
    char *form;
    char *panell;

    if ( terse ) txt = 1; /* force text for terse */
    
    G_get_set_window(&window);

    G_setup_plot(D_get_d_north(), D_get_d_south(), D_get_d_west(),
		 D_get_d_east(), D_move_abs, D_cont_abs);


    G_begin_polygon_area_calculations();
    nrows = window.rows;
    ncols = window.cols;

    screen_x = ((int) D_get_d_west() + (int) D_get_d_east()) / 2;
    screen_y = ((int) D_get_d_north() + (int) D_get_d_south()) / 2;

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();
    db_init_string (&html);

    if (!isatty(fileno(stdout))) notty = 1; /* no terminal */
    else notty = 0;

    panell = G_tempfile();
    flash_basecolr=YELLOW;

    do {
	R_panel_save(panell, R_screen_top(), R_screen_bot(),
		     R_screen_left(), R_screen_rite());

	if (!terse) show_buttons(once);
	R_get_location_with_pointer(&screen_x, &screen_y, &button);
	if (!once) {
	    if (button == 3) {
		R_panel_delete(panell);
		break;
	    }
	    if (button == 2) {
		R_panel_delete(panell);
		flash_basecolr++;
		if (flash_basecolr > MAX_COLOR_NUM) flash_basecolr = 1;
		continue;
	    }
	}

	east = D_d_to_u_col((double) screen_x);
	north = D_d_to_u_row((double) screen_y);

	row = (window.north - north) / window.ns_res;
	col = (east - window.west) / window.ew_res;
	if (row < 0 || row >= nrows) continue;
	if (col < 0 || col >= ncols) continue;

	x1 = D_d_to_u_col((double) (screen_x - WDTH));
	y1 = D_d_to_u_row((double) (screen_y - WDTH));
	x2 = D_d_to_u_col((double) (screen_x + WDTH));
	y2 = D_d_to_u_row((double) (screen_y + WDTH));

	x1 = fabs(x2 - x1);
	y1 = fabs(y2 - y1);

	if (x1 > y1) maxdist = x1;
	else maxdist = y1;
	G_debug(1, "Maximum distance in map units = %f\n", maxdist);

	flash_colr = flash_basecolr;
	F_clear ();
	for (i = 0; i < nvects; i++) {
	    Vect_reset_cats ( Cats );
	    /* Try to find point first and only if no one was found try lines,
	       *  otherwise point on line could not be selected ans similarly for areas */
	    line = Vect_find_line(&Map[i], east, north, 0, GV_POINT | GV_CENTROID, maxdist, 0, 0);
	    if (line == 0) line = Vect_find_line(&Map[i], east, north, 0,
			                         GV_LINE | GV_BOUNDARY | GV_FACE, maxdist, 0, 0);

	    area = 0; 
	    if ( line == 0 ) {
	        area = Vect_find_area(&Map[i], east, north);
	        getz = Vect_tin_get_z(&Map[i], east, north, &z, NULL, NULL);
	    } 

	    G_debug (2, "line = %d, area = %d", line, area);

	    if ( !i && txt ) {
		G_format_easting(east, east_buf, G_projection());
		G_format_northing(north, north_buf, G_projection());
		fprintf(stdout, "\n%s(E) %s(N)\n", east_buf, north_buf);
		if (notty) fprintf(stderr, "\n%s(E) %s(N)\n", east_buf, north_buf);
		nlines++;
	    }

	    strcpy(buf, vect[i]);
	    if ((str = strchr(buf, '@'))) *str = 0;

	    if ( txt ) {
	        fprintf(stdout, "\n%*s in %-*s  ", width, Map[i].name, mwidth, Map[i].mapset); 
	        if (notty) fprintf(stderr, "\n%*s in %-*s  ", width, Map[i].name, mwidth, Map[i].mapset);
	        nlines++;
	    }

	    if (line + area == 0) {
                if ( txt ) {
		    fprintf(stdout, "Nothing Found.\n");
		    if (notty) fprintf(stderr, "Nothing Found.\n");
		    nlines++;
		}
		continue;
	    } else {
		/* Start form */
		if ( !txt ) {
		    sprintf (title, "%s", Map[i].name );
		    db_set_string (&html, ""); 
		    db_append_string (&html, "<HTML><HEAD><TITLE>Form</TITLE><BODY>"); 
		    sprintf ( buf,"map: '%s'<BR>mapset: '%s'<BR>", Map[i].name, Map[i].mapset);
		    db_append_string (&html, buf);
		}
	    }

	    if ( line >  0) {
		type = Vect_read_line(&Map[i], Points, Cats, line);
		switch ( type ) {
		    case GV_POINT:
			sprintf ( buf, "Point" );
			break;
		    case GV_LINE:
			sprintf ( buf, "Line" );
			break;
		    case GV_BOUNDARY:
			sprintf ( buf, "Boundary" );
			break;
		    case GV_FACE:
			sprintf ( buf, "Face" );
			break;
		    case GV_CENTROID:
			sprintf ( buf, "Centroid" );
			break;
		    default:
			sprintf ( buf, "Unknown" );
		}
		if ( type & GV_LINES ) {
		    if ( G_projection() == 3) l = Vect_line_geodesic_length ( Points );
		    else l = Vect_line_length ( Points );
		}

		if ( txt ) {
		    fprintf(stdout, "%s\n", buf);
		    if ( type & GV_LINES ) fprintf(stdout, "length %f\n", l);
		} else { 
		    db_append_string (&html, "feature type: " );
		    db_append_string (&html, buf );
		    db_append_string (&html, "<BR>" );

		    if ( type & GV_LINES ) {  
		        sprintf ( buf, "length: %f<BR>", l );
		        db_append_string (&html, buf );
		    }
		}
		flash_line(&Map[i], line, Points, BLACK);
		flash_line(&Map[i], line, Points, flash_colr);
	    }

	    if (area > 0) {
		if (Map[i].head.with_z && getz) {
		    if ( txt ) fprintf(stdout, "Area height: %f\n", z);
		    else {
			sprintf(buf, "feature type: Area<BR>height: %f<BR>", z );
		        db_append_string (&html, buf);
		    }
		} else {
		    if ( txt ) fprintf(stdout, "Area\n");
		    else {
			sprintf(buf, "feature type: Area<BR>");
		        db_append_string (&html, buf);
		    }
		}

		sq_meters = Vect_get_area_area(&Map[i], area);
		if ( txt ) {
		    fprintf(stdout, "Size - Sq Meters: %.3f\t\tHectares: %.3f\n",
			    sq_meters, (sq_meters / 10000.));

		    fprintf(stdout, "           Acres: %.3f\t\tSq Miles: %.4f\n",
			    ((sq_meters * 10.763649) / 43560.),
			    ((sq_meters * 10.763649) / 43560.) / 640.);
		    if (notty) {
			fprintf(stderr,
				"Size - Sq Meters: %.3f\t\tHectares: %.3f\n",
				sq_meters, (sq_meters / 10000.));

			fprintf(stderr,
				"           Acres: %.3f\t\tSq Miles: %.4f\n",
				((sq_meters * 10.763649) / 43560.),
				((sq_meters * 10.763649) / 43560.) / 640.);
		    }
		    nlines += 3;
		} else {
		    sprintf( buf, "area size: %f<BR>", sq_meters );
                    db_append_string (&html, buf);
		}

		centroid = Vect_get_area_centroid(&Map[i], area);
		if (centroid > 0) {
		    Vect_read_line(&Map[i], Points, Cats, centroid);
		}

		flash_area(&Map[i], area, Points, BLACK);
		flash_area(&Map[i], area, Points, flash_colr);
	    }

	    if ( Cats->n_cats > 0 ) {
		for (j = 0; j < Cats->n_cats; j++) {
		    G_debug(2, "field = %d category = %d", Cats->field[j], Cats->cat[j]);
		    if ( txt ) {
			fprintf( stdout, "field: %d\ncategory: %d\n", Cats->field[j], Cats->cat[j] );
		    } else { 
			db_append_string (&html, "<HR><BR>");
			sprintf(buf, "field: %d<BR>category: %d<BR>", Cats->field[j], Cats->cat[j] );
			db_append_string (&html, buf);
		    }
		    Fi = Vect_get_field_info(Map[i].name, Map[i].mapset, Cats->field[j]);
		    if (Fi == NULL) {
			if ( txt ) {
			    fprintf( stdout, "Database connection not defined\n" );
			} else { 
			    db_append_string (&html, "Database connection not defined<BR>" );
			}
		    } else {
			if ( txt ) {
			    fprintf( stdout, "driver: %s\ndatabase: %s\ntable: %s\nkey column: %s\n",
				     Fi->driver, Fi->database, Fi->table, Fi->key);
			} else { 
			    sprintf(buf, "driver: %s<BR>database: %s<BR>table: %s<BR>key column: %s<BR>",
				         Fi->driver, Fi->database, Fi->table, Fi->key);
			    db_append_string (&html, buf);
			}
			
			if ( strcmp(Map[i].mapset, G_mapset() ) == 0 ) edit_mode = F_EDIT;
			else edit_mode = F_VIEW;
			F_generate ( Fi->driver, Fi->database, Fi->table, Fi->key, Cats->cat[j], 
				 NULL, NULL, edit_mode, F_HTML, &form);
			
			if ( txt ) {
			    fprintf( stdout, "%s", db_get_string ( &html) );
			} else {
			    db_append_string (&html, form); 
			}
			G_free (form);
			G_free(Fi);
		    }
		}
	    }
	    fflush(stdout);
	    if (!txt ) {
		db_append_string (&html, "</BODY></HTML>"); 
		G_debug ( 3, db_get_string (&html) ); 
		F_open ( title, db_get_string(&html) );
	    }
	 
	 flash_colr++; if (flash_colr > MAX_COLOR_NUM) flash_colr=1;
	}

	R_panel_restore(panell);
	R_panel_delete(panell);

    } while (!once);
    Vect_destroy_line_struct(Points);

    return 0;
}

/* TODO */
int show_buttons(int once)
{
    if (once) {
	fprintf(stderr, "\nClick mouse button on desired location\n\n");
	nlines = 3;
    }
    else if (nlines >= 18) {	/* display prompt every screen full */
	fprintf(stderr, "\n");
	fprintf(stderr, "Buttons\n");
	fprintf(stderr, " Left:  what's here\n");
	fprintf(stderr, " Middle: toggle flash color\n");
	fprintf(stderr, " Right: quit\n");
	nlines = 5;
    }

    return 0;
}
