/* Author: Radim Blazek
*
* added color support: Markus Neteler
*/

#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "plot.h"
#include "colors.h"
#include "dbmi.h"

/* TODO: should use 24bit instead of 16 colors, maybe implement
   predefined color tables? */
struct rgb_color {
        unsigned char R, G, B;
       };
static const int palette_ncolors = 16;
static struct rgb_color palette[16] =  {
	{198, 198, 198}, /*  1: light gray */
	{127, 127, 127}, /*  2: medium/dark gray */
	{255,   0,   0}, /*  3: bright red */
	{139,   0,   0}, /*  4: dark red */
	{  0, 255,   0}, /*  5: bright green */
	{  0, 139,   0}, /*  6: dark green */
	{  0,   0, 255}, /*  7: bright blue */
	{  0,   0, 139}, /*  8: dark blue   */
	{255, 255,   0}, /*  9: yellow */
	{139, 126,  10}, /* 10: olivey brown */
	{255, 165,   0}, /* 11: orange */
	{255, 192, 203}, /* 12: pink   */
	{255,   0, 255}, /* 13: magenta */
	{139,   0, 139}, /* 14: dark magenta */
	{  0, 255, 255}, /* 15: cyan */
	{  0, 139, 139}  /* 16: dark cyan */
};

int darea ( struct Map_info *Map, struct cat_list *Clist, int bcolor, int fcolor, 
	     int chcat, int id_flag, int table_colors_flag, int cats_color_flag, struct Cell_head *window) {
    int    num, area, isle, n_isles, n_points;
    double xl, yl;
    struct line_pnts *Points, *IPoints;
    struct line_cats *Cats;
    int cat, centroid;
    int red, grn, blu;

    struct field_info *fi=NULL;
    dbDriver *driver;
    dbHandle handle;
    dbString stmt, valstr;
    dbCursor cursor;
    dbTable  *table;
    dbColumn *column;
    char buf[2000], colorstring[12]; /* RRR:GGG:BBB */
    int more, ret;
    unsigned char which;

    G_debug (1, "display areas:");
    Points = Vect_new_line_struct ();
    IPoints = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();

    if( table_colors_flag ) {
      /* for reading RRR:GGG:BBB color strings from table */
      db_init_string (&stmt);
      db_init_string (&valstr);
      fi = Vect_get_field( Map, Clist->field);
      if ( fi == NULL )   G_fatal_error ("Cannot read field info");
      driver = db_start_driver(fi->driver);
      if (driver == NULL) G_fatal_error("Cannot open driver %s", fi->driver);
      db_init_handle (&handle);
      db_set_handle (&handle, fi->database, NULL);
      if (db_open_database(driver, &handle) != DB_OK)
	  G_fatal_error("Cannot open database %s", fi->database);
    }
    
    num = Vect_get_num_areas(Map);
    G_debug (2, "n_areas = %d", num);
    
    for ( area = 1; area <= num; area++ ) {
	int i;
	BOUND_BOX box;
        G_debug (3, "area = %d", area);

	if ( !Vect_area_alive (Map, area) ) continue;
	
	/* Check box */
	Vect_get_area_box (Map, area, &box);
	if ( box.N < window->south || box.S > window->north || 
		box.E < window->west || box.W > window->east) {

	    if ( window->proj != PROJECTION_LL )
		continue;
	    else {   /* out of bounds for -180 to 180, try 0 to 360 as well */
		if ( box.N < window->south || box.S > window->north )
		    continue;
		if ( box.E+360 < window->west || box.W+360 > window->east )
		    continue;
	    }
	}

        if ( chcat ) /* check category: where_opt or cat_opt used */
        { 
	     if ( id_flag ) {
		 if ( !(Vect_cat_in_cat_list (area, Clist)) )
		     continue;
	     } else {
		 int found = 0;

		 centroid = Vect_get_area_centroid ( Map, area ); 
		 G_debug (3, "centroid = %d", centroid);
		 if ( centroid < 1 ) continue;
		 Vect_read_line (Map, Points, Cats, centroid );
		 
		 for ( i = 0; i < Cats->n_cats; i++ ) {
		     G_debug (3, "  centroid = %d, field = %d, cat = %d", centroid, 
			                 Cats->field[i], Cats->cat[i]);

		     if ( Cats->field[i] == Clist->field && Vect_cat_in_cat_list ( Cats->cat[i], Clist) ) {
			 found = 1;
			 break;
		     }
		 }
		 if (!found) continue;
	     } /* end else */
        } /* end if id_flag */
        G_debug (3, "display area %d", area);

        /* fill */
	Vect_get_area_points ( Map, area, Points );   
        G_debug (3, "n_points = %d", Points->n_points);
  
	n_points = Points->n_points;
	xl = Points->x[n_points-1];
	yl = Points->y[n_points-1];
	n_isles = Vect_get_area_num_isles ( Map, area );   
        for ( i = 0; i < n_isles; i++) {
	    isle = Vect_get_area_isle ( Map, area, i );   
	    Vect_get_isle_points ( Map, isle, IPoints );
	    Vect_append_points ( Points, IPoints, GV_FORWARD);
	    Vect_append_point ( Points, xl, yl, 0.0 ); /* ??? */
	}
	
	if( table_colors_flag ) {
	     cat=Vect_get_area_cat ( Map, area, Clist->field );
	     if( cat >= 0 ){
		G_debug (3, "display area %d, centroid %d, cat %d", area, centroid, cat);

		/* Read RGB colors from db for current area # */
		db_init_string (&stmt);
		db_init_string (&valstr);
		sprintf ( buf, "select %s from %s where %s = %d", "GRASSRGB", fi->table,  fi->key, cat);
		G_debug (3, "SQL: %s", buf);
		db_append_string ( &stmt, buf);
		if (db_open_select_cursor(driver, &stmt, &cursor, DB_SEQUENTIAL) != DB_OK)
			G_fatal_error ("Cannot select attributes for area # %d.", area);

		table = db_get_cursor_table (&cursor);
		column = db_get_table_column(table, 0); /* first column */

		if(db_fetch (&cursor, DB_NEXT, &more) != DB_OK) continue;
		db_convert_column_value_to_string (column, &valstr);
		sprintf (colorstring, "%s", db_get_string(&valstr));

		/* only draw if GRASSRGB was defined */
		if (strlen(colorstring) != 0) {
			G_debug(3, "area centroid %d: colorstring: %s", centroid, colorstring);
	
			ret =  G_str_to_color(colorstring, &red, &grn, &blu);
			if ( ret == 1 ) {
				R_RGB_color ((unsigned char) red, (unsigned char) grn, (unsigned char) blu);
				G_debug(3, "area:%d  cat %d r:%d g:%d b:%d", area, cat, red, grn, blu);
				G_plot_polygon ( Points->x, Points->y, Points->n_points);
			} else if ( ret == 0 ) { /* error */
				G_warning("Error in color definition column GRASSRGB, area %d with cat %d: colorstring %s (not drawing this area)", area, cat, colorstring);
			}
		}
	     } /* end if cat */
	} /* end if table_colors_flag */
 	
	/* random colors */
	if( cats_color_flag ) {
	    cat=Vect_get_area_cat ( Map, area, Clist->field );
	     if( cat >= 0 ){
		G_debug (3, "display area %d, centroid %d, cat %d", area, centroid, cat);
		/* fetch color number from category */
		which = (cat % palette_ncolors);
		G_debug(3,"cat:%d which color:%d r:%d g:%d b:%d",cat, which,palette[which].R,palette[which].G,palette[which].B);
		R_RGB_color (palette[which].R,palette[which].G,palette[which].B);
		G_plot_polygon ( Points->x, Points->y, Points->n_points);
	     }
	}
	
	if ( fcolor > -1 && !table_colors_flag && !cats_color_flag) {
                R_color(fcolor) ;
		G_plot_polygon ( Points->x, Points->y, Points->n_points);
	}
	
	/* boundary */
	if ( bcolor > -1 ) {
	    int i, j;
	    Vect_get_area_points ( Map, area, Points );   
	    R_color(bcolor) ;
	    for ( i = 0; i < Points->n_points - 1; i++) { 
		G_plot_line (Points->x[i], Points->y[i], Points->x[i+1], Points->y[i+1]);
	    }
	    for ( i = 0; i < n_isles; i++) {
		isle = Vect_get_area_isle ( Map, area, i );   
		Vect_get_isle_points ( Map, isle, Points );
		for ( j = 0; j < Points->n_points - 1; j++) { 
		    G_plot_line (Points->x[j], Points->y[j], Points->x[j+1], Points->y[j+1]);
		}
	    }
	}
    } /* end for */

    if( table_colors_flag ) {
       db_close_database(driver);
       db_shutdown_driver(driver);
    }

    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);
    
    return 0;
}

