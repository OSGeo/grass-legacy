#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "plot.h"
#include "colors.h"
#include "dbmi.h"

int darea ( struct Map_info *Map, struct cat_list *Clist, int bcolor, int fcolor, 
	     int chcat, int id_flag, int table_colors_flag ) {
    int    i, j, num, area, isle, n_isles, n_points;
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
    char buf[2000], colorstring[8]; /* RR:GG:BB */
    int more, ret;

    G_debug (1, "display areas:");
    Points = Vect_new_line_struct ();
    IPoints = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();

    if( table_colors_flag ) {
      /* for reading RR:GG:BB color strings from table */
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
        G_debug (3, "area = %d", area);

	if ( !Vect_area_alive (Map, area) ) continue;
        if ( chcat ) /* check category: where_opt or cat_opt used */
          { 
	     if ( id_flag ) {
		 if ( !(Vect_cat_in_cat_list (area, Clist)) )
		     continue;
	     } else {
		 centroid = Vect_get_area_centroid ( Map, area ); 
		 G_debug (3, "centroid = %d", centroid);
		 if ( centroid < 1 ) continue;
		 Vect_read_line (Map, Points, Cats, centroid );
		 if ( Vect_cat_get(Cats, Clist->field, &cat) ) { 
		     if ( !(Vect_cat_in_cat_list (cat, Clist)) )
			 continue;
		 } else {
		     continue;
		 } 
	     }
          }
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
	     }
	}
 	
	/* TODO: implement random colors here */
	
	if ( fcolor > -1 && ! table_colors_flag ) {
                R_color(fcolor) ;
		G_plot_polygon ( Points->x, Points->y, Points->n_points);
	}
	
	/* boundary */
	if ( bcolor > -1 ) {
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
    }

   if( table_colors_flag ) {
       db_close_database(driver);
       db_shutdown_driver(driver);
   }

    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);
    
    return 0;
}

