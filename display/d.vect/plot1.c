/* plot1() - Level One vector reading */

#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "plot.h"
#include "local_proto.h"
#include "symbol.h"
#include "glocale.h"
#include "dbmi.h"

int plot1 (
    struct Map_info *Map, int type, int area, 
    struct cat_list *Clist, int color, int fcolor, int chcat, SYMBOL *Symb, int size, int id_flag,
    int table_colors_flag, int cats_color_flag)
{
    int i, j, k, ltype, nlines = 0, line, cat = -1;
    double *x, *y, xd, yd, xd0 = 0, yd0 = 0;
    struct line_pnts *Points, *PPoints;
    struct line_cats *Cats;
    double msize;
    SYMBPART *part;
    SYMBCHAIN *chain;
    int x0, y0, xp, yp;

    struct field_info *fi = NULL;
    dbDriver *driver = NULL;
    dbCatValArray cvarr;
    dbCatVal *cv_rgb = NULL;
    int nrec;

    int rgb = 0; /* 0|1 */
    char colorstring[12]; /* RRR:GGG:BBB */
    int red, grn, blu;
    unsigned char which;

    msize = size * ( D_d_to_u_col(2.0) - D_d_to_u_col(1.0 ) ); /* do it better */
    
    Points = Vect_new_line_struct ();
    PPoints = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();

    if( table_colors_flag ) {
      /* for reading RRR:GGG:BBB color strings from table */
      db_CatValArray_init (&cvarr);     

      fi = Vect_get_field (Map, Clist -> field);
      if (fi == NULL) {
	G_fatal_error (_("Cannot read field info"));
      }
      
      driver = db_start_driver_open_database(fi->driver, fi->database);
      if (driver == NULL)
	G_fatal_error (_("Cannot open database %s by driver %s"), fi->database, fi->driver);

      nrec = db_select_CatValArray(driver, fi->table, fi->key, 
				   "GRASSRGB", NULL, &cvarr);

      G_debug (3, "nrec (grassrgb) = %d", nrec);

      if (cvarr.ctype != DB_C_TYPE_STRING)
	G_fatal_error (_("Column type (grassrgb) not supported"));

      if ( nrec < 0 ) G_fatal_error (_("Cannot select data (grassrgb) from table"));

      G_debug (2, "\n%d records selected from table", nrec);

      db_close_database_shutdown_driver(driver);

      for ( i = 0; i < cvarr.n_values; i++ ) {
	G_debug (4, "cat = %d grassrgb = %s", cvarr.value[i].cat, 
		 db_get_string(cvarr.value[i].val.s));

	/* test for background color */
	if (test_bg_color (db_get_string(cvarr.value[i].val.s))) {
	  G_warning (_("Category <%d>: Line color and background color are the same!"),
		     cvarr.value[i].cat);
	}
      }
    }

    Vect_rewind ( Map );
    
    /* Is it necessary to reset line/label color in each loop ? */

    if ( color > -1 && !table_colors_flag && !cats_color_flag) R_color(color) ;

    if ( Vect_level ( Map ) >= 2 )
	nlines = Vect_get_num_lines ( Map );

    line = 0;
    while (1) {
	if ( Vect_level ( Map ) >= 2 ) { 
	    line++;
	    if ( line > nlines ) return 0;
	    if ( !Vect_line_alive (Map, line) ) continue;
	    ltype =  Vect_read_line (Map, Points, Cats, line);   
	} else {
	    ltype =  Vect_read_next_line (Map, Points, Cats);   
	    switch ( ltype )
	    {
	    case -1:
		fprintf (stderr, _("\nERROR: vector file - can't read\n" ));
		return -1;
	    case -2: /* EOF */
		return  0;
	    }
	}

	if ( !(type & ltype) ) continue;

	if ( chcat ) {
	     int found = 0;

	     if ( id_flag ) { /* use line id */
		 if ( !(Vect_cat_in_cat_list ( line, Clist)) )
		     continue;
	     } else {
		 for ( i = 0; i < Cats->n_cats; i++ ) {
		     if ( Cats->field[i] == Clist->field && Vect_cat_in_cat_list ( Cats->cat[i], Clist) ) {
			 found = 1;
			 cat = Cats->cat[i];
                         break;
                     }
                 }
                 if (!found) continue;
	     }
	}

	if( table_colors_flag ) {

	  cat=Vect_get_line_cat ( Map, line, Clist->field ); /* only first category */
	  
	  if (cat >= 0) {
	    G_debug (3, "display element %d, cat %d", line, cat);
	    
	    /* Read RGB colors from db for current area # */
	   
	    if (db_CatValArray_get_value (&cvarr, cat, &cv_rgb) != DB_OK) {
	      rgb = 0;
	    }
	    else {
	      sprintf (colorstring, "%s", db_get_string(cv_rgb -> val.s));
	    
	      if (strlen(colorstring) != 0) {
		G_debug (3, "element %d: colorstring: %s", line, colorstring);
		
		if ( G_str_to_color(colorstring, &red, &grn, &blu) == 1) {
		  rgb = 1;
		  G_debug (3, "element:%d  cat %d r:%d g:%d b:%d", line, cat, red, grn, blu);
		} 
		else { 
		  rgb = 0;
		  G_warning (_("Error in color definition column GRASSRGB, element %d with cat %d: colorstring %s"), line, cat, colorstring);
		} 
	      }
	      else {
		rgb = 0;
		G_warning (_("Error in color definition column GRASSRGB, element %d with cat %d"), line, cat);
	      }
	    }
	  } /* end if cat */
	  else {
	    rgb = 0;
	  } 
	} /* end if table_colors_flag */

	/* random colors */
	if( cats_color_flag ) {
	  cat = Vect_get_line_cat ( Map, line, Clist->field );
	  if( cat >= 0 ) {
	    G_debug (3, "display element %d, cat %d", line, cat);
	    /* fetch color number from category */
	    which = (cat % palette_ncolors);
	    G_debug (3,"cat:%d which color:%d r:%d g:%d b:%d", cat, which, 
		    palette[which].R, palette[which].G, palette[which].B);

	    rgb = 1;
	    red = palette[which].R;
	    grn = palette[which].G;
	    blu = palette[which].B;
	    
	  }
	  else {
	    rgb = 0;
	  }
	}
	
	x = Points->x;
	y = Points->y;

        if ( (ltype & GV_POINTS) && Symb != NULL ) {
	    /* Note: this should go to some library function */
	  if ((color != -1 || fcolor != -1) || rgb) {
	    G_plot_where_xy(x[0], y[0], &x0, &y0);
	  }  
 
            for ( i = 0; i < Symb->count; i++ ) {
                part = Symb->part[i];

	        switch ( part->type ) {
		    case S_POLYGON:
			/* Note: it may seem to be strange to calculate coor in pixels, then convert
			 *       to E-N and plot. I hope that we get some D_polygon later. */
			if ( (part->fcolor.color == S_COL_DEFAULT && fcolor > -1) ||
			      part->fcolor.color == S_COL_DEFINED || rgb) 
			{
			    if (!table_colors_flag && !cats_color_flag) {
			      if ( part->fcolor.color == S_COL_DEFAULT )
				R_color(fcolor);
			      else
				R_RGB_color ( part->fcolor.r, part->fcolor.g, part->fcolor.b );
			  }
			  else {
			    if (rgb) {
			      R_RGB_color ((unsigned char) red, (unsigned char) grn, (unsigned char) blu);
			    }
			    else {
			      R_color (fcolor);
			    }
			  }

			    Vect_reset_line ( PPoints );

			    for ( j = 0; j < part->count; j++ ) { /* Construct polygon */
				chain = part->chain[j];
				for ( k = 0; k < chain->scount; k++ ) { 
				    xp  = x0 + chain->sx[k];
				    yp  = y0 - chain->sy[k];
				    G_plot_where_en ( xp, yp, &xd, &yd );
				    Vect_append_point ( PPoints, xd, yd, 0.0);
				}
				if ( j == 0 ) {
				    xd0 = PPoints->x[0];
				    yd0 = PPoints->y[0];
				} else {
				    Vect_append_point ( PPoints, xd0, yd0, 0.0);
				}
			    }
			    
			    G_plot_polygon ( PPoints->x, PPoints->y, PPoints->n_points);

			}
			if ( (part->color.color == S_COL_DEFAULT && color > -1 ) ||
			      part->color.color == S_COL_DEFINED  ) 
			{
			    if ( part->color.color == S_COL_DEFAULT ) {
				R_color(color);
			    } else {
			        R_RGB_color ( part->color.r, part->color.g, part->color.b );
			    }

			    for ( j = 0; j < part->count; j++ ) { 
				chain = part->chain[j];
				for ( k = 0; k < chain->scount; k++ ) { 
				    xp  = x0 + chain->sx[k];
				    yp  = y0 - chain->sy[k];
				    if ( k == 0 ) D_move_abs ( xp, yp );
				    else D_cont_abs ( xp, yp );

				}
			    }
			    
			}
			
                        break;
                    case S_STRING: 
			if ( part->color.color == S_COL_NONE ) break;
			else {
			  if (!table_colors_flag && !cats_color_flag) {
			    if ( part->color.color == S_COL_DEFAULT ) R_color(color) ;
			    else R_RGB_color ( part->color.r, part->color.g, part->color.b );
			  }
			  else {
			    if (ltype == GV_CENTROID) {
			      R_color (color);
			    }
			    else {
			      if (rgb) {
				R_RGB_color ((unsigned char) red, (unsigned char) grn, (unsigned char) blu);
			      }
			      else {
				R_color (color);
			      }
			    }
			  }
			}
			    
			chain = part->chain[0];

                        for ( j = 0; j < chain->scount; j++ ) { 
			    xp  = x0 + chain->sx[j];
			    yp  = y0 - chain->sy[j];
			    if ( j == 0 ) D_move_abs ( xp, yp );
			    else D_cont_abs ( xp, yp );

                        }
                        break;
                }
            }
	} else if (color > -1 || rgb) {
	  if (!table_colors_flag && !cats_color_flag) {
	    R_color (color);
	  }
	  else {
	    if (rgb) {
	      R_RGB_color ((unsigned char) red, (unsigned char) grn, (unsigned char) blu);
	    }
	    else {
	      R_color (color);
	    }
	  }
	    if ( Points->n_points == 1 ) { /* line with one coor */
	        G_plot_line(x[0], y[0], x[0], y[0]);
	    } else {
		for(i=1; i < Points->n_points; i++) {
		    G_plot_line(x[0], y[0], x[1], y[1]);
		    x++;
		    y++;
		  }
	    }
	}
    }

    Vect_destroy_line_struct (Points);
    Vect_destroy_cats_struct (Cats);
    
    return 0; /* not reached */
}


