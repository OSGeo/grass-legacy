#include <string.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "what.h"

static int nlines = 50;

#define WDTH 5

int what(int once, int terse, int width, int mwidth,
	 int dodbmi)
{
  int row, col;
  int nrows, ncols;
  struct Cell_head window;
  int screen_x, screen_y ;
  double east, north ;
  int button ;
  char east_buf[40], north_buf[40];
  double sq_meters;
  double x1, y1, x2, y2, z;
  int notty;
  double maxdist;
  int getz;
  struct Plus_head *Plus ;
  struct field_info *Fi;
  
  plus_t line, area, centroid ;
  int i, j;
  struct line_pnts * Points;
  struct line_cats *Cats;
  char temp[512], *str;
  
  
  G_get_set_window (&window);
  G_begin_polygon_area_calculations();
  nrows = window.rows;
  ncols = window.cols;
  
  screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
  screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;
  
  Points = Vect_new_line_struct();
  Cats = Vect_new_cats_struct ();

  if (!isatty(fileno(stdout)))
    notty = 1;
  else 
    notty = 0;
  
  do
    {
      if (!terse)
	show_buttons (once);
      R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
      if (!once)
	{
	  if (button == 3) break;
	  if (button == 2) continue;
	}
      
      east  = D_d_to_u_col((double)screen_x) ;
      north = D_d_to_u_row((double)screen_y) ;

      row = (window.north - north) / window.ns_res ;
      col = (east - window.west) / window.ew_res ;
      if (row < 0 || row >= nrows) continue;
      if (col < 0 || col >= ncols) continue;
      
      x1 = D_d_to_u_col ((double)(screen_x-WDTH));
      y1 = D_d_to_u_row ((double)(screen_y-WDTH));
      x2 = D_d_to_u_col ((double)(screen_x+WDTH));
      y2 = D_d_to_u_row ((double)(screen_y+WDTH));

      x1 = fabs ( x2 - x1 );
      y1 = fabs ( y2 - y1 );
      
      if ( x1 > y1 ) maxdist = x1;
      else maxdist = y1;
      G_debug (1, "Maximum distance in map units = %f\n", maxdist);
      
      for(i=0; i<nvects; i++)
        {
          Plus = &(Map[i].plus);
          line = Vect_find_line (&Map[i], east, north, 0, 
		                    GV_POINT|GV_LINE|GV_BOUNDARY|GV_CENTROID,
				    maxdist);
          area = Vect_find_area (&Map[i], east, north) ;
	  getz = Vect_tin_get_z (&Map[i], east, north, &z, NULL, NULL);
          
	  if(!i)
	    {
              G_format_easting(east, east_buf, G_projection());
              G_format_northing(north, north_buf, G_projection());
              fprintf(stdout,"\n%s(E) %s(N)\n", east_buf, north_buf);
              if (notty)
                fprintf(stderr,"\n%s(E) %s(N)\n", east_buf, north_buf);
              nlines++ ;
	    }
    
	  strcpy(temp, vect[i]);
	  if((str = strchr(temp, '@'))) *str = 0;

	  fprintf(stdout, "%*s in %-*s  ", width, temp, mwidth,
			  G_find_vector2(vect[i], ""));
	  if (notty)
	    fprintf(stderr, "%*s in %-*s  ", width, temp, mwidth,
			  G_find_vector2(vect[i], ""));
	  nlines++ ;

          if (line + area == 0)
            {
              fprintf (stdout,"Nothing Found.\n") ;
              if (notty)
                fprintf (stderr,"Nothing Found.\n") ;
              nlines++ ;
            }
           
          if (line == 0)
            // fprintf (stdout,"Line not found.\n") 
	    ;
          else 
            {
              fprintf (stdout,"Line %d\n", line);
	      
	      V2_read_line (Map, Points, Cats, line );
              for(j = 0; j < Cats->n_cats; j++ ) {
                  fprintf (stdout,"field = %d category = %d\n", Cats->field[j],
			             Cats->cat[j]);
	          if(dodbmi) {
		      Fi = Vect_get_field_info(Map[i].name, Map[i].mapset, Cats->field[j]);    
		      if (Fi == NULL) {
			  G_warning ("Cannot read field info");
		      } else {
                          fprintf (stdout,"driver=%s, database=%s, table=%s, key=%s\n", 
    			           Fi->driver, Fi->database, Fi->table, Fi->key);
	                  disp_attr( Fi->driver, Fi->database, Fi->table, 
				       Fi->key, Cats->cat[j] );
			  G_free ( Fi );
		      }
		  }
	      }
              nlines++ ;
            }
          
          if (area == 0)
            {
              //fprintf (stdout,"Area not found.\n") 
	      ;
            }
          else 
            {
	      if (Map->head.with_z && getz )	
                  fprintf (stdout,"Area %d z = %f\n", area, z);
	      else
                  fprintf (stdout,"Area %d\n", area);

	      centroid = Vect_get_area_centroid ( Map, area );
	      if ( centroid > 0 ) {
		  V2_read_line (Map, Points, Cats, centroid );
		  for(j = 0; j < Cats->n_cats; j++ ) {
		      fprintf (stdout,"field = %d category = %d\n", Cats->field[j],
					 Cats->cat[j]);
		      if(dodbmi) {
			  Fi = Vect_get_field_info(Map[i].name, Map[i].mapset, Cats->field[j]);    
			  if (Fi == NULL) {
			      G_warning ("Cannot read field info");
			  } else {
			      fprintf (stdout,"driver=%s, database=%s, table=%s, key=%s\n", 
				       Fi->driver, Fi->database, Fi->table, Fi->key);
			      disp_attr( Fi->driver, Fi->database, Fi->table, 
					   Fi->key, Cats->cat[j] );
			      G_free ( Fi );
			  }
		      }
		  }
	      }
	      
              sq_meters = Vect_get_area_area (&Map[i], area);
              
              fprintf (stdout,"Size - Sq Meters: %.3f\t\tHectares: %.3f\n",
            	   sq_meters, (sq_meters/10000.) );
              
              fprintf (stdout,"           Acres: %.3f\t\tSq Miles: %.4f\n",
            	   ((sq_meters*10.763649)/43560.),((sq_meters*10.763649)/43560.)/640. );
              if (notty) {
                fprintf (stderr,"Size - Sq Meters: %.3f\t\tHectares: %.3f\n",
            	     sq_meters, (sq_meters/10000.) );
                
                fprintf (stderr,"           Acres: %.3f\t\tSq Miles: %.4f\n",
            	     ((sq_meters*10.763649)/43560.),((sq_meters*10.763649)/43560.)/640. );
              }
              nlines += 3 ;
            }
          fflush(stdout);
        }
    }while (!once);
  Vect_destroy_line_struct(Points);
  
  return 0;
}

/* TODO */
int 
show_buttons (int once)
{
  if (once)
    {
      fprintf (stderr, "\nClick mouse button on desired location\n\n");
      nlines = 3;
    }
  else if (nlines >= 18)      /* display prompt every screen full */
    {
      fprintf (stderr, "\n");
      fprintf (stderr, "Buttons\n");
      fprintf (stderr, " Left:  what's here\n");
      fprintf (stderr, " Right: quit\n");
      nlines = 4;
    }
  
  return 0;
}

