/*
	added flashing selected vectors --alex, nov/02
*/
#include <string.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "what.h"
#include "colors.h"

static int nlines = 50;

#define WDTH 2

int what(int once, int terse, int width, int mwidth,
	 int dodbmi, char *table, char *key)
{
  int lcat, acat ;
  int row, col;
  int nrows, ncols;
  struct Cell_head window;
  int screen_x, screen_y ;
  double east, north ;
  int button ;
  char east_buf[40], north_buf[40];
  double sq_meters;
  double x1, y1, x2, y2;
  int notty;
  
  P_LINE *Line ;
  P_AREA *Area ;
  plus_t line, area ;
  int i, j;
  struct line_pnts * Points;
  char temp[512], *str;
  
  char *panell;
  int flash_basecolr, flash_colr;
  
  G_get_set_window (&window);

  G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);

  G_begin_polygon_area_calculations();
  nrows = window.rows;
  ncols = window.cols;
  
  screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
  screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;
  
  Points = Vect_new_line_struct();
  if (!isatty(fileno(stdout)))
    notty = 1;
  else 
    notty = 0;
  
  panell = G_tempfile() ;
  flash_basecolr=D_translate_color("gray");
  
  do
    {
    
      R_panel_save(panell,R_screen_top(),R_screen_bot(),
		R_screen_left(), R_screen_rite());

      if (!terse)
	show_buttons (once);
      R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
      if (!once)
	{
	  if (button == 3) {R_panel_delete(panell);break;}
	  if (button == 2) {R_panel_delete(panell); flash_basecolr++; 
	  	if (flash_basecolr == 14) flash_basecolr = 1;continue;}
	}
      
      east  = D_d_to_u_col((double)screen_x) ;
      north = D_d_to_u_row((double)screen_y) ;

      row = (window.north - north) / window.ns_res ;
      col = (east - window.west) / window.ew_res ;
      if (row < 0 || row >= nrows) continue;
      if (col < 0 || col >= ncols) continue;
      
      /*  Nov 20, 1992   -dpg   the -1 allows ALL lines 
       *     including deleted lines  Also switch to point_by_line
       * line = dig_point_to_line (&Map[i], east, north, -1);
       */
      
      x1 = D_d_to_u_col ((double)(screen_x-WDTH));
      y1 = D_d_to_u_row ((double)(screen_y-WDTH));
      x2 = D_d_to_u_col ((double)(screen_x+WDTH));
      y2 = D_d_to_u_row ((double)(screen_y+WDTH));
	
      flash_colr = flash_basecolr;
      
      for(i=0; i<nvects; i++)
        {
          line = dig_point_by_line (&Map[i], x1, y1, x2, y2, LINE|AREA|DOT);
          
          area = dig_point_to_area (&Map[i], east, north) ;
          
          /*        	fprintf (stdout,"\nUTM  - %9.2f %10.2f\n", east, north);*/
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
            /* fprintf (stdout,"Line not found.\n") */
            ;
          else 
            { 
		
              Line = &(Map[i].Line[line]);
              if (Line->att)
                {
                  lcat = Map[i].Att[Line->att].cat ;
                  if (Cats[i].num > 0) 
            	{
		  if(dodbmi){
		    disp_attr( table, key, lcat );
		  } else {
            	    fprintf (stdout,"Line - Category %d %s\n", lcat,  
		    	G_get_cat(lcat, &Cats[i]));
            	    if (notty)
            	      fprintf (stderr,"Line - Category %d %s\n", lcat,
            		     G_get_cat(lcat, &Cats[i]));
		  }
            	}
                  else 
            	{
		  if(dodbmi){
		    disp_attr( table, key, lcat );
		  } else {
            	    fprintf (stdout,"Line - Category %d <not labeled>\n", lcat);
            	    if (notty)
            	      fprintf (stderr,"Line - Category %d <not labeled>\n", lcat);
		  }
            	}
                }
              else 
                {
                  fprintf (stdout,"Line - Category <not tagged>\n");
                  if (notty)
            	fprintf (stderr,"Line - Category <not tagged>\n");
                }
              nlines++ ;
	      	      
	      flash_line(&Map[i], line, Points, flash_colr);
	      Vect_destroy_line_struct(Points);
	      Points = Vect_new_line_struct();
		
            }
          
          if (area == 0)
            {
              /* fprintf (stdout,"Area not found.\n")  */
              ;
            }
          else 
            {
              Area = &(Map[i].Area[area]);
              if (Area->att)
                {
                  acat = Map[i].Att[Area->att].cat ;
                  if (Cats[i].num > 0) 
            	{
		  if(dodbmi){
		    disp_attr( table, key, acat );
		  } else {
            	    fprintf (stdout,"Area - Category %d %s	Flash: %s\n", acat,
            		   G_get_cat(acat, &Cats[i]), colr_str(flash_colr));
            	    if (notty)
            	      fprintf (stderr,"Area - Category %d %s	Flash: %s\n", acat,
            		     G_get_cat(acat, &Cats[i]), colr_str(flash_colr));
		  }
            	}
                  else 
            	{
		  if(dodbmi){
		    disp_attr( table, key, acat );
		  } else {
            	    fprintf (stdout,"Area - Category %d <not labeled>	Flash: %s\n", acat, colr_str(flash_colr));
            	    if (notty)
            	      fprintf (stderr,"Area - Category %d <not labeled>	Flash: %s\n", acat, colr_str(flash_colr));
		  }
            	}
                }
              else 
                {
                  fprintf (stdout,"Area - Category <not tagged>	Flash: %s\n", colr_str(flash_colr));
                  if(notty)
            	fprintf (stderr,"Area - Category <not tagged>	Flash: %s\n", colr_str(flash_colr));
                }
              /* Area stats - just for grins */
              /* dig_find_area2(&Map[i], Area, &sq_meters); */
		
		Vect_get_area_points(&Map[i], area, Points);
		
			flash_area(&Map[i], area, Points, flash_colr);

              sq_meters = 
                G_area_of_polygon(Points->x, Points->y, Points->n_points);
              /* substructing island areas */
              for(j = 0;j<Area->n_isles;j++)
                {
                  Vect_get_isle_points(&Map[i], Area->isles[j], Points);
                  sq_meters = sq_meters -
            	G_area_of_polygon(Points->x, Points->y, Points->n_points);
                }

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
	  flash_colr++; if (flash_colr==14) flash_colr=1;
        } /*end for*/

	R_panel_restore(panell);
	R_panel_delete(panell);		

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
      fprintf (stderr, " Middle: toggle flash color\n");
      nlines = 5;
    }
  
  return 0;
}

char * colr_str(int flash_colr)
{
static char str[10];
memset(str,'\0',sizeof(str));

switch(flash_colr)
{
    case RED: sprintf(str,"red");
    	break;
    case ORANGE: sprintf(str,"orange");
    	break;
    case YELLOW: sprintf(str,"yellow");
    	break;
    case GREEN: sprintf(str,"green");
    	break;
    case BLUE: sprintf(str,"blue");
    	break;
    case INDIGO: sprintf(str,"indigo");
    	break;
    case VIOLET: sprintf(str,"violet");
    	break;
    case WHITE: sprintf(str,"white");
    	break;
    case BLACK: sprintf(str,"black");
    	break;
    case GRAY: sprintf(str,"gray");
    	break;
    case BROWN: sprintf(str,"brown");
    	break;
    case MAGENTA: sprintf(str,"magenta");
    	break;
    case AQUA: sprintf(str,"aqua");
    	break;
    default:   sprintf(str,"unknown");
    	break;
}
return str;
}
