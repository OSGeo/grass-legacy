/* Added visual marks allowing to see better
	what was selected. --alex, nov/02
*/
#include <unistd.h>
#include <string.h>
#include "local_proto.h"
#include "gis.h"
#include "raster.h"
#include "display.h"

int what (int once, int terse, int width, int mwidth)
{
  int row, col;
  int nrows, ncols;
  struct Cell_head window;
  int screen_x, screen_y ;
  double east, north, cs_distance, cs_rdist, cs_rad, sina, cosa;
  int button ;
  Site *close;
  char *desc;
  char east_buf[40], north_buf[40];
  char temp[512], *str, *mapset, *Gmapset;
  int i,j;
  char *paneli, *panelm;
  int flash_colr, flash_basecolr, D_X, D_Y;
  
  G_get_set_window (&window);
  nrows = window.rows;
  ncols = window.cols;
  
    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);

  paneli = G_tempfile() ;  
  screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
  screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;
  flash_basecolr=D_translate_color("yellow");
  
  do
    {	
      R_panel_save(paneli,R_screen_top(),R_screen_bot(),
		R_screen_left(), R_screen_rite());
      flash_colr = flash_basecolr;
      if (!terse)
	show_buttons(once);
      R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
      if (!once)
	{
	  if (button == 2) {R_panel_delete(paneli);continue;}
	  if (button == 3) {R_panel_delete(paneli);break;}
	}
      east  = D_d_to_u_col((double)screen_x) ;
      north = D_d_to_u_row((double)screen_y) ;
      row = (window.north - north) / window.ns_res ;
      col = (east - window.west) / window.ew_res ;
      if (row < 0 || row >= nrows) continue;
      if (col < 0 || col >= ncols) continue;
      
      for(i=0; i<nsites; i++){
      
        R_standard_color(flash_colr);
	if(!i)
	{
	  G_format_easting(east, east_buf, G_projection());
	  G_format_northing(north, north_buf, G_projection());
	  if (!isatty(fileno(stdout)))
	    fprintf(stdout, "\n\"+\" at %s(E) %s(N)\n", east_buf, north_buf);
	  fprintf(stderr, "\n\"+\" at %s(E) %s(N)\n", east_buf, north_buf);
	  Gmapset = G_mapset();
	}
	strcpy(temp, site[i]);
	if((str = strchr(temp, '@'))){
	  *str = 0;
	  mapset = str+1;
	}else{
	  mapset = Gmapset;
	}
	if (!isatty(fileno(stdout)))
	  fprintf(stdout, "%*s in %-*s  ", width, temp, mwidth, mapset);
	fprintf(stderr, "%*s in %-*s  ", width, temp, mwidth, mapset);

	  R_standard_color(D_translate_color("black"));
	  draw_point_plus(screen_x, screen_y, 5);
	  R_flush();
	  R_standard_color(D_translate_color("red"));
	  draw_point_plus(screen_x, screen_y, 5);
	  R_flush();

        if(NULL != (close = closest_site(i, east, north))){
	  
	  D_X = (int)D_u_to_d_col(close->east) ;
	  D_Y = (int)D_u_to_d_row(close->north) ;
	  cs_distance = sqrt((D_X - screen_x)*(D_X - screen_x) + (D_Y - screen_y)*(D_Y - screen_y));
	  cs_rdist = sqrt((close->east - east)*(close->east - east) + 
	  	(close->north - north)*(close->north - north));
	  cosa = (D_X - screen_x)/cs_distance;
	  sina = (D_Y - screen_y)/cs_distance;
	  cs_rad = (fabs(D_get_d_north() - D_get_d_south()))/20;
	  j = 1;
	  while ((cs_rad * j <= cs_distance) || (j == 1)) {
	  	R_standard_color(D_translate_color("black"));
	  	draw_sector(sina, cosa, screen_x-cs_rad*j,screen_y+cs_rad*j,screen_x+cs_rad*j,screen_y-cs_rad*j);
		R_flush();
		R_standard_color(D_translate_color("yellow"));
		draw_sector(sina, cosa, screen_x-cs_rad*j,screen_y+cs_rad*j,screen_x+cs_rad*j,screen_y-cs_rad*j);
		R_flush();
		j++;
	  }

	  R_standard_color(D_translate_color("black"));
	  draw_point_x(D_X, D_Y,10);
	  R_flush();
	  R_standard_color(D_translate_color("yellow"));
	  draw_point_x(D_X, D_Y,10);
	  R_flush();
	  
	  desc =  G_site_format (close, NULL, 0);
	  if (!isatty(fileno(stdout)))
	    fprintf(stdout, "%s\n  Distance from \"+\":%10.2f\n", desc, cs_rdist);
	  fprintf(stderr, "%s\n  Distance from \"+\":%10.2f\n", desc, cs_rdist);
        }else{
/*
	  R_standard_color(D_translate_color("black"));
	  draw_point_plus(screen_x, screen_y, 5);
	  R_flush();
	  R_standard_color(D_translate_color("red"));
	  draw_point_plus(screen_x, screen_y, 5);
	  R_flush();
	  sleep(1);*/

	  if (!isatty(fileno(stdout)))
	    fprintf(stdout, "Nothing Found.\n");
	  fprintf(stderr, "Nothing Found.\n");
	}
	flash_colr++; if (flash_colr == 14) flash_colr = 1;
      } /*end for*/
    
    R_panel_restore(paneli);
    R_panel_delete(paneli);		

    } while (! once) ;
  
  return 0;
}

/*code partly taken from r.le --alex*/

void draw_sector(double sina, double cosa, double x0, double y0, double xp, double yp)
{
  int i;
  double ang, xinc, yinc, xstart, ystart, radius;

/*PARAMETERS
  x0 = leftmost position of enclosing square
  y0 = topmost position of enclosing square
  xp = rightmost position of enclosing square
  yp = bottommost position of enclosing square
  i = index for incrementing process
  ang = angle in radians that is the
       angle to be moved in connecting
       a straight line segment to the
       previous location
*/
     radius = (xp - x0)/2;
     xstart = x0 +  radius * (1.0 + cosa);
     ystart = yp +  radius * (1.0 + sina);
     ang = - 0.01;
     xstart += cos(ang) * sin(ang) * (yp - y0);
     ystart += sin(ang) * sin(ang) * (yp - y0);
     R_move_abs(xstart, ystart);
     ang = 0.01;
     for (i = 1; i < 5; i++) {
	xinc = cos((double)i * ang/2.0) * sin((double)i * ang/2.0) * (yp - y0);
        yinc = sin((double)i * ang/2.0) * sin((double)i * ang/2.0) * (yp - y0);
	
        R_cont_abs(xstart + (int)xinc, ystart + (int)yinc);
     }
  return;
}

void draw_point_x(int D_X, int D_Y, int size)
{

		R_move_abs(D_X-size, D_Y-size) ;
		R_cont_abs(D_X+size, D_Y+size) ;
		R_move_abs(D_X+size, D_Y-size) ;
		R_cont_abs(D_X-size, D_Y+size) ;
	return;
}

void draw_point_plus(int D_X, int D_Y, int size)
{

		R_move_abs(D_X-size, D_Y) ;
		R_cont_abs(D_X+size, D_Y) ;
		R_move_abs(D_X, D_Y-size) ;
		R_cont_abs(D_X, D_Y+size) ;
	return;
}
