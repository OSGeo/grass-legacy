#include <unistd.h>
#include <string.h>
#include "local_proto.h"

int what (int once, int terse, int width, int mwidth)
{
  int row, col;
  int nrows, ncols;
  struct Cell_head window;
  int screen_x, screen_y ;
  double east, north;
  int button ;
  Site *close;
  char *desc;
  char east_buf[40], north_buf[40];
  char temp[512], *str, *mapset, *Gmapset;
  int i;
  
  G_get_set_window (&window);
  nrows = window.rows;
  ncols = window.cols;
  
  screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
  screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;
  
  do
    {
      if (!terse)
	show_buttons(once);
      R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
      if (!once)
	{
	  if (button == 2) continue;
	  if (button == 3) break;
	}
      east  = D_d_to_u_col((double)screen_x) ;
      north = D_d_to_u_row((double)screen_y) ;
      row = (window.north - north) / window.ns_res ;
      col = (east - window.west) / window.ew_res ;
      if (row < 0 || row >= nrows) continue;
      if (col < 0 || col >= ncols) continue;
      
      for(i=0; i<nsites; i++){
	if(!i)
	{
	  G_format_easting(east, east_buf, G_projection());
	  G_format_northing(north, north_buf, G_projection());
	  if (!isatty(fileno(stdout)))
	    fprintf(stdout, "\n%s(E) %s(N)\n", east_buf, north_buf);
	  fprintf(stderr, "\n%s(E) %s(N)\n", east_buf, north_buf);
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

        if(NULL != (close = closest_site(i, east, north))){
	  desc =  G_site_format (close, NULL, 0);
	  if (!isatty(fileno(stdout)))
	    fprintf(stdout, "%s\n", desc);
	  fprintf(stderr, "%s\n", desc);
        }else{
	  if (!isatty(fileno(stdout)))
	    fprintf(stdout, "Nothing Found.\n");
	  fprintf(stderr, "Nothing Found.\n");
	}
      }
    } while (! once) ;
  
  return 0;
}
