#include <stdlib.h>
#include <gis.h>

int getcoord(double *return_east, double *return_north)
{
  
  char buffer[200] ;
  char buf1[50], buf2[50];
  char temp[100];
  double lat, lon ;
  int screen_x, screen_y ;
  int cur_screen_x, cur_screen_y ;
  double east, north ;
  int button ;
  double D_get_d_north(), D_get_d_south() ;
  double D_get_d_east(), D_get_d_west() ;
  double D_d_to_u_row(), D_d_to_u_col() ;
  int white, black ;
  int projection;
  int draw_on ;
  double re, rn ;

  projection = G_projection();
  screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
  screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ; 

  R_get_location_with_pointer(&screen_x, &screen_y, &button) ;

  east = D_d_to_u_col((double)screen_x) ;
  north = D_d_to_u_row((double)screen_y) ;
  
  G_format_easting  (east,  buf1, projection);
  G_format_northing (north, buf2, projection);

  re=atof(buf1); rn=atof(buf2);

  
  *return_east=atof(buf1);
  *return_north=atof(buf2);
  
  return(button);
  }












































































