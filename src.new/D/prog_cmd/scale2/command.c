/*  %W%  %G%  */

#include "options.h"

set_default_options()
{
	color1 = D_translate_color("black") ;
	color2 = D_translate_color("white") ;
	easting = 0.0;
	northing = 0.0;
	coord_inp = 0;
	mouse = 0;
}

stash_away(pos, option)
	int pos ;
	char *option ;
{
  int new_colr, new_mouse ;
  double new_coord;

  switch(pos)
    {
    case COLOR1:
      new_colr = D_translate_color(option) ;
      if (new_colr == 0)
	goto color_error ;
      color1 = new_colr ;
      break ;
    case COLOR2:
      new_colr = D_translate_color(option) ;
      if (new_colr == 0)
	goto color_error ;
      color2 = new_colr ;
      break ;
    case EASTING:
      if (sscanf(option, " %lf ", &new_coord) == 1) {
	coord_inp++;
	easting = new_coord;
      }
      else
	easting = 0.0;
      break;
    case NORTHING:
      if (sscanf(option, " %lf ", &new_coord) == 1) {
	coord_inp++;
	northing = new_coord;
      }
      else
	northing = 0.0;
      break;
    case MOUSE:
      if (sscanf(option, " %d ", &new_mouse) == 1)
	mouse = new_mouse;
      else
	mouse = 0;
      break;
    default:
      printf("Unknown option\n") ;
      return(-1) ;
      break ;
    }
  return(0) ;

 color_error:
  printf("Don't know the color %s\n", option) ;
  printf("Available colors:\n") ;
  printf("  red      orange      yellow     green\n") ;
  printf("  blue     indigo      violet     gray\n") ;
  printf("  brown    magenta     white      black\n") ;
  return(-1) ;
}
