#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"
#include "glocale.h"

int quit (struct Cell_head *defwin, struct Cell_head *currwin) {
  
  int screen_x, screen_y, button;
  int hand = 0;
  double ux1, uy1, ux2, uy2, ew, ns;
  
  fprintf(stderr, "\n\nButtons:\n") ;
  fprintf(stderr, "%s reset to default region\n", lefts) ;
  fprintf(stderr, "%s reset to region before d.zoom started\n", middles) ;
  fprintf(stderr, "%s Quit\n", rights) ;	
  
  R_get_location_with_pointer(&screen_x, &screen_y, &button);		
  
  if (button == leftb) {
    ew = defwin->east - defwin->west;
    ns = defwin->north - defwin->south;

    if ( ns <= defwin->ns_res ) {
      ns = 2 * defwin->ns_res;
    }

    if ( ew <= defwin->ew_res ) {
      ew = 2 * defwin->ew_res;
    }

    ux1 = defwin->east;
    ux2 = defwin->west;
    uy1 = defwin->north;
    uy2 = defwin->south;

    set_win (defwin, ux1, uy1, ux2, uy2, hand);
    return 0;
  }
 
  if (button == middleb) {
    ew = currwin->east - currwin->west;
    ns = currwin->north - currwin->south;

    if ( ns <= currwin->ns_res ) {
      ns = 2 * currwin->ns_res;
    }

    if ( ew <= currwin->ew_res ) {
      ew = 2 * currwin->ew_res;
    }

    ux1 = currwin->east;
    ux2 = currwin->west;
    uy1 = currwin->north;
    uy2 = currwin->south;

    set_win (currwin, ux1, uy1, ux2, uy2, hand);
    return 0;
  }
  
  if (button == rightb) {
    return 0;
  }

 return 0;

}
