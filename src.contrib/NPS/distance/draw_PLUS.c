#include "distance.h"
int
draw_PLUS(map,p,n,e,color)
 struct Map_info *map;
 struct line_pnts *p;
 double n;
 double e;
 char *color;
 {
  double D_u_to_d_col();
  double D_u_to_d_row();
  int color_num;
  int screen_x, screen_y;
  int seg_x[2], seg_y[2];
  int plus_drawn;
  static int num = {2};
  extern pt_in_WIND();

  plus_drawn = 0;
  color_num = D_translate_color(color);
  R_standard_color(color_num);
  if ( pt_in_WIND(n,e) )
   {
    screen_x = (int) (D_u_to_d_col( e )  );
    screen_y = (int) (D_u_to_d_row( n )  );
/* draw horizonal line of "plus" */
    seg_x[0] = screen_x - 5;  
    seg_x[1] = screen_x + 5;  
    seg_y[0] = screen_y+1;
    seg_y[1] = screen_y+1;
    color_num = D_translate_color(color);
    R_standard_color(color_num);
/* draw horizonal line that is above middle line */
    R_polyline_abs(seg_x,seg_y,num);
    seg_y[0] = screen_y-1;
    seg_y[1] = screen_y-1;
/* draw horizontal line that is below middle line */
    R_polyline_abs(seg_x,seg_y,num);

/* draw vertical line of "plus" */
    seg_y[0] = screen_y + 5;
    seg_y[1] = screen_y - 5;
    seg_x[0] = screen_x+1;
    seg_x[1] = screen_x+1;
/* draw vertical line that is right of middle line */
    R_polyline_abs(seg_x,seg_y,num);
    seg_x[0] = screen_x-1;
    seg_x[1] = screen_x-1;
/* draw vertical line that is left of middle line */
    R_polyline_abs(seg_x,seg_y,num);

/* draw vertical "black" middle line of "plus" */
    seg_y[0] = screen_y + 5;
    seg_y[1] = screen_y - 5;
    seg_x[0] = screen_x;
    seg_x[1] = screen_x;
    color_num = D_translate_color(BG_COLOR);
    R_standard_color(color_num);
    R_polyline_abs(seg_x,seg_y,num);

/* draw horizonal "black" middle line of "plus" */
    seg_x[0] = screen_x - 5;  
    seg_x[1] = screen_x + 5;  
    seg_y[0] = screen_y;
    seg_y[1] = screen_y;
    R_polyline_abs(seg_x,seg_y,num);

    R_flush();
    plus_drawn = 1;
   }
  return(plus_drawn);
 }
