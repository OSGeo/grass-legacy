#include "distance.h"
int
draw_NODE(map,p,node,color)
 struct Map_info *map;
 struct line_pnts *p;
 int node;
 char *color;
 {
  double D_u_to_d_col();
  double D_u_to_d_row();
  register double x,y;
  int color_num;
  static int n = {2};
  int screen_x, screen_y;
  int seg_x[2], seg_y[2];
  int node_drawn;

  node_drawn = 0;
  if (map->Node[node].alive == 0)
    return(node_drawn);
  color_num = D_translate_color(color);
  R_standard_color(color_num);
  x = map->Node[node].x;
  y = map->Node[node].y;
  if ( (y >= window.south && y <= window.north) &&
       (x >= window.west  && x <= window.east)     )
   {
    screen_x = (int) (D_u_to_d_col( x )  );
    screen_y = (int) (D_u_to_d_row( y )  );
  /* draw horizonal line of "plus" */
    seg_x[0] = screen_x - 5;  
    seg_x[1] = screen_x + 5;  
    seg_y[0] = screen_y;
    seg_y[1] = screen_y;
    R_polyline_abs(seg_x,seg_y,n);
  /* draw vertical line of "plus" */
    seg_x[0] = screen_x;
    seg_x[1] = screen_x;
    seg_y[0] = screen_y + 5;
    seg_y[1] = screen_y - 5;
    R_polyline_abs(seg_x,seg_y,n);
    R_flush();
    node_drawn = 1;
   }
  return(node_drawn);
 }
