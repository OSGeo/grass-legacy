#include "distance.h"
int
draw_SEGMENT(map,p,arc,segment,color)
  struct Map_info *map;
  struct line_pnts *p;
  int arc;
  int segment;
  char *color;
 {
  register int i;
  int color_num;
  double D_u_to_d_col();
  double D_u_to_d_row();
  double x1,y1,x2,y2;
  long int screen_x1,screen_y1,screen_x2,screen_y2;
  short int accept;
  extern short int clip();
  int segment_drawn;

  segment_drawn = 0;
  if (arc_is_dot(map,p,abs(arc)))
   {
    return(segment_drawn);
   }
  dig_P_read_line(map,abs(arc),&p);
  if (segment>(p->n_points-1))
    return(segment_drawn);
  if (segment<1)
    return(segment_drawn);
  color_num = D_translate_color(color);
  R_standard_color(color_num);
  x1 = p->x[segment-1];
  y1 = p->y[segment-1];
  x2 = p->x[segment];
  y2 = p->y[segment];
  clip(&accept,&x1,&y1,&x2,&y2,window.west,window.east,
         window.south,window.north);
  if (accept)
   {
    screen_x1 = (int)((D_u_to_d_col( x1 ))  );
    screen_y1 = (int) (D_u_to_d_row( y1 )   );
    screen_x2 = (int)((D_u_to_d_col( x2 ))  );
    screen_y2 = (int) (D_u_to_d_row( y2 )   );
    R_move_abs(screen_x1,screen_y1);
    R_cont_abs(screen_x2,screen_y2);
    segment_drawn = 1;
   }
  R_flush();
  return(segment_drawn);
 }
