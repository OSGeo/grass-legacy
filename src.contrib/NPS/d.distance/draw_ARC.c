#include "distance.h"

int
draw_ARC(map,p,arc,stripe,first_color,second_color)
  struct Map_info *map;
  struct line_pnts *p;
  int arc;
  int stripe;
  char *first_color;
  char *second_color;
 {
  register int i;
  int color_num;
  double D_u_to_d_col();
  double D_u_to_d_row();
  double x1,y1,x2,y2;
  long int screen_x1,screen_y1,screen_x2,screen_y2;
  short int accept;
  extern short int clip();
  extern arc_is_dot();
  int arc_drawn;

  arc_drawn = 0;
  if (arc_is_dot(map,p,arc))
   {
    return(arc_drawn);
   }
  color_num = D_translate_color(first_color);
  R_standard_color(color_num);
  dig_P_read_line(map,abs(arc),&p);
  for (i=0; i < (p->n_points-1); i++)
   {
    x1 = p->x[i];
    y1 = p->y[i];
    x2 = p->x[i+1];
    y2 = p->y[i+1];
    clip(&accept,&x1,&y1,&x2,&y2,window.west,window.east,
         window.south,window.north);
    if (accept)
     {
/* "Stripe" the arc "color" and "green"  */
      if (stripe)
       {
        if ( (i % 2) == 0 )
          {
           color_num = D_translate_color(first_color);
           R_standard_color(color_num);
          }
        else
          {
           color_num = D_translate_color(second_color);
           R_standard_color(color_num);
          }
       }
      screen_x1 = (int)((D_u_to_d_col( x1 ))  );
      screen_y1 = (int) (D_u_to_d_row( y1 )   );
      screen_x2 = (int)((D_u_to_d_col( x2 ))  );
      screen_y2 = (int) (D_u_to_d_row( y2 )   );
      R_move_abs(screen_x1,screen_y1);
      R_cont_abs(screen_x2,screen_y2);
      arc_drawn = 1;
     }
   }
  R_flush();
  return(arc_drawn);
 }
