#include "distance.h"
int
draw_seg(map,p,n1,e1,n2,e2,color)
  struct Map_info *map;
  struct line_pnts *p;
  double n1,e1,n2,e2;
  char *color;
 {
  register int i;
  int color_num;
  double D_u_to_d_col();
  double D_u_to_d_row();
  long int screen_x1,screen_y1,screen_x2,screen_y2;
  int seg_drawn;
  short int accept;
  extern short int clip();

  seg_drawn = 0;
  if (n1==n2&&e1==e2)
    return(seg_drawn);
  color_num = D_translate_color(color);
  R_standard_color(color_num);
  clip(&accept,&e1,&n1,&e2,&n2,window.west,window.east,
         window.south,window.north);
  if (accept)
   {
    screen_x1 = (int)((D_u_to_d_col( e1 ))  );
    screen_y1 = (int) (D_u_to_d_row( n1 )   );
    screen_x2 = (int)((D_u_to_d_col( e2 ))  );
    screen_y2 = (int) (D_u_to_d_row( n2 )   );
    R_move_abs(screen_x1,screen_y1);
    R_cont_abs(screen_x2,screen_y2);
    seg_drawn = 1;
   }
  R_flush();
  return(seg_drawn);
 }
