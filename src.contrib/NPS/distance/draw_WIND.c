#include "distance.h"
int
draw_WIND(map,p)
  struct Map_info *map;
  struct line_pnts *p;
 {
  double D_u_to_d_col();
  double D_u_to_d_row();
  long int screen_x1,screen_y1,screen_x2,screen_y2;
  int color_num;

  color_num = D_translate_color(WI_COLOR);
  R_standard_color(color_num);
/* Draw West boundary of WIND */
  screen_x1 = (int)((D_u_to_d_col( window.west ))  );
  screen_y1 = (int) (D_u_to_d_row( window.south)   );
  screen_x2 = (int)((D_u_to_d_col( window.west ))  );
  screen_y2 = (int) (D_u_to_d_row( window.north)   );
  R_move_abs(screen_x1,screen_y1);
  R_cont_abs(screen_x2,screen_y2);
/* Draw North boundary of WIND */
  screen_x1 = (int)((D_u_to_d_col( window.west ))  );
  screen_y1 = (int) (D_u_to_d_row( window.north)   );
  screen_x2 = (int)((D_u_to_d_col( window.east ))  );
  screen_y2 = (int) (D_u_to_d_row( window.north)   );
  R_cont_abs(screen_x2,screen_y2);
/* Draw East boundary of WIND */
  screen_x1 = (int)((D_u_to_d_col( window.east ))  );
  screen_y1 = (int) (D_u_to_d_row( window.north)   );
  screen_x2 = (int)((D_u_to_d_col( window.east ))  );
  screen_y2 = (int) (D_u_to_d_row( window.south)   );
  R_cont_abs(screen_x2,screen_y2);
/* Draw South boundary of WIND */
  screen_x1 = (int)((D_u_to_d_col( window.east ))  );
  screen_y1 = (int) (D_u_to_d_row( window.south)   );
  screen_x2 = (int)((D_u_to_d_col( window.west ))  );
  screen_y2 = (int) (D_u_to_d_row( window.south)   );
  R_cont_abs(screen_x2,screen_y2);
  R_flush();
  return(1);
 }
