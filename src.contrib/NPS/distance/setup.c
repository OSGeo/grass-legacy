#include "distance.h"
int
setup(map,p,mapset,name,number_of_arcs,overlay_flag)
 struct Map_info *map;
 struct line_pnts *p;
 char *mapset;
 char *name;
 int *number_of_arcs;
 int overlay_flag;
 {
  char window_name[64] ;
  int t, b, l, r ;
  static char prog_name[15];
  static char usage[20] = {"  vector-file-name "};
  int length;
  char gisbase[256];
  char command[512];
  int arc;
  int exit_status;
  int stripe;

#ifdef DEBUG
fprintf(stderr,"setup\n");
#endif DEBUG
/* If "overlay_flag" is equal to 1 then overlay and do not                   */
/* clear the screen.                                                         */
  if (overlay_flag==0)
   {
/* Clear the screen as "BG_COLOR" (black).                                   */
    sprintf(command,"$GISBASE/bin/Derase %s",BG_COLOR);
    system (command);
   }
  R_open_driver();
  if (D_get_cur_wind(window_name))
   {
    G_fatal_error("No current graphics window") ;
    return(0);
   }
  if (D_set_cur_wind(window_name))
   {
    G_fatal_error("Current graphics window not available") ;
    return(0);
   }
/* Read in the map window associated with window.                            */
  G_get_window(&window) ;
  if (D_check_map_window(&window))
   {
    G_fatal_error("Setting map window") ;
    return(0);
   }
  if (G_set_window(&window) == -1) 
   {
    G_fatal_error("Current graphics window not settable") ;
    return(0);
   }
/* Determine conversion factors.                                             */
  if (D_get_screen_window(&t, &b, &l, &r))
   {
    G_fatal_error("Getting screen window") ;
    return(0);
   }
  if (D_do_conversions(&window, t, b, l, r))
   {
    G_fatal_error("Error in calculating conversions") ;
    return(0);
   }
  dig_P_init (name,mapset,map); 
/* Retrieve total number of arcs for "map".                                  */
  *number_of_arcs = dig_P_num_lines(map);
  if (map->n_nodes <= 0) 
    return(0);
  return(1);
 }
