#include "globals.h"
#include "crs.h"


_warp_vect()
{
  int i;
  char msg[80];


  /* any vectors to zoom */
  if (display_list.num_vectors >= 0) {
    
    for (i = 0; i <= display_list.num_vectors; i++)  {
        /* write a message */
  	sprintf (msg, "Plotting warped vector file <%s>",
	         display_list.vects[i].vect_name);
  	Menu_msg (msg);

        _plotvect_warp(display_list.vects[i].vect_name,
		     display_list.vects[i].vect_mapset,
		     display_list.vects[i].vect_color, 0);

    }


    /** TODO -- is this needed ? **/
    select_current_env();
  }
    
  return 0;
}



_warp_grid()
{
  char color_name[20], not_used[20];
  char msg[80];

  
  /* write a message */
  sprintf (msg, "Choose grid color");
  Menu_msg (msg);


  /* ask for color to draw vectors */
  if (!choose_gridcolor (color_name, not_used))
    return 0;  

  /* write a message */
  sprintf (msg, "Plotting warped grid");
  Menu_msg (msg);


  /* plot square grid over source image */
  select_current_env();
  _plotgrid (color_name, 0);

  /* plot warp grid over target image */
  select_target_env();

  _plotgrid_warp(color_name, 0);

  select_current_env();

  return 0;
}

static
choose_gridcolor (color_name, not_used)
    char *color_name, *not_used;
{
    return ask_gis_files ("", color_list, color_name, not_used, 1);
}





