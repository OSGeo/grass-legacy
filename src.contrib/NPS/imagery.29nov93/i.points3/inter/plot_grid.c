
#include "globals.h"
/**
#include "dig_structs.h"
#include "dig_externs.h"
**/
extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_get_d_east();
extern int D_move_abs();
extern int D_cont_abs();


static int use = 1;

plotgrid()
{
  char color_name[40];
  char not_used[40];
  int  n;

/*
 * Double check to make sure the vect list is available.
 * If so then call the plot routine.
 */

    if (access (color_list,0) == 0)  {

      /* ask for color to draw vectors */
      if (!choose_gridcolor (color_name, not_used))
	return 0;

      _plotgrid  (color_name, 0);
    }
    else {
      return 0;
    }

    return 0; 
}


_plotgrid(color_name, which)
char *color_name;
int  which;     /* 0 - both target&zoom, 1 - zoom only */
{
  struct Cell_head cellhd, window, tmp_window; 
  char buf[80], msg[80], *tmp_mapset;
  int color; 
  
  /* plot grid (squares) on source imagery */
  select_current_env();

  /* plot the main window VIEW_MAP1 */
  if (which != 1) {
    
    /* vect or cell must be configured */
    if ((VIEW_MAP1->vect.configured == 0) &&  
	(VIEW_MAP1->cell.configured == 0))
      return 1;
    
    /* configure vect from cell */
    if (VIEW_MAP1->vect.configured == 0) {
      if (VIEW_MAP1->cell.configured == 1) { 
	G_copy (&VIEW_MAP1->vect.head, 
		&VIEW_MAP1->cell.head, sizeof (window));
	Configure_view_vect (VIEW_MAP1, "grid", G_mapset(), 
			     VIEW_MAP1->vect.head.ns_res, 
			     VIEW_MAP1->vect.head.ew_res);
      }
    }
    color = D_translate_color(color_name);
    R_standard_color(color);
    grid1(0);
  }

  /* plot the vector in the zoom window VIEW_MAP1_ZOOM */
  /* vect or cell must be configured */
  if ((VIEW_MAP1_ZOOM->vect.configured == 0) &&  
      (VIEW_MAP1_ZOOM->cell.configured == 0))
    return 1;

  if (VIEW_MAP1_ZOOM->vect.configured == 0) {
    if (VIEW_MAP1_ZOOM->cell.configured == 1) {
      G_copy (&VIEW_MAP1_ZOOM->vect.head, 
	      &VIEW_MAP1_ZOOM->cell.head, sizeof (window));
      Configure_view_vect (VIEW_MAP1_ZOOM, "grid", G_mapset(), 
			   VIEW_MAP1_ZOOM->vect.head.ns_res,         
			   VIEW_MAP1_ZOOM->vect.head.ew_res);
    }
  }
  color = D_translate_color(color_name); 
  R_standard_color(color) ;
  grid1(1);
           
  return 1;
}

/***********************************************************
/** _plotgrid_warp (color_name, which, E, N, order)
/** char *color_name;
/** int  which;        /* 0 - both target&zoom, 1 - zoom only */
/** double E[];
/** double N[];
/** int  order;
***********************************************************/

_plotgrid_warp (color_name, which, E, N, order)
 char *color_name;
 int  which;        /* 0 - both target&zoom, 1 - zoom only */
{
  struct Cell_head cellhd, window, tmp_window; 
  char buf[80], msg[80], *tmp_mapset;
  int color; 

  /* plot grid (warped) on target location */
  select_target_env();

  /* plot the main window VIEW_MAP2 */
  if (which != 1) {
    
    /* vect or cell must be configured */
    if ((VIEW_MAP2->vect.configured == 0) &&  
	(VIEW_MAP2->cell.configured == 0))
      return 1;
    
    /* configure vect from cell */
    if (VIEW_MAP2->vect.configured == 0) {
      if (VIEW_MAP2->cell.configured == 1) { 
	G_copy (&VIEW_MAP2->vect.head, 
		&VIEW_MAP2->cell.head, sizeof (window));
	Configure_view_vect (VIEW_MAP2, "grid", G_mapset(), 
			     VIEW_MAP2->vect.head.ns_res, 
			     VIEW_MAP2->vect.head.ew_res);
      }
    }
    color = D_translate_color(color_name);
    R_standard_color(color);
    grid1_warp (0);
  }

  /* plot the vector in the zoom window VIEW_MAP2_ZOOM */
  /* vect or cell must be configured */
  if ((VIEW_MAP2_ZOOM->vect.configured == 0) &&  
      (VIEW_MAP2_ZOOM->cell.configured == 0))
    return 1;


  if (VIEW_MAP2_ZOOM->vect.configured == 0) {
    if (VIEW_MAP2_ZOOM->cell.configured == 1) {
      G_copy (&VIEW_MAP2_ZOOM->vect.head, 
	      &VIEW_MAP2_ZOOM->cell.head, sizeof (window));
      Configure_view_vect (VIEW_MAP2_ZOOM, "grid", G_mapset(), 
			   VIEW_MAP2_ZOOM->vect.head.ns_res,         
			   VIEW_MAP2_ZOOM->vect.head.ew_res);
    }
  }
  color = D_translate_color(color_name); 
  R_standard_color(color) ;

  grid1_warp (1);
           
  return 1;
}


static
choose_gridcolor (color_name, not_used)
    char *color_name, *not_used;
{
    return ask_gis_files ("", color_list, color_name, not_used, 1);
}





