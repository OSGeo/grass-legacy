/*=======================================================================
				i.points
  vect.c --
    int plotvect (void)
       Ask for vect file to plot in target location window.
       Updates the display_list, which contains names and mapsets
       of all currently display data.

       Calls _plotvect to do actual configuation, which calls 
       plot1() to to the actual drawing.       

       RETURNS:
          0     - always so Input_pointer in plot() does NOT quit

    int _plotvect(name, mapset, color_name, which)
       Handles view configuration for all vector drawing in the TARGET
       location.  "which" identifies if the target VIEW, target ZOOM_VIEW,
       or both VIEW and ZOOM_VIEW should be ploted.
       Call plot1() for the actual drawing.


    int _plotvect_warp (name, mapset, color_name, which)
       char *name, *mapset, *color_name;
       int  which;  == 0 - both target&zoom,
                       1 - zoom only 

       _plotvect_warp handles the polynomial transformation for 
       ploting an vector file on top of the source imagery file.

=======================================================================*/

#include <unistd.h>
#include <string.h>
#include "globals.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"

         /* internal function prototypes */
#ifdef _NO_PROTO
   static int choose_vectfile  ( );
   static int choose_vectcolor ( );
#else
   static int choose_vectfile  (char *name, char *mapset);
   static int choose_vectcolor (char *color_name, char *not_used);
#endif

static int use = 1;

/*---------------------------------------------------------------------*/
int plotvect(void)
{
  char name[40];
  char mapset[40];
  char color_name[40];
  char not_used[40];
  int  n;

  /*
   * Double check to make sure the vect list is available.
   * If so then call the plot routine.
   */

    if (access (vect_list,0) == 0)  {

      /* get the vector name and mapset in target location */
      if (!choose_vectfile (name, mapset))
	return 0;

      /* ask for color to draw vectors */
      if (!choose_vectcolor (color_name, not_used))
	return 0;

      /* add to display list */
      n = ++display_list.num_vectors;
      strcpy (display_list.vects[n].vect_name, name);
      strcpy (display_list.vects[n].vect_mapset, mapset);
      strcpy (display_list.vects[n].vect_color, color_name); 

      /* call the plot routine */
      _plotvect  (name, mapset, color_name, 0);
    }

    /* RETURN 0 - always so Input_pointer in plot() does NOT quit */
    return 0; 
}


/*---------------------------------------------------------------------*/
/* Plot the vector file "name"@"mapset" in the target location VIEWS,  */
/* using "colorname".                                                  */
/* If "which" == 0   - plot both VIEW_MAP2 and VIEW_MAP2_ZOOM          */
/*            == 1   - plot only VIEW_MAP2_ZOOM (i.e. someone zoomed)  */
/* RETURN 1  unless fatal error                                        */
/*---------------------------------------------------------------------*/
int _plotvect(char *name,char *mapset,char *color_name,int which)
{
    struct Cell_head window, tmp_window; 
    struct Map_info Map;
    char msg[80],*tmp_mapset;
    int color; 

    /* working with TARGET LOCATION enviroment */
    select_target_env();

    if (which != 1)   /* if which == 1 don't plot in VIEW_MAP2 */
    {
      /* plot in main window (VIEW_MAP2) */

      if (VIEW_MAP2->vect.configured == 0) {
	if (VIEW_MAP2->cell.configured == 1) { /* there a cell in there */
	  G_copy (&VIEW_MAP2->vect.head, 
		  &VIEW_MAP2->cell.head, sizeof (window));
	  Configure_view_vect (VIEW_MAP2, name, mapset, 
			       VIEW_MAP2->vect.head.ns_res, 
			       VIEW_MAP2->vect.head.ew_res);
	}
	else {	/* set window to vector window */
	  G_get_window (&tmp_window);                
	  
	  tmp_mapset = G_find_vector2 (name, "");
	  if (!tmp_mapset) {
	    sprintf (msg, "vector map <%s> not found", name);
	    G_fatal_error (msg);
	  }
	  
	  Vect_set_open_level (1);
	  if (1 != Vect_open_old (&Map, name, tmp_mapset)) {
	    sprintf (msg, "can't open vector file <%s> in <%s>", 				         name, mapset);
	    G_fatal_error (msg);
	  }
	  
	  window.north = Map.head.N;
	  window.south = Map.head.S;
	  window.west  = Map.head.W;
	  window.east  = Map.head.E;
	  G_align_window (&window, &tmp_window);
	  
	  Vect_close (&Map);
	  
	  G_adjust_window_to_box (&window, &VIEW_MAP2->vect.head, 
				  VIEW_MAP2->nrows, VIEW_MAP2->ncols);
	  Configure_view_vect (VIEW_MAP2, name, mapset, 
			       window.ns_res, 
			       window.ew_res);
	}
      }
      color = D_translate_color(color_name);
      R_standard_color(color);
      plot1(name, mapset, 0);
    } /* the end of VIEW_MAP2 */



    /* now try to plot in VIEW_MAP2_ZOOM */
    /* NOTE: nothing get drawn in VIEW_MAP2_ZOOM  */      
    /* the user had not previouse done a zoom     */

       /*   if MAP2_ZOOM is congigured, then plot it */
    if (VIEW_MAP2_ZOOM->vect.configured == 1) {
      color = D_translate_color(color_name); 
      R_standard_color(color) ;
      plot1(name, mapset, 1);
    }
      
      /* otherwise if there is a cell in VIEW_MAP2_ZOOM  */      
      /* then configure the vect to the same region as the cell */
    else if (VIEW_MAP2_ZOOM->cell.configured == 1) {
      G_copy (&VIEW_MAP2_ZOOM->vect.head, 
	      &VIEW_MAP2_ZOOM->cell.head, sizeof (window));
      Configure_view_vect (VIEW_MAP2_ZOOM, name, mapset, 
			   VIEW_MAP2_ZOOM->vect.head.ns_res,         
			   VIEW_MAP2_ZOOM->vect.head.ew_res);
      /* G_set_window (&VIEW_MAP2_ZOOM->vect.head); */
      
      color = D_translate_color(color_name); 
      R_standard_color(color) ;
      plot1(name, mapset, 1);
    }


    /* All done with plotting */

    /* return to current (imagery) location */
    select_current_env();

    /* update input method flages */
    if (from_screen < 0)
      {
	from_flag = 1;
	from_screen = 0;
	if (from_keyboard < 0)
	  {
	    from_keyboard = 0;
	    from_screen = 1;
	  }
      }

    /* diplay control points */
    display_points(1); 
    return 1;
}


/*---------------------------------------------------------------------*/
/* Plot the vector file "name"@"mapset" from the target location       */
/* ontop of the source imagery, by using the polynomial transformation */
/* coeefients  (E[], N[], order)                                       */
/*                                                                     */
/* If "which" == 0   - plot both VIEW_MAP1 and VIEW_MAP1_ZOOM          */
/*            == 1   - plot only VIEW_MAP1_ZOOM (i.e. someone zoomed)  */
/* RETURN 1  unless fatal error                                        */
/*---------------------------------------------------------------------*/

/***********************************************************************
** _plotvect_warp (name, mapset, color_name, which, E, N, order)
** char *name, *mapset, *color_name;
** int  which;           0 - both target&zoom, 1 - zoom only 
** double E[];
** double N[];
** int  order;
***********************************************************************/

int _plotvect_warp (char *name,char *mapset,char *color_name,
 int  which)          /* 0 - both target&zoom, 1 - zoom only */
{
    struct Cell_head window; 
    int color; 


    select_target_env();
    if (which != 1) {
      /* plot in main window */
      if (VIEW_MAP1->vect.configured == 0) {
	if (VIEW_MAP1->cell.configured == 1) { /* there a cell in there */
	  G_copy (&VIEW_MAP1->vect.head, 
		  &VIEW_MAP1->cell.head, sizeof (window));
	  Configure_view_vect (VIEW_MAP1, name, mapset, 
			       VIEW_MAP1->vect.head.ns_res, 
			       VIEW_MAP1->vect.head.ew_res);
	}
      }

      color = D_translate_color(color_name);
      R_standard_color(color);
      plot1_warp(name, mapset, 0);
    }

    /* plot the vector in the zoom window */
    /* there a cell in there */
    if (VIEW_MAP1_ZOOM->cell.configured == 1) {
      G_copy (&VIEW_MAP1_ZOOM->vect.head, 
	      &VIEW_MAP1_ZOOM->cell.head, sizeof (window));
      Configure_view_vect (VIEW_MAP1_ZOOM, name, mapset, 
			   VIEW_MAP1_ZOOM->vect.head.ns_res,         
			   VIEW_MAP1_ZOOM->vect.head.ew_res);
      /* G_set_window (&VIEW_MAP1_ZOOM->vect.head); */
      
      color = D_translate_color(color_name); 
      R_standard_color(color) ;
      plot1_warp (name, mapset, 1);
    }
      
    select_current_env();
    display_points(1); 
    return 1;
}


/*------------------------------------------------------------------------*/
/* Ask the user for an imagery file to display.  Uses the vect_list built */
/* in main.c of all vector files in target location search path.          */
/* "name" and "mapset" will contain the users response.                   */
/* RETURNS 1 if a vecto file was selected or                              */
/*         0 if nothing selected                                          */
/*------------------------------------------------------------------------*/
static int choose_vectfile (char *name,char *mapset)
{
    return ask_gis_files ("vector", vect_list, name, mapset, 1);
}

/*------------------------------------------------------------------------*/
/* Ask the user for a color to display the vector data                    */
/* Uses the color_list file built in main.c (a simple color list)         */
/* "color_name" will contain the users response.                          */
/* RETURNS 1 if a color was selected or                                   */
/*         0 if nothing selected                                          */
/*------------------------------------------------------------------------*/
static int choose_vectcolor (char *color_name,char *not_used)
{
    return ask_gis_files ("", color_list, color_name, not_used, 1);
}
