/*=======================================================================
				i.points
  cell.c --
    int plotcell (void)
       Ask for cell file to plot in target location window.
       Updates the display_list, which contains names and mapsets
       of all currently display data.
       Calls _plotcell to do actual configuation, which calls 
       drawcell() to to the actual drawing.       

       RETURNS:
          0     - always so Input_pointer in plot() does NOT quit

=======================================================================*/

#include <string.h>
#include <unistd.h>
#include "globals.h"

         /* internal function prototypes */
#ifdef _NO_PROTO
   static int choose_cellfile ( );
   static int _plotcell       ( );
#else
   static int choose_cellfile (char *name, char *mapset);
   static int _plotcell (char *name, char *mapset );
#endif

static int use = 1;


/*---------------------------------------------------------------------*/
int plotcell()
{
  char name[40];
  char mapset[40];

  /*
   * Double check to make sure the cell list is available.
   * If so then call the plot routine.
   */

    if (access (cell_list,0) == 0) {
	if (!choose_cellfile (name, mapset))
	    return 0;
	/* add to the Display List */
	display_list.num_raster = 1;
	strcpy (display_list.rast_name, name);
	strcpy (display_list.rast_mapset, mapset);

	/* clear vectors in display_list */
	display_list.num_vectors = -1;

	_plotcell   (name, mapset);
      }
    else {
        return 0;
    }

    return 0;
}

/*---------------------------------------------------------------------*/
/* Plot cell files in VIEW_MAP2 (the target location)                  */
/* "name" and "mapset" define the map to be displayed                  */
/* Configures the views, selects target enviroment,                    */
/* then call drawcell to do the actual drawing                         */
/* RETURNS 1 always for no apparent reason                             */
/*---------------------------------------------------------------------*/
static int _plotcell (char *name,char *mapset)
{
    struct Cell_head cellhd;


    /* set internal stuff to the target location */
    select_target_env();

    /* get cell header for the desired file in the target location */
    if(G_get_cellhd(name, mapset, &cellhd) < 0)
      {
	select_current_env();
	return 1;
      }

    
    /* erase the VIEW2 (top right view) and unconfig both cell */
    /* and vector for it and its ZOOM view */
    Erase_view (VIEW_MAP2_ZOOM);
    VIEW_MAP2->cell.configured = 0;
    VIEW_MAP2->vect.configured = 0;
    VIEW_MAP2_ZOOM->cell.configured = 0;
    VIEW_MAP2_ZOOM->vect.configured = 0;

    /* now configure just the VIEW_MAP2 cell header */
    G_adjust_window_to_box (&cellhd, &VIEW_MAP2->cell.head, VIEW_MAP2->nrows, VIEW_MAP2->ncols);
    Configure_view (VIEW_MAP2, name, mapset, cellhd.ns_res, cellhd.ew_res);

    /* now draw it */
    drawcell(VIEW_MAP2);

    /* retrun to the current (source imagery) location */
    select_current_env();
    

    /* update the global flags above input methods for */ 
    /* marking control points */
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


    /* display the control points */
    display_points(1); 
    return 1;
}


/*------------------------------------------------------------------------*/
/* Ask the user for an imagery file to display.  Uses the cell_list build */
/* in main.c of all imagery group files.                                  */
/* "name" and "mapset" will contain the users response.                   */
/* RETURNS 1 if an imagery file was selected or                           */
/*         0 if nothing selected                                          */
/*------------------------------------------------------------------------*/
static int choose_cellfile (char *name,char *mapset)
{
    return ask_gis_files ("raster", cell_list, name, mapset, 1);
}
