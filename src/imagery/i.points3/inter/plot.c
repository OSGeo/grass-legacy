/*=======================================================================
				i.points
  plot.c --
    int plot (void)
       The main zoom menu.
           CANCEL     - contained internally
           IMAGERY    - plotimg()   contained internally
	   RASTER     - plotcell()  plotcell.c
	   VECTOR     - 
	   REFRESH    -
	   CLEAR      -

       The routine Input_pointer loops untill the select option
       retuns a non-zero status.

       RETURNS:
          0     - always so that driver() does NOT quit

=======================================================================*/

#include "globals.h"

         /* internal function prototypes */
#ifdef _NO_PROTO
   static int cancel  ( );
   static int choose_cellfile ( );
/*    static int plotimg ( ); */
#else
   static int cancel  ( void );
   static int choose_cellfile (char *name, char *mapset);
/*   static int plotimg ( void ); */
#endif

static int use = 1;
static int use_cell = 1;
static int use_vect = 1;

/*---------------------------------------------------------------------
int plot()
{
    static Objects objects[] =
    {
	MENU ("CANCEL", cancel, &use),
	TITLE("<PLOT>", &use),
	MENU ("IMAGERY",plotimg, &use),
	MENU ("RASTER",plotcell, &use_cell), 
	MENU ("VECTOR",plotvect, &use_vect), 
	MENU ("REFRESH", plot_refresh, &use), 
	MENU ("CLEAR",   plot_clear, &use), 
	INFO ("Select Plot Function.", &use),
	{0}
    };

    Input_pointer (objects);
    return 0;     
}
---------------------------------------------------------------------*/

/*---------------------------------------------------------------------*/
/* Plot imagery group files in VIEW_MAP1 (not for target location)     */
/* Ask user for imagery file to plot, erase VIEW, configure VIEW,      */
/* then call drawcell to do the actual drawing                         */
/* RETURNS 0 allways so that Input_pointer in plot() above continues   */
/*---------------------------------------------------------------------*/
int plotimg()
{
    char name[100], mapset[100];
    struct Cell_head cellhd;


    /* ask for an imagery group file */
    if (!choose_groupfile(name,mapset))
      return 0;

    /* get  the selected files cell header */
    if(G_get_cellhd(name, mapset, &cellhd) < 0)
      return 0;
    
    /* erase the VIEW (top left view) and unconfig both cell */
    /* and vector for it and its ZOOM view */
    Erase_view (VIEW_MAP1_ZOOM);
    VIEW_MAP1->cell.configured = 0;
    VIEW_MAP1->vect.configured = 0;
    VIEW_MAP1_ZOOM->cell.configured = 0;
    VIEW_MAP1_ZOOM->vect.configured = 0;
    
    /* now configure just the VIEW_MAP1 cell header */
    G_adjust_window_to_box (&cellhd, &VIEW_MAP1->cell.head, VIEW_MAP1->nrows, VIEW_MAP1->ncols);
    Configure_view (VIEW_MAP1, name, mapset, cellhd.ns_res, cellhd.ew_res);

    /* now draw it */
    drawcell(VIEW_MAP1);

    /* now display the CONTROL POINTS */
    display_points(1);
    display_fiducial_points(1);  /** TODO -- not right **/

    return 0;  /* so that Input_pointer in plot() doesnt quit */
}

/*------------------------------------------------------------------------*/
/* Ask the user for an imagery file to display.  Uses the cell_list build */
/* in main.c of all imagery group files.                                  */
/* "name" and "mapset" will contain the users response.                   */
/* RETURNS 1 if an imagery file was selected or                           */
/*         0 if nothing selected                                          */
/*------------------------------------------------------------------------*/
static int choose_cellfile (name, mapset)
    char *name, *mapset;
{
    return ask_gis_files ("raster", cell_list, name, mapset, 1);
}

/*---------------------------------------------------------------------*/
static int cancel()
{
    return 1;   /* just return a non-zero value to Input_pointer in plot() */
}
