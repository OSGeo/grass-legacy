/*=======================================================================
				i.points
  zoom_box.c --
    int zoom_box (int x, int y)
       The zoom_box routine allows the user to zoom either the 
       source imagery of target location VIEWS using a zoom box.
       All currently displayed raster and vector data will be 
       redisplayed for the appropriate VIEW.  After redisplay, 
       any of the CONTROL_POINTS are redisplayed.


       The routine Input_pointer is used to call zoom1() on the 
       first mouse click.  

       zoom1() sets up a second Input_pointer to call zoom2()
       when the user clicks the second time to define the box.

       zoom2() then does the appropriate step to set the coorect 
       display window and region, then calls drawcell() and _plotvect()
       to do the actual redraw.

       RETURNS:
          0     - always so that zoom() does NOT quit

=======================================================================*/

#include "globals.h"
#include "raster.h"


         /* internal function prototypes */
#ifdef _NO_PROTO
   static int cancel ( );
   static int zoom1  ( );
   static int zoom2  ( );
#else
   static int cancel ( void );
   static int zoom1  (int x, int y);
   static int zoom2  (int x, int y);
#endif


static int x1, y1, x2, y2;
static View *pick_view, *zoom_view, *main_view;
static int target_flag;
static int use = 1;

/*---------------------------------------------------------------------*/
/* called by zoom() in driver.c.  The (x,y) coords are passed by       */
/* Input_pointer and give the mouse position in display pixels         */
/* for the location of the mouse click                                 */
/*---------------------------------------------------------------------*/
int zoom_box(int x,int y)
{

  zoom1(x,y);

  /* return but dont quit */
  return 0;
}

/*---------------------------------------------------------------------*/
/* called by zoom_box above.  The (x,y) coords are passed by           */
/* Input_pointer and give the mouse position in display pixels         */
/* for the location of the mouse click                                 */
/*---------------------------------------------------------------------*/
static int zoom1(int x,int y)     
{


    /* set up objects for the second mouse click */
    static Objects objects[] =
    {
	MENU("CANCEL",cancel,&use),
	TITLE("<ZOOM-BOX>",&use),
	INFO(" Define the zoom box with mouse.",&use),
	OTHER(zoom2,&use),
	{0}
    };



    /* 
     * user has marked first corner 
     * this determines which view is being zoomed
     */
    x1 = x;
    y1 = y;

    if (In_view (pick_view = VIEW_MAP1, x1, y1))
    {
	main_view = VIEW_MAP1;
	zoom_view = VIEW_MAP1_ZOOM;
	target_flag = 0;
	/* just to be sure */
	if (!pick_view->cell.configured) return 0;
    }
    else if (In_view (pick_view = VIEW_MAP2, x1, y1))
    {
	if ((!pick_view->cell.configured) &&
	    (!pick_view->vect.configured))
	    return 0;	/* ignore the mouse event */
	main_view = VIEW_MAP2;
	zoom_view = VIEW_MAP2_ZOOM;
	target_flag = 1;
    }
    else if (In_view (pick_view = VIEW_MAP1_ZOOM, x1, y1))
    {
	if (!pick_view->cell.configured)
	    return 0;	/* ignore the mouse event */
	main_view = VIEW_MAP1;
	zoom_view = VIEW_MAP1_ZOOM;
	target_flag = 0;
    }
    else if (In_view (pick_view = VIEW_MAP2_ZOOM, x1, y1))
    {
	if ((!pick_view->cell.configured) &&
	    (!pick_view->vect.configured))
	    return 0;	/* ignore the mouse event */
	main_view = VIEW_MAP2;
	zoom_view = VIEW_MAP2_ZOOM;
	target_flag = 1;
    }
    else
	return 0;	/* ignore the mouse event if not in a VIEW */


    /* now call Input_pointer to get second point */
    return Input_box (objects, x, y);
}

/*---------------------------------------------------------------------*/
/* called by Input_pointer routine in zoom1()                          */
/* Determines the zoom window and configures the appropriate view      */
/* Finnally call drawcell and _plotvect to do the redisplay            */
static int zoom2 (int x,int y)
{
    int top, bottom, left, right;
    int row,col;
    int i;
    struct Cell_head cellhd;

    x2 = x;
    y2 = y;

    /* 
     * user has completed the zoom window.
     * must be in same view as first corner
     */
    if (x1 == x2 || y1 == y2) return 0;	/* ignore event */
    if (!In_view (pick_view,x2,y2)) return 0;

    /*
     * ok, erase menu messages
     */
    Menu_msg("");

    /*
     * assign window coordinates to top,bottom,left,right
     */
    if (x1 < x2)
    {
	left = x1;
	right = x2;
    }
    else
    {
	left = x2;
	right = x1;
    }
    if (y1 < y2)
    {
	top = y1;
	bottom = y2;
    }
    else
    {
	top = y2;
	bottom = y1;
    }

/* 
 * Determine the the zoom window (ie, cellhd)
 * must copy the current view cellhd first, to preserve header info
 * (such as projection, zone, and other items.)
 * compute zoom window northings,eastings, rows, cols, and resolution
 */

    if (pick_view->cell.configured)
      G_copy (&cellhd, &pick_view->cell.head, sizeof(cellhd));
    else if (pick_view->vect.configured)
      G_copy (&cellhd, &pick_view->vect.head, sizeof(cellhd));

/* convert top to northing at top edge of cell
 * left to easting at left edge
 */
    col = view_to_col(pick_view,left);
    row = view_to_row(pick_view,top);
    if (pick_view->cell.configured) {
      cellhd.north = row_to_northing (&pick_view->cell.head,row,0.0);
      cellhd.west  = col_to_easting  (&pick_view->cell.head,col,0.0);
    }
    else if (pick_view->vect.configured) {
      cellhd.north = row_to_northing (&pick_view->vect.head,row,0.0);
      cellhd.west  = col_to_easting  (&pick_view->vect.head,col,0.0);
    }

/* convert bottom to northing at bottom edge of cell
 * right to easting at right edge
 */
    col = view_to_col(pick_view,right);
    row = view_to_row(pick_view,bottom);
    if (pick_view->cell.configured) {
      cellhd.south = row_to_northing (&pick_view->cell.head,row,1.0);
      cellhd.east  = col_to_easting  (&pick_view->cell.head,col,1.0);
    }
    else if (pick_view->vect.configured) {
      cellhd.south = row_to_northing (&pick_view->vect.head,row,1.0);
      cellhd.east  = col_to_easting  (&pick_view->vect.head,col,1.0);
    }

    cellhd.rows = bottom-top+1;
    cellhd.cols = right-left+1;
    cellhd.ns_res = (cellhd.north-cellhd.south)/cellhd.rows;
    cellhd.ew_res = (cellhd.east-cellhd.west)/cellhd.cols;

/*
 * Outline the zoom window on the main map
 * Turn previous one to grey.
 */
    if (zoom_view->cell.configured)
    {
	R_standard_color (I_COLOR_GREY);
	Outline_cellhd (main_view, &zoom_view->cell.head);
    }
    else if (zoom_view->vect.configured)
    {
	R_standard_color (I_COLOR_GREY);
	Outline_cellhd (main_view, &zoom_view->vect.head);
    }
    /* the new one */
    R_standard_color (I_COLOR_RED);
    Outline_cellhd (main_view, &cellhd);


/*
 * zoom
 */
    /* zoom imagery */
    if (!target_flag) {
      G_adjust_window_to_box (&cellhd, &zoom_view->cell.head, zoom_view->nrows, zoom_view->ncols);
      Configure_view (zoom_view, pick_view->cell.name, pick_view->cell.mapset,
		      pick_view->cell.ns_res, pick_view->cell.ew_res);

      drawcell (zoom_view);
    }

    /* target stuff */
    else if (target_flag) {
      select_target_env();

      Erase_view (VIEW_MAP2_ZOOM);
      
      /* display raster */
      if (display_list.num_raster > 0) {
	G_adjust_window_to_box (&cellhd, &zoom_view->cell.head, zoom_view->nrows, zoom_view->ncols);
	Configure_view (zoom_view, pick_view->cell.name, pick_view->cell.mapset,
			pick_view->cell.ns_res, pick_view->cell.ew_res);
	drawcell (zoom_view);
      }

      /* any vectors to zoom */
      if (display_list.num_vectors >= 0) {

	G_adjust_window_to_box (&cellhd, &zoom_view->vect.head, zoom_view->nrows, zoom_view->ncols);
	Configure_view_vect (zoom_view, pick_view->vect.name, pick_view->vect.mapset,
			     pick_view->vect.ns_res, pick_view->vect.ew_res);
	
	for (i = 0; i <= display_list.num_vectors; i++) 
	  _plotvect(display_list.vects[i].vect_name,
		    display_list.vects[i].vect_mapset,
		    display_list.vects[i].vect_color, 1);
	select_current_env();
      }
    }

    display_points(1); 
    return 1;	/* pop back */
}

/*---------------------------------------------------------------------*/
static int cancel(void)
{
    return -1;
}
