/*=======================================================================
				i.points
  zoom_pnt.c --
    int zoom_point (void)
       The zoom_point routine allows the user to zoom either the 
       source imagery of target location VIEWS by selecting
       a center points to zoom into and a magnification factor.
.
       All currently displayed raster and vector data will be 
       redisplayed for the appropriate VIEW.  After redisplay, 
       any of the CONTROL_POINTS are redisplayed.

       The routine Input_pointer is used to call zoom1() on the 
       first mouse click.  

       zoom1() then pops up a "magnification" window to ask the 
       user for the magnification desired.
       
       zoom1() then does the appropriate steps to set the coorect 
       display window and region, then calls drawcell() and _plotvect()
       to do the actual redraw.

       RETURNS:
          0     - always so that zoom() does NOT quit

=======================================================================*/

#include "globals.h"
#include "raster.h"
#include "math.h"

         /* internal function prototypes */
#ifdef _NO_PROTO
   static int cancel ( );
   static int zoom1  ( );
#else
   static int cancel ( void );
   static int zoom1  (int x, int y);
#endif


static View *pick_view, *zoom_view, *main_view;
static int target_flag;
static int use = 1;

/*---------------------------------------------------------------------*/
/* called by zoom() in driver.c.  The (x,y) coords are passed by       */
/* Input_pointer and give the mouse position in display pixels         */
/* for the location of the mouse click                                 */
/*---------------------------------------------------------------------*/
int zoom_point(int x,int y)
{

    zoom1(x,y);
    return 0;
}

/*---------------------------------------------------------------------*/
/* called by zoom_point above.  The (x,y) coords are passed by         */
/* Input_pointer and give the mouse position in display pixels         */
/* for the location of the mouse click                                 */
/*---------------------------------------------------------------------*/
static int zoom1(int x,int y)	/* called by Input_pointer */
{
    int top, bottom, left, right;
    int n,row,col;
    int nrows, ncols;
    struct Cell_head cellhd;
    int mag;
    double magnification();
    double north, south, east, west;
    int i;

    if (In_view (pick_view = VIEW_MAP1, x, y))
    {
	main_view = VIEW_MAP1;
	zoom_view = VIEW_MAP1_ZOOM;
	target_flag = 0;
	/* just to be sure */
	if (!pick_view->cell.configured) return 0;
    }
    else if (In_view (pick_view = VIEW_MAP2, x, y))
    {
	if ((!pick_view->cell.configured) &&
	    (!pick_view->vect.configured))
	    return 0;	/* ignore the mouse event */
	main_view = VIEW_MAP2;
	zoom_view = VIEW_MAP2_ZOOM;
	target_flag = 1;
    }
    else if (In_view (pick_view = VIEW_MAP1_ZOOM, x, y))
    {
	if (!pick_view->cell.configured)
	    return 0;	/* ignore the mouse event */
	main_view = VIEW_MAP1;
	zoom_view = VIEW_MAP1_ZOOM;
	target_flag = 0;
    }
    else if (In_view (pick_view = VIEW_MAP2_ZOOM, x, y))
    {
	if ((!pick_view->cell.configured) &&
	    (!pick_view->vect.configured))
	    return 0;	/* ignore the mouse event */
	main_view = VIEW_MAP2;
	zoom_view = VIEW_MAP2_ZOOM;
	target_flag = 1;
    }
    else
	return 0;	/* ignore the mouse event */

/*
 * make sure point is within edges of image as well
 */
    if (pick_view->cell.configured) {
      if (x <= pick_view->cell.left) return 0;
      if (x >= pick_view->cell.right) return 0;
      if (y <= pick_view->cell.top) return 0;
      if (y >= pick_view->cell.bottom) return 0;
    }
    if (pick_view->vect.configured) {
      if (x <= pick_view->vect.left) return 0;
      if (x >= pick_view->vect.right) return 0;
      if (y <= pick_view->vect.top) return 0;
      if (y >= pick_view->vect.bottom) return 0;
    }


/*
 * ok, erase menu messages
 */
    Menu_msg("");

/* determine magnification of zoom */
    if ((zoom_view->cell.configured) || (zoom_view->vect.configured))
    {
	if (zoom_view == pick_view)
	    mag = floor(magnification (zoom_view) + 1.0) + .1;
	else
	    mag = ceil(magnification (zoom_view)) + .1;
    }
    else
    {
	mag = floor(magnification (main_view) + 1.0) + .1;
    }
    if(!ask_magnification (&mag))
	return 1;
/* 
 * Determine the the zoom window (ie, cellhd)
 */

    if (main_view->cell.configured) {
      G_copy (&cellhd, &pick_view->cell.head, sizeof(cellhd));
      cellhd.ns_res = main_view->cell.ns_res / mag;
      cellhd.ew_res = main_view->cell.ew_res / mag;
    }
    else if (main_view->vect.configured) {
      G_copy (&cellhd, &pick_view->vect.head, sizeof(cellhd));
      cellhd.ns_res = main_view->vect.ns_res / mag;
      cellhd.ew_res = main_view->vect.ew_res / mag;
    }

    cellhd.cols   = (cellhd.east - cellhd.west) / cellhd.ew_res;
    cellhd.rows   = (cellhd.north - cellhd.south) / cellhd.ns_res;


/* convert x,y to col,row */

    col  = view_to_col(pick_view,x);
    if (pick_view->cell.configured) {
      east = col_to_easting (&pick_view->cell.head, col, 0.5);
    }
    else if (pick_view->vect.configured) {
      east = col_to_easting (&pick_view->vect.head, col, 0.5);
    }
    col  = easting_to_col (&cellhd, east);

    row   = view_to_row(pick_view,y);
    if (pick_view->cell.configured) {
      north = row_to_northing (&pick_view->cell.head, row, 0.5);
    }
    else if (pick_view->vect.configured) {
      north = row_to_northing (&pick_view->vect.head, row, 0.5);
    }
    row   = northing_to_row (&cellhd, north);

    ncols = zoom_view->ncols ;
    nrows = zoom_view->nrows ;


    n = cellhd.cols - col;
    if (n > col)
	n = col;
    if (n+n+1 >= ncols)
    {
	n = ncols/2;
	if (n+n+1 >= ncols) n--;
    }
    left = col - n;
    right = col + n;

    n = cellhd.rows - row;
    if (n > row)
	n = row;
    if (n+n+1 >= nrows)
    {
	n = nrows/2;
	if (n+n+1 >= nrows) n--;
    }
    top = row - n;
    bottom = row + n;


    north = row_to_northing (&cellhd, top,0.0);
    west  = col_to_easting  (&cellhd,left,0.0);
    south = row_to_northing (&cellhd,bottom,1.0);
    east  = col_to_easting  (&cellhd,right,1.0);


    cellhd.north = north;
    cellhd.south = south;
    cellhd.east  = east ;
    cellhd.west  = west ;

    cellhd.rows = (cellhd.north-cellhd.south)/cellhd.ns_res;
    cellhd.cols = (cellhd.east-cellhd.west)/cellhd.ew_res ;

/*
 * Outline the zoom window on the main map
 * Turn previous one to grey.
 */
    if (zoom_view->cell.configured)
    {
	R_standard_color (I_COLOR_GREY);
	Outline_cellhd (main_view, &zoom_view->cell.head);
    }
    if (zoom_view->vect.configured)
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
    if (target_flag)
	select_target_env();
    G_copy (&zoom_view->cell.head, &cellhd, sizeof (cellhd));
    Configure_view (zoom_view, pick_view->cell.name, pick_view->cell.mapset,
	pick_view->cell.ns_res, pick_view->cell.ew_res);
    drawcell (zoom_view);
    select_current_env();


    /* display vectors */
    if (target_flag) {
      select_target_env();

      if (display_list.num_vectors >= 0) {

	G_copy (&zoom_view->vect.head, &cellhd, sizeof (cellhd));
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
