/*=======================================================================
				i.points
  view.c --
     Configure_view (view, name, mapset, ns_res, ew_res)
        Configure a view (i.e. VIEW_MAP1, VIEW_MAP2, ...) by
	writing the name and mapset of the cell file.  Also 
	set the view.cellhd resolutions to the values passed by
	ns_res and ew_res.  The view->cell.left (right, top, bottom)
	are pixel values

     Configure_view_vect (view, name, mapset, ns_res, ew_res)
        Just like the above routine except deals with vector data
	layers.

     In_view (view, x, y)
        Returns true if the points (x,y) is contained in the
	view boundaries. 

     Erase_view (view)
        Erase the specified view by calling R_abs_box with the view
	dimensions.  The erase color is black.

     magnification (view)
        Returns a double indicating the relative magnification.  The 
	mag is calculated as the ratio of view resolution to the 
	original cell (or vect) resolution.  This really doesn't make 
	sense for vectors.  The magnification is diplay for the
	ZOOM VIEWS on the title lines.
=======================================================================*/


#include "globals.h"
#include <string.h>
#include "raster.h"

         /* internal function prototypes */
#ifdef _NO_PROTO
#else
#endif

/*---------------------------------------------------------------------*/
void Configure_view (
     View *view,
     char *name,char *mapset,
     double ns_res,double ew_res)	/* original map resolution */
{
    Erase_view(view);
    view->cell.configured = 0;

/* copy the cell name into the view */
    strcpy (view->cell.name, name);
    strcpy (view->cell.mapset, mapset);

/* determine the map edges */
    view->cell.left   = view->left + (view->ncols - view->cell.head.cols)/2;
    view->cell.right  = view->cell.left + view->cell.head.cols - 1;
    view->cell.top    = view->top  + (view->nrows - view->cell.head.rows)/2;
    view->cell.bottom = view->cell.top + view->cell.head.rows - 1;

/* remember original resolutions */
    view->cell.ns_res = ns_res;
    view->cell.ew_res = ew_res;

    view->cell.configured = 1;

    return;
}



/*---------------------------------------------------------------------*/
void Configure_view_vect (
     View *view,
     char *name,char *mapset,
     double ns_res,double ew_res)	/* original map resolution */
{
    /* Erase_view(view); */
    view->vect.configured = 0;

/* copy the cell name into the view */
    strcpy (view->vect.name, name);
    strcpy (view->vect.mapset, mapset);

/* determine the map edges */
    view->vect.left   = view->left + (view->ncols - view->vect.head.cols)/2;
    view->vect.right  = view->vect.left + view->vect.head.cols - 1;
    view->vect.top    = view->top  + (view->nrows - view->vect.head.rows)/2;
    view->vect.bottom = view->vect.top + view->vect.head.rows - 1;

/* remember original resolutions */
    view->vect.ns_res = ns_res;
    view->vect.ew_res = ew_res;

    view->vect.configured = 1;

    return;
}


/*---------------------------------------------------------------------*/
int In_view (View *view,int x,int y)
{
    return (x >= view->left && x <= view->right && y >= view->top && y <= view->bottom);
}

/*---------------------------------------------------------------------*/
int Erase_view (View *view)
{
    R_standard_color (I_COLOR_BLACK);
    R_box_abs (view->left, view->top, view->right, view->bottom);

    return 0;
}


/*---------------------------------------------------------------------*/
double magnification (View *view)
{
    if ((!view->cell.configured) && (!view->vect.configured))
	return ((double) 0.0);

    if (view->cell.configured)
       return (view->cell.ew_res / view->cell.head.ew_res); 

    else if (view->vect.configured)
       /* return (view->vect.ew_res / view->vect.head.ew_res); */
       return (1.0);
}

