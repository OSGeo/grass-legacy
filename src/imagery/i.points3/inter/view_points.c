#include "globals.h"
#include "raster.h"

int display_points(int in_color)
{

      Control_Points *points;


      /* make visiable */
      points = (Control_Points *) group.points;

      display_points_in_view (VIEW_MAP1, in_color,
	      points->points_temp.e1, points->points_temp.n1,
	      points->points_temp.status, points->points_temp.count);

      display_points_in_view (VIEW_MAP1_ZOOM, in_color,
	      points->points_temp.e1, points->points_temp.n1,
	      points->points_temp.status, points->points_temp.count);

      display_points_in_view (VIEW_MAP2, in_color,
	      points->points_temp.e2, points->points_temp.n2,
	      points->points_temp.status, points->points_temp.count);

      display_points_in_view (VIEW_MAP2_ZOOM, in_color,
	      points->points_temp.e2, points->points_temp.n2,
	      points->points_temp.status, points->points_temp.count);

    return 0;
}

int display_fiducial_points(int in_color)
{
Auxillary_Photo *auxil;

    /* check trans_type */
    if (group.trans_type != PHOTO) {
      /* TODO mesage */
      return (0);
    }

    /* make visiable */
    auxil = (Auxillary_Photo *) group.auxil;

    display_points_in_view (VIEW_MAP1, in_color,
	    auxil->points_fid.e1, auxil->points_fid.n1,
	    auxil->points_fid.status, auxil->points_fid.count);

    display_points_in_view (VIEW_MAP1_ZOOM, in_color,
	    auxil->points_fid.e1, auxil->points_fid.n1,
	    auxil->points_fid.status, auxil->points_fid.count);


    return 0;
}

int display_points_in_view (View *view, int in_color,
    double *east,double *north, int *status, int count)
{
    if ((!view->cell.configured)  && (!view->vect.configured)) 
      return 1;

    while (count-- > 0)
    {
	if (in_color && (*status > 0))
	    R_standard_color (I_COLOR_GREEN);
	else if (in_color && (*status == 0))
	    R_standard_color (I_COLOR_RED);
	else
	    R_standard_color (I_COLOR_GREY);
	status++;
	display_one_point (view, *east++, *north++);
    }

    return 0;
}

int display_one_point (View *view, double east,double north)
{
    int row, col, x, y;

    if (view->cell.configured) {
/**      row = northing_to_row (&view->cell.head, north) + .5;
**      col = easting_to_col  (&view->cell.head, east) + .5;
**/
      row = northing_to_row (&view->cell.head, north);
      col = easting_to_col  (&view->cell.head, east);

    }
    else if (view->vect.configured) {
/**      row = northing_to_row (&view->vect.head, north) + .5;
**      col = easting_to_col  (&view->vect.head, east) + .5;
**/
      row = northing_to_row (&view->vect.head, north);
      col = easting_to_col  (&view->vect.head, east);
    }

    y = row_to_view (view, row);
    x = col_to_view (view, col);
    if (In_view(view, x, y))
	dot (x,y);

    return 0;
}
