#include "globals.h"
display_points(in_color)
{

    display_points_in_view (VIEW_MAP1, in_color,
	    group.points.e1, group.points.n1,
	    group.points.status, group.points.count);

    display_points_in_view (VIEW_MAP1_ZOOM, in_color,
	    group.points.e1, group.points.n1,
	    group.points.status, group.points.count);

    display_points_in_view (VIEW_MAP2, in_color,
	    group.points.e2, group.points.n2,
	    group.points.status, group.points.count);

    display_points_in_view (VIEW_MAP2_ZOOM, in_color,
	    group.points.e2, group.points.n2,
	    group.points.status, group.points.count);

}

display_points_in_view (view, in_color, east, north, status, count)
    View *view;
    double *east, *north;
    int *status;
{
    if (!view->cell.configured) return;
    while (count-- > 0)
    {
	if (in_color && (*status > 0))
	    R_standard_color (YELLOW); /* GREEN); */
	else if (in_color && (*status == 0))
	    R_standard_color (RED);
	else
	    R_standard_color (GREY);
	status++;
	display_one_point (view, *east++, *north++);
    }
}

display_one_point (view, east, north)
    View *view;
    double east, north;
{
    int row, col, x, y;

    row = northing_to_row (&view->cell.head, north) + .5;
    col = easting_to_col  (&view->cell.head, east) + .5;
    y = row_to_view (view, row);
    x = col_to_view (view, col);
    if (In_view(view, x, y))
	dot (x,y);
}
