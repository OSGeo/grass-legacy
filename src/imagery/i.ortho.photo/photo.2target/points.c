#include "globals.h"
display_conz_points(in_color)
{
/*   double x,y;

    I_georef (group.control_points.e1, group.control_points.n1, &x, &y,
            group.E21, group.N21);
****/

    display_conz_points_in_view (VIEW_MAP1, in_color,  
            group.control_points.e1, group.control_points.n1, 
	    group.control_points.status, group.control_points.count);

    display_conz_points_in_view (VIEW_MAP1_ZOOM, in_color,  
            group.control_points.e1, group.control_points.n1,  
	    group.control_points.status, group.control_points.count);

    display_conz_points_in_view (VIEW_MAP2, in_color,
	    group.control_points.e2, group.control_points.n2,
	    group.control_points.status, group.control_points.count);

    display_conz_points_in_view (VIEW_MAP2_ZOOM, in_color,
	    group.control_points.e2, group.control_points.n2,
	    group.control_points.status, group.control_points.count);
}

display_conz_points_in_view (view, in_color, east, north, status, count)
    View *view;
    double *east, *north;
    int *status;
{
    if (!view->cell.configured) return;
    while (count-- > 0)
    {
	if (in_color && (*status > 0))
	    R_standard_color (GREEN);
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
    double x0, y0;

    row = northing_to_row (&view->cell.head, north) + .5;
    col = easting_to_col  (&view->cell.head, east) + .5;
    y = row_to_view (view, row);
    x = col_to_view (view, col);

    if ((view == VIEW_MAP1) || (view == VIEW_MAP1_ZOOM))
    {
/*
       I_georef (east,north,&x0,&y0,group.E21,group.N21); 
*/
       x0 = east;
       y0 = north;
       
       row = northing_to_row (&view->cell.head, y0) + .5;
       col = easting_to_col  (&view->cell.head, x0) + .5;
       y = row_to_view (view, row);
       x = col_to_view (view, col);
    }
 
    if (In_view(view, x, y))
	dot (x,y);
}
