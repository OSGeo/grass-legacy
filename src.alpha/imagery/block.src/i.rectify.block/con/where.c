#include "globals.h"
where(x,y)
{
    if (VIEW_MAP1->cell.configured && In_view (VIEW_MAP1, x, y))
	where_12 (VIEW_MAP1, x, y);
    else if (VIEW_MAP1_ZOOM->cell.configured && In_view (VIEW_MAP1_ZOOM, x, y))
	where_12 (VIEW_MAP1_ZOOM, x, y);
    else if (VIEW_MAP2->cell.configured && In_view (VIEW_MAP2, x, y))
	where_21 (VIEW_MAP2, x, y);
    else if (VIEW_MAP2_ZOOM->cell.configured && In_view (VIEW_MAP2_ZOOM, x, y))
	where_21 (VIEW_MAP2_ZOOM, x, y);
    return 0 ; /* return but don't quit */
}

static
where_12 (view, x, y)
    View *view;
{
    where_am_i (view, x, y, MENU_WINDOW, group.E12, group.N12, INFO_WINDOW);
}

static
where_21 (view, x, y)
    View *view;
{
    where_am_i (view, x, y, INFO_WINDOW, group.E21, group.N21, MENU_WINDOW);
}

static
where_am_i (view, x, y, w1, E, N, w2)
    View *view;
    Window *w1, *w2;
    double *E, *N;
{
    double e1,n1,e2,n2;
    int row,col;

    char buf[100];

/* convert x,y to east,north at center of cell */
    col = view_to_col (view, x);
    e1 = col_to_easting (&view->cell.head, col, 0.5);
    row = view_to_row (view, y);
    n1 = row_to_northing (&view->cell.head, row, 0.5);

    Curses_clear_window (w1);
    sprintf (buf, "IMAGE X:  %10.2lf", e1);
    Curses_write_window (w1, 3, 3, buf);
    sprintf (buf, "IMAGE Y: %10.2lf", n1);
    Curses_write_window (w1, 4, 3, buf);

/* if tranformation equation is useable, determine point via equation */
    if (group.ref_equation_stat <= 0) return;

/*    I_georef (e1, n1, &e2, &n2, E, N); */
    e2 = e1;
    n2 = n1;

    Curses_clear_window (w2);
    sprintf (buf, "IMAGE X:  %10.2lf", e2);
    Curses_write_window (w2, 3, 3, buf);
    sprintf (buf, "IMAGE Y: %10.2lf", n2);
    Curses_write_window (w2, 4, 3, buf);
}



