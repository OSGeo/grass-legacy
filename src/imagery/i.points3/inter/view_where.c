#include "globals.h"

static int where_12 (View *,int,int);
static int where_21 (View *,int,int);
static int where_am_i (View *,int,int,Window *,double *,double *, Window *);

int where(int x,int y)
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

static int where_12 (View *view, int x, int y)
{
/*TODO    where_am_i (view, x, y, MENU_WINDOW, group.E12, group.N12, INFO_WINDOW); **/
	return 0;
}

static int where_21 (View *view, int x, int y)
{
/*TODO    where_am_i (view, x, y, INFO_WINDOW, group.E21, group.N21, MENU_WINDOW); **/

    return 0;
}

static int where_am_i (View *view,int x, int y, Window *w1,
    double *E,double *N, Window *w2)
{
    double e1,n1,e2,n2;
    int row,col;

    char buf[100];

/* convert x,y to east,north at center of cell */
    col = view_to_col (view, x);
    e1 = col_to_easting (&view->cell.head, col, 0.5);
    row = view_to_row (view, y);
    n1 = row_to_northing (&view->cell.head, row, 0.5);

/**    Curses_clear_window (w1);
**    sprintf (buf, "East:  %10.2lf", e1);
**    Curses_write_window (w1, 3, 3, buf);
**    sprintf (buf, "North: %10.2lf", n1);
**    Curses_write_window (w1, 4, 3, buf);
**/


/* if tranformation equation is useable, determine point via equation */
/*TODO    if (group.equation_stat <= 0) return; **/
/*TODO **/
/*TODO    I_georef (e1, n1, &e2, &n2, E, N); **/
/*TODO    Curses_clear_window (w2); **/
/*TODO    sprintf (buf, "East:  %10.2lf", e2); **/
/*TODO    Curses_write_window (w2, 3, 3, buf); **/
/*TODO    sprintf (buf, "North: %10.2lf", n2); **/
/*TODO    Curses_write_window (w2, 4, 3, buf); **/

    return 0;
}
