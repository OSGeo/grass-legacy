#include "gis.h"
#include "graphics.h"
#include "text.h"
#include "misc.h"
#include "parms.h"
#include "fullwindow.h"

extern double floor() ;

do_grid ()
{
    double g;
    double east, west, incr;
    int i;

    if (parms.grid <= 0)
        return;

/* set color to black, line width to 1 */

    set_color (parms.grid_color);
    set_width (1);

/* draw horizontal lines in 3 pieces -- lat-lon lines must not
 * extend more than half the globe
 * start with first grid line just south of the window north
 */
    west = graphics.window.west;
    incr = (graphics.window.east-graphics.window.west)/3;
    for (i = 0; i < 3; i++)
    {

	east = west + incr;
	g = floor (graphics.window.north/parms.grid) * parms.grid ;

	for ( ; g >= graphics.window.south; g -= parms.grid)
	{
	    if (g == fullwindow.north)
		continue;
	    if (g == fullwindow.south)
		continue;
	    G_plot_line(west, g, east, g);
	}

	west = east;
    }

/* vertical lines */
/* start with first grid line just west of the window east */

    g = floor (graphics.window.east/parms.grid) * parms.grid ;

    for ( ; g > graphics.window.west; g -= parms.grid)
    {
	if (g == fullwindow.east)
	    continue;
	if (g == fullwindow.west)
	    continue;
        G_plot_line(g, graphics.window.north, g, graphics.window.south);
    }
}

do_grid_numbers (panel)
{
    double g;
    char num_text[50];
    BOX H,V,box;
    int grid;
    int first, len, x, y;
    int rounded_grid;
    char *format_northing(), *format_easting();

    if (parms.grid <= 0 || parms.grid_numbers <= 0)
        return;
    grid = parms.grid * parms.grid_numbers;

/* round grid to multiple of 10 */
    rounded_grid = 1;
    if (fullwindow.proj != PROJECTION_LL)
    {
	sprintf (num_text, "%d", parms.grid);
	len = strlen (num_text);
	while (len-- && num_text[len] == '0')
	    rounded_grid *= 10;
	if (rounded_grid == 10)
	    rounded_grid = 1;
    }

/* initialize */
    select_standard_font();
    set_reasonable_text_size();
    set_text_width (1);
    set_text_hwidth (0);
    set_text_color (parms.grid_numbers_color);
    set_text_hcolor (-1);
    set_text_background (WHITE);
    set_text_border (-1);

    first = 1;

/* horizontal grid numbers */
/* these numbers only appear on the left edge of the first panel */
    if (panel != 1) goto Vertical;
    set_text_rotation (0);

/*
 * center the numbers on each grid line.
 * suppress number if it falls off the map or would overlay the previous
 *  label
 */

    g = floor (fullwindow.north/grid) * grid ;
    for ( ; g > fullwindow.south; g -= grid)
    {
        sprintf (num_text, "%s", format_northing(g,rounded_grid));

/**********************************************************************/
/* determine where to place the label.
 * (this logic will have to change when projections are supported)
 */
	G_plot_where_xy (fullwindow.west, g, &x, &y);
        x += 2 ;  /* move it over a bit */
/**********************************************************************/

        set_text_yref (CENTER);
        set_text_xref (LEFT);
        text_bounds(num_text,x,y,&box);

	if (offmap (&box))
	{
	    continue;
	}

        if (first)
        {
	    G_copy (&V, &box, sizeof box);
	    first = 0;
	}
	else if (overlap (&box, &H))
	{
	    continue;
	}

        draw_text (num_text, x, y);
	G_copy (&H, &box, sizeof box);
    }

/* vertical grid numbers */
Vertical:
    set_text_rotation (1) ;

/*
 * center the numbers on each grid line.
 * suppress number if it falls of the map or would overlay the previous
 *  label
 */

    g = floor (fullwindow.west/grid) * grid ;
    for ( ; g < fullwindow.east; g += grid)
    {
        sprintf (num_text, "%s", format_easting(g,rounded_grid));


/**********************************************************************/
/* determine where to place the label.
 * (this logic will have to change when projections are supported)
 */
	G_plot_where_xy (g, fullwindow.north, &x, &y);
        y += 2 ;  /* move it down a bit */
/**********************************************************************/

        set_text_yref (UPPER);
        set_text_xref (CENTER);
        text_bounds(num_text,x,y,&box);

        if (offmap(&box))
	{
	    continue;
	}

	if (first)
	    first = 0;
	else if (overlap (&box, &V))
	{
	    continue;
	}

        draw_text (num_text, x, y) ;
	G_copy (&V, &box, sizeof box);
    }
}

static overlap (b1, b2)
    BOX *b1, *b2;
{
    if (b1->bottom < b2->top) return 0;
    if (b2->bottom < b1->top) return 0;
    if (b1->left > b2->right) return 0;
    if (b2->left > b1->right) return 0;

    return 1;
}

static offmap (b)
    BOX *b;
{
    if (b->top < 0 || b->bottom >= fullwindow.rows) return 1;
    if (b->left < 0 || b->right >= fullwindow.cols) return 1;
    return 0;
}


static
char *format_northing(n, round)
    double n;
{
    static char text[50];

    if (fullwindow.proj == PROJECTION_LL)
	G_format_northing (n, text, fullwindow.proj);
    else
    {
	n = floor (n / round);
	sprintf (text,"%.0lf", n);
    }
    return text;
}

static
char *format_easting(e, round)
    double e;
{
    static char text[50];

    if (fullwindow.proj == PROJECTION_LL)
	G_format_easting (e, text, fullwindow.proj);
    else
    {
	e = floor (e / round);
	sprintf (text,"%.0lf", e);
    }
    return text;
}
