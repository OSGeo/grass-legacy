#include <string.h>
#include <math.h>
#include "gis.h"
#include "graphics.h"
#include "text.h"
#include "misc.h"
#include "parms.h"
#include "fullwindow.h"
#include "grid.h"
#include "sgrid.h"

double intervalh, intervalw;
static int overlap (BOX *, BOX *);
static int offmap (BOX *);
static char *format_northing (double,int);
static char *format_easting (double,int);
static double Mheight (void);

int drawgrid (void)
{
    double g;
    double east, west, incr;
    int i;
    double dticke, dtickn;
    int x, y;
    int tick;

    tick = 0;

    if (m1panels > 1 && set_grid_on)
	m_set_grid	= 1;

    if (strcmp (grid.pattern, "tick")==0) 
	tick = 1;

    G_plot_where_xy(fullwindow.west, fullwindow.north, &x, &y);
    x	= x + grid.patternw;
    y	= y + grid.patternh;
    G_plot_where_en(x, y, &dticke, &dtickn);

    intervalh	= (fullwindow.north - dtickn)/2;
    intervalw	= (dticke - fullwindow.west)/2; 

/* draw horizontal lines in 3 pieces -- lat-lon lines must not
 * extend more than half the globe
 * start with first grid line just south of the window north
 */
    west = graphics.window.west;
    incr = (graphics.window.east - graphics.window.west)/3;

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

	    if (tick){ 
		draw_tickew(g);
	        if (fullwindow.south+intervalh+intervalh <= g)
                   G_plot_line(fullwindow.west, g, fullwindow.west+intervalh, g);
		}
	    else
	    {
	        G_plot_line(west, g, east, g);
            }
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
	if (tick) 
	   draw_tickns(g);
	else
           G_plot_line(g, graphics.window.north, g, graphics.window.south);
    }

    m_set_grid	= 0;
    drows	= 0;

    return 0;
}

int do_grid_numbers (int panel)
{
    double g;
    char num_text[50];
    BOX H,V,box;
    int grid;
    int first, len, x, y;
    int rounded_grid;

    set_num	= 1;
    if (parms.grid <= 0 || parms.grid_numbers <= 0)
        return 1;
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
	if (gridtextsize == 0.0)
    set_reasonable_text_size();
	else
	set_text_size(gridtextsize/Mheight());
    set_text_width (1);
    set_text_hwidth (0);
    set_text_color (parms.grid_numbers_color);
    set_text_hcolor (-1);
    set_text_background (gridtextbg);
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

	set_hort = 0;
/**********************************************************************/
/* determine where to place the label.
 * (this logic will have to change when projections are supported)
 */
	G_plot_where_xy (fullwindow.west, g, &x, &y);
        x += 2 ;  /* move it over a bit */
/**********************************************************************/

        set_text_yref (CENTER);
        set_text_xref (LEFT);
        text_bounds(num_text,x,y,&box, 1);

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

	set_hort = 1;
        draw_text (num_text, x, y, 2);
	G_copy (&H, &box, sizeof box);
	set_hort = 0;
    }

	set_vert = 0;
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
        text_bounds(num_text,x,y,&box, 1);

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

	set_vert = 1;
        draw_text (num_text, x, y, 2) ;
	G_copy (&V, &box, sizeof box);
	set_vert = 0;
    }

	set_num 	= 0;

    return 0;
}

static int overlap (BOX *b1, BOX *b2)
{
    if (b1->bottom < b2->top) return 0;
    if (b2->bottom < b1->top) return 0;
    if (b1->left > b2->right) return 0;
    if (b2->left > b1->right) return 0;

    return 1;
}

static int offmap (BOX *b)
{
    if (b->top < 0 || b->bottom >= fullwindow.rows+1000) return 1;
    if (b->left < 0 || b->right >= fullwindow.cols+1000) return 1;
    return 0;
}


static char *format_northing (double n, int round)
{
    static char text[50];

    if (fullwindow.proj == PROJECTION_LL)
	G_format_northing (n, text, fullwindow.proj);
    else
    {
	n = floor (n / round);
	sprintf (text,"%.0f", n);
    }
    return text;
}

static char *format_easting (double e, int round)
{
    static char text[50];

    if (fullwindow.proj == PROJECTION_LL)
	G_format_easting (e, text, fullwindow.proj);
    else
    {
	e = floor (e / round);
	sprintf (text,"%.0f", e);
    }
    return text;
}

void draw_tickew (double north)
{
double g;


G_plot_line(fullwindow.east, north, fullwindow.east-intervalw, north);

if (fullwindow.south+intervalh+intervalh > north) {


	return;
	}


if (parms.grid < (int)(2. * intervalh))
    g = floor (graphics.window.east/parms.grid) * parms.grid -
	parms.grid;
else
    g = floor (graphics.window.east/parms.grid) * parms.grid; 


    for ( ; g > graphics.window.west+intervalw*2; g -= parms.grid ) 
    {
	if (g == fullwindow.east)
	    continue;
	if (g == fullwindow.west)
	    continue;
    G_plot_line(g-intervalw, north, g+intervalw, north);
    }


}

void draw_tickns (double east)
{
double g;

	G_plot_line(east, fullwindow.north, east, fullwindow.north-intervalh);
	G_plot_line(east, fullwindow.south, east, fullwindow.south+intervalh);


g = floor (graphics.window.north/parms.grid) * parms.grid  ;

	for ( ; g >= graphics.window.south+intervalh*2; g -= parms.grid )
	{
	    if (g == fullwindow.north)
		continue;
	    if (g == fullwindow.south)
		continue;
	    G_plot_line(east, g-intervalh, east, g+intervalh);
	}

}

static double Mheight (void)
{
	BOX box;

	set_text_border(-1);
	set_text_background(-1);
	set_text_width(1);
	set_text_size(100.0);
	text_bounds ("M",0,0,&box, 0);
	return (fullwindow.ns_res * (box.bottom-box.top+1) / 100.0) ;
}
