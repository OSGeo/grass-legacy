/* Functions: ps_outline, outlinefile
**
** Author: Paul W. Carlson	May 1992
*/

#include "ps_info.h"
#include <stdio.h>

extern int verbose;
static int k, col, row, top, bottom;
static CELL tl, tr, bl, br;
static CELL *buffer[2];
static int scan_length;
double e1, e2, e3, n1, n2, n3;
/*
  e1 e2 e3
   *--*--* n1
   |  |  |
   *--*--* n2
   |  |  |
   *--*--* n3
*/
/* the ps_outline function creates a vector file called "tmp.outl" in
** the current location.  This file is removed after it has been
** plotted.
*/

ps_outline()
{
    /* let user know what's happenning */
    if (verbose > 1)
    {
        printf("PS-PAINT: outlining areas in raster file <%s in %s> ...",
	    PS.cell_name, PS.cell_mapset);
        fflush(stdout);
    }
    /* set the outline color and width */
    set_rgb_color(PS.outline_color);
    set_line_width(PS.outline_width );

    /* create temporary vector file containing outlines */
    o_io_init();
    o_open_file(PS.cell_name);
    draw_outline();
    o_close_file();

    if (verbose > 1) printf("\n");
}


/* The outlinefile function is just slightly modified p.map code. */
#define KEY(x) (strcmp(key,x)==0)
static char *help[] =
{
    "color  color",
    "width  #",
    ""
};
outlinefile()
{	
    char buf[1024];
    char ch, *key, *data;
    int color;

    PS.outline_width = 1.;
    color = BLACK;
    while (input(2, buf, help))
    {
	if (!key_data(buf, &key, &data)) continue;
	if (KEY("color"))
	{
	    color = get_color_number(data);
	    if (color < 0)
	    {
		color = BLACK;
		error(key, data, "illegal color request");
	    }
	    continue;
	}
	if (KEY("width"))
	{
	    PS.outline_width = -1.;
	    ch = ' ';
	    if(sscanf(data, "%lf%c", &(PS.outline_width), &ch) < 1 
	       || PS.outline_width < 0.)
	    {
	        PS.outline_width = 1.;
		error(key, data, "illegal width request");
	    }
	    if(ch=='i') PS.outline_width = PS.outline_width/72.;
	    continue;
	}
	error(key, data, "illegal outline sub-request");
	error(key, data, "illegal outline sub-request");
    }
    PS.outline_color = color;
    PS.do_outline = 1;
}


/* draw_outline - draw boundaries of polygons in file */

draw_outline()
{
  row = col = top = 0;			/* get started for read of first */
  bottom = 1;				/*   line from cell file */
  scan_length = read_next();
  k = 0;
  while (read_next())			/* read rest of file, one row at */
  {					/*   a time */
     n1 = G_row_to_northing((double)row -1., &(PS.w));
     n2 = G_row_to_northing((double)row, &(PS.w));
     n3 = G_row_to_northing((double)row + 1., &(PS.w));

    for (col = 0; col < scan_length - 1; col++)
    {
      e1 = G_col_to_easting((double)col - 1., &(PS.w));
      e2 = G_col_to_easting((double)col, &(PS.w));
      e3 = G_col_to_easting((double)col + 1., &(PS.w));
      tl = *(buffer[top] + col);	/* top left in window */
      tr = *(buffer[top] + col + 1);	/* top right */
      bl = *(buffer[bottom] + col);	/* bottom left */
      br = *(buffer[bottom] + col + 1);	/* bottom right */
      draw_boundaries();
      if (k==3) k = 0;
    }
    row++;
  }
}					/* draw_outlines */


static 
draw_boundaries()
{
  CELL right, left;

  if( bl != br) draw_bot();
  if( tr != br) draw_rite();
}						/* draw_boundaries */

/* read_next - read another line from input file */

static int 
read_next()
{
  int n;

  top = bottom++;			/* switch top and bottom, */
  bottom = 1 & bottom;			/*   which are always 0 or 1 */
  n = o_read_row(buffer[bottom]);
  return(n);
}

/* alloc_bufs - allocate buffers we will need for storing cell file */
/* data, pointers to extracted lines, area number information */

o_alloc_bufs(size)
    int size;
{
  char *G_calloc();
  int i;

  buffer[0] = (CELL *) G_calloc(size, sizeof(CELL));
  buffer[1] = (CELL *) G_calloc(size, sizeof(CELL));
}

draw_top()
/*    *--*--*    */
/*    |  |  |    */
/*    *  |  *    */
/*    |     |    */
/*    *--*--*    */
{
   start_line(e2, n2);
   sec_draw = 0;
   G_plot_line(e2, n2, e2, n1);
   if(++k==3) fprintf(PS.fp," D\n");
   else fprintf(PS.fp, " D ");
}

draw_rite()
/*    *--*--*    */
/*    |     |    */
/*    *  ---*    */
/*    |     |    */
/*    *--*--*    */
{
   start_line(e2, n2);
   sec_draw = 0;
   G_plot_line(e2, n2, e3, n2);
   if(++k==3) fprintf(PS.fp," D\n");
   else fprintf(PS.fp, " D ");
}

draw_left()
/*    *--*--*    */
/*    |     |    */
/*    *---  *    */
/*    |     |    */
/*    *--*--*    */
{
   start_line(e2, n2);
   sec_draw = 0;
   G_plot_line(e2, n2, e1, n2);
   if(++k==3) fprintf(PS.fp," D\n");
   else fprintf(PS.fp, " D ");
}

draw_bot()
/*    *--*--*    */
/*    |     |    */
/*    *  |  *    */
/*    |  |  |    */
/*    *--*--*    */
{
   start_line(e2, n2);
   sec_draw = 0;
   G_plot_line(e2, n2, e2, n3);
   if(++k==3) fprintf(PS.fp," D\n");
   else fprintf(PS.fp, " D ");
}
