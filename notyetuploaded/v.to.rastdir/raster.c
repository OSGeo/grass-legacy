#include "gis.h"
#include "format.h"
#include "null.h"
#define debug stderr

struct Cell_head region, page;

static union
{
    char **c;
    unsigned char **u;
    short **s;
    CELL **cell;
} raster;
static int max_rows;
static int at_row;
static CELL cat;
static int cur_x, cur_y;
static int format;
static CELL *cell;
static char **null_flags;
static char isnull;

int move(), cont();
int uchar_dot(), char_dot(), short_dot(), cell_dot();

static int (*dot)();

begin_rasterization(nrows, f)
{
    int i,size;
    int pages;

    format = f;

    max_rows = nrows;
    if (max_rows <= 0)
	max_rows = 512;

    G_get_set_window (&region);
    G_get_set_window (&page);

    pages = (region.rows + max_rows - 1) / max_rows;

    if (max_rows > region.rows)
	max_rows = region.rows;

    size = max_rows * region.cols;
    switch (format)
    {
    case USE_CHAR:
	raster.c = (char **) G_calloc (max_rows, sizeof (char *));
	raster.c[0] = (char *) G_calloc (size, sizeof(char));
	for (i = 1; i < max_rows; i++)
	    raster.c[i] = raster.c[i-1] + region.cols;
	dot = char_dot;
	break;

    case USE_UCHAR:
	raster.u = (unsigned char **) G_calloc (max_rows, sizeof (unsigned char *));
	raster.u[0] = (unsigned char *) G_calloc (size, sizeof(unsigned char));
	for (i = 1; i < max_rows; i++)
	    raster.u[i] = raster.u[i-1] + region.cols;
	dot = uchar_dot;
	break;

    case USE_SHORT:
	raster.s = (short **) G_calloc (max_rows, sizeof (short *));
	raster.s[0] = (short *) G_calloc (size, sizeof(short));
	for (i = 1; i < max_rows; i++)
	    raster.s[i] = raster.s[i-1] + region.cols;
	dot = short_dot;
	break;

    case USE_CELL:
	raster.cell = (CELL **) G_calloc (max_rows, sizeof (CELL *));
	raster.cell[0] = (CELL *) G_calloc (size, sizeof(CELL));
	for (i = 1; i < max_rows; i++)
	    raster.cell[i] = raster.cell[i-1] + region.cols;
	dot = cell_dot;
	break;
    }
    if (format != USE_CELL)
	cell = G_allocate_cell_buf();

    null_flags = (char **) G_calloc (max_rows, sizeof (char *));
    null_flags[0] = (char *) G_calloc (size, sizeof(char));
    for (i = 1; i < max_rows; i++)
	null_flags[i] = null_flags[i-1] + region.cols;

    at_row = 0;
    configure_plot();

    return pages;
}

#define DONE 1
#define ERROR -1
#define AGAIN 0

configure_plot()
{
    int i,j;
    int nrows;
    int ncols;

    nrows = region.rows - at_row;
    if (nrows <= 0)
	return DONE;

    if (nrows > max_rows)
	nrows = max_rows;
    
    ncols = region.cols;

/* zero the raster */
    switch (format)
    {
    case USE_CHAR:
	for (i = 0; i < nrows; i++)
	    for (j = 0; j < ncols; j++)
		raster.c[i][j] = 0;
	break;

    case USE_UCHAR:
	for (i = 0; i < nrows; i++)
	    for (j = 0; j < ncols; j++)
		raster.u[i][j] = 0;
	break;

    case USE_SHORT:
	for (i = 0; i < nrows; i++)
	    for (j = 0; j < ncols; j++)
		raster.s[i][j] = 0;
	break;

    case USE_CELL:
	for (i = 0; i < nrows; i++)
	    for (j = 0; j < ncols; j++)
		raster.cell[i][j] = 0;
	break;
    }

    for (i = 0; i < nrows; i++)
	for (j = 0; j < ncols; j++)
	    null_flags[i][j] = 1;

/* change the region */
    page.north = region.north - at_row * region.ns_res;
    page.south = page.north - nrows * region.ns_res;
    G_set_window (&page);

/* configure the plot routines */
    G_setup_plot (-0.5, page.rows-0.5, -0.5, page.cols-0.5, move, cont);

    return AGAIN;
}

output_raster (fd)
{
    int i,j;

    for (i = 0; i < page.rows; i++, at_row++)
    {
	switch (format)
	{

	case USE_CHAR:
	    for (j = 0; j < page.cols; j++)
		cell[j] = (CELL) raster.c[i][j];
	    break;

	case USE_UCHAR:
	    for (j = 0; j < page.cols; j++)
		cell[j] = (CELL) raster.u[i][j];
	    break;

	case USE_SHORT:
	    for (j = 0; j < page.cols; j++)
		cell[j] = (CELL) raster.s[i][j];
	    break;

	case USE_CELL:
	    cell = raster.cell[i];
	    break;
	}
	/* insert the NULL values */
	G_insert_c_null_values (cell, null_flags[i], page.cols);
	if (G_put_c_raster_row (fd, cell) < 0)
	    return ERROR;
    }
    return configure_plot();
}

set_cat(x)
    CELL x;
{
    cat = x;
    if (isnull = ISNULL(&cat))
	cat = 0;
}

raster_dot(x,y)
{
    dot(x,y);
}

static
move (x, y)
{
    cur_x = x;
    cur_y = y;
}

static
cont (x, y)
{
    if(cur_x < 0 && x < 0) goto set;
    if(cur_y < 0 && y < 0) goto set;
    if(cur_x >= page.cols && x >= page.cols) goto set;
    if(cur_y >= page.rows && y >= page.rows) goto set;

    G_bresenham_line (cur_x, cur_y, x, y, dot);

set:
    move (x, y);
}

static
cell_dot (x, y)
{
    if (x >= 0 && x < page.cols && y >= 0 && y < page.rows)
    {
	raster.cell[y][x] = cat;
	null_flags[y][x] = isnull;
    }
}

static
uchar_dot (x, y)
{
    if (x >= 0 && x < page.cols && y >= 0 && y < page.rows)
    {
	raster.u[y][x] = cat;
	null_flags[y][x] = isnull;
    }
}

static
char_dot (x, y)
{
    if (x >= 0 && x < page.cols && y >= 0 && y < page.rows)
    {
	raster.c[y][x] = cat;
	null_flags[y][x] = isnull;
    }
}

static
short_dot (x, y)
{
    if (x >= 0 && x < page.cols && y >= 0 && y < page.rows)
    {
	raster.s[y][x] = cat;
	null_flags[y][x] = isnull;
    }
}
