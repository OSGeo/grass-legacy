#include "raster.h"
#include "globals.h"
#include "local_proto.h"

static int fill(CELL *,unsigned char *,int,char,CELL,CELL,unsigned char *);

int 
allocate_color_bars (void)
{
    VIEW_RED->red = (unsigned char *) G_malloc (VIEW_RED->ncols);
    VIEW_RED->grn = (unsigned char *) G_malloc (VIEW_RED->ncols);
    VIEW_RED->blu = (unsigned char *) G_malloc (VIEW_RED->ncols);
    VIEW_RED->cell = (CELL *) G_malloc (VIEW_RED->ncols * sizeof(CELL));
    G_zero (VIEW_RED->grn, VIEW_RED->ncols);
    G_zero (VIEW_RED->blu, VIEW_RED->ncols);

    VIEW_GRN->red = (unsigned char *) G_malloc (VIEW_GRN->ncols);
    VIEW_GRN->grn = (unsigned char *) G_malloc (VIEW_GRN->ncols);
    VIEW_GRN->blu = (unsigned char *) G_malloc (VIEW_GRN->ncols);
    VIEW_GRN->cell = (CELL *) G_malloc (VIEW_GRN->ncols * sizeof(CELL));
    G_zero (VIEW_GRN->red, VIEW_GRN->ncols);
    G_zero (VIEW_GRN->blu, VIEW_GRN->ncols);

    VIEW_BLU->red = (unsigned char *) G_malloc (VIEW_BLU->ncols);
    VIEW_BLU->grn = (unsigned char *) G_malloc (VIEW_BLU->ncols);
    VIEW_BLU->blu = (unsigned char *) G_malloc (VIEW_BLU->ncols);
    VIEW_BLU->cell = (CELL *) G_malloc (VIEW_BLU->ncols * sizeof(CELL));
    G_zero (VIEW_BLU->red, VIEW_BLU->ncols);
    G_zero (VIEW_BLU->grn, VIEW_BLU->ncols);

    return 0;
}

int fill_color_bar (char color)
{
    switch (color)
    {
    case 'r':
	if (group.ref.red.n >= 0)
	  fill (VIEW_RED->cell, VIEW_RED->red, VIEW_RED->ncols, color,
		group.ref.red.min, group.ref.red.max, group.ref.red.index);
	break;
    case 'g':
	if (group.ref.grn.n >= 0)
	  fill (VIEW_GRN->cell, VIEW_GRN->grn, VIEW_GRN->ncols, color,
		group.ref.grn.min, group.ref.grn.max, group.ref.grn.index);
	break;
    case 'b':
	if (group.ref.blu.n >= 0)
	  fill (VIEW_BLU->cell, VIEW_BLU->blu, VIEW_BLU->ncols, color,
		group.ref.blu.min, group.ref.blu.max, group.ref.blu.index);
	break;
    }

    return 0;
}

static int fill (
    CELL *cell,
    unsigned char *buf,
    int ncols,
    char color,
    CELL min,CELL max,
    unsigned char *index)
{
    double x;
    int i;

/* first build a CELL buffer from min to max over ncols */
    x = (double)(max - min) / (double)(ncols-1);

    for (i = 0; i < ncols; i++)
	cell[i] = min + i * x;

/* now translate CELL data to unsigned char for graphics */
    I_translate_image_data (cell, buf, min, max, index, ncols);

    return 0;
}

int display_color_bar(View *view)
{
    load_colors();
    R_move_abs (view->left, view->top);
    R_RGB_raster (view->ncols, view->nrows, view->red, view->grn, view->blu, 0);

    return 0;
}
