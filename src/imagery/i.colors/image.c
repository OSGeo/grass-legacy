#include "raster.h"
#include "globals.h"
#include "local_proto.h"

int draw_image (View *view, int color)
{
    int n;
    int row, nrows, ncols;
    int x,y;
    struct Cell_head w;
    unsigned char *red, *grn, *blu;
    int n1 = 0,n2 = 0;

/* adjust window to view and postion it */
    G_adjust_window_to_box (&window, &w, view->nrows, view->ncols);
    G_set_window (&w);

    view->image.left = view->left + (view->ncols - w.cols)/2;
    view->image.right = view->image.left + w.cols - 1;
    view->image.top = view->top  + (view->nrows - w.rows)/2;
    view->image.bottom = view->image.top + w.rows - 1;

    Menu_msg("Plotting ...");
    R_standard_color (WHITE);
    Outline_box (view->image.top,
		 view->image.bottom,
	         view->image.left,
		 view->image.right);
    Outline_box (view->image.top+1,
		 view->image.bottom-1,
	         view->image.left+1,
		 view->image.right-1);
    
    if (group.ref.red.n < 0 && group.ref.grn.n < 0 && group.ref.blu.n < 0)
    {
	R_standard_color (BLACK);
	Solid_box (view->image.top+1,
	 	   view->image.bottom-1,
	           view->image.left+1,
		   view->image.right-1);
	return 0;
    }

    if(!I_open_image (&group.ref))
	return 0;

    nrows = G_window_rows();
    ncols = G_window_cols();
    x = view->image.left;
    y = view->image.top;

    switch (color)
    {
    case 'r':
	red = grn = blu = group.ref.red.buf;
	n1 = group.ref.grn.n; group.ref.grn.n = -1;
	n2 = group.ref.blu.n; group.ref.blu.n = -1;
	R_set_RGB_color (red_colors, red_colors, red_colors);
	break;
    case 'g':
	red = grn = blu = group.ref.grn.buf;
	n1 = group.ref.red.n; group.ref.red.n = -1;
	n2 = group.ref.blu.n; group.ref.blu.n = -1;
	R_set_RGB_color (grn_colors, grn_colors, grn_colors);
	break;
    case 'b':
	red = grn = blu = group.ref.blu.buf;
	n1 = group.ref.red.n; group.ref.red.n = -1;
	n2 = group.ref.grn.n; group.ref.grn.n = -1;
	R_set_RGB_color (blu_colors, blu_colors, blu_colors);
	break;
    default:
	red = group.ref.red.buf;
        grn = group.ref.grn.buf;
        blu = group.ref.blu.buf;
	R_set_RGB_color (red_colors, grn_colors, blu_colors);
	break;
    }

    for (row = 0; row < nrows; row += n)
    {
	R_move_abs (x, y+row);
	if ((n = I_get_image_row (&group.ref, row)) <= 0)
	    break;
	R_RGB_raster (ncols, n, red, grn, blu, 0);
    }
    I_close_image (&group.ref);
    R_set_RGB_color (red_colors, grn_colors, blu_colors);

    switch (color)
    {
    case 'r':
	group.ref.grn.n = n1;
	group.ref.blu.n = n2;
	break;
    case 'g':
	group.ref.red.n = n1;
	group.ref.blu.n = n2;
	break;
    case 'b':
	group.ref.red.n = n1;
	group.ref.grn.n = n2;
	break;
    }

    return 0;
}

int 
draw_red_band (void)
{
    Erase_view (VIEW_RED_IMAGE);
    if (group.ref.red.n >= 0)
	draw_image (VIEW_RED_IMAGE, 'r');

    return 0;
}

int 
draw_grn_band (void)
{
    Erase_view (VIEW_GRN_IMAGE);
    if (group.ref.grn.n >= 0)
	draw_image (VIEW_GRN_IMAGE, 'g');

    return 0;
}

int 
draw_blu_band (void)
{
    Erase_view (VIEW_BLU_IMAGE);
    if (group.ref.blu.n >= 0)
	draw_image (VIEW_BLU_IMAGE, 'b');

    return 0;
}
