#include "globals.h"

display_red_color_info()
{
    Erase_view (VIEW_RED);
    if (group.ref.red.n >= 0)
	display (VIEW_RED, 'r');
    draw_red_band();
}

display_grn_color_info()
{
    Erase_view (VIEW_GRN);
    if (group.ref.grn.n >= 0)
	display (VIEW_GRN, 'g');
    draw_grn_band();
}

display_blu_color_info()
{
    Erase_view (VIEW_BLU);
    if (group.ref.blu.n >= 0)
	display (VIEW_BLU, 'b');
    draw_blu_band();
}

static
display (view, color)
    View *view;
    char color;
{
    fill_color_bar (color);
    display_color_bar (view);
}
