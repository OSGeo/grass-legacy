#include "globals.h"
#include "local_proto.h"

static int display(View *,char);

int 
display_red_color_info (void)
{
    Erase_view (VIEW_RED);
    if (group.ref.red.n >= 0)
	display (VIEW_RED, 'r');
    draw_red_band();

    return 0;
}

int 
display_grn_color_info (void)
{
    Erase_view (VIEW_GRN);
    if (group.ref.grn.n >= 0)
	display (VIEW_GRN, 'g');
    draw_grn_band();

    return 0;
}

int 
display_blu_color_info (void)
{
    Erase_view (VIEW_BLU);
    if (group.ref.blu.n >= 0)
	display (VIEW_BLU, 'b');
    draw_blu_band();

    return 0;
}

static int display ( View *view, char color)
{
    fill_color_bar (color);
    display_color_bar (view);

    return 0;
}
