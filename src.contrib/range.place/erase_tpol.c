#include "menu.h"

struct box *erase_polygon(n)

        struct box *n;
{
        int color;
         
        color = D_translate_color("gray");
        R_standard_color(color);
	draw_polygon(n->t-2,n->b+2,n->l-2,n->r+2);
        color = D_translate_color("black");
        R_standard_color(color);
        return(n);
}

