#include "menu.h"

struct box *erase_in_poly(ptr)
	struct box *ptr;
{

	int color;
	int t,b,l,r;
        register line;
	extern struct box talk;

	t= talk.t + 1;
	b= talk.b - 1;
	l= talk.l + 1;
	r= talk.r - 1;
	
	color = D_translate_color("black");
        R_standard_color(color);
	paint_block(t,b,l,r);
        color = D_translate_color("gray");
        R_standard_color(color);

        return(ptr);
}

		
