#include "menu.h"

struct box *scroll(fnd)

	struct box *fnd;

{
	int t=88,b=85,l=86,r=95;
	extern struct box *ahead;
	struct box *checker;
	extern int layer_count;
	int color;
	register i;
	extern int perm_cell_count,user_cell_count,
			basic_patt_count,land_patt_count;
	extern struct box *perm_cell_ptr,*user_cell_ptr,
				*basic_patt_ptr,*land_patt_ptr;

	struct box *parent;

	parent = fnd->parent;

	if(parent == perm_cell_ptr)	layer_count = perm_cell_count;
 else   if(parent == user_cell_ptr)	layer_count = user_cell_count;
 else   if(parent == basic_patt_ptr) 	layer_count = basic_patt_count;
 else   if(parent == land_patt_ptr)	layer_count = land_patt_count;
	

	if(layer_count <= 21) return(fnd);
	else
	{

	checker = ahead;
	do{ 
	if(checker->t != 0 && checker != fnd && checker != fnd->brother)
	    {
		checker->t=0;
		checker->b=0;
		checker->l=0;
		checker->r=0;
	 	}
	checker = checker->brother;
        }
        while(checker != ahead);
	
	
	i=0;
	while(i< 21)
	  {
	if(ahead != fnd && ahead != fnd->brother)
	    {
	i++;
	ahead->t = raster_y(t);
	ahead->b = raster_y(b);
	ahead->l = raster_x(l);
	ahead->r = raster_x(r);

	paint_block(ahead->t+1,ahead->b-1,ahead->l+1,ahead->r-1);

	color = D_translate_color("white");
        R_standard_color(color);
	write_box_text(ahead);
	color = D_translate_color("black");
        R_standard_color(color);
	
	t -= 4;
	b -= 4;
	    }
	ahead = ahead->brother;
	}

	return(fnd);
	}
}

	
