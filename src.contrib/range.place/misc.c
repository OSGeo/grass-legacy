#include "menu.h"

struct box *scroll_enter(ptr)

	struct box *ptr;
{
	extern struct box *ahead;
	extern int layer_count;
	extern int perm_cell_count,user_cell_count,
                        basic_patt_count,land_patt_count;
	int i;
	int t=92, b=89, l=86, r=95;

        extern struct box *perm_cell_ptr,*user_cell_ptr,
                                *basic_patt_ptr,*land_patt_ptr;
	struct box *arrow,*pivot;
	struct box *parent;

	arrow = ptr;  /* arrow points to "scroll" window
*/
        pivot = arrow->brother;  /* pivot points to "return" window
*/

        parent = ptr->parent;

        if(parent == perm_cell_ptr)     layer_count = perm_cell_count;
 else   if(parent == user_cell_ptr)     layer_count = user_cell_count;
 else   if(parent == basic_patt_ptr)    layer_count = basic_patt_count;
 else   if(parent == land_patt_ptr)     layer_count = land_patt_count;


	i=0;
	do{

	if(arrow == ptr  || arrow == pivot);
	else{	
	i++;
	if(i<= 21){
	t -= 4;
        b -= 4;
        arrow->l = raster_x(l);
        arrow->r=raster_x(r);
        arrow->b=raster_y(b);
        arrow->t=raster_y(t);
        }
        else
        {
        arrow->l= 0;
        arrow->r= 0;
        arrow->b= 0;
	arrow->t= 0;}

	}

	arrow = arrow->brother;
	} while(arrow != ptr);



	ahead= ptr;
	if(layer_count > 21)
	for(i=0;i<23;i++) ahead = ahead->brother;

	


	return(ptr);

}



struct box *message(ptr)
	struct box *ptr;
{
	extern int txt_a,txt_b;
	extern char buf[];

	throw_text(buf,txt_a,txt_b);
	sleep(4);
	erase_in_poly(ptr);
	
	return(ptr);
}
