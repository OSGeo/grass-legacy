#include "menu.h"


struct box *draw_scroll_triangle(ptr)

	struct box *ptr;
{

	char *malloc();
        int *x_ptr, *y_ptr;
	int width,height,color;
	struct box *scroll;

	 x_ptr = (int *) malloc(3*sizeof(int));
         y_ptr = (int *) malloc(3*sizeof(int));
	
	scroll = ptr->parent->child;
	height = scroll->b - scroll->t;
	width = scroll->r - scroll->l;

	R_move_abs(scroll->l,scroll->t); 

	*x_ptr =  (int ) (width/3.0);
	*y_ptr =  (int ) (height/4.0);
	*(x_ptr + 1) = (int ) (width/3.0);
        *(y_ptr + 1) = 0;
        *(x_ptr + 2) = - (int ) (width/6.0);
        *(y_ptr + 2) = (int ) (height/1.5);


	color = D_translate_color("white");
        R_standard_color(color);

	R_polygon_rel(x_ptr,y_ptr,3);
	color = D_translate_color("black");
        R_standard_color(color);
        R_flush();

	return(ptr);
}



	


