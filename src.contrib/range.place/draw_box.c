/****************************************************************/
/*								*/
/*	draw_box.c  in 	~/src/i_range				*/
/*								*/
/*	This function draws a menu box on the screen.		*/
/*
/****************************************************************/

#include "menu.h"

struct box *draw_polygon(box)

        struct box *box;
{
	paint_block(box->t,box->b,box->l,box->r);
        return(box);
}

/*************** END OF FUNCTION "DRAW_BOX" *********************/

