/****************************************************************/
/*								*/
/*	erase_ring.c	in 	~/src/i_range			*/
/*								*/
/*	This function erases those option boxes from an option	*/
/*	ring that are visible on the graphics monitor at the	*/
/*	time of this function call.				*/
/*								*/
/****************************************************************/	

#include "menu.h"

struct box *erase_ring(box)
        struct box *box;

{
        struct box *ptr;
	int color;

        ptr = box;

	/* set paint color to that of the background for 	*/
	/* erasing effect					*/
	color = D_translate_color("gray");
        R_standard_color(color);

	/* loop for the ring painting every visible box gray	*/
        do
	{ 
	if(box->t != 0)
	paint_block(box->t-2,box->b+2,box->l-2,box->r+2);
        box = box->brother;
        }
        while(box != ptr );

	/*	restore previous black color			*/
	color = D_translate_color("black");
        R_standard_color(color);

        return(box);
}

/************ END OF FUNCTION "ERASE_RING" **********************/
