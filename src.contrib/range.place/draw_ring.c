/****************************************************************/
/*                                                              */
/*      draw_ring.c    in      ~/src/i_range                    */
/*                                                              */
/*      This function draws option boxes from an option ring 	*/
/*      on the graphics screen.	If a ring has a large number	*/
/*	of option boxes, only the first 23 are drawn since	*/
/*	the screen can accommodate only these.			*/
/*                                                              */
/****************************************************************/ 

#include "menu.h"

struct box *draw_ring(box)
        struct box *box;

{       struct box *t;
	struct box *write_box_text();
        int color;
        int i=0;
        
        t = box;
        
	/*	loop over the boxes 				*/
        do
	{i++;

	/*	draw black box inside border			*/
        paint_block(box->t+1,box->b-1,box->l+1,box->r-1);
        R_flush();

	/*	draw border in white				*/
        color = D_translate_color("white");
        R_standard_color(color);
        outline_box(box->t,box->b,box->l,box->r);
        R_flush();
	
	/*	write out box text in white			*/
        box= write_box_text(box);
        R_flush();

	/*	restore black color				*/
        color = D_translate_color("black");
        R_standard_color(color);

	/*	move to next box				*/
        box = box->brother;

        }
        while(t!=box && i<23);

        return(box);
}

/************ END OF FUNCTION "DRAW_RING" ***********************/

