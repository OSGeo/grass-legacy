/****************************************************************/
/*								*/
/*	write_box_text.c  in  ~/src/i_range			*/
/*								*/
/*	This function writes the text of the box signifying     */
/*	the menu option inside the box on the graphics monitor. */
/*								*/
/****************************************************************/

#include "menu.h"

struct box *write_box_text(box)
        struct box *box;
{
	char trunc[10];		/*	truncated char string	*/ 
	int i=0;

	/* if text of the box is other than '0', throw it on	*/
	/* the graphics monitor making sure it is truncated 	*/
	/* to fit inside box.   				*/

	if(box->text[0] == '0')	/*	do nothing		*/
				;

	else{			/*	show text		*/
	
	while(box->text[i] != '\0' && i<9)
	{ trunc[i]= box->text[i];	/* truncate if reqd.	*/
			    i++;	
	}

	trunc[i]= '\0';		/*	 end with null marker 	*/

	R_move_abs(box->l+(box->r-box->l)/22,
		   box->t+2*(box->b-box->t)/3);
        R_text(trunc);
	R_flush();
	}

        return(box);
}

/********** END OF FUNCTION "WRITE_BOX_TEXT" ********************/
