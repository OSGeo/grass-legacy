/****************************************************************/
/*								*/
/*	go_up_down.c	in 	~/src/i_range			*/
/*								*/
/*	The following functions move the box pointer into 	*/
/*	the parent or the child ring.				*/
/*								*/
/****************************************************************/

#include "menu.h"

struct box *go_to_parent(box)
        struct box *box;
 
{
	/*	return parent box's address			*/
        return(box->parent);
}	

/************* END OF FUNCTION "GO_TO_PARENT" *******************/



struct box *go_to_child(box)
        struct box *box;
{
	/*	return child box's address			*/
        return(box->child);
}

/************ END OF FUNCTION  "GO_TO_CHILD" ********************/
