/****************************************************************/
/*							 	*/
/*	free_child_ring.c  in 	~/src/i_range			*/
/*								*/
/* 	This function frees the memory allocated to all		*/
/*  	data structures lying in a child ring other than the	*/
/*	"scroll" and "return" windows.  It receives as		*/
/* 	an input parameter the address of the parent of the 	*/
/*  	ring to be destroyed and returns the same address.	*/
/*								*/
/****************************************************************/

#include "menu.h"

struct box *free_child_ring(parent)
	struct box *parent;

{	
	struct box *to_be_killed,*ptr,*scroll,*ret;

	scroll = parent->child; /* scroll points to "scroll" box*/
	ret = scroll->brother;	/* ret points ro "return" box   */
	ptr = ret->brother;
	
	/*	make one round trip over the child ring		*/
	while(ptr != scroll)
	{	to_be_killed = ptr;	/* box to be removed	*/
		ptr = ptr->brother;
		free(to_be_killed);	/*  free memory for box	*/
	}

	/*  leave a ring of "scroll" and "return" boxes only	*/
	ret->brother = scroll;

	return(parent);		/* 	return parent's address	*/
}
	
/************ END OF FUNCTION "FREE_CHILD_RING" *****************/
