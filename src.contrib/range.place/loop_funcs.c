
/****************************************************************/
/*                                                              */
/*      loop_over_funcs.c       in  ~/src/i_range               */
/*                                                              */
/*	This function executes all the functions whose index	*/
/*	numbers (referring to the function table) are stored	*/
/*	in the "func_list"					*/
/*                                                              */
/****************************************************************/

#include "menu.h"

typedef struct box *  (*pvoidfn)();

struct box *loop_over_functions(box,dispatch)
        struct box *box;
	pvoidfn dispatch[];

{
        int j,index_no,no_of_functions;
	extern struct box *base;

        no_of_functions = box->i;

	/*	base holds the address of the option box	*/
	/*	whose list of functions is to be executed	*/
        base = box;

        for(j=0; j<no_of_functions; j++)
        {
	/*  pick up index number of function from function list	*/
	index_no = *(base->func_list + j);

	/*  dispatch to execute the function pointed to	by the  */
	/*  respective entry in the funct pointer array (table) */
        box = (*dispatch[index_no])(box);
	}

        return(box);
}

/************* END OF FUNCTION "LOOP_OVER_FUNCTIONS" ************/
