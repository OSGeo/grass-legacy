/* %W% %G% */
/**************************************************************/
/*                                                            */
/*     stash.c           in    ~/src/Gsurface                 */
/*                                                            */
/*     This routine assigns the values of the command line    */
/*     arguments to the appropriate variables depending       */
/*     on the positions occupied by those arguments on the    */
/*     command line.                                          */
/*                                                            */
/**************************************************************/

#include "stash.h"

stash_away(pos,option)
    int pos;
    char *option;
{

	if (pos == INPUT_LAYER) 
	{
        strcpy(input_layer,option);
        return(0);
    	}

	if (pos == OUTPUT_LAYER) 
	{
        strcpy(output_layer,option);
        return(0);
    	}
}

/**************** END OF FUNCTION "STASH_AWAY" ******************/
